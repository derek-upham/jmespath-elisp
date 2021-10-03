;;; jmespath.el --- Query JSON data using the JMESPath expression language.  -*- lexical-binding: t -*-

;; Copyright (C) 2021 Derek Upham

;; Author: Derek Upham <derek_upham@mailfence.com>
;; Description: Query JSON data using JMESPath
;; Keywords: data, json
;; Package-Requires: ((emacs "26.1"))
;; Package-Version: 20210928.1904
;; URL: https://github.com/derek-upham/jmespath-el
;; Version: 1.0.0

;; FIXME: Make compatible with package manager.
;;
;; - https://github.com/melpa/melpa
;; - https://spin.atomicobject.com/2016/05/27/write-emacs-package

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; JMESPath (https://jmespath.org) is a query language for JSON.  The
;; JMESPath website includes a documentation, a specification, a test
;; suite, and links to implementations in various languages.
;;
;; This package implements JMESpath in Emacs Lisp.  All names use a
;; "jmespath-" prefix.  The entry point is the `jmespath-search'
;; function.  Call it by passing in a JMESPath string and a JSON value
;; (a parsed JSON data structure).  Example, straight from the
;; https://jmespath.org/ home page:
;;
;;   (jmespath-search
;;     "locations[?state == 'WA'].name | sort(@) | {WashingtonCities: join(', ', @)}"
;;     (json-parse-string "{\"locations\": [{\"name\": \"Seattle\", \"state\": \"WA\"},
;;                                          {\"name\": \"New York\", \"state\": \"NY\"},
;;                                          {\"name\": \"Bellevue\", \"state\": \"WA\"},
;;                                          {\"name\": \"Olympia\", \"state\": \"WA\"}]}"))
;;       ⇒ #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125
;;                       data ("WashingtonCities" "Bellevue, Olympia, Seattle"))
;;
;; `jmespath-search' parses the query on the fly from the printed
;; representation.  Use `jmespath-compile-query' to parse the string
;; once into a format that you can store and reuse (similar to a
;; regular expression "compile" operation).
;;
;; This implementation requires parsed JSON data with the following
;; conventions:
;;
;;  | JSON type | Emacs Lisp type |
;;  |-----------|-----------------|
;;  | object    | hash table      |
;;  | array     | vector          |
;;  | string    | string          |
;;  | number    | number          |
;;  | boolean   | t, :false       |
;;  | null      | :null           |
;;
;; These are the Jansson parser defaults.  The function returns a JSON
;; data structure with the results of the query, using the same
;; conventions.
;;
;; JMESPath defines a set of built-in functions.  If you wish, you can
;; extend this set by defining your own specializer on
;; `jmespath-call-function' (q.v.).
;;
;; The implementation passes the compliance test suite.  Load
;; "jmespath-test.el" and run `jmespath-run-test-files'.  When
;; prompted, provide a directory with the test suite files.
;;
;; FIXME: An obvious next step is to add an alternative query language
;; with the same semantics but an s-expression format.  This would be
;; `jmespath-compile-sexp', plus additional support in
;; `jmespath-search'.

;;; Code:

(jmespath-search
 "locations[?state == 'WA'].name | sort(@) | {WashingtonCities: join(', ', @)}"
 (json-parse-string "{\"locations\": [{\"name\": \"Seattle\", \"state\": \"WA\"},
                                         {\"name\": \"New York\", \"state\": \"NY\"},
                                         {\"name\": \"Bellevue\", \"state\": \"WA\"},
                                         {\"name\": \"Olympia\", \"state\": \"WA\"}]}"))
#s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("WashingtonCities" "Bellevue, Olympia, Seattle"))
⇒ ((WashingtonCities . "Bellevue, Olympia, Seattle"))


(require 'cl-lib)                       ; soooo much cl-lib...

;;;
;;; Errors
;;;

;; The jmespath.test "README.rst" file mandates the following error types:
;;
;; * ``syntax`` - Syntax error from an invalid JMESPath expression.
;; * ``invalid-arity`` - Wrong number of arguments passed to a function.
;; * ``invalid-type`` - Invalid argument type for a function.
;; * ``invalid-value`` - Semantically incorrect value (used in slice tests)
;; * ``unknown-function`` - Attempting to invoke an unknown function.
;;
;; It looks like most type mismatch problems or input range problems
;; just return :null, without triggering an error, but functions make
;; stronger guarantees.

(define-error 'jmespath-error "JMESPath error" 'error)
(define-error 'jmespath-invalid-arity-error "JMESPath invalid arity" 'jmespath-error)
(define-error 'jmespath-invalid-type-error "JMESPath invalid type" 'jmespath-error)
(define-error 'jmespath-invalid-value-error "JMESPath invalid value" 'jmespath-error)
(define-error 'jmespath-syntax-error "JMESPath syntax error" 'jmespath-error)
(define-error 'jmespath-unknown-function-error "JMESPath unknown function" 'jmespath-error)

;;;
;;; Types
;;;

;;; https://alhassy.github.io/TypedLisp.html
;;; Learn it.  Know it.  Live it.

;;; First we define the JSON types.  Notice that objects are
;;; association lists, not hash tables.

(defun jmespath-json-null-p (thing)
  (eq :null thing))

(cl-deftype jmespath-json-null ()
  '(satisfies jmespath-json-null-p))

(defun jmespath-json-boolean-p (thing)
  (or (eq t thing)
      (eq :false thing)))

(cl-deftype jmespath-json-boolean ()
  '(satisfies jmespath-json-boolean-p))

(defun jmespath-json-number-p (thing)
  (numberp thing))

(cl-deftype jmespath-json-number ()
  '(satisfies jmespath-json-number-p))

(defun jmespath-json-string-p (thing)
  (stringp thing))

(cl-deftype jmespath-json-string ()
  '(satisfies jmespath-json-string-p))

(defun jmespath-json-array-p (thing)
  (and (vectorp thing)
       (not (stringp thing))))

(cl-deftype jmespath-json-array ()
  '(satisfies jmespath-json-array-p))

(defun jmespath-json-object-p (thing)
  (hash-table-p thing))

(cl-deftype jmespath-json-object ()
  '(satisfies jmespath-json-object-p))

(defun jmespath-json-value-p (thing)
  (or (jmespath-json-null-p thing)
      (jmespath-json-boolean-p thing)
      (jmespath-json-number-p thing)
      (jmespath-json-string-p thing)
      (jmespath-json-array-p thing)
      (jmespath-json-object-p thing)))

(cl-deftype jmespath-json-value ()
  '(satisfies jmespath-json-value-p))

;;; These are JMESPath concepts, not JSON concepts, so there's no
;;; "-json-" infix.

(defun jmespath-truthy-p (value)
  "Return true boolean and filter expressions should treat VALUE as true."
  (cl-typecase value
    ;; never truthy
    (jmespath-json-null nil)
    ;; non-empty objects are truthy
    (jmespath-json-object (cl-plusp (hash-table-count value)))
    ;; test other types against their falsey values
    (jmespath-json-boolean (not (or (eq value :false) (eq value :nil))))
    (jmespath-json-string (not (string= value "")))
    (jmespath-json-array (not (eq value [])))
    ;; numbers are never falsey
    (jmespath-json-number t)
    (t (error "unexpected type for value %s" value))))

;; "Identifier" is just another way of saying "unquoted string that
;; I'm going to use as an object's key".  Emacs's JSON representations
;; use symbols for keys in association lists, but strings for keys in
;; hash tables.  We leave it as a string in the AST and intern if and
;; when necessary, in order to not lock ourselves into a format.
(defun jmespath-identifier-p (thing)
  (stringp thing))

(cl-deftype jmespath-identifier ()
  '(satisfies jmespath-identifier-p))

;;; Lots of type-safety validators for functions.

(defun jmespath-array-of-numbers-p (thing)
  (and (jmespath-json-array-p thing)
       (cl-every #'jmespath-json-number-p thing)))

(cl-deftype jmespath-array-of-numbers ()
  '(satisfies jmespath-array-of-numbers-p))

(defun jmespath-array-of-strings-p (thing)
  (and (jmespath-json-array-p thing)
       (cl-every #'jmespath-json-string-p thing)))

(cl-deftype jmespath-array-of-strings ()
  '(satisfies jmespath-array-of-strings-p))

(defun jmespath-array-of-objects-p (thing)
  (and (jmespath-json-array-p thing)
       (cl-every #'jmespath-json-object-p thing)))

(cl-deftype jmespath-array-of-objects ()
  '(satisfies jmespath-array-of-objects-p))

(defun jmespath-list-of-objects-p (thing)
  (and (listp thing)
       (cl-every #'jmespath-json-object-p thing)))

(cl-deftype jmespath-list-of-objects ()
  '(satisfies jmespath-list-of-objects-p))

(defun jmespath-orderable-p (thing)
  (or (jmespath-json-number-p thing)
      (jmespath-json-string-p thing)))

(cl-deftype jmespath-orderable ()
  '(satisfies jmespath-orderable-p))

;; This isn't the best name, since it doesn't make clear that these
;; are *homogenous* arrays, with all of one type or all of the other.
;; `jmespath-homogenous-array-of-orderables-p' is kind of long,
;; though.  Probably not a concern.
(defun jmespath-array-of-orderables-p (thing)
  (or (jmespath-array-of-numbers-p thing)
      (jmespath-array-of-strings-p thing)))

(cl-deftype jmespath-array-of-orderables ()
  '(satisfies jmespath-array-of-orderables-p))

(defun jmespath-arraylike-p (thing)
  (or (jmespath-json-array-p thing)
      (jmespath-json-string-p thing)))

(cl-deftype jmespath-arraylike ()
  '(satisfies jmespath-arraylike-p))

(defun jmespath-countable-p (thing)
  (or (jmespath-json-array-p thing)
      (jmespath-json-string-p thing)
      (jmespath-json-object-p thing)))

(cl-deftype jmespath-countable ()
  '(satisfies jmespath-countable-p))



;;;;
;;;; Utility functions
;;;;

(defun jmespath-json-equal-compare (left-value right-value)
  "Return JSON true if LEFT-VALUE and RIGHT-VALUE are equal.

Return JSON false otherwise."
  (cond
   ((and (cl-typep left-value 'jmespath-json-number)
         (cl-typep right-value 'jmespath-json-number))
    (if (= left-value right-value)
        t
      :false))
   ((and (cl-typep left-value 'jmespath-json-string)
         (cl-typep right-value 'jmespath-json-string))
    (if (string= left-value right-value)
        t
      :false))
   ((and (cl-typep left-value 'jmespath-json-boolean)
         (cl-typep right-value 'jmespath-json-boolean))
    (if (eq left-value right-value)
        t
      :false))
   ((and (cl-typep left-value 'jmespath-json-array)
         (cl-typep right-value 'jmespath-json-array))
    (if (and (= (length left-value) (length right-value))
             (cl-loop for x across left-value
                      for y across right-value
                      unless (eq (jmespath-json-equal-compare x y) t)
                      return nil
                      finally return t))
        t
      :false))
   ((and (cl-typep left-value 'jmespath-json-object)
         (cl-typep right-value 'jmespath-json-object))
    (if (and (cl-loop for k being the hash-keys of left-value
                      using (hash-values lv)
                      unless (eq (jmespath-json-equal-compare lv (gethash k right-value nil)) t)
                      return nil
                      finally return t)
             (cl-loop for k being the hash-keys of right-value
                      using (hash-values rv)
                      unless (eq (jmespath-json-equal-compare rv (gethash k left-value nil)) t)
                      return nil
                      finally return t))
        t
      :false))
   ((and (cl-typep left-value 'jmespath-json-null)
         (cl-typep right-value 'jmespath-json-null))
    t)
   (t
    :false)))

(defun jmespath-json-order-compare (comparator left-value right-value)
  "Return JSON true if LEFT-VALUE and RIGHT-VALUE are ordered per COMPARATOR.

Return JSON false otherwise.  COMPARATOR is a funcallable value.

Note that comparisons are only meaningful for numbers."
  (if (and (cl-typep left-value 'jmespath-json-number)
           (cl-typep right-value 'jmespath-json-number))
      (if (funcall comparator left-value right-value)
          t
        :false)
    :null))

;; This implements the "flatten" operator, of course.
(defun jmespath-flatten-1-array (arr)
  "Flatten JSON array ARR by one level.

The function splices the elements of any JSON array elements,
while preserving all non-array elements as-is."
  (cl-coerce (cl-loop for x across arr
                      if (cl-typep x 'jmespath-json-array)
                      append (cl-coerce x 'list)
                      else
                      collect x
                      end)
             'vector))

;; All of the projection operations use this.
(defun jmespath-strip-nulls (seq)
  "Strip all :nulls from SEQ and return it.

SEQ is any sequence type.  This function returns a value of the
same type."
  (cl-delete :null seq :test 'eq))

;;;;
;;;; Abstract Syntax Tree
;;;;

;;; These are predeclarations.  We'll go into more detail later.

(defclass jmespath-ast-node () nil
  :documentation "The root type for JMESPath evaluation Abstract Syntax Trees.")
(cl-defgeneric jmespath-ast-eval-with-current (node current)
  "Evaluate the abstract syntax tree NODE against the CURRENT JSON data.")

;; Here's another type definition.  This one defines certain function
;; argument lists.
(defun jmespath-list-of-expressions-p (thing)
  (and (listp thing)
       (cl-every #'(lambda (x) (object-of-class-p x 'jmespath-ast-node)) thing)))

(cl-deftype jmespath-list-of-expressions ()
  '(satisfies jmespath-list-of-expressions-p))


;;;;
;;;; Linearization algorithm.
;;;;

;;; Okay, this isn't anything to do with a linear type system.  We're
;;; just rearranging parts of the AST.
;;;
;;; What's going on here?
;;;
;;; The JMESPath grammar assigns no particular precedence to the
;;; "flatten" operation, but "flatten" operations differ from other
;;; (sub-)expression operations: we can't evaluate them using our
;;; normal "fan-out" recursion.  We have to fully realize the current
;;; (left-hand side) result list; only then can we flatten that list
;;; and continue with the remainder of the evaluation.
;;;
;;; That means we want to reshape certain expressions from the AST,
;;; for example:
;;;
;;;                      △
;;;                     ╱ ╲
;;;                    F   □
;;;                   ╱ ╲
;;;                  △   □
;;;                 ╱ ╲
;;;                ∅   □
;;;
;;;   ∅ = empty/start    △  = expression AST node
;;;   F = flatten        □  = expression parameters
;;;                     ╱ ╲ = AST relationship
;;;
;;; The replacement tree format puts the "flatten" operation at the
;;; top:
;;;
;;;                      F
;;;                     ╱ ╲
;;;            (△□ → △□)   (△□)
;;;
;;;  ( … → … ) = linear evaluation   △□ = expression with parameters
;;;          F = flatten            ╱ ╲ = evaluation relationship
;;;              
;;; We can evaluate this tree using an in-order traversal.  The
;;; "flatten" operation passes the current JSON value to the left-hand
;;; side, gets the result, manipulates it per the specification, and
;;; then passes it to the right-hand side for further evaluation.  A
;;; couple of things are worth mentioning:
;;;
;;; * The general case supports any number of "flatten" operations.
;;;   The resulting structure is always a binary tree of
;;;   "flatten" interior nodes and "linear" leaves.
;;;
;;; * We evaluate these nodes with in-order traversal, whereas the
;;;   normal AST evaluation happens post-order.
;;;   (`jmespath-ast-eval-with-current` hides that.)
;;;
;;; Linear evaluation is a combination of immediate and recursive
;;; steps, and we discuss it in more detail below.
;;;
;;; Yes, since we're converting a portion of the LL tree into a linked
;;; list, one could argue that we're not "really" linearizing it, just
;;; reversing the direction of the links.  Hush.

(cl-defgeneric jmespath-linearize-aux (node trailing))

(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-node) _trailing)
  node)

(defun jmespath-linearize (node)
  "Top-level entry point for the linearization pass."
  (let ((new-tree (jmespath-linearize-aux node nil)))
    ;; We might get back an AST node, but a lot of the time we get
    ;; back a stripped-down s-expression; in that case we have to wrap
    ;; it with the right node type.
    ;;
    ;; FIXME: Is it worth coming up with a more "typeful"
    ;; representation than s-expressions?
    (cl-typecase new-tree
      (jmespath-ast-node
       new-tree)
      (list
       (jmespath-eval-sequence :operations new-tree)))))

;;;;
;;;; AST/Eval nodes
;;;;

;;; "Normal" AST nodes, passing JSON data up an expression tree.
;;;
;;; This is all fairly boilerplate: the AST class matches the grammar;
;;; we support recursive evaluation; and we support recursive
;;; linearization.  Notice how linearization is pretty much a no-op
;;; for these "normal" nodes.

(defclass jmespath-ast-andthen (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-andthen) _trailing)
  (jmespath-ast-andthen
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-andthen) current)
  ;; The specification does not require this, but we only evaluate the
  ;; right-side expression when the left returns a truthy value.  See
  ;; the comments for the `jmespath-linear-immediate' specialization
  ;; on `function-call' regarding potential implications.
  (let ((left-value (jmespath-ast-eval-with-current (left-expr-of node) current)))
    (if (not (jmespath-truthy-p left-value))
        left-value
      (jmespath-ast-eval-with-current (right-expr-of node) current))))

(defclass jmespath-ast-comparison-== (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-comparison-==) _trailing)
  (jmespath-ast-comparison-==
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-comparison-==) current)
  (jmespath-json-equal-compare
   (jmespath-ast-eval-with-current (left-expr-of node) current)
   (jmespath-ast-eval-with-current (right-expr-of node) current)))

(defclass jmespath-ast-comparison-!= (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-comparison-!=) _trailing)
  (jmespath-ast-comparison-!=
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-comparison-!=) current)
  (let ((is-equal (jmespath-json-equal-compare
                   (jmespath-ast-eval-with-current (left-expr-of node) current)
                   (jmespath-ast-eval-with-current (right-expr-of node) current))))
    (if (eq is-equal t)
        :false
      t)))

(defclass jmespath-ast-comparison-< (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-comparison-<) _trailing)
  (jmespath-ast-comparison-<
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-comparison-<) current)
  (jmespath-json-order-compare
   '<
   (jmespath-ast-eval-with-current (left-expr-of node) current)
   (jmespath-ast-eval-with-current (right-expr-of node) current)))

(defclass jmespath-ast-comparison-<= (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-comparison-<=) _trailing)
  (jmespath-ast-comparison-<=
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-comparison-<=) current)
  (jmespath-json-order-compare
   '<=
   (jmespath-ast-eval-with-current (left-expr-of node) current)
   (jmespath-ast-eval-with-current (right-expr-of node) current)))

(defclass jmespath-ast-comparison-> (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-comparison->) current)
  (jmespath-json-order-compare
   '>
   (jmespath-ast-eval-with-current (left-expr-of node) current)
   (jmespath-ast-eval-with-current (right-expr-of node) current)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-comparison->) _trailing)
  (jmespath-ast-comparison->
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))

(defclass jmespath-ast-comparison->= (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-comparison->=) _trailing)
  (jmespath-ast-comparison->=
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-comparison->=) current)
  (jmespath-json-order-compare
   '>=
   (jmespath-ast-eval-with-current (left-expr-of node) current)
   (jmespath-ast-eval-with-current (right-expr-of node) current)))

(defclass jmespath-ast-not (jmespath-ast-node)
  ((right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-not) _trailing)
  (jmespath-ast-not :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-not) current)
  (let ((right-value (jmespath-ast-eval-with-current (right-expr-of node) current)))
    (if (jmespath-truthy-p right-value)
        :false
      t)))

(defclass jmespath-ast-orelse (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-orelse) _trailing)
  (jmespath-ast-orelse
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-orelse) current)
  ;; The specification does not require this, but we only evaluate the
  ;; right-side expression when the left returns a falsey value.  See
  ;; the comments for the `jmespath-linear-immediate' specialization
  ;; on `function-call' regarding potential implications.
  (let ((left-value (jmespath-ast-eval-with-current (left-expr-of node) current)))
    (if (jmespath-truthy-p left-value)
        left-value
      (jmespath-ast-eval-with-current (right-expr-of node) current))))

(defclass jmespath-ast-pipe (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-pipe) _trailing)
  (jmespath-ast-pipe
   :left-expr (jmespath-linearize (left-expr-of node))
   :right-expr (jmespath-linearize (right-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-pipe) current)
  (let ((left-value (jmespath-ast-eval-with-current (left-expr-of node) current)))
    (jmespath-ast-eval-with-current (right-expr-of node) left-value)))


;;; JMESPath could actually get away without ampersands in front of
;;; "quoted" expressions: expressions in argument lists always occur
;;; in specific positions, so there's no ambiguity about what each
;;; function should do with which argument.  However, the ampersand
;;; provides more guidance to the query writer and reader.
;;;
;;; The corresponding AST node sits in the middle of the graph and
;;; provides a bit of extra type-checking.  It works normally during
;;; linearization, but signals an error if someone tries to evaluate
;;; it directly.  To evaluate, use `jmespath-funeval-expression' to
;;; make your intent explicit.
;;;
;;; Interestingly, the interactive website accepts a bare
;;; expression-type in spite of the grammar.  The result object is a
;;; JSON AST of the expression.  Try "&locations[*].name" there.

(defclass jmespath-ast-quoted-expression (jmespath-ast-node)
  ((child-expr :initarg :child-expr :reader child-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-quoted-expression) trailing)
  (jmespath-ast-quoted-expression :child-expr (jmespath-linearize (child-expr-of node))))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-ast-quoted-expression) current)
  (error "direct evaluation of quoted JMESPath expression: %S" node))
(defun jmespath-funeval-expression (node current)
  (cl-check-type node jmespath-ast-quoted-expression)
  (jmespath-ast-eval-with-current (child-expr-of node) current))


;;; Remember: "flatten" is weird.  It bridges the gap between the
;;; plain AST nodes and the nodes that we specialize for evaluation.
;;; When it linearizes, this node returns a different node type from
;;; the original, and we evaluate the AST against that node type.

(defclass jmespath-ast-flatten (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-flatten) trailing)
  (let ((left (jmespath-linearize (left-expr-of node))))
    (jmespath-eval-flatten
     :left-expr left
     :right-expr (jmespath-eval-sequence :operations trailing))))

(defclass jmespath-eval-flatten (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)
   (right-expr :initarg :right-expr :reader right-expr-of)))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-eval-flatten) current)
  (let ((left-current (jmespath-ast-eval-with-current (left-expr-of node) current)))
    (cl-typecase left-current
      (jmespath-json-array
       (let ((flattened-array (jmespath-flatten-1-array left-current)))
         (jmespath-strip-nulls
          (cl-map 'vector
                  #'(lambda (x) (jmespath-ast-eval-with-current (right-expr-of node) x))
                  flattened-array))))
      (t
       :null))))


;;; These are AST nodes that don't persist after we're done parsing.
;;; We turn them into the "linear" form based on s-expressions.

;; We plug linear sequences into the AST, and
;; `jmespath-ast-eval-with-current` evaluation reaches them, but they
;; have their own evaluation mechanism (dispatching to
;; `jmespath-linear-eval`).
(defclass jmespath-eval-sequence (jmespath-ast-node)
  ((operations :initarg :operations :reader operations-of)))
(cl-defmethod jmespath-ast-eval-with-current ((node jmespath-eval-sequence) current)
  (jmespath-linear-eval (operations-of node) current))

(defclass jmespath-ast-current-node (jmespath-ast-node) nil)
(cl-defmethod jmespath-linearize-aux ((_node jmespath-ast-current-node) trailing)
  (cons '(current-node) trailing))

(defclass jmespath-ast-literal (jmespath-ast-node)
  ((tree :initarg :tree :reader tree-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-literal) trailing)
  (cons `(literal ,(tree-of node)) trailing))

(defclass jmespath-ast-extract-field (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (field-name :initarg :field-name :reader field-name-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-extract-field) trailing)
  (jmespath-linearize-aux (left-expr-of node)
                          (cons `(extract-field ,(field-name-of node)) trailing)))

(defclass jmespath-ast-extract-field-wildcard (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-extract-field-wildcard) trailing)
  (jmespath-linearize-aux (left-expr-of node)
                          (cons `(extract-field-wildcard) trailing)))

(defclass jmespath-ast-extract-index (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (index-value :initarg :index-value :reader index-value-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-extract-index) trailing)
  (jmespath-linearize-aux (left-expr-of node)
                          (cons `(extract-index ,(index-value-of node)) trailing)))

(defclass jmespath-ast-extract-index-wildcard (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-extract-index-wildcard) trailing)
  (jmespath-linearize-aux (left-expr-of node)
                          (cons `(extract-index-wildcard ) trailing)))

(defclass jmespath-ast-extract-slice (jmespath-ast-node)
  ((left-expr  :initarg :left-expr  :reader left-expr-of)
   (start-value :initarg :start-value :reader start-value-of :initform nil)
   (stop-value  :initarg :stop-value  :reader stop-value-of :initform nil)
   (step-value  :initarg :step-value  :reader step-value-of :initform 1)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-extract-slice) trailing)
  (jmespath-linearize-aux (left-expr-of node)
                          (cons `(extract-slice ,(start-value-of node)
                                                ,(stop-value-of node)
                                                ,(step-value-of node))
                                trailing)))

(defclass jmespath-ast-multi-select-hash (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)
   (pairs :initarg :pairs :reader pairs-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-multi-select-hash) trailing)
  (let ((linearized-pairs (cl-loop for (k . v) in (pairs-of node)
                                   collect (cons k (jmespath-linearize v)))))
    (jmespath-linearize-aux (left-expr-of node)
                            (cons `(multi-select-hash ,linearized-pairs) trailing))))

(defclass jmespath-ast-multi-select-list (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)
   (elements :initarg :elements :reader elements-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-multi-select-list) trailing)
  (let ((linearized-elements (cl-mapcar #'(lambda (x) (jmespath-linearize x)) (elements-of node))))
    (jmespath-linearize-aux (left-expr-of node)
                            (cons `(multi-select-list ,linearized-elements) trailing))))

(defclass jmespath-ast-filter-expression (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)
   (filter-expr :initarg :filter-expr :reader filter-expr-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-filter-expression) trailing)
  (jmespath-linearize-aux (left-expr-of node)
                          (cons `(filter ,(jmespath-linearize (filter-expr-of node)))
                                trailing)))

(defclass jmespath-ast-function-call (jmespath-ast-node)
  ((left-expr :initarg :left-expr :reader left-expr-of)
   (function-name :initarg :function-name :reader function-name-of)
   (function-args :initarg :function-args :reader function-args-of)))
(cl-defmethod jmespath-linearize-aux ((node jmespath-ast-function-call) trailing)
  (jmespath-linearize-aux
   (left-expr-of node)
   (cons `(function-call ,(function-name-of node)
                         ,(cl-mapcar #'(lambda (x) (jmespath-linearize x))
                                     (function-args-of node)))
         trailing)))


;;;;
;;;; "Linear" sequence evaluation
;;;;

;;; "*-immediate" operations don't fan-out recursively to children,
;;; and don't go through :null removal.  Notice how they use iteration
;;; directly in the loop.  We could even inline their associated
;;; functions, but a bulked-out loop would be harder to read.
;;;
;;; "*-project" operations do fan-out to multiple children, so they
;;; make recursive calls to `jmespath-linear-eval'.  They all remove
;;; :null values as well.

(defun jmespath-linear-eval (op-chain current)
  (cl-loop for cursor on op-chain
           for operation = (car cursor)
           for ops-remaining = (cdr cursor)
           for opcode = (car operation)
           for opargs = (cdr operation)
           do (cond
               ((eq opcode 'current-node)      (setq current (jmespath-linear-immediate opcode opargs current)))
               ((eq opcode 'literal)           (setq current (jmespath-linear-immediate opcode opargs current)))
               ((eq opcode 'extract-field)     (setq current (jmespath-linear-immediate opcode opargs current)))
               ((eq opcode 'extract-index)     (setq current (jmespath-linear-immediate opcode opargs current)))
               ((eq opcode 'multi-select-list) (setq current (jmespath-linear-immediate opcode opargs current)))
               ((eq opcode 'multi-select-hash) (setq current (jmespath-linear-immediate opcode opargs current)))
               ((eq opcode 'function-call)     (setq current (jmespath-linear-immediate opcode opargs current)))
               (t (cl-return (jmespath-linear-project opcode opargs current ops-remaining))))
           finally return current))

;; All of the implementing methods dispatch based on the first
;; `opcode' argument.  This is a new feature in Emacs 28.
(cl-defgeneric jmespath-linear-immediate (opcode opargs current))

(cl-defmethod jmespath-linear-immediate ((_opcode (eql 'current-node)) _opargs current)
  current)

(cl-defmethod jmespath-linear-immediate ((_opcode (eql 'literal)) opargs _current)
  (car opargs))

(cl-defmethod jmespath-linear-immediate ((_opcode (eql 'extract-field)) opargs current)
  (cl-typecase current
    (jmespath-json-object
     (cl-destructuring-bind (field-name) opargs
       (cl-check-type field-name jmespath-identifier)
       (gethash field-name current :null)))
    (t
     :null)))

(cl-defmethod jmespath-linear-immediate ((_opcode (eql 'extract-index)) opargs current)
  (cl-typecase current
    (jmespath-json-array
     (cl-destructuring-bind (idx) opargs
       (cl-check-type idx jmespath-json-number)
       (let* ((len (length current))
              (logical-idx (if (< idx 0)
                               (+ len idx)
                             idx)))
         (if (and (<= 0 logical-idx) (< logical-idx len))
             (aref current logical-idx)
           :null))))
    (t
     :null)))

(cl-defmethod jmespath-linear-immediate ((_opcode (eql 'multi-select-list)) opargs current)
  (cl-typecase current
    (jmespath-json-null
     :null)
    (t
     (cl-destructuring-bind (elements) opargs
       (cl-map 'vector
               #'(lambda (e) (jmespath-ast-eval-with-current e current))
               elements)))))

(cl-defmethod jmespath-linear-immediate ((_opcode (eql 'multi-select-hash)) opargs current)
  (cl-typecase current
    (jmespath-json-null
     :null)
    (t
     (cl-destructuring-bind (pairs) opargs
       (cl-loop with table = (make-hash-table :size (length pairs) :test 'equal)
                for (key . expr) in pairs
                do (cl-check-type key jmespath-identifier)
                do (cl-check-type expr jmespath-ast-node)
                do (puthash key (jmespath-ast-eval-with-current expr current) table)
                finally return table)))))

(cl-defmethod jmespath-linear-immediate ((_opcode (eql 'function-call)) opargs current)
  ;; The spec is explicit that "not_null" evaluates left-to-right
  ;; until it finds a non-null value.  In a language without
  ;; side-effects (which JMESPath is), there is no way for an observer
  ;; to tell whether the implementation actually does that, or instead
  ;; evaluates all arguments and then scans the results in order.
  ;; The latter implementation would just be slower.
  ;;
  ;; However, our implementation allows user-defined functions, and
  ;; user-defined functions might have side-effects...  That means
  ;; we'll play it safe and defer evaluation to the individual
  ;; functions.

  ;; This is another generic function with `eql' dispatch, defined
  ;; later.  We could actually move the interning of the function name
  ;; all the way up to the parser, if we care to in the future.
  (jmespath-call-function (intern (cl-first opargs)) (cl-second opargs) current))

(cl-defgeneric jmespath-linear-project (opcode opargs current ops-remaining))

(cl-defmethod jmespath-linear-project ((_opcode (eql 'extract-field-wildcard)) _opargs current ops-remaining)
  (cl-typecase current
    (jmespath-json-object
     (jmespath-strip-nulls
      (cl-coerce (cl-loop for v being the hash-values of current
                          collect (jmespath-linear-eval ops-remaining v))
                 'vector)))
    (t
     :null)))

(cl-defmethod jmespath-linear-project ((_opcode (eql 'extract-index-wildcard)) _opargs current ops-remaining)
  (cl-typecase current
    (jmespath-json-array
     (jmespath-strip-nulls
      (cl-map 'vector #'(lambda (x) (jmespath-linear-eval ops-remaining x)) current)))
    (t
     :null)))

(cl-defmethod jmespath-linear-project ((_opcode (eql 'extract-slice)) opargs current ops-remaining)
  (cl-destructuring-bind (start stop step) opargs
    (cl-check-type start (or null jmespath-json-number))
    (cl-check-type stop (or null jmespath-json-number))
    (cl-check-type step (or null jmespath-json-number))

    (unless step
      (setq step 1))

    (when (= step 0)
      (signal 'jmespath-invalid-value-error
              (list "illegal zero step" (format "[%s:%s:%s]" start stop step))))

    (cl-typecase current
      (jmespath-json-array
       (let* ((len (length current)))
         ;; The slice iteration always starts *at* the `start' and
         ;; stops *before* reaching the `stop'; it doesn't matter
         ;; whether the `stop' is the upper or lower bound.
         ;;
         ;; The JMESPath slice definition says the following:
         ;;
         ;;   If no stop position is given, it is assumed to be the
         ;;   length of the array if the given step is greater than 0 or
         ;;   0 if the given step is less than 0.
         ;;
         ;; But this *doesn't* match the compliance tests: slice [:0:-1]
         ;; does not return slot 0, but slice [::-1] does return slot 0.
         ;;
         ;; It seems like under the hood the slice algorithm treats a
         ;; missing `start' or `stop' bound as "unbounded and
         ;; infinite", but we never see that because in all cases the
         ;; implementation ignores indices that end up outside of the
         ;; array.  (Because they're null, and slice is a projection
         ;; so it deletes nulls.)
         ;;
         ;; We handle all of this by futzing with the `start' and `stop'
         ;; values.

         (cond
          ((null start)
           ;; No start defined, pick the "first" for our array,
           ;; according to the step.
           (if (< 0 step)
               (setq start 0)
             (setq start (1- len))))
          ;; Negative value, so loop around per the spec.
          ((< start 0)
           (setq start (+ len start))))

         (cond
          ;; No stop defined, pick the "one-past-the-last" for our
          ;; array, according to the step.
          ((null stop)
           (if (< 0 step)
               (setq stop len)
             (setq stop -1)))
          ;; Negative value, so loop around per the spec.
          ((< stop 0)
           (setq stop (+ len stop))))

         (let ((collection (if (< 0 step)
                               ;; We might get a performance boost by looking for `step' of 1 and
                               ;; using `seq-subseq' in that case.  It's probably not worth it.
                               (cl-coerce (cl-loop for i from start below stop by step
                                                   when (and (<= 0 i) (< i len))
                                                   collect (jmespath-linear-eval ops-remaining (aref current i)))
                                          'vector)
                             ;; The `by' clause always has to be positive, sadly.
                             (cl-coerce (cl-loop for i from start above stop by (- step)
                                                 when (and (<= 0 i) (< i len))
                                                 collect (jmespath-linear-eval ops-remaining (aref current i)))
                                        'vector))))
           (jmespath-strip-nulls collection))))
      (t
       :null))))

(cl-defmethod jmespath-linear-project ((_opcode (eql 'filter)) opargs current ops-remaining)
  (cl-destructuring-bind (filter-expr) opargs
    (cl-check-type filter-expr jmespath-ast-node)
    (cl-typecase current
      (jmespath-json-array
       (let ((stripped-elements
              (cl-remove-if-not #'(lambda (x)
                                    (jmespath-truthy-p (jmespath-ast-eval-with-current filter-expr x)))
                                current)))
         (jmespath-strip-nulls
          (cl-map 'vector
                  #'(lambda (x) (jmespath-linear-eval ops-remaining x))
                  stripped-elements))))
      (t
       :null))))


;;;;
;;;; Function call evaluation
;;;;

(defmacro jmespath-call-function-eval-args (function-identifier vars+types exprs current &rest body)
  "Convenience macro for writing JMESPath functions.

  1. Associates variables with function expressions (and validates arity).
  2. Evaluates the function expressions and values the types of the results.

"
  (declare (indent 4) (debug (sexp sexp form form &rest form)))
  (let ((exprs-var (gensym)))
    (cond
     ((null vars+types)
      `(let ((,exprs-var ,exprs))
         (cond
          ((null ,exprs)
           (progn ,@body))
          (t
           (signal 'jmespath-invalid-arity-error (list ,function-identifier))))))
     (t
      (cl-destructuring-bind ((v tp) &rest v+tp_) vars+types
        `(let ((,exprs-var ,exprs))
           (cond
            ((null ,exprs-var)
             (signal 'jmespath-invalid-arity-error (list ,function-identifier)))
            (t
             (let* ((,v (jmespath-ast-eval-with-current (car ,exprs-var) ,current)))
               (cl-typecase ,v
                 (,tp
                  (jmespath-call-function-eval-args ,function-identifier ,v+tp_ (cdr ,exprs-var) ,current ,@body))
                 (t
                  (signal 'jmespath-invalid-type-error (list ,function-identifier ,exprs-var ',tp)))))))))))))

;; Another generic function using `eql' dispatch.
;;
;; All of these come from the JMESPath specification, but because we
;; use `eql' dispatch, you can add your own function by defining your
;; own `jmespath-call-function' specialiser.
(cl-defgeneric jmespath-call-function (function-identifier function-args current))

(cl-defmethod jmespath-call-function (function-identifier _function-args _current)
  (signal 'jmespath-unknown-function-error (list function-identifier)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'abs)) function-args current)
  (jmespath-call-function-eval-args 'abs ((val jmespath-json-number)) function-args current
                                    (abs val)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'avg)) function-args current)
  (jmespath-call-function-eval-args 'avg ((vals jmespath-array-of-numbers-p)) function-args current
    (if (zerop (length vals))
        :null
      (/ (cl-reduce #'+ vals :initial-value 0)
         (float (length vals))))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'ceil)) function-args current)
  (jmespath-call-function-eval-args 'ceil ((val jmespath-json-number)) function-args current
    (ceiling val)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'contains)) function-args current)
  (jmespath-call-function-eval-args 'contains ((data jmespath-arraylike) (candidate jmespath-json-value))
                                    function-args current
    (cond
     ((and (cl-typep data 'jmespath-json-string) (cl-typep candidate 'jmespath-json-string))
      (if (cl-search candidate data) t :false))
     ((cl-typep data 'jmespath-json-array)
      (if (cl-find candidate data :test 'equal) t :false))
     (t
      (signal 'jmespath-invalid-type-error (list 'contains data candidate))))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'ends_with)) function-args current)
  (jmespath-call-function-eval-args 'ends_with ((data jmespath-json-string) (candidate jmespath-json-string))
                                    function-args current
    (cond
     ((< (length data) (length candidate))
      :false)
     ((string= (substring data (- (length candidate))) candidate)
      t)
     (t
      :false))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'floor)) function-args current)
  (jmespath-call-function-eval-args 'floor ((val jmespath-json-number)) function-args current
    (floor val)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'join)) function-args current)
  (jmespath-call-function-eval-args 'join ((glue jmespath-json-string) (vals jmespath-array-of-strings))
                                    function-args current
    (mapconcat #'identity vals glue)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'keys)) function-args current)
  (jmespath-call-function-eval-args 'keys ((val jmespath-json-object)) function-args current
    (cl-coerce (hash-table-keys val) 'vector)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'length)) function-args current)
  (jmespath-call-function-eval-args 'length ((val jmespath-countable)) function-args current
    (cl-typecase val
      ((or jmespath-json-string
           jmespath-json-array)
       (length val))
      (jmespath-json-object
       (hash-table-size val))
      (t
       (signal 'jmespath-invalid-type-error (list 'length val))))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'map)) function-args current)
  (cl-destructuring-bind (extractor-expr array-expr) function-args
    (let ((array-value (jmespath-ast-eval-with-current array-expr current)))
      (cond
       ((not (cl-typep extractor-expr 'jmespath-ast-quoted-expression))
        (signal 'jmespath-invalid-type-error (list 'map extractor-expr)))
       ((not (cl-typep array-value 'jmespath-json-array))
        (signal 'jmespath-invalid-type-error (list 'map array-expr)))
       (t
        (cl-map 'vector #'(lambda (x) (jmespath-funeval-expression extractor-expr x)) array-value))))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'max)) function-args current)
  (jmespath-call-function-eval-args 'max ((vals jmespath-array-of-orderables)) function-args current
    (cond
     ((zerop (length vals))
      :null)
     ((cl-typep (elt vals 0) 'jmespath-json-string)
      (cl-reduce #'(lambda (a b) (if (string< a b) b a)) vals))
     ((cl-typep (elt vals 0) 'jmespath-json-number)
      (cl-reduce #'max vals)))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'max_by)) function-args current)
  (cl-destructuring-bind (array-expr key-expr) function-args
    (let ((array-value (jmespath-ast-eval-with-current array-expr current)))
      (cond
       ((not (cl-typep key-expr 'jmespath-ast-quoted-expression))
        (signal 'jmespath-invalid-type-error (list 'max_by key-expr)))
       ((not (cl-typep array-value 'jmespath-json-array))
        (signal 'jmespath-invalid-type-error (list 'max_by array-expr)))
       ((= (length array-value) 0)
        (signal 'jmespath-invalid-value-error (list 'max_by array-expr)))
       (t
        (let ((kv-pairs (cl-mapcar #'(lambda (x)
                                       (cons (jmespath-funeval-expression key-expr x) x))
                                   array-value)))
          (unless (cl-typep (cl-map 'vector #'car kv-pairs) 'jmespath-array-of-orderables)
            (signal 'jmespath-invalid-type-error (list 'max_by array-expr)))
          (let ((pred (cond
                       ((cl-typep (car (cl-first kv-pairs)) 'jmespath-json-string) #'string<)
                       ((cl-typep (car (cl-first kv-pairs)) 'jmespath-json-number) #'<)
                       (t (signal 'jmespath-invalid-type-error ())))))            
            (cdr (cl-reduce #'(lambda (most x) (if (funcall pred (car most) (car x)) x most)) kv-pairs)))))))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'merge)) function-args current)
  (cl-typecase function-args
    (null '())
    (jmespath-list-of-expressions
     (cl-loop with tmp = (make-hash-table :test 'equal)
              for expr in function-args
              do (cl-loop for k being the hash-keys of (jmespath-ast-eval-with-current expr current)
                          using (hash-values v)
                          do (puthash k v tmp))
              finally return tmp))
    (t
     (signal 'jmespath-invalid-type-error (list 'merge function-args current)))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'min)) function-args current)
  (jmespath-call-function-eval-args 'min ((vals jmespath-array-of-orderables)) function-args current
    (cond
     ((zerop (length vals))
      :null)
     ((cl-typep (elt vals 0) 'jmespath-json-string)
      (cl-reduce #'(lambda (a b) (if (string< a b) a b)) vals))
     ((cl-typep (elt vals 0) 'jmespath-json-number)
      (cl-reduce #'min vals)))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'min_by)) function-args current)
  (cl-destructuring-bind (array-expr key-expr) function-args
    (let ((array-value (jmespath-ast-eval-with-current array-expr current)))
      (cond
       ((not (cl-typep key-expr 'jmespath-ast-quoted-expression))
        (signal 'jmespath-invalid-type-error (list 'min_by key-expr)))
       ((not (cl-typep array-value 'jmespath-json-array))
        (signal 'jmespath-invalid-type-error (list 'min_by array-expr)))
       ((= (length array-value) 0)
        (signal 'jmespath-invalid-value-error (list 'min_by array-expr)))
       (t
        (let ((kv-pairs (cl-mapcar #'(lambda (x)
                                       (cons (jmespath-funeval-expression key-expr x) x))
                                   array-value)))
          (unless (cl-typep (cl-map 'vector #'car kv-pairs) 'jmespath-array-of-orderables)
            (signal 'jmespath-invalid-type-error (list 'min_by array-expr)))
          (let ((pred (cond
                       ((cl-typep (car (cl-first kv-pairs)) 'jmespath-json-string) #'string<)
                       ((cl-typep (car (cl-first kv-pairs)) 'jmespath-json-number) #'<)
                       (t (signal 'jmespath-invalid-type-error ())))))            
            (cdr (cl-reduce #'(lambda (least x) (if (funcall pred (car x) (car least)) x least)) kv-pairs)))))))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'not_null)) function-args current)
  (cl-check-type function-args jmespath-list-of-expressions)
  (if (null function-args)
      (signal 'jmespath-invalid-arity-error (list 'not_null))
    (cl-loop with tmp = :null
             for expr in function-args
             do (setq tmp (jmespath-ast-eval-with-current expr current))
             unless (eq tmp :null) return tmp
             finally return :null)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'reverse)) function-args current)
  (jmespath-call-function-eval-args 'reverse ((vals jmespath-arraylike)) function-args current
    (reverse vals)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'sort)) function-args current)
  (jmespath-call-function-eval-args 'sort ((vals jmespath-array-of-orderables)) function-args current
    (cond
     ((zerop (length vals))
      (vector))
     ((cl-typep (elt vals 0) 'jmespath-json-string)
      (cl-sort vals #'string<))
     ((cl-typep (elt vals 0) 'jmespath-json-number)
      (cl-sort vals #'<)))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'sort_by)) function-args current)
  (cl-destructuring-bind (array-expr key-expr) function-args
    (let ((array-value (jmespath-ast-eval-with-current array-expr current)))
      (cond
       ((not (cl-typep key-expr 'jmespath-ast-quoted-expression))
        (signal 'jmespath-invalid-type-error (list 'sort_by key-expr)))
       ((not (cl-typep array-value 'jmespath-json-array))
        (signal 'jmespath-invalid-type-error (list 'sort_by array-expr)))
       ((= (length array-value) 0)
        array-value)
       (t
        (let ((kv-pairs (cl-mapcar #'(lambda (x)
                                       (cons (jmespath-funeval-expression key-expr x) x))
                                   array-value)))
          (unless (cl-typep (cl-map 'vector #'car kv-pairs) 'jmespath-array-of-orderables)
            (signal 'jmespath-invalid-type-error (list 'sort_by array-expr)))
          (let ((pred (cond
                       ((cl-typep (car (cl-first kv-pairs)) 'jmespath-json-string) #'string<)
                       ((cl-typep (car (cl-first kv-pairs)) 'jmespath-json-number) #'<)
                       (t (signal 'jmespath-invalid-type-error ())))))            
            (cl-map 'vector #'cdr (cl-sort kv-pairs pred :key #'car)))))))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'starts_with)) function-args current)
  (jmespath-call-function-eval-args 'starts_with ((data jmespath-json-string) (candidate jmespath-json-string))
                                    function-args current
    (cond
     ((< (length data) (length candidate))
      :false)
     ((string= (substring data 0 (length candidate)) candidate)
      t)
     (t
      :false))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'sum)) function-args current)
  (jmespath-call-function-eval-args 'sum ((vals jmespath-array-of-numbers)) function-args current
    (cl-reduce #'+ vals :initial-value 0)))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'to_array)) function-args current)
  (jmespath-call-function-eval-args 'to_array ((val jmespath-json-value)) function-args current
    (cl-typecase val
      (jmespath-json-array
       val)
      (t
       (vector val)))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'to_string)) function-args current)
  (jmespath-call-function-eval-args 'to_string ((val jmespath-json-value)) function-args current
    (cl-typecase val
      (jmespath-json-string
       val)
      (t
       (json-encode val)))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'to_number)) function-args current)
  (jmespath-call-function-eval-args 'to_number ((val jmespath-json-value)) function-args current
    (cl-typecase val
      (jmespath-json-string
       (condition-case nil
           (json-parse-string val)
         ((json-parse-error) :null)))
      (jmespath-json-number
       val)
      (t
       :null))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'type)) function-args current)
  (jmespath-call-function-eval-args 'type ((val jmespath-json-value)) function-args current
    (cl-typecase val
      (jmespath-json-null "null")
      (jmespath-json-boolean "boolean")
      (jmespath-json-number "number")
      (jmespath-json-string "string")
      (jmespath-json-array "array")
      (jmespath-json-object "object"))))

(cl-defmethod jmespath-call-function ((_function-identifier (eql 'values)) function-args current)
  (jmespath-call-function-eval-args 'values ((val jmespath-json-object)) function-args current
    (cl-coerce (hash-table-values val) 'vector)))


;;;;
;;;; Parsing
;;;;

;; Where I mention "this is for performance", it comes from lessons
;; learned writing a parser for JSON-complexity data in multi-megabyte
;; files.  In general, I push for tokenizer optimization over parser
;; optimization.

;; If we want our token-matching parser code to use jump-tables, the
;; token tags have to be true constants.  We use macros to get name
;; validation while providing constants to the compiler.
;;
;; (Alternative: create a `cl-symbol-macro' to set up all of these
;; token tags, and wrap that some utility macro.  Every tag reference
;; would have to happen inside one of those macro bodies.  But I kind
;; of like how Emacs puts the tags in `keyword' face right now.)
;;
;; Those jump table token tags also have to support `eq' matches.
;; Symbols aren't self-evaluating, which would make them more fiddly
;; to work with.  We use keywords instead.

(defmacro jmespath-token-tag-AMPERSAND ()            (declare (debug t)) :ampersand)
(defmacro jmespath-token-tag-ASTERISK ()             (declare (debug t)) :asterisk)
(defmacro jmespath-token-tag-AT ()                   (declare (debug t)) :at)
(defmacro jmespath-token-tag-COLON ()                (declare (debug t)) :colon)
(defmacro jmespath-token-tag-COMMA ()                (declare (debug t)) :comma)
(defmacro jmespath-token-tag-DOUBLEAMPERSAND ()      (declare (debug t)) :double-ampersand)
(defmacro jmespath-token-tag-DOUBLEEQUAL ()          (declare (debug t)) :double-equal)
(defmacro jmespath-token-tag-DOUBLEPIPE ()           (declare (debug t)) :double-pipe)
(defmacro jmespath-token-tag-EOF ()                  (declare (debug t)) :eof)
(defmacro jmespath-token-tag-EXCLAMATION ()          (declare (debug t)) :exclamation)
(defmacro jmespath-token-tag-EXCLAMATIONEQUAL ()     (declare (debug t)) :exclamation-equal)
(defmacro jmespath-token-tag-GREATER ()              (declare (debug t)) :greater)
(defmacro jmespath-token-tag-GREATEREQUAL ()         (declare (debug t)) :greater-equal)
(defmacro jmespath-token-tag-LBRACE ()               (declare (debug t)) :left-brace)
(defmacro jmespath-token-tag-LBRACKET ()             (declare (debug t)) :left-bracket)
(defmacro jmespath-token-tag-LBRACKETQUESTIONMARK () (declare (debug t)) :left-bracket-questionmark)
(defmacro jmespath-token-tag-LESSEQUAL ()            (declare (debug t)) :less-equal)
(defmacro jmespath-token-tag-LESS ()                 (declare (debug t)) :less)
(defmacro jmespath-token-tag-LPAREN ()               (declare (debug t)) :left-paren)
(defmacro jmespath-token-tag-PERIOD ()               (declare (debug t)) :period)
(defmacro jmespath-token-tag-PIPE ()                 (declare (debug t)) :pipe)
(defmacro jmespath-token-tag-RBRACE ()               (declare (debug t)) :right-brace)
(defmacro jmespath-token-tag-RBRACKET ()             (declare (debug t)) :right-bracket)
(defmacro jmespath-token-tag-RPAREN ()               (declare (debug t)) :right-paren)

(defmacro jmespath-token-tag-LITERAL ()              (declare (debug t)) :literal)
(defmacro jmespath-token-tag-NUMBER ()               (declare (debug t)) :number)
(defmacro jmespath-token-tag-QUOTEDSTRING ()         (declare (debug t)) :quoted-string)
(defmacro jmespath-token-tag-RAWSTRING ()            (declare (debug t)) :raw-string)
(defmacro jmespath-token-tag-UNQUOTEDSTRING ()       (declare (debug t)) :unquoted-string)

;; Tokens combine a "tag" value that we match on for parsing, plus a
;; position that we can use for rewinding the parse and providing
;; diagnostics.
;;
;; We could use `cl-defstruct' or something similar, but we hand-write
;; our own representation on top of a vector to take advantage of
;; `defsubst' for speed.

(defsubst jmespath-token-p (obj)
  (and (vectorp obj)
       (eq (aref obj 0) 'token)
       (or (integerp (aref obj 1))
           (keywordp (aref obj 1)))))

(cl-deftype jmespath-token ()
  '(satisfies jmespath-token-p))

(defsubst jmespath-make-token (tag position payload)
  (vector 'token position tag payload))

(defsubst jmespath-token-position (token)
  (aref token 1))

(defsubst jmespath-token-tag (token)
  (aref token 2))

(defsubst jmespath-token-payload (token)
  (aref token 3))

(defsubst jmespath-token-tag-matches-p (actual expected)
  (eq actual expected))

(defsubst jmespath-token-is-a-p (token expected-tag)
  (jmespath-token-tag-matches-p (jmespath-token-tag token)
                                expected-tag))

(defun jmespath-token-tag-description (tag)
  "Return a descriptive label for a tag, suitable for a human debugging."
  (cond
   ((keywordp tag) (symbol-name tag))
   ((characterp tag) (string tag))
   ;; This is an implementation failure, not a problem in the user's query.
   ;; Report it as a normal error, not a JMESPath error.
   (t (error "unknown token tag: %s" tag))))

(defun jmespath-token-description (token)
  "Return a descriptive label for a token, suitable for a human debugging."
  (if (jmespath-token-payload token)
      (format "token '%s', value '%s', position %d"
              (jmespath-token-tag-description (jmespath-token-tag token))
              (jmespath-token-payload token)
              (jmespath-token-position token))
    (format "token '%s', position %d"
            (jmespath-token-tag-description (jmespath-token-tag token))
            (jmespath-token-position token))))

(defun jmespath-parse-error (message token)
  "Signal a parse error providing MESSAGE and the details of TOKEN."
  (signal 'jmespath-syntax-error
          (list message
                (list 'tag (jmespath-token-tag-description (jmespath-token-tag token)))
                (list 'payload (jmespath-token-payload token))
                (list 'position (jmespath-token-position token)))))

;; We set up a custom query table so we can use `forward-sexp' to
;; advance across quoted strings below.  The 
(defconst jmespath-query-syntax-table
  (let ((table (make-syntax-table)))
    ;; Single-quotes indicate "raw" strings while backticks indicate
    ;; literal JSON values.
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?` "\"" table)
    table))

(defun jmespath-read-token-literal (token-start)
  (forward-sexp 1)
  ;; Per the spec, we strip the backquotes, unescape any embedded
  ;; backquotes, and then parse as JSON.
  (let* ((text (buffer-substring-no-properties (1+ token-start) (1- (point)))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (save-match-data
        (while (search-forward "\\`" nil t)
          (replace-match "`")))
      (goto-char (point-min))
      (condition-case nil
          (jmespath-make-token (jmespath-token-tag-LITERAL)
                               token-start
                               (json-parse-buffer))
        ((json-end-of-file)
         (jmespath-parse-error "illegal JSON literal"
                               (jmespath-make-token (jmespath-token-tag-LITERAL)
                                                    token-start
                                                    (format "%s" text))))))))

(defun jmespath-read-token-number (token-start)
  (save-match-data
    (if (re-search-forward "\\=-?[0-9]+" nil t)
        (jmespath-make-token (jmespath-token-tag-NUMBER) token-start (string-to-number (match-string 0)))
      (jmespath-parse-error "could not parse number"
                            (jmespath-make-token (jmespath-token-tag-NUMBER)
                                                 token-start
                                                 nil)))))

(defun jmespath-read-token-raw-string (token-start)
  ;; The spec ABNF suggests a difference between "preserved-escape"
  ;; and "raw-string-escape", but I don't see that there is one in the
  ;; text.
  ;;
  ;; The tests ("literal.json", input 3, cases 11 through 13) make it
  ;; clear that backslashes escape single quotes and nothing else.

  (forward-sexp 1)
  (let* ((text (buffer-substring-no-properties (1+ token-start) (1- (point)))))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (save-match-data
        (while (search-forward "\\'" nil t)
          (replace-match "'")))
      (jmespath-make-token (jmespath-token-tag-RAWSTRING)
                           token-start
                           (buffer-string)))))

(defun jmespath-read-token-quoted-string (token-start)
  (condition-case nil
      (progn
        (forward-sexp 1)
        (let* ((text (buffer-substring-no-properties token-start (point))))
          (jmespath-make-token (jmespath-token-tag-QUOTEDSTRING) token-start (json-parse-string text))))
    ((scan-error)
     (signal 'jmespath-syntax-error
             (list (format-message "unterminated string starting at position %d" token-start))))
    ((json-parse-error)
     (signal 'jmespath-syntax-error
             (list (format-message "illegal JSON string starting at position %d" token-start))))))

(defun jmespath-read-token-unquoted-string (token-start)
  (save-match-data
    (re-search-forward "\\=[A-Za-z_][A-Za-z0-9_]*" nil t)
    (jmespath-make-token (jmespath-token-tag-UNQUOTEDSTRING) token-start (match-string 0))))

(defun jmespath-read-token ()
  (skip-syntax-forward " ")

  (if (eobp)
      (jmespath-make-token (jmespath-token-tag-EOF) (point) nil)

    ;; Emacs 27+ compiles this `cond' idiom into a switch statement
    ;; with a jump table, *if* the clauses are all `eq' checks against
    ;; the same variable.
    (let* ((token-start (point))
           (next-char (progn (forward-char 1) (char-before))))
      (cond
       ((eq next-char ?*)  (jmespath-make-token (jmespath-token-tag-ASTERISK) token-start nil))
       ((eq next-char ?.)  (jmespath-make-token (jmespath-token-tag-PERIOD) token-start nil))
       ((eq next-char ?:)  (jmespath-make-token (jmespath-token-tag-COLON) token-start nil))
       ((eq next-char ?,)  (jmespath-make-token (jmespath-token-tag-COMMA) token-start nil))
       ((eq next-char ?@)  (jmespath-make-token (jmespath-token-tag-AT) token-start nil))
       ((eq next-char ?\() (jmespath-make-token (jmespath-token-tag-LPAREN) token-start nil))
       ((eq next-char ?\)) (jmespath-make-token (jmespath-token-tag-RPAREN) token-start nil))
       ((eq next-char ?\]) (jmespath-make-token (jmespath-token-tag-RBRACKET) token-start nil))
       ((eq next-char ?{)  (jmespath-make-token (jmespath-token-tag-LBRACE) token-start nil))
       ((eq next-char ?})  (jmespath-make-token (jmespath-token-tag-RBRACE) token-start nil))
       ;; The "[?" construct can't have any space between the two
       ;; characters.  One of the syntax tests validates that
       ;; behavior.  That's why it has its own token.
       ((eq next-char ?\[) (if (eq (char-after) ??)
                               (progn
                                 (forward-char 1)
                                 (jmespath-make-token (jmespath-token-tag-LBRACKETQUESTIONMARK) token-start nil))
                             (jmespath-make-token (jmespath-token-tag-LBRACKET) token-start nil)))
       ((eq next-char ?&) (if (eq (char-after) ?&)
                              (progn
                                (forward-char 1)
                                (jmespath-make-token (jmespath-token-tag-DOUBLEAMPERSAND) token-start nil))
                            (jmespath-make-token (jmespath-token-tag-AMPERSAND) token-start nil)))
       ((eq next-char ?|) (if (eq (char-after) ?|)
                              (progn
                                (forward-char 1)
                                (jmespath-make-token (jmespath-token-tag-DOUBLEPIPE) token-start nil))
                            (jmespath-make-token (jmespath-token-tag-PIPE) token-start nil)))
       ((eq next-char ?<) (if (eq (char-after) ?=)
                              (progn
                                (forward-char 1)
                                (jmespath-make-token (jmespath-token-tag-LESSEQUAL) token-start nil))
                            (jmespath-make-token (jmespath-token-tag-LESS) token-start nil)))
       ((eq next-char ?=) (cond
                           ((null (char-after))
                            (signal 'jmespath-syntax-error
                                    (list (format-message "illegal EOF at position %d" (1+ (point))))))
                           ((eq (char-after) ?=)
                            (forward-char 1)
                            (jmespath-make-token (jmespath-token-tag-DOUBLEEQUAL) token-start nil))
                           (t
                            (signal 'jmespath-syntax-error
                                    (list (format-message "illegal character `%s' looking for token at position %d"
                                                          (string (char-after))
                                                          (point)))))))
       ((eq next-char ?>) (if (eq (char-after) ?=)
                              (progn
                                (forward-char 1)
                                (jmespath-make-token (jmespath-token-tag-GREATEREQUAL) token-start nil))
                            (jmespath-make-token (jmespath-token-tag-GREATER) token-start nil)))
       ((eq next-char ?!) (if (eq (char-after) ?=)
                              (progn
                                (forward-char 1)
                                (jmespath-make-token (jmespath-token-tag-EXCLAMATIONEQUAL) token-start nil))
                            (jmespath-make-token (jmespath-token-tag-EXCLAMATION) token-start nil)))
       ((eq next-char ?-)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?0)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?1)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?2)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?3)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?4)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?5)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?6)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?7)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?8)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?9)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?-)  (forward-char -1) (jmespath-read-token-number token-start))
       ((eq next-char ?`)  (forward-char -1) (jmespath-read-token-literal token-start))
       ((eq next-char ?')  (forward-char -1) (jmespath-read-token-raw-string token-start))
       ((eq next-char ?\") (forward-char -1) (jmespath-read-token-quoted-string token-start))
       ((eq next-char ?A)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?B)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?C)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?D)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?E)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?F)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?G)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?H)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?I)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?J)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?K)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?L)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?M)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?N)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?O)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?P)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?Q)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?R)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?S)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?T)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?U)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?V)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?W)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?X)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?Y)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?Z)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?a)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?b)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?c)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?d)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?e)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?f)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?g)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?h)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?i)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?j)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?k)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?l)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?m)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?n)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?o)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?p)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?q)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?r)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?s)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?t)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?u)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?v)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?w)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?x)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?y)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?z)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       ((eq next-char ?_)  (forward-char -1) (jmespath-read-token-unquoted-string token-start))
       (t
        (signal 'jmespath-syntax-error
                (list (format-message "illegal character `%s' looking for token at position %d"
                                      (string next-char)
                                      (1- (point))))))))))

;; We call these functions a lot, and they're simple.  Turn them into
;; `defsubst' definitions as well.

;; Read, but throw an error if it's not what we expect.  Use this when
;; we have an expected context such as "start of parenthetical
;; expression".
(defsubst jmespath-expect-token (expected-tag)
  (let ((tok (jmespath-read-token)))
    (unless (jmespath-token-is-a-p tok expected-tag)
      (jmespath-parse-error (format "expected token tagged '%s'" expected-tag) tok))
    tok))

;; Token objects include the position of the start of the token, in
;; the current buffer.  That means rolling back is just a matter of
;; moving point.
(defsubst jmespath-rollback-to-token (tok)
  (goto-char (jmespath-token-position tok))
  tok)

;; This "peek" implementation is inefficient, because we can end up
;; scanning long strings only to throw them away.  (The fast
;; alternative is to just detect the token type from the first
;; characters of lookahead, without bothering to move point forward.)
;; That would require duplicating and tweaking a whole bunch of
;; `jmespath-read-token', however.
(defsubst jmespath-peek-token ()
  (jmespath-rollback-to-token (jmespath-read-token)))

;; This alias for `jmespath-read-token' can make it easier for
;; programmers to express intent in the parser code: "advance, but throw
;; away the data".  Usually it comes after a `jmespath-peek-token' that
;; provided all of the data that we need for our branch choice.
(defsubst jmespath-drop-token ()
  (jmespath-read-token))

;; Let's group all of the things that the spec says can be expressions.
;; Rows go roughly from high to low precedence.  (In particular, notice
;; that an identifier followed by a trailing parenthesis is always a
;; function call).
;;
;; token | op type  | non-terminal          | notes
;; ------|----------|-----------------------|-------------------------------
;; @     |          | current-node
;; *     |          | wildcard
;; '...' |          | raw-string
;; `...` |          | literal
;; id    | postfix  | function-expression   | identifier with a trailing parenthesis
;; id    |          | identifier
;; {     | explicit | multi-select-hash
;; [     | explicit | multi-select-list
;; (     | explicit | paren-expression
;;       | postfix  | index-expression      | "followed by" left-bracket
;;       | postfix  | sub-expression        | "followed by" period
;; !     | prefix   | not-expression        | binds less tightly than the two below
;;       | infix    | comparator-expression | "followed by" one of {"==", "!=", "<", "<=", ">", ">="}
;;       | infix    | and-expression        | "followed by" double-ampersand
;;       | infix    | or-expression         | "followed by" double-pipe
;;       | infix    | pipe-expression       | "followed by" single-pipe

;; We model this precdence list in our
;; 'parse-expression-{pipe,orelse,andthen,comparison,not}' functions:
;; each function is aware of the chaining operator that it accepts,
;; *plus* the higher-priority operators.  Each function knows to bail
;; out if it sees an operator that a higher level needs to handle.

;; Notice that we have not aggressively optimized the parser to
;; eliminate redundant "get another token" operations.  We peek with
;; abandon, and roll back as necessary because it lets us group
;; "sensible" token references into "sensible" functions.  With our
;; `defsubst' definitions, most of what we're doing turns into point
;; jiggling back and forth in the current buffer.

;; This is a convenience macro that makes it easy to express something
;; that will compile into a jump table.  Yes, I know that it's
;; annoying that the ELSE-CLAUSE comes at the top.
;;
;; Once again, we use the Emacs 27+ switch/jump-table feature.
(defmacro jmespath-token-dispatch (token-clause else-clause &rest body-clauses)
  (declare (indent 2) (debug ((sexp form) sexp &rest (sexp &rest form))))
  (let* ((token-var (car token-clause))
         (token-expr (cadr token-clause))
         (tag-var (gensym))
         (match-clauses (cl-loop for (tag . body) in body-clauses
                                 collect `((eq ,tag-var ,tag)
                                           ,@body))))
    `(let* ((,token-var ,token-expr)
            (,tag-var (jmespath-token-tag ,token-var)))
       (cond
        ,@match-clauses
        (t ,else-clause)))))

;; It's helpful to the human reader if we can write parser code in
;; terms of an "empty" AST as a placeholder.  This is is that alias.
(defun jmespath-ast-empty () (jmespath-ast-current-node))

;; A generic entry point.  This is the recursive entry point, too.
(defun jmespath-parse-expression ()
  (jmespath-parse-expression-pipe))

(defun jmespath-parse-expression-pipe ()
  (cl-loop with left-operand = (jmespath-parse-expression-orelse)
           do (jmespath-token-dispatch (operator (jmespath-read-token))
                  (jmespath-parse-error "expected connecting operator '|' or EOF" operator)
                ;; These terminate expressions, but code higher in the
                ;; call chain has to determine whether it's
                ;; appropriate to terminate in our current context.
                ((jmespath-token-tag-EOF)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACKET)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RPAREN)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-COMMA)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))

                ((jmespath-token-tag-PIPE)
                 (let ((right-operand (jmespath-parse-expression-orelse)))
                   (setq left-operand (jmespath-ast-pipe
                                       :left-expr left-operand
                                       :right-expr right-operand)))))))

(defun jmespath-parse-expression-orelse ()
  (cl-loop with left-operand = (jmespath-parse-expression-andthen)
           do (jmespath-token-dispatch (operator (jmespath-read-token))
                  (jmespath-parse-error "expected connecting operator '||', '|', or EOF" operator)
                ;; These terminate expressions, but code higher in the
                ;; call chain has to determine whether it's
                ;; appropriate to terminate via these tokens.
                ((jmespath-token-tag-EOF)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACKET)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RPAREN)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-COMMA)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))

                ((jmespath-token-tag-PIPE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACKET)
                 ;; For multi-select lists terminating expressions.
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACE)
                 ;; For multi-select hashes terminating expressions.
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-DOUBLEPIPE)
                 (let ((right-operand (jmespath-parse-expression-andthen)))
                   (setq left-operand (jmespath-ast-orelse
                                       :left-expr left-operand
                                       :right-expr right-operand)))))))

(defun jmespath-parse-expression-andthen ()
  (cl-loop with left-operand = (jmespath-parse-expression-comparison)
           do (jmespath-token-dispatch (operator (jmespath-read-token))
                  (jmespath-parse-error "expected connecting operator '&&', '||', '|', or EOF" operator)
                ;; These terminate expressions, but code higher in the
                ;; call chain has to determine whether it's
                ;; appropriate to terminate via these tokens.
                ((jmespath-token-tag-EOF)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACKET)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RPAREN)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-COMMA)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))

                ((jmespath-token-tag-PIPE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-DOUBLEPIPE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-DOUBLEAMPERSAND)
                 (let ((right-operand (jmespath-parse-expression-comparison)))
                   (setq left-operand (jmespath-ast-andthen
                                       :left-expr left-operand
                                       :right-expr right-operand)))))))

;; The spec doesn't say anything about operator precedence among
;; comparisons.
(defun jmespath-parse-expression-comparison ()
  (cl-loop with left-operand = (jmespath-parse-expression-not)
           do (jmespath-token-dispatch (operator (jmespath-read-token))
                  (jmespath-parse-error "expected connecting operator '&&', '||', '|', or EOF" operator)
                ;; These terminate expressions, but code higher in the
                ;; call chain has to determine whether it's
                ;; appropriate to terminate via these tokens.
                ((jmespath-token-tag-EOF)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACKET)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RBRACE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-RPAREN)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-COMMA)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))

                ((jmespath-token-tag-PIPE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-DOUBLEPIPE)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))
                ((jmespath-token-tag-DOUBLEAMPERSAND)
                 (jmespath-rollback-to-token operator)
                 (cl-return left-operand))

                ((jmespath-token-tag-DOUBLEEQUAL)
                 (let ((right-operand (jmespath-parse-expression-not)))
                   (setq left-operand (jmespath-ast-comparison-==
                                       :left-expr left-operand
                                       :right-expr right-operand))))
                ((jmespath-token-tag-EXCLAMATIONEQUAL)
                 (let ((right-operand (jmespath-parse-expression-not)))
                   (setq left-operand (jmespath-ast-comparison-!=
                                       :left-expr left-operand
                                       :right-expr right-operand))))
                ((jmespath-token-tag-LESS)
                 (let ((right-operand (jmespath-parse-expression-not)))
                   (setq left-operand (jmespath-ast-comparison-<
                                       :left-expr left-operand
                                       :right-expr right-operand))))
                ((jmespath-token-tag-LESSEQUAL)
                 (let ((right-operand (jmespath-parse-expression-not)))
                   (setq left-operand (jmespath-ast-comparison-<=
                                       :left-expr left-operand
                                       :right-expr right-operand))))
                ((jmespath-token-tag-GREATER)
                 (let ((right-operand (jmespath-parse-expression-not)))
                   (setq left-operand (jmespath-ast-comparison->
                                       :left-expr left-operand
                                       :right-expr right-operand))))
                ((jmespath-token-tag-GREATEREQUAL)
                 (let ((right-operand (jmespath-parse-expression-not)))
                   (setq left-operand (jmespath-ast-comparison->=
                                       :left-expr left-operand
                                       :right-expr right-operand)))))))

(defun jmespath-parse-expression-not ()
  (jmespath-token-dispatch (operator (jmespath-peek-token))
      (jmespath-parse-expression-left-to-right)
    ((jmespath-token-tag-EOF)
     (jmespath-parse-error "expected basic expression " operator))
    ((jmespath-token-tag-PIPE)
     (jmespath-parse-error "expected basic expression " operator))
    ((jmespath-token-tag-DOUBLEPIPE)
     (jmespath-parse-error "expected basic expression" operator))
    ((jmespath-token-tag-DOUBLEAMPERSAND)
     (jmespath-parse-error "expected basic expression" operator))
    ((jmespath-token-tag-EXCLAMATION)
     (jmespath-drop-token)
     (jmespath-ast-not :right-expr (jmespath-parse-expression-not)))))

(defun jmespath-parse-expression-left-to-right ()
  (cl-loop with left-expr = (jmespath-token-dispatch (tok (jmespath-peek-token))
                                (jmespath-parse-expression-basic)
                              ((jmespath-token-tag-EOF)
                               (jmespath-parse-error "unexpected EOF at start of expression" tok))
                              ((jmespath-token-tag-PERIOD)
                               (jmespath-parse-error "illegal period at start of expression" tok)))
           do (jmespath-token-dispatch (tok (jmespath-peek-token))
                  (cl-return left-expr)
                ((jmespath-token-tag-PERIOD)
                 (setq left-expr (jmespath-parse-expression-subexpr left-expr)))
                ((jmespath-token-tag-LBRACKETQUESTIONMARK)
                 (setq left-expr (jmespath-parse-expression-filter left-expr)))
                ((jmespath-token-tag-LBRACKET)
                 (setq left-expr (jmespath-parse-expression-bracket-specifier left-expr))))))

(defun jmespath-parse-expression-basic ()
  (jmespath-token-dispatch (tok (jmespath-peek-token))
      (jmespath-parse-error "expected basic expression" tok)
    ((jmespath-token-tag-LPAREN) (jmespath-parse-paren-expression))
    ((jmespath-token-tag-LITERAL) (jmespath-ast-literal :tree (jmespath-token-payload (jmespath-read-token))))
    ;; Note that rawstrings are just a convenient way to express
    ;; literal JSON strings They don't need their own AST node type.
    ((jmespath-token-tag-RAWSTRING) (jmespath-ast-literal :tree (jmespath-token-payload (jmespath-read-token))))
    ((jmespath-token-tag-AT) (jmespath-drop-token) (jmespath-ast-current-node))
    ;; We have explicit calls to `jmespath-ast-empty' here because the
    ;; language structure assumes that these elements operate *on*
    ;; something, and this context is the left-most part of an
    ;; expression.  They operate implicitly on the current context.
    ((jmespath-token-tag-QUOTEDSTRING) (jmespath-parse-selector-identifier (jmespath-ast-empty)))
    ((jmespath-token-tag-UNQUOTEDSTRING) (jmespath-parse-selector-identifier/funcall (jmespath-ast-empty)))
    ((jmespath-token-tag-LBRACE) (jmespath-parse-multi-select-hash (jmespath-ast-empty)))
    ((jmespath-token-tag-ASTERISK)
     (jmespath-drop-token)
     (jmespath-ast-extract-field-wildcard :left-expr (jmespath-ast-empty)))
    ((jmespath-token-tag-LBRACKETQUESTIONMARK) (jmespath-parse-expression-filter (jmespath-ast-empty)))
    ;; This is completely ambiguous.  We can't tell what a left
    ;; bracket here means without looking at what's inside.
    ((jmespath-token-tag-LBRACKET) (jmespath-parse-expression-unknown-bracket (jmespath-ast-empty)))))

(defun jmespath-parse-expression-subexpr (left-expr)
  (jmespath-expect-token (jmespath-token-tag-PERIOD))
  (jmespath-token-dispatch (tok (jmespath-peek-token))
      (jmespath-parse-error "expected selector expression period right-hand side" tok)
    ((jmespath-token-tag-QUOTEDSTRING) (jmespath-parse-selector-identifier left-expr))
    ((jmespath-token-tag-UNQUOTEDSTRING) (jmespath-parse-selector-identifier/funcall left-expr))
    ((jmespath-token-tag-ASTERISK)
     (jmespath-read-token)
     (jmespath-ast-extract-field-wildcard :left-expr left-expr))
    ((jmespath-token-tag-LBRACE)
     (jmespath-parse-multi-select-hash left-expr))
    ;; Multi-select lists are the only bracket constructs that come
    ;; after a "." subexpression delimiter.
    ((jmespath-token-tag-LBRACKET)
     (jmespath-parse-multi-select-list left-expr))))

(defun jmespath-parse-expression-index/slice (left-expr)
  (cl-block nil
    (let ((starting-tok (jmespath-peek-token))
          start stop step)
      (when (jmespath-token-is-a-p (jmespath-peek-token) (jmespath-token-tag-NUMBER))
        (setq start (jmespath-token-payload (jmespath-read-token))))

      (jmespath-token-dispatch (tok (jmespath-read-token))
          (jmespath-parse-error "unrecognized index/slice contents" starting-tok)
        ((jmespath-token-tag-RBRACKET)
         (cl-return
          (jmespath-ast-extract-index :left-expr left-expr
                                      :index-value start)))
        ((jmespath-token-tag-COLON)
         nil))

      (when (jmespath-token-is-a-p (jmespath-peek-token) (jmespath-token-tag-NUMBER))
        (setq stop (jmespath-token-payload (jmespath-read-token))))

      (jmespath-token-dispatch (tok (jmespath-read-token))
          (jmespath-parse-error "unrecognized index/slice contents" starting-tok)
        ((jmespath-token-tag-RBRACKET)
         (cl-return
          (jmespath-ast-extract-slice :left-expr left-expr
                                      :start-value start
                                      :stop-value stop)))
        ((jmespath-token-tag-COLON)
         nil))

      (when (jmespath-token-is-a-p (jmespath-peek-token) (jmespath-token-tag-NUMBER))
        (setq step (jmespath-token-payload (jmespath-read-token))))

      (jmespath-token-dispatch (tok (jmespath-read-token))
          (jmespath-parse-error "unrecognized index/slice contents" starting-tok)
        ((jmespath-token-tag-RBRACKET)
         (cl-return
          (jmespath-ast-extract-slice :left-expr left-expr
                                      :start-value start
                                      :stop-value stop
                                      :step-value step)))))))

;; These are the LBRACKET cases that concatenate against another
;; expression to the left.  The next token could be a number (an index
;; *or* a slice), a colon (slice again), an asterisk (wildcard), or a
;; right brace (flatten), but not an arbitrary expression
;; (multi-select list) and not a question mark (since "[?" forms its
;; own token).

(defun jmespath-parse-expression-bracket-specifier (left-expr)
  (jmespath-expect-token (jmespath-token-tag-LBRACKET))
  (jmespath-token-dispatch (tok (jmespath-peek-token))
      (jmespath-parse-error "expected index/slice/wildcard/filter/empty)" tok)
    ((jmespath-token-tag-RBRACKET)
     (jmespath-drop-token)
     (jmespath-ast-flatten :left-expr left-expr))
    ((jmespath-token-tag-ASTERISK)
     (jmespath-drop-token)
     (jmespath-expect-token (jmespath-token-tag-RBRACKET))
     (jmespath-ast-extract-index-wildcard :left-expr left-expr))
    ((jmespath-token-tag-NUMBER)
     (jmespath-parse-expression-index/slice left-expr))
    ((jmespath-token-tag-COLON)
     (jmespath-parse-expression-index/slice left-expr))))

;; These LBRACKET cases are when the LBRACKET turns up as the first
;; thing in an expression.  We have every situation listed above, plus
;; multi-select list this time.

(defun jmespath-parse-expression-unknown-bracket (left-expr)
  (let ((left-bracket-token (jmespath-expect-token (jmespath-token-tag-LBRACKET))))
    (jmespath-token-dispatch (tok (jmespath-peek-token))
        ;; If we don't recognize it, it's a multi-select list.
        (progn (jmespath-rollback-to-token left-bracket-token)
               (jmespath-parse-multi-select-list left-expr))
      ((jmespath-token-tag-RBRACKET)
       (jmespath-drop-token)
       (jmespath-ast-flatten :left-expr left-expr))
      ((jmespath-token-tag-ASTERISK)
       (jmespath-drop-token)
       ;; Asterisks could be wildcards or could be multi-select list
       ;; expressions.  We have to look ahead.
       (cond
        ((jmespath-token-is-a-p (jmespath-read-token) (jmespath-token-tag-RBRACKET))
         (jmespath-ast-extract-index-wildcard :left-expr left-expr))
        (t
         (jmespath-rollback-to-token left-bracket-token)
         (jmespath-parse-multi-select-list left-expr))))
      ((jmespath-token-tag-NUMBER)
       (jmespath-parse-expression-index/slice left-expr))
      ((jmespath-token-tag-COLON)
       (jmespath-parse-expression-index/slice left-expr)))))

(defun jmespath-parse-multi-select-hash (left-expr)
  (jmespath-expect-token (jmespath-token-tag-LBRACE))
  (cl-loop with elements = (list (jmespath-parse-keyval-expr))
           until (jmespath-token-is-a-p (jmespath-peek-token) (jmespath-token-tag-RBRACE))
           do (jmespath-expect-token (jmespath-token-tag-COMMA))
           do (push (jmespath-parse-keyval-expr) elements)
           finally do (jmespath-expect-token (jmespath-token-tag-RBRACE))
           finally return (jmespath-ast-multi-select-hash :left-expr left-expr :pairs (nreverse elements))))

(defun jmespath-parse-keyval-expr ()
  (let* ((identifier
          (jmespath-token-dispatch (tok (jmespath-read-token))
              (jmespath-parse-error "invalid multiselect hash" tok)
            ((jmespath-token-tag-QUOTEDSTRING) (jmespath-token-payload tok))
            ((jmespath-token-tag-UNQUOTEDSTRING) (jmespath-token-payload tok)))))
    (jmespath-expect-token (jmespath-token-tag-COLON))
    (cons identifier (jmespath-parse-expression))))

(defun jmespath-parse-multi-select-list (left-expr)
  (jmespath-expect-token (jmespath-token-tag-LBRACKET))
  (cl-loop with elements = (list (jmespath-parse-expression))
           until (jmespath-token-is-a-p (jmespath-peek-token) (jmespath-token-tag-RBRACKET))
           do (jmespath-expect-token (jmespath-token-tag-COMMA))
           do (push (jmespath-parse-expression) elements)
           finally do (jmespath-expect-token (jmespath-token-tag-RBRACKET))
           finally return (jmespath-ast-multi-select-list :left-expr left-expr :elements (nreverse elements))))

(defun jmespath-parse-paren-expression ()
  (jmespath-expect-token (jmespath-token-tag-LPAREN))
  (prog1
      (jmespath-parse-expression)
    (jmespath-expect-token (jmespath-token-tag-RPAREN))))

(defun jmespath-parse-selector-identifier (left-expr)
  (jmespath-token-dispatch (tok (jmespath-read-token))
      (jmespath-parse-error "expected one of QUOTEDSTRING, UNQUOTEDSTRING" tok)
    ((jmespath-token-tag-QUOTEDSTRING)
     (jmespath-ast-extract-field :left-expr left-expr :field-name (jmespath-token-payload tok)))
    ((jmespath-token-tag-UNQUOTEDSTRING)
     (jmespath-ast-extract-field :left-expr left-expr :field-name (jmespath-token-payload tok)))))

(defun jmespath-parse-selector-identifier/funcall (left-expr)
  (jmespath-token-dispatch (idtok (jmespath-read-token))
      (jmespath-parse-error "expected one of QUOTEDSTRING, UNQUOTEDSTRING" idtok)
    ((jmespath-token-tag-QUOTEDSTRING)
     (jmespath-rollback-to-token idtok)
     (jmespath-parse-selector-identifier left-expr))
    ((jmespath-token-tag-UNQUOTEDSTRING)
     (jmespath-token-dispatch (parentok (jmespath-read-token))
         (progn
           (jmespath-rollback-to-token idtok)
           (jmespath-parse-selector-identifier left-expr))
       ((jmespath-token-tag-LPAREN)
        (progn
          (jmespath-rollback-to-token idtok)
          (jmespath-parse-funcall-arglist left-expr)))))))

(defun jmespath-parse-funcall-arglist (left-expr)
  (let* ((idtok (jmespath-read-token))
         (_left-paren (jmespath-expect-token (jmespath-token-tag-LPAREN))))
    (jmespath-token-dispatch (maybe-rparen-tok (jmespath-peek-token))
        (cl-loop with expressions = (list (jmespath-parse-funcall-argument))
                 until (jmespath-token-is-a-p (jmespath-peek-token) (jmespath-token-tag-RPAREN))
                 do (jmespath-expect-token (jmespath-token-tag-COMMA))
                 do (push (jmespath-parse-funcall-argument) expressions)
                 finally do (jmespath-expect-token (jmespath-token-tag-RPAREN))
                 finally return (jmespath-ast-function-call :left-expr left-expr
                                                            :function-name (jmespath-token-payload idtok)
                                                            :function-args (nreverse expressions)))
      ;; Look for a close-paren that comes immediately after the
      ;; open-paren.  It's easier to check for that special case here
      ;; than it is to embed it into the loop.  'max', 'min', and
      ;; 'not_null' all accept empty args as a degenerate case.
      ((jmespath-token-tag-RPAREN)
       (jmespath-drop-token)
       (jmespath-ast-function-call :left-expr left-expr
                                   :function-name (jmespath-token-payload idtok)
                                   :function-args nil)))))

(defun jmespath-parse-funcall-argument ()
  ;; AMPERSAND indicates "expression that is not immediately
  ;; evaluated", and per the spec it's only valid inside a function
  ;; argument list.
  (jmespath-token-dispatch (tok (jmespath-peek-token))
      (jmespath-parse-expression)
    ((jmespath-token-tag-AMPERSAND)
     (jmespath-drop-token)
     (jmespath-ast-quoted-expression :child-expr (jmespath-parse-expression)))))

(defun jmespath-parse-expression-filter (left-expr)
  (jmespath-expect-token (jmespath-token-tag-LBRACKETQUESTIONMARK))
  (prog1
      (jmespath-ast-filter-expression :left-expr left-expr
                                      :filter-expr (jmespath-parse-expression))
    (jmespath-expect-token (jmespath-token-tag-RBRACKET))))

(defun jmespath-query-string-to-ast (query-string)
  "Parse a JMESPath string and return an evaluation tree."
  ;; Use this as the entry point for parsing; it handles EOF detection
  ;; and linearization.
  (with-temp-buffer
    (with-syntax-table jmespath-query-syntax-table
      (insert query-string)
      (goto-char (point-min))
      (if (jmespath-token-is-a-p (jmespath-peek-token) (jmespath-token-tag-EOF))
          (jmespath-linearize (jmespath-ast-empty))
        (prog1
            (jmespath-linearize (jmespath-parse-expression))
          (jmespath-expect-token (jmespath-token-tag-EOF)))))))

;;;
;;; Entry points
;;;

;;;###autoload
(defun jmespath-compile-query (query-string-or-sexp)
  "Compile QUERY-STRING into a re-usable form."
  (cl-typecase query-string-or-sexp
    (string
     (jmespath-query-string-to-ast query-string-or-sexp))
    (jmespath-ast-node
     query-string-or-sexp)
    (t
     (error "invalid value for query to compile"))))

;;;###autoload
(defun jmespath-search (query-string json-object)
  "Evaluate JMESPath QUERY-STRING against JSON-OBJECT.

QUERY-STRING may also be a pre-compiled value from
`jmespath-compile-query'."
  (cl-typecase query-string
    (string
     (jmespath-ast-eval-with-current
      (jmespath-query-string-to-ast query-string)
      json-object))
    (jmespath-ast-node
     (jmespath-ast-eval-with-current query-string json-object))
    (t
     :null)))

(provide 'jmespath)
