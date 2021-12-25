;; Copyright (C) 2021 Derek Upham

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

;; Instructions:
;;
;; Load this file.
;;
;; Invoke `jmespath-run-test-files' and tell the command where to find
;; the JMESPath test files.  (Example: "~/src/jmespath.test/tests".)
;;
;; If the resulting "*JMESPath Tests*" pop-up buffer has any contents,
;; then there's a regression.

;; FIXME: What's the right way of structuring/running this in $CURRENT_YEAR?

;; FIXME: Support the "benchmarks.json" file.

(require 'jmespath)

;;;;
;;;; Testing
;;;;

(defun jmespath-run-test-case (test-file test-def-idx test-case-idx given-input test-case)
  (let* ((jmespath-query (jmespath-json-object-get-field test-case "expression"))
         (expect-error (jmespath-json-object-has-field test-case "error"))
         (expected-result (cond ((jmespath-json-object-has-field test-case "result")
                                 (jmespath-json-object-get-field test-case "result"))
                                ((jmespath-json-object-has-field test-case "error")
                                 (jmespath-json-object-get-field test-case "error"))
                                (t
                                 (error "no expected-result found"))))
         ;; We don't want to trap errors for debugging when running
         ;; error tests.
         (debug-on-error (if expect-error nil debug-on-error))
         ast
         commands
         actual-result
         (status :failure))

    (condition-case-unless-debug err
        (progn
          (setq ast (jmespath-query-string-to-ast jmespath-query))
          (setq actual-result (jmespath-ast-eval-with-current ast given-input))
          (setq status (if (eq (jmespath-json-equal-compare expected-result actual-result) t)
                           :success
                         :failure)))
      (jmespath-invalid-arity-error
       (setq actual-result "invalid-arity")
       (setq status (if (equal expected-result actual-result)
                        :success
                      :failure)))
      (jmespath-invalid-type-error
       (setq actual-result "invalid-type")
       (setq status (if (equal expected-result actual-result)
                        :success
                      :failure)))
      (jmespath-invalid-value-error
       (setq actual-result "invalid-value")
       (setq status (if (equal expected-result actual-result)
                        :success
                      :failure)))
      (jmespath-syntax-error
       (setq actual-result "syntax")
       (setq status (if (equal expected-result actual-result)
                        :success
                      :failure)))
      (jmespath-unknown-function-error
       (setq actual-result "unknown-function")
       (setq status (if (equal expected-result actual-result)
                        :success
                      :failure)))
      (error
       (setq actual-result err)
       (setq status :error)))

    (list jmespath-query expected-result ast commands actual-result status)))

(defun jmespath-elisp-to-json (obj)
  (let ((json-object-type 'hash-table)
        (json-array-type 'vector)
        (json-false :false)
        (json-null :null))
    (condition-case err
        (json-encode obj)
      ((error) "#<not JSON>"))))

(defun jmespath-run-test-definition (test-file test-def-idx test-def)
  ;; One "given" key with the input data.
  ;; One "cases" key with a list of JMESPath queries and their outputs.
  (let* ((given-input (jmespath-json-object-get-field test-def "given"))
         (test-cases (jmespath-json-object-get-field test-def "cases"))
         (print-circle nil))
    (cl-loop for test-case-idx from 1
             for test-case across test-cases
             do (cl-destructuring-bind (jmespath-query expected-result ast commands actual-result status)
                    (jmespath-run-test-case test-file test-def-idx test-case-idx given-input test-case)
                  (cl-ecase status
                    ((:success)
                     ;; (insert (format "* Test file %s, input %d, case %2d, status SUCCESS\n" (file-name-nondirectory test-file) test-def-idx test-case-idx))
                     nil)
                    ((:failure)
                     (insert (format "* Test file %s, input %2d, case %2d, status FAILURE\n" (file-name-nondirectory test-file) test-def-idx test-case-idx))
                     (insert (format "JSON data:           %s\n" (jmespath-elisp-to-json given-input)))
                     (insert (format "JMESPath expression: %s\n" jmespath-query))
                     (insert (format "Expected result:     %s ⇥elisp↦ %S\n" (jmespath-elisp-to-json expected-result) expected-result))
                     (insert (format "AST:                 %S\n" ast))
                     (insert (format "Commands:            %S\n" commands))
                     (insert (format "Actual result:       %s ⇥elisp↦ %S\n" (jmespath-elisp-to-json actual-result) actual-result))
                     (insert (format "Replay:              %S\n" `(jmespath-search ,jmespath-query ',given-input))))
                    ((:error)
                     (insert (format "* Test file %s, input %2d, case %2d, status ERROR\n" (file-name-nondirectory test-file) test-def-idx test-case-idx))
                     (insert (format "JSON data:           %s\n" (jmespath-elisp-to-json given-input)))
                     (insert (format "JMESPath expression: %s\n" jmespath-query))
                     (insert (format "Expected result:     %s ⇥elisp↦ %S\n" (jmespath-elisp-to-json expected-result) expected-result))
                     (insert (format "AST:                 %S\n" ast))
                     (insert (format "Commands:            %S\n" commands))
                     (insert (format "Actual result:       %s ⇥elisp↦ %S\n" (jmespath-elisp-to-json actual-result) actual-result))
                     (insert (format "Replay:              %S\n" `(jmespath-search ,jmespath-query ',given-input)))))))))

(defun jmespath-run-test-file (filename object-type)
  (let ((test-definitions-list
         (with-temp-buffer
           (insert-file-contents filename)
           (json-parse-buffer :object-type object-type))))
    (cl-loop for test-def-idx from 1
             for test-def across test-definitions-list
             do (jmespath-run-test-definition filename test-def-idx test-def))))

(defun jmespath-run-test-files (tests-directory &optional object-type)
  (interactive "DPath to test files: ")
  ;; Use symbol `hash-table' to parse JSON as hash tables by default,
  ;; to double-check support for that feature.
  (unless object-type (setq object-type 'alist))
  (let ((report-buffer (get-buffer-create "*JMESPath Tests*")))
    (with-current-buffer report-buffer
      (setq buffer-read-only t)
      (let ((buffer-read-only nil))
        (erase-buffer)
        (outline-mode)
        ;; "benchmarks.json" is a completely different format, not tests.
        ;; It doesn't belong in these lists.
        (let* ((filenames '(
                            "basic.json"
                            "boolean.json"
                            "current.json"
                            "escape.json"
                            "filters.json"
                            "functions.json"
                            "identifiers.json"
                            "indices.json"
                            "literal.json"
                            "multiselect.json"
                            "pipe.json"
                            "slice.json"
                            "syntax.json"
                            "unicode.json"
                            "wildcard.json"
                            )))
          (cl-loop for f in filenames
                   do (let ((current-file (expand-file-name f tests-directory)))
                        (jmespath-run-test-file current-file object-type)))))

      (visual-line-mode -1)
      (toggle-truncate-lines 1)
      (outline-hide-body)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    (pop-to-buffer report-buffer)))
