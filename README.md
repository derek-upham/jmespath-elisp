# jmespath-elisp

JMESPath (https://jmespath.org) is a query language for JSON.  The
JMESPath website includes a documentation, a specification, a test
suite, and links to implementations in various languages.

This package implements JMESpath in Emacs Lisp.  All names use a
"jmespath-" prefix.  The entry point is the `jmespath-search`
function.  Call it by passing in a JMESPath string and a JSON value
(a parsed JSON data structure).  Example, straight from the
https://jmespath.org/ home page:

```
(jmespath-search
  "locations[?state == 'WA'].name | sort(@) | {WashingtonCities: join(', ', @)}"
  (json-parse-string "{\"locations\": [{\"name\": \"Seattle\", \"state\": \"WA\"},
                                       {\"name\": \"New York\", \"state\": \"NY\"},
                                       {\"name\": \"Bellevue\", \"state\": \"WA\"},
                                       {\"name\": \"Olympia\", \"state\": \"WA\"}]}"))
    ⇒ #s(hash-table size 1 test equal rehash-size 1.5 rehash-threshold 0.8125
                    data ("WashingtonCities" "Bellevue, Olympia, Seattle"))
```

See the source file `jmespath.el` for more commentary.
