;; Defining various strings and characters.
;; It is better not to embed literal strings, characters, or numerals in your 
;; code (as "magic strings" or "magic numbers"). Instead use 
;; identifiers with suggestive names.
;; These exist in their own file because we'll need them
;; in several places.

(define prompt-string          "input > ")
(define result-label           "result: ")
(define syntax-tree-label      "syntax tree: ")

(define quit-expr "quit")

(define multiply-char          '#\* )
(define divide-char            '#\/ )
(define add-char               '#\+ )
(define subtract-char          '#\- )
(define negation-char          '#\- )
(define equal-char             '#\= )

(define space                  '#\ )
(define left-paren             '#\( )
(define right-paren            '#\) )

(define point-char             '#\. )
(define exponent-char          '#\^ )
(define norm-char              '#\% )