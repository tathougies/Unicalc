;; File:    parser.rkt
;; Author:  You!
;; A starting point for the unilang parser

;; We need to make QLs here.  So either include your make-UQL function
;; or just load your whole unicalc API here.
(define (make-UQL quant err num den)
  (list (list quant err) num den))

(load "symbol-definitions.rkt")
(load "tokenizer.rkt")
(load "tester.rkt")

;; An interactive read-eval-print loop just for the parser.  It's
;; not necessary, but it's sort of fun.
(define (read-eval-print)
  (begin
    (prompt)
    (let* (
          (expression (read-line))
          )
      (if (equal? expression "quit")
          expression                     ;; exit
          (begin
           (display (unicalc-parse expression))
           (read-eval-print)
           ))
      )
    ))


;; Prompt the user to enter an expression.

(define (prompt)
  (newline)
  (display prompt-string))


;; Function: unicalc-parse
;; input: a string to parse
;; Output: a list of three elements, the same as the parse and parse-input
;;    functions
;; Also prints the results to the screen
(define (unicalc-parse exp)
  (let ((output (parse exp)))
    (display result-label)
    (display (first output))
    (display "\n")
    (display syntax-tree-label)
    (display (second output))
    (display "\n")
    output))


;; Function begins-with?
;; input: an element and a list
;; output: #t if the list L begins with the element elem, #f otherwise
;; This function is also defined in the tokenizer, which is included
;; above, but we include it again in case you write your own tokenizer
;; and don't implement this function.
(define (begins-with? elem L)
  (if (null? L) 
      #f
      (equal? elem (first L))))


;; Function: parse, the top-level parsing function
;; input: 
;;  expression - a string of input from the user
;; ouput: a list of three elements: ( parse-outcome  parse-result  residual )
;;    parse-outcome: #t if the input was successfully parsed, and #f if it was not
;;    parse-result: the parse tree
;;    residual: the rest of the tokens in the input list that are not included 
;;         in the parse tree (or an empty list, if all tokens were accounted for).
(define (parse expression)
  (parse-input (tokenize expression)))


;; Parse the following grammar
;;
;; I -> V = E | % varname | varname
;; E -> S | Val
;; S -> P + S | P - S | P
;; P -> Pow * P | Pow / P | Pow
;; Pow -> Sub^Amt | Sub
;; Sub -> ( S ) | varname | Amt
;; Val -> Amt +- Amt UL | Amt UL
;; Amt -> number | - number
;; UL -> Units / Units | Units
;; Units -> empty | unitname Units
;;

;; Define parser succeed and fail functions
;; Both return 3-lists of the form
;;  (<outcome: #t for success, #f for failure>   <result: in the event of success>  <residue: remaining input>)

(define (succeed result residue)
  (list #t result residue))

(define (fail residue)
  (list #f () residue))

;; function varname?.
;; Input: token
;; Ouput: true if token is a string
;;      false if token is not a string
(define varname? string?) 

;; function parse-input.
;; Input: tokens, a list of tokens
;; output: a list of three elements: ( parse-outcome  parse-result  residual )
;;    parse-outcome: #t if the input was successfully parsed, and #f if it was not
;;    parse-result: the parse tree
;;    residual: the rest of the tokens in the input list that are not included 
;;         in the parse tree (or an empty list, if all tokens were accounted for).
;; Parse a top level expression using recursive descent parsing.  
;; For now this function calls parse-val (which itself is only partly implemented, 
;; but make sure to update it as you implement your grammar.
(define (parse-input tokens)
  (if (begins-with? norm-char tokens) ;; If norm-char is first, this must be normalization
      (if (varname? (second tokens)) ;; Check if we have a variable to normalize
          (succeed (list '% (string->symbol (second tokens))) (rest (rest tokens)))
          (fail tokens)) ;; Here we had a lone norm-char: Invalid syntax
      (if (varname? (first tokens)) ;; Check to see if we have a variable (either definition or a lone variable)
          (if (and (not (null? (rest tokens))) (equal? (second tokens) equal-char)) ;; Check for a definition
              (let* ([exp-triple (parse-exp (rest (rest tokens)))] ;; If we have a definition, hand off control to E
                     [have-exp (outcome exp-triple)])
                (if have-exp
                    (succeed (list '= (string->symbol (first tokens)) (result exp-triple)) (residual exp-triple))
                    (fail (residual exp-triple))))
              (succeed (string->symbol (first tokens)) (rest tokens))) ;; Otherwise, we have a lone variable, which is still proper syntax
          (fail tokens)))) ;; If we had neither a percent sign or a variable name, then we fail

;; function: parse-exp
;; Input: list containg tokens
;; Output: Parse tree
;; implements the E rule:
;; E -> S | Val
(define (parse-exp tokens)
  (let* ([value (parse-val tokens)]
         [val-succeeded (outcome value)])
    (if val-succeeded
        value
        (parse-sum tokens))))

;; function: parse-val
;; input: input, a list of tokens
;; output: the same as parse-input
;; Parse a value according to the rule
;;       Val -> Amt +- Amt UL | Amt UL
(define (parse-val input)
  (let* (
         (triple (parse-amt input))
         (have-amt (outcome triple))
         (amt (result triple))
         (after-amt (residual triple))
         )
    (if have-amt
        (if (begins-with? add-char after-amt)
            (parse-uncertain-val amt after-amt)
            (let* (
                   (triple2 (parse-ul after-amt))
                   (have-ul (outcome triple2))
                   (unit-list (result triple2))
                   (after-ul (residual triple2))
                   )
              (if have-ul
                  (succeed (make-UQL amt 0 (first unit-list) 
                                     (second unit-list)) after-ul)
                  (fail after-amt))))
        (fail input))))
  
;; Function parse-uncertain-val
;; Input:
;;   amt - the amount we have already parsed
;;   tokens - The tokens to parse
;; Output:
;;   A parse tree
;; Description: Takes an already parsed amount and attempts
;;              to parse the +- Amt UL part of the Val rule
(define (parse-uncertain-val amt tokens)
  (if (and (>= (length tokens) 3)            ;; Check for a plus or minus sign and
           (equal? (first tokens) add-char) ;; at least enough tokens for amount
           (equal? (second tokens) subtract-char))
      (let* ([err-triple (parse-amt (rest (rest tokens)))]
             [have-err (outcome err-triple)]
             [after-err (residual err-triple)])
        (if have-err
            (let* ([ul-triple (parse-ul after-err)]
                   [have-ul (outcome ul-triple)]
                   [ul (result ul-triple)]
                   [num (first ul)]
                   [den (second ul)])
              (if have-ul
                  (succeed (make-UQL amt (result err-triple) num den) (residual ul-triple))
                  (fail after-err)))
            (fail (rest (rest tokens)))))
      (fail tokens)))

;; parse-amt
;; input: a list of tokens
;; output: the same as parse-input
;; Parse an ammount according to the rule
;;    Amt -> number | - number
;; NOTE: THIS FUNCTION IS ONLY PARTIALLY IMPLEMENTED.  IT CURRENTLY IMPLEMENTS:
;;    Amt -> number
(define (parse-amt input)
  (cond 
    ((number? (first input)) (succeed (first input) (rest input)))
    (else (fail input))))

;; parse a unit-list.  
;; input: a list of tokens
;; output: the same as parse-input
;; UL -> Units / Units | Units
(define (parse-ul input)
  (let* (
         (triple (parse-units input))
         (have-units (outcome triple))
         (units (result triple))
         (after-units (residual triple))
         )
    (if have-units
        (if (begins-with? divide-char after-units)
            (let* (
                   (triple2 (parse-units (rest after-units)))
                   (have-denom (outcome triple2))
                   (denom-units (result triple2))
                   (after-denom (residual triple2))
                   )
              (if have-denom
                  (succeed (list units denom-units) after-denom)
                  (succeed (list units '()) (rest after-units))))
            (succeed (list units '()) after-units))
        (fail input))))

;; parse-units
;; input: a list of tokens
;; output: the same as parse-input
;; Parse a list of Units according to the rule
;;        Units -> empty | unitname Units
(define (parse-units input)
  (parse-units-help input '()))

;; parse-units
;; input: 
;;   input: a list of tokens
;;   accum: The units accumulated so far
;; output: the same as parse-input
;; Parse a list of Units according to the rule
;;        Units -> empty | unitname Units
(define (parse-units-help input accum)
  (cond 
    ((null? input) (succeed (map string->symbol (reverse accum)) '()))
    ((string? (first input)) (parse-units-help (rest input) 
                                               (cons (first input) accum)))
    (else (succeed (map string->symbol (reverse accum)) input))))


;; Helper functions you might find useful

;; char->num
;; input: a character representing a number
;; output: the number represented by the input character
(define (char->number c)
  (string->number (list->string (list c))))

;; string->char
;; input: a string of length 1
;; output: a character representation of the single character in the input string.
(define (string->char s)
  (first (string->list s)))

;; char->symbol
;; input: a character
;; output: a symbol representation of the same character
(define (char->symbol char)
  (string->symbol (list->string (list char))))

(define (outcome  triple) (first  triple))
(define (result   triple) (second triple))
(define (residual triple) (third  triple))

(test (parse-input '(#\% "var")) (succeed '(% var) '()))
(test (parse-input '("var")) (succeed 'var '()))

(test (parse-exp '(14 "meters")) (parse-val '(14 "meters")))

;; Tests for the parser.  You should add at least 2 more.
(test (parse "x = 42+-0 meter") '(#t (= x ((42 0) (meter) ())) ()))
(test (parse "% x") '(#t (% x) ()))
(test (parse "z = y + g - x") '(#t (= z (+ y (- g x))) ()))
(test (parse "x = 3.42 +- .75 foot foot / second") '(#t (= x ((3.42 0.75) (foot foot) (second))) ()))
(test (parse "var = 3") '(#t (= var ((3 0) () ())) ()))
(test (parse "var = -2.5 meter / second") '(#t (= var ((-2.5 0) (meter) (second))) ()))
(test (parse "x = (y+x)^-2") '(#t (= x (^ (+ y x) -2)) () ))
(test (parse "x = y+x^2") '(#t (= x (+ y (^ x 2))) ()))
(test (parse "x = y+(x^2-(z/y))") '(#t (= x (+ y (- (^ x 2) (/ z y)))) ()))
(test (parse "x = y+(x^2-z/y)") '(#t (= x  (+ y (- (^ x 2) (/ z y)))) ()))
(test (parse "x = y+z+2*a^-3") '(#t (= x (+ y (+ z (* 2 (^ a -3))))) ()))
(test (parse "a = 3.5 +- .2 / cm") '(#t (= a ((3.5 0.2) () (cm))) ()) )
(test (parse "a") '(#t a ()))

(tester 'show)

