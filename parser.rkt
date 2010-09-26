;; File:    parser.rkt
;; Authors: Travis Athougies and Peter Andrien
;; Submitted separately to allow room for extensions
(load "unicalc.rkt")
(load "symbol-definitions.rkt")
(load "tokenizer.rkt")
(load "tester.rkt")

;; An interactive read-eval-print loop just for the parser.  It's
;; not necessary, but it's sort of fun.
(define (read-eval-print)
  (begin
    (prompt)
    (let* ([expression (read-line)])
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
  (let ([output (parse exp)])
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
  (list #f '() residue))

;; Function varname?.
;; input: token
;; ouput: true if token is a string
;;        false if token is not a string
(define varname? string?) 

;; Function parse-input.
;; input: tokens, a list of tokens
;; output: a list of three elements: ( parse-outcome  parse-result  residual )
;;         parse-outcome: #t if the input was successfully parsed, and #f if it was not
;;         parse-result: the parse tree
;;         residual: the rest of the tokens in the input list that are not included 
;;           in the parse tree (or an empty list, if all tokens were accounted for).
;; Parse a top level expression using recursive descent parsing.
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

;; Function: parse-exp
;; input: list containg tokens
;; output: parse tree
;; Implements the E rule:
;; E -> S | Val
(define (parse-exp tokens)
  (let* ([sum-triple (parse-sum tokens)]
         [sum-outcome (outcome sum-triple)]
         [after-sum (residual sum-triple)])
    (if (and sum-outcome (null? after-sum))
        sum-triple
        (parse-val tokens))))

;; Function: parse-sum
;; input: list containg tokens
;; ouput: parse tree
;; Implements the S rule:
;; S -> P+S | P-S | P
(define (parse-sum tokens)
  (let* ([prod-triple (parse-product tokens)] ;; Parse the product first
         [prod-outcome (outcome prod-triple)]
         [prod (result prod-triple)]
         [after-prod (residual prod-triple)])
    (if prod-outcome ;; make sure everything worked
        (if (null? after-prod)
            prod-triple ;; This is the third case
            (if (or (equal? (first after-prod) add-char)
                    (equal? (first after-prod) subtract-char)) ;; Check for a summation expression
                (let*
                    ([sum-tokens (rest after-prod)] ;; Recursive descent at work!
                     [sum-triple (parse-sum sum-tokens)]
                     [sum-outcome (outcome sum-triple)]
                     [sum (result sum-triple)]
                     [after-sum (residual sum-triple)])
                  (if sum-outcome ;; Make sure the recursive parse-sum worked
                      (succeed (if (equal? (first after-prod) add-char)
                                (list '+  prod sum)
                                (list '-  prod sum)) after-sum) ;; Construct the proper list based on the character
                      (fail sum-tokens)))
                prod-triple)) ;; Otherwise, return prod-triple
        (fail tokens))))

;; Function: parse-product
;; input: list containg tokens
;; ouput: parse tree
;; Implements the P rule:
;; P -> Pow * P | Pow/P | Pow
(define (parse-product tokens)
  (let* ([pow-triple (parse-pow tokens)] ;; Parse the power first
         [pow-outcome (outcome pow-triple)]
         [pow (result pow-triple)]
         [after-pow (residual pow-triple)])
    (if pow-outcome ;; Power succeeded! Yay!
        (if (null? after-pow)
            pow-triple ;; This is the third case
            (if (or (equal? (first after-pow) multiply-char) ;; Check for product expression
                    (equal? (first after-pow) divide-char))
                (let*
                    ([prod-tokens (rest after-pow)] ;; Recursive descent
                     [prod-triple (parse-product prod-tokens)]
                     [prod-outcome (outcome prod-triple)]
                     [prod (result prod-triple)]
                     [after-prod (residual prod-triple)])
                  (if prod-outcome  ;; check if we have more things to multiply
                      (succeed (if (equal? (first after-pow) multiply-char)
                                (list '*  pow prod)
                                (list '/  pow prod)) after-prod) ;; return the proper parse tree
                      (fail prod-tokens)))
                pow-triple)) ;; Otherwise, let the above function handle it
        (fail tokens))))

;; function: parse-prod
;; Input: list containg tokens
;; Ouput: Parse tree
;; implemnets the Pow rule:
;; Pow -> Sub^Amt | Sub
(define (parse-pow tokens)
  (let* ([sub-triple (parse-subexp tokens)] ;; Check for a subexpression
         [sub-outcome (outcome sub-triple)]
         [sub (result sub-triple)]
         [after-sub (residual sub-triple)])
    (if sub-outcome
        (if (null? after-sub)
            sub-triple ;; Third case
            (if (equal? (first after-sub) exponent-char) ;; See if this is exponentiation
                (let* ([after-exp (rest after-sub)]
                       [amt-triple (parse-amt after-exp)] ;; Check for an amount
                       [amt-outcome (outcome amt-triple)]
                       [amt (result amt-triple)]
                       [after-amt (residual amt-triple)])
                  (if amt-outcome ;; If we have an amount, this is a power expression
                      (succeed (list '^ sub amt) after-amt)
                      (fail after-exp)))
                sub-triple)) ;; Otherwise, let product handle it
        (fail tokens))))

;; function: parse-subexp
;; Input:
;;   tokens - a list of tokens to parse
;; Output:
;;   parse tree
;; Implements Sub rule:
;; Sub -> ( S ) | varname | Amt 
(define (parse-subexp tokens)
  (if (equal? (first tokens) left-paren)
      (let* ([after-lparen (rest tokens)] ;; Subexpression (enclosed in parentheses)
             [sum-triple (parse-sum after-lparen)]
             [sum-outcome (outcome sum-triple)]
             [sum (result sum-triple)]
             [after-sum (residual sum-triple)])
        (if sum-outcome
            (if (equal? (first after-sum) right-paren) ;; Check for right parenthesis
                (succeed sum (rest after-sum))
                (fail after-sum)) ;; No right parentheses!!!!! AAAAARGGGH!
            (fail after-lparen)))
      (if (varname? (first tokens))
          (succeed (string->symbol (first tokens)) (rest tokens)) ;; Variable name
          (let* ([amt-triple (parse-amt tokens)] ;; Must be an amount
                 [amt-outcome (outcome amt-triple)])
            (if amt-outcome
                amt-triple
                (fail tokens))))))

;; function: parse-val
;; input: input, a list of tokens
;; output: the same as parse-input
;; Parse a value according to the rule
;;       Val -> Amt +- Amt UL | Amt UL
(define (parse-val input)
  (let* ([triple (parse-amt input)]
         [have-amt (outcome triple)]
         [amt (result triple)]
         [after-amt (residual triple)])
    (if have-amt
        (if (eq? plusminus-s (first after-amt))
            (parse-uncertain-val amt (rest after-amt))
            (let* ([triple2 (parse-ul after-amt)]
                   [have-ul (outcome triple2)]
                   [unit-list (result triple2)]
                   [after-ul (residual triple2)])
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
  (let* ([err-triple (parse-amt tokens)]
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
	(fail (rest tokens)))))

;; parse-amt
;; input: a list of tokens
;; output: the same as parse-input
;; Parse an ammount according to the rule
;;    Amt -> number | - number
(define (parse-amt input)
  (cond 
    [(and (equal? (first input) negation-char)
          (number? (second input))) (succeed (- (second input)) (rest (rest input)))] 
    [(number? (first input)) (succeed (first input) (rest input))]
    [else (fail input)]))

;; parse a unit-list.
;; input: a list of tokens
;; output: the same as parse-input
;; UL -> Units / Units | Units
(define (parse-ul input)
  (let* ([triple (parse-units input)]
         [have-units (outcome triple)]
         [units (result triple)]
         [after-units (residual triple)])
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
    [(null? input) (succeed (map string->symbol (reverse accum)) '())]
    [(string? (first input)) (parse-units-help (rest input) 
                                               (cons (first input) accum))]
    [else (succeed (map string->symbol (reverse accum)) input)]))

(define (outcome  triple) (first  triple))
(define (result   triple) (second triple))
(define (residual triple) (third  triple))

;; Our test cases
(test (parse-input '(#\% "var")) (succeed '(% var) '()))
(test (parse-input '("var")) (succeed 'var '()))

(test (parse-exp '(14 "meters")) (parse-val '(14 "meters")))
(test (parse-exp '(14 #\+ 2)) (parse-sum '(14 #\+ 2)))
(test (parse-exp '(14 #\- 2)) (parse-sum '(14 #\- 2)))

(test (parse-sum '(14 #\* 2)) (parse-product '(14 #\* 2)))
(test (parse-sum '(14 #\/ 2)) (parse-product '(14 #\/ 2)))
(test (parse-sum '(14)) (parse-amt '(14)))
(test (parse-sum '(14 #\+ 12 #\- 4)) '(#t (+ 14 (- 12 4)) ()))

;; Tests for the parser.  You should add at least 2 more.
(test (parse "x = 42+-0 meter") '(#t (= x (42 0 (meter) ())) ()))
(test (parse "% x") '(#t (% x) ()))
(test (parse "z = y + g - x") '(#t (= z (+ y (- g x))) ()))
(test (parse "x = 3.42 +- .75 foot foot / second") '(#t (= x (3.42 0.75 (foot foot) (second))) ()))
(test (parse "var = 3") '(#t (= var (3 0 () ())) ()))
(test (parse "var = -2.5 meter / second") '(#t (= var (-2.5 0 (meter) (second))) ()))
(test (parse "x = (y+x)^-2") '(#t (= x (^ (+ y x) -2)) () ))
(test (parse "x = y+x^2") '(#t (= x (+ y (^ x 2))) ()))
(test (parse "x = y+(x^2-(z/y))") '(#t (= x (+ y (- (^ x 2) (/ z y)))) ()))
(test (parse "x = y+(x^2-z/y)") '(#t (= x  (+ y (- (^ x 2) (/ z y)))) ()))
(test (parse "x = y+z+2*a^-3") '(#t (= x (+ y (+ z (* 2 (^ a -3))))) ()))
(test (parse "a = 3.5 +- .2 / cm") '(#t (= a (3.5 0.2 () (cm))) ()) )
(test (parse "a") '(#t a ()))

(tester 'show)

