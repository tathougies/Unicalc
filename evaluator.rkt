;; File:    evaluator.rkt
;; Author:  You!
;; A starting point for the unilang evaluator

;; Load the unicalc api, the symbol definitions and the parser.
(load "ucErrorProp.rkt")
(load "symbol-definitions.rkt")
(load "parser.rkt")

;; The read-eval-print loop
(define (read-eval-print)
  (begin
    (prompt)
    (let* ( (expression (read-line)) )
      (if (equal? expression quit-expr)
          expression                     ;; exit
          (begin
            ;; Parse and eval the expression
            (let* ((parsed (parse expression)))
              (unicalc-display-pt parsed)
              (if (outcome parsed)  ;; if parse is OK, then eval and print
                  (unicalc-display 
                   (unicalc-eval (second parsed)))))
            ;; Then repeat the loop
            (read-eval-print))))))

;; function: unicalc-display-pt
;; input: a list in the form of the output of the parser
;; output: void
;; Displays a parse tree.
(define (unicalc-display-pt parsed)
  (if (first parsed)
      (begin 
        (display syntax-tree-label)
        (display (second parsed))
        (newline))
      (begin
        (display "Parse error: ")
        (display parsed)
        (newline))))

;; function: unicalc-display
;; input: a UQL or void
;; output: unspecified
;; Displays a UQL in a nice way to the user.  Also displays the current 
;;   environment
(define (unicalc-display exp)
  (display "environment: ")  ;; display the current environment
  (display global-env)
  (newline)
  (cond
    ((void? exp) exp)
    ((quant-list? exp) (display-quant-list exp))
    (else (display exp))))

;; function: display-quant-list
;; input: a UQL
;; output: unspecified
;; Displays a UQL in a nice way to the user.  
(define (display-quant-list exp)
  (begin (display (real->decimal-string (get-quant exp)))
         (if (> (get-error exp) 0)
             (begin (display "+-")
                    (display (real->decimal-string (get-error exp)))))
         (display " ")
         (print-list (get-num exp))
         (if (not (null? (get-den exp)))
             (begin
               (display " / ")
               (print-list (get-den exp))))))

;; function: print-list
;; input: a list, L
;; output: null
;; prints a list of elements
(define (print-list L)
  (if (null? L)
      L
      (begin
        (display (first L))
        (display " ")
        (print-list (rest L)))))

;; Prompt the user to enter an expression.
(define (prompt)
  (newline)
  (display prompt-string))



;; This is the global database of mappings from variable names to values
(define global-env '())

;; store a new named value in the global environment
;; input: a name and a value
;; output: void
;; modifies the global environment to contain the new binding
(define (save-definition! name val)
  (set! global-env (cons (list name val) global-env)))

;; clears the global environment
;; no input or output
(define (clear-environment!)
  (set! global-env '()))

;; function: lookup
;; input: a variable name
;; Output: the value of a variable in the given environment.
;; If the variable is unbound, an error value is returned.
(define (lookup var)
  (let (
        (found (assoc var global-env))
        )
    (if found
        (second found)
        (list 'error "unbound variable" var))))

;; Evaluate the current expression in the global environment (extra credit
;; for handling sub-environments, i.e., lambdas + local/static scope)
;; This is the main evaluation function.  You will extend this function.
(define (unicalc-eval parse-tree)
  (cond 
    ((quant-list? parse-tree) parse-tree)  ;; No need to evaluate a ql
    (else (list 'error "unrecognized expression" exp))))


;; function: quant-list?
;; input: an expression
;; output: #t if the expression is a UQL, #f otherwise
;; NOTE: You may have to change this function depending on your implementation
;; of quantity lists.
(define (quant-list? exp)
  (cond
    ((not (list? exp)) #f)
    ((not (= 3 (length exp))) #f)
    (else (list? (first exp)))))
               

;; eval tests
(define tolerance 1e-6)
(define error-tolerance 1e-6)

(define (relative-error x y)
  (cond ((= x y) 0)
        ((not (= x 0)) (/ (abs (- x y)) x))
        (else (/ (abs (- x y)) y))))

(define (close-enough Quantity1 Quantity2)
  (and (equal? (list (get-num Quantity1) (get-den Quantity1))
               (list (get-num Quantity2) (get-den Quantity2)))
       (and (< (relative-error (get-quant Quantity1) (get-quant Quantity2) ) tolerance)
            (< (relative-error (get-error Quantity1) (get-error Quantity2) ) error-tolerance))))


(test (void? (unicalc-eval (list '= 'a (make-UQL 14.4 0.3 '(meter) '())))) #t)
(test (void? (unicalc-eval (list '= 'b (make-UQL 9.3 0.2 '(meter) '())))) #t)
(test (close-enough (unicalc-eval '(- a b)) (make-UQL 5.1 0.360555127 '(meter) '())) #t)
(test (close-enough (unicalc-eval '(+ a b)) (make-UQL 23.7 0.360555127 '(meter) '())) #t)

(test (void? (unicalc-eval (list '= 'c (make-UQL 14.4 0.3 '(foot) '())))) #t)
(test (close-enough (unicalc-eval '(% c)) (make-UQL 4.3890407 0.09143834 '(meter) '())) #t)

(test (void? (unicalc-eval (list '= 'd (make-UQL 2 0.1 '(foot) '(minute))))) #t)
(test (close-enough
       (unicalc-eval '(% d)) (make-UQL 0.010159816 0.0005079908 '(meter) '(second))) #t)

(test (void? (unicalc-eval (list '= 'e (make-UQL 5.1 0.4 '(meter) '())))) #t)
(test (void? (unicalc-eval (list '= 'f (make-UQL 0.4 0.1 '(second) '())))) #t)
(test (close-enough
       (unicalc-eval '(/ e f))
       (make-UQL 12.75 3.340682 '(meter) '(second))) #t)

(test (void? (unicalc-eval (list '= 'w (make-UQL 4.52 0.02 '(cm) '())))) #t)
(test (void? (unicalc-eval (list '= 'x (make-UQL 2.0 0.2 '(cm) '())))) #t)
(test (void? (unicalc-eval (list '= 'y (make-UQL 3.0 0.6 '(cm) '())))) #t)
(test (close-enough 
       (unicalc-eval '(- (+ x y) w))
       (normalize (make-UQL 0.48  0.6327716 '(cm) '()))) #t)

(test (close-enough
       (unicalc-eval '(+ (* w x) (^ y 2)))
       (normalize (make-UQL 18.04 3.7119827 '(cm cm) '()))) #t)

(clear-environment!)
(tester 'show)

  

