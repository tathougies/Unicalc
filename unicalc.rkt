; CS 42 Homework 2, Problem 1: Unicalc
; Author(s): Travis Athougies and Peter Andrien
; Time spent: [enter the time spent on this homework here]

; Additional comments (optional):
;

; Again you need the tester
(load "tester.rkt")
; Be sure to download the unicalc database file
(load "unicalc-db.rkt")

;; Data abstraction for a quantity list
; Function: make-QL
; Input:
;    quant - a number
;    num - a list of symbols representing the units in the numerator
;    den - a list of symbols representing the units in the denominator
; Output:
;    a representation of a quantity list
(define (make-QL quant num den)
  (list quant num den))

; Accessor functions for our QL abstraction
(define get-quant first)

(define get-num second)

(define get-den third)

;; Add your unicalc functions below.
(define (product L) (foldr multiply (make-QL 1 null null) L))

(define (normalize-unit unit)
  (let ([next-unit (assoc unit unicalc-db)]) ;; Attempt to locate the unit in the unit database
    (if next-unit 
	(let* ([conv-info (second next-unit)]
	       [factor (get-quant conv-info)] ;; Complex unit
	       [numerators (get-num conv-info)] ;; Our basic algorithm here is factor * (normalized numerators/normalized denominators)
	       [denominators (get-den conv-info)]
	       [normalized-numerator (product (map normalize-unit numerators))]
	       [normalized-denominator (product (map normalize-unit denominators))])
	  (multiply (make-QL factor null null) (divide normalized-numerator normalized-denominator)))
	(make-QL 1 (list unit) null)))) ;; Basic unit

(define (cancel-unit num den ans)
  (cond
    [(null? num) (list ans den)]
    [(memq (first num) den)(cancel-unit (rest num) (remove (first num) den) ans)]
    [else (cancel-unit (rest num) den (cons (first num) ans))]
    ))

(test (cancel-unit '(joule second) '(second second) '()) '((joule)(second)))
(test (cancel-unit '(second joule second) '(second second) '()) '((joule)()))
(test (cancel-unit '(meters meters meters meters meters seconds meters seconds grams) '(meters meters meters seconds N grams grams) '()) '((seconds meters meters meters )(N grams)))

(define (cancel num den)
  (let [(l (cancel-unit num den '()))
        (symbol<? (lambda (x y) (string<? (string-downcase (symbol->string x)) (string-downcase (symbol->string y)))))]
       (list (sort (first l) symbol<?) (sort (second l) symbol<?))))


(test (cancel '(seconds grams meters kilograms N) '()) '((grams kilograms meters N seconds) ()))

(define (multiply a b)
  (let* ([a-qt (get-quant a)]
	 [a-num (get-num a)]
	 [a-den (get-den a)]
	 [b-qt (get-quant b)]
	 [b-num (get-num b)]
	 [b-den (get-den b)]
	 [num (append a-num b-num)]
	 [den (append a-den b-den)]
         [cancelled-numden (cancel num den)]
	 [cancelled-num (first cancelled-numden)]
	 [cancelled-den (second cancelled-numden)])
    (make-QL (* a-qt b-qt) cancelled-num cancelled-den)))

(define (divide a b) ;; Multiply a by the reciprocal of b
  (let ([b-num (get-num b)]
	[b-den (get-den b)]
	[b-quant (get-quant b)])
    (multiply a (make-QL (/ 1 b-quant) b-den b-num))))

;; Load and run the tests
(load "unicalc-tests.rkt")



                   


  
