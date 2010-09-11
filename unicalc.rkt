; CS 42 Homework 2, Problem 1: Unicalc
; Author(s): Travis Athougies and Peter Andrien
; Time spent: [enter the time spent on this homework here]

; Additional comments (optional):

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

; Some helper functions
; Function name: list=?
; Input:
;   a - a list
;   b - a list
;   E - an equality function
; Output:
;   Returns true if the corresponding elements in a and b
;   are equal, according to the results of E.
(define (list=? a b E)
  (let ([my-and (lambda (x y) (and x y))])
    (if (= (length a) (length b))
	   (foldl my-and #t (map E a b))
	   #f)))

; Function name: id
; Input:
;   x - anything
; Output:
;   A function that takes 1 argument that always returns x
(define (id x)
  (lambda (y) x))

;; Add your unicalc functions below.
; Function name: product
; Input:
;   L - a list of numbers
; Output:
;   The product of all the numbers in L
(define (product L) (foldl multiply (make-QL 1 null null) L))

; Function name: normalize-unit
; Input:
;   unit - a symbol referencing a defined unit
; Output:
;   A quantity list that expresses the unit in basic units
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

; Function name: normalize
; Input:
;   quantity - a quantity list
; Output:
;   A quantity list that has been normalized into the most basic units
(define (normalize quantity)
  (make-QL 1 '() '()))

; Function name: cancel-unit
; Input:
;    num - a list of units in the numerator
;    den - a list of units in the denominator
;    ans - FOR INTERNAL USE ONLY. Always pass as null
; Output:
;    A list of the form (new-num new-den), where new-num and new-dem are
;    the resulting numerator and denominator, respectively, after duplicate
;    units have been cancelled out
; Notes:
;   In general, you never want to interact with this function, instead,
;   call the cancel function which not only cancels out units but also
;   normalizes the units, by sorting them alphabetically
(define (cancel-unit num den ans)
  (cond
    [(null? num) (list ans den)]
    [(member (first num) den) (cancel-unit (rest num) (remove (first num) den) ans)]
    [else (cancel-unit (rest num) den (cons (first num) ans))]
    ))

(test (cancel-unit '(joule second) '(second second) '()) '((joule)(second)))
(test (cancel-unit '(second joule second) '(second second) '()) '((joule)()))
(test (cancel-unit '(meters meters meters meters meters seconds meters seconds grams) '(meters meters meters seconds N grams grams) '()) '((seconds meters meters meters )(N grams)))

; Function name: cancel
; Input:
;    num - a list of units in the numerator
;    den - a list of units in the denominator
; Output:
;    A list of the form (new-num new-den), where new-num and new-dem are
;    the resulting numerator and denominator, respectively, after duplicate
;    units have been cancelled out. New-num and new-den are sorted to be
;    in alphabetical order
(define (cancel num den)
  (let [(l (cancel-unit num den '()))
        (symbol<? (lambda (x y) (string<? (string-downcase (symbol->string x)) (string-downcase (symbol->string y)))))]
       (list (sort (first l) symbol<?) (sort (second l) symbol<?))))

(test (cancel '(seconds grams meters kilograms N) '()) '((grams kilograms meters N seconds) ()))

; Function name: multiply
; Input:
;   a - a normalized quantity list
;   b - a normalized quantity list
; Output:
;   the product of a and b, as a normalized quantity list
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

; Test alphabetical sorting once again
(test (multiply (make-QL 6 '(kg second) '(meter meter))
		(make-QL 7 '(ampere kg second second) '(meter)))
      (make-QL 42 '(ampere kg kg second second second) '(meter meter meter)))
(test (multiply '(1 ()()) '(2 (meter)(second))) '(2 (meter)(second)))

; Function name: divide
; Input:
;  a - a normalized quantity list
;  b - a normalized quantity list
; Output:
;  The quotient of a and b. This is identical to multiplying
;  a by the reciprocal of b.
(define (divide a b) ;; Multiply a by the reciprocal of b
  (let ([b-num (get-num b)]
        [b-den (get-den b)]
        [b-quant (get-quant b)])
    (multiply a (make-QL (/ 1 b-quant) b-den b-num))))

; Function name: basic-add
; Input:
;   a - a normalized quantity list
;   b - a normalized quantity list
; Output:
;   The sum of a and b, so long as the units of a and b are
;   interconvertible. If no suitable conversion can be found,
;   basic-add evaluates to #f
(define (basic-add a b)
   (let ([a-qt (get-quant a)] ; If both a and b are normalized, then both
         [a-num (get-num a)]  ; a and b's denominator and numerator must
         [a-den (get-den a)]  ; be exactly equal
         [b-qt (get-quant b)]
         [b-num (get-num b)]
         [b-den (get-den b)])
     (if (and (list=? a-num b-num symbol=?) (list=? a-den b-den symbol=?))
	 (make-QL (+ a-qt b-qt) a-num a-den)
	 #f)))

(test (basic-add (make-QL 1 '(meter) '()) (make-QL 1 '(Newton) '())) #f) ; basic-add takes only *normalized* QL's
(test (basic-add (make-QL 1 '(foot) '(second second)) (make-QL (- 4) '(foot) '(second second)))
      (make-QL (- 3) '(foot) '(second second)))
; Can't add incompatible units
(test (basic-add (make-QL 1 '(foot) '(second)) (make-QL 2 '(foot) '(second second))) #f) 

; Function name: add
; Input:
;   a - a normalized quantity list
;   b - a normalized quantity list
; Output:
;   The sum of a and b, so long as the units of a and b are
;   interconvertible. If no suitable conversion can be found,
;   an error is raised
(define (add a b)
  (let ([added (basic-add a b)])
    (if added added (error "Illegal add" a b))))

; Function name: subtract
; Input:
;   a - a normalized quantity list
;   b - a normalized quantity list
; Output:
;   The difference a - b. This is identical to the sum of
;   a and the inverse of b. If the units of a and b are not
;   interconvertible, this function will complain
(define (subtract a b)
  (let* ([neg-b (make-QL (- (get-quant b)) (get-num b) (get-den b))]
	 [added (basic-add a neg-b)])
    (if added added (error "Illegal subtract" a b))))

; Function name: power
; Input:
;   a - a normalized quantity list
;   p - an integer
; Output:
;   Returns a to the p'th power, with the proper units
; Implementation notes:
;   The power operation is quite simple, since no unit
;   checking need be done. Essentially, we repeat the
;   numerator and denominator p times a to the p'th
;   power
(define (power a p) 
  (let* ([a-qt (get-quant a)]
	 [a-num (get-num a)]
	 [a-den (get-den a)]
	 [repeat (lambda (x) (build-list p (id x)))] ; Repeat repeats a value p times
	 [new-num (flatten (repeat a-num))]
	 [new-den (flatten (repeat a-den))]
	 [cancelled (cancel new-num new-den)]) ; This puts new-num and new-den in alphabetic order
    (make-QL (foldl * 1 (repeat a-qt))
	     (first cancelled) ; Get the numerator out
	     (second cancelled)))) ; Get the denominator

(test (power (make-QL 4 '(meter) '(second)) 3) (make-QL 64 '(meter meter meter) '(second second second)))

; Function name: normalize
; Input:
;   quantity - a quantity list
; Output:
;   A quantity list that has been normalized into the most basic units
(define (normalize quantity)
  (let ([q (get-quant quantity)]
        [n (get-num quantity)]
        [d (get-den quantity)])
    (multiply (make-QL q '()'())
              (divide (product (map normalize-unit n))
                      (product (map normalize-unit d))))))

(test (normalize '(2 (newton meter)(second))) '(2 (kg meter meter) (second second second)))

;; Load and run the tests
(load "unicalc-tests.rkt")

; The following tests depend on some functions defined in unicalc-tests.rkt
(test (close-enough (power (make-QL 4.1 '(meter) '(second)) 3)
		    (make-QL 68.920999999999978 
			     '(meter meter meter) 
			     '(second second second)))
      #t)
; Tests alphabetic order of normalized quantities in numerator and denominator
(test (close-enough (power (make-QL 3.7 '(foot meter) '(ampere second second)) 2)
		    (make-QL 13.69
			     '(foot foot meter meter) 
			     '(ampere ampere second second second second)))
      #t)

; The following line was moved from unicalc-tests.rkt, since we needed to run
; some tests after loading the file
(tester 'show)
