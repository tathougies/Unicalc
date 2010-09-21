; CS 42 Homework 3, Problem 1: Unicalc with Error
; Author(s): Travis Athougies and Peter Andrien
; Time spent: 2 hours

; Additional comments (optional):
; See our git repository at http://github.com/iammisc/Unicalc

; Again you need the tester
(load "tester.rkt")

;; Data abstraction for a quantity list
; Function: make-UQL
; Input:
;    quant - a number
;    error - amount of uncertainty 
;    num - a list of symbols representing the units in the numerator
;    den - a list of symbols representing the units in the denominator
; Output:
;    a representation of an uncertain quantity list
(define (make-UQL quant error num den)
  (list quant error num den))

; same as (make-UQL quant 0.0 num den)
(define (make-QL quant num den)
  (make-UQL quant 0.0 num den))

; Accessor functions for our QL abstraction
(define get-quant first)

(define get-error second)

(define get-num third)

(define get-den fourth)

; Since the new unicalc db uses make-QL, it should be loaded after that is defined
(load "unicalc-db.rkt")

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

; Function name: concat
; Input:
;   L - a list of lists
; Output:
;   A list containing all the elements of the lists
;   in L
(define (concat L)
  (foldl append '() L))

;; Add your unicalc functions below.
; Function name: product
; Input:
;   L - a list of numbers
; Output:
;   The product of all the numbers in L
(define (product L) (foldl multiply (make-QL 1.0 null null) L))

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
;   a - a quantity list
;   b - a quantity list
; Output:
;   the product of a and b. The result will be normalized only
;   if a and b are normalized
(define (multiply a b)
  (let* ([a-qt (get-quant a)]
         [a-err (get-error a)]
         [a-num (get-num a)]
         [a-den (get-den a)]
         [b-qt (get-quant b)]
         [b-err (get-error b)]
         [b-num (get-num b)]
         [b-den (get-den b)]
         [num (append a-num b-num)]
         [den (append a-den b-den)]
         [Ex (/ a-err a-qt)]
         [Ey (/ b-err b-qt)]
         [new-error (sqrt (+ (* Ex Ex) (* Ey Ey)))]
         [z (* a-qt b-qt)]
         [cancelled-numden (cancel num den)]
         [cancelled-num (first cancelled-numden)]
         [cancelled-den (second cancelled-numden)])
    (make-UQL z (* new-error z) cancelled-num cancelled-den)))

; extra test
(test (multiply (make-UQL 1.0 3.0 '(meters) '(seconds)) (make-UQL 1.0 4.0 '(seconds) '(meters)))
      (make-UQL 1.0 5.0 '() '()))

; Test alphabetical sorting once again
(test (multiply (make-QL 6.0 '(kg second) '(meter meter))
		(make-QL 7.0 '(ampere kg second second) '(meter)))
      (make-QL 42.0 '(ampere kg kg second second second) '(meter meter meter)))
(test (multiply (make-QL 1.0 '() '()) (make-QL 2.0 '(meter) '(second))) (make-QL 2.0 '(meter) '(second)))

; Function name: divide
; Input:
;  a - a quantity list
;  b - a quantity list
; Output:
;  The quotient of a and b. This is identical to multiplying
;  a by the reciprocal of b.
(define (divide a b) ;; Multiply a by the reciprocal of b
  (let ([b-num (get-num b)]
        [b-den (get-den b)]
        [b-error (get-error b)]
        [b-quant (get-quant b)])
    ; For an explanation of why b-error is adjusted such see below
    (multiply a (make-UQL (/ 1 b-quant) (/ b-error (* b-quant b-quant)) b-den b-num))))

; The error in division is
; z-err = sqrt((a-err/a-quant) ^ 2 + (b-err/b-quant) ^ 2) * z
; Since we implement division through multiplication by 
; the reciprocal, when we calculate error in the multiply function
; b-quant ends up being the reciprocal of b, when in fact it should
; be just b. Therefore we need a correction. We can calculate this
; correction as follows, by solving for k.
;
; b-err/b-quant = ((k * b-err)/(1 / b-quant))
;
; By cross multiplying, we end up with
;
; b-err/b-quant = k(b-err)(b-quant)
; 
; Moving terms around, we end up with
;
; k = 1/(b-quant ^ 2)

(test (divide (make-QL 1.0 '() '()) (make-QL 2.0 '(meter) '(second)))
      (make-QL 0.5 '(second) '(meter)))
(test (divide (make-UQL 1.0 4.0 '(meter) '(second)) (make-UQL 1.0 3.0 '(meter) '(second)))
      (make-UQL 1.0 5.0 '() '()))

; Function name: basic-add
; Input:
;   a - a normalized quantity list
;   b - a normalized quantity list
; Output:
;   The sum of a and b, so long as the units of a and b are
;   interconvertible. If no suitable conversion can be found,
;   basic-add evaluates to #f
(define (basic-add a b)
   (let* ([a-qt (get-quant a)]  ; If both a and b are normalized, then both
          [a-err (get-error a)] ; a and b's denominator and numerator must
          [a-num (get-num a)]   ; be exactly equal
          [a-den (get-den a)]
          [b-qt (get-quant b)]
          [b-err (get-error b)]
          [b-num (get-num b)]
          [b-den (get-den b)]
          [new-error (sqrt (+ (* a-err a-err) (* b-err b-err)))])
     (if (and (list=? a-num b-num symbol=?) (list=? a-den b-den symbol=?))
         (make-UQL (+ a-qt b-qt) new-error a-num a-den)
         #f)))

(test (basic-add (make-QL 1.0 '(meter) '()) (make-QL 1.0 '(Newton) '())) #f) ; basic-add takes only *normalized* QL's
(test (basic-add (make-QL 1.0 '(foot) '(second second)) (make-QL (- 4.0) '(foot) '(second second)))
      (make-QL (- 3.0) '(foot) '(second second)))
; Can't add incompatible units
(test (basic-add (make-QL 1.0 '(foot) '(second)) (make-QL 2.0 '(foot) '(second second))) #f)

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
    (if added added (list 'error "Illegal add" a b))))

(test (add (make-QL 1.0 '(foot) '(second second)) (make-QL 4.0 '(foot) '(second second)))
      (make-QL 5.0 '(foot) '(second second)))

; Function name: subtract
; Input:
;   a - a normalized quantity list
;   b - a normalized quantity list
; Output:
;   The difference a - b. This is identical to the sum of
;   a and the inverse of b. If the units of a and b are not
;   interconvertible, this function will complain
(define (subtract a b)
  (let* ([neg-b (make-UQL (- (get-quant b)) (get-error b) (get-num b) (get-den b))]
	 [added (basic-add a neg-b)])
    (if added added (list 'error "Illegal subtract" a b))))

(test (subtract (make-QL 1.0 '(foot) '(second second)) (make-QL 4.0 '(foot) '(second second)))
      (make-QL (- 3.0) '(foot) '(second second)))

; Function name: repeat
; Input:
;  n - a positive integer
;  x - a value
; Output:
;  A list of length n filled with x
(define (repeat n x)
  (if (= n 0)
      null
      (cons x (repeat (- n 1) x))))

; Function name: calc-power
; Input:
;   a - a quantity list
;   p - an integer greater than 0
; Output:
;   Returns a to the p'th power, with the proper units
; Implementation notes:
;   The power operation is quite simple, since no unit
;   checking need be done. Essentially, we repeat the
;   numerator and denominator p times a to the p'th
;   power
(define (calc-power a p)
  (let* ([a-qt (get-quant a)]
	 [a-num (get-num a)]
	 [a-den (get-den a)]
	 [new-num (concat (repeat p a-num))]
	 [new-den (concat (repeat p a-den))]
	 [cancelled (cancel new-num new-den)]) ; This puts new-num and new-den in alphabetic order
    (make-QL (foldl * 1 (repeat p a-qt))
	     (first cancelled) ; Get the numerator out
	     (second cancelled)))) ; Get the denominator

;; Function name: power-no-error
;; Input:
;;   a - a normalized quantity list
;;   p - an exponent
;; Output:
;;   Returns a ^ p without proper error calculation
(define (power-no-error a p)
  (cond
   [(< p 0) (divide (make-QL 1.0 '() '()) (calc-power a (- p)))]
   [(= p 0) (make-QL 1.0 '() '())]
   [else (calc-power a p)]))

(test (power-no-error (make-QL 4.0 '(meter) '(second)) 3) (make-QL 64.0 '(meter meter meter) '(second second second)))

;; Function name: power
;; Input:
;;   a - a normalized quantity list
;;   p - an exponent
;; Output:
;;   Returns a ^ p 
(define (power a p)
  (let* ([calcd (power-no-error a p)]
         [quant (get-quant calcd)]
         [num (get-num calcd)]
         [den (get-den calcd)]
         [a-err (get-error a)]
         [a-quant (get-quant a)]
         ; We use absolute value here, since taking the square root of a square 
         ; is equivalent to the absolute value. Also exact->inexact is needed in
         ; the case that p = 0 (since p is an integer). In this case, the racket
         ; documentation defines anything multiplied by the integer 0, including
         ; floating point numbers, to be equivalent to the integer 0, not 0.0
         [error (exact->inexact (* quant (abs (* (/ a-err a-quant) p))))])
    (make-UQL quant error num den)))

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
      (make-QL 1.0 (list unit) null)))) ;; Basic unit

(test (normalize-unit 'kg) (make-QL 1.0 '(kg) '()))
(test (normalize-unit 'kilogram) (make-QL 1.0 '(kg) '()))

; Function name: normalize
; Input:
;   quantity - a quantity list
; Output:
;   A quantity list that has been normalized into the most basic units
(define (normalize quantity)
  (let ([q (get-quant quantity)]
        [err (get-error quantity)]
        [n (get-num quantity)]
        [d (get-den quantity)])
    (multiply (make-UQL q err '()'())
              (divide (product (map normalize-unit n))
                      (product (map normalize-unit d))))))

(test (normalize (make-QL 2.0 '(newton meter)'(second))) (make-QL 2.0 '(kg meter meter) '(second second second)))

;; Load and run the tests
(load "unicalc-tests.rkt")
;; These Test Include Error Propagation.
(load "unicalc-with-error-tests.rkt")

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
