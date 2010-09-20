;; Unicalc test cases

;; Function close-enough is meant to test whether two inexact quantites
;; have a relative difference that is within the specified tolerance.

;; This is used in cases where answers might not match exactly.



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

(test (close-enough 
       (subtract (normalize (make-UQL 14.4 0.3 '(meter) '())) (normalize (make-UQL 9.3 0.2 '(meter) '()))) 
       (make-UQL 5.1 0.360555127 '(meter) '())) #t)

(test (close-enough 
       (add (normalize (make-UQL 14.4 0.3 '(meter) '())) (normalize (make-UQL 9.3 0.2 '(meter) '()))) 
       (make-UQL 23.7 0.360555127 '(meter) '())) #t)

(test (close-enough 
       (normalize (make-UQL 14.4 0.3 '(foot) '())) 
       (make-UQL 4.3890407 0.09143834 '(meter) '())) #t)

(test (close-enough
       (normalize (make-UQL 2 0.1 '(foot) '(minute)))
       (make-UQL 0.010159816 0.0005079908 '(meter) '(second))) #t)

(test (close-enough
       (divide (normalize (make-UQL 5.1 0.4 '(meter) '())) 
               (normalize (make-UQL 0.4 0.1 '(second) '())))
       (make-UQL 12.75 3.340682 '(meter) '(second))) #t)

(test (close-enough 
       (divide (normalize (make-UQL 3.8 0.3 '(meter) '(second) ))
               (normalize (make-QL 9.81 '(meter) '(second second))))
       (make-UQL 0.38735983 0.030581039 '(second) '())) #t)

(test (close-enough 
       (let ((w (make-UQL 4.52 0.02 '(cm) '()))
             (x (make-UQL 2.0 0.2 '(cm) '()))
             (y (make-UQL 3.0 0.6 '(cm) '())))
         (subtract (add (normalize x) (normalize y)) (normalize w)))
       (normalize (make-UQL 0.48  0.6327716 '(cm) '()))) #t)

(test (close-enough
       (let ((w (make-UQL 4.52 0.02 '(cm) '()))
             (x (make-UQL 2.0 0.2 '(cm) '()))
             (y (make-UQL 3.0 0.6 '(cm) '())))
         (normalize (add (multiply w x) (power y 2))))
       (normalize (make-UQL 18.04 3.7119827 '(cm cm) '()))) #t)




