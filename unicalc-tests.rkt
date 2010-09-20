;; Unicalc test cases

;; Function close-enough is meant to test whether two inexact quantites
;; have a relative difference that is within the specified tolerance.

;; This is used in cases where answers might not match exactly.


(define tolerance 1e-6)

(define (relative-error x y)
  (cond ((= x y) 0)
        ((not (= x 0)) (/ (abs (- x y)) x))
        (else (/ (abs (- x y)) y))))

(define (close-enough Quantity1 Quantity2)
  (and (equal? (list (get-num Quantity1) (get-den Quantity1))
               (list (get-num Quantity2) (get-den Quantity2)))
       (< (relative-error (get-quant Quantity1) (get-quant Quantity2) ) tolerance)))


(test (normalize-unit 
       'second) 
       (make-QL 1.0 '(second) '()))

(test (normalize-unit 
       'gram) 
       (make-QL 0.001 '(kg) '()))

(test (normalize-unit 
       'kilometer) 
       (make-QL 1000.0 '(meter) '()))

(test (normalize-unit
       'day) 
       (make-QL 86400.0 '(second) '()))

(test (normalize-unit
       'newton) 
       (make-QL 1.0 '(kg meter) '(second second)))


(test (close-enough 
       (normalize 
        (make-QL 1.0 '(mile) '(hour))) 
        (make-QL 0.447031923888 '(meter) '(second))) #t)

(test (close-enough 
       (normalize 
        (make-QL 1e6 '(pound) '(mile mile))) 
        (make-QL 0.1751393211439157 '(kg) '(meter meter))) #t)

(test (normalize 
       (make-QL 1.0 '(newton meter) '(second))) 
       (make-QL 1.0 '(kg meter meter) '(second second second)))

(test (normalize 
       (make-QL 1.0 '(volt) '())) 
       (make-QL 1.0 '(kg meter meter) '(ampere second second second)))


(test (close-enough 
       (multiply (normalize (make-QL 1 '(foot) '())) (normalize (make-QL 1.0 '(acre) '()))) 
       (make-QL 1233.414987438996 '(meter meter meter) '())) #t)

(test (multiply (make-QL 1.0 '() '()) (make-QL 1.0 '(kg meter) '(second))) 
      (make-QL 1.0 '(kg meter) '(second)))

(test (multiply (make-QL 1.0 '(chicken) '(meter meter)) 
                (make-QL 100.0 '(meter meter) '())) (make-QL 100.0 '(chicken) '()))

(test (close-enough (multiply (normalize (make-QL 100 '(tadpole) '(gallon))) 
                              (make-QL 1.0 '(meter meter meter) '())) 
                    (make-QL 26418.63702726775 '(tadpole) '())) #t)


(test (divide (normalize (make-QL 1.0 '(yard) '())) 
              (normalize (make-QL 1.0 '(foot) '()))) 
      (make-QL 3.0 '() '()))

(test (divide (normalize (make-QL 1.0 '(mile) '())) 
              (normalize (make-QL 1.0 '(foot) '()))) 
      (make-QL 5280.0 '() '()))

(test (close-enough (divide (normalize (make-QL 1.0 '(torr) '())) (normalize (make-QL 1.0 '(atmosphere) '()))) 
                    (make-QL 0.0013157894736842105 '() '())) #t)


(test (add (make-QL 1.0 '(meter) '()) 
           (make-QL 1.0 '(meter) '())) 
      (make-QL 2.0 '(meter) '()))

(test (close-enough (add (normalize (make-QL 1.0 '(foot) '())) (normalize (make-QL 1.0 '(meter) '()))) 
                    (make-QL 1.30479449356 '(meter) '())) #t)

(test (close-enough (add (normalize (make-QL 1 '(meter) '())) (normalize (make-QL 1.0 '(foot) '()))) 
                    (make-QL 1.30479449356 '(meter) '())) #t)


(test (subtract (make-QL 1.0 '(meter) '()) (make-QL 1.0 '(meter) '())) 
      (make-QL 0.0 '(meter) '()))

(test (close-enough (subtract (normalize (make-QL 1.0 '(foot) '())) (normalize (make-QL 1.0 '(meter) '()))) 
                    (make-QL -0.69520550644 '(meter) '())) #t)

(test (close-enough (subtract (normalize (make-QL 1.0 '(meter) '())) (normalize (make-QL 1.0 '(foot) '()))) 
                    (make-QL 0.69520550644 '(meter) '())) #t)

(test (close-enough (power (normalize (make-QL 3.0 '(foot) '(second))) 2)
                    (make-QL 0.8360971497404719 '(meter meter) '(second second))) #t)

(test (power (normalize (make-QL 42.0 '(meter) '(second))) 0)
      (make-QL 1.0 '() '()))

(test (power (normalize (make-QL 2.0 '() '(second))) -1) 
      (make-QL 0.5 '(second) '()))

