;; Scheme function tester
;; Robert Keller

;; This defines tester code, to be loaded or inserted at the start
;; To load, include in your program
;;     (load "tester.scm")
;; To run a test, call (test <expression> <desired value>)
;; To show totals of wrong vs. right, call (tester 'show)
;;
;; In using this package, you should treat 'tester' and 'test' as reserved words.

(require mzlib/defmacro)

(define (make-tester)
  (let (
        (right 0)
        (wrong 0)
        (counter 0)
        )
    (define (tester . args)
      (if (equal? 'show (first args)) ;; special case of 1 argument: 'show
          (begin
            (display "\nTest-results: ")
            (display right)
            (display " right, ")
            (display wrong)
            (display " wrong, out of ")
            (display counter)
            (display " total.\n")
            )
          (if (not (equal? (length args) 3))
              (begin
                (display "Wrong number of arguments to test: ")
                (display args))
              (let (
                    (expr (first args))
                    (actual (second args))
                    (desired (third args))
                    )
                (begin
                  (set! counter (+ 1 counter))
                  (display "Test ")
                  (display counter)
                  (display ": ")
                  (display expr)
                  (if (equal? actual desired)
                      (begin 
                        (display " succeeds with result: ")
                        (display actual)
                        (display "\n")
                        (set! right (+ 1 right)))
                      (begin (display " fails with result: ")
                             (display actual)
                             (display "\n         but desired is: ")
                             (display desired)
                             (display "\n")
                             (set! wrong (+ 1 wrong)))))))))
    tester))
(define tester (make-tester))

(define-macro (test expr desired)
  `(tester ',expr ,expr ,desired))
