(load "symbol-definitions.rkt")



;; Function begins-with?
;; input: an element and a list
;; output: #t if the list L begins with the element elem, #f otherwise
(define (begins-with? elem L)
  (if (null? L) 
      #f
      (equal? elem (first L))))

(define (skip-whitespace input)
  (if (null? input)
      '()
      (if (whitespace? (first input))
          (skip-whitespace (rest input))
          input)))


(define (whitespace? char)
  (equal? space char))

(define (char->number c)
  (string->number (list->string (list c))))


;; Tokenize an input string
;; input: an input string
;; output: a list of tokens
;; Tokens include:
;; numbers (positive only, decimal or whole)
;; variables
;; strings (e.g. unit names)
;; arithmetic operators
;; 
(define (tokenize input)
  (tokenize-tr (string->list input) '()))

;; Tokenize the input, but accumulate it in
;; reverse order for efficiency's sake
(define (tokenize-tr input accum)
  (if (null? (skip-whitespace input))
      (reverse accum)
      (let ((token-and-rest (split-next-token input)))
        (tokenize-tr (second token-and-rest) 
                     (cons (first token-and-rest) accum)))))

;; funtion: split-next-token
;; input: an input string of characters
;; output: a list containing the first token in the input list
;;   and the rest of the input list (without the characters in the
;;   first token).
(define (split-next-token input)
  (split-token-no-white (skip-whitespace input) '()))

(define (begins-with-valid-single-char? input)
  (or 
   (begins-with? multiply-char input)
   (begins-with? divide-char input)
   (begins-with? add-char input)
   (begins-with? subtract-char input)
   (begins-with? equal-char input)
   (begins-with? exponent-char input) 
   (begins-with? norm-char input)
   (begins-with? left-paren input)
   (begins-with? right-paren input)))

;; function: split-token-no-white
;; input: 
;;    input - an input string of characters with no leading whitespace
;;    token - the token, partially constructed
;; output: a list containing the first token in the input list
;;   and the rest of the input list (without the characters in the
;;   first token).
(define (split-token-no-white input token)
  (cond
    ((null? token)  ;; we haven't seen any of the token yet
     (cond   ;; arithmetic operators are singleton tokens
       ((null? input) (list '() '()))
       ((begins-with-valid-single-char? input)
        (list (first input) (rest input)))
       ;; otherwise, keep accumulating the token until we see space
       (else (split-token-no-white 
              (rest input) 
              (append token (list (first input)))))))
    ;; if the token is a number, keep building the number
    ((begins-with-num token)  
     (cond
       ((begins-with-num input) 
        (split-token-no-white (rest input) 
                              (append token (list (first input)))))
       (else (list (list->number token) input))))
    ;; If the token is a string, keep building the string
    ((begins-with-letter token)
     (cond
       ((begins-with-letter input) 
        (split-token-no-white (rest input) 
                              (append token (list (first input)))))
       (else (list (list->string token) input))))
    (else (list 'error "unidentified token sequence"))))

;; Function: begins-with-num
;; input: a string
;; output: #t if the string begins with a number character 
;;   (including a decimal point), #f otherwise
(define (begins-with-num input)
  (if (null? input)
      #f
      (or (char->number (first input)) 
          (equal? point-char (first input)))))

;; Function: begins-with-num
;; input: a string
;; output: #t if the string begins with a letter character 
;;   #f otherwise
(define (begins-with-letter input)
  (if (null? input)
      #f
      (and (char-ci<=? '#\a (first input)) (char-ci>=? '#\z (first input)))))

;; funtion: list->number
;; input: a list of numeric characters
;; output: the number represented by the list of characters
(define (list->number input)
  (string->number (list->string input)))
