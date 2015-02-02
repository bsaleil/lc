;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err
;; Test bind define


(define (string . chars)
   (if (null? chars)
      ""
      (let ((str (make-string (length chars))))
         (string-h str chars 0))))

(define string
  (lambda chars
    (if (null? chars)
        ""
        ((lambda (str) (string-h str chars 0)) (make-string (length chars))))))


; (foo 10)
; (bar 10)
