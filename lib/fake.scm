
(define / (lambda (x y) 0))

(define (exact? n) #t)

(define exact->inexact (lambda (x) x))

(define integer? number?)

(define call-with-current-continuation (lambda (r) (r #f)))

(define abs (lambda (x) (if (< x 0) (- x) x)))