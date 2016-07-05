
(define (bar) 1)

(define (foo n)
  (bar)
  (bar))
  
(foo 100)

;(define (fib n)
;  (if (< n 2)
;    n
;    (+ (fib (- n 1))
;       (fib (- n 2)))))
;
;(fib 40)
