;;; FIB -- A classic benchmark

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

;(apply fib (list 35))
(fib 35)
