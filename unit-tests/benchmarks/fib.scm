;;; FIB -- A classic benchmark

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(pp (fib 0))
(pp (fib 1))
(pp (fib 10))
(pp (fib 20))
(pp (fib 30))
(pp (fib 35))

;0
;1
;55
;6765
;832040
;9227465
