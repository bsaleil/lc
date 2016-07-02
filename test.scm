
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

($apply fib '(35))
($apply fib '(35))
($apply fib '(35))
($apply fib '(35))
($apply fib '(35))
