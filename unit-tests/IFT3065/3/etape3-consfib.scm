(define (fib n)
  (if (< n 2)
      (cons n '())
      (cons (+ (car (fib (- n 1))) (car (fib (- n 2)))) '())))

(println (car (fib 30)))
(println (car (fib 30)))
(println (car (fib 30)))
(println (car (fib 30)))
(println (car (fib 30)))

;832040
;832040
;832040
;832040
;832040
