
(define (fact n)
   (if (or (= n 0) (= n 1))
       1
       (* n (fact (- n 1)))))

(println (fact 10))

(define (fibo n)
   (if (or (= n 0) (= n 1))
      n
      (+ (fibo (- n 1)) (fibo (- n 2)))))

(println (fibo 35))

;3628800
;9227465
