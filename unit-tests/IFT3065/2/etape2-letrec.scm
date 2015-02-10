(define (f x)
  (letrec ((fact (lambda (n)
                   (if (= n 0)
                       1
                       (* n (fact (- n 1)))))))
    (fact x)))

(println (f 1))
(println (f 2))
(println (f 3))
(println (f 4))

;1
;2
;6
;24
