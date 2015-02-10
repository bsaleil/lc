(define a (make-vector 5))

(vector-set! a 0 11)
(vector-set! a 1 22)

(println (vector-ref a 0))
(println (vector-ref a 1))

;11
;22
