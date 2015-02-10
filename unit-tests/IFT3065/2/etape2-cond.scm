(define (f x)
  (cond ((= x 1) 11)
        ((= x 2) 22)
        ((= x 3) 33)
        (else    44)))

(println (f 1))
(println (f 2))
(println (f 3))
(println (f 4))

;11
;22
;33
;44
