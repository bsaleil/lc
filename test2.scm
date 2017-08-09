
;(define gambit$$pp pp)

(define (foo x)
  (let loop ((i 0))
      (if (>= x 1.)
        (begin (loop 1.))
        (begin (gambit$$pp "C2")))))

(foo 10.)
