
(define (fibfp n)
  (if (< n 2.)
    n
    (fibfp 0.)))


(gambit$$pp (fibfp 1.))
