(define (foo n . r)
  (gambit$$pp n)
  (gambit$$pp r)
  (+ n (car r)))

(foo 1 2 3)
(foo 1 2 3 4 5 6)
