;;; TAK -- A vanilla version of the TAKeuchi function.

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(pp (tak 9 6 3))
(pp (tak 18 12 6))
(pp (tak 27 18 9))


;6
;7
;18
