;/
;abs
;apply
;call/cc
;exact->inexact
;peek-char
;rational?
;truncate

;; Set! dans le corps
;; Set! dans les valeurs

(define (foo n)
  (let* ((a 10)
         (b (begin (set! a 1000) 1)))
    (pp a)
    (pp b)))

(foo 10)