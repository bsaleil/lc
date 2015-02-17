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

;; TODO : corps define begin ?

; (define (foo n m)
;   ($$putchar n)
;   ($$putchar m))

; (foo 55 56)

(let ((a 48))
  (define b 55)
  (pp a)
  (pp b))
