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

(define (bar n)
  ($$putchar n))

(define (foo a)
  (bar 56))

(foo 55)

(println 10)

;(print-pos 10)