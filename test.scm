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

(define (- n . l)
  (cond ((null? l) ($- 0 n))
        ((= (length l) 1) ($- n (car l)))

(pp (- 1 2))
(pp (- 1))
