
; (define (foo a b)
;   (gambit$$pp 1))
;
; (foo 1 1)
; (foo #f 5)
; (foo #\A 5)
; (foo #\A 4)
; (foo #\A 3)

;(pp (compute 1000))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(gambit$$pp (fib 40))




; (define (create-x n)
;   (define result (make-vector n))
;   (do ((i 0 (+ i 1)))
;       ((>= i n) result)
;     (vector-set! result i i)))

;5


;(define add (lambda (a b c) (+ a b c)))
;(define sub (lambda (a b c) (- a b c)))
;(define mul (lambda (a b c) (* a b c)))
;(define div (lambda (a b c) (/ a b c)))
;(define oth (lambda (a b c) (add 1. 2. 3.)))
;
;
;(define (caller fn)
;  (fn 1 2 3))
;
;(caller add)
;(caller sub)
;(caller mul)
;(caller div)
;(caller oth)

; A chaque fois qu'on perd l'identité d'une fonction, ici oth,
; alors on dit que la version la plus générique est unk pour tous les arguments de la fonction
