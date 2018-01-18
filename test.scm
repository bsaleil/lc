
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(gambit$$pp (fact 10))

; (define (fact n)
;   (if (= n 0)
;       1
;       (* n (fact (- n 1)))))
;
; (gambit$$pp (fact 10))



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
