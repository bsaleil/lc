;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

    (letrec ((loop (lambda (i)
                     (if (>= i 10)
                         0
                         (loop (+ i 1))))))
      (loop 0))

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
