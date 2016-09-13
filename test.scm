
(define (number->string num)
  (define (digit->string d)
    (make-string 1 (integer->char (+ d 48))))
  (define (number->string-h num)
    (if (= num 0)
        ""
        (string-append (number->string-h (quotient num 10))
                       (digit->string    (modulo   num 10)))))
  (number->string-h num))

(gambit$$pp (number->string 1))

;-----







;; Liveness: terminer le travail
;; Letrec: attention aux lates !function
;; Letrec: détecter constantes récursives (a <-> b) (bindings aux bindings cst)
;; Liveness: pb sur '() ?
;; Liveness: cas spécial, set-box! est un kill
;; Liveness: alpha conversion
;; Regalloc: pb movs en trop (fib.s)

;; TODO: optimization: pour un pt entrée:
;;       * si on génère un pt entrée dont la 1ere instruction est un jump,
;;       * on peut patcher le pt d'entrée pour sauter directement au bon endroit
;; TODO: utiliser les informations de type pour:
;;       * eq?
;;       * equal?
;;       * eqv?
;; TODO: inliner les primitives + inliner en fonction des types pour:
;;       * eq?
;;       * equal?
;;       * eqv?
