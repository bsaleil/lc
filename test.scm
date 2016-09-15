




(define (bar n m o p q)
  (+ n m o p q))

(gambit$$pp (bar 10 20 30 40 50))

;(define (bar a b c d)
;  (println a b c d))
;
;(apply bar 22)


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
