
(define (foo8 a b c d e f . g)
	(pp a)
	(pp f)
	(pp g))

(foo8 0 1 2 3 4 5 6 7 8 9)




;; TODO: merde de regalloc
;; TODO: jitter le alloc-rt pour ne pas générer de code si la taille ne nécessite pas un still
;; TODO: Quote cst
;; TODO: attention aux constantes qui sont mem-allocated pour les primitives et appels, etc...
;; TODO: GERER tous les cas ou les opérandes sont toutes cst, donc le résultat l'est aussi
;; TODO: pour (dé)sactiver les cst interprocédural:
;;   - Ajouter le support pour apply-moves et ctx-get-call-args-moves
;;   - Dans ctx-init-fn, modifier la stack pour enlever la cst
;; TODO: ajouter le support des constantes dans les globales non mutables


;(define (bar a b c d)
;  (println a b c d))
;
;(apply bar 22)


;; Liveness: terminer le travail
;; Letrec: attention aux lates !function
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
