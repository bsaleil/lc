

(define (make-lattice elem-list cmp-func)
    (cons elem-list cmp-func))




(define (maps source target)
    (pp source)
    (pp target))

(define (run)
  (let* ((l2
            (make-lattice '(low high)
                (lambda (lhs rhs)
                    100))))
    (maps l2 l2)))

(run)















;; TODO: #<ctx-tclo #3 sym: closure mem-allocated?: #t is-cst: (lambda () ($$atom 1)) cst: #f fn-num: 0>
;;       pourquoi l'ast dans is-cst?
;; TODO: cas spéciaux comme not, eof?, ... si l'opérande est constante, on connait le résultat
;;       pour le moment traité dans codegen, mais movs inutiles, et on perd l'info cst
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
