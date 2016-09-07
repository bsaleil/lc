
(define foo
   (lambda ()
     (letrec ((tmp1 (lambda (n)
                   (if (($$atom >) ($$atom n) ($$atom 1))
                       (letrec ((tmp2
                                 (lambda (j) (($$atom tmp1) ($$atom 0)))))
                         (($$atom tmp2) ($$atom 1)))
                       ($$atom #f)))))
       (($$atom tmp1) ($$atom 2)))))
(($$atom foo))





;(i)
;Point fixe:
;Pour chaque binding !cst
;S'il a pas de free mais des lates,
;on l'ajoute à la liste des possibles
;
;(ii)
;new-possibles = '()
;pour chaque id de possible
;si toutes ses lates sont dans possibles, on l'ajoute au new-possible
;sinon on l'ignore
;si len(new-possibles) != len(new-possibles), (ii)




;; mlc-define:
;; If it's a lambda, it's a cst lambda:
;; -> do not generate code
;; -> init entry and set stype of global
;; mlc-call:
;; -> check in global asc for stype and identity
;; mlc-identifier:
;; -> if closure does not exist, create it, store it in global slot, and put it


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
