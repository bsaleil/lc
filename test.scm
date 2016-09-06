
;; Globals:
;; Add asc and type
;; (asc-global-add ...)
;; (asc-global-get ...)
;; (global-pos ...)
;; (global-stype ...)


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
