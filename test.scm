(define fib
   (lambda (n)
     (if (($$atom <) ($$atom n) ($$atom 2))
         ($$atom n)
         (($$atom +)
          (let ((n (($$atom -) ($$atom n) ($$atom 1))))
            (if (($$atom <) ($$atom n) ($$atom 2))
                ($$atom n)
                (($$atom +) (($$atom fib) (($$atom -) ($$atom n) ($$atom 1))) (($$atom fib) (($$atom -) ($$atom n) ($$atom 2))))))
          (let ((n (($$atom -) ($$atom n) ($$atom 2))))
            (if (($$atom <) ($$atom n) ($$atom 2))
                ($$atom n)
                (($$atom +) (($$atom fib) (($$atom -) ($$atom n) ($$atom 1))) (($$atom fib) (($$atom -) ($$atom n) ($$atom 2))))))))))
 (($$atom gambit$$pp) (($$atom fib) ($$atom 8)))


;; ->
;; * Merge regalloc
;; * Merge max versions
;; * add bound tests


;; NEXT:
;; * check cc-key
;; * utiliser un systeme pour les globales non mutables compatible avec le nouvel cst vers.
;; * return value (type cr)

;; TODO: quand on récupère l'emplacement d'une variable, regarder les slots pour trouver la meilleure loc (cst > reg > mem)
;; TODO: #<ctx-tclo #3 sym: closure mem-allocated?: #t is-cst: (lambda () ($$atom 1)) cst: #f fn-num: 0>
;;       pourquoi l'ast dans is-cst?
;; TODO: merge de regalloc
;; TODO: merge de version
;; TODO: jitter le alloc-rt pour ne pas générer de code si la taille ne nécessite pas un still
;; TODO: ajouter le support des constantes dans les globales non mutables

;; Liveness: terminer le travail
;; Letrec: attention aux lates !function
;; Liveness: pb sur '() ?
;; Liveness: cas spécial, set-box! est un kill
;; Liveness: alpha conversion
;; Regalloc: pb movs en trop (fib.s)

;; TODO: optimization: pour un pt entrée:
;;       * si on génère un pt entrée dont la 1ere instruction est un jump,
;;       * on peut patcher le pt d'entrée pour sauter directement au bon endroit
