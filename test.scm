
(define substring-h
   (lambda (to0 from0 posf0 post0 end0)
     (if (($$atom =) ($$atom posf0) ($$atom end0))
         ($$atom to0)
         (begin
           (($$atom string-set!)
            ($$atom to0)
            ($$atom post0)
            (($$atom string-ref) ($$atom from0) ($$atom posf0)))
           (let ((post1 (($$atom +) ($$atom post0) ($$atom 1)))
                 (posf1 (($$atom +) ($$atom posf0) ($$atom 1))))
             (if (($$atom =) ($$atom posf1) ($$atom end0))
                 ($$atom to0)
                 (begin
                   (($$atom string-set!)
                    ($$atom to0)
                    ($$atom post1)
                    (($$atom string-ref) ($$atom from0) ($$atom posf1)))
                   (($$atom substring-h)
                    ($$atom to0)
                    ($$atom from0)
                    (($$atom +) ($$atom posf1) ($$atom 1))
                    (($$atom +) ($$atom post1) ($$atom 1))
                    ($$atom end0)))))))))
 (define s15 ($$atom "Dark vador"))
 (($$atom gambit$$pp)
  (($$atom substring-h)
   (($$atom make-string) ($$atom 10))
   ($$atom "Dark vador")
   ($$atom 0)
   ($$atom 0)
   ($$atom 10)))

;(define (foo)
;
;  (let ((a 100))
;    (let ((b (+ a 12)))
;      11)))
;
;(foo)
;
;
;
;
;
;

;; WIP:
;; -> Quand on génère un E.P. générique, il faut patcher le pt générique + la fermeture a l'index

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
