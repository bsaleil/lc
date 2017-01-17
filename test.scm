
(define make-sumer
  (lambda (n)
    (letrec ((f (lambda (x)
                  (if (> x n)
                      0
                      (+ x (f (+ x 1)))))))
      f)))

(define sum-to-10 (make-sumer 10))
(define sum-to-pi (make-sumer 3.14))

(println (sum-to-10 6))    ; 6 + 7 + 8 + 9 + 10
(println (sum-to-10 7.5))  ; 7.5 + 8.5 + 9.5
(println (sum-to-pi 1.10)) ; 1.10 + 2.10 + 3.10
;(define (make-sumer n)
;  (letrec ((f (lambda (x)
;    (if (> x n)
;        0
;        (+ x (f (+ x 1)))))))
;  f))
;
;(define sum-to-10 (make-sumer 10))
;(define sum-to-pi (make-sumer 3.14))
;
;(println (sum-to-10 6)) ; 6 + 7 + 8 + 9 + 10
;(println (sum-to-10 7.5)) ; 7.5 + 8.5 + 9.5
;(println (sum-to-pi 1.10)) ; 1.10 + 2.10 + 3.10


;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 1))
;         (fib (- n 2)))))
;
;(fib 42)

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
