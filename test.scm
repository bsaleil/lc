;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

(define (lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (car x)))
        (if (eq? (car pair) key)
          pair
          (loop (cdr x)))))))

(define properties '())


(define (put key1 key2 val)
  (let ((x (lookup key1 properties)))
    (if x
      1
      (set! properties
        (cons (list key1 (cons key2 val)) properties)))))

(define (init n m npats ipats)
  (let ((ipats ipats))

    (do ((a 0))
        ((= 1 0) 0)
        (put 100
             200
             (do ((i 0 0)
                  (a 0))
                 ((zero? 0) 0)
                 1)))))

(init 1 0 0 '(()))



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
