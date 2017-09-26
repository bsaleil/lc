(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

(println (fib 40))










;(define (bar f)
;  (gambit$$pp f))
;
;(define (foo n)
;  (let ((f (lambda (m) (+ n m))))
;    (bar f)))
;
;(define rr (cons 10 20))
;
;(foo (car rr))
;;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;;; A good test of first class procedures and tail recursion.
;
;(define (cpstak x y z)
;
;  (define (tak x y z k)
;    (if (not (< y x))
;        (k z)
;        (tak (- x 1)
;             y
;             z
;             (lambda (v1)
;               (tak (- y 1)
;                    z
;                    x
;                    (lambda (v2)
;                      (tak (- z 1)
;                           x
;                           y
;                           (lambda (v3)
;                             (tak v1 v2 v3 k)))))))))
;
;  (tak x y z (lambda (a) a)))
;
;(cpstak 1 0 3)



;(define (tak x y k)
;  (if (>= y x)
;      (k 11)
;      (tak (- x 1)
;           y
;           (lambda (v1)
;             (tak (- y 1)
;                  11
;                  k)))))
;
;(tak 1 0 (lambda (a) a))


;* Ajouter test unitaire
;* Fixer mov inutile au prep-args du test unitaire
;* vérifier que la constante des fonctions !purecst est propagée interprocéduralement si demandé
;* vérifier que la constante !purecst n'est PAS propagée au call/ret si pas demandé
;* optimiser l'appel avec l'information d'id !cst
;; ADD THIS EXAMPLE TO UT
;(define (tak x k)
;  (if (= x 0)
;      (k)
;      (tak 0 (lambda ()
;                   (k)))))
;(tak 1 (lambda () 100))
; * Propager l'identité quand la fonction est !cst
; * étudier le cst et cst-id de letrec (utile?)



;; WIP:
;; 1. Propagate continuation when call is inlined
;; 2. optimization when --disable-return-points ?
;;
;; a. continuation jmp optimization with return site patching (generic + specialized)
;; b. continuation inlining

;; c. do *not* allocate slot in cc/cr-table is call/return is inlined


;
;(define (map2 f l1 l2)
;  (if (null? l1)
;      l1
;      (list (cons 'a 5))))
;
;(define (peval proc args)
;    (let ((parms (cadr proc))  ; get the parameter list
;          (body  (caddr proc))) ; get the body of the procedure
;
;      (list 'lambda
;            (beta-subst ; in the body, replace variable refs to the constant
;              body      ; parameters by the corresponding constant
;              (map2 (lambda (x y) (cons x y))
;                    parms
;                    args)))))
;
;(define (assq el lst)
;  (cond ((null? lst) #f)
;        ((eq? el (car (car lst))) (car lst))
;        (else (assq el (cdr lst)))))
;
;(define (beta-subst exp env) ; return a modified 'exp' where each var named in
;  (define (bs exp)           ; 'env' is replaced by the corresponding expr (it
;    (cond
;          ((symbol? exp)
;             (gambit$$pp env)
;             (gambit$$pp exp)
;             (gambit$$pp (assq exp env))
;             (error "K"))
;
;
;          (else
;           (map bs exp))))
;  (if (fixnum? exp)
;      exp
;      (bs (caddr exp))))
;
;
;(define example5
;    '(lambda (a) 11))
;
;(define example6
;  '(lambda ()
;     (let 11
;       fib)))
;
;(peval example5 (list 5))
;(peval example6 '())

;-----

;;------------------------------------------------------------------------------

;(define (foo xp yp x y)
;  (let loop ((c #f) (i 0) (j 0))
;
;    (if (< i 0)
;      0
;      (if (or (> (vector-ref yp 0) y)
;              (>= x (vector-ref xp i)))
;        (loop c (- i 1) i)
;        (loop c (- i 1) i)))))
;
;(let ((xp (vector  1.0))
;      (yp (vector  2.0)))
;  (gambit$$pp (foo xp yp .5 .5)))




;loop sans const:
;main:
;    mov r1, i
;    mov r2, sum
;    cmp r1, r2
;    jge belse
;    mov rr, sum
;    ret
;belse:
;    mov r1, i
;    sub r1, 1
;    mov r2, i
;    add r2, sum
;    call main

;(define (create-n n)
;  (do ((n n (- n 1))
;       (a '() (cons '() a)))
;      ((= n 0) a)))
;
;(define *ll* (create-n 200))
;
;(define (recursive-div2 l)
;  (cond ((null? l) '())
;        (else (cons (car l) (recursive-div2 (cddr l))))))
;
;(pp (length (recursive-div2 (create-n 1000))))



;;-------------------------

;(pp (##lc-exec-stats (lambda () (fib 40))))


;(let ((r (##lc-time (lambda () (fib 40)))))
;  (pp "R IS")
;  (pp r))



;; CONST VERS HEURISTIQUES:
  ;; * Nb versions cst/non-cst
  ;; * Certains entiers petits / grands
  ;; * Cas spéciaux p.e. map, etc...
  ;; * Versioner si la cst est dans l'appel

;; On peut versionner avec le type null.
;;   -> à la construction d'une liste, la constante est retournée.
;;   -> à chaque cons, on peut conserver dans le contexte le type: liste/length/type (0/1/2)
;; Ex. * fonctionne avec une fonction récursive qui créé une liste, mais une version par appel
;;     * fonctionne avec (list ...), on a l'information tout de suite.
;;     * fonctionne avec (make-list .. ..)
;; * Permet de conserver le type au car
;; * Permet de conserver le type au cdr
;; * Permet d'optimiser (length l)

;; -> On peut faire pareil avec le type vector: conserver le type: vector/length/type
;; ex. avec (make-vector 10 #\R) -> vector/10/char
;; Permet de conserver le type au vector-ref
;; Permet d'éventuellement conserver le type au vector-set
;; Permet d'optimiser (vector-length v)
;; Permet déviter l'utilisation des vecteurs homogènes -> comportement dynamique

;* Full interp versions 5 8 10 15
;* EP only 5 8 10 15
;* RP only 5 8 10 15
;
;--max-versions 5
;--max-versions 5 --enable-const-vers --const-vers-types boo --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types cha --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types clo --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types str --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types vec --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types nul --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types pai --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types int --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types flo --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types voi --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types sym --enable-cxoverflow-fallback


;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 1))
;         (fib (- n 2)))))
;
;(gambit$$pp (fib 30))



;; Versions: ctx -> label
;; EPTable:  stack -> label



;; Vérifier tous les opt-const-vers:

    ;; ast.scm 2
    ;; ctx.scm 9
    ;; OK core.scm 1
    ;; OK main.scm 1
    ;; OK codegen  1


;; Si on atteint la limite du nb de versions:
;; Si c'est un entry un tail, on doit forcément avoir un fallback.
;; 1. Déplacer les arguments dans les bons registres. (rien à faire si pas de cst ni flo)
;; 2. Récupérer un ctx générique, ou le ctx generique du fallback
;; 3. Set le ctx générique (si n'existe pas)
;; 4. Récupérer la version générique du fallback, ou la générer si elle n'existe pas


;; mazefun
;; compiler

;; out.scm 1333

;; 1. Au retour, on regarde le nb de params, et si c'est des csts
;;    Si c'est pas le cas, rien à faire.
;;    Si c'est le cas, on ajoute une entrés dans la table d'assoc fib: cst -> res
;;      avec res la cst si c'est une cst, ou la valeur du registre sinon

;; 2. A chaque compilation d'un appel à fib, si l'association existe,
;;    -> on push simplement la cst

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
