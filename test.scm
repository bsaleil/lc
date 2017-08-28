(define make-global-environment #f)
(set! make-global-environment (lambda () (env-frame #f '())))
(define (env-frame env vars) (vector (cons vars #f) '() '() env))

(define (parse-program program env module-name proc)
  (define (parse-prog program env lst proc)
    (if (null? program)
        (begin (pp "BASE")
               (proc))
        (parse-prog '() '() '() proc)))


  (parse-prog '() '() '()
   (lambda ()
     (pp "BASE-cont")
     (proc '() env #f))))

(define (jump-info nb-args)
  (let ((nb-stacked (max 0 (- nb-args nb-arg-regs))))
    (define (location-of-args i)
      (if (> i nb-args)
          '()
          (cons (cons i
                      (if (> i nb-stacked)
                          (make-reg (- i nb-stacked))
                          (make-stk i)))
                (location-of-args (+ i 1)))))
    (make-pcontext
     nb-stacked
     (cons (cons 'return (make-reg 0)) (location-of-args 1)))))
(define (label-info min-args nb-parms rest? closed?)
  (let ((nb-stacked (max 0 (- nb-parms nb-arg-regs))))
    (define (location-of-parms i)
      (if (> i nb-parms)
          '()
          (cons (cons i
                      (if (> i nb-stacked)
                          (make-reg (- i nb-stacked))
                          (make-stk i)))
                (location-of-parms (+ i 1)))))
    (let ((x (cons (cons 'return 0) (location-of-parms 1))))
      (make-pcontext
       nb-stacked
       (if closed?
           (cons (cons 'closure-env (make-reg (+ nb-arg-regs 1))) x)
           x)))))
(define (prim-info name)
  (let ((x (assq name prim-proc-table))) (if x (cdr x) #f)))
(define nb-gvm-regs 5)
(define (end!) '())
(define (dump) '())
(define return-reg '())
(define (make-reg num) num)

(define (make-target version name)
  (let ((x (make-vector 11))) (vector-set! x 1 name) x))
(define (target-name x) (vector-ref x 1))
(define (target-begin! x) (vector-ref x 2))
(define (target-begin!-set! x y) (vector-set! x 2 y))
(define (target-end! x) (vector-ref x 3))
(define (target-end!-set! x y) (vector-set! x 3 y))
(define (target-dump x) (vector-ref x 4))
(define (target-dump-set! x y) (vector-set! x 4 y))
(define (target-nb-regs x) (vector-ref x 5))
(define (target-nb-regs-set! x y) (vector-set! x 5 y))
(define (target-prim-info x) (vector-ref x 6))
(define (target-prim-info-set! x y) (vector-set! x 6 y))
(define (target-label-info x) (vector-ref x 7))
(define (target-label-info-set! x y) (vector-set! x 7 y))
(define (target-jump-info x) (vector-ref x 8))
(define (target-jump-info-set! x y) (vector-set! x 8 y))
(define (target-proc-result x) (vector-ref x 9))
(define (target-proc-result-set! x y) (vector-set! x 9 y))
(define (target-task-return x) (vector-ref x 10))
(define (target-task-return-set! x y) (vector-set! x 10 y))
(define targets-loaded '())
(define MTAR #f)
(define (get-target name)
  (let ((x (assq name targets-loaded)))
    (if x (cdr x) (error "Target package is not available"))))
(define (put-target targ)
  (let* ((name (target-name targ)) (x (assq name targets-loaded)))
    (if x
        (set-cdr! x targ)
        (set! targets-loaded (cons (cons name targ) targets-loaded)))
    '()))
(define (default-target)
  (if (null? targets-loaded)
      (error "No target package is available")
      (car (car targets-loaded))))
(define (select-target! name info-port)
  (set! target (get-target name))
  ((target-begin! target) info-port)
  '())
(define (unselect-target!) ((target-end! target)) '())
(define target '())
(define target.dump '())
(define target.nb-regs '())
(define target.prim-info '())
(define target.label-info '())
(define target.jump-info '())
(define target.proc-result '())
(define target.task-return '())
(define **not-proc-obj '())
(define (target.specialized-prim-info* name decl)
  (let ((x (target.prim-info* name decl)))
    (and x ((proc-obj-specialize x) decl))))
(define (target.prim-info* name decl)
  (and (if (standard-procedure name decl)
           (standard-binding? name decl)
           (extended-binding? name decl))
       (target.prim-info name)))



       (define (begin! info-port targ)
         (set! return-reg (make-reg 0))
         (target-end!-set! targ end!)
         (target-dump-set! targ dump)
         (target-nb-regs-set! targ nb-gvm-regs)
         (target-prim-info-set! targ prim-info)
         (target-label-info-set! targ label-info)
         (target-jump-info-set! targ jump-info)
         (target-proc-result-set! targ (make-reg 1))
         ;(target-task-return-set! targ return-reg)
         ;(set! *info-port* info-port)
         '())

         (define (pp n) (gambit$$pp n))

         (define (char-whitespace? c)
           (let ((c (char->integer c)))
             (or (= c 32) (= c 9) (= c 10) (= c 12) (= c 13))))

         (define (memv el lst)
           (cond ((null? lst) #f) ((eqv? el (car lst)) lst) (else (memv el (cdr lst)))))

         (define (char-numeric? c)
           (let ((c (char->integer c))) (and (> c 47) (< c 58))))

         (define (string->number str . l)
           (define (s->n str pos)
             (if (= pos (string-length str))
                 ""
                 (let ((c (string-ref str pos)))
                   (if (char-numeric? c)
                       (let ((r (s->n str (+ pos 1))))
                         (if r (string-append (make-string 1 c) r) #f))
                       #f))))
           (if (= (string-length str) 0) #f (s->n str 0)))

         (define (string=?-h str1 str2 pos)
           (cond ((= pos (string-length str1)) (= pos (string-length str2)))
                 ((= pos (string-length str2)) #f)
                 (else
                  (if (char=? (string-ref str1 pos) (string-ref str2 pos))
                      (string=?-h str1 str2 (+ pos 1))
                      #f))))
         (define (string=? str1 str2) (string=?-h str1 str2 0))

         (define (length l)
           (let loop ((l l) (len 0))
             (cond ((null? l) len)
                   ((pair? l) (loop (cdr l) (+ 1 len)))
                   (else (error "LIST expected")))))

         (define (call/cc . n)
           (let ((l (length n)))
             (cond ((= l 1) ((car n) #f))
                   ((= l 2) ((car n) #f (cadr n)))
                   (else (error "call/cc")))))

         (define (assq el lst)
           (cond ((null? lst) #f)
                 ((eq? el (car (car lst))) (car lst))
                 (else (assq el (cdr lst)))))

 (define (##append-two lst1 lst2)
   (define (append-twon lst1 lst2)
     (let loop ((lst1 (reverse lst1)) (r lst2))
       (if (null? lst1) r (loop (cdr lst1) (cons (car lst1) r)))))
   (define (append-twok lst1 lst2 k)
     (if (null? lst1)
         (k lst2)
         (append-twok (cdr lst1) lst2 (lambda (r) (k (cons (car lst1) r))))))
   (append-twok lst1 lst2 (lambda (r) r)))


(define (valid-module-name? module-name)
  (define (valid-char? c)
    (and (not (memv c
                    '(#\#
                      #\;
                      #\(
                      #\)
                      #\space
                      #\[
                      #\]
                      #\{
                      #\}
                      #\"
                      #\'
                      #\`
                      #\,)))
         (not (char-whitespace? c))))
  (let ((n (string-length module-name)))
    (and (> n 0)
         (not (string=? module-name "."))
         (not (string->number module-name 10))
         (let loop ((i 0))
           (if (< i n)
               (if (valid-char? (string-ref module-name i)) (loop (+ i 1)) #f)
               #t)))))

(define (with-exception-handling proc)
  ;(let ((old-exception-handler throw-to-exception-handler))
    (let ((val (call-with-current-continuation
                (lambda (cont)
                  (proc)))))
      val))

(define (compile-program program target-name opts module-name dest info-port)
  (define (compiler-body)
    (if (not (valid-module-name? module-name))
        1
        (begin
          (set! target (get-target target-name))
          ((target-begin! target) info-port)
          (parse-program
           (list '#(11 #(expr 11 #f)))
           (make-global-environment)
           module-name
           (lambda (lst env c-intf)
             (pp "KK"))))))
  (let ((successful (with-exception-handling compiler-body)))
    successful))

(let ((targ (make-target 4 'm68000)))
  (target-begin!-set! targ (lambda (info-port) (begin! info-port targ)))
  (put-target targ))

(define input-source-code '11)

(compile-program
         input-source-code
         'm68000
         '(asm)
         "program"
         "program"
         #f)




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
