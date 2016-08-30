
;; TODO: pb $$atom sur '() ?

;; Constantes:
;;   (1) - Propagation des constantes dans le ctx (cas spéciaux au merge)
;;   (2) - Propagation des constantes sûres (aucun cas spéciaux, cette constante est valable pour toutes les versions)


;(define foo (lambda () 1))



;; (1) Changements au ctx:
;;  * quand on stocke l'identifiant, si a un moment il a une loc mémoire, la garder pour lui et le noter dans l'objet identifier
;;    -> chaque id a donc sa loc de la pile + son emplacement mémoire. (voir si viable avec le regallocs désactivé)
;;  * ajouter un nouveau flag aux identifiants "cstfun" pour les fonctions constantes et stocker l'adresse de son epobj
;;    -> (voir si viable avec le regallocs désactivé)

;; (2) Liveness:
;;  * ajouter une alpha conversion pour gérer les noms de variables identiques
;;  * continuer avec la stratégie utilisée dans liveness.scm

;; (3) Grouper allocations letrec (reprendre et continuer le travail)

;; (?) Repenser completement la manière de représenter un contexte ?
;;  * En pensant à la fusion,
;;  * Aux positions multiples
;;  * Aux constantes statiques
;;  * Aux constantes dynamiques
;;  * Aux types de manière générale


;; -> au let ((lst2 ...)), c'est le lst2 global qui est retourné, pb ctx let


;(define append
;   (lambda lsts
;     (letrec ((append-h
;               (lambda (lsts)
;                 (if (null? lsts)
;                     '()
;                     (let ((lst2 (append-h (cdr lsts))))
;                       lst2)))))
;       (append-h lsts))))

;(define ($$atom l) l)
;(define (gambit$$pp l) (pp l))


;(let ((a ($$atom 1))
;      (b ($$atom "kk")))
; ($$atom a))

(letrec ((a (lambda () 1))
         (b (lambda () (a)))
         (c (lambda () b)))
  (b))


;(define (gambit$$pp n) (pp n))
;
;(let ((aa 11))
;  (let ((bb (lambda () (gambit$$pp "bb")))
;        (cc (lambda () (gambit$$pp aa))))
;    (letrec ((dd (lambda (i) (if (= i 0) 1 (begin (bb) (cc) (dd (- i 1))))))
;             (oo (lambda (i) dd))
;             (ee 100)
;             (ii "jjj")
;             (ff (lambda () (gambit$$pp "ff"))))
;      (gambit$$pp ee)
;      (gambit$$pp ii)
;      (ff)
;      (bb)
;      (cc)
;      ((oo 1) 4)
;      (dd 0))))

;
;(let ((init (lambda () ($$atom #f))))
; (letrec ((for-aux (lambda (lo)
;                     (if (($$atom <) ($$atom lo) ($$atom 2))
;                         (($$atom cons)
;                          (($$atom init))
;                          (let ((lo (($$atom +) ($$atom lo) ($$atom 1))))
;                            (if (($$atom <) ($$atom lo) ($$atom 2))
;                                (($$atom cons)
;                                 ($$atom 2)
;                                 (($$atom for-aux) (($$atom +) ($$atom lo) ($$atom 1))))
;                                '())))
;                         '()))))
;   (($$atom for-aux) ($$atom 0))))





;(define for
;  (lambda (lo hi f)
;
;    (define for-aux
;      (lambda (lo)
;        (if (< lo hi)
;            (cons (f lo) (for-aux (+ lo 1)))
;            '())))
;
;    (for-aux lo)))
;
;(define make-matrix
;  (lambda (init)
;    (for 0 0 (lambda (i) (init 0)))))
;
;
;(define make-maze
;  (lambda (n m) ; n and m must be odd
;    (make-matrix (lambda (i)
;                       (if (and (even? i) (even? 0))
;                           (cons i 0)
;                           #f)))))
;
;(make-maze 1 1)





;;
;; TODO: ##set-box! -> cas spécial, c'est un kill

;(define (liveness ast succs)
;
;  (define (compute-in-out ast succs)
;    (let* ((out
;             (foldr (lambda (succ live)
;                      (set-union
;                        (liveness-in succ)
;                        live))
;                    '()
;                    succs))
;           (in
;             (set-union
;               (ast-use ast)
;               (set-sub out (ast-def ast)))))
;      (liveness-in-set! ast in)
;      (liveness-in-set! ast out)))
;
;  (cond ;; ATOM node
;        ((atom-node? ast)
;           (compute-in-out ast succs))
;        ((eq? (car ast) 'lambda)
;           (error "NYI"))
;        ((eq? (car ast) 'let)
;           (error "NYI"))
;        ((eq? (car ast) 'letrec)
;           (error "NYI"))
;        ((or (eq? (car ast) 'define)
;             (eq? (car ast) 'set!))
;           (compute-in-out ast succs)
;           (liveness (caddr ast) (list ast)))
;        ;; IF node
;        ((if-expr? ast)
;           (liveness (caddr ast) succs)
;           (liveness (cadddr ast) succs)
;           (liveness (cadr ast) (list (caddr ast) (cadddr ast)))
;           (compute-in-out ast succs))
;        ;; Others
;        (else
;           (compute-in-out ast succs)
;           (let loop ((exprs (reverse ast))
;                      (succs (list ast)))
;             (if (not (null? exprs))
;                 (begin (liveness (car exprs) succs)
;                        (loop (cdr exprs) (list (car exprs)))))))))


;(define (foo n)
;  (pp n)
;  (set! n 10)
;  (pp n))
;
;(foo 33)


;* Liveness
;* Allocation groupées
;* Propagation des constantes
;* Regalloc: registre préféré (opérandes)
;* Regalloc: pb des movs en trop (voir fib.s)


;; Si juste bbv
;;  * on découvre le type à n < 2
;;  * pas de test généré pour n-1 et n-2
;;  * mais toujours test +

;; Si pt entrée
;;  * on connait le type pas de test à < n 2
;;  * pas de test a n-1 et n-2
;;  * mais toujours à +

;; Si pt de retour
;;  * on connait le type pas de test à < n 2
;;  * pas de test a n-1 et n-2
;;  * pas de test à +
;;  -> plus aucun test
;; sauf le tout tout premier <2

;; Si pr entrée
;; on connait déjà le type de


;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 1))
;         (fib (- n 2)))))
;
;(fib 40)

;(define shorterp
;  (lambda (x y)
;    (if (null? y)
;        #f
;        (let ((#:g0 (null? x)))
;          (if #:g0
;              #:g0
;              (shorterp (cdr x) (cdr y)))))))
;
;(shorterp '(18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0) '(12 11 10 9 8 7 6 5 4 3 2 1 0)))
;




;(define (foo)
;  (make-vector 5))
;
;(pp (foo))
;(println (make-vector 5))

;; Au moment d'un binding d'un id, regarder si c'est une fonction non mutable.
;; Si c'est le cas:
;;   - stocker dans le ctx l'id de fn global
;;   - à l'appel, on peut vérifier ça et utiliser l'info

;
;(define (run n)
;  (let loop ((i n) (sum 0))
;    (if (< i 0)
;      sum
;      (loop (- i 1) (+ i sum)))))
;
;(run 10000)

;(apply do-loop (list 100000000))

;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 2))
;         (fib (- n 1)))))
;
;(fib 40)

;(define (fibfp n)
;  (if (< n 2.)
;    n
;    (+ (fibfp (- n 1.))
;       (fibfp (- n 2.)))))
;
;(time
;(apply fibfp (list 30.)))


;(define sum 0)
;
;(define (do-loop n)
;  (set! sum 0)
;  (do ((i 0 (+ i 1)))
;      ((>= i n) sum)
;    (set! sum (+ sum 1))))
;
;($apply do-loop '(100000000))


;; gen-version-fn:
;;   * on génère le code avec le générateur
;;   * on va lire le premier octet du code généré
;;   * si l'octet est 0xeb, c'est un jmp rel8
;;   * si l'octet est 0xe9, c'est un jmp rel32
;;   * les autres jumps ne sont pas optimisables
;;
;;   -> si on obtient un jump
;;   -> on va lire l'opérande pour récupérer l'adresse de destination qu'on stocke dans un label
;;   -> on remet code-alloc à la position de ce jump, on peut écraser son contenu
;;   -> on retourne se label comme étant le label de la version

;(define (foo n)
;  (eq? n 10))
;
;(pp (foo #f))
;(pp (foo 10))
;(pp (foo 1))
;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 1))
;         (fib (- n 2)))))
;
;($apply fib '(40))

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

;(define (fib n)
;  (if (< n 2)
;    n
;    (+ (fib (- n 1))
;       (fib (- n 2)))))
;
;(fib 40)
