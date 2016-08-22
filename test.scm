;
;WIP:
;  - si nb-opnds est passé,
;  - on récupère les registres associés depuis la pile
;  - on récupère le registre lié au plus profond dans la pile (opérande la plus à gauche)
;  - on le retourne
;
;Donc au moment de générer le résultats:
;  - quand on fait (ctx-pop n nb-opnds), ca va clean le ctx
;  - puis on push la nouvelle valeur avec le nouveau registre

(define foo 33)

(gambit$$lceval '(println foo))

;(define (fibo n)
;   (if (or (= n 0) (= n 1))
;      n
;      (+ (fibo (- n 1)) (fibo (- n 2)))))
;
;(println (fibo 35))

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
