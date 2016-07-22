

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
