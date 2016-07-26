
(define  (interval-list m n)
  (if (> m n)
    '()
    (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ((remove-multiples
            (lambda (n l)
              (if (null? l)
                '()
                (if (= (modulo (car l) n) 0)
                  (remove-multiples n (cdr l))
                  (cons (car l)
                        (remove-multiples n (cdr l))))))))
    (if (null? l)
      '()
      (cons (car l)
            (sieve (remove-multiples (car l) (cdr l)))))))

(define (execs n)
  (if (> n 0)
      (begin (sieve GLLIST)
             (execs (- n 1)))))

(define GLLIST '(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100))

(execs 100000)

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
