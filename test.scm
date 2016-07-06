

(define (foo n)
  (if (not (eq? 1 n))
      1
      2))

(foo 10)
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
