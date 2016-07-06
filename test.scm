
(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
         (fib (- n 2)))))

($apply fib '(40))


;; Dans gen-call-sequence:
  ;; -> Dire que l'eploc-direct est le stub.
  ;; -> ajouter un label pour le chargement de l'adresse du stub (pour le patch)

;; Dans gen-version-fn (pour le moment dans patch-closure):
  ;; -> a l'avenir dans patch-closure-ep aussi.
  ;; -> on a la cctable et l'index, patcher tous les jmp

;; Diviser gen-call-sequence pour les différents cas possibles

;(define (fib n
;  (if (< n 2)
;      1
;      (+ (fib (- n 1))
;         (fib (- n 2)))))
;
;(fib 40)

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
