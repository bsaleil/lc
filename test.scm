
;; Globals:
;; Add asc and type
;; (asc-global-add ...)
;; (asc-global-get ...)
;; (global-pos ...)
;; (global-stype ...)

;; car/cdr primitive


(define (permutations x)
  (let ((x x)
        (perms (list x)))
    (define (P n)
      (pp "A")
      (if (> n 1)
          (do ((j (- n 1) (- j 1)))
              ((zero? j) (pp n)
               (P (- n 1)))
              (P (- n 1))
              (F n))))
    (define (F n)
      (set! x (revloop x n (list-tail x n)))
      (set! perms (cons x perms)))
    (define (revloop x n y)
      (if (zero? n)
          y
          (revloop (cdr x)
                   (- n 1)
                   (cons (car x) y))))
    (define (list-tail x n)
      (if (zero? n)
          x
          (list-tail (cdr x) (- n 1))))
    (P (length x))
    perms))

;-----

(permutations '(1 2))


;; mlc-define:
;; If it's a lambda, it's a cst lambda:
;; -> do not generate code
;; -> init entry and set stype of global
;; mlc-call:
;; -> check in global asc for stype and identity
;; mlc-identifier:
;; -> if closure does not exist, create it, store it in global slot, and put it


;; Liveness: terminer le travail
;; Letrec: attention aux lates !function
;; Letrec: détecter constantes récursives (a <-> b) (bindings aux bindings cst)
;; Liveness: pb sur '() ?
;; Liveness: cas spécial, set-box! est un kill
;; Liveness: alpha conversion
;; Regalloc: pb movs en trop (fib.s)

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
