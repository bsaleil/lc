;;; TRIANGL -- Board game benchmark.

(define *board*
  (list->vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (list->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (list->vector '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                  13 7 8 4 4 7 11 8 12 13 6 10
                  15 9 14 13 13 14 15 9 10
                  6 6)))

(define *b*
  (list->vector '(2 4 7 5 8 9 3 6 10 5 9 8
                  12 13 14 8 9 5 2 4 7 5 8
                  9 3 6 10 5 9 8 12 13 14
                  8 9 5 5)))

(define *c*
  (list->vector '(4 7 11 8 12 13 6 10 15 9 14 13
                  13 14 15 9 10 6 1 2 4 3 5 6 1
                  3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())

(define (attempt i depth)
  (cond ((= depth 14)
         (set! *answer*
               (cons (cdr (vector->list *sequence*)) *answer*))
         #t)
        ((and (= 1 (vector-ref *board* (vector-ref *a* i)))
              (= 1 (vector-ref *board* (vector-ref *b* i)))
              (= 0 (vector-ref *board* (vector-ref *c* i))))
         (vector-set! *board* (vector-ref *a* i) 0)
         (vector-set! *board* (vector-ref *b* i) 0)
         (vector-set! *board* (vector-ref *c* i) 1)
         (vector-set! *sequence* depth i)
         (do ((j 0 (+ j 1))
              (depth (+ depth 1)))
             ((or (= j 36) (attempt j depth)) #f))
         (vector-set! *board* (vector-ref *a* i) 1)
         (vector-set! *board* (vector-ref *b* i) 1)
         (vector-set! *board* (vector-ref *c* i) 0) #f)
        (else #f)))

(define (test i depth)
  (set! *answer* '())
  (attempt i depth))
  
;-----

(test 22 1)

;; TODO: Quote cst
;; TODO: attention aux constantes qui sont mem-allocated pour les primitives et appels, etc...
;; TODO: GERER tous les cas ou les opérandes sont toutes cst, donc le résultat l'est aussi
;; TODO: pour (dé)sactiver les cst interprocédural:
;;   - Ajouter le support pour apply-moves et ctx-get-call-args-moves
;;   - Dans ctx-init-fn, modifier la stack pour enlever la cst
;; TODO: ajouter le support des constantes dans les globales non mutables


;(define (bar a b c d)
;  (println a b c d))
;
;(apply bar 22)


;; Liveness: terminer le travail
;; Letrec: attention aux lates !function
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
