;; TODO : UNIT TEST :
;; make-vector
;; vector-length
;; vector-ref
;; vector-set
;; vector->list
;; list->vector
;; vector?
;; pp vector
;; println vector
;; TODO : expand à l'exécution directement ?
;; vector-fill!
;; make vector l init


;(define vv (make-vector 3))

; (println (vector? vv))
; (println (pair? vv))

; (println (vector-length vv))



;(println vv)
; (pp vv)
; (vector-set! vv 0 100)
; (vector-set! vv 1 200)
; (vector-set! vv 2 300)
; (pp vv)

; (pp (vector->list vv))

; (define ll '(100 #f 200 (1 2 3)))

; (pp ll)

; (pp (list->vector ll))


; (define vect (list->vector '(100 #f 200 #t (200 300))))

; (pp vect)
; (vector-fill! vect 1)
; (pp vect)

; (pp (vector->list vect))


(define gg (make-vector 10 #f))

(pp gg)