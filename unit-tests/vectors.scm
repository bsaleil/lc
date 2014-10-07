
(define vect (make-vector 5))

(println (vector? vect))
(println vect)
(println (vector-length vect))

(vector-set! vect 2 100)

(println vect)
(println (list? (vector->list vect)))
(println (vector->list vect))

(vector-fill! vect #f)

(println vect)

(println (list->vector '(1 2 3 4)))
(println (vector? (list->vector '(1 2 3 4))))
(println (vector-ref (list->vector '(1 2 3 4)) 0))
(println (vector-ref (list->vector '(1 2 3 4)) 1))
(println (vector-ref (list->vector '(1 2 3 4)) 2))
(println (vector-ref (list->vector '(1 2 3 4)) 3))

(println (make-vector 2 #f))
(pp      (make-vector 2 #f))

;#t
;00000
;5
;0010000
;#t
;0010000
;#f#f#f#f#f
;1234
;#t
;1
;2
;3
;4
;#f#f
;#(#f #f)
