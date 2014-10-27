
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

(define a (vector))
(define b (vector 1))
(define c (vector 1 2))
(define d (vector 1 2 3 4 5))

(pp (list? a))
(pp (vector? a))
(pp a)

(pp (list? b))
(pp (vector? b))
(pp b)

(pp (list? c))
(pp (vector? c))
(pp c)

(pp (list? d))
(pp (vector? d))
(pp d)

(pp (make-vector 0))
(pp (make-vector 0 #t))
(pp (make-vector 1))
(pp (make-vector 1 #f))
(pp (make-vector 4))
(pp (make-vector 4 #f))

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
;#f
;#t
;#()
;#f
;#t
;#(1)
;#f
;#t
;#(1 2)
;#f
;#t
;#(1 2 3 4 5)
;#()
;#()
;#(0)
;#(#f)
;#(0 0 0 0)
;#(#f #f #f #f)
