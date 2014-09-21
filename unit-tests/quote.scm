
(println '())
(println (list? '()))
(println (pair? '()))
(println (length '()))

(println '(1 #f 3))
(println (list? '(1 2 3)))
(println (pair? '(1 2 3)))
(println (length '(1 2 3)))

(println '(10 20 (30 40) 50))
(println (list? '(10 20 (30 40) 50)))
(println (pair? '(10 20 (30 40) 50)))
(println (length '(10 20 (30 40) 50)))

(println (null? (car (cdr '(1 () 3)))))

(println '(1 . 2))
(println (list? '(1 . 2)))
(println (pair? '(1 . 2)))

;
;#t
;#f
;0
;1#f3
;#t
;#t
;3
;1020304050
;#t
;#t
;4
;#t
;12
;#f
;#t
