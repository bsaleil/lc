
(println (cons 1 2))
(println (length (cons 1 '())))
(println (length (cons (cons 1 2) '())))
(println (length (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))
(println (length (cons 1 (cons #f (cons '() (cons (lambda (x) x) '()))))))

(define a (list))
(define b (list 1))
(define c (list 1 2))
(define d (list 1 2 3 4 5))

(pp (pair? a))
(pp (list? a))
(pp (null? a))
(pp (length a))
(pp a)

(pp (pair? b))
(pp (list? b))
(pp (null? b))
(pp (length b))
(pp b)

(pp (pair? c))
(pp (list? c))
(pp (null? c))
(pp (length c))
(pp c)

(pp (pair? d))
(pp (list? d))
(pp (null? d))
(pp (length d))
(pp d)

;12
;1
;1
;5
;4
;#f
;#t
;#t
;0
;()
;#t
;#t
;#f
;1
;(1)
;#t
;#t
;#f
;2
;(1 2)
;#t
;#t
;#f
;5
;(1 2 3 4 5)
