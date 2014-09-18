
(println (number?  1))
(println (number? -1))
(println (number?  0))
(println (number? #t))
(println (number? #f))
(println (number? '()))
(println (number? (cons 1 2)))
(println (number? (cons 1 (cons 2 '()))))
(println (number? (lambda (a b c) (+ a c))))

(println 123456789)

(println (boolean?  1))
(println (boolean? -1))
(println (boolean?  0))
(println (boolean? #t))
(println (boolean? #f))
(println (boolean? '()))
(println (boolean? (cons 1 2)))
(println (boolean? (cons 1 (cons 2 '()))))
(println (boolean? (lambda (a b c) (+ a c))))

(println 123456789)

(println (null?  1))
(println (null? -1))
(println (null?  0))
(println (null? #t))
(println (null? #f))
(println (null? '()))
(println (null? (cons 1 2)))
(println (null? (cons 1 (cons 2 '()))))
(println (null? (lambda (a b c) (+ a c))))

(println 123456789)

(println (pair?  1))
(println (pair? -1))
(println (pair?  0))
(println (pair? #t))
(println (pair? #f))
(println (pair? '()))
(println (pair? (cons 1 2)))
(println (pair? (cons 1 (cons 2 '()))))
(println (pair? (lambda (a b c) (+ a c))))

(println 123456789)

(println (procedure?  1))
(println (procedure? -1))
(println (procedure?  0))
(println (procedure? #t))
(println (procedure? #f))
(println (procedure? '()))
(println (procedure? (cons 1 2)))
(println (procedure? (cons 1 (cons 2 '()))))
(println (procedure? (lambda (a b c) (+ a c))))

(println 123456789)

(println (list?  1))
(println (list? -1))
(println (list?  0))
(println (list? #t))
(println (list? #f))
(println (list? '()))
(println (list? (cons 1 2)))
(println (list? (cons 1 (cons 2 '()))))
(println (list? (lambda (a b c) (+ a c))))

;#t
;#t
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;123456789
;#f
;#f
;#f
;#t
;#t
;#f
;#f
;#f
;#f
;123456789
;#f
;#f
;#f
;#f
;#f
;#t
;#f
;#f
;#f
;123456789
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#f
;123456789
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;123456789
;#f
;#f
;#f
;#f
;#f
;#t
;#f
;#t
;#f
