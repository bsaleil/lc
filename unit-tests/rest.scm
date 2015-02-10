(define (foo1 a b)
	(pp a)
	(pp b))

(define (foo2 a b . c)
	(pp a)
	(pp b)
	(pp c))

(define (foo3 a b)
	(pp a)
	(pp b)
	(set! a 100)
	(pp a)
	(pp b))

(define (foo4 a b . c)
	(pp a)
	(pp b)
	(pp c)
	(set! a 200)
	(pp a)
	(pp b)
	(pp c))

(define (foo5 a b . c)
	(pp a)
	(pp b)
	(pp c)
	(set! a 300)
	(set! c '(1 2 3))
	(pp a)
	(pp b)
	(pp c))

(define (foo6 a b . c)
	(pp a)
	(pp b)
	(pp (null? c))
	(pp c))

(define (foo7 a . b)
	(pp a)
	(pp b))

(define (foo8 a b c d e f . g)
	(pp a)
	(pp f)
	(pp g))

(define foo9 (lambda x x))

(foo1 1 2)
(foo2 1 2)
(foo2 1 2 3 4 5)
(foo3 1 2)
(foo4 1 2 3 4)
(foo5 1 2 3 4)
(foo6 1 2)
(foo7 1 2 3 4 5)
(foo8 0 1 2 3 4 5 6 7 8 9)

(pp (foo9))
(pp (foo9 1))
(pp (foo9 1 2))
(pp (foo9 1 2 3))

;1
;2
;1
;2
;()
;1
;2
;(3 4 5)
;1
;2
;100
;2
;1
;2
;(3 4)
;200
;2
;(3 4)
;1
;2
;(3 4)
;300
;2
;(1 2 3)
;1
;2
;#t
;()
;1
;(2 3 4 5)
;0
;5
;(6 7 8 9)
;()
;(1)
;(1 2)
;(1 2 3)
