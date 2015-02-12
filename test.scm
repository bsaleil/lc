;/
;abs
;apply
;call/cc
;exact->inexact
;peek-char
;rational?
;truncate

(pp "BEGIN")

(define (foo)
	(letrec ((a 55)
	         (b 56)
	         (c b)
	         (d 57))
	  (foo)))

(foo)