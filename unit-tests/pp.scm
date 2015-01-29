
(pp 0)
(pp -0)
(pp -000)
(pp 12345)
(pp -12345)
(pp (* (+ 5 2) (- 5 1)))

(pp #t)
(pp #f)
(pp (not (not (not (<= 10 10)))))

(pp '())

(pp (lambda (x) (* x x)))

(pp (cons 10 20))
(pp (cons 10 (cons 20 '())))
(pp (cons 99 (cons #f (cons '() '()))))

(pp '(1 #f 3))
(pp '(1 () (#f 4) 5 #t))
(pp '(1 #f (3 4) ()))

(pp (make-vector 2 42))
(pp (list->vector '(1 2 3 4 5)))
(pp (make-vector 0))

(pp #\a)
(pp #\A)
(pp #\newline)
(pp (string-ref "a a" 1))

(pp "Hello World")
(pp (make-string 4 #\Z))

(pp 'SYMBOL)
(pp (string->symbol (string-append "SYM" (symbol->string 'BOL))))

;0
;0
;0
;12345
;-12345
;28
;#t
;#f
;#f
;()
;#<procedure>
;(10 . 20)
;(10 20)
;(99 #f ())
;(1 #f 3)
;(1 () (#f 4) 5 #t)
;(1 #f (3 4) ())
;#(42 42)
;#(1 2 3 4 5)
;#()
;#\a
;#\A
;#\newline
;#\space
;"Hello World"
;"ZZZZ"
;SYMBOL
;SYMBOL
