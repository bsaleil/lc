
;; Number
(println 0)
(println -0)
(println -000)
(println 12345)
(println -12345)
(println (* (+ 5 2) (- 5 1)))

;; Boolean
(println #t)
(println #f)
(println (not (not (not (<= 10 10)))))

;; Null
(println '())

;; Procedure
(println (lambda (x) (* x x)))

;; Pair
(println (cons 10 20))
(println (cons 10 (cons 20 '())))
(println (cons 99 (cons #f (cons '() '()))))
;;
(println '(1 2 3))
(println '(1 2 (3 4) 5 6))
(println '(1 2 (3 4) 5))

;; Char
(println #\a)
(println #\Z)
(println #\?)
(println #\newline)
(println (integer->char 104))

;; String
(println "Hello World")
(println "éêà")
(println "10€ or 10$")
(println (make-string 5 (integer->char 65)))
(println (substring "Dark Vador" 0 4))

;; Vector
(println (make-vector 4 "Hi."))
(define v (make-vector 4 0))
(vector-set! v 2 "Hey")
(println v)
(println (vector-ref v 0))

;; Symbol
(println 'Hello)
(define sym 'Dark)
(define str " Vador")
(println (string->symbol (string-append (symbol->string sym) str)))

;0
;0
;0
;12345
;-12345
;28
;#t
;#f
;#f
;
;#<procedure>
;1020
;1020
;99#f
;123
;123456
;12345
;a
;Z
;?
;
;
;h
;Hello World
;éêà
;10€ or 10$
;AAAAA
;Dark
;Hi.Hi.Hi.Hi.
;00Hey0
;0
;Hello
;Dark Vador
