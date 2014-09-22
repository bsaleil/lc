
(println 0)
(println -0)
(println -000)
(println 12345)
(println -12345)
(println (* (+ 5 2) (- 5 1)))

(println #t)
(println #f)
(println (not (not (not (<= 10 10)))))

(println '())

(println (lambda (x) (* x x)))

(println (cons 10 20))
(println (cons 10 (cons 20 '())))
(println (cons 99 (cons #f (cons '() '()))))

(println '(1 2 3))
(println '(1 2 (3 4) 5 6))
(println '(1 2 (3 4) 5))

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
