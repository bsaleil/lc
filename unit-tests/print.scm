
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
