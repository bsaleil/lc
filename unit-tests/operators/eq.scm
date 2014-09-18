
(println (eq?  10  10))
(println (eq?  10 -10))
(println (eq? -10  10))
(println (eq? -10 -10))
(println (eq? (- 20 10) (+ 5 5)))

(println (eq? #f 10))
(println (eq? #f #t))
(println (eq? #f #f))
(println (eq? #t #t))

(println (eq? '() 10))
(println (eq? '() #f))
(println (eq? '() '()))

(println (eq? (cons 1 2) 10))
(println (eq? (cons 1 2) #t))
(println (eq? (cons 1 2) '()))
(println (eq? (cons 1 2) (cons 1 2)))

(println (eq? (lambda (x y) (* x y)) 10))
(println (eq? (lambda (x y) (* x y)) #f))
(println (eq? (lambda (x y) (* x y)) '()))
(println (eq? (lambda (x y) (* x y)) (cons 1 2)))
(println (eq? (lambda (x y) (* x y)) (lambda (x y) (* x y))))

;#t
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
