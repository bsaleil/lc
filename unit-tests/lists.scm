
(println (cons 1 2))
(println (length (cons 1 '())))
(println (length (cons (cons 1 2) '())))
(println (length (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))
(println (length (cons 1 (cons #f (cons '() (cons (lambda (x) x) '()))))))

;12
;1
;1
;5
;4
