
(println (not #t))
(println (not #f))
(println (not 10))
(println (not '()))
(println (not (cons 1 2)))
(println (not (lambda (x y) (+ x y))))
(println (not (> 10 12)))
(println (not (> 12 10)))
(println (not (not (not (not (not (not (> 10 12))))))))

;#f
;#t
;#f
;#f
;#f
;#f
;#t
;#f
;#f
