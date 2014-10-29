


(println (char? 0))
(println (char? 99999999))
(println (char? '()))
(println (char? #t))
(println (char? #f))
(println (char? (list 1 2 3)))
(println (char? (vector 1 2 3)))
(println (char? (lambda (n) n)))
(println (char? #\newline))
(println (char? #\a))

;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
