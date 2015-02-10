(define a '((1 . 11) (2 . 22) (3 . 33)))

(println (member 99 a))
(println (length (member (cons 2 22) a)))

;#f
;2
