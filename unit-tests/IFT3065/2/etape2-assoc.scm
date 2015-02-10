(define a '((1 . 11) (2 . 22) (3 . 33)))

(println (cdr (assoc 2 a)))
(println (assoc 5 a))

;22
;#f
