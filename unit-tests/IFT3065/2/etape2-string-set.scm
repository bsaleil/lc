(define a (make-string 5))

(string-set! a 0 #\b)
(string-set! a 1 #\a)

(println (char->integer (string-ref a 0)))
(println (char->integer (string-ref a 1)))

;98
;97
