(define t1 64)
(define t2 33)

(define (try p1 p2) (println (eqv? (integer->char p1) p2)))

(try t1 #\@)
(try t2 #\!)
(try t2 #\x)

;#t
;#t
;#f
