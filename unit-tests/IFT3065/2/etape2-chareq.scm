(define a #\a)
(define b #\b)

(define (try p1 p2) (println (char=? p1 p2)))

(try a a)
(try a b)
(try b a)

;#t
;#f
;#f
