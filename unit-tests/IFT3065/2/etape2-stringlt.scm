(define a "a")
(define b "b")
(define c "ab")

(define (try p1 p2) (println (string<? p1 p2)))

(try a a)
(try a b)
(try a c)

(try b a)
(try b b)
(try b c)

(try c a)
(try c b)
(try c c)

;#f
;#t
;#t
;#f
;#f
;#f
;#f
;#t
;#f
