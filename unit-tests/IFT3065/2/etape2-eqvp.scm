(define a #f)
(define b 0)
(define c (cons 11 22))
(define d '(11 . 22))

(define (try p1 p2) (println (eqv? p1 p2)))

(try a a)
(try a b)
(try a c)
(try a d)

(try b a)
(try b b)
(try b c)
(try b d)

(try c a)
(try c b)
(try c c)
(try c d)

(try d a)
(try d b)
(try d c)
(try d d)

;#t
;#f
;#f
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#t
