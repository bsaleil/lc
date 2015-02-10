(define t1 42)
(define t2 #f)
(define t3 #\x)
(define t4 "abc")
(define t5 '(1 2 3))
(define t6 '())
(define t7 cons)

(define (try p) (println (number? p)))

(try t1)
(try t2)
(try t3)
(try t4)
(try t5)
(try t6)
(try t7)

;#t
;#f
;#f
;#f
;#f
;#f
;#f
