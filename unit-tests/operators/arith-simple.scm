
(println (+ 0 0))
(println (+ 0 -0))
(println (+ 10 0))
(println (+ 0 10))
(println (+ 0 -10))
(println (+ 10 10))
(println (+ -10 10))
(println (+ (+ 2 1) (+ 2 2)))

(println 123456789)

(println (- 0 0))
(println (- 0 -0))
(println (- 10 0))
(println (- 0 10))
(println (- 0 -10))
(println (- 10 10))
(println (- -10 10))
(println (- (+ -10 15) (- 10 12)))

(println 123456789)

(println (* 0 0))
(println (* 0 -0))
(println (* 10 0))
(println (* 0 10))
(println (* 0 -10))
(println (* 10 10))
(println (* -10 10))
(println (- (+ -10 15) (- 10 (* 2 6))))

(println 123456789)

(println (quotient 0 10))
(println (quotient 9 10))
(println (quotient 10 10))
(println (quotient 11 10))
(println (quotient -10 10))
(println (quotient 10 -10))
(println (quotient -10 -10))
(println (- (+ -10 (quotient 156 10)) (- 10 (* 2 6))))

(println 123456789)

(println (modulo 0 10))
(println (modulo 9 10))
(println (modulo 9 -10))
(println (modulo -9 10))
(println (modulo -9 -10))
(println (modulo 10 10))
(println (modulo 11 10))
(println (modulo -10 10))
(println (modulo 10 -10))
(println (modulo -10 -10))
(println (- (+ -10 (quotient 156 10)) (- 10 (* 2 (modulo 48 7)))))

;0
;0
;10
;10
;-10
;20
;0
;7
;123456789
;0
;0
;10
;-10
;10
;0
;-20
;7
;123456789
;0
;0
;0
;0
;0
;100
;-100
;7
;123456789
;0
;0
;1
;1
;-1
;-1
;1
;7
;123456789
;0
;9
;-1
;1
;-9
;0
;1
;0
;0
;0
;7
