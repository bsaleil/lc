
(println
	((lambda (n)
		n) 1))

(println ((lambda (n)
		 	(set! n 2)
			n) 100))

(println ((lambda (n)
	      	((lambda () (set! n 3)))
			n) 100))

(println ((lambda (n)
			((lambda (n) (set! n 100)) 200)
			n) 4))

(let ((a 100)
	  (b 200))
   (set! a 3)
   (set! b 2)
   (println (+ a b)))

(define global 100)
(set! global 6)
(println global)

((lambda ()
    (set! global 7)
    (println global)))

;1
;2
;3
;4
;5
;6
;7
