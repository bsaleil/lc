
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


(let ((f #f)
	  (g #f))
   (set! f (lambda (n)
              (if (= 0 n)
              	(write-char #\P)
              	(g (- n 1)))))
   (set! g (lambda (n)
              (if (= 0 n)
              	(write-char #\I)
              	(f (- n 1)))))
   (f 0)
   (f 1)
   (f 2)
   (f 3)
   (f 4)
   (f 5)
   (newline))

;1
;2
;3
;4
;5
;6
;7
;PIPIPI
