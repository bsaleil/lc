
;;; Simple do
(do ((i 0 (+ i 1)))
    ((= i 4) (println #t))
    (println i))

;;; No step
(do ((i 0))
	((= i 4) (println #t))
	(println i)
	(set! i (+ i 1)))

;;; No command
(do ((i 0))
	((= (begin (set! i (+ i 1))
		       (println (- i 1))
		       i) 4) (println #t)))

;;; More complex form
(pp 
	(do ((vec (make-vector 4))
	     (i 0 (+ i 100))
	     (j 0 (+ j 1)))
	    ((= i 400) (println #t) vec)
	    (println j)
	    (vector-set! vec j i))
)

;0
;1
;2
;3
;#t
;0
;1
;2
;3
;#t
;0
;1
;2
;3
;#t
;0
;1
;2
;3
;#t
;#(0 100 200 300)
