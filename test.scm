; (do ((a 0 (+ a 1))
;      (b 0 (+ b 2)))
;   ((= a 6) (println 10) 20)
;   (println a))

; =

; (letrec ((LAFUN (lambda (a b)
; 				 (if (= a 6)
; 				    (begin (println 10) 20)
; 				    (begin (println a) (LAFUN (+ a 1) (+ b 2)))))))
;    (LAFUN 0 0))

; =

; (let ((LAFUN #f))
;    (set! LAFUN (lambda (a b)
;                   (if (= a 6)
;                      (begin (println 10) 20)
;                      (begin (println a) (LAFUN (+ a 1) (+ b 2))))))
;    (LAFUN 0 0))


((lambda (a b)
	((lambda () ($verif a b)))) 10 20)

; (let ((a 10)
; 	  (b 20)
; 	  (c 30))
;    ((lambda () (println a) (println b) (println c))))

;(((lambda (LAFUN) ((lambda (#:g27) (println LAFUN)) (set! LAFUN 10))) #f))

;(aa free . 0)
;(lavar . 0)



; (define (fn2 a b c)
;    (+ a c))

; (println (fn2 1 2 3))
; ;(println (fn2 #t 2 3))

; ;; ----
; ;; FACT
; ;; ----

; (define (fact n)
;    (if (= n 0)
;       1
;       (if (= n 1)
;          1
;          (* n (fact (- n 1))))))

; (println (fact 10))

; ;; ----
; ;; FIBO
; ;; ----

; (define (fibo n)
;    (if (= n 0)
;       0
;       (if (= n 1)
;          1
;          (+ (fibo (- n 1)) (fibo (- n 2))))))

; (println (fibo 30))
