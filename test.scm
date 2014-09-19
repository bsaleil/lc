
(println (modulo 13 4))
(println (modulo -13 4))
(println (modulo 13 -4))
(println (modulo -13 -4))

; (println 101)
; (println 202)
; (println 303)
; (println 0)
; (println -101)
; (println -202)
; (println -303)
; (println #f)
; (println #t)
; (println '())
; (println (lambda (x) x))
; (println (boolean? 10))
; (println (number?  10))
; (println (number?  #f))
; (println (boolean? #f))
; (println (null? 10))
; (println (null? '()))
; (println (procedure? 10))
; (println (procedure? (lambda (x) x)))
; (println (pair? 10))
; (println (pair? (cons 1 2)))
; (println (pair? (lambda (x) x)))
; (println (pair? (cons (lambda (x) x) (lambda (y) y))))

; ;; ------
; ;; TEST 1
; ;; ------

; (define (fn1 a b c)
;    (+ b c))


; (println (fn1 1 2 3))
; (println (fn1 1 2 10))

; ;; ------
; ;; TEST 2
; ;; ------

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
