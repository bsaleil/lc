(lambda (a b c) 10)

($$putchar 50)
($$putchar 50)

; (println 101)
; (println 202)
; (println 303)
; (println 0)
; (println -101)
; (println -202)
; (println -303)
; (println #f)
; (println #t)

; ;; ------
; ;; TEST 1
; ;; ------

; (define fn1
;   (lambda (a b c) (+ b c)))


; (println (fn1 1 2 3))
; (println (fn1 1 2 10))

; ; ;; ------
; ; ;; TEST 2
; ; ;; ------

; ; (define fn2
; ;   (lambda (a b c) (+ a c)))

; ; (println (fn2 1 2 3))
; ; (println (fn2 #t 2 3))

; ;; ----
; ;; FACT
; ;; ----

; (define fact
;   (lambda (n)
;     (if (= n 0)
;       1
;       (if (= n 1)
;         1
;         (* n (fact (- n 1)))))))

; (println (fact 10))

; ;; ----
; ;; FIBO
; ;; ----

; (define fibo
;   (lambda (n)
;     (if (= n 0)
;       0
;       (if (= n 1)
;         1
;         (+ (fibo (- n 1)) (fibo (- n 2)))))))

; (println (fibo 40))
