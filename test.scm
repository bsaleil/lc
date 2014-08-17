;(+ 14 11)
10

;; ------
;; TEST 1
;; ------

; (define fn1
;   (lambda (a b c) (+ b c)))

; ($$msg "EXEC 1")
; (fn1 1 2 3)
; ($$msg "EXEC 2")
; (fn1 1 2 10)

;; ------
;; TEST 2
;; ------

; (define fn2
;   (lambda (a b c) (+ b c)))

; ($$msg "EXEC 1")
; (fn2 1 2 3)
; ($$msg "EXEC 2")
; (fn2 #t 2 3)

;; ----
;; FACT
;; ----

; (define fact
;   (lambda (n)
;     (if (= n 0)
;       1
;       (if (= n 1)
;         1
;         (* n (fact (- n 1)))))))

; (fact 10)

;; ----
;; FIBO
;; ----

; (define fibo
;   (lambda (n)
;     (if (= n 0)
;       0
;       (if (= n 1)
;         1
;         (+ (fibo (- n 1)) (fibo (- n 2)))))))

; (fibo 40)