(lambda () (+ 1 2))

;; n : ascii code
; (define putchar
; 	(lambda (n)
; 		($$putchar n)))

; (putchar 48)
; (putchar 49)
; (putchar 50)
; (putchar 51)
; (putchar 52)
; (putchar 53)
; (putchar 54)
; (putchar 55)
; (putchar 56)
; (putchar 57) 
; (putchar 10) ;; EOL

;(modulo 12 5)

; (define print
;   (lambda (n)
;     (print-number n)))

; (define println
;   (lambda (n)
;     (let ((a (print n)))
;       ($$putchar 10))))

; (define print-number
;   (lambda (n)
;     (if (= n 0)
;         ($$putchar 48)
;         (print-number-nz n))))

; (define print-number-nz
;   (lambda (n)
;     (if (> n 0)
;         (let ((a (print-number-nz (quotient n 10))))
;           ($$putchar (+ (modulo n 10) 48))))))
        
; (println 10)
        
; (define print-number
;   (lambda (n)
;     (if (= n 0)
;         ($$putchar 48)
;         (print-number-nz n))))

;(begin 10 20)

;(< #t 99)
;($$msg "TEST MSG SF")
;(define f (lambda (a) (+ a 11)))
;(f 33)

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
