(define print-pos-nz
  (lambda (n)
    (if (> n 0)
        ((lambda (aa nn) ($$putchar (+ (modulo nn 10) 48)))  (print-pos-nz (quotient n 10)) n)
        #f)))

(define print-pos
  (lambda (n)
    (if (= n 0)
        ($$putchar 48)
        (print-pos-nz n))))

(define print-nb
  (lambda (n)
    (if (< n 0)
        ((lambda (aa nn) (print-pos (* -1 nn))) ($$putchar 45) n)
        (print-pos n))))

(define print-ln
  (lambda (n)
    ((lambda (aa) ($$putchar 10)) (print-nb n))))

(print-ln 101)
(print-ln 202)
(print-ln 303)
(print-ln 0)
(print-ln -101)
(print-ln -202)
(print-ln -303)


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
