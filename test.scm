
; ;; OPERATORS
; (define + (lambda (x y) ($+ x y)))
; (define - (lambda (x y) ($- x y)))
; (define * (lambda (x y) ($* x y)))
; (define quotient (lambda (x y) ($quotient x y)))
; (define modulo (lambda (x y) ($modulo x y)))
; (define < (lambda (x y) ($< x y)))
; (define > (lambda (x y) ($> x y)))
; (define = (lambda (x y) ($= x y)))

; ;; TYPE TESTS

; (define number? (lambda (n) ($number? n)))

; (define boolean?
;   (lambda (n)
;     (if (= n #t)
;         #t
;         (if (= n #f)
;             #t
;             #f))))

; ;; PRINT NUMBER

; (define print-pos-nz
;   (lambda (n)
;     (if (> n 0)
;         ((lambda (aa nn) ($$putchar (+ (modulo nn 10) 48)))  (print-pos-nz (quotient n 10)) n)
;         #f)))

; (define print-pos
;   (lambda (n)
;     (if (= n 0)
;         ($$putchar 48)
;         (print-pos-nz n))))

; (define print-nb
;   (lambda (n)
;     (if (< n 0)
;         ((lambda (aa nn) (print-pos (* -1 nn))) ($$putchar 45) n)
;         (print-pos n))))

; (define print-bool
;   (lambda (n)
;     (if ($eq? n #t)
;         ((lambda (aa) ($$putchar 116)) ($$putchar 35))
;         ((lambda (aa) ($$putchar 102)) ($$putchar 35)))))

; (define println
;   (lambda (n)
;     (if (number? n)
;       ((lambda (aa) ($$putchar 10)) (print-nb n))
;       ((lambda (aa) ($$putchar 10)) (print-bool n)))))

; (println 101)
; (println 202)
; (println 303)
; (println 0)
; (println -101)
; (println -202)
; (println -303)
; (println #f)
; (println #t)

(let ((a 101) (b 102) (c 103))
  (println a)
  (println b)
  (println c))
; ;; ------
; ;; TEST 1
; ;; ------

; (define fn1
;   (lambda (a b c) (+ b c)))


; (print-ln (fn1 1 2 3))
; (print-ln (fn1 1 2 10))

; ;; ------
; ;; TEST 2
; ;; ------

; ; (define fn2
; ;   (lambda (a b c) (+ a c)))

; ; (print-ln (fn2 1 2 3))
; ; (print-ln (fn2 #t 2 3))

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

; (print-ln (fact 10))

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

; (print-ln (fibo 30))
