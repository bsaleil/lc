
; ; ;; OPERATORS

; ; (define (+ x y)        ($+ x y))
; ; (define (- x y)        ($- x y))
; ; (define (* x y)        ($* x y))
; ; (define (quotient x y) ($quotient x y))
; ; (define (modulo x y)   ($modulo x y))
; ; (define (< x y)        ($< x y))
; ; (define (> x y)        ($> x y))
; ; (define (= x y)        ($= x y))

; ; ;; TYPE TESTS

; (define + (lambda (x y) ($+ x y)))
;  (define - (lambda (x y) ($- x y)))
;  (define * (lambda (x y) ($* x y)))
;  (define quotient (lambda (x y) ($quotient x y)))
;  (define modulo (lambda (x y) ($modulo x y)))
;  (define < (lambda (x y) ($< x y)))
;  (define > (lambda (x y) ($> x y)))
;  (define = (lambda (x y) ($= x y)))
;  (define number? (lambda (n) ($number? n)))
;  (define boolean? (lambda (n) (if (= n #t) #t (if (= n #f) #t #f))))
;  (define print-pos-nz
;    (lambda (n) (if (> n 0) ((lambda (#:g0) ($$putchar (+ (modulo n 10) 48))) (print-pos-nz (quotient n 10))) #f)))
;  (define print-pos (lambda (n) (if (= n 0) ($$putchar 48) (print-pos-nz n))))
;  (define print-nb (lambda (n) (if (< n 0) ((lambda (#:g1) (print-pos (* -1 n))) ($$putchar 45)) (print-pos n))))
;  (define print-bool
;    (lambda (n)
;      (if ($eq? n #t) ((lambda (#:g2) ($$putchar 116)) ($$putchar 35)) ((lambda (#:g3) ($$putchar 102)) ($$putchar 35)))))
;  (define println
;    (lambda (n)
;      (if (number? n) ((lambda (#:g4) ($$putchar 10)) (print-nb n)) ((lambda (#:g5) ($$putchar 10)) (print-bool n)))))

; ; (define (number? n)
; ;   ($number? n))

; ; (define (boolean? n)
; ;     (if (= n #t)
; ;         #t
; ;         (if (= n #f)
; ;             #t
; ;             #f)))

; ; ;; PRINT NUMBER

; ; (define (print-pos-nz n)
; ;     (if (> n 0)
; ;         (begin (print-pos-nz (quotient n 10))
; ;                ($$putchar (+ (modulo n 10) 48)))
; ;         #f))

; ; (define (print-pos n)
; ;     (if (= n 0)
; ;         ($$putchar 48)
; ;         (print-pos-nz n)))

; ; (define (print-nb n)
; ;     (if (< n 0)
; ;         (begin ($$putchar 45)
; ;                (print-pos (* -1 n)))
; ;         (print-pos n)))

; ; (define (print-bool n)
; ;   (if ($eq? n #t)
; ;       (begin ($$putchar  35)
; ;              ($$putchar 116))
; ;       (begin ($$putchar  35)
; ;              ($$putchar 102))))

; ; (define (println n)
; ;   (if (number? n)
; ;       (begin (print-nb n)
; ;              ($$putchar 10))
; ;       (begin (print-bool n)
; ;              ($$putchar 10))))