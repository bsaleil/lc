
;; OPERATORS

(define (+ x y)        ($+ x y))
(define (- x y)        ($- x y))
(define (* x y)        ($* x y))
(define (quotient x y) ($quotient x y))
(define (modulo x y)   ($modulo x y))
(define (< x y)        ($< x y))
(define (> x y)        ($> x y))
(define (= x y)        ($= x y))

;; TYPE TESTS

(define (number? n)
  ($number? n))

(define (boolean? n)
    (if (= n #t)
        #t
        (if (= n #f)
            #t
            #f)))

;; PRINT NUMBER

;; TODO
(define (print-pos-nz n)
    (if (> n 0)
        (begin (print-pos-nz (quotient n 10))
               ($$putchar (+ (modulo n 10) 48)))
        #f))

(define (print-pos n)
    (if (= n 0)
        ($$putchar 48)
        (print-pos-nz n)))

;; TODO
(define (print-nb n)
    (if (< n 0)
        (begin ($$putchar 45)
               (print-pos (* -1 n)))
        (print-pos n)))

(define (print-bool n)
  (if ($eq? n #t)
      (begin ($$putchar  35)
             ($$putchar 116))
      (begin ($$putchar  35)
             ($$putchar 102))))

(define (println n)
  (if (number? n)
      (begin (print-nb n)
             ($$putchar 10))
      (begin (print-bool n)
             ($$putchar 10))))

(println 101)
(println 202)
(println 303)
(println 0)
(println -101)
(println -202)
(println -303)
(println #f)
(println #t)

;; ------
;; TEST 1
;; ------

(define fn1
  (lambda (a b c) (+ b c)))


(println (fn1 1 2 3))
(println (fn1 1 2 10))

; ;; ------
; ;; TEST 2
; ;; ------

; (define fn2
;   (lambda (a b c) (+ a c)))

; (println (fn2 1 2 3))
; (println (fn2 #t 2 3))

;; ----
;; FACT
;; ----

(define fact
  (lambda (n)
    (if (= n 0)
      1
      (if (= n 1)
        1
        (* n (fact (- n 1)))))))

(println (fact 10))

;; ----
;; FIBO
;; ----

(define fibo
  (lambda (n)
    (if (= n 0)
      0
      (if (= n 1)
        1
        (+ (fibo (- n 1)) (fibo (- n 2)))))))

(println (fibo 40))
