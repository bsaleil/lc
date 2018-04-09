; (define (fib n)
;   (if (< n 2.0)
;       1.0
;       (+ (fib (- n 1.0))
;          (fib (- n 2.0)))))
;
;
;
; (define pair (cons 10 40.0))
;
; (define (bar p)
;   (cdr p))
;
; (fib (cdr pair))

(define ncall 0)
(define nplus 0)
(define nminus 0)
(define ncomp 0)

(define (plus a b)
  (set! nplus (+ nplus 1))
  (+ a b))

(define (minus a b)
  (set! nminus (+ nminus 1))
  (- a b))

(define (comp a b)
  (set! ncomp (+ ncomp 1))
  (< a b))

(define (fib n)
  (set! ncall (+ ncall 1))
  (if (< n 2.0)
      1.0
      (+ (fib (- n 1))
            (fib (- n 2)))))


(fib 35.0)
(println "call : " ncall)
(println "plus : " nplus)
(println "minus: " nminus)
(println "comp : " ncomp)
