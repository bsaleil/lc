
;; OPERATORS

(define (+ x y)        ($+ x y))
(define (- x y)        ($- x y))
(define (* x y)        ($* x y))
(define (quotient x y) ($quotient x y))
(define (modulo x y)   ($modulo x y))
(define (< x y)        ($< x y))
(define (> x y)        ($> x y))
(define (= x y)        ($= x y))
(define (eq? x y)      ($eq? x y))

;; TYPE TESTS

(define (number? n)
  ($number? n))

(define (boolean? n)
    (if (eq? n #t)
        #t
        (if (eq? n #f)
            #t
            #f)))

(define (null? n)
	(eq? n '()))

;; PRINT

(define (print-pos-nz n)
    (if (> n 0)
        (begin (print-pos-nz (quotient n 10))
               ($$putchar (+ (modulo n 10) 48)))
        #f))

(define (print-pos n)
    (if (= n 0)
        ($$putchar 48)
        (print-pos-nz n)))

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
  (if (null? n)
      ($$putchar 10)
	  (if (number? n)
	      (begin (print-nb n)
	             ($$putchar 10))
	      (begin (print-bool n)
	             ($$putchar 10)))))