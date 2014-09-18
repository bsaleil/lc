
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

(define (not x)
  (if (eq? x #f)
      #t
      #f))

(define (<= x y)
  (if (< x y)
      #t
      (= x y)))

(define (>= x y)
  (if (> x y)
      #t
      (= x y)))

;; TYPE TESTS

(define (number? n)
  ($number? n))

(define (procedure? n)
  ($procedure? n))

(define (pair? n)
  ($pair? n))

(define (boolean? n)
    (if (eq? n #t)
        #t
        (if (eq? n #f)
            #t
            #f)))

(define (null? n)
	(eq? n '()))

;; LISTS

(define (cons a b)
  ($cons a b))

(define (car l)
  ($car l))

(define (cdr l)
  ($cdr l))


;; TODO : type test 
(define (list? n)
  (if (null? n)
      #t
      (if (pair? n)
      	(list? (cdr n))
        #f)))

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

(define (print-procedure n)
  ($$putchar  35)
  ($$putchar  60)
  ($$putchar 112)
  ($$putchar 114)
  ($$putchar 111)
  ($$putchar  99)
  ($$putchar 101)
  ($$putchar 100)
  ($$putchar 117)
  ($$putchar 114)
  ($$putchar 101)
  ($$putchar  62))

(define (print n)
  (if (null? n)
      #f
      (if (number? n)
          (print-nb n)
          (if (procedure? n)
              (print-procedure n)
              (if (pair? n)
                  (begin (print (car n))
                         (print (cdr n)))
	              (begin (print-bool n)))))))

(define (println n)
  (print n)
  ($$putchar 10))