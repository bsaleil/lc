
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
  (eq? x #f))

(define (<= x y)
  (not (> x y)))

(define (>= x y)
  (not (< x y)))

;; TYPE TESTS

(define (number? n)
  ($number? n))

(define (procedure? n)
  ($procedure? n))

(define (pair? n)
  ($pair? n))

(define (boolean? n)
  (cond ((eq? n #t) #t)
        ((eq? n #f) #t)
        (else #f)))

(define (null? n)
	(eq? n '()))

;; LISTS

(define (cons a b)
  ($cons a b))

(define (car l)
  ($car l))

(define (cdr l)
  ($cdr l))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

;; TODO : type test 
(define (list? n)
  (cond ((null? n) #t)
        ((pair? n) (list? (cdr n)))
        (else #f)))

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
  (cond ((null? n) #f)
        ((number? n) (print-nb n))
        ((procedure? n) (print-procedure n))
        ((pair? n) (begin (print (car n))
                          (print (cdr n))))
        (else (print-bool n))))
  
(define (println n)
  (print n)
  ($$putchar 10))

;; PP
(define pp-h #f)

(define (pp-pair-h n)
  (cond ;; (e1)
        ((and (list? n) (= (length n) 1)) (pp-h (car n)))
        ;; (e1 . e2)
        ((not (pair? (cdr n)))
         (let  ((aaa (pp-h (car n)))
                (bbb ($$putchar 32))
                (ccc ($$putchar 46))
                (ddd ($$putchar 32)))
           (pp-h (cdr n))))
        ;; (e1 ...)
        (else (let ((aaa (pp-h (car n)))
                    (bbb ($$putchar 32)))
                (pp-pair-h (cdr n))))))

(define (pp-pair n)
  ($$putchar 40)
  (pp-pair-h n)
  ($$putchar 41))

(define (pp-h n)
  (cond ((null? n) (begin ($$putchar 40) ($$putchar 41))) ;; ()
        ((number? n) (print-nb n))
        ((procedure? n) (print-procedure n))
        ((pair? n) (pp-pair n))
        (else (print-bool n))))

(define (pp n)
  (pp-h n)
  ($$putchar 10))