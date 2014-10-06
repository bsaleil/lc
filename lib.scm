
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

(define (vector? n)
  ($vector? n))

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

;; VECTORS

(define (make-vector l)
   ($make-vector l))

(define (vector-length v)
   ($vector-length v))

(define (vector-ref v i)
  ($vector-ref v i))

(define (vector-set! v i val)
  ($vector-set! v i val))

(define (vector-fill!-h v pos val len)
  (if (= pos len)
    #f
    (begin (vector-set! v pos val)
           (vector-fill!-h v (+ pos 1) val len))))

(define (vector-fill! v val)
  (vector-fill!-h v 0 val (vector-length v)))

;; TYPES CONVERSION

(define (vector->list-h vector idx length)
   (if (= idx length)
      '()
      (cons (vector-ref vector idx) (vector->list-h vector (+ idx 1) length))))

(define (vector->list v)
   (vector->list-h v 0 (vector-length v)))

(define (list->vector-h lst vec pos len)
  (if (null? lst)
    vec
    (begin (vector-set! vec pos (car lst))
           (list->vector-h (cdr lst) vec (+ pos 1) len))))

(define (list->vector l)
  (let ((v (make-vector (length l))))
     (list->vector-h l v 0 (length l))))

;; PRINT
(define print #f)

(define (print-pos-nz n)
    (if (> n 0)
        (begin (print-pos-nz (quotient n 10))
               ($$putchar (+ (modulo n 10) 48)))))

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

(define (print-vector vector idx length)
  (if (< idx length)
    (begin (print (vector-ref vector idx))
           (print-vector vector (+ idx 1) length))))

(set! print
  (lambda (n)
    (cond ((null? n) #f)
          ((number? n) (print-nb n))
          ((procedure? n) (print-procedure n))
          ((pair? n) (begin (print (car n))
                            (print (cdr n))))
          ((vector? n) (print-vector n 0 (vector-length n)))
          (else (print-bool n)))))
  
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
            (begin (pp-h (car n))
                   ($$putchar 32)
                   ($$putchar 46)
                   ($$putchar 32)
                   (pp-h (cdr n))))
        ;; (e1 ...)
        (else (begin (pp-h (car n))
                     ($$putchar 32)
                     (pp-pair-h (cdr n))))))

(define (pp-pair n)
  ($$putchar 40)
  (pp-pair-h n)
  ($$putchar 41))

(define (pp-vector-h vector idx length)
  (cond ((= idx (- length 1))
            (pp-h (vector-ref vector idx)))
        ((< idx length)
            (begin (pp-h (vector-ref vector idx))
                   ($$putchar 32)
                   (pp-vector-h vector (+ idx 1) length)))))

(define (pp-vector vector)
  ($$putchar 35)
  ($$putchar 40)
  (pp-vector-h vector 0 (vector-length vector))
  ($$putchar 41))

(set! pp-h (lambda (n)
  (cond ((null? n) (begin ($$putchar 40) ($$putchar 41))) ;; ()
        ((number? n) (print-nb n))
        ((procedure? n) (print-procedure n))
        ((pair? n) (pp-pair n))
        ((vector? n) (pp-vector n))
        (else (print-bool n)))))

(define (pp n)
  (pp-h n)
  ($$putchar 10))