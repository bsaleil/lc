
(define (zero? x)
  (= x 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (even? x)
  (= (modulo x 2) 0))

(define (odd? x)
  (= (modulo x 2) 1))

(define (expt n m)
  (if (= m 0)
     1
     (* n (expt n (- m 1)))))

(define (max a . l)
  (define (max-h els m)
    (if (null? els)
       m
       (if (> (car els) m)
          (max-h (cdr els) (car els))
          (max-h (cdr els) m))))
  (max-h l a))

(define (min a . l)
  (define (min-h els m)
    (if (null? els)
       m
       (if (< (car els) m)
          (min-h (cdr els) (car els))
          (min-h (cdr els) m))))
  (min-h l a))

