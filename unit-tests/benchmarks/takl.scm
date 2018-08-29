;;; TAKL -- The TAKeuchi function using lists as counters.

(define (listn n)
  (if (= n 0)
    '()
    (cons n (listn (- n 1)))))

(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))

(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

;;; RUN TEST

(define l9 (listn 9))
(define l3 (listn 3))
(define l21 (listn 21))
(define l15 (listn 15))

(pp (mas l9 l6 l3))
(pp (mas l18 l12 l6))
(pp (mas l21 l15 l9))

;(6 5 4 3 2 1)
;(7 6 5 4 3 2 1)
;(10 9 8 7 6 5 4 3 2 1)
