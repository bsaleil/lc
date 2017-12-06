
(define (run2)
    (let ((result (fibfp 30.)))
        (gambit$$pp result)
        (equal? result 832040.)))

(define *ll* '(() () () () () () () ()))

(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))

;;; FIBFP -- Computes fib(35) using floating point

(define (fibfp n)
  (if (< n 2.)
    n
    (+ (fibfp (- n 1.))
       (fibfp (- n 2.)))))


(equal? (recursive-div2 *ll*) '(() () () ()))

(run2)
