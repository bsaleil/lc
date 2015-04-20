;;; DIVREC -- Benchmark which divides by 2 using lists of n ()'s.

;;; LC NOTE : Can't compute more because of heap/stack overflow

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define *ll* (create-n 200))

(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))
  
(recursive-div2 *ll*)