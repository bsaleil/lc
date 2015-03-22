;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

;;; LC NOTE : Can't compute more because of heap/stack overflow
 
(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))
 
(define *ll* (create-n 200))

(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))
 
(iterative-div2 *ll*)