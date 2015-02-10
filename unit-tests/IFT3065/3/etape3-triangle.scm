(define (list2vector lst)
  (list2vector-aux lst 0))

(define (list2vector-aux lst n)
  (if (pair? lst)
      (let ((v (list2vector-aux (cdr lst) (+ n 1))))
        (vector-set! v n (car lst))
        v)
      (make-vector n)))

(define *board*
  (list2vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (list2vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (list2vector '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                   13 7 8 4 4 7 11 8 12 13 6 10
                   15 9 14 13 13 14 15 9 10
                   6 6)))

(define *b*
  (list2vector '(2 4 7 5 8 9 3 6 10 5 9 8
                   12 13 14 8 9 5 2 4 7 5 8
                   9 3 6 10 5 9 8 12 13 14
                   8 9 5 5)))

(define *c*
  (list2vector '(4 7 11 8 12 13 6 10 15 9 14 13
                   13 14 15 9 10 6 1 2 4 3 5 6 1
                   3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())
 
(define (attempt i depth)
  (cond ((= depth 14)
         ;;(set! *answer* (cons (cdr (vector->list *sequence*)) *answer*))
         (set! *answer* (cons 999 *answer*))
         #t)
        ((and (= 1 (vector-ref *board* (vector-ref *a* i)))
              (= 1 (vector-ref *board* (vector-ref *b* i)))
              (= 0 (vector-ref *board* (vector-ref *c* i))))
         (vector-set! *board* (vector-ref *a* i) 0)
         (vector-set! *board* (vector-ref *b* i) 0)
         (vector-set! *board* (vector-ref *c* i) 1)
         (vector-set! *sequence* depth i)
         (search 0 (+ depth 1))
         (vector-set! *board* (vector-ref *a* i) 1)
         (vector-set! *board* (vector-ref *b* i) 1)
         (vector-set! *board* (vector-ref *c* i) 0) #f)
        (else
         #f)))

(define (search j depth)
  (if (or (= j 36) (attempt j depth))
      #f
      (search (+ j 1) depth)))

(define (test i depth)
  (set! *answer* '())
  (attempt i depth)
  *answer*)
 
(println (length (test 22 1)))

;775
