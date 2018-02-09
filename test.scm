(define (foo a)
  (letrec ((loop (lambda (z)
                   (if (null? z)
                       1
                       (loop (cdr z))))))
    (loop a)))

(define (bar x)
    (bar (foo x)))

(bar '(1 2))
