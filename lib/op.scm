(define (equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (equal? (car x) (car y))
                        (equal? (cdr x) (cdr y))))
        ((string? x) (and (string? y)
                          (string=? x y)))
        (else (eqv? x y))))