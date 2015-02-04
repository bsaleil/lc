(define (map fn lst)
  (if (null? lst)
    '()
    (cons (fn (car lst)) (map fn (cdr lst)))))