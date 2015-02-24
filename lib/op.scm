
(define (modulo x y)   ($modulo x y))

;; TODO : IT IS NOT THE SAME FUNCTION !
(define remainder modulo)

(define (not x)
  (eq? x #f))

(define (eq? x y)
  (if (symbol? x)
      (and (symbol? y)
           (string=? (symbol->string x) (symbol->string y)))
      ($eq? x y)))

(define (eqv? x y)
  (if (number? x)
      (and (number? y)
           (= x y))
      (eq? x y)))

(define (equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (equal? (car x) (car y))
                        (equal? (cdr x) (cdr y))))
        ((string? x) (and (string? y)
                          (string=? x y)))
        (else (eqv? x y))))