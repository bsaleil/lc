(define (map fn lst)
  (if (null? lst)
    '()
    (cons (fn (car lst)) (map fn (cdr lst)))))

(define (error msg . msgs)
  
  (define (print-msgs msgs)
    (if (null? msgs)
        (newline)
        (begin (print (car msgs))
               (print " ")
               (print-msgs (cdr msgs)))))
  
  (print "!!! ERROR - ")
  (print-msgs (cons msg msgs))
  ($error))

(define fatal-error error)

(define (apply fn args)
  ($apply fn args))