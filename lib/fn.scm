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
  (exit))

(define fatal-error
  (lambda (msg . msgs)
    (apply error (cons msg msgs))))

(define (apply fn args . r)
  ;; If more than 2 args, build arg list from rest param
  ;; (last arg must be a list)
  (define (gen-args l)
    (if (= (length l) 1)
        (if (list? (car l))
            (car l)
            (error "APPLY ERROR"))
        (cons (car l) (gen-args (cdr l)))))

  (if (null? r)
    ($apply fn args)
    ($apply fn (gen-args (cons args r)))))
