
(define (cons a b)
  ($cons a b))

(define (car l)   ($car l))
(define (cdr l)   ($cdr l))
(define (cddr l)  (cdr (cdr l)))
(define (cadr l)  (car (cdr l)))
(define (caddr l) (car (cddr l)))

(define (set-car! p v)
  ($set-car! p v))

(define (set-cdr! p v)
  ($set-cdr! p v))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define list (lambda x x))

(define append ;; TODO : append-two 
   (lambda (lst1 lst2)
      (if (null? lst1)
         lst2
         (cons (car lst1) (append (cdr lst1) lst2)))))

(define (list? n)
  (cond ((null? n) #t)
        ((pair? n) (list? (cdr n)))
        (else #f)))
