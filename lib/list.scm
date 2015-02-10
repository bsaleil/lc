
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

(define (append . lsts)
  (define (append-two lst1 lst2)
    (if (null? lst1)
      lst2
      (cons (car lst1) (append-two (cdr lst1) lst2))))
  (define (append-h lsts)
    (if (null? lsts)
      '()
      (append-two (car lsts) (append-h (cdr lsts)))))
  (append-h lsts))

(define (list? n)
  (cond ((null? n) #t)
        ((pair? n) (list? (cdr n)))
        (else #f)))

(define (list-ref lst i)
  (if (= i 0)
    (car lst)
    (list-ref (cdr lst) (- i 1))))

(define (reverse lst)
   (if (null? lst)
       '()
       (append (reverse (cdr lst))
               (list (car lst)))))

(define (for-each f lst)
   (if (pair? lst)
     (begin (f (car lst))
            (for-each f (cdr lst)))))

(define (assq el lst)
  (cond ((null? lst) #f)
        ((eq? el (car (car lst))) (car lst))
        (else (assq el (cdr lst)))))

(define (assoc el lst)
  (cond ((null? lst) #f)
        ((equal? el (car (car lst))) (car lst))
        (else (assoc el (cdr lst)))))

(define (memq el lst)
  (cond ((null? lst) #f)
        ((eq? el (car lst)) lst)
        (else (memq el (cdr lst)))))

(define (member el lst)
  (cond ((null? lst) #f)
        ((equal? el (car lst)) lst)
        (else (member el (cdr lst)))))