
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




(define (lots-append l)
  (if (null? l)
      '()
      (append (lots-append (cdr l))
              (list (car l)))))

(let loop ((i 250000))
  (if (not (= i 0))
      (begin (lots-append '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25))
             (loop (- i 1)))))
;(define (run-bench count)
;  (let loop ((i count) (result '(undefined)))
;    (if (##fx< 0 i)
;      (loop (##fx- i 1) (nqueens 8))
;      result)))
;
;(define (ok? row dist placed)
;  (if (null? placed)
;    #t
;    (and (not (##fx= (car placed) (##fx+ row dist)))
;         (not (##fx= (car placed) (##fx- row dist)))
;         (ok? row (##fx+ dist 1) (cdr placed)))))
;
;(define (my-try x y z)
;  (if (null? x)
;    (if (null? y)
;      0
;      0)
;    (##fx+ (if (ok? (car x) 1 z)
;               (my-try (append (cdr x) y) '() (cons (car x) z))
;               0)
;           (my-try (cdr x) (cons (car x) y) z))))
;
;(define (nqueens n)
;  (my-try '(0 1 2 3 4 5) '() '()))

;(run-bench 10000)
