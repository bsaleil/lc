

(define (bar1 n)
  (if (= n 0)
    #t
    (bar1 (- n 1))))

(define (bar2 n)
  (if (= n 0)
    #t
    (begin 10
           (bar2 (- n 1)))))

(define (foo1 n)
  (if (= n 0)
    #t
    (let ((a (- n 1)))
      100
      (foo1 a))))

(define (foo2 n)
  (if (= n 0)
    #t
    (let* ((a (- n 1)))
      100
      (foo2 a))))

(define (fun n)
  (if (= n 0)
     #t
     ((lambda () 100 (fun (- n 1))))))

(pp (bar1 999999))
(pp (bar2 999999))
(pp (foo1 999999))
(pp (foo2 999999))
(pp (fun  99999))

;#t
;#t
;#t
;#t
;#t
