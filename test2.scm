

;(letrec ((f (lambda (n)
;              (if (= 0 n)
;                (pp "A")
;                (g (- n 1)))))
;         (g (lambda (n)
;              (if (= 0 n)
;                (pp "B")
;                (f (- n 1))))))
;   (f 11))

(let ((a 100)
      (b 33))

  (letrec ((baz (lambda () (+ a b)))
           ;(r (test))
           (foo (lambda () (+ (baz) a)))
           (bar (lambda () (+ (foo) b))))
    (set! baz (lambda () 10))
    (pp (bar))
    (pp (foo))
    (pp (baz))))

;(letrec ((lst
;          (##box #f))
;         (bar
;          (lambda (lst)
;            (##set-box! lst 1))))
; (##set-box! lst '())
; (bar lst))


;
;(define (test)
;
;  (define lst '())
;
;  (define (foo)
;    (set! lst 1))
;
;  (define (bar)
;    (foo)
;    (error "K")
;    (bar))
;
;  (bar))
;
;(test)
