
(define (call/cc . n)
  (let ((l (length n)))
    (cond ((= l 1)
             ((car n) #f))
          ((= l 2)
             ((car n) #f (cadr n)))
          (else (error "call/cc")))))

(call/cc (lambda (c) (println 10)))


(call/cc (lambda (a b) (b)) (lambda () (println 100)))

;(define call/cc (lambda (r) (r #f)))
;
;(define (foo proc)
;
;    (call-with-current-continuation
;                (lambda (cont)
;                  (proc))))
;
;(foo (lambda () 10))
