;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err
;; Test bind define






(define (foo a . b)
	($+ a 10))


(foo 1 2)







; (define (string-append-h strings)
;   (cond ((null? strings) "")
;         ((null? (cdr strings)) (car strings))
;         (else
;          (string-append-h
;           (cons (string-append-two (car strings) (cadr strings))
;                 (cddr strings))))))


; (foo 10)
; (bar 10)
