;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err
;; Test bind define

; (define (bar n m)
;   (begin ($$putchar ($+ n m))
;          (foo 55)))

; (define (foo n)
;   (begin ($$putchar n)
;          (bar 55 2)))

; (foo 55)

(define (foo n)
  10)

(define (make-string size . init)
    (foo 20))


(make-string 4)




; (define (string-append-h strings)
;   (cond ((null? strings) "")
;         ((null? (cdr strings)) (car strings))
;         (else
;          (string-append-h
;           (cons (string-append-two (car strings) (cadr strings))
;                 (cddr strings))))))


; (foo 10)
; (bar 10)
