
(define (foo a b c . d)
  (+ a b c (car d)))

(define (bar) '(1 2 3 4 5 6 7 8 9 10))

(apply foo (bar))

;832040
;832040
;832040
;832040
;832040


;
; (define (foo n)
;   (+ n 1))
;
; (foo 100)
; (define ll '(1))
;
; (define (bar)
;   '(1))
;
; (##apply foo (bar))
