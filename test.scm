;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err

(define (foo n)
  (println (+ n 10)))

(define (bar n)
  (foo (+ 100 n)))

(bar 10)

; (foo 10)
; (bar 10)
