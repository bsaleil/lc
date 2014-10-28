
(define a 100)

(define (foo) (println (+ a 10)))

(define (bar) (set! a #f))

(foo)
(bar)
(foo)