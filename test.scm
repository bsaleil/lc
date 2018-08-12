
(define (foo n)
  (AAcompile-program n n))

(define (AAcompile-program a b)
   (+ a b))

(display (foo 15))
(newline)
