(define (adder y)
  (lambda (x) (+ x y)))

(define inc (adder 1))
(define dec (adder -1))

(println (inc 100))
(println (dec 100))

;101
;99
