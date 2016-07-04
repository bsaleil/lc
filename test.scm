


(define (fib n)
  (if (< n 2)
      0
      (+ (fib (- n 1))
         (fib (- n 2)))))

(fib 40)

;(define (fib n)
;  (if (< n 2)
;    n
;    (+ (fib (- n 1))
;       (fib (- n 2)))))
;
;($apply fib '(35))
;;($apply fib '(35))
;;($apply fib '(35))
;;($apply fib '(35))
;;($apply fib '(35))
