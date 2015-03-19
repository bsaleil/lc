;;; ACK -- One of the Kernighan and Van Wyk benchmarks.

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(println (ack 0 0))
(println (ack 1 2))
(println (ack 3 4))
(println (ack 4 0))
(println (ack 3 9))

;1
;4
;125
;13
;4093
