;;; SUM -- Compute sum of integers from 0 to n

(define (run n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
      sum
      (loop (- i 1) (+ i sum)))))

(pp (run 0))
(pp (run 1))
(pp (run 10))
(pp (run 100))
(pp (run 1000))
(pp (run 10000))
(pp (run 100000))

;0
;1
;55
;5050
;500500
;50005000
;5000050000
