;;; SUM -- Compute sum of integers from 0 to n

(define (run n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
      sum
      (loop (- i 1) (+ i sum)))))

(apply run '(10000))
