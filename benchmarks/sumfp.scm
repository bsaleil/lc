;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

(define (run n)
  (let loop ((i n) (sum 0.))
    (if (FLOAT< i 0.)
      sum
      (loop (FLOAT- i 1.) (FLOAT+ i sum)))))

(apply run (list 10000.))
