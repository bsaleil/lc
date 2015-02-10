(define (loop i)
  (if (< i 0)
      i
      (loop (- i 3))))

(println (loop 1000000))

;-2
