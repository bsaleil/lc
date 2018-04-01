

(define (go n)
  (let loop ((repeat 100))
    (if (> repeat 0)
        (loop (- repeat 1) (make-vector n))
        10)))

(go 80000)
