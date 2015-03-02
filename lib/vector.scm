
(define (vector . l) (list->vector l))

(define (vector-fill! v val)
  (vector-fill!-h v 0 val (vector-length v)))

(define (vector-fill!-h v pos val len)
  (if (= pos len)
    #f
    (begin (vector-set! v pos val)
           (vector-fill!-h v (+ pos 1) val len))))