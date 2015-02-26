
(define (vector . l) (list->vector l))

(define (vector-set! v i val)
  ($vector-set! v i val))

(define (vector-fill! v val)
  (vector-fill!-h v 0 val (vector-length v)))

(define (vector-fill!-h v pos val len)
  (if (= pos len)
    #f
    (begin (vector-set! v pos val)
           (vector-fill!-h v (+ pos 1) val len))))

(define (make-vector size . init)
  (let ((v ($make-vector size)))
    (if (not (null? init))
        (vector-fill! v (car init)))
    v))