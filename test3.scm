
(define *opnd-table-alloc* 0)
(define *opnd-table* (make-vector 10000))
(define (enter-opnd arg1 arg2)
  (let loop ((i 0))
    (if (< i *opnd-table-alloc*)
        (let ((x (vector-ref *opnd-table* i)))
          (if (and (eqv? (car x) arg1) (eqv? (cdr x) arg2)) i (loop (+ i 1))))
        (if (< *opnd-table-alloc* 10000)
            (begin
              (set! *opnd-table-alloc* (+ *opnd-table-alloc* 1))
              (vector-set! *opnd-table* i (cons arg1 arg2))
              i)
            (error "CE")))))

(define (make-glo name)
  (enter-opnd 'ack #t)
  (error "NNN"))

(make-glo 34)
