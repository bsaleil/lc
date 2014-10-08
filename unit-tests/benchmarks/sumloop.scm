;;; SUMLOOP -- One of the Kernighan and Van Wyk benchmarks.

;;; LC NOTE : Can't compute more because of heap/stack overflow

(define sum 0)

(define (tail-rec-aux i n)
  (if (< i n)
      (begin (set! sum (+ sum 1)) (tail-rec-aux (+ i 1) n))
      sum))

(define (tail-rec-loop n)
  (set! sum 0)
  (tail-rec-aux 0 n)
  sum)

(define (do-loop n)
  (set! sum 0)
  (do ((i 0 (+ i 1)))
      ((>= i n) sum)
    (set! sum (+ sum 1))))

(pp (do-loop 0))
(pp (do-loop 1))
(pp (do-loop 10))
(pp (do-loop 50))
(pp (do-loop 100))

;0
;1
;10
;50
;100
