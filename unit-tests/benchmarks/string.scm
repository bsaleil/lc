;;; STRING -- One of the Kernighan and Van Wyk benchmarks.

;;; LC NOTE : Can't compute more because of heap/stack overflow

(define s "abcdef")

(define (grow)
  (set! s (string-append "123" s "456" s "789"))
  (set! s (string-append
           (substring s (quotient (string-length s) 2) (string-length s))
           (substring s 0 (+ 1 (quotient (string-length s) 2)))))
  s)

(define (trial n)
  (do ((i 0 (+ i 1)))
      ((> (string-length s) n) (string-length s))
    (grow)))

(define (my-try n)
  (do ((i 0 (+ i 1)))
      ((>= i 10) (string-length s))
    (set! s "abcdef")
    (trial n)))

(println (my-try 0))
(println (my-try 1))
(println (my-try 5))
(println (my-try 10))

;6
;6
;6
;22
