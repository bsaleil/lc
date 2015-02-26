
(define (write-char char . port)
  (if (null? port)
    (print char)
    ($write-char char (car port))))

(define (input-port? port)
  ($input-port? port))

(define (output-port? port)
  ($output-port? port))

(define (port? port)
  (or (input-port?  port)
      (output-port? port)))