
(define (write-char char . port)
  (if (null? port)
    (print char)
    ($write-char char (car port))))