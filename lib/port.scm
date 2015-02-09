
(define (open-input-file path)
  ($open-input-file path))

(define (open-output-file path)
  ($open-output-file path))

(define (close-input-port port)
  ($close-input-port port))

(define (close-output-port port)
  ($close-output-port port))

(define (eof-object? o)
  ($eof-object? o))

(define (read-char port)
  ($read-char port))

(define (write-char char port)
  ($write-char char port))

(define (input-port? port)
  ($input-port? port))

(define (output-port? port)
  ($output-port? port))

(define (port? port)
  (or (input-port?  port)
      (output-port? port)))