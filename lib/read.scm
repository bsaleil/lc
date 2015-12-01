(define (read)

  ;; Port to read
  (define port (current-input-port))

  ;; Syntax error
  (define (syntax-error msg)
    (error msg))

  (define (objread-char o)
    (car o))
  (define (objread-line o)
    (cadr o))
  (define (objread-col o)
    (caddr o))
  (define (objread-next o)
    (if (char=? (objread-char o) #\newline)
        (list (read-char port) (+ (objread-line o) 1) 1)
        (list (read-char port) (objread-line o) (+ (objread-col o) 1))))
  (define (objret-new o val)
    (list val (objread-line o) (objread-col o)))
  (define (objret-val o)
    (car o))

  (define (read-num o num)
    (if (and (char>=? (objread-char o) #\0)
             (char<=? (objread-char o) #\9))
        (if (not num)
            (read-num (objread-next o) (objret-new o (- (char->integer (objread-char o)) 48)))
            (read-num (objread-next o) (objret-new num (+ (* (objret-val num) 10) (char->integer (objread-char o)) -48))))
        num))

  (let ((r (read-num (list (read-char port) 1 1) #f)))
    (objret-val r)))
