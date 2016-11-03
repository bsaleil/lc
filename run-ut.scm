#! /usr/bin/env gsi

;;;============================================================================

;;; File: "run-unit-tests.scm"

;;; Copyright (c) 2012-2014 by Marc Feeley, All Rights Reserved.

;;;----------------------------------------------------------------------------

;;; This is a modified version of the run-unit-tests.scm
;;; file from Gambit Scheme (http://gambitscheme.org/)

(define cleanup? #f)

(define nb-good 0)
(define nb-fail 0)
(define nb-other 0)
(define nb-total 0)
(define start 0)

(define (num->string num w d) ; w = total width, d = decimals
  (let ((n (floor (inexact->exact (round (* (abs num) (expt 10 d)))))))
    (let ((i (quotient n (expt 10 d)))
          (f (modulo n (expt 10 d))))
      (let ((si (string-append
                  (if (< num 0) "-" "")
                  (if (and (= i 0) (> d 0)) "" (number->string i 10))))
            (sf (number->string (+ f (expt 10 d)) 10)))
        (if (> d 0)
          (string-set! sf 0 #\.)
          (set! sf ""))
        (let ((lsi (string-length si))
              (lsf (string-length sf)))
          (let ((blanks (- w (+ lsi lsf))))
            (string-append (make-string (max blanks 0) #\space) si sf)))))))

(define (show-bar nb-good nb-fail nb-other nb-total elapsed)

  (define (ratio n)
    (quotient (* n (+ nb-good nb-fail nb-other)) nb-total))

  (if (tty? (current-output-port))

      (let* ((bar-width 42)
             (bar-length (ratio bar-width)))

        (define (esc x)
          x)

        (print "\r"
               "["
               (esc "\33[32;1m") (num->string nb-good 4 0) (esc "\33[0m")
               "|"
               (esc "\33[31;1m") (num->string nb-fail 4 0) (esc "\33[0m")
               "|"
               (esc "\33[34;1m") (num->string nb-other 4 0) (esc "\33[0m")
               "] "
               (num->string (ratio 100) 3 0)
               "% "
               (make-string bar-length #\#)
               (make-string (- bar-width bar-length) #\.)
               (num->string elapsed 6 1)
               "s"
               (esc "\33[K")))))

(define (run path . args)

  (let* ((port
          (open-process (list path: path
                              arguments: args
                              ;;stderr-redirection: #t
                              )))
         (output
          (read-line port #f))
         (status
          (process-status port)))
    (close-port port)
    (cons status output)))

(define (run-lc file . args)
  (pp file)
  (let* ((run-args (append (list "./lazy-comp" file) args))
         (x (apply run run-args)))
    (if (= (car x) 0)
        (let ((y (apply run run-args)))
          (if (= (car y) 0)
              (if (equal? (cdr y)
                          (get-expected-output file))
                  y
                  (cons 1 (cdr y)))))
        x)))

(define (test-using-mode file mode)

  (cond ((equal? mode "gsi")
         (run "gsi" "-:d-,fu,=.." file))
        ((equal? mode "lc")
         (run-lc file))
        ((equal? mode "lc-nep")
         (run-lc file "--disable-entry-points"))
        ((equal? mode "lc-nrp")
         (run-lc file "--disable-return-points"))
        ((or (equal? mode "lc-nep-nrp")
             (equal? mode "lc-nrp-nep"))
         (run-lc file "--disable-entry-points" "--disable-return-points"))
        ((equal? mode "lc-m5")
         (run-lc file "--max-versions 5"))
        (else (error "Unknown mode " mode))))

(define (get-expected-output filename)
  (call-with-input-file
      filename
    (lambda (port)
      (let ((lines (read-all port (lambda (p) (read-line p #\newline #t)))))
        (let loop ((rev-lines (reverse lines))
                   (output '()))
          (if (and (pair? rev-lines)
                   (>= (string-length (car rev-lines)) 1)
                   (char=? (string-ref (car rev-lines) 0) #\;))
              (loop (cdr rev-lines)
                    (cons (substring (car rev-lines)
                                     1
                                     (string-length (car rev-lines)))
                          output))
              (apply string-append output)))))))

(define (test file)
  (for-each

   (lambda (mode)
     (let ((result (test-using-mode file mode)))
       (if (= 0 (car result))
           (set! nb-good (+ nb-good 1))
           (begin
             (set! nb-fail (+ nb-fail 1))
             (print "\n")
             (print "*********************** FAILED TEST " mode " " file "\n")
             (print (cdr result))))

       (show-bar nb-good
                 nb-fail
                 nb-other
                 nb-total
                 (- (time->seconds (current-time)) start))))

   modes))

(define (run-tests files)

  (set! nb-good 0)
  (set! nb-fail 0)
  (set! nb-other 0)
  (set! nb-total (* (length modes) (length files)))
  (set! start (time->seconds (current-time)))

  (for-each test files)

  (print "\n")

  (if (= nb-good nb-total)
      (begin
        (print "PASSED ALL " nb-total " UNIT TESTS\n")
        (exit 0))
      (begin
        (print "FAILED " nb-fail " UNIT TESTS OUT OF " nb-total " (" (num->string (* 100. (/ nb-fail nb-total)) 0 1) "%)\n")
        (exit 1))))

(define (find-files file-or-dir filter)
  (if (eq? (file-type file-or-dir) 'directory)

      (apply
       append
       (map
        (lambda (f)
          (find-files (path-expand f file-or-dir) filter))
        (directory-files file-or-dir)))

      (if (filter file-or-dir)
          (list file-or-dir)
          (list))))

(define (list-of-scm-files args)
  (apply
   append
   (map
    (lambda (f)
      (find-files f
                  (lambda (filename)
                    (equal? (path-extension filename) ".scm"))))
    args)))

(define modes '())

(define (main . args)

  (let loop ()
    (if (and (pair? args)
             (> (string-length (car args)) 1)
             (char=? #\- (string-ref (car args) 0)))
        (begin
          (set! modes
                (cons (substring (car args) 1 (string-length (car args)))
                      modes))
          (set! args (cdr args))
          (loop))))

  (if (null? args)
      (set! args '("unit-tests")))

  (if (null? modes)
      (set! modes '("lc")))

  (run-tests (list-of-scm-files args)))

;;;============================================================================
