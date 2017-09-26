#! /usr/bin/env gsi

;;;============================================================================

;;; File: "run-unit-tests.scm"

;;; Copyright (c) 2012-2014 by Marc Feeley, All Rights Reserved.

;;;----------------------------------------------------------------------------

;;; This is a modified version of the run-unit-tests.scm
;;; file from Gambit Scheme (http://gambitscheme.org/)

(define nb-good 0)
(define nb-fail 0)
(define nb-other 0)
(define nb-total 0)
(define start 0)

(define PATH "")
(define EXEC "")
(define ARGS '())

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
  (let* ((run-args (append (list "./lc" file) args))
         (x (apply run run-args)))
    (if (= (car x) 0)
        (let ((y (apply run run-args)))
          (if (= (car y) 0)
              (if (equal? (cdr y)
                          (get-expected-output file))
                  y
                  (cons 1 (cdr y)))))
        x)))

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

 (let ((result (apply run-lc (cons file ARGS))))
   (if (= 0 (car result))
       (set! nb-good (+ nb-good 1))
       (begin
         (set! nb-fail (+ nb-fail 1))
         (print "\n")
         (print "*********************** FAILED TEST " file "\n")
         (print (cdr result))))

   (show-bar nb-good
             nb-fail
             nb-other
             nb-total
             (- (time->seconds (current-time)) start))))

(define (run-tests files)

  (set! nb-good 0)
  (set! nb-fail 0)
  (set! nb-other 0)
  (set! nb-total (length files))
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

(define (main . args)

  (set! PATH "unit-tests")
  (set! EXEC "lc")
  (set! ARGS args)

  (run-tests (list-of-scm-files (list PATH))))

;;;============================================================================
