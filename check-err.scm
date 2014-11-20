#! /usr/bin/env gsi
                  
;; $$putchar
;; out of bounds

(define tests '(       
  ;; Primitives
  "($+ #f 2)"
  "($+ 2 #f)"
  "($- #f 2)"
  "($- 2 #f)"
  "($* #f 2)"
  "($* 2 #f)"
  "($quotient #f 2)"
  "($quotient 2 #f)"
  "($modulo #f 2)"
  "($modulo 2 #f)"
  "($< #f 1)"
  "($< 1 #f)"
  "($> #f 1)"
  "($> 1 #f)"
  "($= #f 1)"
  "($= 1 #f)"
  ;; 
  "($car #f)"
  "($cdr #f)"
  ;; Vectors
  "($vector-length #f)"
  "($vector-ref #f 1)"
  "($vector-ref (make-vector 10) #f)"
  "($vector-set! #f 1 1)"
  "($vector-set! (make-vector 10) #t 1)"
  "($make-vector #f)"
  ;; Strings
  "($string-length #f)"
  "($string-ref #f 1)"
  "($string-ref \"Hi\" #f)"
  "($string-set! #f 1 #\\A)"
  "($string-set! \"Hi\" #f #\\A)"
  "($string-set! \"Hi\" 1 #f)"
  "($make-string #f)"
  ;; Types
  "($integer->char #f)"
  "($char->integer #f)"
))
  
(define (run path . args)
  (let* ((port
          (open-process (list path: path
                              arguments: args
                              stderr-redirection: #t
                              )))
         (output
          (read-line port #f))
         (status
          (process-status port)))
    (close-port port)
    (cons status output)))
  
(define (check tests)
  (if (null? tests)
      #t
      (let ((p (open-output-file "ERR.scm")))
        (display (car tests) p)
        (close-port p)
        (let ((res (run "./lazy-comp" "ERR.scm")))
           (if (not (is-err (cdr res)))
               (println "Err check FAIL: " (car tests)))
           (check (cdr tests))))))

(define (is-err str)
  (and (string? str)
       (>= (string-length str) 9)
       (equal? (substring str 0 9) "!!! ERROR")))

(check tests)
(run "rm" "ERR.scm")