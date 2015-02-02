(include "expand.scm")

(define LIB_FILES
  '("./lib/op.scm"
    "./lib/string.scm"
    "./lib/print.scm"
    "./lib/vector.scm"
    "./lib/types.scm"
    "./lib/char.scm"
    "./lib/list.scm"))

(define (get-lib files)
    (if (null? files)
        '()
        (append (read-all (open-input-file (car files)))
                (get-lib (cdr files)))))

(define (write-lib exprs)
  
  (define (write-lib-h exprs f)
    (if (not (null? exprs))
      (begin (pretty-print (car exprs) f)
             (write-lib-h (cdr exprs) f))))
  
  (let ((f (open-output-file "./lib.scm")))
    (write-lib-h exprs f)
    (force-output f)))
    
(let ((lib (get-lib LIB_FILES)))
  (write-lib lib))
  ;(pp lib))