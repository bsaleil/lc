;;---------------------------------------------------------------------------
;;
;;  Copyright (c) 2015, Baptiste Saleil. All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;   3. The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior written
;;      permission.
;;
;;  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
;;  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;---------------------------------------------------------------------------

(include "expand.scm")

(define LIB_FILES
  '("./lib/op.scm"
    "./lib/string.scm"
    "./lib/print.scm"
    "./lib/vector.scm"
    "./lib/types.scm"
    "./lib/char.scm"
    "./lib/list.scm"
    "./lib/fn.scm"
    "./lib/num.scm"
    "./lib/fake.scm"
    "./lib/read.scm"))

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
