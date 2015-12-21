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

(define (map fn lst)
  (if (null? lst)
    '()
    (cons (fn (car lst)) (map fn (cdr lst)))))

(define (error msg . msgs)

  (define (print-msgs msgs)
    (if (null? msgs)
        (newline)
        (begin (print (car msgs))
               (print " ")
               (print-msgs (cdr msgs)))))

  (print "!!! ERROR - ")
  (print-msgs (cons msg msgs))
  (exit))

(define fatal-error
  (lambda (msg . msgs)
    (apply error (cons msg msgs))))

(define (apply fn args . r)
  ;; If more than 2 args, build arg list from rest param
  ;; (last arg must be a list)
  (define (gen-args l)
    (if (= (length l) 1)
        (if (list? (car l))
            (car l)
            (error "APPLY ERROR"))
        (cons (car l) (gen-args (cdr l)))))

  (if (null? r)
    ($apply fn args)
    ($apply fn (gen-args (cons args r)))))
