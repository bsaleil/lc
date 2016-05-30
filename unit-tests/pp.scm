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

(pp 0)
(pp -0)
(pp -000)
(pp 12345)
(pp -12345)
(pp (* (+ 5 2) (- 5 1)))

(pp #t)
(pp #f)
(pp (not (not (not (<= 10 10)))))

(pp '())

(pp (lambda (x) (* x x)))

(pp (cons 10 20))
(pp (cons 10 (cons 20 '())))
(pp (cons 99 (cons #f (cons '() '()))))

(pp '(1 #f 3))
(pp '(1 () (#f 4) 5 #t))
(pp '(1 #f (3 4) ()))

(pp (make-vector 2 42))
(pp (list->vector '(1 2 3 4 5)))
(pp (make-vector 0))

(pp #\a)
(pp #\A)
(pp #\newline)
(pp (string-ref "a a" 1))

(pp "Hello World")
(pp (make-string 4 #\Z))

(pp 'SYMBOL)
(pp (string->symbol (string-append "SYM" (symbol->string 'BOL))))

;0
;0
;0
;12345
;-12345
;28
;#t
;#f
;#f
;()
;#<procedure #2>
;(10 . 20)
;(10 20)
;(99 #f ())
;(1 #f 3)
;(1 () (#f 4) 5 #t)
;(1 #f (3 4) ())
;#(42 42)
;#(1 2 3 4 5)
;#()
;#\a
;#\A
;#\newline
;#\space
;"Hello World"
;"ZZZZ"
;SYMBOL
;SYMBOL
