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

;; Number
(println 0)
(println -0)
(println -000)
(println 12345)
(println -12345)
(println (* (+ 5 2) (- 5 1)))

;; Boolean
(println #t)
(println #f)
(println (not (not (not (<= 10 10)))))

;; Null
(println '())

;; Pair
(println (cons 10 20))
(println (cons 10 (cons 20 '())))
(println (cons 99 (cons #f (cons '() '()))))
;;
(println '(1 2 3))
(println '(1 2 (3 4) 5 6))
(println '(1 2 (3 4) 5))

;; Char
(println #\a)
(println #\Z)
(println #\?)
(println #\newline)
(println (integer->char 104))

;; String
(println "Hello World")
(println "éêà")
(println "10€ or 10$")
(println (make-string 5 (integer->char 65)))
(println (substring "Dark Vador" 0 4))

;; Vector
(println (make-vector 4 "Hi."))
(define v (make-vector 4 0))
(vector-set! v 2 "Hey")
(println v)
(println (vector-ref v 0))

;; Symbol
(println 'Hello)
(define sym 'Dark)
(define str " Vador")
(println (string->symbol (string-append (symbol->string sym) str)))

;0
;0
;0
;12345
;-12345
;28
;#t
;#f
;#f
;
;1020
;1020
;99#f
;123
;123456
;12345
;a
;Z
;?
;
;
;h
;Hello World
;éêà
;10€ or 10$
;AAAAA
;Dark
;Hi.Hi.Hi.Hi.
;00Hey0
;0
;Hello
;Dark Vador
