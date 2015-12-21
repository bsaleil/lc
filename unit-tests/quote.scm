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

(println '())
(println (list? '()))
(println (pair? '()))
(println (length '()))

(println '(1 #f 3))
(println (list? '(1 2 3)))
(println (pair? '(1 2 3)))
(println (length '(1 2 3)))

(println '(10 20 (30 40) 50))
(println (list? '(10 20 (30 40) 50)))
(println (pair? '(10 20 (30 40) 50)))
(println (length '(10 20 (30 40) 50)))

(println (null? (car (cdr '(1 () 3)))))

(println '(1 . 2))
(println (list? '(1 . 2)))
(println (pair? '(1 . 2)))

(pp (vector-length '#(1 2 3)))
(pp (vector? '#(1 2 3)))
(pp '#(1 2 3))
(pp (vector-length '#("Hello" #\W #\o #\r #\l #\d)))
(pp (vector? '#("Hello" #\W #\o #\r #\l #\d)))
(pp '#("Hello" #\W #\o #\r #\l #\d))

;
;#t
;#f
;0
;1#f3
;#t
;#t
;3
;1020304050
;#t
;#t
;4
;#t
;12
;#f
;#t
;3
;#t
;#(1 2 3)
;6
;#t
;#("Hello" #\W #\o #\r #\l #\d)
