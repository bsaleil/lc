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

(define vect (make-vector 5))

(println (vector? vect))
(println vect)
(println (vector-length vect))

(vector-set! vect 2 100)

(println vect)
(println (list? (vector->list vect)))
(println (vector->list vect))

(vector-fill! vect #f)

(println vect)

(println (list->vector '(1 2 3 4)))
(println (vector? (list->vector '(1 2 3 4))))
(println (vector-ref (list->vector '(1 2 3 4)) 0))
(println (vector-ref (list->vector '(1 2 3 4)) 1))
(println (vector-ref (list->vector '(1 2 3 4)) 2))
(println (vector-ref (list->vector '(1 2 3 4)) 3))

(println (make-vector 2 #f))
(pp      (make-vector 2 #f))

(define a (vector))
(define b (vector 1))
(define c (vector 1 2))
(define d (vector 1 2 3 4 5))

(pp (list? a))
(pp (vector? a))
(pp a)

(pp (list? b))
(pp (vector? b))
(pp b)

(pp (list? c))
(pp (vector? c))
(pp c)

(pp (list? d))
(pp (vector? d))
(pp d)

(pp (make-vector 0))
(pp (make-vector 0 #t))
(pp (make-vector 1))
(pp (make-vector 1 #f))
(pp (make-vector 4))
(pp (make-vector 4 #f))

;#t
;00000
;5
;0010000
;#t
;0010000
;#f#f#f#f#f
;1234
;#t
;1
;2
;3
;4
;#f#f
;#(#f #f)
;#f
;#t
;#()
;#f
;#t
;#(1)
;#f
;#t
;#(1 2)
;#f
;#t
;#(1 2 3 4 5)
;#()
;#()
;#(0)
;#(#f)
;#(0 0 0 0)
;#(#f #f #f #f)
