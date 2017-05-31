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

(define (foo1 a b)
	(pp a)
	(pp b))

(define (foo2 a b . c)
	(pp a)
	(pp b)
	(pp c))

(define (foo3 a b)
	(pp a)
	(pp b)
	(set! a 100)
	(pp a)
	(pp b))

(define (foo4 a b . c)
	(pp a)
	(pp b)
	(pp c)
	(set! a 200)
	(pp a)
	(pp b)
	(pp c))

(define (foo5 a b . c)
	(pp a)
	(pp b)
	(pp c)
	(set! a 300)
	(set! c '(1 2 3))
	(pp a)
	(pp b)
	(pp c))

(define (foo6 a b . c)
	(pp a)
	(pp b)
	(pp (null? c))
	(pp c))

(define (foo7 a . b)
	(pp a)
	(pp b))

(define (foo8 a b c d e f . g)
	(pp a)
	(pp f)
	(pp g))

(define foo9 (lambda x x))

(foo1 1 2)
(foo2 1 2)
(foo2 1 2 3 4 5)
(foo3 1 2)
(foo4 1 2 3 4)
(foo5 1 2 3 4)
(foo6 1 2)
(foo7 1 2 3 4 5)
(foo8 0 1 2 3 4 5 6 7 8 9)

(pp (foo9))
(pp (foo9 1))
(pp (foo9 1 2))
(pp (foo9 1 2 3))

(define (bar1 a b c d e f h i . g) (print a) (print c) (pp g))
(define (bar2 a b c d e f h . g)   (print a) (print c) (pp g))
(define (bar3 a b c d e f . g)     (print a) (print c) (pp g))
(define (bar4 a b c d e . g)       (print a) (print c) (pp g))
(define (bar5 a b c d . g)         (print a) (print c) (pp g))
(define (bar6 a b c . g)           (print a) (print c) (pp g))

(bar1 1 2 3 4 5 6 7 8)
(bar1 1 2 3 4 5 6 7 8 9)
(bar1 1 2 3 4 5 6 7 8 9 10)
(bar2 1 2 3 4 5 6 7)
(bar2 1 2 3 4 5 6 7 9)
(bar2 1 2 3 4 5 6 7 9 10)
(bar3 1 2 3 4 5 6)
(bar3 1 2 3 4 5 6 9)
(bar3 1 2 3 4 5 6 9 10)
(bar4 1 2 3 4 5)
(bar4 1 2 3 4 5 9)
(bar4 1 2 3 4 5 9 10)
(bar5 1 2 3 4)
(bar5 1 2 3 4 9)
(bar5 1 2 3 4 9 10)
(bar6 1 2 3)
(bar6 1 2 3 9)
(bar6 1 2 3 9 10)

(define (cs1 . l) (pp l))
(cs1 1 2 3 4 5 6 7)

;1
;2
;1
;2
;()
;1
;2
;(3 4 5)
;1
;2
;100
;2
;1
;2
;(3 4)
;200
;2
;(3 4)
;1
;2
;(3 4)
;300
;2
;(1 2 3)
;1
;2
;#t
;()
;1
;(2 3 4 5)
;0
;5
;(6 7 8 9)
;()
;(1)
;(1 2)
;(1 2 3)
;13()
;13(9)
;13(9 10)
;13()
;13(9)
;13(9 10)
;13()
;13(9)
;13(9 10)
;13()
;13(9)
;13(9 10)
;13()
;13(9)
;13(9 10)
;13()
;13(9)
;13(9 10)
;(1 2 3 4 5 6 7)
