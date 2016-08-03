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

(println (eq?  10  10))
(println (eq?  10 -10))
(println (eq? -10  10))
(println (eq? -10 -10))
(println (eq? (- 20 10) (+ 5 5)))

(println (eq? #f 10))
(println (eq? #f #t))
(println (eq? #f #f))
(println (eq? #t #t))

(println (eq? '() 10))
(println (eq? '() #f))
(println (eq? '() '()))

(println (eq? (cons 1 2) 10))
(println (eq? (cons 1 2) #t))
(println (eq? (cons 1 2) '()))
(println (eq? (cons 1 2) (cons 1 2)))

(println (eq? (lambda (x y) (* x y)) 10))
(println (eq? (lambda (x y) (* x y)) #f))
(println (eq? (lambda (x y) (* x y)) '()))
(println (eq? (lambda (x y) (* x y)) (cons 1 2)))

;#t
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
