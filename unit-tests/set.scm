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

(println
	((lambda (n)
		n) 1))

(println ((lambda (n)
		 	(set! n 2)
			n) 100))

(println ((lambda (n)
	      	((lambda () (set! n 3)))
			n) 100))

(println ((lambda (n)
			((lambda (n) (set! n 100)) 200)
			n) 4))

(let ((a 100)
	  (b 200))
   (set! a 3)
   (set! b 2)
   (println (+ a b)))

(define global 100)
(set! global 6)
(println global)

((lambda ()
    (set! global 7)
    (println global)))

(define (setlet1 n)
 (let ((n (begin (set! n 7) n)))
   (+ n 1)))

(println (setlet1 000))

(define (setlet2 n)
 (let ((n (begin (set! n 200) n)))
   (set! n 8)
   (+ n 1)))

(println (setlet2 111))

(define (setlet3 n)
 (set! n 100)
 (let ((n (begin (set! n 9) n)))
   (+ n 1)))

(println (setlet3 222))

(define (setletrec1 n)
 (letrec ((m 100)
          (n (begin (set! n 10) n)))
   (+ n 1)))

(println (setletrec1 333))

(define (setletrec2 n)
 (let ((m 200)
       (n (begin (set! n 200) n)))
   (set! n 11)
   (+ n 1)))

(println (setletrec2 444))

(define (setletrec3 n)
 (set! n 100)
 (let ((m 300)
       (n (begin (set! n 12) n)))
   (+ n 1)))

(println (setletrec3 555))

(let ((f #f)
	  (g #f))
   (set! f (lambda (n)
              (if (= 0 n)
              	(write-char #\L)
              	(g (- n 1)))))
   (set! g (lambda (n)
              (if (= 0 n)
              	(write-char #\N)
              	(f (- n 1)))))
   (f 0)
   (f 1)
   (f 2)
   (f 3)
   (f 4)
   (f 5)
   (newline))

;1
;2
;3
;4
;5
;6
;7
;8
;9
;10
;11
;12
;13
;LNLNLN
