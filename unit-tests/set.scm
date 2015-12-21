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


(let ((f #f)
	  (g #f))
   (set! f (lambda (n)
              (if (= 0 n)
              	(write-char #\P)
              	(g (- n 1)))))
   (set! g (lambda (n)
              (if (= 0 n)
              	(write-char #\I)
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
;PIPIPI
