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

(define (bar1 n)
  (if (= n 0)
    #t
    (bar1 (- n 1))))

(define (bar2 n)
  (if (= n 0)
    #t
    (begin 10
           (bar2 (- n 1)))))

(define (foo1 n)
  (if (= n 0)
    #t
    (let ((a (- n 1)))
      100
      (foo1 a))))

(define (foo2 n)
  (if (= n 0)
    #t
    (let* ((a (- n 1)))
      100
      (foo2 a))))

(define (fun n)
  (if (= n 0)
     #t
     ((lambda () 100 (fun (- n 1))))))

(define (fun2 a b c d e f g h i j k l)
  (if (= n 0)
      #t
      ((lambda () 100 (fun a b c d e f g h i j k l)))))

(pp (bar1 999999))
(pp (bar2 999999))
(pp (foo1 999999))
(pp (foo2 999999))
(pp (fun  99999))

;#t
;#t
;#t
;#t
;#t
