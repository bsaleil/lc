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

(let ((x 1))
  (println x))

(let ((x 1)
      (y 2))
  (println x))

(let ((x 1)
      (y 2))
  (println y))

(let ((x 1)
      (y 2)
      (z 3))
  (println x)
  (println y)
  (println z)
  z)

(define lx 10)
(let ((lx 100)
      (ly (+ lx 200)))
  (println lx)
  (println ly))

(println 123456789)

(let* ((x 1)
      (y 2))
  (println x))

(let* ((x 1)
      (y 2))
  (println y))

(let* ((x 1)
      (y 2)
      (z 3))
  (println x)
  (println y)
  (println z)
  z)

(define la 10)
(let* ((lb (+ la 200))
       (la 100)
       (lc (+ la 200)))
  (println la)
  (println lb)
  (println lc))

(letrec ((f (lambda (n)
              (if (= 0 n)
                (write-char #\P)
                (g (- n 1)))))
         (g (lambda (n)
              (if (= 0 n)
                (write-char #\I)
                (f (- n 1))))))
   (f 11)
   (f 12)
   (f 13)
   (f 14)
   (f 15)
   (f 16)
   (newline))

(println

(let fact ((n 4))
   (println 10)
   (if (= n 0)
      1
      (* n (fact (- n 1)))))

)

;1
;1
;2
;1
;2
;3
;100
;210
;123456789
;1
;2
;1
;2
;3
;100
;210
;300
;IPIPIP
;10
;10
;10
;10
;10
;24
