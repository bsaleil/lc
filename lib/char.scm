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

(define (char<? c1 c2)
   (< (char->integer c1) (char->integer c2)))

(define (char>? c1 c2)
   (> (char->integer c1) (char->integer c2)))

(define (char<=? c1 c2)
   (<= (char->integer c1) (char->integer c2)))

(define (char>=? c1 c2)
   (>= (char->integer c1) (char->integer c2)))

(define (char-alphabetic? c)
  (let ((c (char->integer c)))
    (or (and (> c 64) (< c 91))
      (and (> c 96) (< c 123)))))

(define (char-numeric? c)
  (let ((c (char->integer c)))
    (and (> c 47) (< c 58))))

(define (char-whitespace? c)
  (let ((c (char->integer c)))
    (or (= c 32) (= c 9) (= c 10) (= c 12) (= c 13))))

(define (char-upper-case? c)
  (let ((c (char->integer c)))
    (and (> c 64) (< c 91))))

(define (char-lower-case? c)
  (let ((c (char->integer c)))
    (and (> c 96) (< c 123))))

(define (char-upcase c)
   (let ((v (char->integer c)))
     (if (and (> v 96) (< v 123))
        (integer->char (- v 32))
        c)))

(define (char-downcase c)
   (let ((v (char->integer c)))
     (if (and (> v 64) (< v 91))
        (integer->char (+ v 32))
        c)))

(define (char-ci=? c1 c2)
   (= (char->integer (char-downcase c1))
      (char->integer (char-downcase c2))))

(define (char-ci<? c1 c2)
   (< (char->integer (char-downcase c1))
      (char->integer (char-downcase c2))))

(define (char-ci>? c1 c2)
   (> (char->integer (char-downcase c1))
      (char->integer (char-downcase c2))))

(define (char-ci<=? c1 c2)
   (<= (char->integer (char-downcase c1))
       (char->integer (char-downcase c2))))

(define (char-ci>=? c1 c2)
   (>= (char->integer (char-downcase c1))
       (char->integer (char-downcase c2))))
