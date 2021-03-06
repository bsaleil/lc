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

(define (integer? n)
  (fixnum? n)) ;; TODO flonum

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (expt n m)
  (if (= m 0)
     1
     (* n (expt n (- m 1)))))

(define (max a . l)
  (define (max-h els m)
    (if (null? els)
       m
       (let ((c (car els)))
         (if (> c m)
            (max-h (cdr els) c)
            (max-h (cdr els) m)))))
  (max-h l a))

(define (min a . l)
  (define (min-h els m)
    (if (null? els)
       m
       (let ((c (car els)))
         (if (< (car els) m)
            (min-h (cdr els) (car els))
            (min-h (cdr els) m)))))
  (min-h l a))

(define (arithmetic-shift n s)
  (cond ((> s 0) (* n (expt 2 s)))
        (else
          (if (and (< n 0) (odd? n))
              (error "NYI case shr")
              (quotient n (expt 2 (* -1 s)))))))
