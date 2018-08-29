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

;; Predicates

(define (port? p)
  (or (input-port? p)
      (output-port? p)))

(define (boolean? n)
  (or (eq? n #t)
      (eq? n #f)))

;; Conversion

(define (vector->list v)
   (vector->list-h v 0 (vector-length v)))

(define (vector->list-h vector idx length)
   (if (= idx length)
      '()
      (cons (vector-ref vector idx) (vector->list-h vector (+ idx 1) length))))

(define (list->vector l)
  (let ((v (make-vector (length l))))
     (list->vector-h l v 0 (length l))))

(define (list->vector-h lst vec pos len)
  (if (null? lst)
    vec
    (begin (vector-set! vec pos (car lst))
           (list->vector-h (cdr lst) vec (+ pos 1) len))))

(define (list->f64vector lst)
  (let ((len (length lst)))
    (let loop ((v (make-f64vector len))
               (lst lst)
               (idx 0))
      (if (= idx len)
          v
          (begin (f64vector-set! v idx (car lst))
                 (loop v (cdr lst) (+ idx 1)))))))

(define (number->string num)
  (define (digit->string d)
    (make-string 1 (integer->char (+ d 48))))
  (define (number->string-h num)
    (if (= num 0)
        ""
        (string-append (number->string-h (quotient num 10))
                       (digit->string    (modulo   num 10)))))
  (cond ((= num 0) "0")
        ((< num 0) (string-append "-" (number->string-h (* num -1))))
        (else (number->string-h num))))

(define (string->number str . l)
  (define (s->n str pos)
    (if (= pos (string-length str))
      ""
      (let ((c (string-ref str pos)))
        (if (char-numeric? c)
          ;;
          (let ((r (s->n str (+ pos 1))))
            (if r
               (string-append (make-string 1 c) r)
               #f))
          ;;
          #f))))
  (if (= (string-length str) 0)
     #f
     (s->n str 0)))
