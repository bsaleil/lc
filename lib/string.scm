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

(define (string->list-h s pos)
  (if (= (string-length s) pos)
      '()
      (cons (string-ref s pos) (string->list-h s (+ pos 1)))))

(define (string->list s)
  (string->list-h s 0))

(define (string-fill!-h str char pos len)
  (if (< pos len)
    (begin (string-set! str pos char)
         (string-fill!-h str char (+ pos 1) len))
    str))

(define (string-fill! str char)
  (string-fill!-h str char 0 (string-length str)))

(define (list->string-h l str pos)
  (if (null? l)
     str
     (begin (string-set! str pos (car l))
            (list->string-h (cdr l) str (+ pos 1)))))

(define (list->string l)
  (let ((str (make-string (length l))))
     (list->string-h l str 0)))

(define (string-h str chars pos)
  (if (null? chars)
     str
     (begin (string-set! str pos (car chars))
            (string-h str (cdr chars) (+ pos 1)))))

(define (string . chars)
   (if (null? chars)
      ""
      (let ((str (make-string (length chars))))
         (string-h str chars 0))))

(define (substring-h to from posf post end)
  (if (= posf end)
     to
     (begin (string-set! to post (string-ref from posf))
            (substring-h to from (+ posf 1) (+ post 1) end))))

(define (substring string start end)
   (if (or (< start 0) (> end (string-length string)) (< end start))
      (error "Substring")
      (let ((new-str (make-string (- end start))))
        (substring-h new-str string start 0 end))))

(define (string-append-two str str2)
  (let* ((l1 (string-length str))
       (l2 (string-length str2))
       (new-str (make-string (+ l1 l2))))
    (do ((pos 0 (+ pos 1)))
      ((= pos (+ l1 l2)) new-str)
      (if (< pos l1)
         (string-set! new-str pos (string-ref str pos))
         (string-set! new-str pos (string-ref str2 (- pos l1)))))))

(define (string-append-h strings)
   (cond ((null? strings) "")
         ((null? (cdr strings)) (car strings))
         (else (string-append-h (cons (string-append-two (car strings) (cadr strings))
                                      (cddr strings))))))

(define (string-append . strings)
   (string-append-h strings))

(define (string-copy str)
   (do ((new-str (make-string (string-length str)))
        (pos 0 (+ pos 1)))
       ((= pos (string-length str)) new-str)
       (string-set! new-str pos (string-ref str pos))))

(define (string=?-h str1 str2 pos)
   (cond ((= pos (string-length str1)) (= pos (string-length str2)))
         ((= pos (string-length str2)) #f)
         (else (if (char=? (string-ref str1 pos) (string-ref str2 pos))
                  (string=?-h str1 str2 (+ pos 1))
                  #f))))

(define (string=? str1 str2)
  (string=?-h str1 str2 0))

;; Rewrite stirng<?
(define (string<? str1 str2)
  (define (string<?-h str1 str2 pos)
    (cond ((= pos (- (string-length str2) 1))
             (char<? (string-ref str1 pos)
                     (string-ref str2 pos)))
          ((= pos (- (string-length str1) 1))
             (char<=? (string-ref str1 pos)
                     (string-ref str2 pos)))
          (else (and (char<=? (string-ref str1 pos)
                             (string-ref str2 pos))
                     (string<?-h str1 str2 (+ pos 1))))))

  (cond ((= (string-length str1) 0)
           (> (string-length str2) 0))
        ((= (string-length str2) 0)
           #f)
        (else (string<?-h str1 str2 0))))
