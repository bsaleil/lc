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

;; Utils functions

;;-----------------------------------------------------------------------------
;; Sets
(define LIFE_PERM #f)
(define TAG_MEMOBJ #f)

;; Set subtraction with lists
;; return lsta - lstb
(define (set-sub lsta lstb)
  (keep (lambda (el) (not (member el lstb))) lsta))

;; Set union with lists
;; return lsta U lstb
(define (set-union lsta lstb)
  (if (null? lsta)
    lstb
    (if (member (car lsta) lstb)
      (set-union (cdr lsta) lstb)
      (set-union (cdr lsta) (cons (car lsta) lstb)))))

;; Set intersect with lists
;; return lsta INTERSECT lstb
(define (set-inter lsta lstb)
  (if (null? lsta)
      '()
      (if (member (car lsta) lstb)
          (cons (car lsta) (set-inter (cdr lsta) lstb))
          (set-inter (cdr lsta) lstb))))

;;-----------------------------------------------------------------------------
;; Lists

;; Build a new list of length n and apply proc to each element
(define (build-list n proc)
  (define (build-list-h p proc)
    (if (= p 0)
      '()
      (cons (proc (- n p)) (build-list-h (- p 1) proc))))
  (build-list-h n proc))

;; Build list of length n with optional init value
(define (make-list n #!optional (init #f))
  (if (= 0 n)
    '()
    (cons init (make-list (- n 1) init))))

;; Flatten list x
(define (flatten x)
   (cond ((null? x) '())
         ((not (pair? x)) (list x))
         (else (append (flatten (car x))
                       (flatten (cdr x))))))

(define (keep fn lst)
  (foldr (lambda (el r) (if (fn el) (cons el r) r)) '() lst))

(define (count lst fn)
  (foldr (lambda (n r) (if (fn n) (+ 1 r) r)) 0 lst))

(define (find fn lst)
  (if (null? lst)
      #f
      (or (and (fn (car lst)) (car lst))
          (find fn (cdr lst)))))

;; Return n firsts elements of lst
(define (list-head lst n)
  (cond ((= n 0) '())
        ((null? lst) (error "Error in list-head"))
        (else (cons (car lst) (list-head (cdr lst) (- n 1))))))

;; Return last element of lst
(define (list-last lst)
  (cond ((null? lst) (error "Internal error"))
        ((null? (cdr lst)) (car lst))
        (else (list-last (cdr lst)))))

;; Does l recursively contains el?
(define (contains l el)
  (if (pair? l)
     (or (contains (car l) el)
         (contains (cdr l) el))
     (if (eq? l el)
        #t
        #f)))

(define (index-of el lst)
  (let loop ((lst lst) (idx 0))
    (cond ((null? lst) #f)
          ((eq? (car lst) el) idx)
          (else (loop (cdr lst) (+ idx 1))))))

;; Return #t if lst contains at least one duplicate
;; Return #f if lst contains no duplicate
;; Check for duplicate using test function (eq?, equal?, ...)
(define (findDuplicates lst test)
  (let ((els (make-table test: test)))
    (define (find lst)
      (cond ((null? lst) #f)
            ((table-ref els (car lst) #f) #t)
            (else
              (table-set! els (car lst) #t)
              (find (cdr lst)))))
    (find lst)))

;; Return pair:
;; car: removed element
;; cdr: list with first occurrence of el removed
(define (assoc-remove el lst)
  (if (null? lst)
      (cons #f '())
      (if (equal? (car (car lst)) el)
          lst
          (let ((r (assoc-remove el (cdr lst))))
            (cons (car r)
                  (cons (car lst)
                        (cdr r)))))))

(define (symbol->list s)
  (string->list (symbol->string s)))

(define (list->symbol s)
  (string->symbol (list->string s)))

;;-----------------------------------------------------------------------------
;; Strings

;; Returns a newly allocated string which is a copy of str with all chars upcase
;; Accepts symbols
(define (string-upcase str)
  (define (string-upcase-h str pos newstr)
    (if (= pos (string-length str))
      newstr
      (begin (string-set! newstr pos (char-upcase (string-ref str pos)))
             (string-upcase-h str (+ pos 1) newstr))))
  (if (symbol? str)
    (let ((s (symbol->string str)))
      (string-upcase-h s 0 (make-string (string-length s))))
    (string-upcase-h str 0 (make-string (string-length str)))))

;;-----------------------------------------------------------------------------
;; Others

;; Foldr
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

;; Count assoc of el in lst
(define (assocount el lst)
  (if (null? lst)
    0
    (let ((c (car lst)))
      (if (equal? (car c) el)
        (+ 1 (assocount el (cdr lst)))
        (assocount el (cdr lst))))))

(define (void? n)
  (eq? #!void n))

;; Is the v a literal ?
(define (literal? v)
   (or (char? v) (number? v) (symbol? v) (vector? v) (string? v) (boolean? v) (null? v)))

;; Call n times the function fn with given args
(define (call-n n fn . args)
  (if (> n 0)
    (begin (apply fn args)
           (apply call-n (append (list (- n 1) fn ) args)))))

(define-macro (1-if condition)
  `(if ,condition 1 0))

;;-----------------------------------------------------------------------------
;; Specific to Gambit

(define (make-mcb size)
  (let ((mcb (##make-machine-code-block size)))
    (if (not mcb)
        (error "Internal error: can't allocate block of size" size)
        mcb)))

(define (pp-flonum n #!optional (nfrac 2))
  (define (print-int-part n)
      (print (##flonum->fixnum (truncate n))))
  (define (print-frac n nfrac)
    (if (not (= nfrac 0))
      (let ((mt (exact->inexact (* n 10))))
        (print-int-part mt)
        (print-frac (- mt (truncate mt)) (- nfrac 1)))))
  (let ((n (exact->inexact n)))
    (print-int-part n)
    (print ".")
    (print-frac (- n (truncate n)) nfrac)
    (newline)))

(define (permanent-object? obj)
  (and (##mem-allocated? obj)
       (let* ((obj-addr (- (tagging-obj-encoding obj 2000) TAG_MEMOBJ))
              (life (bitwise-and (get-i64 obj-addr) 7)))
         (= life LIFE_PERM))))

(define (alloc-perm-procedure)
  ((c-lambda ()
             scheme-object
             "___result = ___EXT(___alloc_scmobj) (NULL, ___sPROCEDURE, 8);")))

(define (alloc-still-vector-i64 len v)
  ((c-lambda (int int64)
             scheme-object
             "___result = ___EXT(___make_vector) (___PSTATE, ___arg1, ___arg2);")
   len
   v))

(define (alloc-still-vector len)
  ((c-lambda (int)
             scheme-object
             "___result = ___EXT(___make_vector) (___PSTATE, ___arg1, ___FAL);")
   len))

(define (alloc-perm-vector-i64 len init)
 ((c-lambda (int64 int64)
            scheme-object
            "___result = ___EXT(___make_vector) (NULL, ___arg1, ___arg2);")
  len init))

(define (release-still-vector vect)
  ((c-lambda (scheme-object)
             void
             "___EXT(___release_scmobj) (___arg1);")
   vect))

(define (get-u64 addr)
  ((c-lambda (int64) unsigned-long "___result = *___CAST(___U64*,___arg1);")
   addr))

(define (get-u48 addr)
  ((c-lambda (int64) unsigned-long "___result = (*___CAST(___U64*,___arg1)) & 0xFFFFFFFFFFFF;")
   addr))

(define (get-i64 addr)
  ((c-lambda (int64) long "___result = *___CAST(___S64*,___arg1);")
   addr))

(define (put-i64 addr val)
  ((c-lambda (int64 int64) void "*___CAST(___S64*,___arg1) = ___arg2;")
   addr
   val))

(define (get-i32 addr)
  ((c-lambda (int64) long "___result = *___CAST(___S32*,___arg1);")
   addr))

(define (put-i32 addr val)
  ((c-lambda (int64 int32) void "*___CAST(___S32*,___arg1) = ___arg2;")
   addr
   val))

(define (get-u8 addr)
  ((c-lambda (int64) long "___result = *___CAST(___U8*,___arg1);")
   addr))

(define (put-u8 addr val)
  ((c-lambda (int64 unsigned-int8) void "*___CAST(___U8*,___arg1) = ___arg2;")
   addr
   val))

 (define (get-i8 addr)
   ((c-lambda (int64) long "___result = *___CAST(___S8*,___arg1);")
    addr))

(define (get-scmobj addr)
  ((c-lambda (int64) scheme-object "___result = *___CAST(___SCMOBJ*,___arg1);")
   addr))

(define (put-scmobj addr val)
  ((c-lambda (int64 scheme-object) void "*___CAST(___SCMOBJ*,___arg1) = ___arg2;")
   addr
   val))

;;-----------------------------------------------------------------------------
