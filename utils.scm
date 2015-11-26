;; Utils functions

;;-----------------------------------------------------------------------------
;; Sets

;; Set subtraction with lists
;; return lsta - lstb
;; res is accu
(define (set-sub lsta lstb res)
  (if (null? lsta)
    res
    (if (member (car lsta) lstb)
      (set-sub (cdr lsta) lstb res)
      (set-sub (cdr lsta) lstb (cons (car lsta) res)))))

;; Set union with lists
;; return lsta U lstb
(define (set-union lsta lstb)
  (if (null? lsta)
    lstb
    (if (member (car lsta) lstb)
      (set-union (cdr lsta) lstb)
      (set-union (cdr lsta) (cons (car lsta) lstb)))))

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

(define (count lst fn)
  (foldr (lambda (n r) (if (fn n) (+ 1 r) r)) 0 lst))

;; Return n firsts elements of lst
(define (list-head lst n)
  (cond ((= n 0) '())
        ((null? lst) (error "Error in list-head"))
        (else (cons (car lst) (list-head (cdr lst) (- n 1))))))

;; Does l recursively contains el?
(define (contains l el)
  (if (pair? l)
     (or (contains (car l) el)
         (contains (cdr l) el))
     (if (eq? l el)
        #t
        #f)))

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

;; Is the v a literal ?
(define (literal? v)
   (or (char? v) (number? v) (symbol? v) (vector? v) (string? v) (boolean? v) (null? v)))

;; Call n times the function fn with given args
(define (call-n n fn . args)
  (if (> n 0)
    (begin (apply fn args)
           (apply call-n (append (list (- n 1) fn ) args)))))

;;-----------------------------------------------------------------------------
;; Specific to Gambit

(define (make-mcb size)
  (let ((mcb (##make-machine-code-block size)))
    (if (not mcb)
        (error "Internal error: can't allocate block of size" size)
        mcb)))

(define (obj-encoding obj)
  (let ((n (##object->encoding obj)))
    (if (>= n (expt 2 63)) (- n (expt 2 64)) n)))

(define (encoding-obj encoding)
  (##encoding->object encoding))

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

(define (alloc-still-vector len)
  ((c-lambda (int)
             scheme-object
             "___result = ___EXT(___make_vector) (___PSTATE, ___arg1, ___FAL);")
   len))

(define (release-still-vector vect)
  ((c-lambda (scheme-object)
             void
             "___EXT(___release_scmobj) (___arg1);")
   vect))

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

(define (get-scmobj addr)
  ((c-lambda (int64) scheme-object "___result = *___CAST(___SCMOBJ*,___arg1);")
   addr))

(define (put-scmobj addr val)
  ((c-lambda (int64 scheme-object) void "*___CAST(___SCMOBJ*,___arg1) = ___arg2;")
   addr
   val))

;;-----------------------------------------------------------------------------
