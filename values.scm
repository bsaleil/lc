
(include "config.scm")
(include "extern/copy-permanent.scm")

;; Return address of a gambit object (tagged object)
(define (object-address obj)
  (assert (or (perm-object? obj) (still-object? obj)) "Internal error")
  (let ((r (tagging-obj-encoding obj)))
    (- r TAG_MEMOBJ)))

(define opt-nan-boxing #f)
(define OFFSET_PAIR_CAR #f)
(define OFFSET_PAIR_CDR #f)
(define STAG_PAIR #f)
(define STAG_SYMBOL #f)
(define get-u64 #f)

(define STAG_STRING #f)
(define STAG_VECTOR #f)
(define STAG_PROCEDURE #f)
(define get-ieee754-imm64 #f)
(define ieee754->flonum #f)
(define STAG_STRING #f)
(define STAG_VECTOR #f)
(define get-ieee754-imm64 #f)
(define ieee754->flonum #f)
(define put-i64 #f)
(define perm-domain #f)

;;-------------------------------------------------------------------------
;; TAGGING

(define (tagging-obj-encoding obj  #!optional i)
  (let ((n (##object->encoding obj)))
    (if (>= n (expt 2 63)) (- n (expt 2 64)) n)))

(define (tagging-encoding-obj encoding)
  (let ((n
          (if (< encoding 0)
              (+ (expt 2 63) (bitwise-and encoding (- (expt 2 63) 1)))
              encoding)))
    (##encoding->object n)))

(define (to-64-value v)
  (if (>= v (expt 2 63)) (- v (expt 2 64)) v))

(define (to-32-value v)
  (if (>= v (expt 2 31)) (- v (expt 2 32)) v))

;;-------------------------------------------------------------------------
;; NAN BOXING

(define NB_MASK_SHIFT 46)
(define NB_MASK_VALUE_32  #xFFFFFFFF)
(define NB_MASK_VALUE_48  #xFFFFFFFFFFFF)

(define NB_MASK_FIX_UNSHIFTED #x3FFF8)
(define NB_MASK_CHA_UNSHIFTED #x3FFF9)

(define NB_MASK_FIX  (arithmetic-shift #x3FFF8 NB_MASK_SHIFT))
(define NB_MASK_CHA  (arithmetic-shift #x3FFF9 NB_MASK_SHIFT))
(define NB_MASK_SPE  (arithmetic-shift #x3FFFA NB_MASK_SHIFT))
(define NB_MASK_MEM  (arithmetic-shift #x3FFFC NB_MASK_SHIFT))
(define NB_MASK_TYPE (arithmetic-shift #x3FFFF NB_MASK_SHIFT))
(define NB_MASK_TAG  (arithmetic-shift #xFFFF 48))
(define NB_MASK_FLO_MAX_UNSHIFTED #xFFF8)
(define NB_MASK_FLO_MAX (arithmetic-shift #xFFF8 48))

(define NB_BIT_CHA (expt 2 46))

(define NB_ENCODED_VOID  (+ NB_MASK_SPE 1))
(define NB_ENCODED_NULL  (+ NB_MASK_SPE 2))
(define NB_ENCODED_EOF   (+ NB_MASK_SPE 3))
(define NB_ENCODED_TRUE  (+ NB_MASK_SPE 4))
(define NB_ENCODED_FALSE (+ NB_MASK_SPE 5))

(define NB_MAX_FIX (- (expt 2 31) 1))
(define NB_MIN_FIX (* -1 (expt 2 31)))

;;-----------------------------------------------------------------------------
;; OBJECT -> ENCODING
;;-----------------------------------------------------------------------------

(define (nanboxing-obj-encoding obj #!optional i)
  (let ((v
          (cond ;; 32 bits fixnum
                ((and (##fixnum? obj) (<= obj NB_MAX_FIX) (>= obj NB_MIN_FIX))
                   (if (< obj 0)
                       (let ((two-compl
                               (+ (bitwise-and (bitwise-not (* obj -1)) (- (expt 2 32) 1)) 1)))
                         (+ NB_MASK_FIX two-compl))
                       (+ NB_MASK_FIX obj)))
                ;; Procedure
                ((procedure? obj)
                   (let* ((addr (- (tagging-obj-encoding obj) 1))
                          (head (get-i64 addr)))
                     (if (not (= head 2166))
                         (error "K"))
                     (let* ((tagged (tagging-obj-encoding obj))
                            (addr (- tagged TAG_MEMOBJ)))
                       (+ NB_MASK_MEM addr))))
                ((port? obj) (error "Unexpected port"))
                ;;
                ;; MEM ALLOCATED
                ((or (symbol? obj) (string? obj) (port? obj) (f64vector? obj))
                   (if (not (perm-object? obj))
                       (set! obj (copy-permanent obj #f perm-domain)))
                   (let* ((tagged (tagging-obj-encoding obj))
                          (addr (- tagged TAG_MEMOBJ)))
                     (+ NB_MASK_MEM addr)))
                ;; pair
                ((pair? obj)
                   (if (not (perm-object? obj)) (error "Unexpected !perm object 2"))
                   (taggedpair->nanpair obj))
                ;; vector
                ((vector? obj)
                   (if (not (perm-object? obj)) (error "Unexpected !perm object 3"))
                   (taggedvector->nanvector obj))
                ;; END MEM ALLOCATED
                ;;
                ;; void
                ((equal? obj #!void)
                   NB_ENCODED_VOID)
                ;; null
                ((null? obj)
                   NB_ENCODED_NULL)
                ;; flonum
                ((or (##flonum? obj) (##fixnum? obj))
                   (get-ieee754-imm64 obj))
                ;; boolean
                ((boolean? obj)
                   (if obj
                       NB_ENCODED_TRUE
                       NB_ENCODED_FALSE))
                ;; char
                ((char? obj)
                   (let ((int (char->integer obj)))
                     (+ NB_MASK_CHA int)))
                ;; eof
                ((eof-object? obj)
                   NB_ENCODED_EOF)
                (else
                   (error "NYI1")))))
   (let ((encoding
           (if (>= v (expt 2 63)) (- v (expt 2 64)) v)))
     encoding)))

(define (taggedpair->nanpair pair)
  (let* ((ecar (nanboxing-obj-encoding (car pair) -1))
         (ecdr (nanboxing-obj-encoding (cdr pair) -2))
         ;(npair-m (cons 0 0))
         ;(npair (copy-permanent npair-m #f perm-domain))
         (npair (alloc-pair #f))
         (tagged (tagging-obj-encoding npair))
         (addr (- tagged TAG_PAIR)))
    (put-i64 (+ addr  8) ecdr)
    (put-i64 (+ addr 16) ecar)
    (+ NB_MASK_MEM addr)))

(define (taggedvector->nanvector vec)
  (let* ((len (vector-length vec))
         (v-m (make-vector len))
         (v (copy-permanent v-m #f perm-domain))
         (tagged (tagging-obj-encoding v))
         (addr (- tagged TAG_MEMOBJ)))
    (let loop ((i 0))
      (if (not (= i len))
          (let ((obj (vector-ref vec i)))
            (put-i64 (+ addr 8 (* 8 i)) (nanboxing-obj-encoding obj -3))
            (loop (+ i 1)))))
    (+ NB_MASK_MEM addr)))

;;-----------------------------------------------------------------------------
;; ENCODING -> OBJECT
;;-----------------------------------------------------------------------------

(define (nanboxing-encoding-obj encoding #!optional (i #f) (recursive #f))
  (if (not recursive)
      (set! cycle-set (make-table)))
  (let* ((encoding
           (if (< encoding 0)
               (+ (expt 2 63) (bitwise-and encoding (- (expt 2 63) 1)))
               encoding))
         (type-mask (bitwise-and encoding NB_MASK_TYPE))
         (tag-mask  (bitwise-and encoding NB_MASK_TAG)))

    (cond ;; 32 bits fixnum
          ((= type-mask NB_MASK_FIX)
             (let* ((32val (bitwise-and (- (expt 2 32) 1) encoding))
                    (sign (bitwise-and (expt 2 31) encoding))
                    (nencoding
                      (if (> sign 0) ;; neg
                          (bitwise-ior (arithmetic-shift (- (expt 2 32) 1) 32)
                                       (arithmetic-shift 32val 2))
                          (arithmetic-shift 32val 2))))
             (tagging-encoding-obj nencoding)))
          ;; Special
          ((= type-mask NB_MASK_SPE)
             (cond ;; null
                   ((= encoding NB_ENCODED_NULL) '())
                   ;; booleans
                   ((= encoding NB_ENCODED_TRUE) #t)
                   ((= encoding NB_ENCODED_FALSE) #f)
                   ;; void
                   ((= encoding NB_ENCODED_VOID) #!void)
                   (else
                     (error "NYI4"))))
          ;; Mem obj
          ((= tag-mask NB_MASK_MEM)
             (let* ((encoding (i))
                    (address (- encoding #xFFFF000000000000))
                    (header (get-u64 address))
                    (stag (arithmetic-shift (bitwise-and header 248) -3)))
               (if (not (= (+ address #xFFFF000000000000) (i)))
                   (error "Encoding changed"))
               (if (and (not (= stag STAG_STRING))
                        (not (= stag STAG_SYMBOL))
                        (not (= stag 29))) ;; f64vector
                   (begin (pp stag)
                          (error "Unexpected type")))
               (tagging-encoding-obj (+ address 1))))
          ;; Flonum
          ((< (bitwise-and encoding NB_MASK_TAG) NB_MASK_FLO_MAX)
             (ieee754->flonum encoding 'double))
          ;; Char
          ((= type-mask NB_MASK_CHA)
             (let ((int (bitwise-and encoding NB_MASK_VALUE_32)))
               (integer->char int)))
          ;;
          (else
             (pp encoding)
             (pp type-mask)
             (pp i)
             (error "NYI2")))))

;;
(define (nanpair->taggedpair addr)
  (error "wip 1")
  (let* ((ecar (get-u64 (+ addr OFFSET_PAIR_CAR)))
         (ecdr (get-u64 (+ addr OFFSET_PAIR_CDR)))
         (pair (alloc-pair #f)))
   (table-set! cycle-set addr pair)
   (##set-car! pair (nanboxing-encoding-obj ecar -1 #t))
   (##set-cdr! pair (nanboxing-encoding-obj ecdr -1 #t))
   pair))

;;
(define cycle-set (make-table))
(define (nanvector->taggedvector addr)
  (error "wip 2")
  (let* ((header (get-u64 addr))
         (len (arithmetic-shift header -11))
         (v (make-vector len))
         (vector (copy-permanent v #f perm-domain)))
    (table-set! cycle-set addr vector)
    (let loop ((i 0))
      (if (= i len)
          vector
          (let ((v (nanboxing-encoding-obj (get-u64 (+ addr 8 (* 8 i))) -1 #t)))
            (vector-set! vector i v)
            (loop (+ i 1)))))))

;;-------------------------------------------------------------------------

(define obj-encoding #f)
(define encoding-obj #f)

(define (init-values)
  (if opt-nan-boxing
      (begin (set! obj-encoding nanboxing-obj-encoding)
             (set! encoding-obj nanboxing-encoding-obj))
      (begin (set! obj-encoding tagging-obj-encoding)
             (set! encoding-obj tagging-encoding-obj))))
