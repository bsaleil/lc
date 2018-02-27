
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

;;-------------------------------------------------------------------------
;; TAGGING

(define (tagging-obj-encoding obj)
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

;;-------------------------------------------------------------------------
;; NAN BOXING

(define NB_MASK_SHIFT 46)
(define NB_MASK_VALUE_32  #xFFFFFFFF)
(define NB_MASK_VALUE_48  #xFFFFFFFFFFFF)

(define NB_MASK_FIX_UNSHIFTED #x3FFF8)

(define NB_MASK_FIX  (arithmetic-shift #x3FFF8 NB_MASK_SHIFT))
(define NB_MASK_CHA  (arithmetic-shift #x3FFF9 NB_MASK_SHIFT))
(define NB_MASK_SPE  (arithmetic-shift #x3FFFA NB_MASK_SHIFT))
(define NB_MASK_MEM  (arithmetic-shift #x3FFFC NB_MASK_SHIFT))
(define NB_MASK_TYPE (arithmetic-shift #x3FFFF NB_MASK_SHIFT))
(define NB_MASK_TAG  (arithmetic-shift #xFFFF 48))
(define NB_MASK_FLO_MAX (arithmetic-shift #xFFF8 48))

(define NB_BIT_CHA (expt 2 46))

(define NB_ENCODED_VOID  (+ NB_MASK_SPE 1))
(define NB_ENCODED_NULL  (+ NB_MASK_SPE 2))
(define NB_ENCODED_EOF   (+ NB_MASK_SPE 3))
(define NB_ENCODED_TRUE  (+ NB_MASK_SPE 4))
(define NB_ENCODED_FALSE (+ NB_MASK_SPE 5))

(define NB_MAX_FIX (- (expt 2 32) 1))

(define (nanboxing-obj-encoding obj)
  (let ((v
          (cond ;; 32 bits fixnum
                ((and (##fixnum? obj) (<= obj NB_MAX_FIX))
                   (+ NB_MASK_FIX obj))
                ;; Symbol
                ((symbol? obj)
                   (let* ((tagged (tagging-obj-encoding obj))
                          (addr (- tagged TAG_MEMOBJ)))
                     (+ NB_MASK_MEM addr)))
                ;; void
                ((equal? obj #!void)
                   NB_ENCODED_VOID)
                ;; null
                ((null? obj)
                   NB_ENCODED_NULL)
                ;; flonum
                ((##flonum? obj)
                   (let ((w (get-ieee754-imm64 obj)))
                     w))
                ;; boolean
                ((boolean? obj)
                   (if obj
                       NB_ENCODED_TRUE
                       NB_ENCODED_FALSE))
                ;; char
                ((char? obj)
                   (let ((int (char->integer obj)))
                     (+ NB_MASK_CHA int)))
                (else
                   (error "NYI1")))))
    (if (>= v (expt 2 63)) (- v (expt 2 64)) v)))

(define (nanboxing-encoding-obj encoding #!optional (i #f))
  (let* ((encoding
           (if (< encoding 0)
               (+ (expt 2 63) (bitwise-and encoding (- (expt 2 63) 1)))
               encoding))
         (type-mask (bitwise-and encoding NB_MASK_TYPE))
         (tag-mask  (bitwise-and encoding NB_MASK_TAG)))

    (cond ;;
          ((= encoding 0) 0) ;; TODO: i == 11 when closure is not given
          ;; 32 bits fixnum
          ((= type-mask NB_MASK_FIX)
             (bitwise-and encoding NB_MASK_VALUE_32))
          ;; Special
          ((= type-mask NB_MASK_SPE)
             (cond ;; null
                   ((= encoding NB_ENCODED_NULL) '())
                   ;; booleans
                   ((= encoding NB_ENCODED_TRUE) #t)
                   ((= encoding NB_ENCODED_FALSE #f))
                   (else (error "NYI4"))))
          ;; Mem obj
          ((= tag-mask NB_MASK_MEM)
             (let* ((address (bitwise-and encoding NB_MASK_VALUE_48))
                    (header (get-u64 address))
                    (stag (arithmetic-shift (bitwise-and header 248) -3)))
               (cond ;; Symbol
                     ((= stag STAG_SYMBOL)
                        (tagging-encoding-obj (+ address TAG_MEMOBJ)))
                     ;; Pair
                     ((= stag STAG_PAIR)
                        (nanpair->taggedpair address))
                     (else
                        (pp header)
                        (pp stag)
                        (error "NYI3")))))
          ;; TODO: should be else case when nan boxing fully implemented
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
             (pp i)
             (error "NYI2")))))

;;
(define (nanpair->taggedpair addr)
  (let ((ecar (get-u64 (+ addr OFFSET_PAIR_CAR)))
        (ecdr (get-u64 (+ addr OFFSET_PAIR_CDR))))
   (cons (nanboxing-encoding-obj ecar)
         (nanboxing-encoding-obj ecdr))))


;;-------------------------------------------------------------------------

(define obj-encoding #f)
(define encoding-obj #f)

(define (init-values)
  (if opt-nan-boxing
      (begin (set! obj-encoding nanboxing-obj-encoding)
             (set! encoding-obj nanboxing-encoding-obj))
      (begin (set! obj-encoding tagging-obj-encoding)
             (set! encoding-obj tagging-encoding-obj))))
