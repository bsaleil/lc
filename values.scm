
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
(define get-ieee754-imm64 #f)
(define ieee754->flonum #f)
(define STAG_STRING #f)
(define STAG_VECTOR #f)
(define get-ieee754-imm64 #f)
(define ieee754->flonum #f)
(define put-i64 #f)

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
                ((or (symbol? obj) (string? obj) (port? obj))
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
                ;; pair
                ((pair? obj)
                   (taggedpair->nanpair obj))
                ;; vector
                ((vector? obj)
                   (taggedvector->nanvector obj))
                (else
                   (pp obj)
                   (error "NYI1")))))
    (if (>= v (expt 2 63)) (- v (expt 2 64)) v)))

(define (taggedpair->nanpair pair)
  (let* ((ecar (nanboxing-obj-encoding (car pair)))
         (ecdr (nanboxing-obj-encoding (cdr pair)))
         (npair (cons 0 0))
         (tagged (tagging-obj-encoding npair))
         (addr (- tagged TAG_PAIR)))
    (put-i64 (+ addr  8) ecdr)
    (put-i64 (+ addr 16) ecar)
    (+ NB_MASK_MEM addr)))

(define (taggedvector->nanvector vec)
  (let* ((len (vector-length vec))
         (v (make-vector len))
         (tagged (tagging-obj-encoding v))
         (addr (- tagged TAG_MEMOBJ)))
    (let loop ((i 0))
      (if (not (= i len))
          (let ((obj (vector-ref vec i)))
            (put-i64 (+ addr 8 (* 8 i)) (nanboxing-obj-encoding obj))
            (loop (+ i 1)))))
    (+ NB_MASK_MEM addr)))

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
             (let* ((address (bitwise-and encoding NB_MASK_VALUE_48))
                    (header (get-u64 address))
                    (stag (arithmetic-shift (bitwise-and header 248) -3)))
               (cond ;; Symbol
                     ((or (= stag STAG_SYMBOL)
                          (= stag STAG_STRING)
                          (= stag 4)) ;; box
                        (tagging-encoding-obj (+ address TAG_MEMOBJ)))
                     ;; Pair
                     ((= stag STAG_PAIR)
                        (nanpair->taggedpair address))
                     ;; Vector
                     ((= stag STAG_VECTOR)
                       (nanvector->taggedvector address))
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

;;
(define (nanvector->taggedvector addr)
  (let* ((header (get-u64 addr))
         (len (arithmetic-shift header -11))
         (vector (make-vector len)))
    (let loop ((i 0))
      (if (= i len)
          vector
          (let ((v (nanboxing-encoding-obj (get-u64 (+ addr 8 (* 8 i))))))
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
