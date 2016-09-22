(include "~~lib/_gambit#.scm")

(define (get-i64 addr)
  ((c-lambda (int64) long "___result = *___CAST(___S64*,___arg1);")
   addr))

(define (alloc-pair still?)
  ((c-lambda (scheme-object) scheme-object
             "___result = ___EXT(___make_pair) ((___arg1 == ___FAL) ? NULL : ___ps, ___FIX(0), ___FIX(0));")
   still?))

(define (alloc-ovector len still?)
  ((c-lambda (scheme-object scheme-object) scheme-object
             "___result = ___EXT(___make_vector) ((___arg2 == ___FAL) ? NULL : ___ps, ___INT(___arg1), ___FIX(0));")
   len
   still?))

(define (alloc-bvector st len still?)
  ((c-lambda (scheme-object scheme-object scheme-object) scheme-object
             "___result = ___EXT(___alloc_scmobj) ((___arg3 == ___FAL) ? NULL : ___ps, ___INT(___arg1), ___INT(___arg2));")
   st
   len
   still?))

(define-type domain
  constructor: make-initialized-domain
  copies
  bytes-copied
)

(define (make-perm-domain)
  (make-initialized-domain
   (make-table test: eq?)
   0))

(define (copy-permanent obj still? domain)

  (define (register-copy! obj copy)
    (table-set! (domain-copies domain) obj copy))

  (define (existing-copy-of obj)
    ;;TODO: check if the existing copy is of the kind we want
    (table-ref (domain-copies domain) obj #f))

  (define (count-bytes-copied! obj)
    (domain-bytes-copied-set!
     domain
     (+ (##u8vector-length obj)
        (##u8vector-length '#(1))
        (domain-bytes-copied domain)))
    obj)

  (define (new-pair)
    (count-bytes-copied! (alloc-pair still?)))

  (define (new-ovector len)
    (count-bytes-copied! (alloc-ovector len still?)))

  (define (new-bvector st len)
    (count-bytes-copied! (alloc-bvector st len still?)))

  (define (copy-pair obj)
    (let ((copy (new-pair)))
      (register-copy! obj copy)
      (##set-car! copy (copy-object (car obj)))
      (##set-cdr! copy (copy-object (cdr obj)))
      copy))

  (define (copy-ovector obj st)
    (let* ((len (##vector-length obj))
           (copy (new-ovector len)))
      (##subtype-set! copy st)
      (register-copy! obj copy)
      (let loop ((i 0))
        (if (< i len)
            (begin
              (##vector-set! copy i (copy-object (##vector-ref obj i)))
              (loop (+ i 1)))
            copy))))

  (define (copy-bvector obj st)
    (let* ((len (##u8vector-length obj))
           (copy (new-bvector st len)))
      (register-copy! obj copy)
      (let loop ((i 0))
        (if (< i len)
            (begin
              (##u8vector-set! copy i (##u8vector-ref obj i))
              (loop (+ i 1)))
            copy))))

  (define (copy-object obj)
    ;(pp obj)
    (if (not (##mem-allocated? obj))

        obj

        (or (existing-copy-of obj)
            (let ((st (##subtype obj)))

              (cond ((= st (macro-subtype-pair))
                     (copy-pair obj))

                    ((or (= st (macro-subtype-vector))
                         (= st (macro-subtype-ratnum))
                         (= st (macro-subtype-cpxnum))
                         (= st (macro-subtype-structure))
                         (= st (macro-subtype-boxvalues))
                         (= st (macro-subtype-meroon))
                         (= st (macro-subtype-jazz))
                         (= st (macro-subtype-promise)))
                     (copy-ovector obj st))

                    ((= st (macro-subtype-symbol))
                     obj)

                    ((= st (macro-subtype-keyword))
                     obj)

                    ((= st (macro-subtype-frame))
                     (error "cannot copy frame:" obj))

                    ((= st (macro-subtype-continuation))
                     (error "cannot copy continuation:" obj))

                    ((= st (macro-subtype-weak))
                     (if (will? obj)
                         (error "cannot copy will:" obj)
                         (copy-ovector obj st)))

                    ((= st (macro-subtype-procedure))
                     (if (eq? (mem-allocated-kind obj) 'PERM)
                         obj
                         (copy-ovector obj st)))

                    ((= st (macro-subtype-return))
                     (error "cannot copy return:" obj))

                    ((= st (macro-subtype-foreign))
                     (error "cannot copy foreign:" obj))

                    ((or (= st (macro-subtype-string))
                         (= st (macro-subtype-s8vector))
                         (= st (macro-subtype-u8vector))
                         (= st (macro-subtype-s16vector))
                         (= st (macro-subtype-u16vector))
                         (= st (macro-subtype-s32vector))
                         (= st (macro-subtype-u32vector))
                         (= st (macro-subtype-f32vector))
                         (= st (macro-subtype-s64vector))
                         (= st (macro-subtype-u64vector))
                         (= st (macro-subtype-f64vector))
                         (= st (macro-subtype-flonum))
                         (= st (macro-subtype-bignum)))
                     (copy-bvector obj st))

                    (else
                     (error "cannot copy object:" obj)))))))

  (copy-object obj))

(define MOVABLE0 0)
(define MOVABLE1 1)
(define MOVABLE2 2)
(define STILL    5)
(define PERM     6)

(define TAG_MEMOBJ  1)
(define TAG_PAIR    3)

(define (mem-allocated-kind obj)
  (let ((k (bitwise-and
             (if (pair? obj)
                 (get-i64 (- (##object->encoding obj) TAG_PAIR))
                 (get-i64 (- (##object->encoding obj) TAG_MEMOBJ)))
             7)))
    (cond ((= k MOVABLE0) 'MOVABLE0)
          ((= k MOVABLE1) 'MOVABLE1)
          ((= k MOVABLE2) 'MOVABLE2)
          ((= k STILL)    'STILL)
          ((= k PERM)     'PERM)
          (else '???))))
