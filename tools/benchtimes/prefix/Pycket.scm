#lang r5rs
(#%require compatibility/defmacro)
(#%require (only racket/base time arithmetic-shift bitwise-and))

(define-syntax define-syntax-rule
  (syntax-rules ()
    ((define-syntax-rule (name . pattern) template)
     (define-syntax name
       (syntax-rules ()
         ((name . pattern) template))))))

(define-syntax-rule (error a ...) #f)
(define-syntax-rule (fatal-error a ...) #f)

(define-syntax-rule (list->f64vector lst)    (list->vector lst))
(define-syntax-rule (f64vector? lst)         (vector? lst))
(define-syntax-rule (f64vector a ...)        (vector a ...))
(define-syntax-rule (make-f64vector a ...)   (make-vector a ...))
(define-syntax-rule (f64vector-ref a ...)    (vector-ref a ...))
(define-syntax-rule (f64vector-set! a ...)   (vector-set! a ...))
(define-syntax-rule (f64vector-length a ...) (vector-length a ...))

;------------------------------------------------------------------------------
; Macros

(define-syntax-rule (nuc-const a ...) '#(a ...))
(define-syntax-rule (floatvector-const a ...) (vector a ...))
;(define-macro (FLOATvector-const . lst)   `',(list->f64vector lst))
(define-syntax-rule (FLOATvector? x)         (f64vector? x))
(define-syntax-rule (FLOATvector a ...)      (f64vector a ...))
(define-syntax-rule (FLOATmake-vector n a ...) (make-f64vector n a ...))
(define-syntax-rule (FLOATvector-ref v i)    (f64vector-ref v i))
(define-syntax-rule (FLOATvector-set! v i x) (f64vector-set! v i x))
(define-syntax-rule (FLOATvector-length v)   (f64vector-length v))

; (define-macro (nuc-const . lst)
;   `',(list->vector
;        (map (lambda (x)
;               (if (vector? x)
;                 (list->f64vector (vector->list x))
;                 x))
;             lst)))

(define-syntax-rule (FLOAT+ a ...)  (+  a ...))
(define-syntax-rule (FLOAT- a ...)  (-  a ...))
(define-syntax-rule (FLOAT* a ...)  (*  a ...))
(define-syntax-rule (FLOAT/ a ...)  (/  a ...))
(define-syntax-rule (FLOAT= a ...)  (=  a ...))
(define-syntax-rule (FLOAT< a ...)  (<  a ...))
(define-syntax-rule (FLOAT<= a ...) (<= a ...))
(define-syntax-rule (FLOAT> a ...)  (>  a ...))
(define-syntax-rule (FLOAT>= a ...) (>= a ...))
(define-syntax-rule (FLOATnegative? a ...) (negative? a ...))
(define-syntax-rule (FLOATpositive? a ...) (positive? a ...))
(define-syntax-rule (FLOATzero? a ...)     (zero?     a ...))
(define-syntax-rule (FLOATabs a ...)   (abs  a ...))
(define-syntax-rule (FLOATsin a ...)   (sin  a ...))
(define-syntax-rule (FLOATcos a ...)   (cos  a ...))
(define-syntax-rule (FLOATatan a ...)  (atan a ...))
(define-syntax-rule (FLOATsqrt a ...)  (sqrt a ...))
(define-syntax-rule (FLOATmin a ...)   (min  a ...))
(define-syntax-rule (FLOATmax a ...)   (max  a ...))
(define-syntax-rule (FLOATround a ...) (round a ...))
(define-syntax-rule (FLOATinexact->exact a ...) (inexact->exact a ...))

(define-syntax-rule (GENERIC+ a ...) (+ a ...))
(define-syntax-rule (GENERIC- a ...) (- a ...))
(define-syntax-rule (GENERIC* a ...) (* a ...))
(define-syntax-rule (GENERIC/ a ...) (/ a ...))
(define-syntax-rule (GENERICquotient a ...)  (quotient a ...))
(define-syntax-rule (GENERICremainder a ...) (remainder a ...))
(define-syntax-rule (GENERICmodulo a ...)    (modulo a ...))
(define-syntax-rule (GENERIC= a ...)    (= a ...))
(define-syntax-rule (GENERIC< a ...)    (< a ...))
(define-syntax-rule (GENERIC<= a ...)   (<= a ...))
(define-syntax-rule (GENERIC> a ...)    (> a ...))
(define-syntax-rule (GENERIC>= a ...)   (>= a ...))
(define-syntax-rule (GENERICexpt a ...) (expt a ...))

;;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (let ((run (apply run-maker args)))
    (let ((result (time (run-bench name count ok? run))))
      (if (not (ok? result))
        (begin
          (display "*** wrong result ***")
          (newline)
          (display "*** got: ")
          (write result)
          (newline))))))
