;;------------------------------------------------------------------------------
;; Macros

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
(def-macro (FLOATvector? x)            `(vector? ,x))
(def-macro (FLOATvector . lst)         `(vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(make-vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(vector-length ,v))

(def-macro (nuc-const . lst)
`',(list->vector lst))

(def-macro (FLOAT+ . lst) `(+ ,@lst))
(def-macro (FLOAT- . lst) `(- ,@lst))
(def-macro (FLOAT* . lst) `(* ,@lst))
(def-macro (FLOAT/ . lst) `(/ ,@lst))
(def-macro (FLOAT= . lst)  `(= ,@lst))
(def-macro (FLOAT< . lst)  `(< ,@lst))
(def-macro (FLOAT<= . lst) `(<= ,@lst))
(def-macro (FLOAT> . lst)  `(> ,@lst))
(def-macro (FLOAT>= . lst) `(>= ,@lst))
(def-macro (FLOATnegative? . lst) `(negative? ,@lst))
(def-macro (FLOATpositive? . lst) `(positive? ,@lst))
(def-macro (FLOATzero? . lst)     `(zero? ,@lst))
(def-macro (FLOATabs . lst) `(abs ,@lst))
(def-macro (FLOATsin . lst) `(sin ,@lst))
(def-macro (FLOATcos . lst) `(cos ,@lst))
(def-macro (FLOATatan . lst) `(atan ,@lst))
(def-macro (FLOATsqrt . lst) `(sqrt ,@lst))
(def-macro (FLOATmin . lst) `(min ,@lst))
(def-macro (FLOATmax . lst) `(max ,@lst))
(def-macro (FLOATround . lst) `(round ,@lst))
(def-macro (FLOATinexact->exact . lst) `(inexact->exact ,@lst))

(def-macro (GENERIC+ . lst) `(+ ,@lst))
(def-macro (GENERIC- . lst) `(- ,@lst))
(def-macro (GENERIC* . lst) `(* ,@lst))
(def-macro (GENERIC/ . lst) `(/ ,@lst))
(def-macro (GENERICquotient . lst)  `(quotient ,@lst))
(def-macro (GENERICremainder . lst) `(remainder ,@lst))
(def-macro (GENERICmodulo . lst)    `(modulo ,@lst))
(def-macro (GENERIC= . lst)  `(= ,@lst))
(def-macro (GENERIC< . lst)  `(< ,@lst))
(def-macro (GENERIC<= . lst) `(<= ,@lst))
(def-macro (GENERIC> . lst)  `(> ,@lst))
(def-macro (GENERIC>= . lst) `(>= ,@lst))
(def-macro (GENERICexpt . lst) `(expt ,@lst))

;;------------------------------------------------------------------------------
;; Functions used by LC to get time info

(def-macro (##lc-time expr)
  (let ((sym (gensym)))
    `(let ((r (##lc-exec-stats (lambda () ,expr))))
       (##print-perm-string "CPU time: ")
       (##print-double (+ (cdr (assoc "User time" (cdr r)))
                          (cdr (assoc "Sys time" (cdr r)))))
       (##print-perm-string "\n")
       (##print-perm-string "GC CPU time: ")
       (##print-double (+ (cdr (assoc "GC user time" (cdr r)))
                          (cdr (assoc "GC sys time" (cdr r)))))
       (##print-perm-string "\n")

       (map (lambda (el)
              (##print-perm-string (car el))
              (##print-perm-string ": ")
              (##print-double (cdr el))
              (##print-perm-string "\n"))
            (cdr r))
       r)))

(define (##lc-exec-stats thunk)
  (let* ((at-start (##process-statistics))
         (result (thunk))
         (at-end (##process-statistics)))
    (define (get-info msg idx)
      (cons msg
            (- (f64vector-ref at-end idx)
               (f64vector-ref at-start idx))))
    (list
      result
      (get-info "User time" 0)
      (get-info "Sys time" 1)
      (get-info "Real time" 2)
      (get-info "GC user time" 3)
      (get-info "GC sys time" 4)
      (get-info "GC real time" 5)
      (get-info "Nb gcs" 6))))

;;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (let ((run (apply run-maker args)))
    (let ((result (car (##lc-time (run-bench name count ok? run)))))
      (if (not (ok? result))
        (begin
          (display "*** wrong result ***")
          (newline)
          (display "*** got: ")
          (write result)
          (newline))))))

; Gabriel benchmarks
(define boyer-iters        20)
(define browse-iters      600)
(define cpstak-iters     1000)
(define ctak-iters        100)
(define dderiv-iters  2000000)
(define deriv-iters   2000000)
(define destruc-iters     500)
(define diviter-iters 1000000)
(define divrec-iters  1000000)
(define puzzle-iters      100)
(define tak-iters        2000)
(define takl-iters        300)
(define trav1-iters       100)
(define trav2-iters        20)
(define triangl-iters      10)

; Kernighan and Van Wyk benchmarks
(define ack-iters          10)
(define array1-iters        1)
(define cat-iters           1)
(define string-iters       10)
(define sum1-iters         10)
(define sumloop-iters      10)
(define tail-iters          1)
(define wc-iters            1)

; C benchmarks
(define fft-iters        2000)
(define fib-iters           5)
(define fibfp-iters         2)
(define mbrot-iters       100)
(define nucleic-iters       5)
(define pnpoly-iters   100000)
(define sum-iters       20000)
(define sumfp-iters     20000)
(define tfib-iters         20)

; Other benchmarks
(define conform-iters      40)
(define dynamic-iters      20)
(define earley-iters      200)
(define fibc-iters        500)
(define graphs-iters      300)
(define lattice-iters       1)
(define matrix-iters      400)
(define maze-iters       4000)
(define mazefun-iters    1000)
(define nqueens-iters    2000)
(define paraffins-iters  1000)
(define peval-iters       200)
(define pi-iters            2)
(define primes-iters   100000)
(define ray-iters           5)
(define scheme-iters    20000)
(define simplex-iters  100000)
(define slatex-iters       20)
(define perm9-iters        10)
(define nboyer-iters      100)
(define sboyer-iters      100)
(define gcbench-iters       1)
(define compiler-iters    300)
(define nbody-iters         1)
(define fftrad4-iters       4)

;;; FFT - Fast Fourier Transform, translated from "Numerical Recipes in C"

(define (four1 data)
  (let ((n (FLOATvector-length data))
        (pi*2 6.28318530717959)) ; to compute the inverse, negate this value

    ; bit-reversal section

    (let loop1 ((i 0) (j 0))
      (if (< i n)
        (begin
          (if (< i j)
            (begin
              (let ((temp (FLOATvector-ref data i)))
                (FLOATvector-set! data i (FLOATvector-ref data j))
                (FLOATvector-set! data j temp))
              (let ((temp (FLOATvector-ref data (+ i 1))))
                (FLOATvector-set! data (+ i 1) (FLOATvector-ref data (+ j 1)))
                (FLOATvector-set! data (+ j 1) temp))))
          (let loop2 ((m (quotient n 2)) (j j))
            (if (and (>= m 2) (>= j m))
              (loop2 (quotient m 2) (- j m))
              (loop1 (+ i 2) (+ j m)))))))

    ; Danielson-Lanczos section

    (let loop3 ((mmax 2))
      (if (< mmax n)
        (let* ((theta
                (FLOAT/ pi*2 (exact->inexact mmax)))
               (wpr
                (let ((x (FLOATsin (FLOAT* 0.5 theta))))
                  (FLOAT* -2.0 (FLOAT* x x))))
               (wpi
                (FLOATsin theta)))
          (let loop4 ((wr 1.0) (wi 0.0) (m 0))
            (if (< m mmax)
              (begin
                (let loop5 ((i m))
                  (if (< i n)
                    (let* ((j
                            (+ i mmax))
                           (tempr
                            (FLOAT-
                              (FLOAT* wr (FLOATvector-ref data j))
                              (FLOAT* wi (FLOATvector-ref data (+ j 1)))))
                           (tempi
                            (FLOAT+
                              (FLOAT* wr (FLOATvector-ref data (+ j 1)))
                              (FLOAT* wi (FLOATvector-ref data j)))))
                      (FLOATvector-set! data j
                        (FLOAT- (FLOATvector-ref data i) tempr))
                      (FLOATvector-set! data (+ j 1)
                        (FLOAT- (FLOATvector-ref data (+ i 1)) tempi))
                      (FLOATvector-set! data i
                        (FLOAT+ (FLOATvector-ref data i) tempr))
                      (FLOATvector-set! data (+ i 1)
                        (FLOAT+ (FLOATvector-ref data (+ i 1)) tempi))
                      (loop5 (+ j mmax)));***))
                (loop4 (FLOAT+ (FLOAT- (FLOAT* wr wpr) (FLOAT* wi wpi)) wr)
                       (FLOAT+ (FLOAT+ (FLOAT* wi wpr) (FLOAT* wr wpi)) wi)
                       (+ m 2)))))
));******
          (loop3 (* mmax 2)))))))

(define data
  (FLOATmake-vector 1024 0.0))

(define (run data)
  (four1 data)
  (FLOATvector-ref data 0))

(define (main . args)
  (run-benchmark
    "fft"
    fft-iters
    (lambda (result) (equal? result 0.0))
    (lambda (data) (lambda () (run data)))
    data))

(main)
