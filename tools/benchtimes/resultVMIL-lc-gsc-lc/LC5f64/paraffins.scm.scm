;;------------------------------------------------------------------------------
;; Macros

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

(def-macro (FLOATvector-const . lst)   `',(list->f64vector lst))
(def-macro (FLOATvector? x)            `(f64vector? ,x))
(def-macro (FLOATvector . lst)         `(f64vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(make-f64vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(f64vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(f64vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(f64vector-length ,v))

(def-macro (nuc-const . lst)
  `',(list->vector
       (map (lambda (x)
              (if (vector? x)
                (list->f64vector (vector->list x))
                x))
            lst)))

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

;;; PARAFFINS -- Compute how many paraffins exist with N carbon atoms.

(define (gen n)
  (let* ((n/2 (quotient n 2))
         (radicals (make-vector (+ n/2 1) '(H))))

    (define (rads-of-size n)
      (let loop1 ((ps
                   (three-partitions (- n 1)))
                  (lst
                   '()))
        (if (null? ps)
          lst
          (let* ((p (car ps))
                 (nc1 (vector-ref p 0))
                 (nc2 (vector-ref p 1))
                 (nc3 (vector-ref p 2)))
            (let loop2 ((rads1
                         (vector-ref radicals nc1))
                        (lst
                         (loop1 (cdr ps)
                                lst)))
              (if (null? rads1)
                lst
                (let loop3 ((rads2
                             (if (= nc1 nc2)
                               rads1
                               (vector-ref radicals nc2)))
                            (lst
                             (loop2 (cdr rads1)
                                    lst)))
                  (if (null? rads2)
                    lst
                    (let loop4 ((rads3
                                 (if (= nc2 nc3)
                                   rads2
                                   (vector-ref radicals nc3)))
                                (lst
                                 (loop3 (cdr rads2)
                                        lst)))
                      (if (null? rads3)
                        lst
                        (cons (vector 'C
                                      (car rads1)
                                      (car rads2)
                                      (car rads3))
                              (loop4 (cdr rads3)
                                     lst))))))))))))

    (define (bcp-generator j)
      (if (odd? j)
        '()
        (let loop1 ((rads1
                     (vector-ref radicals (quotient j 2)))
                    (lst
                     '()))
          (if (null? rads1)
            lst
            (let loop2 ((rads2
                         rads1)
                        (lst
                         (loop1 (cdr rads1)
                                lst)))
              (if (null? rads2)
                lst
                (cons (vector 'BCP
                              (car rads1)
                              (car rads2))
                      (loop2 (cdr rads2)
                             lst))))))))

    (define (ccp-generator j)
      (let loop1 ((ps
                   (four-partitions (- j 1)))
                  (lst
                   '()))
        (if (null? ps)
          lst
          (let* ((p (car ps))
                 (nc1 (vector-ref p 0))
                 (nc2 (vector-ref p 1))
                 (nc3 (vector-ref p 2))
                 (nc4 (vector-ref p 3)))
            (let loop2 ((rads1
                         (vector-ref radicals nc1))
                        (lst
                         (loop1 (cdr ps)
                                lst)))
              (if (null? rads1)
                lst
                (let loop3 ((rads2
                             (if (= nc1 nc2)
                               rads1
                               (vector-ref radicals nc2)))
                            (lst
                             (loop2 (cdr rads1)
                                    lst)))
                  (if (null? rads2)
                    lst
                    (let loop4 ((rads3
                                 (if (= nc2 nc3)
                                   rads2
                                   (vector-ref radicals nc3)))
                                (lst
                                 (loop3 (cdr rads2)
                                        lst)))
                      (if (null? rads3)
                        lst
                        (let loop5 ((rads4
                                     (if (= nc3 nc4)
                                       rads3
                                       (vector-ref radicals nc4)))
                                    (lst
                                     (loop4 (cdr rads3)
                                            lst)))
                          (if (null? rads4)
                            lst
                            (cons (vector 'CCP
                                          (car rads1)
                                          (car rads2)
                                          (car rads3)
                                          (car rads4))
                                  (loop5 (cdr rads4)
                                         lst))))))))))))))

    (let loop ((i 1))
      (if (> i n/2)
        (vector (bcp-generator n)
                (ccp-generator n))
        (begin
          (vector-set! radicals i (rads-of-size i))
          (loop (+ i 1)))))))

(define (three-partitions m)
  (let loop1 ((lst '())
              (nc1 (quotient m 3)))
    (if (< nc1 0)
      lst
      (let loop2 ((lst lst)
                  (nc2 (quotient (- m nc1) 2)))
        (if (< nc2 nc1)
          (loop1 lst
                 (- nc1 1))
          (loop2 (cons (vector nc1 nc2 (- m (+ nc1 nc2))) lst)
                 (- nc2 1)))))))

(define (four-partitions m)
  (let loop1 ((lst '())
              (nc1 (quotient m 4)))
    (if (< nc1 0)
      lst
      (let loop2 ((lst lst)
                  (nc2 (quotient (- m nc1) 3)))
        (if (< nc2 nc1)
          (loop1 lst
                 (- nc1 1))
          (let ((start (max nc2 (- (quotient (+ m 1) 2) (+ nc1 nc2)))))
            (let loop3 ((lst lst)
                        (nc3 (quotient (- m (+ nc1 nc2)) 2)))
              (if (< nc3 start)
                (loop2 lst (- nc2 1))
                (loop3 (cons (vector nc1 nc2 nc3 (- m (+ nc1 (+ nc2 nc3)))) lst)
                       (- nc3 1))))))))))

(define (nb n)
  (let ((x (gen n)))
    (+ (length (vector-ref x 0))
       (length (vector-ref x 1)))))

(define (main . args)
  (run-benchmark
   "paraffins"
   paraffins-iters
   (lambda (result) (equal? result 24894))
   (lambda (n) (lambda () (nb n)))
   17))

(main)
