(define ###TIME_BEFORE### 0)
(define ###TIME_AFTER###  0)

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (let ((run (apply run-maker args)))
    (set! ###TIME_BEFORE### ($$sys-clock-gettime-ns))
    (let ((result (run-bench name count ok? run)))
      (set! ###TIME_AFTER### ($$sys-clock-gettime-ns))
      (let ((ms (/ (- ###TIME_AFTER### ###TIME_BEFORE###) 1000000)))
        (print ms)
        (println " ms real time")
        (if (not (ok? result))
          (begin
            (display "*** wrong result ***")
            (newline)
            (display "*** got: ")
            (write result)
            (newline)))))))

(define BENCH_IN_FILE_PATH "/home/bapt/Bureau/these/lazy-comp/tools/benchtimes/bench/")
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

;;; NQUEENS -- Compute number of solutions to 8-queens problem.

(define trace? #f)

(define (nqueens n)

  (define (_1-to n)
    (let loop ((i n) (l '()))
      (if (= i 0) l (loop (- i 1) (cons i l)))))

  (define (my-try x y z)
    (if (null? x)
      (if (null? y)
        (begin (if trace? (begin (write z) (newline))) 1)
        0)
      (+ (if (ok? (car x) 1 z)
           (my-try (append (cdr x) y) '() (cons (car x) z))
           0)
         (my-try (cdr x) (cons (car x) y) z))))

  (define (ok? row dist placed)
    (if (null? placed)
      #t
      (and (not (= (car placed) (+ row dist)))
           (not (= (car placed) (- row dist)))
           (ok? row (+ dist 1) (cdr placed)))))

  (my-try (_1-to n) '() '()))

(define (main)
  (run-benchmark
   "nqueens"
   nqueens-iters
   (lambda (result) (equal? result 92))
   (lambda (n) (lambda () (nqueens n)))
   8))

(main)
