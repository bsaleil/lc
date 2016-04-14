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

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (newline)
  (let* ((run (apply run-maker args))
         (result (run-bench name count ok? run) (current-output-port)))
    (if (not (ok? result))
      (begin
        (display "*** wrong result ***")
        (newline)
        (display "*** got: ")
        (write result)
        (newline)))))

(main)

;;; FIB -- A classic benchmark, computes fib(35) inefficiently.

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(define (main . args)
  (run-benchmark
    "fib"
    fib-iters
    (lambda (result) (equal? result 55))
    (lambda (n) (lambda () (fib n)))
    10))

(main)
