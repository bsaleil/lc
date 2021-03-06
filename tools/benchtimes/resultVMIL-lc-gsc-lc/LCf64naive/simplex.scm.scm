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

;;; SIMPLEX -- Simplex algorithm.

(define (matrix-rows a) (vector-length a))
(define (matrix-columns a) (FLOATvector-length (vector-ref a 0)))
(define (matrix-ref a i j) (FLOATvector-ref (vector-ref a i) j))
(define (matrix-set! a i j x) (FLOATvector-set! (vector-ref a i) j x))

(define (fuck-up)
  (fatal-error "This shouldn't happen"))

(define (simplex a m1 m2 m3)
 (define *epsilon* 1e-6)
 (if (not (and (>= m1 0)
               (>= m2 0)
               (>= m3 0)
               (= (matrix-rows a) (+ m1 m2 m3 2))))
  (fuck-up))
 (let* ((m12 (+ m1 m2 1))
        (m (- (matrix-rows a) 2))
        (n (- (matrix-columns a) 1))
        (l1 (make-vector n))
        (l2 (make-vector m))
        (l3 (make-vector m2))
        (nl1 n)
        (iposv (make-vector m))
        (izrov (make-vector n))
        (ip 0)
        (kp 0)
        (bmax 0.0)
        (one? #f)
        (pass2? #t))
  (define (simp1 mm abs?)
   (set! kp (vector-ref l1 0))
   (set! bmax (matrix-ref a mm kp))
   (do ((k 1 (+ k 1))) ((>= k nl1))
    (if (FLOATpositive?
         (if abs?
             (FLOAT- (FLOATabs (matrix-ref a mm (vector-ref l1 k)))
                     (FLOATabs bmax))
             (FLOAT- (matrix-ref a mm (vector-ref l1 k)) bmax)))
        (begin
         (set! kp (vector-ref l1 k))
         (set! bmax (matrix-ref a mm (vector-ref l1 k)))))))
  (define (simp2)
   (set! ip 0)
   (let ((q1 0.0)
         (flag? #f))
    (do ((i 0 (+ i 1))) ((= i m))
     (if flag?
         (if (FLOAT< (matrix-ref a (vector-ref l2 i) kp) (FLOAT- *epsilon*))
             (begin
              (let ((q (FLOAT/ (FLOAT- (matrix-ref a (vector-ref l2 i) 0))
                               (matrix-ref a (vector-ref l2 i) kp))))
               (cond
                ((FLOAT< q q1)
                 (set! ip (vector-ref l2 i))
                 (set! q1 q))
                ((FLOAT= q q1)
                 (let ((qp 0.0)
                       (q0 0.0))
                  (let loop ((k 1))
                   (if (<= k n)
                       (begin
                        (set! qp (FLOAT/ (FLOAT- (matrix-ref a ip k))
                                         (matrix-ref a ip kp)))
                        (set! q0 (FLOAT/ (FLOAT-
                                           (matrix-ref a (vector-ref l2 i) k))
                                         (matrix-ref a (vector-ref l2 i) kp)))
                        (if (FLOAT= q0 qp)
                            (loop (+ k 1))))))
                  (if (FLOAT< q0 qp)
                      (set! ip (vector-ref l2 i)))))))))
         (if (FLOAT< (matrix-ref a (vector-ref l2 i) kp) (FLOAT- *epsilon*))
             (begin
              (set! q1 (FLOAT/ (FLOAT- (matrix-ref a (vector-ref l2 i) 0))
                               (matrix-ref a (vector-ref l2 i) kp)))
              (set! ip (vector-ref l2 i))
              (set! flag? #t)))))))
  (define (simp3 one?)
   (let ((piv (FLOAT/ (matrix-ref a ip kp))))
    (do ((ii 0 (+ ii 1))) ((= ii (+ m (if one? 2 1))))
     (if (not (= ii ip))
         (begin
          (matrix-set! a ii kp (FLOAT* piv (matrix-ref a ii kp)))
          (do ((kk 0 (+ kk 1))) ((= kk (+ n 1)))
           (if (not (= kk kp))
               (matrix-set! a ii kk (FLOAT- (matrix-ref a ii kk)
                                            (FLOAT* (matrix-ref a ip kk)
                                                    (matrix-ref a ii kp)))))))))
    (do ((kk 0 (+ kk 1))) ((= kk (+ n 1)))
     (if (not (= kk kp))
         (matrix-set! a ip kk (FLOAT* (FLOAT- piv) (matrix-ref a ip kk)))))
    (matrix-set! a ip kp piv)))
  (do ((k 0 (+ k 1))) ((= k n))
   (vector-set! l1 k (+ k 1))
   (vector-set! izrov k k))
  (do ((i 0 (+ i 1))) ((= i m))
   (if (FLOATnegative? (matrix-ref a (+ i 1) 0))
       (fuck-up))
   (vector-set! l2 i (+ i 1))
   (vector-set! iposv i (+ n i)))
  (do ((i 0 (+ i 1))) ((= i m2)) (vector-set! l3 i #t))
  (if (positive? (+ m2 m3))
      (begin
       (do ((k 0 (+ k 1))) ((= k (+ n 1)))
        (do ((i (+ m1 1) (+ i 1)) (sum 0.0 (FLOAT+ sum (matrix-ref a i k))))
          ((> i m) (matrix-set! a (+ m 1) k (FLOAT- sum)))))
       (let loop ()
        (simp1 (+ m 1) #f)
        (cond
         ((FLOAT<= bmax *epsilon*)
          (cond ((FLOAT< (matrix-ref a (+ m 1) 0) (FLOAT- *epsilon*))
                 (set! pass2? #f))
                ((FLOAT<= (matrix-ref a (+ m 1) 0) *epsilon*)
                 (let loop ((ip1 m12))
                  (if (<= ip1 m)
                      (cond ((= (vector-ref iposv (- ip1 1)) (+ ip n -1))
                             (simp1 ip1 #t)
                             (cond ((FLOATpositive? bmax)
                                    (set! ip ip1)
                                    (set! one? #t))
                                   (else
                                    (loop (+ ip1 1)))))
                            (else
                             (loop (+ ip1 1))))
                      (do ((i (+ m1 1) (+ i 1))) ((>= i m12))
                       (if (vector-ref l3 (- i (+ m1 1)))
                           (do ((k 0 (+ k 1))) ((= k (+ n 1)))
                            (matrix-set! a i k (FLOAT- (matrix-ref a i k)))))))))
                (else (simp2) (if (zero? ip) (set! pass2? #f) (set! one? #t)))))
         (else (simp2) (if (zero? ip) (set! pass2? #f) (set! one? #t))))
        (if one?
            (begin
             (set! one? #f)
             (simp3 #t)
             (cond
              ((>= (vector-ref iposv (- ip 1)) (+ n m12 -1))
               (let loop ((k 0))
                (cond
                 ((and (< k nl1) (not (= kp (vector-ref l1 k))))
                  (loop (+ k 1)))
                 (else
                  (set! nl1 (- nl1 1))
                  (do ((is k (+ is 1))) ((>= is nl1))
                   (vector-set! l1 is (vector-ref l1 (+ is 1))))
                  (matrix-set! a (+ m 1) kp (FLOAT+ (matrix-ref a (+ m 1) kp) 1.0))
                  (do ((i 0 (+ i 1))) ((= i (+ m 2)))
                   (matrix-set! a i kp (FLOAT- (matrix-ref a i kp))))))))
              ((and (>= (vector-ref iposv (- ip 1)) (+ n m1))
                    (vector-ref l3 (- (vector-ref iposv (- ip 1)) (+ m1 n))))
               (vector-set! l3 (- (vector-ref iposv (- ip 1)) (+ m1 n)) #f)
               (matrix-set! a (+ m 1) kp (FLOAT+ (matrix-ref a (+ m 1) kp) 1.0))
               (do ((i 0 (+ i 1))) ((= i (+ m 2)))
                (matrix-set! a i kp (FLOAT- (matrix-ref a i kp))))))
             (let ((t (vector-ref izrov (- kp 1))))
              (vector-set! izrov (- kp 1) (vector-ref iposv (- ip 1)))
              (vector-set! iposv (- ip 1) t))
             (loop))))))
  (and pass2?
       (let loop ()
        (simp1 0 #f)
        (cond
         ((FLOATpositive? bmax)
          (simp2)
          (cond ((zero? ip) #t)
                (else (simp3 #f)
                      (let ((t (vector-ref izrov (- kp 1))))
                       (vector-set! izrov (- kp 1) (vector-ref iposv (- ip 1)))
                       (vector-set! iposv (- ip 1) t))
                      (loop))))
         (else (list iposv izrov)))))))

(define (test)
 (simplex (vector (FLOATvector 0.0 1.0 1.0 3.0 -0.5)
                  (FLOATvector 740.0 -1.0 0.0 -2.0 0.0)
                  (FLOATvector 0.0 0.0 -2.0 0.0 7.0)
                  (FLOATvector 0.5 0.0 -1.0 1.0 -2.0)
                  (FLOATvector 9.0 -1.0 -1.0 -1.0 -1.0)
                  (FLOATvector 0.0 0.0 0.0 0.0 0.0))
          2 1 1))

(define (main . args)
  (run-benchmark
    "simplex"
    simplex-iters
    (lambda (result) (equal? result '(#(4 1 3 2) #(0 5 7 6))))
    (lambda () (lambda () (test)))))

(main)
