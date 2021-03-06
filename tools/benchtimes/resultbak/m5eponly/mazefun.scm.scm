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
       (println "CPU time: "
         (+ (cdr (assoc "User time" (cdr r)))
            (cdr (assoc "Sys time" (cdr r)))))
       (map (lambda (el) (println (car el) ": " (cdr el))) (cdr r))
       r)))

(define (##lc-exec-stats thunk)
  (let* ((at-start (gambit$$##process-statistics))
         (result (thunk))
         (at-end (gambit$$##process-statistics)))
    (define (get-info msg idx)
      (cons msg
            (- (gambit$$##f64vector-ref at-end idx)
            (gambit$$##f64vector-ref at-start idx))))
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

(define ###TIME_BEFORE### 0)
(define ###TIME_AFTER###  0)

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

;;; MAZEFUN -- Constructs a maze in a purely functional way,
;;; written by Marc Feeley.

(define foldr
  (lambda (f base lst)

    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (car lst) (foldr-aux (cdr lst))))))

    (foldr-aux lst)))

(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define for
  (lambda (lo hi f)

    (define for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            '())))

    (for-aux lo)))

(define concat
  (lambda (lists)
    (foldr append '() lists)))

(define list-read
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read (cdr lst) (- i 1)))))

(define list-write
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write (cdr lst) (- i 1) val)))))

(define list-remove-pos
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos (cdr lst) (- i 1))))))

(define duplicates?
  (lambda (lst)
    (if (null? lst)
        #f
        (or (member (car lst) (cdr lst))
            (duplicates? (cdr lst))))))

(define make-matrix
  (lambda (n m init)
    (for 0 n (lambda (i) (for 0 m (lambda (j) (init i j)))))))

(define matrix-read
  (lambda (mat i j)
    (list-read (list-read mat i) j)))

(define matrix-write
  (lambda (mat i j val)
    (list-write mat i (list-write (list-read mat i) j val))))

(define matrix-size
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(define matrix-map
  (lambda (f mat)
    (map (lambda (lst) (map f lst)) mat)))

(define initial-random 0)

(define next-random
  (lambda (current-random)
    (remainder (+ (* current-random 3581) 12751) 131072)))

(define shuffle
  (lambda (lst)
    (shuffle-aux lst initial-random)))

(define shuffle-aux
  (lambda (lst current-random)
    (if (null? lst)
        '()
        (let ((new-random (next-random current-random)))
          (let ((i (modulo new-random (length lst))))
            (cons (list-read lst i)
                  (shuffle-aux (list-remove-pos lst i)
                               new-random)))))))

(define make-maze
  (lambda (n m) ; n and m must be odd
    (if (not (and (odd? n) (odd? m)))
        'error
        (let ((cave
               (make-matrix n m (lambda (i j)
                                  (if (and (even? i) (even? j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat
                (for 0 n (lambda (i)
                           (concat
                            (for 0 m (lambda (j)
                                       (if (equal? (even? i) (even? j))
                                           '()
                                           (list (cons i j)))))))))))
          (cave-to-maze (pierce-randomly (shuffle possible-holes) cave))))))

(define cave-to-maze
  (lambda (cave)
    (matrix-map (lambda (x) (if x '_ '*)) cave)))

(define pierce
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (matrix-write cave i j pos))))

(define pierce-randomly
  (lambda (possible-holes cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (pierce-randomly (cdr possible-holes)
                           (try-to-pierce hole cave))))))

(define try-to-pierce
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities pos cave)))
        (if (duplicates?
             (map (lambda (nc) (matrix-read cave (car nc) (cdr nc))) ncs))
            cave
            (pierce pos
                    (foldl (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

(define change-cavity
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux cave pos new-cavity-id (matrix-read cave i j)))))

(define change-cavity-aux
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux c nc new-cavity-id old-cavity-id))
                   (matrix-write cave i j new-cavity-id)
                   (neighboring-cavities pos cave))
            cave)))))

(define neighboring-cavities
  (lambda (pos cave)
    (let ((size (matrix-size cave)))
      (let ((n (car size)) (m (cdr size)))
        (let ((i (car pos)) (j (cdr pos)))
          (append (if (and (> i 0) (matrix-read cave (- i 1) j))
                      (list (cons (- i 1) j))
                      '())
                  (if (and (< i (- n 1)) (matrix-read cave (+ i 1) j))
                      (list (cons (+ i 1) j))
                      '())
                  (if (and (> j 0) (matrix-read cave i (- j 1)))
                      (list (cons i (- j 1)))
                      '())
                  (if (and (< j (- m 1)) (matrix-read cave i (+ j 1)))
                      (list (cons i (+ j 1)))
                      '())))))))

(define expected-result
  '((_ * _ _ _ _ _ _ _ _ _)
    (_ * * * * * * * _ * *)
    (_ _ _ * _ _ _ * _ _ _)
    (_ * _ * _ * _ * _ * _)
    (_ * _ _ _ * _ * _ * _)
    (* * _ * * * * * _ * _)
    (_ * _ _ _ _ _ _ _ * _)
    (_ * _ * _ * * * * * *)
    (_ _ _ * _ _ _ _ _ _ _)
    (_ * * * * * * * _ * *)
    (_ * _ _ _ _ _ _ _ _ _)))

(define (main . args)
  (run-benchmark
    "mazefun"
    mazefun-iters
    (lambda (result)
      (equal? result expected-result))
    (lambda (n m) (lambda () (make-maze n m)))
    11
    11))

(main)
