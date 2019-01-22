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


;;; The Computer Language Benchmarks Game
;;; http://shootout.alioth.debian.org/
;;;
;;; contributed by Anthony Borla
;;; modified by Graham Fawcett

;;-----------------------------------------------------------------------------
;; use vector to implement body type instead of record for LC
(define (make-body x y z vx vy vz mass) (FLOATvector x y z vx vy vz mass))
(define (body-x body)    (FLOATvector-ref body 0))
(define (body-y body)    (FLOATvector-ref body 1))
(define (body-z body)    (FLOATvector-ref body 2))
(define (body-vx body)   (FLOATvector-ref body 3))
(define (body-vy body)   (FLOATvector-ref body 4))
(define (body-vz body)   (FLOATvector-ref body 5))
(define (body-mass body) (FLOATvector-ref body 6))
(define (body-x-set! body v)    (FLOATvector-set! body 0 v))
(define (body-y-set! body v)    (FLOATvector-set! body 1 v))
(define (body-z-set! body v)    (FLOATvector-set! body 2 v))
(define (body-vx-set! body v)   (FLOATvector-set! body 3 v))
(define (body-vy-set! body v)   (FLOATvector-set! body 4 v))
(define (body-vz-set! body v)   (FLOATvector-set! body 5 v))
(define (body-mass-set! body v) (FLOATvector-set! body 6 v))
;;-----------------------------------------------------------------------------

;; define planetary masses, initial positions & velocity

(define +pi+ 3.141592653589793)
(define +days-per-year+ 365.24)

(define +solar-mass+ (* 4 +pi+ +pi+))

;(define-record body x y z vx vy vz mass)

(define *sun*
  (make-body 0.0 0.0 0.0 0.0 0.0 0.0 +solar-mass+))

(define *jupiter*
  (make-body 4.84143144246472090
             -1.16032004402742839
             -1.03622044471123109e-1
             (* 1.66007664274403694e-3 +days-per-year+)
             (* 7.69901118419740425e-3 +days-per-year+)
             (* -6.90460016972063023e-5 +days-per-year+)
             (* 9.54791938424326609e-4 +solar-mass+)))

(define *saturn*
  (make-body 8.34336671824457987
             4.12479856412430479
             -4.03523417114321381e-1
             (* -2.76742510726862411e-3 +days-per-year+)
             (* 4.99852801234917238e-3 +days-per-year+)
             (* 2.30417297573763929e-5 +days-per-year+)
             (* 2.85885980666130812e-4 +solar-mass+)))

(define *uranus*
  (make-body 1.28943695621391310e1
             -1.51111514016986312e1
             -2.23307578892655734e-1
             (* 2.96460137564761618e-03 +days-per-year+)
             (* 2.37847173959480950e-03 +days-per-year+)
             (* -2.96589568540237556e-05 +days-per-year+)
             (*  4.36624404335156298e-05 +solar-mass+)))

(define *neptune*
  (make-body 1.53796971148509165e+01
             -2.59193146099879641e+01
             1.79258772950371181e-01
             (* 2.68067772490389322e-03 +days-per-year+)
             (* 1.62824170038242295e-03 +days-per-year+)
             (* -9.51592254519715870e-05 +days-per-year+)
             (* 5.15138902046611451e-05 +solar-mass+)))

;; -------------------------------
(define (offset-momentum system)
  (let loop-i ((i system) (px 0.0) (py 0.0) (pz 0.0))
    (if (null? i)
        (begin
          (body-vx-set! (car system) (/ (- px) +solar-mass+))
          (body-vy-set! (car system) (/ (- py) +solar-mass+))
          (body-vz-set! (car system) (/ (- pz) +solar-mass+)))
        (loop-i (cdr i)
      	  (+ px (* (body-vx (car i)) (body-mass (car i))))
      	  (+ py (* (body-vy (car i)) (body-mass (car i))))
      	  (+ pz (* (body-vz (car i)) (body-mass (car i))))))))

;; -------------------------------
(define (energy system)
  (let loop-o ((o system) (e 0.0))
      (if (null? o)
          e
          (let ((e (+ e (* 0.5 (body-mass (car o))
      		     (+ (* (body-vx (car o)) (body-vx (car o)))
      			(* (body-vy (car o)) (body-vy (car o)))
      			(* (body-vz (car o)) (body-vz (car o))))))))

            (let loop-i ((i (cdr o)) (e e))
      	(if (null? i)
      	    (loop-o (cdr o) e)
      	    (let* ((dx (- (body-x (car o)) (body-x (car i))))
      		   (dy (- (body-y (car o)) (body-y (car i))))
      		   (dz (- (body-z (car o)) (body-z (car i))))
      		   (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))
      	      (let ((e  (- e (/ (* (body-mass (car o)) (body-mass (car i))) distance))))
      		(loop-i (cdr i) e)))))))))

;; -------------------------------
(define (advance system dt)
  (let loop-o ((o system))
    (if (not (null? o))
      (begin
        (let loop-i ((i (cdr o)))
          (if (not (null? i))
            (let* ((o1 (car o))
        	   (i1 (car i))
        	   (dx (- (body-x o1) (body-x i1)))
        	   (dy (- (body-y o1) (body-y i1)))
        	   (dz (- (body-z o1) (body-z i1)))
        	   (distance (sqrt (+ (* dx dx) (* dy dy) (* dz dz))))
        	   (mag (/ dt (* distance distance distance)))
        	   (dxmag (* dx mag))
        	   (dymag (* dy mag))
        	   (dzmag (* dz mag))
        	   (om (body-mass o1))
        	   (im (body-mass i1)))
              (body-vx-set! o1 (- (body-vx o1) (* dxmag im)))
              (body-vy-set! o1 (- (body-vy o1) (* dymag im)))
              (body-vz-set! o1 (- (body-vz o1) (* dzmag im)))
              (body-vx-set! i1 (+ (body-vx i1) (* dxmag om)))
              (body-vy-set! i1 (+ (body-vy i1) (* dymag om)))
              (body-vz-set! i1 (+ (body-vz i1) (* dzmag om)))
              (loop-i (cdr i)))))
        (loop-o (cdr o)))))

  (let loop-o ((o system))
    (if (not (null? o))
      (let ((o1 (car o)))
        (body-x-set! o1 (+ (body-x o1) (* dt (body-vx o1))))
        (body-y-set! o1 (+ (body-y o1) (* dt (body-vy o1))))
        (body-z-set! o1 (+ (body-z o1) (* dt (body-vz o1))))
        (loop-o (cdr o))))))

;; -------------------------------

(define (run n)
  (let ((system (list *sun* *jupiter* *saturn* *uranus* *neptune*)))
    (offset-momentum system)
    (let ((before (energy system)))
      (do ((i 1 (+ i 1)))
          ((< n i))
        (advance system 0.01))
      (let ((after (energy system)))
        (cons before after)))))

(define (main . args)
  (run-benchmark
    "nbody"
    nbody-iters
    (lambda (result)
      (and (< (+ 0.169075164 (car result)) 0.000000001)
           (< (+ 0.169086185 (cdr result)) 0.000000001)))
    (lambda (data) (lambda () (run data)))
    1000000))

(main)
