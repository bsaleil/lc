;;------------------------------------------------------------------------------
;; Macros

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

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

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (let ((run (apply run-maker args)))
    (let ((result (run-bench name count ok? run)))
      (if (not (ok? result))
        (begin
          (display "*** wrong result ***")
          (newline)
          (display "*** got: ")
          (write result)
          (newline))))))

(define mbrot-iters 100)

;;; MBROT -- Generation of Mandelbrot set fractal.

(define (count r i step x y)

  (let ((max-count 64)
        (radius^2  16.0))

    (let ((cr (+ r (* (exact->inexact x) step)))
          (ci (+ i (* (exact->inexact y) step))))

      (let loop ((zr cr)
                 (zi ci)
                 (c 0))
        (if (= c max-count)
          c
          (let ((zr^2 (* zr zr))
                (zi^2 (* zi zi)))
            (if (> (+ zr^2 zi^2) radius^2)
              c
              (let ((new-zr (+ (- zr^2 zi^2) cr))
                    (new-zi (+ (* 2.0 (* zr zi)) ci)))
                (loop new-zr new-zi (+ c 1))))))))))

(define (mbrot matrix r i step n)
  (let loop1 ((y (- n 1)))
    (if (>= y 0)
      (let loop2 ((x (- n 1)))
        (if (>= x 0)
          (begin
            (vector-set! (vector-ref matrix x) y (count r i step x y))
            (loop2 (- x 1)))
          (loop1 (- y 1)))))))

(define (test n)
  (let ((matrix (make-vector n)))
    (let loop ((i (- n 1)))
      (if (>= i 0)
        (begin
          (vector-set! matrix i (make-vector n))
          (loop (- i 1)))))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

(define (main . args)
  (run-benchmark
    "mbrot"
    mbrot-iters
    (lambda (result) (equal? result 5))
    (lambda (n) (lambda () (test n)))
    75))

(main)
