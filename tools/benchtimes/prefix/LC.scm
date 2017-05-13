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
