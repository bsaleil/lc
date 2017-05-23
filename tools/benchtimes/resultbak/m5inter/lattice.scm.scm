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

;;; LATTICE -- Obtained from Andrew Wright.

; Given a comparison routine that returns one of
;       less
;       more
;       equal
;       uncomparable
; return a new comparison routine that applies to sequences.
(define lexico
    (lambda (base)
        (define lex-fixed
            (lambda (fixed lhs rhs)
                (define check
                    (lambda (lhs rhs)
                        (if (null? lhs)
                            fixed
                            (let ((probe
                                        (base (car lhs)
                                            (car rhs))))
                                (if (or (eq? probe 'equal)
                                        (eq? probe fixed))
                                    (check (cdr lhs)
                                        (cdr rhs))
                                    'uncomparable)))))
                (check lhs rhs)))
        (define lex-first
            (lambda (lhs rhs)
                (if (null? lhs)
                    'equal
                    (let ((probe
                                (base (car lhs)
                                    (car rhs))))
                        (case probe
                            ((less more)
                                (lex-fixed probe
                                    (cdr lhs)
                                    (cdr rhs)))
                            ((equal)
                                (lex-first (cdr lhs)
                                    (cdr rhs)))
                            ((uncomparable)
                                'uncomparable))))))
        lex-first))

(define (make-lattice elem-list cmp-func)
    (cons elem-list cmp-func))

(define lattice->elements car)

(define lattice->cmp cdr)

; Select elements of a list which pass some test.
(define zulu-select
    (lambda (test lst)
        (define select-a
            (lambda (ac lst)
                (if (null? lst)
                    (reverse! ac)
                    (select-a
                        (let ((head (car lst)))
                            (if (test head)
                                (cons head ac)
                                ac))
                        (cdr lst)))))
        (select-a '() lst)))

(define reverse!
    (letrec ((rotate
                (lambda (fo fum)
                    (let ((next (cdr fo)))
                        (set-cdr! fo fum)
                        (if (null? next)
                            fo
                            (rotate next fo))))))
        (lambda (lst)
            (if (null? lst)
                '()
                (rotate lst '())))))

; Select elements of a list which pass some test and map a function
; over the result.  Note, only efficiency prevents this from being the
; composition of select and map.
(define select-map
    (lambda (test func lst)
        (define select-a
            (lambda (ac lst)
                (if (null? lst)
                    (reverse! ac)
                    (select-a
                        (let ((head (car lst)))
                            (if (test head)
                                (cons (func head)
                                    ac)
                                ac))
                        (cdr lst)))))
        (select-a '() lst)))



; This version of map-and tail-recurses on the last test.
(define map-and
    (lambda (proc lst)
        (if (null? lst)
            #t
            (letrec ((drudge
                        (lambda (lst)
                            (let ((rest (cdr lst)))
                                (if (null? rest)
                                    (proc (car lst))
                                    (and (proc (car lst))
                                        (drudge rest)))))))
                (drudge lst)))))

(define (maps-1 source target pas new)
    (let ((scmp (lattice->cmp source))
            (tcmp (lattice->cmp target)))
        (let ((less
                    (select-map
                        (lambda (p)
                            (eq? 'less
                                (scmp (car p) new)))
                        cdr
                        pas))
                (more
                    (select-map
                        (lambda (p)
                            (eq? 'more
                                (scmp (car p) new)))
                        cdr
                        pas)))
            (zulu-select
                (lambda (t)
                    (and
                        (map-and
                            (lambda (t2)
                                (memq (tcmp t2 t) '(less equal)))
                            less)
                        (map-and
                            (lambda (t2)
                                (memq (tcmp t2 t) '(more equal)))
                            more)))
                (lattice->elements target)))))

(define (maps-rest source target pas rest to-1 to-collect)
    (if (null? rest)
        (to-1 pas)
        (let ((next (car rest))
                (rest (cdr rest)))
            (to-collect
                (map
                    (lambda (x)
                        (maps-rest source target
                            (cons
                                (cons next x)
                                pas)
                            rest
                            to-1
                            to-collect))
                    (maps-1 source target pas next))))))

(define (maps source target)
    (make-lattice
        (maps-rest source
            target
            '()
            (lattice->elements source)
            (lambda (x) (list (map cdr x)))
            (lambda (x) (apply append x)))
        (lexico (lattice->cmp target))))

(define (count-maps source target)
  (maps-rest source
             target
             '()
             (lattice->elements source)
             (lambda (x) 1)
             sum))

(define (sum lst)
  (if (null? lst)
      0
      (+ (car lst) (sum (cdr lst)))))

(define (run)
  (let* ((l2
            (make-lattice '(low high)
                (lambda (lhs rhs)
                    (case lhs
                        ((low)
                            (case rhs
                                ((low)
                                    'equal)
                                ((high)
                                    'less)
                                (else
                                    (fatal-error 'make-lattice "base" rhs))))
                        ((high)
                            (case rhs
                                ((low)
                                    'more)
                                ((high)
                                    'equal)
                                (else
                                    (fatal-error 'make-lattice "base" rhs))))
                        (else
                            (fatal-error 'make-lattice "base" lhs))))))
        (l3 (maps l2 l2))
        (l4 (maps l3 l3)))
    (count-maps l2 l2)
    (count-maps l3 l3)
    (count-maps l2 l3)
    (count-maps l3 l2)
    (count-maps l4 l4)))

(define (main)
  (run-benchmark
   "lattice"
   lattice-iters
   (lambda (result) (equal? result 120549))
   (lambda () (lambda () (run)))))

(main)
