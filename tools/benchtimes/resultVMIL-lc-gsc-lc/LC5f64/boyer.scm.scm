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

;;; BOYER -- Logic programming benchmark, originally written by Bob Boyer.
;;; Fairly CONS intensive.

(define (lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (car x)))
        (if (eq? (car pair) key)
          pair
          (loop (cdr x)))))))

(define properties '())

(define (get key1 key2)
  (let ((x (lookup key1 properties)))
    (if x
      (let ((y (lookup key2 (cdr x))))
        (if y
          (cdr y)
          #f))
      #f)))

(define (put key1 key2 val)
  (let ((x (lookup key1 properties)))
    (if x
      (let ((y (lookup key2 (cdr x))))
        (if y
          (set-cdr! y val)
          (set-cdr! x (cons (cons key2 val) (cdr x)))))
      (set! properties
        (cons (list key1 (cons key2 val)) properties)))))

(define unify-subst '())

(define (add-lemma term)
  (cond ((and (pair? term)
              (eq? (car term)
                   (quote equal))
              (pair? (cadr term)))
         (put (car (cadr term))
              (quote lemmas)
              (cons term (get (car (cadr term)) (quote lemmas)))))
        (else (fatal-error "ADD-LEMMA did not like term:  " term))))

(define (add-lemma-lst lst)
  (cond ((null? lst)
         #t)
        (else (add-lemma (car lst))
              (add-lemma-lst (cdr lst)))))

(define (apply-subst alist term)
  (cond ((not (pair? term))
         (cond ((assq term alist) => cdr)
               (else term)))
        (else (cons (car term)
                    (apply-subst-lst alist (cdr term))))))

(define (apply-subst-lst alist lst)
  (cond ((null? lst)
         '())
        (else (cons (apply-subst alist (car lst))
                    (apply-subst-lst alist (cdr lst))))))

(define (falsep x lst)
  (or (equal? x (quote (f)))
      (member x lst)))

(define (one-way-unify term1 term2)
  (begin (set! unify-subst '())
         (one-way-unify1 term1 term2)))

(define (one-way-unify1 term1 term2)
  (cond ((not (pair? term2))
         (cond ((assq term2 unify-subst) =>
                (lambda (x) (equal? term1 (cdr x))))
               (else (set! unify-subst (cons (cons term2 term1)
                                             unify-subst))
                     #t)))
        ((not (pair? term1))
         #f)
        ((eq? (car term1)
              (car term2))
         (one-way-unify1-lst (cdr term1)
                             (cdr term2)))
        (else #f)))

(define (one-way-unify1-lst lst1 lst2)
  (cond ((null? lst1)
         #t)
        ((one-way-unify1 (car lst1)
                         (car lst2))
         (one-way-unify1-lst (cdr lst1)
                             (cdr lst2)))
        (else #f)))

(define (rewrite term)
  (cond ((not (pair? term))
         term)
        (else (rewrite-with-lemmas (cons (car term)
                                         (rewrite-args (cdr term)))
                                   (get (car term)
                                        (quote lemmas))))))

(define (rewrite-args lst)
  (cond ((null? lst)
         '())
        (else (cons (rewrite (car lst))
                    (rewrite-args (cdr lst))))))

(define (rewrite-with-lemmas term lst)
  (cond ((or (not lst) (null? lst))
         term)
        ((one-way-unify term (cadr (car lst)))
         (rewrite (apply-subst unify-subst (caddr (car lst)))))
        (else (rewrite-with-lemmas term (cdr lst)))))

(define (setup)
  (add-lemma-lst
   (quote ((equal (compile form)
                  (reverse (codegen (optimize form)
                                    (nil))))
           (equal (eqp x y)
                  (equal (fix x)
                         (fix y)))
           (equal (greaterp x y)
                  (lessp y x))
           (equal (lesseqp x y)
                  (not (lessp y x)))
           (equal (greatereqp x y)
                  (not (lessp x y)))
           (equal (boolean x)
                  (or (equal x (t))
                      (equal x (f))))
           (equal (iff x y)
                  (and (implies x y)
                       (implies y x)))
           (equal (even1 x)
                  (if (zerop x)
                      (t)
                      (odd (_1- x))))
           (equal (countps- l pred)
                  (countps-loop l pred (zero)))
           (equal (fact- i)
                  (fact-loop i 1))
           (equal (reverse- x)
                  (reverse-loop x (nil)))
           (equal (divides x y)
                  (zerop (remainder y x)))
           (equal (assume-true var alist)
                  (cons (cons var (t))
                        alist))
           (equal (assume-false var alist)
                  (cons (cons var (f))
                        alist))
           (equal (tautology-checker x)
                  (tautologyp (normalize x)
                              (nil)))
           (equal (falsify x)
                  (falsify1 (normalize x)
                            (nil)))
           (equal (prime x)
                  (and (not (zerop x))
                       (not (equal x (add1 (zero))))
                       (prime1 x (_1- x))))
           (equal (and p q)
                  (if p (if q (t)
                              (f))
                        (f)))
           (equal (or p q)
                  (if p (t)
                        (if q (t)
                              (f))
                        (f)))
           (equal (not p)
                  (if p (f)
                        (t)))
           (equal (implies p q)
                  (if p (if q (t)
                              (f))
                        (t)))
           (equal (fix x)
                  (if (numberp x)
                      x
                      (zero)))
           (equal (if (if a b c)
                      d e)
                  (if a (if b d e)
                        (if c d e)))
           (equal (zerop x)
                  (or (equal x (zero))
                      (not (numberp x))))
           (equal (plus (plus x y)
                        z)
                  (plus x (plus y z)))
           (equal (equal (plus a b)
                         (zero))
                  (and (zerop a)
                       (zerop b)))
           (equal (difference x x)
                  (zero))
           (equal (equal (plus a b)
                         (plus a c))
                  (equal (fix b)
                         (fix c)))
           (equal (equal (zero)
                         (difference x y))
                  (not (lessp y x)))
           (equal (equal x (difference x y))
                  (and (numberp x)
                       (or (equal x (zero))
                           (zerop y))))
           (equal (meaning (plus-tree (append x y))
                           a)
                  (plus (meaning (plus-tree x)
                                 a)
                        (meaning (plus-tree y)
                                 a)))
           (equal (meaning (plus-tree (plus-fringe x))
                           a)
                  (fix (meaning x a)))
           (equal (append (append x y)
                          z)
                  (append x (append y z)))
           (equal (reverse (append a b))
                  (append (reverse b)
                          (reverse a)))
           (equal (times x (plus y z))
                  (plus (times x y)
                        (times x z)))
           (equal (times (times x y)
                         z)
                  (times x (times y z)))
           (equal (equal (times x y)
                         (zero))
                  (or (zerop x)
                      (zerop y)))
           (equal (exec (append x y)
                        pds envrn)
                  (exec y (exec x pds envrn)
                          envrn))
           (equal (mc-flatten x y)
                  (append (flatten x)
                          y))
           (equal (member x (append a b))
                  (or (member x a)
                      (member x b)))
           (equal (member x (reverse y))
                  (member x y))
           (equal (length (reverse x))
                  (length x))
           (equal (member a (intersect b c))
                  (and (member a b)
                       (member a c)))
           (equal (nth (zero)
                       i)
                  (zero))
           (equal (exp i (plus j k))
                  (times (exp i j)
                         (exp i k)))
           (equal (exp i (times j k))
                  (exp (exp i j)
                       k))
           (equal (reverse-loop x y)
                  (append (reverse x)
                          y))
           (equal (reverse-loop x (nil))
                  (reverse x))
           (equal (count-list z (sort-lp x y))
                  (plus (count-list z x)
                        (count-list z y)))
           (equal (equal (append a b)
                         (append a c))
                  (equal b c))
           (equal (plus (remainder x y)
                        (times y (quotient x y)))
                  (fix x))
           (equal (power-eval (big-plus1 l i base)
                              base)
                  (plus (power-eval l base)
                        i))
           (equal (power-eval (big-plus x y i base)
                              base)
                  (plus i (plus (power-eval x base)
                                (power-eval y base))))
           (equal (remainder y 1)
                  (zero))
           (equal (lessp (remainder x y)
                         y)
                  (not (zerop y)))
           (equal (remainder x x)
                  (zero))
           (equal (lessp (quotient i j)
                         i)
                  (and (not (zerop i))
                       (or (zerop j)
                           (not (equal j 1)))))
           (equal (lessp (remainder x y)
                         x)
                  (and (not (zerop y))
                       (not (zerop x))
                       (not (lessp x y))))
           (equal (power-eval (power-rep i base)
                              base)
                  (fix i))
           (equal (power-eval (big-plus (power-rep i base)
                                        (power-rep j base)
                                        (zero)
                                        base)
                              base)
                  (plus i j))
           (equal (gcd x y)
                  (gcd y x))
           (equal (nth (append a b)
                       i)
                  (append (nth a i)
                          (nth b (difference i (length a)))))
           (equal (difference (plus x y)
                              x)
                  (fix y))
           (equal (difference (plus y x)
                              x)
                  (fix y))
           (equal (difference (plus x y)
                              (plus x z))
                  (difference y z))
           (equal (times x (difference c w))
                  (difference (times c x)
                              (times w x)))
           (equal (remainder (times x z)
                             z)
                  (zero))
           (equal (difference (plus b (plus a c))
                              a)
                  (plus b c))
           (equal (difference (add1 (plus y z))
                              z)
                  (add1 y))
           (equal (lessp (plus x y)
                         (plus x z))
                  (lessp y z))
           (equal (lessp (times x z)
                         (times y z))
                  (and (not (zerop z))
                       (lessp x y)))
           (equal (lessp y (plus x y))
                  (not (zerop x)))
           (equal (gcd (times x z)
                       (times y z))
                  (times z (gcd x y)))
           (equal (value (normalize x)
                         a)
                  (value x a))
           (equal (equal (flatten x)
                         (cons y (nil)))
                  (and (nlistp x)
                       (equal x y)))
           (equal (listp (gopher x))
                  (listp x))
           (equal (samefringe x y)
                  (equal (flatten x)
                         (flatten y)))
           (equal (equal (greatest-factor x y)
                         (zero))
                  (and (or (zerop y)
                           (equal y 1))
                       (equal x (zero))))
           (equal (equal (greatest-factor x y)
                         1)
                  (equal x 1))
           (equal (numberp (greatest-factor x y))
                  (not (and (or (zerop y)
                                (equal y 1))
                            (not (numberp x)))))
           (equal (times-list (append x y))
                  (times (times-list x)
                         (times-list y)))
           (equal (prime-list (append x y))
                  (and (prime-list x)
                       (prime-list y)))
           (equal (equal z (times w z))
                  (and (numberp z)
                       (or (equal z (zero))
                           (equal w 1))))
           (equal (greatereqpr x y)
                  (not (lessp x y)))
           (equal (equal x (times x y))
                  (or (equal x (zero))
                      (and (numberp x)
                           (equal y 1))))
           (equal (remainder (times y x)
                             y)
                  (zero))
           (equal (equal (times a b)
                         1)
                  (and (not (equal a (zero)))
                       (not (equal b (zero)))
                       (numberp a)
                       (numberp b)
                       (equal (_1- a)
                              (zero))
                       (equal (_1- b)
                              (zero))))
           (equal (lessp (length (delete x l))
                         (length l))
                  (member x l))
           (equal (sort2 (delete x l))
                  (delete x (sort2 l)))
           (equal (dsort x)
                  (sort2 x))
           (equal (length (cons x1
                                (cons x2
                                      (cons x3 (cons x4
                                                     (cons x5
                                                           (cons x6 x7)))))))
                  (plus 6 (length x7)))
           (equal (difference (add1 (add1 x))
                              2)
                  (fix x))
           (equal (quotient (plus x (plus x y))
                            2)
                  (plus x (quotient y 2)))
           (equal (sigma (zero)
                         i)
                  (quotient (times i (add1 i))
                            2))
           (equal (plus x (add1 y))
                  (if (numberp y)
                      (add1 (plus x y))
                      (add1 x)))
           (equal (equal (difference x y)
                         (difference z y))
                  (if (lessp x y)
                      (not (lessp y z))
                      (if (lessp z y)
                          (not (lessp y x))
                          (equal (fix x)
                                 (fix z)))))
           (equal (meaning (plus-tree (delete x y))
                           a)
                  (if (member x y)
                      (difference (meaning (plus-tree y)
                                           a)
                                  (meaning x a))
                      (meaning (plus-tree y)
                               a)))
           (equal (times x (add1 y))
                  (if (numberp y)
                      (plus x (times x y))
                      (fix x)))
           (equal (nth (nil)
                       i)
                  (if (zerop i)
                      (nil)
                      (zero)))
           (equal (last (append a b))
                  (if (listp b)
                      (last b)
                      (if (listp a)
                          (cons (car (last a))
                                b)
                          b)))
           (equal (equal (lessp x y)
                         z)
                  (if (lessp x y)
                      (equal t z)
                      (equal f z)))
           (equal (assignment x (append a b))
                  (if (assignedp x a)
                      (assignment x a)
                      (assignment x b)))
           (equal (car (gopher x))
                  (if (listp x)
                      (car (flatten x))
                      (zero)))
           (equal (flatten (cdr (gopher x)))
                  (if (listp x)
                      (cdr (flatten x))
                      (cons (zero)
                            (nil))))
           (equal (quotient (times y x)
                            y)
                  (if (zerop y)
                      (zero)
                      (fix x)))
           (equal (get j (set i val mem))
                  (if (eqp j i)
                      val
                      (get j mem)))))))

(define (tautologyp x true-lst false-lst)
  (cond ((truep x true-lst)
         #t)
        ((falsep x false-lst)
         #f)
        ((not (pair? x))
         #f)
        ((eq? (car x)
              (quote if))
         (cond ((truep (cadr x)
                       true-lst)
                (tautologyp (caddr x)
                            true-lst false-lst))
               ((falsep (cadr x)
                        false-lst)
                (tautologyp (cadddr x)
                            true-lst false-lst))
               (else (and (tautologyp (caddr x)
                                      (cons (cadr x)
                                            true-lst)
                                      false-lst)
                          (tautologyp (cadddr x)
                                      true-lst
                                      (cons (cadr x)
                                            false-lst))))))
        (else #f)))

(define (tautp x)
  (tautologyp (rewrite x)
              '() '()))

(define (test alist term)
  (tautp
    (apply-subst alist term)))

(define (trans-of-implies n)
  (list (quote implies)
        (trans-of-implies1 n)
        (list (quote implies)
              0 n)))

(define (trans-of-implies1 n)
  (cond ((equal? n 1)
         (list (quote implies)
               0 1))
        (else (list (quote and)
                    (list (quote implies)
                          (- n 1)
                          n)
                    (trans-of-implies1 (- n 1))))))

(define (truep x lst)
  (or (equal? x (quote (t)))
      (member x lst)))

(setup)

(define (main . args)
  (run-benchmark
    "boyer"
    boyer-iters
    (lambda (result) (equal? result #t))
    (lambda (alist term) (lambda () (test alist term)))
    (quote ((x f (plus (plus a b)
                       (plus c (zero))))
            (y f (times (times a b)
                        (plus c d)))
            (z f (reverse (append (append a b)
                                  (nil))))
            (u equal (plus a b)
                     (difference x y))
            (w lessp (remainder a b)
                     (member a (length b)))))
    (quote (implies (and (implies x y)
                         (and (implies y z)
                              (and (implies z u)
                                   (implies u w))))
                    (implies x w)))))

(main)
