;; ALL benchmarks
;; This benchmark contains all the benchmarks (alphabetically sorted)
;; The benchmarks are slightly modified to avoid global duplicates

(define (run-single-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-single-benchmark name count ok? run-maker . args)
  (let ((run (apply run-maker args)))
    (let ((result (run-single-bench name count ok? run)))
        (if (not (ok? result))
          (begin
            (display "*** wrong result ***")
            (newline)
            (display "*** got: ")
            (write result)
            (newline))))))

;;; ACK -- One of the Kernighan and Van Wyk benchmarks.

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(define (main-ack . args)
  (run-single-benchmark
    "ack"
    ack-iters
    (lambda (result) (equal? result 4093))
    (lambda (m n) (lambda () (ack m n)))
    3
    9))

;;; ARRAY1 -- One of the Kernighan and Van Wyk benchmarks.

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) result)
    (vector-set! result i i)))

(define (create-y x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (- n 1) (- i 1)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(define (my-try n)
  (vector-length (create-y (create-x n))))

(define (go n)
  (let loop ((repeat 100)
             (result '()))
    (if (> repeat 0)
        (loop (- repeat 1) (my-try n))
        result)))

(define (main-array1 . args)
  (run-single-benchmark
    "array1"
    array1-iters
    (lambda (result) (equal? result 200000))
    (lambda (n) (lambda () (go n)))
    200000))

;;; BOYER -- Logic programming benchmark, originally written by Bob Boyer.
;;; Fairly CONS intensive.

(define (boyer-lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (car x)))
        (if (eq? (car pair) key)
          pair
          (loop (cdr x)))))))

(define boyer-properties '())

(define (boyer-get key1 key2)
  (let ((x (boyer-lookup key1 boyer-properties)))
    (if x
      (let ((y (boyer-lookup key2 (cdr x))))
        (if y
          (cdr y)
          #f))
      #f)))

(define (boyer-put key1 key2 val)
  (let ((x (boyer-lookup key1 boyer-properties)))
    (if x
      (let ((y (boyer-lookup key2 (cdr x))))
        (if y
          (set-cdr! y val)
          (set-cdr! x (cons (cons key2 val) (cdr x)))))
      (set! boyer-properties
        (cons (list key1 (cons key2 val)) boyer-properties)))))

(define unify-subst '())

(define (add-lemma term)
  (cond ((and (pair? term)
              (eq? (car term)
                   (quote equal))
              (pair? (cadr term)))
         (boyer-put (car (cadr term))
              (quote lemmas)
              (cons term (boyer-get (car (cadr term)) (quote lemmas)))))
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
                                   (boyer-get (car term)
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

(define (boyer-setup)
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

(define (boyer-test alist term)
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

(boyer-setup)

(define (main-boyer . args)
  (run-single-benchmark
    "boyer"
    boyer-iters
    (lambda (result) (equal? result #t))
    (lambda (alist term) (lambda () (boyer-test alist term)))
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


;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

(define (browse-lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (car x)))
        (if (eq? (car pair) key)
          pair
          (loop (cdr x)))))))

(define browse-properties '())

(define (browse-get key1 key2)
  (let ((x (browse-lookup key1 browse-properties)))
    (if x
      (let ((y (browse-lookup key2 (cdr x))))
        (if y
          (cdr y)
          #f))
      #f)))

(define (browse-put key1 key2 val)
  (let ((x (browse-lookup key1 browse-properties)))
    (if x
      (let ((y (browse-lookup key2 (cdr x))))
        (if y
          (set-cdr! y val)
          (set-cdr! x (cons (cons key2 val) (cdr x)))))
      (set! browse-properties
        (cons (list key1 (cons key2 val)) browse-properties)))))

(define *current-gensym* 0)

(define (generate-symbol)
  (set! *current-gensym* (+ *current-gensym* 1))
  (string->symbol (number->string *current-gensym*)))

(define (append-to-tail! x y)
  (if (null? x)
      y
      (do ((a x b)
           (b (cdr x) (cdr b)))
          ((null? b)
           (set-cdr! a y)
           x))))

(define (tree-copy x)
  (if (not (pair? x))
      x
      (cons (tree-copy (car x))
            (tree-copy (cdr x)))))

;;; n is # of symbols
;;; m is maximum amount of stuff on the plist
;;; npats is the number of basic patterns on the unit
;;; ipats is the instantiated copies of the patterns

(define *rand* 21)

(define (init n m npats ipats)
  (let ((ipats (tree-copy ipats)))
    (do ((p ipats (cdr p)))
        ((null? (cdr p)) (set-cdr! p ipats)))
    (do ((n n (- n 1))
         (i m (cond ((zero? i) m)
                    (else (- i 1))))
         (name (generate-symbol) (generate-symbol))
         (a '()))
        ((= n 0) a)
        (set! a (cons name a))
        (do ((i i (- i 1)))
            ((zero? i))
            (browse-put name (generate-symbol) #f))
        (browse-put name
             'pattern
             (do ((i npats (- i 1))
                  (ipats ipats (cdr ipats))
                  (a '()))
                 ((zero? i) a)
                 (set! a (cons (car ipats) a))))
        (do ((j (- m i) (- j 1)))
            ((zero? j))
            (browse-put name (generate-symbol) #f)))))

(define (browse-random)
  (set! *rand* (remainder (* *rand* 17) 251))
  *rand*)

(define (randomize l)
  (do ((a '()))
      ((null? l) a)
      (let ((n (remainder (browse-random) (length l))))
        (cond ((zero? n)
               (set! a (cons (car l) a))
               (set! l (cdr l))
               l)
              (else
               (do ((n n (- n 1))
                    (x l (cdr x)))
                   ((= n 1)
                    (set! a (cons (cadr x) a))
                    (set-cdr! x (cddr x))
                    x)))))))

(define (my-match pat dat alist)
  (cond ((null? pat)
         (null? dat))
        ((null? dat) '())
        ((or (eq? (car pat) '?)
             (eq? (car pat)
                  (car dat)))
         (my-match (cdr pat) (cdr dat) alist))
        ((eq? (car pat) '*)
         (or (my-match (cdr pat) dat alist)
             (my-match (cdr pat) (cdr dat) alist)
             (my-match pat (cdr dat) alist)))
        (else (cond ((not (pair? (car pat)))
                     (cond ((eq? (string-ref (symbol->string (car pat)) 0)
                                 #\?)
                            (let ((val (assq (car pat) alist)))
                              (cond (val (my-match (cons (cdr val)
                                                      (cdr pat))
                                                dat alist))
                                    (else (my-match (cdr pat)
                                                 (cdr dat)
                                                 (cons (cons (car pat)
                                                             (car dat))
                                                       alist))))))
                           ((eq? (string-ref (symbol->string (car pat)) 0)
                                 #\*)
                            (let ((val (assq (car pat) alist)))
                              (cond (val (my-match (append (cdr val)
                                                        (cdr pat))
                                                dat alist))
                                    (else
                                     (do ((l '()
                                             (append-to-tail!
                                               l
                                               (cons (if (null? d)
                                                         '()
                                                         (car d))
                                                     '())))
                                          (e (cons '() dat) (cdr e))
                                          (d dat (if (null? d) '() (cdr d))))
                                         ((or (null? e)
                                              (my-match (cdr pat)
                                                       d
                                                       (cons
                                                        (cons (car pat) l)
                                                        alist)))
                                          (if (null? e) #f #t)))))))
                           (else #f))) ;;;; fix suggested by Manuel Serrano (cond did not have an else clause); this changes the run time quite a bit
                    (else (and
                           (pair? (car dat))
                           (my-match (car pat)
                                  (car dat) alist)
                           (my-match (cdr pat)
                                  (cdr dat) alist)))))))

(define database
   (randomize
    (init 100 10 4 '((a a a b b b b a a a a a b b a a a)
                     (a a b b b b a a
                                    (a a)(b b))
                     (a a a b (b a) b a b a)))))

(define (browse pats)
  (investigate
    database
    pats))

(define (investigate units pats)
  (do ((units units (cdr units)))
      ((null? units))
      (do ((pats pats (cdr pats)))
          ((null? pats))
          (do ((p (browse-get (car units) 'pattern)
                  (cdr p)))
              ((null? p))
              (my-match (car pats) (car p) '())))))

(define (main-browse . args)
  (run-single-benchmark
    "browse"
    browse-iters
    (lambda (result) #t)
    (lambda (pats) (lambda () (browse pats)))
    '((*a ?b *b ?b a *a a *b *a)
      (*a *b *b *a (*a) (*b))
      (? ? * (b a) * ? ?))))

;; COMPILER
;(define integer->char ascii->char)
;(define char->integer char->ascii)

(define open-input-file* open-input-file)
(define (pp-expression expr port) (write expr port) (newline port))
(define (write-returning-len obj port) (write obj port) 1)
(define (display-returning-len obj port) (display obj port) 1)
(define (write-word w port)
  (write-char (integer->char (quotient w 256)) port)
  (write-char (integer->char (modulo w 256)) port))
(define char-nul (integer->char 0))
(define char-tab (integer->char 9))
(define char-newline (integer->char 10))
(define character-encoding char->integer)
(define max-character-encoding 255)
(define (fatal-err msg arg) (fatal-error msg arg))
(define (scheme-global-var name) name)
(define (scheme-global-var-ref var) (scheme-global-eval var fatal-err))
(define (scheme-global-var-set! var val)
  (scheme-global-eval (list 'set! var (list 'quote val)) fatal-err))
(define (scheme-global-eval expr err) `(eval ,expr)) ;; eval not needed for test
(define (pinpoint-error filename line char) #t)
(define file-path-sep #\:)
(define file-ext-sep #\.)
(define (path-absolute? x)
  (and (> (string-length x) 0)
       (let ((c (string-ref x 0))) (or (char=? c #\/) (char=? c #\~)))))
(define (file-path x)
  (let loop1 ((i (string-length x)))
    (if (and (> i 0) (not (char=? (string-ref x (- i 1)) file-path-sep)))
        (loop1 (- i 1))
        (let ((result (make-string i)))
          (let loop2 ((j (- i 1)))
            (if (< j 0)
                result
                (begin
                  (string-set! result j (string-ref x j))
                  (loop2 (- j 1)))))))))
(define (file-name x)
  (let loop1 ((i (string-length x)))
    (if (and (> i 0) (not (char=? (string-ref x (- i 1)) file-path-sep)))
        (loop1 (- i 1))
        (let ((result (make-string (- (string-length x) i))))
          (let loop2 ((j (- (string-length x) 1)))
            (if (< j i)
                result
                (begin
                  (string-set! result (- j i) (string-ref x j))
                  (loop2 (- j 1)))))))))
(define (file-ext x)
  (let loop1 ((i (string-length x)))
    (if (or (= i 0) (char=? (string-ref x (- i 1)) file-path-sep))
        #f
        (if (not (char=? (string-ref x (- i 1)) file-ext-sep))
            (loop1 (- i 1))
            (let ((result (make-string (- (string-length x) i))))
              (let loop2 ((j (- (string-length x) 1)))
                (if (< j i)
                    result
                    (begin
                      (string-set! result (- j i) (string-ref x j))
                      (loop2 (- j 1))))))))))
(define (file-root x)
  (let loop1 ((i (string-length x)))
    (if (or (= i 0) (char=? (string-ref x (- i 1)) file-path-sep))
        x
        (if (not (char=? (string-ref x (- i 1)) file-ext-sep))
            (loop1 (- i 1))
            (let ((result (make-string (- i 1))))
              (let loop2 ((j (- i 2)))
                (if (< j 0)
                    result
                    (begin
                      (string-set! result j (string-ref x j))
                      (loop2 (- j 1))))))))))
(define (make-counter next limit limit-error)
  (lambda ()
    (if (< next limit)
        (let ((result next)) (set! next (+ next 1)) result)
        (limit-error))))
(define (pos-in-list x l)
  (let loop ((l l) (i 0))
    (cond ((not (pair? l)) #f)
          ((eq? (car l) x) i)
          (else (loop (cdr l) (+ i 1))))))
(define (string-pos-in-list x l)
  (let loop ((l l) (i 0))
    (cond ((not (pair? l)) #f)
          ((string=? (car l) x) i)
          (else (loop (cdr l) (+ i 1))))))
(define (nth-after l n)
  (let loop ((l l) (n n)) (if (> n 0) (loop (cdr l) (- n 1)) l)))
(define (pair-up l1 l2)
  (define (pair l1 l2)
    (if (pair? l1)
        (cons (cons (car l1) (car l2)) (pair (cdr l1) (cdr l2)))
        '()))
  (pair l1 l2))
(define (my-last-pair l)
  (let loop ((l l)) (if (pair? (cdr l)) (loop (cdr l)) l)))
(define (sort-list l <?)
  (define (mergesort l)
    (define (merge l1 l2)
      (cond ((null? l1) l2)
            ((null? l2) l1)
            (else
             (let ((e1 (car l1)) (e2 (car l2)))
               (if (<? e1 e2)
                   (cons e1 (merge (cdr l1) l2))
                   (cons e2 (merge l1 (cdr l2))))))))
    (define (split l)
      (if (or (null? l) (null? (cdr l))) l (cons (car l) (split (cddr l)))))
    (if (or (null? l) (null? (cdr l)))
        l
        (let* ((l1 (mergesort (split l))) (l2 (mergesort (split (cdr l)))))
          (merge l1 l2))))
  (mergesort l))
(define (lst->vector l)
  (let* ((n (length l)) (v (make-vector n)))
    (let loop ((l l) (i 0))
      (if (pair? l)
          (begin (vector-set! v i (car l)) (loop (cdr l) (+ i 1)))
          v))))
(define (vector->lst v)
  (let loop ((l '()) (i (- (vector-length v) 1)))
    (if (< i 0) l (loop (cons (vector-ref v i) l) (- i 1)))))
(define (lst->string l)
  (let* ((n (length l)) (s (make-string n)))
    (let loop ((l l) (i 0))
      (if (pair? l)
          (begin (string-set! s i (car l)) (loop (cdr l) (+ i 1)))
          s))))
(define (string->lst s)
  (let loop ((l '()) (i (- (string-length s) 1)))
    (if (< i 0) l (loop (cons (string-ref s i) l) (- i 1)))))
(define (with-exception-handling proc)
  (let ((old-exception-handler throw-to-exception-handler))
    (let ((val (call-with-current-continuation
                (lambda (cont)
                  (set! throw-to-exception-handler cont)
                  (proc)))))
      (set! throw-to-exception-handler old-exception-handler)
      val)))
(define (throw-to-exception-handler val)
  (fatal-err "Internal error, no exception handler at this point" val))
(define (compiler-error msg . args)
  (newline)
  (display "*** ERROR -- ")
  (display msg)
  (for-each (lambda (x) (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-user-error loc msg . args)
  (newline)
  (display "*** ERROR -- In ")
  (locat-show loc)
  (newline)
  (display "*** ")
  (display msg)
  (for-each (lambda (x) (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-internal-error msg . args)
  (newline)
  (display "*** ERROR -- Compiler internal error detected")
  (newline)
  (display "*** in procedure ")
  (display msg)
  (for-each (lambda (x) (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-limitation-error msg . args)
  (newline)
  (display "*** ERROR -- Compiler limit reached")
  (newline)
  (display "*** ")
  (display msg)
  (for-each (lambda (x) (display " ") (write x)) args)
  (newline)
  (compiler-abort))
(define (compiler-abort) (throw-to-exception-handler #f))
(define (make-gnode label edges) (vector label edges))
(define (gnode-label x) (vector-ref x 0))
(define (gnode-edges x) (vector-ref x 1))
(define (transitive-closure graph)
  (define changed? #f)
  (define (closure edges)
    (list->set
     (set-union
      edges
      (apply set-union
             (map (lambda (label) (gnode-edges (gnode-find label graph)))
                  (set->list edges))))))
  (let ((new-graph
         (set-map (lambda (x)
                    (let ((new-edges (closure (gnode-edges x))))
                      (if (not (set-equal? new-edges (gnode-edges x)))
                          (set! changed? #t))
                      (make-gnode (gnode-label x) new-edges)))
                  graph)))
    (if changed? (transitive-closure new-graph) new-graph)))
(define (gnode-find label graph)
  (define (find label l)
    (cond ((null? l) #f)
          ((eq? (gnode-label (car l)) label) (car l))
          (else (find label (cdr l)))))
  (find label (set->list graph)))
(define (topological-sort graph)
  (if (set-empty? graph)
      '()
      (let ((to-remove (or (remove-no-edges graph) (remove-cycle graph))))
        (let ((labels (set-map gnode-label to-remove)))
          (cons labels
                (topological-sort
                 (set-map (lambda (x)
                            (make-gnode
                             (gnode-label x)
                             (set-difference (gnode-edges x) labels)))
                          (set-difference graph to-remove))))))))
(define (remove-no-edges graph)
  (let ((nodes-with-no-edges
         (set-keep (lambda (x) (set-empty? (gnode-edges x))) graph)))
    (if (set-empty? nodes-with-no-edges) #f nodes-with-no-edges)))
(define (remove-cycle graph)
  (define (remove l)
    (let ((edges (gnode-edges (car l))))
      (define (equal-edges? x) (set-equal? (gnode-edges x) edges))
      (define (member-edges? x) (set-member? (gnode-label x) edges))
      (if (set-member? (gnode-label (car l)) edges)
          (let ((edge-graph (set-keep member-edges? graph)))
            (if (set-every? equal-edges? edge-graph)
                edge-graph
                (remove (cdr l))))
          (remove (cdr l)))))
  (remove (set->list graph)))
(define (list->set list) list)
(define (set->list set) set)
(define (set-empty) '())
(define (set-empty? set) (null? set))
(define (set-member? x set) (memq x set))
(define (set-singleton x) (list x))
(define (set-adjoin set x) (if (memq x set) set (cons x set)))
(define (set-remove set x)
  (cond ((null? set) '())
        ((eq? (car set) x) (cdr set))
        (else (cons (car set) (set-remove (cdr set) x)))))
(define (set-equal? s1 s2)
  (cond ((null? s1) (null? s2))
        ((memq (car s1) s2) (set-equal? (cdr s1) (set-remove s2 (car s1))))
        (else #f)))
(define (set-difference set . other-sets)
  (define (difference s1 s2)
    (cond ((null? s1) '())
          ((memq (car s1) s2) (difference (cdr s1) s2))
          (else (cons (car s1) (difference (cdr s1) s2)))))
  (n-ary difference set other-sets))
(define (set-union . sets)
  (define (union s1 s2)
    (cond ((null? s1) s2)
          ((memq (car s1) s2) (union (cdr s1) s2))
          (else (cons (car s1) (union (cdr s1) s2)))))
  (n-ary union '() sets))
(define (set-intersection set . other-sets)
  (define (intersection s1 s2)
    (cond ((null? s1) '())
          ((memq (car s1) s2) (cons (car s1) (intersection (cdr s1) s2)))
          (else (intersection (cdr s1) s2))))
  (n-ary intersection set other-sets))
(define (n-ary function first rest)
  (if (null? rest)
      first
      (n-ary function (function first (car rest)) (cdr rest))))
(define (set-keep keep? set)
  (cond ((null? set) '())
        ((keep? (car set)) (cons (car set) (set-keep keep? (cdr set))))
        (else (set-keep keep? (cdr set)))))
(define (set-every? pred? set)
  (or (null? set) (and (pred? (car set)) (set-every? pred? (cdr set)))))
(define (set-map proc set)
  (if (null? set) '() (cons (proc (car set)) (set-map proc (cdr set)))))
(define (list->queue list)
  (cons list (if (pair? list) (my-last-pair list) '())))
(define (queue->list queue) (car queue))
(define (queue-empty) (cons '() '()))
(define (queue-empty? queue) (null? (car queue)))
(define (queue-get! queue)
  (if (null? (car queue))
      (compiler-internal-error "queue-get!, queue is empty")
      (let ((x (caar queue)))
        (set-car! queue (cdar queue))
        (if (null? (car queue)) (set-cdr! queue '()))
        x)))
(define (queue-put! queue x)
  (let ((entry (cons x '())))
    (if (null? (car queue))
        (set-car! queue entry)
        (set-cdr! (cdr queue) entry))
    (set-cdr! queue entry)
    x))
(define (string->canonical-symbol str)
  (let ((len (string-length str)))
    (let loop ((str str) (s (make-string len)) (i (- len 1)))
      (if (>= i 0)
          (begin
            (string-set! s i (char-downcase (string-ref str i)))
            (loop str s (- i 1)))
          (string->symbol s)))))
(define quote-sym (string->canonical-symbol "QUOTE"))
(define quasiquote-sym (string->canonical-symbol "QUASIQUOTE"))
(define unquote-sym (string->canonical-symbol "UNQUOTE"))
(define unquote-splicing-sym (string->canonical-symbol "UNQUOTE-SPLICING"))
(define lambda-sym (string->canonical-symbol "LAMBDA"))
(define if-sym (string->canonical-symbol "IF"))
(define set!-sym (string->canonical-symbol "SET!"))
(define cond-sym (string->canonical-symbol "COND"))
(define =>-sym (string->canonical-symbol "=>"))
(define else-sym (string->canonical-symbol "ELSE"))
(define and-sym (string->canonical-symbol "AND"))
(define or-sym (string->canonical-symbol "OR"))
(define case-sym (string->canonical-symbol "CASE"))
(define let-sym (string->canonical-symbol "LET"))
(define let*-sym (string->canonical-symbol "LET*"))
(define letrec-sym (string->canonical-symbol "LETREC"))
(define begin-sym (string->canonical-symbol "BEGIN"))
(define do-sym (string->canonical-symbol "DO"))
(define define-sym (string->canonical-symbol "DEFINE"))
(define delay-sym (string->canonical-symbol "DELAY"))
(define future-sym (string->canonical-symbol "FUTURE"))
(define **define-macro-sym (string->canonical-symbol "DEFINE-MACRO"))
(define **declare-sym (string->canonical-symbol "DECLARE"))
(define **include-sym (string->canonical-symbol "INCLUDE"))
(define not-sym (string->canonical-symbol "NOT"))
(define **c-declaration-sym (string->canonical-symbol "C-DECLARATION"))
(define **c-init-sym (string->canonical-symbol "C-INIT"))
(define **c-procedure-sym (string->canonical-symbol "C-PROCEDURE"))
(define void-sym (string->canonical-symbol "VOID"))
(define char-sym (string->canonical-symbol "CHAR"))
(define signed-char-sym (string->canonical-symbol "SIGNED-CHAR"))
(define unsigned-char-sym (string->canonical-symbol "UNSIGNED-CHAR"))
(define short-sym (string->canonical-symbol "SHORT"))
(define unsigned-short-sym (string->canonical-symbol "UNSIGNED-SHORT"))
(define int-sym (string->canonical-symbol "INT"))
(define unsigned-int-sym (string->canonical-symbol "UNSIGNED-INT"))
(define long-sym (string->canonical-symbol "LONG"))
(define unsigned-long-sym (string->canonical-symbol "UNSIGNED-LONG"))
(define float-sym (string->canonical-symbol "FLOAT"))
(define double-sym (string->canonical-symbol "DOUBLE"))
(define pointer-sym (string->canonical-symbol "POINTER"))
(define boolean-sym (string->canonical-symbol "BOOLEAN"))
(define string-sym (string->canonical-symbol "STRING"))
(define scheme-object-sym (string->canonical-symbol "SCHEME-OBJECT"))
(define c-id-prefix "___")
(define false-object (if (eq? '() #f) (string->symbol "#f") #f))
(define (false-object? obj) (eq? obj false-object))
(define undef-object (string->symbol "#[undefined]"))
(define (undef-object? obj) (eq? obj undef-object))
(define (symbol-object? obj)
  (and (not (false-object? obj)) (not (undef-object? obj)) (symbol? obj)))
(define scm-file-exts '("scm" #f))
(define compiler-version "2.2.2")
(define (open-sf filename)
  (define (open-err) (compiler-error "Can't find file" filename))
  (if (not (file-ext filename))
      (let loop ((exts scm-file-exts))
        (if (pair? exts)
            (let* ((ext (car exts))
                   (full-name
                    (if ext (string-append filename "." ext) filename))
                   (port (open-input-file* full-name)))
              (if port (vector port full-name 0 1 0) (loop (cdr exts))))
            (open-err)))
      (let ((port (open-input-file* filename)))
        (if port (vector port filename 0 1 0) (open-err)))))
(define (close-sf sf) (close-input-port (vector-ref sf 0)))
(define (sf-read-char sf)
  (let ((c (read-char (vector-ref sf 0))))
    (cond ((eof-object? c))
          ((char=? c char-newline)
           (vector-set! sf 3 (+ (vector-ref sf 3) 1))
           (vector-set! sf 4 0))
          (else (vector-set! sf 4 (+ (vector-ref sf 4) 1))))
    c))
(define (sf-peek-char sf) (peek-char (vector-ref sf 0)))
(define (sf-read-error sf msg . args)
  (apply compiler-user-error
         (cons (sf->locat sf)
               (cons (string-append "Read error -- " msg) args))))
(define (sf->locat sf)
  (vector 'file
          (vector-ref sf 1)
          (vector-ref sf 2)
          (vector-ref sf 3)
          (vector-ref sf 4)))
(define (expr->locat expr source) (vector 'expr expr source))
(define (locat-show loc)
  (if loc
      (case (vector-ref loc 0)
        ((file)
         (if (pinpoint-error
              (vector-ref loc 1)
              (vector-ref loc 3)
              (vector-ref loc 4))
             (begin
               (display "file \"")
               (display (vector-ref loc 1))
               (display "\", line ")
               (display (vector-ref loc 3))
               (display ", character ")
               (display (vector-ref loc 4)))))
        ((expr)
         (display "expression ")
         (write (vector-ref loc 1))
         (if (vector-ref loc 2)
             (begin
               (display " ")
               (locat-show (source-locat (vector-ref loc 2))))))
        (else (compiler-internal-error "locat-show, unknown location tag")))
      (display "unknown location")))
(define (locat-filename loc)
  (if loc
      (case (vector-ref loc 0)
        ((file) (vector-ref loc 1))
        ((expr)
         (let ((source (vector-ref loc 2)))
           (if source (locat-filename (source-locat source)) "")))
        (else
         (compiler-internal-error "locat-filename, unknown location tag")))
      ""))
(define (make-source code locat) (vector code locat))
(define (source-code x) (vector-ref x 0))
(define (source-code-set! x y) (vector-set! x 0 y) x)
(define (source-locat x) (vector-ref x 1))
(define (expression->source expr source)
  (define (expr->source x)
    (make-source
     (cond ((pair? x) (list->source x))
           ((vector? x) (vector->source x))
           ((symbol-object? x) (string->canonical-symbol (symbol->string x)))
           (else x))
     (expr->locat x source)))
  (define (list->source l)
    (cond ((pair? l) (cons (expr->source (car l)) (list->source (cdr l))))
          ((null? l) '())
          (else (expr->source l))))
  (define (vector->source v)
    (let* ((len (vector-length v)) (x (make-vector len)))
      (let loop ((i (- len 1)))
        (if (>= i 0)
            (begin
              (vector-set! x i (expr->source (vector-ref v i)))
              (loop (- i 1)))))
      x))
  (expr->source expr))
(define (source->expression source)
  (define (list->expression l)
    (cond ((pair? l)
           (cons (source->expression (car l)) (list->expression (cdr l))))
          ((null? l) '())
          (else (source->expression l))))
  (define (vector->expression v)
    (let* ((len (vector-length v)) (x (make-vector len)))
      (let loop ((i (- len 1)))
        (if (>= i 0)
            (begin
              (vector-set! x i (source->expression (vector-ref v i)))
              (loop (- i 1)))))
      x))
  (let ((code (source-code source)))
    (cond ((pair? code) (list->expression code))
          ((vector? code) (vector->expression code))
          (else code))))
(define (file->sources filename info-port)
  (if info-port
      (begin
        (display "(reading \"" info-port)
        (display filename info-port)
        (display "\"" info-port)))
  (let ((sf (open-sf filename)))
    (define (read-sources)
      (let ((source (read-source sf)))
        (if (not (eof-object? source))
            (begin
              (if info-port (display "." info-port))
              (cons source (read-sources)))
            '())))
    (let ((sources (read-sources)))
      (if info-port (display ")" info-port))
      (close-sf sf)
      sources)))
(define (file->sources* filename info-port loc)
  (file->sources
   (if (path-absolute? filename)
       filename
       (string-append (file-path (locat-filename loc)) filename))
   info-port))
(define (read-source sf)
  (define (read-char*)
    (let ((c (sf-read-char sf)))
      (if (eof-object? c)
          (sf-read-error sf "Premature end of file encountered")
          c)))
  (define (read-non-whitespace-char)
    (let ((c (read-char*)))
      (cond ((< 0 (vector-ref read-table (char->integer c)))
             (read-non-whitespace-char))
            ((char=? c #\;)
             (let loop ()
               (if (not (char=? (read-char*) char-newline))
                   (loop)
                   (read-non-whitespace-char))))
            (else c))))
  (define (delimiter? c)
    (or (eof-object? c) (not (= (vector-ref read-table (char->integer c)) 0))))
  (define (read-list first)
    (let ((result (cons first '())))
      (let loop ((end result))
        (let ((c (read-non-whitespace-char)))
          (cond ((char=? c #\)))
                ((and (char=? c #\.) (delimiter? (sf-peek-char sf)))
                 (let ((x (read-source sf)))
                   (if (char=? (read-non-whitespace-char) #\))
                       (set-cdr! end x)
                       (sf-read-error sf "')' expected"))))
                (else
                 (let ((tail (cons (rd* c) '())))
                   (set-cdr! end tail)
                   (loop tail))))))
      result))
  (define (read-vector)
    (define (loop i)
      (let ((c (read-non-whitespace-char)))
        (if (char=? c #\))
            (make-vector i '())
            (let* ((x (rd* c)) (v (loop (+ i 1)))) (vector-set! v i x) v))))
    (loop 0))
  (define (read-string)
    (define (loop i)
      (let ((c (read-char*)))
        (cond ((char=? c #\") (make-string i #\space))
              ((char=? c #\\)
               (let* ((c (read-char*)) (s (loop (+ i 1))))
                 (string-set! s i c)
                 s))
              (else (let ((s (loop (+ i 1)))) (string-set! s i c) s)))))
    (loop 0))
  (define (read-symbol/number-string i)
    (if (delimiter? (sf-peek-char sf))
        (make-string i #\space)
        (let* ((c (sf-read-char sf)) (s (read-symbol/number-string (+ i 1))))
          (string-set! s i (char-downcase c))
          s)))
  (define (read-symbol/number c)
    (let ((s (read-symbol/number-string 1)))
      (string-set! s 0 (char-downcase c))
      (or (string->number s 10) (string->canonical-symbol s))))
  (define (read-prefixed-number c)
    (let ((s (read-symbol/number-string 2)))
      (string-set! s 0 #\#)
      (string-set! s 1 c)
      (string->number s 10)))
  (define (read-special-symbol)
    (let ((s (read-symbol/number-string 2)))
      (string-set! s 0 #\#)
      (string-set! s 1 #\#)
      (string->canonical-symbol s)))
  (define (rd c)
    (cond ((eof-object? c) c)
          ((< 0 (vector-ref read-table (char->integer c)))
           (rd (sf-read-char sf)))
          ((char=? c #\;)
           (let loop ()
             (let ((c (sf-read-char sf)))
               (cond ((eof-object? c) c)
                     ((char=? c char-newline) (rd (sf-read-char sf)))
                     (else (loop))))))
          (else (rd* c))))
  (define (rd* c)
    (let ((source (make-source #f (sf->locat sf))))
      (source-code-set!
       source
       (cond ((char=? c #\()
              (let ((x (read-non-whitespace-char)))
                (if (char=? x #\)) '() (read-list (rd* x)))))
             ((char=? c #\#)
              (let ((c (char-downcase (sf-read-char sf))))
                (cond ((char=? c #\() (read-vector))
                      ((char=? c #\f) false-object)
                      ((char=? c #\t) #t)
                      ((char=? c #\\)
                       (let ((c (read-char*)))
                         (if (or (not (char-alphabetic? c))
                                 (delimiter? (sf-peek-char sf)))
                             c
                             (let ((name (read-symbol/number c)))
                               (let ((x (assq name named-char-table)))
                                 (if x
                                     (cdr x)
                                     (sf-read-error
                                      sf
                                      "Unknown character name"
                                      name)))))))
                      ((char=? c #\#) (read-special-symbol))
                      (else
                       (let ((num (read-prefixed-number c)))
                         (or num
                             (sf-read-error
                              sf
                              "Unknown '#' read macro"
                              c)))))))
             ((char=? c #\") (read-string))
             ((char=? c #\')
              (list (make-source quote-sym (sf->locat sf)) (read-source sf)))
             ((char=? c #\`)
              (list (make-source quasiquote-sym (sf->locat sf))
                    (read-source sf)))
             ((char=? c #\,)
              (if (char=? (sf-peek-char sf) #\@)
                  (let ((x (make-source unquote-splicing-sym (sf->locat sf))))
                    (sf-read-char sf)
                    (list x (read-source sf)))
                  (list (make-source unquote-sym (sf->locat sf))
                        (read-source sf))))
             ((char=? c #\)) (sf-read-error sf "Misplaced ')'"))
             ((or (char=? c #\[) (char=? c #\]) (char=? c #\{) (char=? c #\}))
              (sf-read-error sf "Illegal character" c))
             (else
              (if (char=? c #\.)
                  (if (delimiter? (sf-peek-char sf))
                      (sf-read-error sf "Misplaced '.'")))
              (read-symbol/number c))))))
  (rd (sf-read-char sf)))
(define named-char-table
  (list (cons (string->canonical-symbol "NUL") char-nul)
        (cons (string->canonical-symbol "TAB") char-tab)
        (cons (string->canonical-symbol "NEWLINE") char-newline)
        (cons (string->canonical-symbol "SPACE") #\space)))
(define read-table
  (let ((rt (make-vector (+ max-character-encoding 1) 0)))
    (vector-set! rt (char->integer char-tab) 1)
    (vector-set! rt (char->integer char-newline) 1)
    (vector-set! rt (char->integer #\space) 1)
    (vector-set! rt (char->integer #\;) -1)
    (vector-set! rt (char->integer #\() -1)
    (vector-set! rt (char->integer #\)) -1)
    (vector-set! rt (char->integer #\") -1)
    (vector-set! rt (char->integer #\') -1)
    (vector-set! rt (char->integer #\`) -1)
    rt))
(define (make-var name bound refs sets source)
  (vector var-tag name bound refs sets source #f))
(define (var? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) var-tag)))
(define (var-name x) (vector-ref x 1))
(define (var-bound x) (vector-ref x 2))
(define (var-refs x) (vector-ref x 3))
(define (var-sets x) (vector-ref x 4))
(define (var-source x) (vector-ref x 5))
(define (var-info x) (vector-ref x 6))
(define (var-name-set! x y) (vector-set! x 1 y))
(define (var-bound-set! x y) (vector-set! x 2 y))
(define (var-refs-set! x y) (vector-set! x 3 y))
(define (var-sets-set! x y) (vector-set! x 4 y))
(define (var-source-set! x y) (vector-set! x 5 y))
(define (var-info-set! x y) (vector-set! x 6 y))
(define var-tag (list 'var-tag))
(define (var-copy var)
  (make-var (var-name var) #t (set-empty) (set-empty) (var-source var)))
(define (make-temp-var name) (make-var name #t (set-empty) (set-empty) #f))
(define (temp-var? var) (eq? (var-bound var) #t))
(define ret-var (make-temp-var 'ret))
(define ret-var-set (set-singleton ret-var))
(define closure-env-var (make-temp-var 'closure-env))
(define empty-var (make-temp-var #f))
(define make-global-environment #f)
(set! make-global-environment (lambda () (env-frame #f '())))
(define (env-frame env vars) (vector (cons vars #f) '() '() env))
(define (env-new-var! env name source)
  (let* ((glob (not (env-parent-ref env)))
         (var (make-var name (not glob) (set-empty) (set-empty) source)))
    (env-vars-set! env (cons var (env-vars-ref env)))
    var))
(define (env-macro env name def)
  (let ((name* (if (full-name? name)
                   name
                   (let ((prefix (env-namespace-prefix env name)))
                     (if prefix (make-full-name prefix name) name)))))
    (vector (vector-ref env 0)
            (cons (cons name* def) (env-macros-ref env))
            (env-decls-ref env)
            (env-parent-ref env))))
(define (env-declare env decl)
  (vector (vector-ref env 0)
          (env-macros-ref env)
          (cons decl (env-decls-ref env))
          (env-parent-ref env)))
(define (env-vars-ref env) (car (vector-ref env 0)))
(define (env-vars-set! env vars) (set-car! (vector-ref env 0) vars))
(define (env-macros-ref env) (vector-ref env 1))
(define (env-decls-ref env) (vector-ref env 2))
(define (env-parent-ref env) (vector-ref env 3))
(define (env-namespace-prefix env name)
  (let loop ((decls (env-decls-ref env)))
    (if (pair? decls)
        (let ((decl (car decls)))
          (if (eq? (car decl) namespace-sym)
              (let ((syms (cddr decl)))
                (if (or (null? syms) (memq name syms))
                    (cadr decl)
                    (loop (cdr decls))))
              (loop (cdr decls))))
        #f)))
(define (env-lookup env name stop-at-first-frame? proc)
  (define (search env name full?)
    (if full?
        (search* env name full?)
        (let ((prefix (env-namespace-prefix env name)))
          (if prefix
              (search* env (make-full-name prefix name) #t)
              (search* env name full?)))))
  (define (search* env name full?)
    (define (search-macros macros)
      (if (pair? macros)
          (let ((m (car macros)))
            (if (eq? (car m) name)
                (proc env name (cdr m))
                (search-macros (cdr macros))))
          (search-vars (env-vars-ref env))))
    (define (search-vars vars)
      (if (pair? vars)
          (let ((v (car vars)))
            (if (eq? (var-name v) name)
                (proc env name v)
                (search-vars (cdr vars))))
          (let ((env* (env-parent-ref env)))
            (if (or stop-at-first-frame? (not env*))
                (proc env name #f)
                (search env* name full?)))))
    (search-macros (env-macros-ref env)))
  (search env name (full-name? name)))
(define (valid-prefix? str)
  (let ((l (string-length str)))
    (or (= l 0) (and (>= l 2) (char=? (string-ref str (- l 1)) #\#)))))
(define (full-name? sym)
  (let ((str (symbol->string sym)))
    (let loop ((i (- (string-length str) 1)))
      (if (< i 0) #f (if (char=? (string-ref str i) #\#) #t (loop (- i 1)))))))
(define (make-full-name prefix sym)
  (if (= (string-length prefix) 0)
      sym
      (string->canonical-symbol (string-append prefix (symbol->string sym)))))
(define (env-lookup-var env name source)
  (env-lookup
   env
   name
   #f
   (lambda (env name x)
     (if x
         (if (var? x)
             x
             (compiler-internal-error
              "env-lookup-var, name is that of a macro"
              name))
         (env-new-var! env name source)))))
(define (env-define-var env name source)
  (env-lookup
   env
   name
   #t
   (lambda (env name x)
     (if x
         (if (var? x)
             (pt-syntax-error source "Duplicate definition of a variable")
             (compiler-internal-error
              "env-define-var, name is that of a macro"
              name))
         (env-new-var! env name source)))))
(define (env-lookup-global-var env name)
  (let ((env* (env-global-env env)))
    (define (search-vars vars)
      (if (pair? vars)
          (let ((v (car vars)))
            (if (eq? (var-name v) name) v (search-vars (cdr vars))))
          (env-new-var! env* name #f)))
    (search-vars (env-vars-ref env*))))
(define (env-global-variables env) (env-vars-ref (env-global-env env)))
(define (env-global-env env)
  (let loop ((env env))
    (let ((env* (env-parent-ref env))) (if env* (loop env*) env))))
(define (env-lookup-macro env name)
  (env-lookup
   env
   name
   #f
   (lambda (env name x) (if (or (not x) (var? x)) #f x))))
(define (env-declarations env) env)
(define flag-declarations '())
(define parameterized-declarations '())
(define boolean-declarations '())
(define namable-declarations '())
(define namable-boolean-declarations '())
(define namable-string-declarations '())
(define (define-flag-decl name type)
  (set! flag-declarations (cons (cons name type) flag-declarations))
  '())
(define (define-parameterized-decl name)
  (set! parameterized-declarations (cons name parameterized-declarations))
  '())
(define (define-boolean-decl name)
  (set! boolean-declarations (cons name boolean-declarations))
  '())
(define (define-namable-decl name type)
  (set! namable-declarations (cons (cons name type) namable-declarations))
  '())
(define (define-namable-boolean-decl name)
  (set! namable-boolean-declarations (cons name namable-boolean-declarations))
  '())
(define (define-namable-string-decl name)
  (set! namable-string-declarations (cons name namable-string-declarations))
  '())
(define (flag-decl source type val) (list type val))
(define (parameterized-decl source id parm) (list id parm))
(define (boolean-decl source id pos) (list id pos))
(define (namable-decl source type val names) (cons type (cons val names)))
(define (namable-boolean-decl source id pos names) (cons id (cons pos names)))
(define (namable-string-decl source id str names)
  (if (and (eq? id namespace-sym) (not (valid-prefix? str)))
      (pt-syntax-error source "Illegal namespace"))
  (cons id (cons str names)))
(define (declaration-value name element default decls)
  (if (not decls)
      default
      (let loop ((l (env-decls-ref decls)))
        (if (pair? l)
            (let ((d (car l)))
              (if (and (eq? (car d) name)
                       (or (null? (cddr d)) (memq element (cddr d))))
                  (cadr d)
                  (loop (cdr l))))
            (declaration-value name element default (env-parent-ref decls))))))
(define namespace-sym (string->canonical-symbol "NAMESPACE"))
(define-namable-string-decl namespace-sym)
(define (node-parent x) (vector-ref x 1))
(define (node-children x) (vector-ref x 2))
(define (node-fv x) (vector-ref x 3))
(define (node-decl x) (vector-ref x 4))
(define (node-source x) (vector-ref x 5))
(define (node-parent-set! x y) (vector-set! x 1 y))
(define (node-fv-set! x y) (vector-set! x 3 y))
(define (node-decl-set! x y) (vector-set! x 4 y))
(define (node-source-set! x y) (vector-set! x 5 y))
(define (node-children-set! x y)
  (vector-set! x 2 y)
  (for-each (lambda (child) (node-parent-set! child x)) y)
  (node-fv-invalidate! x))
(define (node-fv-invalidate! x)
  (let loop ((node x))
    (if node (begin (node-fv-set! node #t) (loop (node-parent node))))))
(define (make-cst parent children fv decl source val)
  (vector cst-tag parent children fv decl source val))
(define (cst? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) cst-tag)))
(define (cst-val x) (vector-ref x 6))
(define (cst-val-set! x y) (vector-set! x 6 y))
(define cst-tag (list 'cst-tag))
(define (make-ref parent children fv decl source var)
  (vector ref-tag parent children fv decl source var))
(define (ref? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) ref-tag)))
(define (ref-var x) (vector-ref x 6))
(define (ref-var-set! x y) (vector-set! x 6 y))
(define ref-tag (list 'ref-tag))
(define (make-set parent children fv decl source var)
  (vector set-tag parent children fv decl source var))
(define (set? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) set-tag)))
(define (set-var x) (vector-ref x 6))
(define (set-var-set! x y) (vector-set! x 6 y))
(define set-tag (list 'set-tag))
(define (make-def parent children fv decl source var)
  (vector def-tag parent children fv decl source var))
(define (def? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) def-tag)))
(define (def-var x) (vector-ref x 6))
(define (def-var-set! x y) (vector-set! x 6 y))
(define def-tag (list 'def-tag))
(define (make-tst parent children fv decl source)
  (vector tst-tag parent children fv decl source))
(define (tst? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) tst-tag)))
(define tst-tag (list 'tst-tag))
(define (make-conj parent children fv decl source)
  (vector conj-tag parent children fv decl source))
(define (conj? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) conj-tag)))
(define conj-tag (list 'conj-tag))
(define (make-disj parent children fv decl source)
  (vector disj-tag parent children fv decl source))
(define (disj? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) disj-tag)))
(define disj-tag (list 'disj-tag))
(define (make-prc parent children fv decl source name min rest parms)
  (vector prc-tag parent children fv decl source name min rest parms))
(define (prc? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) prc-tag)))
(define (prc-name x) (vector-ref x 6))
(define (prc-min x) (vector-ref x 7))
(define (prc-rest x) (vector-ref x 8))
(define (prc-parms x) (vector-ref x 9))
(define (prc-name-set! x y) (vector-set! x 6 y))
(define (prc-min-set! x y) (vector-set! x 7 y))
(define (prc-rest-set! x y) (vector-set! x 8 y))
(define (prc-parms-set! x y) (vector-set! x 9 y))
(define prc-tag (list 'prc-tag))
(define (make-app parent children fv decl source)
  (vector app-tag parent children fv decl source))
(define (app? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) app-tag)))
(define app-tag (list 'app-tag))
(define (make-fut parent children fv decl source)
  (vector fut-tag parent children fv decl source))
(define (fut? x)
  (and (vector? x) (> (vector-length x) 0) (eq? (vector-ref x 0) fut-tag)))
(define fut-tag (list 'fut-tag))
(define (new-cst source decl val) (make-cst #f '() #t decl source val))
(define (new-ref source decl var)
  (let ((node (make-ref #f '() #t decl source var)))
    (var-refs-set! var (set-adjoin (var-refs var) node))
    node))
(define (new-ref-extended-bindings source name env)
  (new-ref source
           (add-extended-bindings (env-declarations env))
           (env-lookup-global-var env name)))
(define (new-set source decl var val)
  (let ((node (make-set #f (list val) #t decl source var)))
    (var-sets-set! var (set-adjoin (var-sets var) node))
    (node-parent-set! val node)
    node))
(define (set-val x)
  (if (set? x)
      (car (node-children x))
      (compiler-internal-error "set-val, 'set' node expected" x)))
(define (new-def source decl var val)
  (let ((node (make-def #f (list val) #t decl source var)))
    (var-sets-set! var (set-adjoin (var-sets var) node))
    (node-parent-set! val node)
    node))
(define (def-val x)
  (if (def? x)
      (car (node-children x))
      (compiler-internal-error "def-val, 'def' node expected" x)))
(define (new-tst source decl pre con alt)
  (let ((node (make-tst #f (list pre con alt) #t decl source)))
    (node-parent-set! pre node)
    (node-parent-set! con node)
    (node-parent-set! alt node)
    node))
(define (tst-pre x)
  (if (tst? x)
      (car (node-children x))
      (compiler-internal-error "tst-pre, 'tst' node expected" x)))
(define (tst-con x)
  (if (tst? x)
      (cadr (node-children x))
      (compiler-internal-error "tst-con, 'tst' node expected" x)))
(define (tst-alt x)
  (if (tst? x)
      (caddr (node-children x))
      (compiler-internal-error "tst-alt, 'tst' node expected" x)))
(define (new-conj source decl pre alt)
  (let ((node (make-conj #f (list pre alt) #t decl source)))
    (node-parent-set! pre node)
    (node-parent-set! alt node)
    node))
(define (conj-pre x)
  (if (conj? x)
      (car (node-children x))
      (compiler-internal-error "conj-pre, 'conj' node expected" x)))
(define (conj-alt x)
  (if (conj? x)
      (cadr (node-children x))
      (compiler-internal-error "conj-alt, 'conj' node expected" x)))
(define (new-disj source decl pre alt)
  (let ((node (make-disj #f (list pre alt) #t decl source)))
    (node-parent-set! pre node)
    (node-parent-set! alt node)
    node))
(define (disj-pre x)
  (if (disj? x)
      (car (node-children x))
      (compiler-internal-error "disj-pre, 'disj' node expected" x)))
(define (disj-alt x)
  (if (disj? x)
      (cadr (node-children x))
      (compiler-internal-error "disj-alt, 'disj' node expected" x)))
(define (new-prc source decl name min rest parms body)
  (let ((node (make-prc #f (list body) #t decl source name min rest parms)))
    (for-each (lambda (x) (var-bound-set! x node)) parms)
    (node-parent-set! body node)
    node))
(define (prc-body x)
  (if (prc? x)
      (car (node-children x))
      (compiler-internal-error "prc-body, 'proc' node expected" x)))
(define (new-call source decl oper args)
  (let ((node (make-app #f (cons oper args) #t decl source)))
    (node-parent-set! oper node)
    (for-each (lambda (x) (node-parent-set! x node)) args)
    node))
(define (new-call* source decl oper args)
  (if *ptree-port*
      (if (ref? oper)
          (let ((var (ref-var oper)))
            (if (global? var)
                (let ((proc (standard-procedure
                             (var-name var)
                             (node-decl oper))))
                  (if (and proc
                           (not (nb-args-conforms?
                                 (length args)
                                 (standard-procedure-call-pattern proc))))
                      (begin
                        (display "*** WARNING -- \"" *ptree-port*)
                        (display (var-name var) *ptree-port*)
                        (display "\" is called with " *ptree-port*)
                        (display (length args) *ptree-port*)
                        (display " argument(s)." *ptree-port*)
                        (newline *ptree-port*))))))))
  (new-call source decl oper args))
(define (app-oper x)
  (if (app? x)
      (car (node-children x))
      (compiler-internal-error "app-oper, 'call' node expected" x)))
(define (app-args x)
  (if (app? x)
      (cdr (node-children x))
      (compiler-internal-error "app-args, 'call' node expected" x)))
(define (oper-pos? node)
  (let ((parent (node-parent node)))
    (if parent (and (app? parent) (eq? (app-oper parent) node)) #f)))
(define (new-fut source decl val)
  (let ((node (make-fut #f (list val) #t decl source)))
    (node-parent-set! val node)
    node))
(define (fut-val x)
  (if (fut? x)
      (car (node-children x))
      (compiler-internal-error "fut-val, 'fut' node expected" x)))
(define (new-disj-call source decl pre oper alt)
  (new-call*
   source
   decl
   (let* ((parms (new-temps source '(temp))) (temp (car parms)))
     (new-prc source
              decl
              #f
              1
              #f
              parms
              (new-tst source
                       decl
                       (new-ref source decl temp)
                       (new-call*
                        source
                        decl
                        oper
                        (list (new-ref source decl temp)))
                       alt)))
   (list pre)))
(define (new-seq source decl before after)
  (new-call*
   source
   decl
   (new-prc source decl #f 1 #f (new-temps source '(temp)) after)
   (list before)))
(define (new-let ptree proc vars vals body)
  (if (pair? vars)
      (new-call
       (node-source ptree)
       (node-decl ptree)
       (new-prc (node-source proc)
                (node-decl proc)
                (prc-name proc)
                (length vars)
                #f
                (reverse vars)
                body)
       (reverse vals))
      body))
(define (new-temps source names)
  (if (null? names)
      '()
      (cons (make-var (car names) #t (set-empty) (set-empty) source)
            (new-temps source (cdr names)))))
(define (new-variables vars)
  (if (null? vars)
      '()
      (cons (make-var
             (source-code (car vars))
             #t
             (set-empty)
             (set-empty)
             (car vars))
            (new-variables (cdr vars)))))
(define (set-prc-names! vars vals)
  (let loop ((vars vars) (vals vals))
    (if (not (null? vars))
        (let ((var (car vars)) (val (car vals)))
          (if (prc? val) (prc-name-set! val (symbol->string (var-name var))))
          (loop (cdr vars) (cdr vals))))))
(define (free-variables node)
  (if (eq? (node-fv node) #t)
      (let ((x (apply set-union (map free-variables (node-children node)))))
        (node-fv-set!
         node
         (cond ((ref? node)
                (if (global? (ref-var node)) x (set-adjoin x (ref-var node))))
               ((set? node)
                (if (global? (set-var node)) x (set-adjoin x (set-var node))))
               ((prc? node) (set-difference x (list->set (prc-parms node))))
               ((and (app? node) (prc? (app-oper node)))
                (set-difference x (list->set (prc-parms (app-oper node)))))
               (else x)))))
  (node-fv node))
(define (bound-variables node) (list->set (prc-parms node)))
(define (not-mutable? var) (set-empty? (var-sets var)))
(define (mutable? var) (not (not-mutable? var)))
(define (bound? var) (var-bound var))
(define (global? var) (not (bound? var)))
(define (global-val var)
  (and (global? var)
       (let ((sets (set->list (var-sets var))))
         (and (pair? sets)
              (null? (cdr sets))
              (def? (car sets))
              (eq? (compilation-strategy (node-decl (car sets))) block-sym)
              (def-val (car sets))))))
(define **not-sym (string->canonical-symbol "##NOT"))
(define **quasi-append-sym (string->canonical-symbol "##QUASI-APPEND"))
(define **quasi-list-sym (string->canonical-symbol "##QUASI-LIST"))
(define **quasi-cons-sym (string->canonical-symbol "##QUASI-CONS"))
(define **quasi-list->vector-sym
  (string->canonical-symbol "##QUASI-LIST->VECTOR"))
(define **case-memv-sym (string->canonical-symbol "##CASE-MEMV"))
(define **unassigned?-sym (string->canonical-symbol "##UNASSIGNED?"))
(define **make-cell-sym (string->canonical-symbol "##MAKE-CELL"))
(define **cell-ref-sym (string->canonical-symbol "##CELL-REF"))
(define **cell-set!-sym (string->canonical-symbol "##CELL-SET!"))
(define **make-placeholder-sym (string->canonical-symbol "##MAKE-PLACEHOLDER"))
(define ieee-scheme-sym (string->canonical-symbol "IEEE-SCHEME"))
(define r4rs-scheme-sym (string->canonical-symbol "R4RS-SCHEME"))
(define multilisp-sym (string->canonical-symbol "MULTILISP"))
(define lambda-lift-sym (string->canonical-symbol "LAMBDA-LIFT"))
(define block-sym (string->canonical-symbol "BLOCK"))
(define separate-sym (string->canonical-symbol "SEPARATE"))
(define standard-bindings-sym (string->canonical-symbol "STANDARD-BINDINGS"))
(define extended-bindings-sym (string->canonical-symbol "EXTENDED-BINDINGS"))
(define safe-sym (string->canonical-symbol "SAFE"))
(define interrupts-enabled-sym (string->canonical-symbol "INTERRUPTS-ENABLED"))
(define-flag-decl ieee-scheme-sym 'dialect)
(define-flag-decl r4rs-scheme-sym 'dialect)
(define-flag-decl multilisp-sym 'dialect)
(define-boolean-decl lambda-lift-sym)
(define-flag-decl block-sym 'compilation-strategy)
(define-flag-decl separate-sym 'compilation-strategy)
(define-namable-boolean-decl standard-bindings-sym)
(define-namable-boolean-decl extended-bindings-sym)
(define-boolean-decl safe-sym)
(define-boolean-decl interrupts-enabled-sym)
(define (scheme-dialect decl)
  (declaration-value 'dialect #f ieee-scheme-sym decl))
(define (lambda-lift? decl) (declaration-value lambda-lift-sym #f #t decl))
(define (compilation-strategy decl)
  (declaration-value 'compilation-strategy #f separate-sym decl))
(define (standard-binding? name decl)
  (declaration-value standard-bindings-sym name #f decl))
(define (extended-binding? name decl)
  (declaration-value extended-bindings-sym name #f decl))
(define (add-extended-bindings decl)
  (add-decl (list extended-bindings-sym #t) decl))
(define (intrs-enabled? decl)
  (declaration-value interrupts-enabled-sym #f #t decl))
(define (add-not-interrupts-enabled decl)
  (add-decl (list interrupts-enabled-sym #f) decl))
(define (safe? decl) (declaration-value safe-sym #f #f decl))
(define (add-not-safe decl) (add-decl (list safe-sym #f) decl))
(define (dialect-specific-keywords dialect)
  (cond ((eq? dialect ieee-scheme-sym) ieee-scheme-specific-keywords)
        ((eq? dialect r4rs-scheme-sym) r4rs-scheme-specific-keywords)
        ((eq? dialect multilisp-sym) multilisp-specific-keywords)
        (else
         (compiler-internal-error
          "dialect-specific-keywords, unknown dialect"
          dialect))))
(define (dialect-specific-procedures dialect)
  (cond ((eq? dialect ieee-scheme-sym) ieee-scheme-specific-procedures)
        ((eq? dialect r4rs-scheme-sym) r4rs-scheme-specific-procedures)
        ((eq? dialect multilisp-sym) multilisp-specific-procedures)
        (else
         (compiler-internal-error
          "dialect-specific-procedures, unknown dialect"
          dialect))))
(define (make-standard-procedure x)
  (cons (string->canonical-symbol (car x)) (cdr x)))
(define (standard-procedure name decl)
  (or (assq name (dialect-specific-procedures (scheme-dialect decl)))
      (assq name common-procedures)))
(define (standard-procedure-call-pattern proc) (cdr proc))
(define ieee-scheme-specific-keywords '())
(define ieee-scheme-specific-procedures (map make-standard-procedure '()))
(define r4rs-scheme-specific-keywords (list delay-sym))
(define r4rs-scheme-specific-procedures
  (map make-standard-procedure
       '(("LIST-TAIL" 2)
         ("-" . 1)
         ("/" . 1)
         ("STRING->LIST" 1)
         ("LIST->STRING" 1)
         ("STRING-COPY" 1)
         ("STRING-FILL!" 2)
         ("VECTOR->LIST" 1)
         ("LIST->VECTOR" 1)
         ("VECTOR-FILL!" 2)
         ("FORCE" 1)
         ("WITH-INPUT-FROM-FILE" 2)
         ("WITH-OUTPUT-TO-FILE" 2)
         ("CHAR-READY?" 0 1)
         ("LOAD" 1)
         ("TRANSCRIPT-ON" 1)
         ("TRANSCRIPT-OFF" 0))))
(define multilisp-specific-keywords (list delay-sym future-sym))
(define multilisp-specific-procedures
  (map make-standard-procedure '(("FORCE" 1) ("TOUCH" 1))))
(define common-keywords
  (list quote-sym
        quasiquote-sym
        unquote-sym
        unquote-splicing-sym
        lambda-sym
        if-sym
        set!-sym
        cond-sym
        =>-sym
        else-sym
        and-sym
        or-sym
        case-sym
        let-sym
        let*-sym
        letrec-sym
        begin-sym
        do-sym
        define-sym
        **define-macro-sym
        **declare-sym
        **include-sym))
(define common-procedures
  (map make-standard-procedure
       '(("NOT" 1)
         ("BOOLEAN?" 1)
         ("EQV?" 2)
         ("EQ?" 2)
         ("EQUAL?" 2)
         ("PAIR?" 1)
         ("CONS" 2)
         ("CAR" 1)
         ("CDR" 1)
         ("SET-CAR!" 2)
         ("SET-CDR!" 2)
         ("CAAR" 1)
         ("CADR" 1)
         ("CDAR" 1)
         ("CDDR" 1)
         ("CAAAR" 1)
         ("CAADR" 1)
         ("CADAR" 1)
         ("CADDR" 1)
         ("CDAAR" 1)
         ("CDADR" 1)
         ("CDDAR" 1)
         ("CDDDR" 1)
         ("CAAAAR" 1)
         ("CAAADR" 1)
         ("CAADAR" 1)
         ("CAADDR" 1)
         ("CADAAR" 1)
         ("CADADR" 1)
         ("CADDAR" 1)
         ("CADDDR" 1)
         ("CDAAAR" 1)
         ("CDAADR" 1)
         ("CDADAR" 1)
         ("CDADDR" 1)
         ("CDDAAR" 1)
         ("CDDADR" 1)
         ("CDDDAR" 1)
         ("CDDDDR" 1)
         ("NULL?" 1)
         ("LIST?" 1)
         ("LIST" . 0)
         ("LENGTH" 1)
         ("APPEND" . 0)
         ("REVERSE" 1)
         ("LIST-REF" 2)
         ("MEMQ" 2)
         ("MEMV" 2)
         ("MEMBER" 2)
         ("ASSQ" 2)
         ("ASSV" 2)
         ("ASSOC" 2)
         ("SYMBOL?" 1)
         ("SYMBOL->STRING" 1)
         ("STRING->SYMBOL" 1)
         ("NUMBER?" 1)
         ("COMPLEX?" 1)
         ("REAL?" 1)
         ("RATIONAL?" 1)
         ("INTEGER?" 1)
         ("EXACT?" 1)
         ("INEXACT?" 1)
         ("=" . 2)
         ("<" . 2)
         (">" . 2)
         ("<=" . 2)
         (">=" . 2)
         ("ZERO?" 1)
         ("POSITIVE?" 1)
         ("NEGATIVE?" 1)
         ("ODD?" 1)
         ("EVEN?" 1)
         ("MAX" . 1)
         ("MIN" . 1)
         ("+" . 0)
         ("*" . 0)
         ("-" 1 2)
         ("/" 1 2)
         ("ABS" 1)
         ("QUOTIENT" 2)
         ("REMAINDER" 2)
         ("MODULO" 2)
         ("GCD" . 0)
         ("LCM" . 0)
         ("NUMERATOR" 1)
         ("DENOMINATOR" 1)
         ("FLOOR" 1)
         ("CEILING" 1)
         ("TRUNCATE" 1)
         ("ROUND" 1)
         ("RATIONALIZE" 2)
         ("EXP" 1)
         ("LOG" 1)
         ("SIN" 1)
         ("COS" 1)
         ("TAN" 1)
         ("ASIN" 1)
         ("ACOS" 1)
         ("ATAN" 1 2)
         ("SQRT" 1)
         ("EXPT" 2)
         ("MAKE-RECTANGULAR" 2)
         ("MAKE-POLAR" 2)
         ("REAL-PART" 1)
         ("IMAG-PART" 1)
         ("MAGNITUDE" 1)
         ("ANGLE" 1)
         ("EXACT->INEXACT" 1)
         ("INEXACT->EXACT" 1)
         ("NUMBER->STRING" 1 2)
         ("STRING->NUMBER" 1 2)
         ("CHAR?" 1)
         ("CHAR=?" 2)
         ("CHAR<?" 2)
         ("CHAR>?" 2)
         ("CHAR<=?" 2)
         ("CHAR>=?" 2)
         ("CHAR-CI=?" 2)
         ("CHAR-CI<?" 2)
         ("CHAR-CI>?" 2)
         ("CHAR-CI<=?" 2)
         ("CHAR-CI>=?" 2)
         ("CHAR-ALPHABETIC?" 1)
         ("CHAR-NUMERIC?" 1)
         ("CHAR-WHITESPACE?" 1)
         ("CHAR-UPPER-CASE?" 1)
         ("CHAR-LOWER-CASE?" 1)
         ("CHAR->INTEGER" 1)
         ("INTEGER->CHAR" 1)
         ("CHAR-UPCASE" 1)
         ("CHAR-DOWNCASE" 1)
         ("STRING?" 1)
         ("MAKE-STRING" 1 2)
         ("STRING" . 0)
         ("STRING-LENGTH" 1)
         ("STRING-REF" 2)
         ("STRING-SET!" 3)
         ("STRING=?" 2)
         ("STRING<?" 2)
         ("STRING>?" 2)
         ("STRING<=?" 2)
         ("STRING>=?" 2)
         ("STRING-CI=?" 2)
         ("STRING-CI<?" 2)
         ("STRING-CI>?" 2)
         ("STRING-CI<=?" 2)
         ("STRING-CI>=?" 2)
         ("SUBSTRING" 3)
         ("STRING-APPEND" . 0)
         ("VECTOR?" 1)
         ("MAKE-VECTOR" 1 2)
         ("VECTOR" . 0)
         ("VECTOR-LENGTH" 1)
         ("VECTOR-REF" 2)
         ("VECTOR-SET!" 3)
         ("PROCEDURE?" 1)
         ("APPLY" . 2)
         ("MAP" . 2)
         ("FOR-EACH" . 2)
         ("CALL-WITH-CURRENT-CONTINUATION" 1)
         ("CALL-WITH-INPUT-FILE" 2)
         ("CALL-WITH-OUTPUT-FILE" 2)
         ("INPUT-PORT?" 1)
         ("OUTPUT-PORT?" 1)
         ("CURRENT-INPUT-PORT" 0)
         ("CURRENT-OUTPUT-PORT" 0)
         ("OPEN-INPUT-FILE" 1)
         ("OPEN-OUTPUT-FILE" 1)
         ("CLOSE-INPUT-PORT" 1)
         ("CLOSE-OUTPUT-PORT" 1)
         ("EOF-OBJECT?" 1)
         ("READ" 0 1)
         ("READ-CHAR" 0 1)
         ("PEEK-CHAR" 0 1)
         ("WRITE" 1 2)
         ("DISPLAY" 1 2)
         ("NEWLINE" 0 1)
         ("WRITE-CHAR" 1 2))))
(define (parse-program program env module-name proc)
  (define (parse-prog program env lst proc)
    (if (null? program)
        (proc (reverse lst) env)
        (let ((source (car program)))
          (cond ((macro-expr? source env)
                 (parse-prog
                  (cons (macro-expand source env) (cdr program))
                  env
                  lst
                  proc))
                ((begin-defs-expr? source)
                 (parse-prog
                  (append (begin-defs-body source) (cdr program))
                  env
                  lst
                  proc))
                ((include-expr? source)
                 (if *ptree-port* (display "  " *ptree-port*))
                 (let ((x (file->sources*
                           (include-filename source)
                           *ptree-port*
                           (source-locat source))))
                   (if *ptree-port* (newline *ptree-port*))
                   (parse-prog (append x (cdr program)) env lst proc)))
                ((define-macro-expr? source env)
                 (if *ptree-port*
                     (begin
                       (display "  \"macro\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (parse-prog (cdr program) (add-macro source env) lst proc))
                ((declare-expr? source)
                 (if *ptree-port*
                     (begin
                       (display "  \"decl\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (parse-prog
                  (cdr program)
                  (add-declarations source env)
                  lst
                  proc))
                ((define-expr? source env)
                 (let* ((var** (definition-variable source))
                        (var* (source-code var**))
                        (var (env-lookup-var env var* var**)))
                   (if *ptree-port*
                       (begin
                         (display "  " *ptree-port*)
                         (display (var-name var) *ptree-port*)
                         (newline *ptree-port*)))
                   (let ((node (pt (definition-value source) env 'true)))
                     (set-prc-names! (list var) (list node))
                     (parse-prog
                      (cdr program)
                      env
                      (cons (cons (new-def source
                                           (env-declarations env)
                                           var
                                           node)
                                  env)
                            lst)
                      proc))))
                ((c-declaration-expr? source)
                 (if *ptree-port*
                     (begin
                       (display "  \"c-decl\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (add-c-declaration (source-code (cadr (source-code source))))
                 (parse-prog (cdr program) env lst proc))
                ((c-init-expr? source)
                 (if *ptree-port*
                     (begin
                       (display "  \"c-init\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (add-c-init (source-code (cadr (source-code source))))
                 (parse-prog (cdr program) env lst proc))
                (else
                 (if *ptree-port*
                     (begin
                       (display "  \"expr\"" *ptree-port*)
                       (newline *ptree-port*)))
                 (parse-prog
                  (cdr program)
                  env
                  (cons (cons (pt source env 'true) env) lst)
                  proc))))))
  (if *ptree-port*
      (begin (display "Parsing:" *ptree-port*) (newline *ptree-port*)))
  (c-interface-begin module-name)
  (parse-prog
   program
   env
   '()
   (lambda (lst env)
     (if *ptree-port* (newline *ptree-port*))
     (proc lst env (c-interface-end)))))
(define (c-interface-begin module-name)
  (set! c-interface-module-name module-name)
  (set! c-interface-proc-count 0)
  (set! c-interface-decls '())
  (set! c-interface-procs '())
  (set! c-interface-inits '())
  #f)
(define (c-interface-end)
  (let ((i (make-c-intf
            (reverse c-interface-decls)
            (reverse c-interface-procs)
            (reverse c-interface-inits))))
    (set! c-interface-module-name #f)
    (set! c-interface-proc-count #f)
    (set! c-interface-decls #f)
    (set! c-interface-procs #f)
    (set! c-interface-inits #f)
    i))
(define c-interface-module-name #f)
(define c-interface-proc-count #f)
(define c-interface-decls #f)
(define c-interface-procs #f)
(define c-interface-inits #f)
(define (make-c-intf decls procs inits) (vector decls procs inits))
(define (c-intf-decls c-intf) (vector-ref c-intf 0))
(define (c-intf-decls-set! c-intf x) (vector-set! c-intf 0 x))
(define (c-intf-procs c-intf) (vector-ref c-intf 1))
(define (c-intf-procs-set! c-intf x) (vector-set! c-intf 1 x))
(define (c-intf-inits c-intf) (vector-ref c-intf 2))
(define (c-intf-inits-set! c-intf x) (vector-set! c-intf 2 x))
(define (c-declaration-expr? source)
  (and (mymatch **c-declaration-sym 1 source)
       (let ((code (source-code source)))
         (or (string? (source-code (cadr code)))
             (pt-syntax-error
              source
              "Argument to '##c-declaration' must be a string")))))
(define (c-init-expr? source)
  (and (mymatch **c-init-sym 1 source)
       (let ((code (source-code source)))
         (or (string? (source-code (cadr code)))
             (pt-syntax-error
              source
              "Argument to '##c-init' must be a string")))))
(define (c-procedure-expr? source)
  (and (mymatch **c-procedure-sym 3 source)
       (let ((code (source-code source)))
         (if (not (string? (source-code (cadddr code))))
             (pt-syntax-error
              source
              "Last argument to '##c-procedure' must be a string")
             (check-arg-and-result-types source (cadr code) (caddr code))))))
(define scheme-to-c-notation
  (list (list void-sym "VOID" "void")
        (list char-sym "CHAR" "char")
        (list signed-char-sym "SCHAR" "signed char")
        (list unsigned-char-sym "UCHAR" "unsigned char")
        (list short-sym "SHORT" "short")
        (list unsigned-short-sym "USHORT" "unsigned short")
        (list int-sym "INT" "int")
        (list unsigned-int-sym "UINT" "unsigned int")
        (list long-sym "LONG" "long")
        (list unsigned-long-sym "ULONG" "unsigned long")
        (list float-sym "FLOAT" "float")
        (list double-sym "DOUBLE" "double")
        (list pointer-sym "POINTER" "void*")
        (list boolean-sym "BOOLEAN" "int")
        (list string-sym "STRING" "char*")
        (list scheme-object-sym "SCMOBJ" "long")))
(define (convert-type typ) (if (assq typ scheme-to-c-notation) typ #f))
(define (check-arg-and-result-types source arg-typs-source res-typ-source)
  (let ((arg-typs (source-code arg-typs-source))
        (res-typ (source-code res-typ-source)))
    (let ((res-type (convert-type res-typ)))
      (if (not res-type)
          (pt-syntax-error res-typ-source "Invalid result type")
          (if (not (proper-length arg-typs))
              (pt-syntax-error
               arg-typs-source
               "Ill-terminated argument type list")
              (let loop ((lst arg-typs))
                (if (pair? lst)
                    (let* ((arg-typ (source-code (car lst)))
                           (arg-type (convert-type arg-typ)))
                      (if (or (not arg-type) (eq? arg-type void-sym))
                          (pt-syntax-error (car lst) "Invalid argument type")
                          (loop (cdr lst))))
                    #t)))))))
(define (add-c-declaration declaration-string)
  (set! c-interface-decls (cons declaration-string c-interface-decls))
  #f)
(define (add-c-init initialization-code-string)
  (set! c-interface-inits (cons initialization-code-string c-interface-inits))
  #f)
(define (add-c-proc scheme-name c-name arity def)
  (set! c-interface-procs
        (cons (vector scheme-name c-name arity def) c-interface-procs))
  #f)
(define (pt-c-procedure source env use)
  (let* ((code (source-code source))
         (name (build-c-procedure
                (map source-code (source-code (cadr code)))
                (source-code (caddr code))
                (source-code (cadddr code))))
         (decl (env-declarations env)))
    (new-ref source decl (env-lookup-global-var env (string->symbol name)))))
(define (build-c-procedure argument-types result-type proc-name-or-code)
  (define proc-name?
    (let loop ((i (- (string-length proc-name-or-code) 1)))
      (if (>= i 0)
          (let ((c (string-ref proc-name-or-code i)))
            (if (or (char-alphabetic? c) (char=? c #\_)) (loop (- i 1)) #f))
          #t)))
  (define nl (string #\newline))
  (define undefined-value "UND")
  (define scheme-arg-prefix "ARG")
  (define scheme-result-name "RESULT")
  (define c-arg-prefix "arg")
  (define c-result-name "result")
  (define scheme-to-c-prefix "SCMOBJ_TO_")
  (define c-to-scheme-suffix "_TO_SCMOBJ")
  (define (c-type-name typ) (cadr (assq typ scheme-to-c-notation)))
  (define (c-type-decl typ) (caddr (assq typ scheme-to-c-notation)))
  (define (listify strings)
    (if (null? strings)
        ""
        (string-append
         (car strings)
         (apply string-append
                (map (lambda (s) (string-append "," s)) (cdr strings))))))
  (define (scheme-arg-var t)
    (string-append c-id-prefix scheme-arg-prefix (number->string (cdr t))))
  (define (c-arg-var t)
    (string-append c-id-prefix c-arg-prefix (number->string (cdr t))))
  (define (make-c-procedure arg-types res-type)
    (define (make-arg-decl)
      (apply string-append
             (map (lambda (t)
                    (string-append
                     (c-type-decl (car t))
                     " "
                     (c-arg-var t)
                     ";"
                     nl))
                  arg-types)))
    (define (make-conversions)
      (if (not (null? arg-types))
          (let loop ((lst arg-types) (str (string-append "if (" nl)))
            (if (null? lst)
                (string-append str "   )" nl)
                (let ((t (car lst)) (rest (cdr lst)))
                  (loop rest
                        (string-append
                         str
                         "    "
                         c-id-prefix
                         scheme-to-c-prefix
                         (c-type-name (car t))
                         "("
                         (scheme-arg-var t)
                         ","
                         (c-arg-var t)
                         ")"
                         (if (null? rest) "" " &&")
                         nl)))))
          ""))
    (define (make-body)
      (if proc-name?
          (let* ((param-list (listify (map c-arg-var arg-types)))
                 (call (string-append proc-name-or-code "(" param-list ")")))
            (if (eq? res-type void-sym)
                (string-append
                 "{"
                 nl
                 call
                 ";"
                 nl
                 c-id-prefix
                 scheme-result-name
                 " = "
                 c-id-prefix
                 undefined-value
                 ";"
                 nl
                 "}"
                 nl)
                (string-append
                 c-id-prefix
                 (c-type-name res-type)
                 c-to-scheme-suffix
                 "("
                 call
                 ","
                 c-id-prefix
                 scheme-result-name
                 ");"
                 nl)))
          (if (eq? res-type void-sym)
              (string-append
               "{"
               nl
               proc-name-or-code
               nl
               c-id-prefix
               scheme-result-name
               " = "
               c-id-prefix
               undefined-value
               ";"
               nl
               "}"
               nl)
              (string-append
               "{"
               nl
               proc-name-or-code
               nl
               c-id-prefix
               (c-type-name res-type)
               c-to-scheme-suffix
               "("
               c-id-prefix
               c-result-name
               ","
               c-id-prefix
               scheme-result-name
               ");"
               nl
               "}"
               nl))))
    (let* ((index (number->string c-interface-proc-count))
           (scheme-name (string-append "#!" c-interface-module-name "#" index))
           (c-name (string-append c-id-prefix (scheme-id->c-id scheme-name)))
           (arity (length argument-types))
           (def (string-append
                 (if (or proc-name? (eq? res-type void-sym))
                     ""
                     (string-append
                      (c-type-decl res-type)
                      " "
                      c-id-prefix
                      c-result-name
                      ";"
                      nl))
                 (make-arg-decl)
                 (make-conversions)
                 (make-body))))
      (set! c-interface-proc-count (+ c-interface-proc-count 1))
      (add-c-proc scheme-name c-name arity def)
      scheme-name))
  (let loop ((i 1) (lst1 argument-types) (lst2 '()))
    (if (pair? lst1)
        (loop (+ i 1) (cdr lst1) (cons (cons (car lst1) i) lst2))
        (make-c-procedure (reverse lst2) result-type))))
(define (scheme-id->c-id s)
  (define (hex->char i) (string-ref "0123456789abcdef" i))
  (let loop ((i (- (string-length s) 1)) (l '()))
    (if (>= i 0)
        (let ((c (string-ref s i)))
          (cond ((or (char-alphabetic? c) (char-numeric? c))
                 (loop (- i 1) (cons c l)))
                ((char=? c #\_) (loop (- i 1) (cons c (cons c l))))
                (else
                 (let ((n (character-encoding c)))
                   (loop (- i 1)
                         (cons #\_
                               (cons (hex->char (quotient n 16))
                                     (cons (hex->char (modulo n 16)) l))))))))
        (lst->string l))))
(define (pt-syntax-error source msg . args)
  (apply compiler-user-error
         (cons (source-locat source)
               (cons (string-append "Syntax error -- " msg) args))))
(define (pt source env use)
  (cond ((macro-expr? source env) (pt (macro-expand source env) env use))
        ((self-eval-expr? source) (pt-self-eval source env use))
        ((quote-expr? source) (pt-quote source env use))
        ((quasiquote-expr? source) (pt-quasiquote source env use))
        ((unquote-expr? source)
         (pt-syntax-error source "Ill-placed 'unquote'"))
        ((unquote-splicing-expr? source)
         (pt-syntax-error source "Ill-placed 'unquote-splicing'"))
        ((var-expr? source env) (pt-var source env use))
        ((set!-expr? source env) (pt-set! source env use))
        ((lambda-expr? source env) (pt-lambda source env use))
        ((if-expr? source) (pt-if source env use))
        ((cond-expr? source) (pt-cond source env use))
        ((and-expr? source) (pt-and source env use))
        ((or-expr? source) (pt-or source env use))
        ((case-expr? source) (pt-case source env use))
        ((let-expr? source env) (pt-let source env use))
        ((let*-expr? source env) (pt-let* source env use))
        ((letrec-expr? source env) (pt-letrec source env use))
        ((begin-expr? source) (pt-begin source env use))
        ((do-expr? source env) (pt-do source env use))
        ((define-expr? source env)
         (pt-syntax-error source "Ill-placed 'define'"))
        ((delay-expr? source env) (pt-delay source env use))
        ((future-expr? source env) (pt-future source env use))
        ((define-macro-expr? source env)
         (pt-syntax-error source "Ill-placed '##define-macro'"))
        ((begin-defs-expr? source)
         (pt-syntax-error source "Ill-placed 'begin' style definitions"))
        ((declare-expr? source)
         (pt-syntax-error source "Ill-placed '##declare'"))
        ((c-declaration-expr? source)
         (pt-syntax-error source "Ill-placed '##c-declaration'"))
        ((c-init-expr? source)
         (pt-syntax-error source "Ill-placed '##c-init'"))
        ((c-procedure-expr? source) (pt-c-procedure source env use))
        ((combination-expr? source) (pt-combination source env use))
        (else (compiler-internal-error "pt, unknown expression type" source))))
(define (macro-expand source env)
  (let ((code (source-code source)))
    (expression->source
     (apply (cdr (env-lookup-macro env (source-code (car code))))
            (cdr (source->expression source)))
     source)))
(define (pt-self-eval source env use)
  (let ((val (source->expression source)))
    (if (eq? use 'none)
        (new-cst source (env-declarations env) undef-object)
        (new-cst source (env-declarations env) val))))
(define (pt-quote source env use)
  (let ((code (source-code source)))
    (if (eq? use 'none)
        (new-cst source (env-declarations env) undef-object)
        (new-cst source
                 (env-declarations env)
                 (source->expression (cadr code))))))
(define (pt-quasiquote source env use)
  (let ((code (source-code source))) (pt-quasiquotation (cadr code) 1 env)))
(define (pt-quasiquotation form level env)
  (cond ((= level 0) (pt form env 'true))
        ((quasiquote-expr? form)
         (pt-quasiquotation-list form (source-code form) (+ level 1) env))
        ((unquote-expr? form)
         (if (= level 1)
             (pt (cadr (source-code form)) env 'true)
             (pt-quasiquotation-list form (source-code form) (- level 1) env)))
        ((unquote-splicing-expr? form)
         (if (= level 1)
             (pt-syntax-error form "Ill-placed 'unquote-splicing'")
             (pt-quasiquotation-list form (source-code form) (- level 1) env)))
        ((pair? (source-code form))
         (pt-quasiquotation-list form (source-code form) level env))
        ((vector? (source-code form))
         (vector-form
          form
          (pt-quasiquotation-list
           form
           (vector->lst (source-code form))
           level
           env)
          env))
        (else
         (new-cst form (env-declarations env) (source->expression form)))))
(define (pt-quasiquotation-list form l level env)
  (cond ((pair? l)
         (if (and (unquote-splicing-expr? (car l)) (= level 1))
             (let ((x (pt (cadr (source-code (car l))) env 'true)))
               (if (null? (cdr l))
                   x
                   (append-form
                    (car l)
                    x
                    (pt-quasiquotation-list form (cdr l) 1 env)
                    env)))
             (cons-form
              form
              (pt-quasiquotation (car l) level env)
              (pt-quasiquotation-list form (cdr l) level env)
              env)))
        ((null? l) (new-cst form (env-declarations env) '()))
        (else (pt-quasiquotation l level env))))
(define (append-form source ptree1 ptree2 env)
  (cond ((and (cst? ptree1) (cst? ptree2))
         (new-cst source
                  (env-declarations env)
                  (append (cst-val ptree1) (cst-val ptree2))))
        ((and (cst? ptree2) (null? (cst-val ptree2))) ptree1)
        (else
         (new-call*
          source
          (add-not-safe (env-declarations env))
          (new-ref-extended-bindings source **quasi-append-sym env)
          (list ptree1 ptree2)))))
(define (cons-form source ptree1 ptree2 env)
  (cond ((and (cst? ptree1) (cst? ptree2))
         (new-cst source
                  (env-declarations env)
                  (cons (cst-val ptree1) (cst-val ptree2))))
        ((and (cst? ptree2) (null? (cst-val ptree2)))
         (new-call*
          source
          (add-not-safe (env-declarations env))
          (new-ref-extended-bindings source **quasi-list-sym env)
          (list ptree1)))
        (else
         (new-call*
          source
          (add-not-safe (env-declarations env))
          (new-ref-extended-bindings source **quasi-cons-sym env)
          (list ptree1 ptree2)))))
(define (vector-form source ptree env)
  (if (cst? ptree)
      (new-cst source (env-declarations env) (lst->vector (cst-val ptree)))
      (new-call*
       source
       (add-not-safe (env-declarations env))
       (new-ref-extended-bindings source **quasi-list->vector-sym env)
       (list ptree))))
(define (pt-var source env use)
  (if (eq? use 'none)
      (new-cst source (env-declarations env) undef-object)
      (new-ref source
               (env-declarations env)
               (env-lookup-var env (source-code source) source))))
(define (pt-set! source env use)
  (let ((code (source-code source)))
    (new-set source
             (env-declarations env)
             (env-lookup-var env (source-code (cadr code)) (cadr code))
             (pt (caddr code) env 'true))))
(define (pt-lambda source env use)
  (let ((code (source-code source)))
    (define (new-params parms)
      (cond ((pair? parms)
             (let* ((parm* (car parms))
                    (parm (source-code parm*))
                    (p* (if (pair? parm) (car parm) parm*)))
               (cons (make-var (source-code p*) #t (set-empty) (set-empty) p*)
                     (new-params (cdr parms)))))
            ((null? parms) '())
            (else
             (list (make-var
                    (source-code parms)
                    #t
                    (set-empty)
                    (set-empty)
                    parms)))))
    (define (min-params parms)
      (let loop ((l parms) (n 0))
        (if (pair? l)
            (if (pair? (source-code (car l))) n (loop (cdr l) (+ n 1)))
            n)))
    (define (rest-param? parms)
      (if (pair? parms) (rest-param? (cdr parms)) (not (null? parms))))
    (define (optionals parms source body env)
      (if (pair? parms)
          (let* ((parm* (car parms)) (parm (source-code parm*)))
            (if (and (pair? parm) (length? parm 2))
                (let* ((var (car parm))
                       (vars (new-variables (list var)))
                       (decl (env-declarations env)))
                  (new-call*
                   parm*
                   decl
                   (new-prc parm*
                            decl
                            #f
                            1
                            #f
                            vars
                            (optionals
                             (cdr parms)
                             source
                             body
                             (env-frame env vars)))
                   (list (new-tst parm*
                                  decl
                                  (new-call*
                                   parm*
                                   decl
                                   (new-ref-extended-bindings
                                    parm*
                                    **unassigned?-sym
                                    env)
                                   (list (new-ref parm*
                                                  decl
                                                  (env-lookup-var
                                                   env
                                                   (source-code var)
                                                   var))))
                                  (pt (cadr parm) env 'true)
                                  (new-ref parm*
                                           decl
                                           (env-lookup-var
                                            env
                                            (source-code var)
                                            var))))))
                (optionals (cdr parms) source body env)))
          (pt-body source body env 'true)))
    (if (eq? use 'none)
        (new-cst source (env-declarations env) undef-object)
        (let* ((parms (source->parms (cadr code))) (frame (new-params parms)))
          (new-prc source
                   (env-declarations env)
                   #f
                   (min-params parms)
                   (rest-param? parms)
                   frame
                   (optionals
                    parms
                    source
                    (cddr code)
                    (env-frame env frame)))))))
(define (source->parms source)
  (let ((x (source-code source))) (if (or (pair? x) (null? x)) x source)))
(define (pt-body source body env use)
  (define (letrec-defines vars vals envs body env)
    (cond ((null? body)
           (pt-syntax-error
            source
            "Body must contain at least one evaluable expression"))
          ((macro-expr? (car body) env)
           (letrec-defines
            vars
            vals
            envs
            (cons (macro-expand (car body) env) (cdr body))
            env))
          ((begin-defs-expr? (car body))
           (letrec-defines
            vars
            vals
            envs
            (append (begin-defs-body (car body)) (cdr body))
            env))
          ((include-expr? (car body))
           (if *ptree-port* (display "  " *ptree-port*))
           (let ((x (file->sources*
                     (include-filename (car body))
                     *ptree-port*
                     (source-locat (car body)))))
             (if *ptree-port* (newline *ptree-port*))
             (letrec-defines vars vals envs (append x (cdr body)) env)))
          ((define-expr? (car body) env)
           (let* ((var** (definition-variable (car body)))
                  (var* (source-code var**))
                  (var (env-define-var env var* var**)))
             (letrec-defines
              (cons var vars)
              (cons (definition-value (car body)) vals)
              (cons env envs)
              (cdr body)
              env)))
          ((declare-expr? (car body))
           (letrec-defines
            vars
            vals
            envs
            (cdr body)
            (add-declarations (car body) env)))
          ((define-macro-expr? (car body) env)
           (letrec-defines
            vars
            vals
            envs
            (cdr body)
            (add-macro (car body) env)))
          ((c-declaration-expr? (car body))
           (add-c-declaration (source-code (cadr (source-code (car body)))))
           (letrec-defines vars vals envs (cdr body) env))
          ((c-init-expr? (car body))
           (add-c-init (source-code (cadr (source-code (car body)))))
           (letrec-defines vars vals envs (cdr body) env))
          ((null? vars) (pt-sequence source body env use))
          (else
           (let ((vars* (reverse vars)))
             (let loop ((vals* '()) (l1 vals) (l2 envs))
               (if (not (null? l1))
                   (loop (cons (pt (car l1) (car l2) 'true) vals*)
                         (cdr l1)
                         (cdr l2))
                   (pt-recursive-let source vars* vals* body env use)))))))
  (letrec-defines '() '() '() body (env-frame env '())))
(define (pt-sequence source seq env use)
  (if (length? seq 1)
      (pt (car seq) env use)
      (new-seq source
               (env-declarations env)
               (pt (car seq) env 'none)
               (pt-sequence source (cdr seq) env use))))
(define (pt-if source env use)
  (let ((code (source-code source)))
    (new-tst source
             (env-declarations env)
             (pt (cadr code) env 'pred)
             (pt (caddr code) env use)
             (if (length? code 3)
                 (new-cst source (env-declarations env) undef-object)
                 (pt (cadddr code) env use)))))
(define (pt-cond source env use)
  (define (pt-clauses clauses)
    (if (length? clauses 0)
        (new-cst source (env-declarations env) undef-object)
        (let* ((clause* (car clauses)) (clause (source-code clause*)))
          (cond ((eq? (source-code (car clause)) else-sym)
                 (pt-sequence clause* (cdr clause) env use))
                ((length? clause 1)
                 (new-disj
                  clause*
                  (env-declarations env)
                  (pt (car clause) env (if (eq? use 'true) 'true 'pred))
                  (pt-clauses (cdr clauses))))
                ((eq? (source-code (cadr clause)) =>-sym)
                 (new-disj-call
                  clause*
                  (env-declarations env)
                  (pt (car clause) env 'true)
                  (pt (caddr clause) env 'true)
                  (pt-clauses (cdr clauses))))
                (else
                 (new-tst clause*
                          (env-declarations env)
                          (pt (car clause) env 'pred)
                          (pt-sequence clause* (cdr clause) env use)
                          (pt-clauses (cdr clauses))))))))
  (pt-clauses (cdr (source-code source))))
(define (pt-and source env use)
  (define (pt-exprs exprs)
    (cond ((length? exprs 0) (new-cst source (env-declarations env) #t))
          ((length? exprs 1) (pt (car exprs) env use))
          (else
           (new-conj
            (car exprs)
            (env-declarations env)
            (pt (car exprs) env (if (eq? use 'true) 'true 'pred))
            (pt-exprs (cdr exprs))))))
  (pt-exprs (cdr (source-code source))))
(define (pt-or source env use)
  (define (pt-exprs exprs)
    (cond ((length? exprs 0)
           (new-cst source (env-declarations env) false-object))
          ((length? exprs 1) (pt (car exprs) env use))
          (else
           (new-disj
            (car exprs)
            (env-declarations env)
            (pt (car exprs) env (if (eq? use 'true) 'true 'pred))
            (pt-exprs (cdr exprs))))))
  (pt-exprs (cdr (source-code source))))
(define (pt-case source env use)
  (let ((code (source-code source)) (temp (new-temps source '(temp))))
    (define (pt-clauses clauses)
      (if (length? clauses 0)
          (new-cst source (env-declarations env) undef-object)
          (let* ((clause* (car clauses)) (clause (source-code clause*)))
            (if (eq? (source-code (car clause)) else-sym)
                (pt-sequence clause* (cdr clause) env use)
                (new-tst clause*
                         (env-declarations env)
                         (new-call*
                          clause*
                          (add-not-safe (env-declarations env))
                          (new-ref-extended-bindings
                           clause*
                           **case-memv-sym
                           env)
                          (list (new-ref clause*
                                         (env-declarations env)
                                         (car temp))
                                (new-cst (car clause)
                                         (env-declarations env)
                                         (source->expression (car clause)))))
                         (pt-sequence clause* (cdr clause) env use)
                         (pt-clauses (cdr clauses)))))))
    (new-call*
     source
     (env-declarations env)
     (new-prc source
              (env-declarations env)
              #f
              1
              #f
              temp
              (pt-clauses (cddr code)))
     (list (pt (cadr code) env 'true)))))
(define (pt-let source env use)
  (let ((code (source-code source)))
    (if (bindable-var? (cadr code) env)
        (let* ((self (new-variables (list (cadr code))))
               (bindings (map source-code (source-code (caddr code))))
               (vars (new-variables (map car bindings)))
               (vals (map (lambda (x) (pt (cadr x) env 'true)) bindings))
               (env (env-frame (env-frame env vars) self))
               (self-proc
                (list (new-prc source
                               (env-declarations env)
                               #f
                               (length vars)
                               #f
                               vars
                               (pt-body source (cdddr code) env use)))))
          (set-prc-names! self self-proc)
          (set-prc-names! vars vals)
          (new-call*
           source
           (env-declarations env)
           (new-prc source
                    (env-declarations env)
                    #f
                    1
                    #f
                    self
                    (new-call*
                     source
                     (env-declarations env)
                     (new-ref source (env-declarations env) (car self))
                     vals))
           self-proc))
        (if (null? (source-code (cadr code)))
            (pt-body source (cddr code) env use)
            (let* ((bindings (map source-code (source-code (cadr code))))
                   (vars (new-variables (map car bindings)))
                   (vals (map (lambda (x) (pt (cadr x) env 'true)) bindings))
                   (env (env-frame env vars)))
              (set-prc-names! vars vals)
              (new-call*
               source
               (env-declarations env)
               (new-prc source
                        (env-declarations env)
                        #f
                        (length vars)
                        #f
                        vars
                        (pt-body source (cddr code) env use))
               vals))))))
(define (pt-let* source env use)
  (let ((code (source-code source)))
    (define (pt-bindings bindings env use)
      (if (null? bindings)
          (pt-body source (cddr code) env use)
          (let* ((binding* (car bindings))
                 (binding (source-code binding*))
                 (vars (new-variables (list (car binding))))
                 (vals (list (pt (cadr binding) env 'true)))
                 (env (env-frame env vars)))
            (set-prc-names! vars vals)
            (new-call*
             binding*
             (env-declarations env)
             (new-prc binding*
                      (env-declarations env)
                      #f
                      1
                      #f
                      vars
                      (pt-bindings (cdr bindings) env use))
             vals))))
    (pt-bindings (source-code (cadr code)) env use)))
(define (pt-letrec source env use)
  (let* ((code (source-code source))
         (bindings (map source-code (source-code (cadr code))))
         (vars* (new-variables (map car bindings)))
         (env* (env-frame env vars*)))
    (pt-recursive-let
     source
     vars*
     (map (lambda (x) (pt (cadr x) env* 'true)) bindings)
     (cddr code)
     env*
     use)))
(define (pt-recursive-let source vars vals body env use)
  (define (dependency-graph vars vals)
    (define (dgraph vars* vals*)
      (if (null? vars*)
          (set-empty)
          (let ((var (car vars*)) (val (car vals*)))
            (set-adjoin
             (dgraph (cdr vars*) (cdr vals*))
             (make-gnode
              var
              (set-intersection (list->set vars) (free-variables val)))))))
    (dgraph vars vals))
  (define (val-of var)
    (list-ref vals (- (length vars) (length (memq var vars)))))
  (define (bind-in-order order)
    (if (null? order)
        (pt-body source body env use)
        (let* ((vars-set (car order)) (vars (set->list vars-set)))
          (let loop1 ((l (reverse vars))
                      (vars-b '())
                      (vals-b '())
                      (vars-a '()))
            (if (not (null? l))
                (let* ((var (car l)) (val (val-of var)))
                  (if (or (prc? val)
                          (set-empty?
                           (set-intersection (free-variables val) vars-set)))
                      (loop1 (cdr l)
                             (cons var vars-b)
                             (cons val vals-b)
                             vars-a)
                      (loop1 (cdr l) vars-b vals-b (cons var vars-a))))
                (let* ((result1 (let loop2 ((l vars-a))
                                  (if (not (null? l))
                                      (let* ((var (car l)) (val (val-of var)))
                                        (new-seq source
                                                 (env-declarations env)
                                                 (new-set source
                                                          (env-declarations
                                                           env)
                                                          var
                                                          val)
                                                 (loop2 (cdr l))))
                                      (bind-in-order (cdr order)))))
                       (result2 (if (null? vars-b)
                                    result1
                                    (new-call*
                                     source
                                     (env-declarations env)
                                     (new-prc source
                                              (env-declarations env)
                                              #f
                                              (length vars-b)
                                              #f
                                              vars-b
                                              result1)
                                     vals-b)))
                       (result3 (if (null? vars-a)
                                    result2
                                    (new-call*
                                     source
                                     (env-declarations env)
                                     (new-prc source
                                              (env-declarations env)
                                              #f
                                              (length vars-a)
                                              #f
                                              vars-a
                                              result2)
                                     (map (lambda (var)
                                            (new-cst source
                                                     (env-declarations env)
                                                     undef-object))
                                          vars-a)))))
                  result3))))))
  (set-prc-names! vars vals)
  (bind-in-order
   (topological-sort (transitive-closure (dependency-graph vars vals)))))
(define (pt-begin source env use)
  (pt-sequence source (cdr (source-code source)) env use))
(define (pt-do source env use)
  (let* ((code (source-code source))
         (loop (new-temps source '(loop)))
         (bindings (map source-code (source-code (cadr code))))
         (vars (new-variables (map car bindings)))
         (init (map (lambda (x) (pt (cadr x) env 'true)) bindings))
         (env (env-frame env vars))
         (step (map (lambda (x)
                      (pt (if (length? x 2) (car x) (caddr x)) env 'true))
                    bindings))
         (exit (source-code (caddr code))))
    (set-prc-names! vars init)
    (new-call*
     source
     (env-declarations env)
     (new-prc source
              (env-declarations env)
              #f
              1
              #f
              loop
              (new-call*
               source
               (env-declarations env)
               (new-ref source (env-declarations env) (car loop))
               init))
     (list (new-prc source
                    (env-declarations env)
                    #f
                    (length vars)
                    #f
                    vars
                    (new-tst source
                             (env-declarations env)
                             (pt (car exit) env 'pred)
                             (if (length? exit 1)
                                 (new-cst (caddr code)
                                          (env-declarations env)
                                          undef-object)
                                 (pt-sequence (caddr code) (cdr exit) env use))
                             (if (length? code 3)
                                 (new-call*
                                  source
                                  (env-declarations env)
                                  (new-ref source
                                           (env-declarations env)
                                           (car loop))
                                  step)
                                 (new-seq source
                                          (env-declarations env)
                                          (pt-sequence
                                           source
                                           (cdddr code)
                                           env
                                           'none)
                                          (new-call*
                                           source
                                           (env-declarations env)
                                           (new-ref source
                                                    (env-declarations env)
                                                    (car loop))
                                           step)))))))))
(define (pt-combination source env use)
  (let* ((code (source-code source))
         (oper (pt (car code) env 'true))
         (decl (node-decl oper)))
    (new-call*
     source
     (env-declarations env)
     oper
     (map (lambda (x) (pt x env 'true)) (cdr code)))))
(define (pt-delay source env use)
  (let ((code (source-code source)))
    (new-call*
     source
     (add-not-safe (env-declarations env))
     (new-ref-extended-bindings source **make-placeholder-sym env)
     (list (new-prc source
                    (env-declarations env)
                    #f
                    0
                    #f
                    '()
                    (pt (cadr code) env 'true))))))
(define (pt-future source env use)
  (let ((decl (env-declarations env)) (code (source-code source)))
    (new-fut source decl (pt (cadr code) env 'true))))
(define (self-eval-expr? source)
  (let ((code (source-code source)))
    (and (not (pair? code)) (not (symbol-object? code)))))
(define (quote-expr? source) (mymatch quote-sym 1 source))
(define (quasiquote-expr? source) (mymatch quasiquote-sym 1 source))
(define (unquote-expr? source) (mymatch unquote-sym 1 source))
(define (unquote-splicing-expr? source)
  (mymatch unquote-splicing-sym 1 source))
(define (var-expr? source env)
  (let ((code (source-code source)))
    (and (symbol-object? code)
         (not-keyword source env code)
         (not-macro source env code))))
(define (not-macro source env name)
  (if (env-lookup-macro env name)
      (pt-syntax-error source "Macro name can't be used as a variable:" name)
      #t))
(define (bindable-var? source env)
  (let ((code (source-code source)))
    (and (symbol-object? code) (not-keyword source env code))))
(define (not-keyword source env name)
  (if (or (memq name common-keywords)
          (memq name
                (dialect-specific-keywords
                 (scheme-dialect (env-declarations env)))))
      (pt-syntax-error
       source
       "Predefined keyword can't be used as a variable:"
       name)
      #t))
(define (set!-expr? source env)
  (and (mymatch set!-sym 2 source)
       (var-expr? (cadr (source-code source)) env)))
(define (lambda-expr? source env)
  (and (mymatch lambda-sym -2 source)
       (proper-parms? (source->parms (cadr (source-code source))) env)))
(define (if-expr? source)
  (and (mymatch if-sym -2 source)
       (or (<= (length (source-code source)) 4)
           (pt-syntax-error source "Ill-formed special form" if-sym))))
(define (cond-expr? source)
  (and (mymatch cond-sym -1 source) (proper-clauses? source)))
(define (and-expr? source) (mymatch and-sym 0 source))
(define (or-expr? source) (mymatch or-sym 0 source))
(define (case-expr? source)
  (and (mymatch case-sym -2 source) (proper-case-clauses? source)))
(define (let-expr? source env)
  (and (mymatch let-sym -2 source)
       (let ((code (source-code source)))
         (if (bindable-var? (cadr code) env)
             (and (proper-bindings? (caddr code) #t env)
                  (or (> (length code) 3)
                      (pt-syntax-error source "Ill-formed named 'let'")))
             (proper-bindings? (cadr code) #t env)))))
(define (let*-expr? source env)
  (and (mymatch let*-sym -2 source)
       (proper-bindings? (cadr (source-code source)) #f env)))
(define (letrec-expr? source env)
  (and (mymatch letrec-sym -2 source)
       (proper-bindings? (cadr (source-code source)) #t env)))
(define (begin-expr? source) (mymatch begin-sym -1 source))
(define (do-expr? source env)
  (and (mymatch do-sym -2 source)
       (proper-do-bindings? source env)
       (proper-do-exit? source)))
(define (define-expr? source env)
  (and (mymatch define-sym -1 source)
       (proper-definition? source env)
       (let ((v (definition-variable source)))
         (not-macro v env (source-code v)))))
(define (combination-expr? source)
  (let ((length (proper-length (source-code source))))
    (if length
        (or (> length 0) (pt-syntax-error source "Ill-formed procedure call"))
        (pt-syntax-error source "Ill-terminated procedure call"))))
(define (delay-expr? source env)
  (and (not (eq? (scheme-dialect (env-declarations env)) ieee-scheme-sym))
       (mymatch delay-sym 1 source)))
(define (future-expr? source env)
  (and (eq? (scheme-dialect (env-declarations env)) multilisp-sym)
       (mymatch future-sym 1 source)))
(define (macro-expr? source env)
  (let ((code (source-code source)))
    (and (pair? code)
         (symbol-object? (source-code (car code)))
         (let ((macr (env-lookup-macro env (source-code (car code)))))
           (and macr
                (let ((len (proper-length (cdr code))))
                  (if len
                      (let ((len* (+ len 1)) (size (car macr)))
                        (or (if (> size 0) (= len* size) (>= len* (- size)))
                            (pt-syntax-error source "Ill-formed macro form")))
                      (pt-syntax-error
                       source
                       "Ill-terminated macro form"))))))))
(define (define-macro-expr? source env)
  (and (mymatch **define-macro-sym -1 source) (proper-definition? source env)))
(define (declare-expr? source) (mymatch **declare-sym -1 source))
(define (include-expr? source) (mymatch **include-sym 1 source))
(define (begin-defs-expr? source) (mymatch begin-sym 0 source))
(define (mymatch keyword size source)
  (let ((code (source-code source)))
    (and (pair? code)
         (eq? (source-code (car code)) keyword)
         (let ((length (proper-length (cdr code))))
           (if length
               (or (if (> size 0) (= length size) (>= length (- size)))
                   (pt-syntax-error source "Ill-formed special form" keyword))
               (pt-syntax-error
                source
                "Ill-terminated special form"
                keyword))))))
(define (proper-length l)
  (define (length l n)
    (cond ((pair? l) (length (cdr l) (+ n 1))) ((null? l) n) (else #f)))
  (length l 0))
(define (proper-definition? source env)
  (let* ((code (source-code source))
         (pattern* (cadr code))
         (pattern (source-code pattern*))
         (body (cddr code)))
    (cond ((bindable-var? pattern* env)
           (cond ((length? body 0) #t)
                 ((length? body 1) #t)
                 (else (pt-syntax-error source "Ill-formed definition body"))))
          ((pair? pattern)
           (if (length? body 0)
               (pt-syntax-error
                source
                "Body of a definition must have at least one expression"))
           (if (bindable-var? (car pattern) env)
               (proper-parms? (cdr pattern) env)
               (pt-syntax-error
                (car pattern)
                "Procedure name must be an identifier")))
          (else (pt-syntax-error pattern* "Ill-formed definition pattern")))))
(define (definition-variable def)
  (let* ((code (source-code def)) (pattern (cadr code)))
    (if (pair? (source-code pattern)) (car (source-code pattern)) pattern)))
(define (definition-value def)
  (let ((code (source-code def)) (loc (source-locat def)))
    (cond ((pair? (source-code (cadr code)))
           (make-source
            (cons (make-source lambda-sym loc)
                  (cons (parms->source (cdr (source-code (cadr code))) loc)
                        (cddr code)))
            loc))
          ((null? (cddr code))
           (make-source
            (list (make-source quote-sym loc) (make-source undef-object loc))
            loc))
          (else (caddr code)))))
(define (parms->source parms loc)
  (if (or (pair? parms) (null? parms)) (make-source parms loc) parms))
(define (proper-parms? parms env)
  (define (proper-parms parms seen optional-seen)
    (cond ((pair? parms)
           (let* ((parm* (car parms)) (parm (source-code parm*)))
             (cond ((pair? parm)
                    (if (eq? (scheme-dialect (env-declarations env))
                             multilisp-sym)
                        (let ((length (proper-length parm)))
                          (if (or (eqv? length 1) (eqv? length 2))
                              (let ((var (car parm)))
                                (if (bindable-var? var env)
                                    (if (memq (source-code var) seen)
                                        (pt-syntax-error
                                         var
                                         "Duplicate parameter in parameter list")
                                        (proper-parms
                                         (cdr parms)
                                         (cons (source-code var) seen)
                                         #t))
                                    (pt-syntax-error
                                     var
                                     "Parameter must be an identifier")))
                              (pt-syntax-error
                               parm*
                               "Ill-formed optional parameter")))
                        (pt-syntax-error
                         parm*
                         "optional parameters illegal in this dialect")))
                   (optional-seen
                    (pt-syntax-error parm* "Optional parameter expected"))
                   ((bindable-var? parm* env)
                    (if (memq parm seen)
                        (pt-syntax-error
                         parm*
                         "Duplicate parameter in parameter list"))
                    (proper-parms (cdr parms) (cons parm seen) #f))
                   (else
                    (pt-syntax-error
                     parm*
                     "Parameter must be an identifier")))))
          ((null? parms) #t)
          ((bindable-var? parms env)
           (if (memq (source-code parms) seen)
               (pt-syntax-error parms "Duplicate parameter in parameter list")
               #t))
          (else
           (pt-syntax-error parms "Rest parameter must be an identifier"))))
  (proper-parms parms '() #f))
(define (proper-clauses? source)
  (define (proper-clauses clauses)
    (or (null? clauses)
        (let* ((clause* (car clauses))
               (clause (source-code clause*))
               (length (proper-length clause)))
          (if length
              (if (>= length 1)
                  (if (eq? (source-code (car clause)) else-sym)
                      (cond ((= length 1)
                             (pt-syntax-error
                              clause*
                              "Else clause must have a body"))
                            ((not (null? (cdr clauses)))
                             (pt-syntax-error
                              clause*
                              "Else clause must be the last clause"))
                            (else (proper-clauses (cdr clauses))))
                      (if (and (>= length 2)
                               (eq? (source-code (cadr clause)) =>-sym)
                               (not (= length 3)))
                          (pt-syntax-error
                           (cadr clause)
                           "'=>' must be followed by a single expression")
                          (proper-clauses (cdr clauses))))
                  (pt-syntax-error clause* "Ill-formed 'cond' clause"))
              (pt-syntax-error clause* "Ill-terminated 'cond' clause")))))
  (proper-clauses (cdr (source-code source))))
(define (proper-case-clauses? source)
  (define (proper-case-clauses clauses)
    (or (null? clauses)
        (let* ((clause* (car clauses))
               (clause (source-code clause*))
               (length (proper-length clause)))
          (if length
              (if (>= length 2)
                  (if (eq? (source-code (car clause)) else-sym)
                      (if (not (null? (cdr clauses)))
                          (pt-syntax-error
                           clause*
                           "Else clause must be the last clause")
                          (proper-case-clauses (cdr clauses)))
                      (begin
                        (proper-selector-list? (car clause))
                        (proper-case-clauses (cdr clauses))))
                  (pt-syntax-error
                   clause*
                   "A 'case' clause must have a selector list and a body"))
              (pt-syntax-error clause* "Ill-terminated 'case' clause")))))
  (proper-case-clauses (cddr (source-code source))))
(define (proper-selector-list? source)
  (let* ((code (source-code source)) (length (proper-length code)))
    (if length
        (or (>= length 1)
            (pt-syntax-error
             source
             "Selector list must contain at least one element"))
        (pt-syntax-error source "Ill-terminated selector list"))))
(define (proper-bindings? bindings check-dupl? env)
  (define (proper-bindings l seen)
    (cond ((pair? l)
           (let* ((binding* (car l)) (binding (source-code binding*)))
             (if (eqv? (proper-length binding) 2)
                 (let ((var (car binding)))
                   (if (bindable-var? var env)
                       (if (and check-dupl? (memq (source-code var) seen))
                           (pt-syntax-error
                            var
                            "Duplicate variable in bindings")
                           (proper-bindings
                            (cdr l)
                            (cons (source-code var) seen)))
                       (pt-syntax-error
                        var
                        "Binding variable must be an identifier")))
                 (pt-syntax-error binding* "Ill-formed binding"))))
          ((null? l) #t)
          (else (pt-syntax-error bindings "Ill-terminated binding list"))))
  (proper-bindings (source-code bindings) '()))
(define (proper-do-bindings? source env)
  (let ((bindings (cadr (source-code source))))
    (define (proper-bindings l seen)
      (cond ((pair? l)
             (let* ((binding* (car l))
                    (binding (source-code binding*))
                    (length (proper-length binding)))
               (if (or (eqv? length 2) (eqv? length 3))
                   (let ((var (car binding)))
                     (if (bindable-var? var env)
                         (if (memq (source-code var) seen)
                             (pt-syntax-error
                              var
                              "Duplicate variable in bindings")
                             (proper-bindings
                              (cdr l)
                              (cons (source-code var) seen)))
                         (pt-syntax-error
                          var
                          "Binding variable must be an identifier")))
                   (pt-syntax-error binding* "Ill-formed binding"))))
            ((null? l) #t)
            (else (pt-syntax-error bindings "Ill-terminated binding list"))))
    (proper-bindings (source-code bindings) '())))
(define (proper-do-exit? source)
  (let* ((code (source-code (caddr (source-code source))))
         (length (proper-length code)))
    (if length
        (or (> length 0) (pt-syntax-error source "Ill-formed exit clause"))
        (pt-syntax-error source "Ill-terminated exit clause"))))
(define (include-filename source) (source-code (cadr (source-code source))))
(define (begin-defs-body source) (cdr (source-code source)))
(define (length? l n)
  (cond ((null? l) (= n 0)) ((> n 0) (length? (cdr l) (- n 1))) (else #f)))
(define (transform-declaration source)
  (let ((code (source-code source)))
    (if (not (pair? code))
        (pt-syntax-error source "Ill-formed declaration")
        (let* ((pos (not (eq? (source-code (car code)) not-sym)))
               (x (if pos code (cdr code))))
          (if (not (pair? x))
              (pt-syntax-error source "Ill-formed declaration")
              (let* ((id* (car x)) (id (source-code id*)))
                (cond ((not (symbol-object? id))
                       (pt-syntax-error
                        id*
                        "Declaration name must be an identifier"))
                      ((assq id flag-declarations)
                       (cond ((not pos)
                              (pt-syntax-error
                               id*
                               "Declaration can't be negated"))
                             ((null? (cdr x))
                              (flag-decl
                               source
                               (cdr (assq id flag-declarations))
                               id))
                             (else
                              (pt-syntax-error
                               source
                               "Ill-formed declaration"))))
                      ((memq id parameterized-declarations)
                       (cond ((not pos)
                              (pt-syntax-error
                               id*
                               "Declaration can't be negated"))
                             ((eqv? (proper-length x) 2)
                              (parameterized-decl
                               source
                               id
                               (source->expression (cadr x))))
                             (else
                              (pt-syntax-error
                               source
                               "Ill-formed declaration"))))
                      ((memq id boolean-declarations)
                       (if (null? (cdr x))
                           (boolean-decl source id pos)
                           (pt-syntax-error source "Ill-formed declaration")))
                      ((assq id namable-declarations)
                       (cond ((not pos)
                              (pt-syntax-error
                               id*
                               "Declaration can't be negated"))
                             (else
                              (namable-decl
                               source
                               (cdr (assq id namable-declarations))
                               id
                               (map source->expression (cdr x))))))
                      ((memq id namable-boolean-declarations)
                       (namable-boolean-decl
                        source
                        id
                        pos
                        (map source->expression (cdr x))))
                      ((memq id namable-string-declarations)
                       (if (not (pair? (cdr x)))
                           (pt-syntax-error source "Ill-formed declaration")
                           (let* ((str* (cadr x)) (str (source-code str*)))
                             (cond ((not pos)
                                    (pt-syntax-error
                                     id*
                                     "Declaration can't be negated"))
                                   ((not (string? str))
                                    (pt-syntax-error str* "String expected"))
                                   (else
                                    (namable-string-decl
                                     source
                                     id
                                     str
                                     (map source->expression (cddr x))))))))
                      (else (pt-syntax-error id* "Unknown declaration")))))))))
(define (add-declarations source env)
  (let loop ((l (cdr (source-code source))) (env env))
    (if (pair? l)
        (loop (cdr l) (env-declare env (transform-declaration (car l))))
        env)))
(define (add-decl d decl) (env-declare decl d))
(define (add-macro source env)
  (define (form-size parms)
    (let loop ((l parms) (n 1))
      (if (pair? l) (loop (cdr l) (+ n 1)) (if (null? l) n (- n)))))
  (define (error-proc . msgs)
    (apply compiler-user-error
           (cons (source-locat source) (cons "(in macro body)" msgs))))
  (let ((var (definition-variable source)) (proc (definition-value source)))
    (if (lambda-expr? proc env)
        (env-macro
         env
         (source-code var)
         (cons (form-size (source->parms (cadr (source-code proc))))
               (scheme-global-eval (source->expression proc) error-proc)))
        (pt-syntax-error source "Macro value must be a lambda expression"))))
(define (ptree.begin! info-port) (set! *ptree-port* info-port) '())
(define (ptree.end!) '())
(define *ptree-port* '())
(define (normalize-parse-tree ptree env)
  (define (normalize ptree)
    (let ((tree (assignment-convert (partial-evaluate ptree) env)))
      (lambda-lift! tree)
      tree))
  (if (def? ptree)
      (begin
        (node-children-set! ptree (list (normalize (def-val ptree))))
        ptree)
      (normalize ptree)))
(define (partial-evaluate ptree) (pe ptree '()))
(define (pe ptree consts)
  (cond ((cst? ptree)
         (new-cst (node-source ptree) (node-decl ptree) (cst-val ptree)))
        ((ref? ptree)
         (let ((var (ref-var ptree)))
           (var-refs-set! var (set-remove (var-refs var) ptree))
           (let ((x (assq var consts)))
             (if x
                 (new-cst (node-source ptree) (node-decl ptree) (cdr x))
                 (let ((y (global-val var)))
                   (if (and y (cst? y))
                       (new-cst (node-source ptree)
                                (node-decl ptree)
                                (cst-val y))
                       (new-ref (node-source ptree)
                                (node-decl ptree)
                                var)))))))
        ((set? ptree)
         (let ((var (set-var ptree)) (val (pe (set-val ptree) consts)))
           (var-sets-set! var (set-remove (var-sets var) ptree))
           (new-set (node-source ptree) (node-decl ptree) var val)))
        ((tst? ptree)
         (let ((pre (pe (tst-pre ptree) consts)))
           (if (cst? pre)
               (let ((val (cst-val pre)))
                 (if (false-object? val)
                     (pe (tst-alt ptree) consts)
                     (pe (tst-con ptree) consts)))
               (new-tst (node-source ptree)
                        (node-decl ptree)
                        pre
                        (pe (tst-con ptree) consts)
                        (pe (tst-alt ptree) consts)))))
        ((conj? ptree)
         (let ((pre (pe (conj-pre ptree) consts)))
           (if (cst? pre)
               (let ((val (cst-val pre)))
                 (if (false-object? val) pre (pe (conj-alt ptree) consts)))
               (new-conj
                (node-source ptree)
                (node-decl ptree)
                pre
                (pe (conj-alt ptree) consts)))))
        ((disj? ptree)
         (let ((pre (pe (disj-pre ptree) consts)))
           (if (cst? pre)
               (let ((val (cst-val pre)))
                 (if (false-object? val) (pe (disj-alt ptree) consts) pre))
               (new-disj
                (node-source ptree)
                (node-decl ptree)
                pre
                (pe (disj-alt ptree) consts)))))
        ((prc? ptree)
         (new-prc (node-source ptree)
                  (node-decl ptree)
                  (prc-name ptree)
                  (prc-min ptree)
                  (prc-rest ptree)
                  (prc-parms ptree)
                  (pe (prc-body ptree) consts)))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (= (length (prc-parms oper)) (length args)))
               (pe-let ptree consts)
               (new-call
                (node-source ptree)
                (node-decl ptree)
                (pe oper consts)
                (map (lambda (x) (pe x consts)) args)))))
        ((fut? ptree)
         (new-fut (node-source ptree)
                  (node-decl ptree)
                  (pe (fut-val ptree) consts)))
        (else (compiler-internal-error "pe, unknown parse tree node type"))))
(define (pe-let ptree consts)
  (let* ((proc (app-oper ptree))
         (vals (app-args ptree))
         (vars (prc-parms proc))
         (non-mut-vars (set-keep not-mutable? (list->set vars))))
    (for-each
     (lambda (var)
       (var-refs-set! var (set-empty))
       (var-sets-set! var (set-empty)))
     vars)
    (let loop ((l vars)
               (v vals)
               (new-vars '())
               (new-vals '())
               (new-consts consts))
      (if (null? l)
          (if (null? new-vars)
              (pe (prc-body proc) new-consts)
              (new-call
               (node-source ptree)
               (node-decl ptree)
               (new-prc (node-source proc)
                        (node-decl proc)
                        #f
                        (length new-vars)
                        #f
                        (reverse new-vars)
                        (pe (prc-body proc) new-consts))
               (reverse new-vals)))
          (let ((var (car l)) (val (pe (car v) consts)))
            (if (and (set-member? var non-mut-vars) (cst? val))
                (loop (cdr l)
                      (cdr v)
                      new-vars
                      new-vals
                      (cons (cons var (cst-val val)) new-consts))
                (loop (cdr l)
                      (cdr v)
                      (cons var new-vars)
                      (cons val new-vals)
                      new-consts)))))))
(define (assignment-convert ptree env)
  (ac ptree (env-declare env (list safe-sym #f)) '()))
(define (ac ptree env mut)
  (cond ((cst? ptree) ptree)
        ((ref? ptree)
         (let ((var (ref-var ptree)))
           (if (global? var)
               ptree
               (let ((x (assq var mut)))
                 (if x
                     (let ((source (node-source ptree)))
                       (var-refs-set! var (set-remove (var-refs var) ptree))
                       (new-call
                        source
                        (node-decl ptree)
                        (new-ref-extended-bindings source **cell-ref-sym env)
                        (list (new-ref source (node-decl ptree) (cdr x)))))
                     ptree)))))
        ((set? ptree)
         (let ((var (set-var ptree))
               (source (node-source ptree))
               (val (ac (set-val ptree) env mut)))
           (var-sets-set! var (set-remove (var-sets var) ptree))
           (if (global? var)
               (new-set source (node-decl ptree) var val)
               (new-call
                source
                (node-decl ptree)
                (new-ref-extended-bindings source **cell-set!-sym env)
                (list (new-ref source (node-decl ptree) (cdr (assq var mut)))
                      val)))))
        ((tst? ptree)
         (new-tst (node-source ptree)
                  (node-decl ptree)
                  (ac (tst-pre ptree) env mut)
                  (ac (tst-con ptree) env mut)
                  (ac (tst-alt ptree) env mut)))
        ((conj? ptree)
         (new-conj
          (node-source ptree)
          (node-decl ptree)
          (ac (conj-pre ptree) env mut)
          (ac (conj-alt ptree) env mut)))
        ((disj? ptree)
         (new-disj
          (node-source ptree)
          (node-decl ptree)
          (ac (disj-pre ptree) env mut)
          (ac (disj-alt ptree) env mut)))
        ((prc? ptree) (ac-proc ptree env mut))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (= (length (prc-parms oper)) (length args)))
               (ac-let ptree env mut)
               (new-call
                (node-source ptree)
                (node-decl ptree)
                (ac oper env mut)
                (map (lambda (x) (ac x env mut)) args)))))
        ((fut? ptree)
         (new-fut (node-source ptree)
                  (node-decl ptree)
                  (ac (fut-val ptree) env mut)))
        (else (compiler-internal-error "ac, unknown parse tree node type"))))
(define (ac-proc ptree env mut)
  (let* ((mut-parms (ac-mutables (prc-parms ptree)))
         (mut-parms-copies (map var-copy mut-parms))
         (mut (append (pair-up mut-parms mut-parms-copies) mut))
         (new-body (ac (prc-body ptree) env mut)))
    (new-prc (node-source ptree)
             (node-decl ptree)
             (prc-name ptree)
             (prc-min ptree)
             (prc-rest ptree)
             (prc-parms ptree)
             (if (null? mut-parms)
                 new-body
                 (new-call
                  (node-source ptree)
                  (node-decl ptree)
                  (new-prc (node-source ptree)
                           (node-decl ptree)
                           #f
                           (length mut-parms-copies)
                           #f
                           mut-parms-copies
                           new-body)
                  (map (lambda (var)
                         (new-call
                          (var-source var)
                          (node-decl ptree)
                          (new-ref-extended-bindings
                           (var-source var)
                           **make-cell-sym
                           env)
                          (list (new-ref (var-source var)
                                         (node-decl ptree)
                                         var))))
                       mut-parms))))))
(define (ac-let ptree env mut)
  (let* ((proc (app-oper ptree))
         (vals (app-args ptree))
         (vars (prc-parms proc))
         (vals-fv (apply set-union (map free-variables vals)))
         (mut-parms (ac-mutables vars))
         (mut-parms-copies (map var-copy mut-parms))
         (mut (append (pair-up mut-parms mut-parms-copies) mut)))
    (let loop ((l vars)
               (v vals)
               (new-vars '())
               (new-vals '())
               (new-body (ac (prc-body proc) env mut)))
      (if (null? l)
          (new-let ptree proc new-vars new-vals new-body)
          (let ((var (car l)) (val (car v)))
            (if (memq var mut-parms)
                (let ((src (node-source val))
                      (decl (node-decl val))
                      (var* (cdr (assq var mut))))
                  (if (set-member? var vals-fv)
                      (loop (cdr l)
                            (cdr v)
                            (cons var* new-vars)
                            (cons (new-call
                                   src
                                   decl
                                   (new-ref-extended-bindings
                                    src
                                    **make-cell-sym
                                    env)
                                   (list (new-cst src decl undef-object)))
                                  new-vals)
                            (new-seq src
                                     decl
                                     (new-call
                                      src
                                      decl
                                      (new-ref-extended-bindings
                                       src
                                       **cell-set!-sym
                                       env)
                                      (list (new-ref src decl var*)
                                            (ac val env mut)))
                                     new-body))
                      (loop (cdr l)
                            (cdr v)
                            (cons var* new-vars)
                            (cons (new-call
                                   src
                                   decl
                                   (new-ref-extended-bindings
                                    src
                                    **make-cell-sym
                                    env)
                                   (list (ac val env mut)))
                                  new-vals)
                            new-body)))
                (loop (cdr l)
                      (cdr v)
                      (cons var new-vars)
                      (cons (ac val env mut) new-vals)
                      new-body)))))))
(define (ac-mutables l)
  (if (pair? l)
      (let ((var (car l)) (rest (ac-mutables (cdr l))))
        (if (mutable? var) (cons var rest) rest))
      '()))
(define (lambda-lift! ptree) (ll! ptree (set-empty) '()))
(define (ll! ptree cst-procs env)
  (define (new-env env vars)
    (define (loop i l)
      (if (pair? l)
          (let ((var (car l)))
            (cons (cons var (cons (length (set->list (var-refs var))) i))
                  (loop (+ i 1) (cdr l))))
          env))
    (loop (length env) vars))
  (cond ((or (cst? ptree)
             (ref? ptree)
             (set? ptree)
             (tst? ptree)
             (conj? ptree)
             (disj? ptree)
             (fut? ptree))
         (for-each
          (lambda (child) (ll! child cst-procs env))
          (node-children ptree)))
        ((prc? ptree)
         (ll! (prc-body ptree) cst-procs (new-env env (prc-parms ptree))))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (= (length (prc-parms oper)) (length args)))
               (ll!-let ptree cst-procs (new-env env (prc-parms oper)))
               (for-each
                (lambda (child) (ll! child cst-procs env))
                (node-children ptree)))))
        (else (compiler-internal-error "ll!, unknown parse tree node type"))))
(define (ll!-let ptree cst-procs env)
  (let* ((proc (app-oper ptree))
         (vals (app-args ptree))
         (vars (prc-parms proc))
         (var-val-map (pair-up vars vals)))
    (define (var->val var) (cdr (assq var var-val-map)))
    (define (liftable-proc-vars vars)
      (let loop ((cst-proc-vars
                  (set-keep
                   (lambda (var)
                     (let ((val (var->val var)))
                       (and (prc? val)
                            (lambda-lift? (node-decl val))
                            (set-every? oper-pos? (var-refs var)))))
                   (list->set vars))))
        (let* ((non-cst-proc-vars
                (set-keep
                 (lambda (var)
                   (let ((val (var->val var)))
                     (and (prc? val) (not (set-member? var cst-proc-vars)))))
                 (list->set vars)))
               (cst-proc-vars*
                (set-keep
                 (lambda (var)
                   (let ((val (var->val var)))
                     (set-empty?
                      (set-intersection
                       (free-variables val)
                       non-cst-proc-vars))))
                 cst-proc-vars)))
          (if (set-equal? cst-proc-vars cst-proc-vars*)
              cst-proc-vars
              (loop cst-proc-vars*)))))
    (define (transitively-closed-free-variables vars)
      (let ((tcfv-map
             (map (lambda (var) (cons var (free-variables (var->val var))))
                  vars)))
        (let loop ((changed? #f))
          (for-each
           (lambda (var-tcfv)
             (let loop2 ((l (set->list (cdr var-tcfv))) (fv (cdr var-tcfv)))
               (if (null? l)
                   (if (not (set-equal? fv (cdr var-tcfv)))
                       (begin (set-cdr! var-tcfv fv) (set! changed? #t)))
                   (let ((x (assq (car l) tcfv-map)))
                     (loop2 (cdr l) (if x (set-union fv (cdr x)) fv))))))
           tcfv-map)
          (if changed? (loop #f) tcfv-map))))
    (let* ((tcfv-map
            (transitively-closed-free-variables (liftable-proc-vars vars)))
           (cst-proc-vars-list (map car tcfv-map))
           (cst-procs* (set-union (list->set cst-proc-vars-list) cst-procs)))
      (define (var->tcfv var) (cdr (assq var tcfv-map)))
      (define (order-vars vars)
        (map car
             (sort-list
              (map (lambda (var) (assq var env)) vars)
              (lambda (x y)
                (if (= (cadr x) (cadr y))
                    (< (cddr x) (cddr y))
                    (< (cadr x) (cadr y)))))))
      (define (lifted-vars var)
        (order-vars (set->list (set-difference (var->tcfv var) cst-procs*))))
      (define (lift-app! var)
        (let* ((val (var->val var)) (vars (lifted-vars var)))
          (define (new-ref* var)
            (new-ref (var-source var) (node-decl val) var))
          (if (not (null? vars))
              (for-each
               (lambda (oper)
                 (let ((node (node-parent oper)))
                   (node-children-set!
                    node
                    (cons (app-oper node)
                          (append (map new-ref* vars) (app-args node))))))
               (set->list (var-refs var))))))
      (define (lift-prc! var)
        (let* ((val (var->val var)) (vars (lifted-vars var)))
          (if (not (null? vars))
              (let ((var-copies (map var-copy vars)))
                (prc-parms-set! val (append var-copies (prc-parms val)))
                (for-each (lambda (x) (var-bound-set! x val)) var-copies)
                (node-fv-invalidate! val)
                (prc-min-set! val (+ (prc-min val) (length vars)))
                (ll-rename! val (pair-up vars var-copies))))))
      (for-each lift-app! cst-proc-vars-list)
      (for-each lift-prc! cst-proc-vars-list)
      (for-each (lambda (node) (ll! node cst-procs* env)) vals)
      (ll! (prc-body proc) cst-procs* env))))
(define (ll-rename! ptree var-map)
  (cond ((ref? ptree)
         (let* ((var (ref-var ptree)) (x (assq var var-map)))
           (if x
               (begin
                 (var-refs-set! var (set-remove (var-refs var) ptree))
                 (var-refs-set! (cdr x) (set-adjoin (var-refs (cdr x)) ptree))
                 (ref-var-set! ptree (cdr x))))))
        ((set? ptree)
         (let* ((var (set-var ptree)) (x (assq var var-map)))
           (if x
               (begin
                 (var-sets-set! var (set-remove (var-sets var) ptree))
                 (var-sets-set! (cdr x) (set-adjoin (var-sets (cdr x)) ptree))
                 (set-var-set! ptree (cdr x)))))))
  (node-fv-set! ptree #t)
  (for-each (lambda (child) (ll-rename! child var-map)) (node-children ptree)))
(define (parse-tree->expression ptree) (se ptree '() (list 0)))
(define (se ptree env num)
  (cond ((cst? ptree) (list quote-sym (cst-val ptree)))
        ((ref? ptree)
         (let ((x (assq (ref-var ptree) env)))
           (if x (cdr x) (var-name (ref-var ptree)))))
        ((set? ptree)
         (list set!-sym
               (let ((x (assq (set-var ptree) env)))
                 (if x (cdr x) (var-name (set-var ptree))))
               (se (set-val ptree) env num)))
        ((def? ptree)
         (list define-sym
               (let ((x (assq (def-var ptree) env)))
                 (if x (cdr x) (var-name (def-var ptree))))
               (se (def-val ptree) env num)))
        ((tst? ptree)
         (list if-sym
               (se (tst-pre ptree) env num)
               (se (tst-con ptree) env num)
               (se (tst-alt ptree) env num)))
        ((conj? ptree)
         (list and-sym
               (se (conj-pre ptree) env num)
               (se (conj-alt ptree) env num)))
        ((disj? ptree)
         (list or-sym
               (se (disj-pre ptree) env num)
               (se (disj-alt ptree) env num)))
        ((prc? ptree)
         (let ((new-env (se-rename (prc-parms ptree) env num)))
           (list lambda-sym
                 (se-parameters
                  (prc-parms ptree)
                  (prc-rest ptree)
                  (prc-min ptree)
                  new-env)
                 (se (prc-body ptree) new-env num))))
        ((app? ptree)
         (let ((oper (app-oper ptree)) (args (app-args ptree)))
           (if (and (prc? oper)
                    (not (prc-rest oper))
                    (= (length (prc-parms oper)) (length args)))
               (let ((new-env (se-rename (prc-parms oper) env num)))
                 (list (if (set-empty?
                            (set-intersection
                             (list->set (prc-parms oper))
                             (apply set-union (map free-variables args))))
                           let-sym
                           letrec-sym)
                       (se-bindings (prc-parms oper) args new-env num)
                       (se (prc-body oper) new-env num)))
               (map (lambda (x) (se x env num)) (cons oper args)))))
        ((fut? ptree) (list future-sym (se (fut-val ptree) env num)))
        (else (compiler-internal-error "se, unknown parse tree node type"))))
(define (se-parameters parms rest min env)
  (define (se-parms parms rest n env)
    (cond ((null? parms) '())
          ((and rest (null? (cdr parms))) (cdr (assq (car parms) env)))
          (else
           (let ((parm (cdr (assq (car parms) env))))
             (cons (if (> n 0) parm (list parm))
                   (se-parms (cdr parms) rest (- n 1) env))))))
  (se-parms parms rest min env))
(define (se-bindings vars vals env num)
  (if (null? vars)
      '()
      (cons (list (cdr (assq (car vars) env)) (se (car vals) env num))
            (se-bindings (cdr vars) (cdr vals) env num))))
(define (se-rename vars env num)
  (define (rename vars)
    (if (null? vars)
        env
        (cons (cons (car vars)
                    (string->canonical-symbol
                     (string-append
                      (symbol->string (var-name (car vars)))
                      "#"
                      (number->string (car num)))))
              (rename (cdr vars)))))
  (set-car! num (+ (car num) 1))
  (rename vars))
(define *opnd-table* '())
(define *opnd-table-alloc* '())
(define opnd-table-size 10000)
(define (enter-opnd arg1 arg2)
  (let loop ((i 0))
    (if (< i *opnd-table-alloc*)
        (let ((x (vector-ref *opnd-table* i)))
          (if (and (eqv? (car x) arg1) (eqv? (cdr x) arg2)) i (loop (+ i 1))))
        (if (< *opnd-table-alloc* opnd-table-size)
            (begin
              (set! *opnd-table-alloc* (+ *opnd-table-alloc* 1))
              (vector-set! *opnd-table* i (cons arg1 arg2))
              i)
            (compiler-limitation-error
             "program is too long [virtual machine operand table overflow]")))))
(define (contains-opnd? opnd1 opnd2)
  (cond ((eqv? opnd1 opnd2) #t)
        ((clo? opnd2) (contains-opnd? opnd1 (clo-base opnd2)))
        (else #f)))
(define (any-contains-opnd? opnd opnds)
  (if (null? opnds)
      #f
      (or (contains-opnd? opnd (car opnds))
          (any-contains-opnd? opnd (cdr opnds)))))
(define (make-reg num) num)
(define (reg? x) (< x 10000))
(define (reg-num x) (modulo x 10000))
(define (make-stk num) (+ num 10000))
(define (stk? x) (= (quotient x 10000) 1))
(define (stk-num x) (modulo x 10000))
(define (make-glo name) (+ (enter-opnd name #t) 30000))
(define (glo? x) (= (quotient x 10000) 3))
(define (glo-name x) (car (vector-ref *opnd-table* (modulo x 10000))))
(define (make-clo base index) (+ (enter-opnd base index) 40000))
(define (clo? x) (= (quotient x 10000) 4))
(define (clo-base x) (car (vector-ref *opnd-table* (modulo x 10000))))
(define (clo-index x) (cdr (vector-ref *opnd-table* (modulo x 10000))))
(define (make-lbl num) (+ num 20000))
(define (lbl? x) (= (quotient x 10000) 2))
(define (lbl-num x) (modulo x 10000))
(define label-limit 9999)
(define (make-obj val) (+ (enter-opnd val #f) 50000))
(define (obj? x) (= (quotient x 10000) 5))
(define (obj-val x) (car (vector-ref *opnd-table* (modulo x 10000))))
(define (make-pcontext fs map) (vector fs map))
(define (pcontext-fs x) (vector-ref x 0))
(define (pcontext-map x) (vector-ref x 1))
(define (make-frame size slots regs closed live)
  (vector size slots regs closed live))
(define (frame-size x) (vector-ref x 0))
(define (frame-slots x) (vector-ref x 1))
(define (frame-regs x) (vector-ref x 2))
(define (frame-closed x) (vector-ref x 3))
(define (frame-live x) (vector-ref x 4))
(define (frame-eq? x y) (= (frame-size x) (frame-size y)))
(define (frame-truncate frame nb-slots)
  (let ((fs (frame-size frame)))
    (make-frame
     nb-slots
     (nth-after (frame-slots frame) (- fs nb-slots))
     (frame-regs frame)
     (frame-closed frame)
     (frame-live frame))))
(define (frame-live? var frame)
  (let ((live (frame-live frame)))
    (if (eq? var closure-env-var)
        (let ((closed (frame-closed frame)))
          (if (or (set-member? var live)
                  (not (set-empty?
                        (set-intersection live (list->set closed)))))
              closed
              #f))
        (if (set-member? var live) var #f))))
(define (frame-first-empty-slot frame)
  (let loop ((i 1) (s (reverse (frame-slots frame))))
    (if (pair? s)
        (if (frame-live? (car s) frame) (loop (+ i 1) (cdr s)) i)
        i)))
(define (make-proc-obj
         name
         primitive?
         code
         call-pat
         side-effects?
         strict-pat
         type)
  (let ((proc-obj
         (vector proc-obj-tag
                 name
                 primitive?
                 code
                 call-pat
                 #f
                 #f
                 #f
                 side-effects?
                 strict-pat
                 type)))
    (proc-obj-specialize-set! proc-obj (lambda (decls) proc-obj))
    proc-obj))
(define proc-obj-tag (list 'proc-obj))
(define (proc-obj? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (eq? (vector-ref x 0) proc-obj-tag)))
(define (proc-obj-name obj) (vector-ref obj 1))
(define (proc-obj-primitive? obj) (vector-ref obj 2))
(define (proc-obj-code obj) (vector-ref obj 3))
(define (proc-obj-call-pat obj) (vector-ref obj 4))
(define (proc-obj-test obj) (vector-ref obj 5))
(define (proc-obj-inlinable obj) (vector-ref obj 6))
(define (proc-obj-specialize obj) (vector-ref obj 7))
(define (proc-obj-side-effects? obj) (vector-ref obj 8))
(define (proc-obj-strict-pat obj) (vector-ref obj 9))
(define (proc-obj-type obj) (vector-ref obj 10))
(define (proc-obj-code-set! obj x) (vector-set! obj 3 x))
(define (proc-obj-test-set! obj x) (vector-set! obj 5 x))
(define (proc-obj-inlinable-set! obj x) (vector-set! obj 6 x))
(define (proc-obj-specialize-set! obj x) (vector-set! obj 7 x))
(define (make-pattern min-args nb-parms rest?)
  (let loop ((x (if rest? (- nb-parms 1) (list nb-parms)))
             (y (if rest? (- nb-parms 1) nb-parms)))
    (let ((z (- y 1))) (if (< z min-args) x (loop (cons z x) z)))))
(define (pattern-member? n pat)
  (cond ((pair? pat) (if (= (car pat) n) #t (pattern-member? n (cdr pat))))
        ((null? pat) #f)
        (else (<= pat n))))
(define (type-name type) (if (pair? type) (car type) type))
(define (type-pot-fut? type) (pair? type))
(define (make-bbs)
  (vector (make-counter 1 label-limit bbs-limit-err) (queue-empty) '()))
(define (bbs-limit-err)
  (compiler-limitation-error "procedure is too long [too many labels]"))
(define (bbs-lbl-counter bbs) (vector-ref bbs 0))
(define (bbs-lbl-counter-set! bbs cntr) (vector-set! bbs 0 cntr))
(define (bbs-bb-queue bbs) (vector-ref bbs 1))
(define (bbs-bb-queue-set! bbs bbq) (vector-set! bbs 1 bbq))
(define (bbs-entry-lbl-num bbs) (vector-ref bbs 2))
(define (bbs-entry-lbl-num-set! bbs lbl-num) (vector-set! bbs 2 lbl-num))
(define (bbs-new-lbl! bbs) ((bbs-lbl-counter bbs)))
(define (lbl-num->bb lbl-num bbs)
  (let loop ((bb-list (queue->list (bbs-bb-queue bbs))))
    (if (= (bb-lbl-num (car bb-list)) lbl-num)
        (car bb-list)
        (loop (cdr bb-list)))))
(define (make-bb label-instr bbs)
  (let ((bb (vector label-instr (queue-empty) '() '() '())))
    (queue-put! (vector-ref bbs 1) bb)
    bb))
(define (bb-lbl-num bb) (label-lbl-num (vector-ref bb 0)))
(define (bb-label-type bb) (label-type (vector-ref bb 0)))
(define (bb-label-instr bb) (vector-ref bb 0))
(define (bb-label-instr-set! bb l) (vector-set! bb 0 l))
(define (bb-non-branch-instrs bb) (queue->list (vector-ref bb 1)))
(define (bb-non-branch-instrs-set! bb l) (vector-set! bb 1 (list->queue l)))
(define (bb-branch-instr bb) (vector-ref bb 2))
(define (bb-branch-instr-set! bb b) (vector-set! bb 2 b))
(define (bb-references bb) (vector-ref bb 3))
(define (bb-references-set! bb l) (vector-set! bb 3 l))
(define (bb-precedents bb) (vector-ref bb 4))
(define (bb-precedents-set! bb l) (vector-set! bb 4 l))
(define (bb-entry-frame-size bb)
  (frame-size (gvm-instr-frame (bb-label-instr bb))))
(define (bb-exit-frame-size bb)
  (frame-size (gvm-instr-frame (bb-branch-instr bb))))
(define (bb-slots-gained bb)
  (- (bb-exit-frame-size bb) (bb-entry-frame-size bb)))
(define (bb-put-non-branch! bb gvm-instr)
  (queue-put! (vector-ref bb 1) gvm-instr))
(define (bb-put-branch! bb gvm-instr) (vector-set! bb 2 gvm-instr))
(define (bb-add-reference! bb ref)
  (if (not (memq ref (vector-ref bb 3)))
      (vector-set! bb 3 (cons ref (vector-ref bb 3)))))
(define (bb-add-precedent! bb prec)
  (if (not (memq prec (vector-ref bb 4)))
      (vector-set! bb 4 (cons prec (vector-ref bb 4)))))
(define (bb-last-non-branch-instr bb)
  (let ((non-branch-instrs (bb-non-branch-instrs bb)))
    (if (null? non-branch-instrs)
        (bb-label-instr bb)
        (let loop ((l non-branch-instrs))
          (if (pair? (cdr l)) (loop (cdr l)) (car l))))))
(define (gvm-instr-type gvm-instr) (vector-ref gvm-instr 0))
(define (gvm-instr-frame gvm-instr) (vector-ref gvm-instr 1))
(define (gvm-instr-comment gvm-instr) (vector-ref gvm-instr 2))
(define (make-label-simple lbl-num frame comment)
  (vector 'label frame comment lbl-num 'simple))
(define (make-label-entry lbl-num nb-parms min rest? closed? frame comment)
  (vector 'label frame comment lbl-num 'entry nb-parms min rest? closed?))
(define (make-label-return lbl-num frame comment)
  (vector 'label frame comment lbl-num 'return))
(define (make-label-task-entry lbl-num frame comment)
  (vector 'label frame comment lbl-num 'task-entry))
(define (make-label-task-return lbl-num frame comment)
  (vector 'label frame comment lbl-num 'task-return))
(define (label-lbl-num gvm-instr) (vector-ref gvm-instr 3))
(define (label-lbl-num-set! gvm-instr n) (vector-set! gvm-instr 3 n))
(define (label-type gvm-instr) (vector-ref gvm-instr 4))
(define (label-entry-nb-parms gvm-instr) (vector-ref gvm-instr 5))
(define (label-entry-min gvm-instr) (vector-ref gvm-instr 6))
(define (label-entry-rest? gvm-instr) (vector-ref gvm-instr 7))
(define (label-entry-closed? gvm-instr) (vector-ref gvm-instr 8))
(define (make-apply prim opnds loc frame comment)
  (vector 'apply frame comment prim opnds loc))
(define (apply-prim gvm-instr) (vector-ref gvm-instr 3))
(define (apply-opnds gvm-instr) (vector-ref gvm-instr 4))
(define (apply-loc gvm-instr) (vector-ref gvm-instr 5))
(define (make-copy opnd loc frame comment)
  (vector 'copy frame comment opnd loc))
(define (copy-opnd gvm-instr) (vector-ref gvm-instr 3))
(define (copy-loc gvm-instr) (vector-ref gvm-instr 4))
(define (make-close parms frame comment) (vector 'close frame comment parms))
(define (close-parms gvm-instr) (vector-ref gvm-instr 3))
(define (make-closure-parms loc lbl opnds) (vector loc lbl opnds))
(define (closure-parms-loc x) (vector-ref x 0))
(define (closure-parms-lbl x) (vector-ref x 1))
(define (closure-parms-opnds x) (vector-ref x 2))
(define (make-ifjump test opnds true false poll? frame comment)
  (vector 'ifjump frame comment test opnds true false poll?))
(define (ifjump-test gvm-instr) (vector-ref gvm-instr 3))
(define (ifjump-opnds gvm-instr) (vector-ref gvm-instr 4))
(define (ifjump-true gvm-instr) (vector-ref gvm-instr 5))
(define (ifjump-false gvm-instr) (vector-ref gvm-instr 6))
(define (ifjump-poll? gvm-instr) (vector-ref gvm-instr 7))
(define (make-jump opnd nb-args poll? frame comment)
  (vector 'jump frame comment opnd nb-args poll?))
(define (jump-opnd gvm-instr) (vector-ref gvm-instr 3))
(define (jump-nb-args gvm-instr) (vector-ref gvm-instr 4))
(define (jump-poll? gvm-instr) (vector-ref gvm-instr 5))
(define (first-class-jump? gvm-instr) (jump-nb-args gvm-instr))
(define (make-comment) (cons 'comment '()))
(define (comment-put! comment name val)
  (set-cdr! comment (cons (cons name val) (cdr comment))))
(define (comment-get comment name)
  (and comment (let ((x (assq name (cdr comment)))) (if x (cdr x) #f))))
(define (bbs-purify! bbs)
  (let loop ()
    (bbs-remove-jump-cascades! bbs)
    (bbs-remove-dead-code! bbs)
    (let* ((changed1? (bbs-remove-common-code! bbs))
           (changed2? (bbs-remove-useless-jumps! bbs)))
      (if (or changed1? changed2?) (loop) (bbs-order! bbs)))))
(define (bbs-remove-jump-cascades! bbs)
  (define (empty-bb? bb)
    (and (eq? (bb-label-type bb) 'simple) (null? (bb-non-branch-instrs bb))))
  (define (jump-to-non-entry-lbl? branch)
    (and (eq? (gvm-instr-type branch) 'jump)
         (not (first-class-jump? branch))
         (jump-lbl? branch)))
  (define (jump-cascade-to lbl-num fs poll? seen thunk)
    (if (memq lbl-num seen)
        (thunk lbl-num fs poll?)
        (let ((bb (lbl-num->bb lbl-num bbs)))
          (if (and (empty-bb? bb) (<= (bb-slots-gained bb) 0))
              (let ((jump-lbl-num
                     (jump-to-non-entry-lbl? (bb-branch-instr bb))))
                (if jump-lbl-num
                    (jump-cascade-to
                     jump-lbl-num
                     (+ fs (bb-slots-gained bb))
                     (or poll? (jump-poll? (bb-branch-instr bb)))
                     (cons lbl-num seen)
                     thunk)
                    (thunk lbl-num fs poll?)))
              (thunk lbl-num fs poll?)))))
  (define (equiv-lbl lbl-num seen)
    (if (memq lbl-num seen)
        lbl-num
        (let ((bb (lbl-num->bb lbl-num bbs)))
          (if (empty-bb? bb)
              (let ((jump-lbl-num
                     (jump-to-non-entry-lbl? (bb-branch-instr bb))))
                (if (and jump-lbl-num
                         (not (jump-poll? (bb-branch-instr bb)))
                         (= (bb-slots-gained bb) 0))
                    (equiv-lbl jump-lbl-num (cons lbl-num seen))
                    lbl-num))
              lbl-num))))
  (define (remove-cascade! bb)
    (let ((branch (bb-branch-instr bb)))
      (case (gvm-instr-type branch)
        ((ifjump)
         (bb-put-branch!
          bb
          (make-ifjump
           (ifjump-test branch)
           (ifjump-opnds branch)
           (equiv-lbl (ifjump-true branch) '())
           (equiv-lbl (ifjump-false branch) '())
           (ifjump-poll? branch)
           (gvm-instr-frame branch)
           (gvm-instr-comment branch))))
        ((jump)
         (if (not (first-class-jump? branch))
             (let ((dest-lbl-num (jump-lbl? branch)))
               (if dest-lbl-num
                   (jump-cascade-to
                    dest-lbl-num
                    (frame-size (gvm-instr-frame branch))
                    (jump-poll? branch)
                    '()
                    (lambda (lbl-num fs poll?)
                      (let* ((dest-bb (lbl-num->bb lbl-num bbs))
                             (last-branch (bb-branch-instr dest-bb)))
                        (if (and (empty-bb? dest-bb)
                                 (or (not poll?)
                                     put-poll-on-ifjump?
                                     (not (eq? (gvm-instr-type last-branch)
                                               'ifjump))))
                            (let* ((new-fs (+ fs (bb-slots-gained dest-bb)))
                                   (new-frame
                                    (frame-truncate
                                     (gvm-instr-frame branch)
                                     new-fs)))
                              (define (adjust-opnd opnd)
                                (cond ((stk? opnd)
                                       (make-stk
                                        (+ (- fs (bb-entry-frame-size dest-bb))
                                           (stk-num opnd))))
                                      ((clo? opnd)
                                       (make-clo
                                        (adjust-opnd (clo-base opnd))
                                        (clo-index opnd)))
                                      (else opnd)))
                              (case (gvm-instr-type last-branch)
                                ((ifjump)
                                 (bb-put-branch!
                                  bb
                                  (make-ifjump
                                   (ifjump-test last-branch)
                                   (map adjust-opnd (ifjump-opnds last-branch))
                                   (equiv-lbl (ifjump-true last-branch) '())
                                   (equiv-lbl (ifjump-false last-branch) '())
                                   (or poll? (ifjump-poll? last-branch))
                                   new-frame
                                   (gvm-instr-comment last-branch))))
                                ((jump)
                                 (bb-put-branch!
                                  bb
                                  (make-jump
                                   (adjust-opnd (jump-opnd last-branch))
                                   (jump-nb-args last-branch)
                                   (or poll? (jump-poll? last-branch))
                                   new-frame
                                   (gvm-instr-comment last-branch))))
                                (else
                                 (compiler-internal-error
                                  "bbs-remove-jump-cascades!, unknown branch type"))))
                            (bb-put-branch!
                             bb
                             (make-jump
                              (make-lbl lbl-num)
                              (jump-nb-args branch)
                              (or poll? (jump-poll? branch))
                              (frame-truncate (gvm-instr-frame branch) fs)
                              (gvm-instr-comment branch)))))))))))
        (else
         (compiler-internal-error
          "bbs-remove-jump-cascades!, unknown branch type")))))
  (for-each remove-cascade! (queue->list (bbs-bb-queue bbs))))
(define (jump-lbl? branch)
  (let ((opnd (jump-opnd branch))) (if (lbl? opnd) (lbl-num opnd) #f)))
(define put-poll-on-ifjump? #f)
(set! put-poll-on-ifjump? #t)
(define (bbs-remove-dead-code! bbs)
  (let ((new-bb-queue (queue-empty)) (scan-queue (queue-empty)))
    (define (reachable ref bb)
      (if bb (bb-add-reference! bb ref))
      (if (not (memq ref (queue->list new-bb-queue)))
          (begin
            (bb-references-set! ref '())
            (bb-precedents-set! ref '())
            (queue-put! new-bb-queue ref)
            (queue-put! scan-queue ref))))
    (define (direct-jump to-bb from-bb)
      (reachable to-bb from-bb)
      (bb-add-precedent! to-bb from-bb))
    (define (scan-instr gvm-instr bb)
      (define (scan-opnd gvm-opnd)
        (cond ((lbl? gvm-opnd)
               (reachable (lbl-num->bb (lbl-num gvm-opnd) bbs) bb))
              ((clo? gvm-opnd) (scan-opnd (clo-base gvm-opnd)))))
      (case (gvm-instr-type gvm-instr)
        ((label) '())
        ((apply)
         (for-each scan-opnd (apply-opnds gvm-instr))
         (if (apply-loc gvm-instr) (scan-opnd (apply-loc gvm-instr))))
        ((copy)
         (scan-opnd (copy-opnd gvm-instr))
         (scan-opnd (copy-loc gvm-instr)))
        ((close)
         (for-each
          (lambda (parm)
            (reachable (lbl-num->bb (closure-parms-lbl parm) bbs) bb)
            (scan-opnd (closure-parms-loc parm))
            (for-each scan-opnd (closure-parms-opnds parm)))
          (close-parms gvm-instr)))
        ((ifjump)
         (for-each scan-opnd (ifjump-opnds gvm-instr))
         (direct-jump (lbl-num->bb (ifjump-true gvm-instr) bbs) bb)
         (direct-jump (lbl-num->bb (ifjump-false gvm-instr) bbs) bb))
        ((jump)
         (let ((opnd (jump-opnd gvm-instr)))
           (if (lbl? opnd)
               (direct-jump (lbl-num->bb (lbl-num opnd) bbs) bb)
               (scan-opnd (jump-opnd gvm-instr)))))
        (else
         (compiler-internal-error
          "bbs-remove-dead-code!, unknown GVM instruction type"))))
    (reachable (lbl-num->bb (bbs-entry-lbl-num bbs) bbs) #f)
    (let loop ()
      (if (not (queue-empty? scan-queue))
          (let ((bb (queue-get! scan-queue)))
            (begin
              (scan-instr (bb-label-instr bb) bb)
              (for-each
               (lambda (gvm-instr) (scan-instr gvm-instr bb))
               (bb-non-branch-instrs bb))
              (scan-instr (bb-branch-instr bb) bb)
              (loop)))))
    (bbs-bb-queue-set! bbs new-bb-queue)))
(define (bbs-remove-useless-jumps! bbs)
  (let ((changed? #f))
    (define (remove-useless-jump bb)
      (let ((branch (bb-branch-instr bb)))
        (if (and (eq? (gvm-instr-type branch) 'jump)
                 (not (first-class-jump? branch))
                 (not (jump-poll? branch))
                 (jump-lbl? branch))
            (let* ((dest-bb (lbl-num->bb (jump-lbl? branch) bbs))
                   (frame1 (gvm-instr-frame (bb-last-non-branch-instr bb)))
                   (frame2 (gvm-instr-frame (bb-label-instr dest-bb))))
              (if (and (eq? (bb-label-type dest-bb) 'simple)
                       (frame-eq? frame1 frame2)
                       (= (length (bb-precedents dest-bb)) 1))
                  (begin
                    (set! changed? #t)
                    (bb-non-branch-instrs-set!
                     bb
                     (append (bb-non-branch-instrs bb)
                             (bb-non-branch-instrs dest-bb)
                             '()))
                    (bb-branch-instr-set! bb (bb-branch-instr dest-bb))
                    (remove-useless-jump bb)))))))
    (for-each remove-useless-jump (queue->list (bbs-bb-queue bbs)))
    changed?))
(define (bbs-remove-common-code! bbs)
  (let* ((bb-list (queue->list (bbs-bb-queue bbs)))
         (n (length bb-list))
         (hash-table-length (cond ((< n 50) 43) ((< n 500) 403) (else 4003)))
         (hash-table (make-vector hash-table-length '()))
         (prim-table '())
         (block-map '())
         (changed? #f))
    (define (hash-prim prim)
      (let ((n (length prim-table)) (i (pos-in-list prim prim-table)))
        (if i
            (- n i)
            (begin (set! prim-table (cons prim prim-table)) (+ n 1)))))
    (define (hash-opnds l)
      (let loop ((l l) (n 0))
        (if (pair? l)
            (loop (cdr l)
                  (let ((x (car l)))
                    (if (lbl? x)
                        n
                        (modulo (+ (* n 10000) x) hash-table-length))))
            n)))
    (define (hash-bb bb)
      (let ((branch (bb-branch-instr bb)))
        (modulo (case (gvm-instr-type branch)
                  ((ifjump)
                   (+ (hash-opnds (ifjump-opnds branch))
                      (* 10 (hash-prim (ifjump-test branch)))
                      (* 100 (frame-size (gvm-instr-frame branch)))))
                  ((jump)
                   (+ (hash-opnds (list (jump-opnd branch)))
                      (* 10 (or (jump-nb-args branch) -1))
                      (* 100 (frame-size (gvm-instr-frame branch)))))
                  (else 0))
                hash-table-length)))
    (define (replacement-lbl-num lbl)
      (let ((x (assv lbl block-map))) (if x (cdr x) lbl)))
    (define (fix-map! bb1 bb2)
      (let loop ((l block-map))
        (if (pair? l)
            (let ((x (car l)))
              (if (= bb1 (cdr x)) (set-cdr! x bb2))
              (loop (cdr l))))))
    (define (enter-bb! bb)
      (let ((h (hash-bb bb)))
        (vector-set! hash-table h (add-bb bb (vector-ref hash-table h)))))
    (define (add-bb bb l)
      (if (pair? l)
          (let ((bb* (car l)))
            (set! block-map
                  (cons (cons (bb-lbl-num bb) (bb-lbl-num bb*)) block-map))
            (if (eqv-bb? bb bb*)
                (begin
                  (fix-map! (bb-lbl-num bb) (bb-lbl-num bb*))
                  (set! changed? #t)
                  l)
                (begin
                  (set! block-map (cdr block-map))
                  (if (eqv-gvm-instr?
                       (bb-branch-instr bb)
                       (bb-branch-instr bb*))
                      (extract-common-tail
                       bb
                       bb*
                       (lambda (head head* tail)
                         (if (null? tail)
                             (cons bb* (add-bb bb (cdr l)))
                             (let* ((lbl (bbs-new-lbl! bbs))
                                    (branch (bb-branch-instr bb))
                                    (fs** (need-gvm-instrs tail branch))
                                    (frame (frame-truncate
                                            (gvm-instr-frame
                                             (if (null? head)
                                                 (bb-label-instr bb)
                                                 (car head)))
                                            fs**))
                                    (bb** (make-bb (make-label-simple
                                                    lbl
                                                    frame
                                                    #f)
                                                   bbs)))
                               (bb-non-branch-instrs-set! bb** tail)
                               (bb-branch-instr-set! bb** branch)
                               (bb-non-branch-instrs-set! bb* (reverse head*))
                               (bb-branch-instr-set!
                                bb*
                                (make-jump (make-lbl lbl) #f #f frame #f))
                               (bb-non-branch-instrs-set! bb (reverse head))
                               (bb-branch-instr-set!
                                bb
                                (make-jump (make-lbl lbl) #f #f frame #f))
                               (set! changed? #t)
                               (cons bb (cons bb* (add-bb bb** (cdr l))))))))
                      (cons bb* (add-bb bb (cdr l)))))))
          (list bb)))
    (define (extract-common-tail bb1 bb2 cont)
      (let loop ((l1 (reverse (bb-non-branch-instrs bb1)))
                 (l2 (reverse (bb-non-branch-instrs bb2)))
                 (tail '()))
        (if (and (pair? l1) (pair? l2))
            (let ((i1 (car l1)) (i2 (car l2)))
              (if (eqv-gvm-instr? i1 i2)
                  (loop (cdr l1) (cdr l2) (cons i1 tail))
                  (cont l1 l2 tail)))
            (cont l1 l2 tail))))
    (define (eqv-bb? bb1 bb2)
      (let ((bb1-non-branch (bb-non-branch-instrs bb1))
            (bb2-non-branch (bb-non-branch-instrs bb2)))
        (and (= (length bb1-non-branch) (length bb2-non-branch))
             (eqv-gvm-instr? (bb-label-instr bb1) (bb-label-instr bb2))
             (eqv-gvm-instr? (bb-branch-instr bb1) (bb-branch-instr bb2))
             (eqv-list? eqv-gvm-instr? bb1-non-branch bb2-non-branch))))
    (define (eqv-list? pred? l1 l2)
      (if (pair? l1)
          (and (pair? l2)
               (pred? (car l1) (car l2))
               (eqv-list? pred? (cdr l1) (cdr l2)))
          (not (pair? l2))))
    (define (eqv-lbl-num? lbl1 lbl2)
      (= (replacement-lbl-num lbl1) (replacement-lbl-num lbl2)))
    (define (eqv-gvm-opnd? opnd1 opnd2)
      (if (not opnd1)
          (not opnd2)
          (and opnd2
               (cond ((lbl? opnd1)
                      (and (lbl? opnd2)
                           (eqv-lbl-num? (lbl-num opnd1) (lbl-num opnd2))))
                     ((clo? opnd1)
                      (and (clo? opnd2)
                           (= (clo-index opnd1) (clo-index opnd2))
                           (eqv-gvm-opnd? (clo-base opnd1) (clo-base opnd2))))
                     (else (eqv? opnd1 opnd2))))))
    (define (eqv-gvm-instr? instr1 instr2)
      (define (eqv-closure-parms? p1 p2)
        (and (eqv-gvm-opnd? (closure-parms-loc p1) (closure-parms-loc p2))
             (eqv-lbl-num? (closure-parms-lbl p1) (closure-parms-lbl p2))
             (eqv-list?
              eqv-gvm-opnd?
              (closure-parms-opnds p1)
              (closure-parms-opnds p2))))
      (let ((type1 (gvm-instr-type instr1)) (type2 (gvm-instr-type instr2)))
        (and (eq? type1 type2)
             (frame-eq? (gvm-instr-frame instr1) (gvm-instr-frame instr2))
             (case type1
               ((label)
                (let ((ltype1 (label-type instr1))
                      (ltype2 (label-type instr2)))
                  (and (eq? ltype1 ltype2)
                       (case ltype1
                         ((simple return task-entry task-return) #t)
                         ((entry)
                          (and (= (label-entry-min instr1)
                                  (label-entry-min instr2))
                               (= (label-entry-nb-parms instr1)
                                  (label-entry-nb-parms instr2))
                               (eq? (label-entry-rest? instr1)
                                    (label-entry-rest? instr2))
                               (eq? (label-entry-closed? instr1)
                                    (label-entry-closed? instr2))))
                         (else
                          (compiler-internal-error
                           "eqv-gvm-instr?, unknown label type"))))))
               ((apply)
                (and (eq? (apply-prim instr1) (apply-prim instr2))
                     (eqv-list?
                      eqv-gvm-opnd?
                      (apply-opnds instr1)
                      (apply-opnds instr2))
                     (eqv-gvm-opnd? (apply-loc instr1) (apply-loc instr2))))
               ((copy)
                (and (eqv-gvm-opnd? (copy-opnd instr1) (copy-opnd instr2))
                     (eqv-gvm-opnd? (copy-loc instr1) (copy-loc instr2))))
               ((close)
                (eqv-list?
                 eqv-closure-parms?
                 (close-parms instr1)
                 (close-parms instr2)))
               ((ifjump)
                (and (eq? (ifjump-test instr1) (ifjump-test instr2))
                     (eqv-list?
                      eqv-gvm-opnd?
                      (ifjump-opnds instr1)
                      (ifjump-opnds instr2))
                     (eqv-lbl-num? (ifjump-true instr1) (ifjump-true instr2))
                     (eqv-lbl-num? (ifjump-false instr1) (ifjump-false instr2))
                     (eq? (ifjump-poll? instr1) (ifjump-poll? instr2))))
               ((jump)
                (and (eqv-gvm-opnd? (jump-opnd instr1) (jump-opnd instr2))
                     (eqv? (jump-nb-args instr1) (jump-nb-args instr2))
                     (eq? (jump-poll? instr1) (jump-poll? instr2))))
               (else
                (compiler-internal-error
                 "eqv-gvm-instr?, unknown 'gvm-instr':"
                 instr1))))))
    (define (update-bb! bb) (replace-label-references! bb replacement-lbl-num))
    (for-each enter-bb! bb-list)
    (bbs-entry-lbl-num-set! bbs (replacement-lbl-num (bbs-entry-lbl-num bbs)))
    (let loop ((i 0) (result '()))
      (if (< i hash-table-length)
          (let ((bb-kept (vector-ref hash-table i)))
            (for-each update-bb! bb-kept)
            (loop (+ i 1) (append bb-kept result)))
          (bbs-bb-queue-set! bbs (list->queue result))))
    changed?))
(define (replace-label-references! bb replacement-lbl-num)
  (define (update-gvm-opnd opnd)
    (if opnd
        (cond ((lbl? opnd) (make-lbl (replacement-lbl-num (lbl-num opnd))))
              ((clo? opnd)
               (make-clo (update-gvm-opnd (clo-base opnd)) (clo-index opnd)))
              (else opnd))
        opnd))
  (define (update-gvm-instr instr)
    (define (update-closure-parms p)
      (make-closure-parms
       (update-gvm-opnd (closure-parms-loc p))
       (replacement-lbl-num (closure-parms-lbl p))
       (map update-gvm-opnd (closure-parms-opnds p))))
    (case (gvm-instr-type instr)
      ((apply)
       (make-apply
        (apply-prim instr)
        (map update-gvm-opnd (apply-opnds instr))
        (update-gvm-opnd (apply-loc instr))
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((copy)
       (make-copy
        (update-gvm-opnd (copy-opnd instr))
        (update-gvm-opnd (copy-loc instr))
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((close)
       (make-close
        (map update-closure-parms (close-parms instr))
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((ifjump)
       (make-ifjump
        (ifjump-test instr)
        (map update-gvm-opnd (ifjump-opnds instr))
        (replacement-lbl-num (ifjump-true instr))
        (replacement-lbl-num (ifjump-false instr))
        (ifjump-poll? instr)
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      ((jump)
       (make-jump
        (update-gvm-opnd (jump-opnd instr))
        (jump-nb-args instr)
        (jump-poll? instr)
        (gvm-instr-frame instr)
        (gvm-instr-comment instr)))
      (else
       (compiler-internal-error "update-gvm-instr, unknown 'instr':" instr))))
  (bb-non-branch-instrs-set!
   bb
   (map update-gvm-instr (bb-non-branch-instrs bb)))
  (bb-branch-instr-set! bb (update-gvm-instr (bb-branch-instr bb))))
(define (bbs-order! bbs)
  (let ((new-bb-queue (queue-empty))
        (left-to-schedule (queue->list (bbs-bb-queue bbs))))
    (define (remove x l)
      (if (eq? (car l) x) (cdr l) (cons (car l) (remove x (cdr l)))))
    (define (remove-bb! bb)
      (set! left-to-schedule (remove bb left-to-schedule))
      bb)
    (define (prec-bb bb)
      (let loop ((l (bb-precedents bb)) (best #f) (best-fs #f))
        (if (null? l)
            best
            (let* ((x (car l)) (x-fs (bb-exit-frame-size x)))
              (if (and (memq x left-to-schedule)
                       (or (not best) (< x-fs best-fs)))
                  (loop (cdr l) x x-fs)
                  (loop (cdr l) best best-fs))))))
    (define (succ-bb bb)
      (define (branches-to-lbl? bb)
        (let ((branch (bb-branch-instr bb)))
          (case (gvm-instr-type branch)
            ((ifjump) #t)
            ((jump) (lbl? (jump-opnd branch)))
            (else
             (compiler-internal-error "bbs-order!, unknown branch type")))))
      (define (best-succ bb1 bb2)
        (if (branches-to-lbl? bb1)
            bb1
            (if (branches-to-lbl? bb2)
                bb2
                (if (< (bb-exit-frame-size bb1) (bb-exit-frame-size bb2))
                    bb2
                    bb1))))
      (let ((branch (bb-branch-instr bb)))
        (case (gvm-instr-type branch)
          ((ifjump)
           (let* ((true-bb (lbl-num->bb (ifjump-true branch) bbs))
                  (true-bb* (and (memq true-bb left-to-schedule) true-bb))
                  (false-bb (lbl-num->bb (ifjump-false branch) bbs))
                  (false-bb* (and (memq false-bb left-to-schedule) false-bb)))
             (if (and true-bb* false-bb*)
                 (best-succ true-bb* false-bb*)
                 (or true-bb* false-bb*))))
          ((jump)
           (let ((opnd (jump-opnd branch)))
             (and (lbl? opnd)
                  (let ((bb (lbl-num->bb (lbl-num opnd) bbs)))
                    (and (memq bb left-to-schedule) bb)))))
          (else (compiler-internal-error "bbs-order!, unknown branch type")))))
    (define (schedule-from bb)
      (queue-put! new-bb-queue bb)
      (let ((x (succ-bb bb)))
        (if x
            (begin
              (schedule-around (remove-bb! x))
              (let ((y (succ-bb bb)))
                (if y (schedule-around (remove-bb! y)))))))
      (schedule-refs bb))
    (define (schedule-around bb)
      (let ((x (prec-bb bb)))
        (if x
            (let ((bb-list (schedule-back (remove-bb! x) '())))
              (queue-put! new-bb-queue x)
              (schedule-forw bb)
              (for-each schedule-refs bb-list))
            (schedule-from bb))))
    (define (schedule-back bb bb-list)
      (let ((bb-list* (cons bb bb-list)) (x (prec-bb bb)))
        (if x
            (let ((bb-list (schedule-back (remove-bb! x) bb-list*)))
              (queue-put! new-bb-queue x)
              bb-list)
            bb-list*)))
    (define (schedule-forw bb)
      (queue-put! new-bb-queue bb)
      (let ((x (succ-bb bb)))
        (if x
            (begin
              (schedule-forw (remove-bb! x))
              (let ((y (succ-bb bb)))
                (if y (schedule-around (remove-bb! y)))))))
      (schedule-refs bb))
    (define (schedule-refs bb)
      (for-each
       (lambda (x)
         (if (memq x left-to-schedule) (schedule-around (remove-bb! x))))
       (bb-references bb)))
    (schedule-from (remove-bb! (lbl-num->bb (bbs-entry-lbl-num bbs) bbs)))
    (bbs-bb-queue-set! bbs new-bb-queue)
    (let ((bb-list (queue->list new-bb-queue)))
      (let loop ((l bb-list) (i 1) (lbl-map '()))
        (if (pair? l)
            (let* ((label-instr (bb-label-instr (car l)))
                   (old-lbl-num (label-lbl-num label-instr)))
              (label-lbl-num-set! label-instr i)
              (loop (cdr l) (+ i 1) (cons (cons old-lbl-num i) lbl-map)))
            (let ()
              (define (replacement-lbl-num x) (cdr (assv x lbl-map)))
              (define (update-bb! bb)
                (replace-label-references! bb replacement-lbl-num))
              (for-each update-bb! bb-list)
              (bbs-lbl-counter-set!
               bbs
               (make-counter
                (* (+ 1 (quotient (bbs-new-lbl! bbs) 1000)) 1000)
                label-limit
                bbs-limit-err))))))))
(define (make-code bb gvm-instr sn) (vector bb gvm-instr sn))
(define (code-bb code) (vector-ref code 0))
(define (code-gvm-instr code) (vector-ref code 1))
(define (code-slots-needed code) (vector-ref code 2))
(define (code-slots-needed-set! code n) (vector-set! code 2 n))
(define (bbs->code-list bbs)
  (let ((code-list (linearize bbs)))
    (setup-slots-needed! code-list)
    code-list))
(define (linearize bbs)
  (let ((code-queue (queue-empty)))
    (define (put-bb bb)
      (define (put-instr gvm-instr)
        (queue-put! code-queue (make-code bb gvm-instr #f)))
      (put-instr (bb-label-instr bb))
      (for-each put-instr (bb-non-branch-instrs bb))
      (put-instr (bb-branch-instr bb)))
    (for-each put-bb (queue->list (bbs-bb-queue bbs)))
    (queue->list code-queue)))
(define (setup-slots-needed! code-list)
  (if (null? code-list)
      #f
      (let* ((code (car code-list))
             (gvm-instr (code-gvm-instr code))
             (sn-rest (setup-slots-needed! (cdr code-list))))
        (case (gvm-instr-type gvm-instr)
          ((label)
           (if (> sn-rest (frame-size (gvm-instr-frame gvm-instr)))
               (compiler-internal-error
                "setup-slots-needed!, incoherent slots needed for LABEL"))
           (code-slots-needed-set! code sn-rest)
           #f)
          ((ifjump jump)
           (let ((sn (frame-size (gvm-instr-frame gvm-instr))))
             (code-slots-needed-set! code sn)
             (need-gvm-instr gvm-instr sn)))
          (else
           (code-slots-needed-set! code sn-rest)
           (need-gvm-instr gvm-instr sn-rest))))))
(define (need-gvm-instrs non-branch branch)
  (if (pair? non-branch)
      (need-gvm-instr
       (car non-branch)
       (need-gvm-instrs (cdr non-branch) branch))
      (need-gvm-instr branch (frame-size (gvm-instr-frame branch)))))
(define (need-gvm-instr gvm-instr sn-rest)
  (case (gvm-instr-type gvm-instr)
    ((label) sn-rest)
    ((apply)
     (let ((loc (apply-loc gvm-instr)))
       (need-gvm-opnds
        (apply-opnds gvm-instr)
        (need-gvm-loc-opnd loc (need-gvm-loc loc sn-rest)))))
    ((copy)
     (let ((loc (copy-loc gvm-instr)))
       (need-gvm-opnd
        (copy-opnd gvm-instr)
        (need-gvm-loc-opnd loc (need-gvm-loc loc sn-rest)))))
    ((close)
     (let ((parms (close-parms gvm-instr)))
       (define (need-parms-opnds p)
         (if (null? p)
             sn-rest
             (need-gvm-opnds
              (closure-parms-opnds (car p))
              (need-parms-opnds (cdr p)))))
       (define (need-parms-loc p)
         (if (null? p)
             (need-parms-opnds parms)
             (let ((loc (closure-parms-loc (car p))))
               (need-gvm-loc-opnd
                loc
                (need-gvm-loc loc (need-parms-loc (cdr p)))))))
       (need-parms-loc parms)))
    ((ifjump) (need-gvm-opnds (ifjump-opnds gvm-instr) sn-rest))
    ((jump) (need-gvm-opnd (jump-opnd gvm-instr) sn-rest))
    (else
     (compiler-internal-error
      "need-gvm-instr, unknown 'gvm-instr':"
      gvm-instr))))
(define (need-gvm-loc loc sn-rest)
  (if (and loc (stk? loc) (>= (stk-num loc) sn-rest))
      (- (stk-num loc) 1)
      sn-rest))
(define (need-gvm-loc-opnd gvm-loc slots-needed)
  (if (and gvm-loc (clo? gvm-loc))
      (need-gvm-opnd (clo-base gvm-loc) slots-needed)
      slots-needed))
(define (need-gvm-opnd gvm-opnd slots-needed)
  (cond ((stk? gvm-opnd) (max (stk-num gvm-opnd) slots-needed))
        ((clo? gvm-opnd) (need-gvm-opnd (clo-base gvm-opnd) slots-needed))
        (else slots-needed)))
(define (need-gvm-opnds gvm-opnds slots-needed)
  (if (null? gvm-opnds)
      slots-needed
      (need-gvm-opnd
       (car gvm-opnds)
       (need-gvm-opnds (cdr gvm-opnds) slots-needed))))
(define (write-bb bb port)
  (write-gvm-instr (bb-label-instr bb) port)
  (display " [precedents=" port)
  (write (map bb-lbl-num (bb-precedents bb)) port)
  (display "]" port)
  (newline port)
  (for-each
   (lambda (x) (write-gvm-instr x port) (newline port))
   (bb-non-branch-instrs bb))
  (write-gvm-instr (bb-branch-instr bb) port))
(define (write-bbs bbs port)
  (for-each
   (lambda (bb)
     (if (= (bb-lbl-num bb) (bbs-entry-lbl-num bbs))
         (begin (display "**** Entry block:" port) (newline port)))
     (write-bb bb port)
     (newline port))
   (queue->list (bbs-bb-queue bbs))))
(define (virtual.dump proc port)
  (let ((proc-seen (queue-empty)) (proc-left (queue-empty)))
    (define (scan-opnd gvm-opnd)
      (cond ((obj? gvm-opnd)
             (let ((val (obj-val gvm-opnd)))
               (if (and (proc-obj? val)
                        (proc-obj-code val)
                        (not (memq val (queue->list proc-seen))))
                   (begin
                     (queue-put! proc-seen val)
                     (queue-put! proc-left val)))))
            ((clo? gvm-opnd) (scan-opnd (clo-base gvm-opnd)))))
    (define (dump-proc p)
      (define (scan-code code)
        (let ((gvm-instr (code-gvm-instr code)))
          (write-gvm-instr gvm-instr port)
          (newline port)
          (case (gvm-instr-type gvm-instr)
            ((apply)
             (for-each scan-opnd (apply-opnds gvm-instr))
             (if (apply-loc gvm-instr) (scan-opnd (apply-loc gvm-instr))))
            ((copy)
             (scan-opnd (copy-opnd gvm-instr))
             (scan-opnd (copy-loc gvm-instr)))
            ((close)
             (for-each
              (lambda (parms)
                (scan-opnd (closure-parms-loc parms))
                (for-each scan-opnd (closure-parms-opnds parms)))
              (close-parms gvm-instr)))
            ((ifjump) (for-each scan-opnd (ifjump-opnds gvm-instr)))
            ((jump) (scan-opnd (jump-opnd gvm-instr)))
            (else '()))))
      (if (proc-obj-primitive? p)
          (display "**** #[primitive " port)
          (display "**** #[procedure " port))
      (display (proc-obj-name p) port)
      (display "] =" port)
      (newline port)
      (let loop ((l (bbs->code-list (proc-obj-code p)))
                 (prev-filename "")
                 (prev-line 0))
        (if (pair? l)
            (let* ((code (car l))
                   (instr (code-gvm-instr code))
                   (src (comment-get (gvm-instr-comment instr) 'source))
                   (loc (and src (source-locat src)))
                   (filename
                    (if (and loc (eq? (vector-ref loc 0) 'file))
                        (vector-ref loc 1)
                        prev-filename))
                   (line (if (and loc (eq? (vector-ref loc 0) 'file))
                             (vector-ref loc 3)
                             prev-line)))
              (if (or (not (string=? filename prev-filename))
                      (not (= line prev-line)))
                  (begin
                    (display "#line " port)
                    (display line port)
                    (if (not (string=? filename prev-filename))
                        (begin (display " " port) (write filename port)))
                    (newline port)))
              (scan-code code)
              (loop (cdr l) filename line))
            (newline port))))
    (scan-opnd (make-obj proc))
    (let loop ()
      (if (not (queue-empty? proc-left))
          (begin (dump-proc (queue-get! proc-left)) (loop))))))
(define (write-gvm-instr gvm-instr port)
  (define (write-closure-parms parms)
    (display " " port)
    (let ((len (+ 1 (write-gvm-opnd (closure-parms-loc parms) port))))
      (display " = (" port)
      (let ((len (+ len (+ 4 (write-gvm-lbl (closure-parms-lbl parms) port)))))
        (+ len
           (write-terminated-opnd-list (closure-parms-opnds parms) port)))))
  (define (write-terminated-opnd-list l port)
    (let loop ((l l) (len 0))
      (if (pair? l)
          (let ((opnd (car l)))
            (display " " port)
            (loop (cdr l) (+ len (+ 1 (write-gvm-opnd opnd port)))))
          (begin (display ")" port) (+ len 1)))))
  (define (write-param-pattern gvm-instr port)
    (let ((len (if (not (= (label-entry-min gvm-instr)
                           (label-entry-nb-parms gvm-instr)))
                   (let ((len (write-returning-len
                               (label-entry-min gvm-instr)
                               port)))
                     (display "-" port)
                     (+ len 1))
                   0)))
      (let ((len (+ len
                    (write-returning-len
                     (label-entry-nb-parms gvm-instr)
                     port))))
        (if (label-entry-rest? gvm-instr)
            (begin (display "+" port) (+ len 1))
            len))))
  (define (write-prim-applic prim opnds port)
    (display "(" port)
    (let ((len (+ 1 (display-returning-len (proc-obj-name prim) port))))
      (+ len (write-terminated-opnd-list opnds port))))
  (define (write-instr gvm-instr)
    (case (gvm-instr-type gvm-instr)
      ((label)
       (let ((len (write-gvm-lbl (label-lbl-num gvm-instr) port)))
         (display " " port)
         (let ((len (+ len
                       (+ 1
                          (write-returning-len
                           (frame-size (gvm-instr-frame gvm-instr))
                           port)))))
           (case (label-type gvm-instr)
             ((simple) len)
             ((entry)
              (if (label-entry-closed? gvm-instr)
                  (begin
                    (display " closure-entry-point " port)
                    (+ len (+ 21 (write-param-pattern gvm-instr port))))
                  (begin
                    (display " entry-point " port)
                    (+ len (+ 13 (write-param-pattern gvm-instr port))))))
             ((return) (display " return-point" port) (+ len 13))
             ((task-entry) (display " task-entry-point" port) (+ len 17))
             ((task-return) (display " task-return-point" port) (+ len 18))
             (else
              (compiler-internal-error
               "write-gvm-instr, unknown label type"))))))
      ((apply)
       (display "  " port)
       (let ((len (+ 2
                     (if (apply-loc gvm-instr)
                         (let ((len (write-gvm-opnd
                                     (apply-loc gvm-instr)
                                     port)))
                           (display " = " port)
                           (+ len 3))
                         0))))
         (+ len
            (write-prim-applic
             (apply-prim gvm-instr)
             (apply-opnds gvm-instr)
             port))))
      ((copy)
       (display "  " port)
       (let ((len (+ 2 (write-gvm-opnd (copy-loc gvm-instr) port))))
         (display " = " port)
         (+ len (+ 3 (write-gvm-opnd (copy-opnd gvm-instr) port)))))
      ((close)
       (display "  close" port)
       (let ((len (+ 7 (write-closure-parms (car (close-parms gvm-instr))))))
         (let loop ((l (cdr (close-parms gvm-instr))) (len len))
           (if (pair? l)
               (let ((x (car l)))
                 (display "," port)
                 (loop (cdr l) (+ len (+ 1 (write-closure-parms x)))))
               len))))
      ((ifjump)
       (display "  if " port)
       (let ((len (+ 5
                     (write-prim-applic
                      (ifjump-test gvm-instr)
                      (ifjump-opnds gvm-instr)
                      port))))
         (let ((len (+ len
                       (if (ifjump-poll? gvm-instr)
                           (begin (display " jump* " port) 7)
                           (begin (display " jump " port) 6)))))
           (let ((len (+ len
                         (write-returning-len
                          (frame-size (gvm-instr-frame gvm-instr))
                          port))))
             (display " " port)
             (let ((len (+ len
                           (+ 1
                              (write-gvm-lbl (ifjump-true gvm-instr) port)))))
               (display " else " port)
               (+ len (+ 6 (write-gvm-lbl (ifjump-false gvm-instr) port))))))))
      ((jump)
       (display "  " port)
       (let ((len (+ 2
                     (if (jump-poll? gvm-instr)
                         (begin (display "jump* " port) 6)
                         (begin (display "jump " port) 5)))))
         (let ((len (+ len
                       (write-returning-len
                        (frame-size (gvm-instr-frame gvm-instr))
                        port))))
           (display " " port)
           (let ((len (+ len
                         (+ 1 (write-gvm-opnd (jump-opnd gvm-instr) port)))))
             (+ len
                (if (jump-nb-args gvm-instr)
                    (begin
                      (display " " port)
                      (+ 1
                         (write-returning-len (jump-nb-args gvm-instr) port)))
                    0))))))
      (else
       (compiler-internal-error
        "write-gvm-instr, unknown 'gvm-instr':"
        gvm-instr))))
  (define (spaces n)
    (if (> n 0)
        (if (> n 7)
            (begin (display "        " port) (spaces (- n 8)))
            (begin (display " " port) (spaces (- n 1))))))
  (let ((len (write-instr gvm-instr)))
    (spaces (- 40 len))
    (display " " port)
    (write-frame (gvm-instr-frame gvm-instr) port))
  (let ((x (gvm-instr-comment gvm-instr)))
    (if x
        (let ((y (comment-get x 'text)))
          (if y (begin (display " ; " port) (display y port)))))))
(define (write-frame frame port)
  (define (write-var var opnd sep)
    (display sep port)
    (write-gvm-opnd opnd port)
    (if var
        (begin
          (display "=" port)
          (cond ((eq? var closure-env-var)
                 (write (map (lambda (var) (var-name var))
                             (frame-closed frame))
                        port))
                ((eq? var ret-var) (display "#" port))
                ((temp-var? var) (display "." port))
                (else (write (var-name var) port))))))
  (define (live? var)
    (let ((live (frame-live frame)))
      (or (set-member? var live)
          (and (eq? var closure-env-var)
               (not (set-empty?
                     (set-intersection
                      live
                      (list->set (frame-closed frame)))))))))
  (let loop1 ((i 1) (l (reverse (frame-slots frame))) (sep "; "))
    (if (pair? l)
        (let ((var (car l)))
          (write-var (if (live? var) var #f) (make-stk i) sep)
          (loop1 (+ i 1) (cdr l) " "))
        (let loop2 ((i 0) (l (frame-regs frame)) (sep sep))
          (if (pair? l)
              (let ((var (car l)))
                (if (live? var)
                    (begin
                      (write-var var (make-reg i) sep)
                      (loop2 (+ i 1) (cdr l) " "))
                    (loop2 (+ i 1) (cdr l) sep))))))))
(define (write-gvm-opnd gvm-opnd port)
  (define (write-opnd)
    (cond ((reg? gvm-opnd)
           (display "+" port)
           (+ 1 (write-returning-len (reg-num gvm-opnd) port)))
          ((stk? gvm-opnd)
           (display "-" port)
           (+ 1 (write-returning-len (stk-num gvm-opnd) port)))
          ((glo? gvm-opnd) (write-returning-len (glo-name gvm-opnd) port))
          ((clo? gvm-opnd)
           (let ((len (write-gvm-opnd (clo-base gvm-opnd) port)))
             (display "(" port)
             (let ((len (+ len
                           (+ 1
                              (write-returning-len
                               (clo-index gvm-opnd)
                               port)))))
               (display ")" port)
               (+ len 1))))
          ((lbl? gvm-opnd) (write-gvm-lbl (lbl-num gvm-opnd) port))
          ((obj? gvm-opnd)
           (display "'" port)
           (+ (write-gvm-obj (obj-val gvm-opnd) port) 1))
          (else
           (compiler-internal-error
            "write-gvm-opnd, unknown 'gvm-opnd':"
            gvm-opnd))))
  (write-opnd))
(define (write-gvm-lbl lbl port)
  (display "#" port)
  (+ (write-returning-len lbl port) 1))
(define (write-gvm-obj val port)
  (cond ((false-object? val) (display "#f" port) 2)
        ((undef-object? val) (display "#[undefined]" port) 12)
        ((proc-obj? val)
         (if (proc-obj-primitive? val)
             (display "#[primitive " port)
             (display "#[procedure " port))
         (let ((len (display-returning-len (proc-obj-name val) port)))
           (display "]" port)
           (+ len 13)))
        (else (write-returning-len val port))))
(define (virtual.begin!)
  (set! *opnd-table* (make-vector opnd-table-size))
  (set! *opnd-table-alloc* 0)
  '())
(define (virtual.end!) (set! *opnd-table* '()) '())
(define (make-target version name)
  (define current-target-version 4)
  (if (not (= version current-target-version))
      (compiler-internal-error
       "make-target, version of target package is not current"
       name))
  (let ((x (make-vector 11))) (vector-set! x 1 name) x))
(define (target-name x) (vector-ref x 1))
(define (target-begin! x) (vector-ref x 2))
(define (target-begin!-set! x y) (vector-set! x 2 y))
(define (target-end! x) (vector-ref x 3))
(define (target-end!-set! x y) (vector-set! x 3 y))
(define (target-dump x) (vector-ref x 4))
(define (target-dump-set! x y) (vector-set! x 4 y))
(define (target-nb-regs x) (vector-ref x 5))
(define (target-nb-regs-set! x y) (vector-set! x 5 y))
(define (target-prim-info x) (vector-ref x 6))
(define (target-prim-info-set! x y) (vector-set! x 6 y))
(define (target-label-info x) (vector-ref x 7))
(define (target-label-info-set! x y) (vector-set! x 7 y))
(define (target-jump-info x) (vector-ref x 8))
(define (target-jump-info-set! x y) (vector-set! x 8 y))
(define (target-proc-result x) (vector-ref x 9))
(define (target-proc-result-set! x y) (vector-set! x 9 y))
(define (target-task-return x) (vector-ref x 10))
(define (target-task-return-set! x y) (vector-set! x 10 y))
(define targets-loaded '())
(define (get-target name)
  (let ((x (assq name targets-loaded)))
    (if x (cdr x) (compiler-error "Target package is not available" name))))
(define (put-target targ)
  (let* ((name (target-name targ)) (x (assq name targets-loaded)))
    (if x
        (set-cdr! x targ)
        (set! targets-loaded (cons (cons name targ) targets-loaded)))
    '()))
(define (default-target)
  (if (null? targets-loaded)
      (compiler-error "No target package is available")
      (car (car targets-loaded))))
(define (select-target! name info-port)
  (set! target (get-target name))
  ((target-begin! target) info-port)
  (set! target.dump (target-dump target))
  (set! target.nb-regs (target-nb-regs target))
  (set! target.prim-info (target-prim-info target))
  (set! target.label-info (target-label-info target))
  (set! target.jump-info (target-jump-info target))
  (set! target.proc-result (target-proc-result target))
  (set! target.task-return (target-task-return target))
  (set! **not-proc-obj (target.prim-info **not-sym))
  '())
(define (unselect-target!) ((target-end! target)) '())
(define target '())
(define target.dump '())
(define target.nb-regs '())
(define target.prim-info '())
(define target.label-info '())
(define target.jump-info '())
(define target.proc-result '())
(define target.task-return '())
(define **not-proc-obj '())
(define (target.specialized-prim-info* name decl)
  (let ((x (target.prim-info* name decl)))
    (and x ((proc-obj-specialize x) decl))))
(define (target.prim-info* name decl)
  (and (if (standard-procedure name decl)
           (standard-binding? name decl)
           (extended-binding? name decl))
       (target.prim-info name)))
(define generic-sym (string->canonical-symbol "GENERIC"))
(define fixnum-sym (string->canonical-symbol "FIXNUM"))
(define flonum-sym (string->canonical-symbol "FLONUM"))
(define-namable-decl generic-sym 'arith)
(define-namable-decl fixnum-sym 'arith)
(define-namable-decl flonum-sym 'arith)
(define (arith-implementation name decls)
  (declaration-value 'arith name generic-sym decls))
(define (cf source target-name . opts)
  (let* ((dest (file-root source))
         (module-name (file-name dest))
         (info-port (if (memq 'verbose opts) (current-output-port) #f))
         (result (compile-program
                  (list **include-sym source)
                  (if target-name target-name (default-target))
                  opts
                  module-name
                  dest
                  info-port)))
    (if (and info-port (not (eq? info-port (current-output-port))))
        (close-output-port info-port))
    result))
(define (ce source target-name . opts)
  (let* ((dest "program")
         (module-name "program")
         (info-port (if (memq 'verbose opts) (current-output-port) #f))
         (result (compile-program
                  source
                  (if target-name target-name (default-target))
                  opts
                  module-name
                  dest
                  info-port)))
    (if (and info-port (not (eq? info-port (current-output-port))))
        (close-output-port info-port))
    result))
(define wrap-program #f)
(set! wrap-program (lambda (program) program))
(define (compile-program program target-name opts module-name dest info-port)
  (define (compiler-body)
    (if (not (valid-module-name? module-name))
        (compiler-error
         "Invalid characters in file name (must be a symbol with no \"#\")")
        (begin
          (ptree.begin! info-port)
          (virtual.begin!)
          (select-target! target-name info-port)
          (parse-program
           (list (expression->source (wrap-program program) #f))
           (make-global-environment)
           module-name
           (lambda (lst env c-intf)
             (let ((parsed-program
                    (map (lambda (x) (normalize-parse-tree (car x) (cdr x)))
                         lst)))
               (if (memq 'expansion opts)
                   (let ((port (current-output-port)))
                     (display "Expansion:" port)
                     (newline port)
                     (let loop ((l parsed-program))
                       (if (pair? l)
                           (let ((ptree (car l)))
                             (pp-expression
                              (parse-tree->expression ptree)
                              port)
                             (loop (cdr l)))))
                     (newline port)))
               (let ((module-init-proc
                      (compile-parsed-program
                       module-name
                       parsed-program
                       env
                       c-intf
                       info-port)))
                 (if (memq 'report opts) (generate-report env))
                 (if (memq 'gvm opts)
                     (let ((gvm-port
                            (open-output-file (string-append dest ".gvm"))))
                       (virtual.dump module-init-proc gvm-port)
                       (close-output-port gvm-port)))
                 (target.dump module-init-proc dest c-intf opts)
                 (dump-c-intf module-init-proc dest c-intf)))))
          (unselect-target!)
          (virtual.end!)
          (ptree.end!)
          #t)))
  (let ((successful (with-exception-handling compiler-body)))
    (if info-port
        (if successful
            (begin
              (display "Compilation finished." info-port)
              (newline info-port))
            (begin
              (display "Compilation terminated abnormally." info-port)
              (newline info-port))))
    successful))
(define (valid-module-name? module-name)
  (define (valid-char? c)
    (and (not (memv c
                    '(#\#
                      #\;
                      #\(
                      #\)
                      #\space
                      #\[
                      #\]
                      #\{
                      #\}
                      #\"
                      #\'
                      #\`
                      #\,)))
         (not (char-whitespace? c))))
  (let ((n (string-length module-name)))
    (and (> n 0)
         (not (string=? module-name "."))
         (not (string->number module-name 10))
         (let loop ((i 0))
           (if (< i n)
               (if (valid-char? (string-ref module-name i)) (loop (+ i 1)) #f)
               #t)))))
(define (dump-c-intf module-init-proc dest c-intf)
  (let ((decls (c-intf-decls c-intf))
        (procs (c-intf-procs c-intf))
        (inits (c-intf-inits c-intf)))
    (if (or (not (null? decls)) (not (null? procs)) (not (null? inits)))
        (let* ((module-name (proc-obj-name module-init-proc))
               (filename (string-append dest ".c"))
               (port (open-output-file filename)))
          (display "/* File: \"" port)
          (display filename port)
          (display "\", C-interface file produced by Gambit " port)
          (display compiler-version port)
          (display " */" port)
          (newline port)
          (display "#define " port)
          (display c-id-prefix port)
          (display "MODULE_NAME \"" port)
          (display module-name port)
          (display "\"" port)
          (newline port)
          (display "#define " port)
          (display c-id-prefix port)
          (display "MODULE_LINKER " port)
          (display c-id-prefix port)
          (display (scheme-id->c-id module-name) port)
          (newline port)
          (display "#define " port)
          (display c-id-prefix port)
          (display "VERSION \"" port)
          (display compiler-version port)
          (display "\"" port)
          (newline port)
          (if (not (null? procs))
              (begin
                (display "#define " port)
                (display c-id-prefix port)
                (display "C_PRC_COUNT " port)
                (display (length procs) port)
                (newline port)))
          (display "#include \"gambit.h\"" port)
          (newline port)
          (display c-id-prefix port)
          (display "BEGIN_MODULE" port)
          (newline port)
          (for-each
           (lambda (x)
             (let ((scheme-name (vector-ref x 0)))
               (display c-id-prefix port)
               (display "SUPPLY_PRM(" port)
               (display c-id-prefix port)
               (display "P_" port)
               (display (scheme-id->c-id scheme-name) port)
               (display ")" port)
               (newline port)))
           procs)
          (newline port)
          (for-each (lambda (x) (display x port) (newline port)) decls)
          (if (not (null? procs))
              (begin
                (for-each
                 (lambda (x)
                   (let ((scheme-name (vector-ref x 0))
                         (c-name (vector-ref x 1))
                         (arity (vector-ref x 2))
                         (def (vector-ref x 3)))
                     (display c-id-prefix port)
                     (display "BEGIN_C_COD(" port)
                     (display c-name port)
                     (display "," port)
                     (display c-id-prefix port)
                     (display "P_" port)
                     (display (scheme-id->c-id scheme-name) port)
                     (display "," port)
                     (display arity port)
                     (display ")" port)
                     (newline port)
                     (display "#undef ___ARG1" port)
                     (newline port)
                     (display "#define ___ARG1 ___R1" port)
                     (newline port)
                     (display "#undef ___ARG2" port)
                     (newline port)
                     (display "#define ___ARG2 ___R2" port)
                     (newline port)
                     (display "#undef ___ARG3" port)
                     (newline port)
                     (display "#define ___ARG3 ___R3" port)
                     (newline port)
                     (display "#undef ___RESULT" port)
                     (newline port)
                     (display "#define ___RESULT ___R1" port)
                     (newline port)
                     (display def port)
                     (display c-id-prefix port)
                     (display "END_C_COD" port)
                     (newline port)))
                 procs)
                (newline port)
                (display c-id-prefix port)
                (display "BEGIN_C_PRC" port)
                (newline port)
                (let loop ((i 0) (lst procs))
                  (if (not (null? lst))
                      (let* ((x (car lst))
                             (scheme-name (vector-ref x 0))
                             (c-name (vector-ref x 1))
                             (arity (vector-ref x 2)))
                        (if (= i 0) (display " " port) (display "," port))
                        (display c-id-prefix port)
                        (display "DEF_C_PRC(" port)
                        (display c-name port)
                        (display "," port)
                        (display c-id-prefix port)
                        (display "P_" port)
                        (display (scheme-id->c-id scheme-name) port)
                        (display "," port)
                        (display arity port)
                        (display ")" port)
                        (newline port)
                        (loop (+ i 1) (cdr lst)))))
                (display c-id-prefix port)
                (display "END_C_PRC" port)
                (newline port)))
          (newline port)
          (display c-id-prefix port)
          (display "BEGIN_PRM" port)
          (newline port)
          (for-each (lambda (x) (display x port) (newline port)) inits)
          (display c-id-prefix port)
          (display "END_PRM" port)
          (newline port)
          (close-output-port port)))))
(define (generate-report env)
  (let ((vars (sort-variables (env-global-variables env)))
        (decl (env-declarations env)))
    (define (report title pred? vars wrote-something?)
      (if (pair? vars)
          (let ((var (car vars)))
            (if (pred? var)
                (begin
                  (if (not wrote-something?)
                      (begin (display " ") (display title) (newline)))
                  (let loop1 ((l (var-refs var)) (r? #f) (c? #f))
                    (if (pair? l)
                        (let* ((x (car l)) (y (node-parent x)))
                          (if (and y (app? y) (eq? x (app-oper y)))
                              (loop1 (cdr l) r? #t)
                              (loop1 (cdr l) #t c?)))
                        (let loop2 ((l (var-sets var)) (d? #f) (a? #f))
                          (if (pair? l)
                              (if (set? (car l))
                                  (loop2 (cdr l) d? #t)
                                  (loop2 (cdr l) #t a?))
                              (begin
                                (display "  [")
                                (if d? (display "D") (display " "))
                                (if a? (display "A") (display " "))
                                (if r? (display "R") (display " "))
                                (if c? (display "C") (display " "))
                                (display "] ")
                                (display (var-name var))
                                (newline))))))
                  (report title pred? (cdr vars) #t))
                (cons (car vars)
                      (report title pred? (cdr vars) wrote-something?))))
          (begin (if wrote-something? (newline)) '())))
    (display "Global variable usage:")
    (newline)
    (newline)
    (report "OTHERS"
            (lambda (x) #t)
            (report "EXTENDED"
                    (lambda (x) (target.prim-info (var-name x)))
                    (report "STANDARD"
                            (lambda (x) (standard-procedure (var-name x) decl))
                            vars
                            #f)
                    #f)
            #f)))
(define (compile-parsed-program module-name program env c-intf info-port)
  (if info-port (display "Compiling:" info-port))
  (set! trace-indentation 0)
  (set! *bbs* (make-bbs))
  (set! *global-env* env)
  (set! proc-queue '())
  (set! constant-vars '())
  (set! known-procs '())
  (restore-context (make-context 0 '() (list ret-var) '() (entry-poll) #f))
  (let* ((entry-lbl (bbs-new-lbl! *bbs*))
         (body-lbl (bbs-new-lbl! *bbs*))
         (frame (current-frame ret-var-set))
         (comment (if (null? program) #f (source-comment (car program)))))
    (bbs-entry-lbl-num-set! *bbs* entry-lbl)
    (set! entry-bb
          (make-bb (make-label-entry entry-lbl 0 0 #f #f frame comment) *bbs*))
    (bb-put-branch! entry-bb (make-jump (make-lbl body-lbl) #f #f frame #f))
    (set! *bb* (make-bb (make-label-simple body-lbl frame comment) *bbs*))
    (let loop1 ((l (c-intf-procs c-intf)))
      (if (not (null? l))
          (let* ((x (car l))
                 (name (vector-ref x 0))
                 (sym (string->canonical-symbol name))
                 (var (env-lookup-global-var *global-env* sym)))
            (add-constant-var
             var
             (make-obj (make-proc-obj name #t #f 0 #t '() '(#f))))
            (loop1 (cdr l)))))
    (let loop2 ((l program))
      (if (not (null? l))
          (let ((node (car l)))
            (if (def? node)
                (let* ((var (def-var node)) (val (global-val var)))
                  (if (and val (prc? val))
                      (add-constant-var
                       var
                       (make-obj
                        (make-proc-obj
                         (symbol->string (var-name var))
                         #t
                         #f
                         (call-pattern val)
                         #t
                         '()
                         '(#f)))))))
            (loop2 (cdr l)))))
    (let loop3 ((l program))
      (if (null? l)
          (let ((ret-opnd (var->opnd ret-var)))
            (seal-bb #t 'return)
            (dealloc-slots nb-slots)
            (bb-put-branch!
             *bb*
             (make-jump ret-opnd #f #f (current-frame (set-empty)) #f)))
          (let ((node (car l)))
            (if (def? node)
                (begin
                  (gen-define (def-var node) (def-val node) info-port)
                  (loop3 (cdr l)))
                (if (null? (cdr l))
                    (gen-node node ret-var-set 'tail)
                    (begin
                      (gen-node node ret-var-set 'need)
                      (loop3 (cdr l))))))))
    (let loop4 ()
      (if (pair? proc-queue)
          (let ((x (car proc-queue)))
            (set! proc-queue (cdr proc-queue))
            (gen-proc (car x) (cadr x) (caddr x) info-port)
            (trace-unindent info-port)
            (loop4))))
    (if info-port (begin (newline info-port) (newline info-port)))
    (bbs-purify! *bbs*)
    (let ((proc (make-proc-obj
                 (string-append "#!" module-name)
                 #t
                 *bbs*
                 '(0)
                 #t
                 '()
                 '(#f))))
      (set! *bb* '())
      (set! *bbs* '())
      (set! *global-env* '())
      (set! proc-queue '())
      (set! constant-vars '())
      (set! known-procs '())
      (clear-context)
      proc)))
(define *bb* '())
(define *bbs* '())
(define *global-env* '())
(define proc-queue '())
(define constant-vars '())
(define known-procs '())
(define trace-indentation '())
(define (trace-indent info-port)
  (set! trace-indentation (+ trace-indentation 1))
  (if info-port
      (begin
        (newline info-port)
        (let loop ((i trace-indentation))
          (if (> i 0) (begin (display "  " info-port) (loop (- i 1))))))))
(define (trace-unindent info-port)
  (set! trace-indentation (- trace-indentation 1)))
(define (gen-define var node info-port)
  (if (prc? node)
      (let* ((p-bbs *bbs*)
             (p-bb *bb*)
             (p-proc-queue proc-queue)
             (p-known-procs known-procs)
             (p-context (current-context))
             (bbs (make-bbs))
             (lbl1 (bbs-new-lbl! bbs))
             (lbl2 (bbs-new-lbl! bbs))
             (context (entry-context node '()))
             (frame (context->frame
                     context
                     (set-union (free-variables (prc-body node)) ret-var-set)))
             (bb1 (make-bb (make-label-entry
                            lbl1
                            (length (prc-parms node))
                            (prc-min node)
                            (prc-rest node)
                            #f
                            frame
                            (source-comment node))
                           bbs))
             (bb2 (make-bb (make-label-simple lbl2 frame (source-comment node))
                           bbs)))
        (define (do-body)
          (gen-proc node bb2 context info-port)
          (let loop ()
            (if (pair? proc-queue)
                (let ((x (car proc-queue)))
                  (set! proc-queue (cdr proc-queue))
                  (gen-proc (car x) (cadr x) (caddr x) info-port)
                  (trace-unindent info-port)
                  (loop))))
          (trace-unindent info-port)
          (bbs-purify! *bbs*))
        (context-entry-bb-set! context bb1)
        (bbs-entry-lbl-num-set! bbs lbl1)
        (bb-put-branch! bb1 (make-jump (make-lbl lbl2) #f #f frame #f))
        (set! *bbs* bbs)
        (set! proc-queue '())
        (set! known-procs '())
        (if (constant-var? var)
            (let-constant-var
             var
             (make-lbl lbl1)
             (lambda () (add-known-proc lbl1 node) (do-body)))
            (do-body))
        (set! *bbs* p-bbs)
        (set! *bb* p-bb)
        (set! proc-queue p-proc-queue)
        (set! known-procs p-known-procs)
        (restore-context p-context)
        (let* ((x (assq var constant-vars))
               (proc (if x
                         (let ((p (cdr x)))
                           (proc-obj-code-set! (obj-val p) bbs)
                           p)
                         (make-obj
                          (make-proc-obj
                           (symbol->string (var-name var))
                           #f
                           bbs
                           (call-pattern node)
                           #t
                           '()
                           '(#f))))))
          (put-copy
           proc
           (make-glo (var-name var))
           #f
           ret-var-set
           (source-comment node))))
      (put-copy
       (gen-node node ret-var-set 'need)
       (make-glo (var-name var))
       #f
       ret-var-set
       (source-comment node))))
(define (call-pattern node)
  (make-pattern (prc-min node) (length (prc-parms node)) (prc-rest node)))
(define (make-context nb-slots slots regs closed poll entry-bb)
  (vector nb-slots slots regs closed poll entry-bb))
(define (context-nb-slots x) (vector-ref x 0))
(define (context-slots x) (vector-ref x 1))
(define (context-regs x) (vector-ref x 2))
(define (context-closed x) (vector-ref x 3))
(define (context-poll x) (vector-ref x 4))
(define (context-entry-bb x) (vector-ref x 5))
(define (context-entry-bb-set! x y) (vector-set! x 5 y))
(define nb-slots '())
(define slots '())
(define regs '())
(define closed '())
(define poll '())
(define entry-bb '())
(define (restore-context context)
  (set! nb-slots (context-nb-slots context))
  (set! slots (context-slots context))
  (set! regs (context-regs context))
  (set! closed (context-closed context))
  (set! poll (context-poll context))
  (set! entry-bb (context-entry-bb context)))
(define (clear-context)
  (restore-context (make-context '() '() '() '() '() '())))
(define (current-context)
  (make-context nb-slots slots regs closed poll entry-bb))
(define (current-frame live) (make-frame nb-slots slots regs closed live))
(define (context->frame context live)
  (make-frame
   (context-nb-slots context)
   (context-slots context)
   (context-regs context)
   (context-closed context)
   live))
(define (make-poll since-entry? delta) (cons since-entry? delta))
(define (poll-since-entry? x) (car x))
(define (poll-delta x) (cdr x))
(define (entry-poll) (make-poll #f (- poll-period poll-head)))
(define (return-poll poll)
  (let ((delta (poll-delta poll)))
    (make-poll (poll-since-entry? poll) (+ poll-head (max delta poll-tail)))))
(define (poll-merge poll other-poll)
  (make-poll
   (or (poll-since-entry? poll) (poll-since-entry? other-poll))
   (max (poll-delta poll) (poll-delta other-poll))))
(define poll-period #f)
(set! poll-period 90)
(define poll-head #f)
(set! poll-head 15)
(define poll-tail #f)
(set! poll-tail 15)
(define (entry-context proc closed)
  (define (empty-vars-list n)
    (if (> n 0) (cons empty-var (empty-vars-list (- n 1))) '()))
  (let* ((parms (prc-parms proc))
         (pc (target.label-info
              (prc-min proc)
              (length parms)
              (prc-rest proc)
              (not (null? closed))))
         (fs (pcontext-fs pc))
         (slots-list (empty-vars-list fs))
         (regs-list (empty-vars-list target.nb-regs)))
    (define (assign-var-to-loc var loc)
      (let ((x (cond ((reg? loc)
                      (let ((i (reg-num loc)))
                        (if (<= i target.nb-regs)
                            (nth-after regs-list i)
                            (compiler-internal-error
                             "entry-context, reg out of bound in back-end's pcontext"))))
                     ((stk? loc)
                      (let ((i (stk-num loc)))
                        (if (<= i fs)
                            (nth-after slots-list (- fs i))
                            (compiler-internal-error
                             "entry-context, stk out of bound in back-end's pcontext"))))
                     (else
                      (compiler-internal-error
                       "entry-context, loc other than reg or stk in back-end's pcontext")))))
        (if (eq? (car x) empty-var)
            (set-car! x var)
            (compiler-internal-error
             "entry-context, duplicate location in back-end's pcontext"))))
    (let loop ((l (pcontext-map pc)))
      (if (not (null? l))
          (let* ((couple (car l)) (name (car couple)) (loc (cdr couple)))
            (cond ((eq? name 'return) (assign-var-to-loc ret-var loc))
                  ((eq? name 'closure-env)
                   (assign-var-to-loc closure-env-var loc))
                  (else (assign-var-to-loc (list-ref parms (- name 1)) loc)))
            (loop (cdr l)))))
    (make-context fs slots-list regs-list closed (entry-poll) #f)))
(define (get-var opnd)
  (cond ((glo? opnd) (env-lookup-global-var *global-env* (glo-name opnd)))
        ((reg? opnd) (list-ref regs (reg-num opnd)))
        ((stk? opnd) (list-ref slots (- nb-slots (stk-num opnd))))
        (else
         (compiler-internal-error
          "get-var, location must be global, register or stack slot"))))
(define (put-var opnd new)
  (define (put-v opnd new)
    (cond ((reg? opnd) (set! regs (replace-nth regs (reg-num opnd) new)))
          ((stk? opnd)
           (set! slots (replace-nth slots (- nb-slots (stk-num opnd)) new)))
          (else
           (compiler-internal-error
            "put-var, location must be register or stack slot, for var:"
            (var-name new)))))
  (if (eq? new ret-var)
      (let ((x (var->opnd ret-var))) (and x (put-v x empty-var))))
  (put-v opnd new))
(define (flush-regs) (set! regs '()))
(define (push-slot)
  (set! nb-slots (+ nb-slots 1))
  (set! slots (cons empty-var slots)))
(define (dealloc-slots n)
  (set! nb-slots (- nb-slots n))
  (set! slots (nth-after slots n)))
(define (pop-slot) (dealloc-slots 1))
(define (replace-nth l i v)
  (if (null? l)
      (if (= i 0) (list v) (cons empty-var (replace-nth l (- i 1) v)))
      (if (= i 0)
          (cons v (cdr l))
          (cons (car l) (replace-nth (cdr l) (- i 1) v)))))
(define (live-vars live)
  (if (not (set-empty? (set-intersection live (list->set closed))))
      (set-adjoin live closure-env-var)
      live))
(define (dead-slots live)
  (let ((live-v (live-vars live)))
    (define (loop s l i)
      (cond ((null? l) (list->set (reverse s)))
            ((set-member? (car l) live-v) (loop s (cdr l) (- i 1)))
            (else (loop (cons i s) (cdr l) (- i 1)))))
    (loop '() slots nb-slots)))
(define (live-slots live)
  (let ((live-v (live-vars live)))
    (define (loop s l i)
      (cond ((null? l) (list->set (reverse s)))
            ((set-member? (car l) live-v) (loop (cons i s) (cdr l) (- i 1)))
            (else (loop s (cdr l) (- i 1)))))
    (loop '() slots nb-slots)))
(define (dead-regs live)
  (let ((live-v (live-vars live)))
    (define (loop s l i)
      (cond ((>= i target.nb-regs) (list->set (reverse s)))
            ((null? l) (loop (cons i s) l (+ i 1)))
            ((and (set-member? (car l) live-v) (not (memq (car l) slots)))
             (loop s (cdr l) (+ i 1)))
            (else (loop (cons i s) (cdr l) (+ i 1)))))
    (loop '() regs 0)))
(define (live-regs live)
  (let ((live-v (live-vars live)))
    (define (loop s l i)
      (cond ((null? l) (list->set (reverse s)))
            ((and (set-member? (car l) live-v) (not (memq (car l) slots)))
             (loop (cons i s) (cdr l) (+ i 1)))
            (else (loop s (cdr l) (+ i 1)))))
    (loop '() regs 0)))
(define (lowest-dead-slot live)
  (make-stk (or (lowest (dead-slots live)) (+ nb-slots 1))))
(define (highest-live-slot live) (make-stk (or (highest (live-slots live)) 0)))
(define (lowest-dead-reg live)
  (let ((x (lowest (set-remove (dead-regs live) 0)))) (if x (make-reg x) #f)))
(define (highest-dead-reg live)
  (let ((x (highest (dead-regs live)))) (if x (make-reg x) #f)))
(define (highest set) (if (set-empty? set) #f (apply max (set->list set))))
(define (lowest set) (if (set-empty? set) #f (apply min (set->list set))))
(define (above set n) (set-keep (lambda (x) (> x n)) set))
(define (below set n) (set-keep (lambda (x) (< x n)) set))
(define (var->opnd var)
  (let ((x (assq var constant-vars)))
    (if x
        (cdr x)
        (if (global? var)
            (make-glo (var-name var))
            (let ((n (pos-in-list var regs)))
              (if n
                  (make-reg n)
                  (let ((n (pos-in-list var slots)))
                    (if n
                        (make-stk (- nb-slots n))
                        (let ((n (pos-in-list var closed)))
                          (if n
                              (make-clo (var->opnd closure-env-var) (+ n 1))
                              (compiler-internal-error
                               "var->opnd, variable is not accessible:"
                               (var-name var))))))))))))
(define (source-comment node)
  (let ((x (make-comment))) (comment-put! x 'source (node-source node)) x))
(define (sort-variables lst)
  (sort-list
   lst
   (lambda (x y)
     (string<? (symbol->string (var-name x)) (symbol->string (var-name y))))))
(define (add-constant-var var opnd)
  (set! constant-vars (cons (cons var opnd) constant-vars)))
(define (let-constant-var var opnd thunk)
  (let* ((x (assq var constant-vars)) (temp (cdr x)))
    (set-cdr! x opnd)
    (thunk)
    (set-cdr! x temp)))
(define (constant-var? var) (assq var constant-vars))
(define (not-constant-var? var) (not (constant-var? var)))
(define (add-known-proc label proc)
  (set! known-procs (cons (cons label proc) known-procs)))
(define (gen-proc proc bb context info-port)
  (trace-indent info-port)
  (if info-port
      (if (prc-name proc)
          (display (prc-name proc) info-port)
          (display "\"unknown\"" info-port)))
  (let ((lbl (bb-lbl-num bb))
        (live (set-union (free-variables (prc-body proc)) ret-var-set)))
    (set! *bb* bb)
    (restore-context context)
    (gen-node (prc-body proc) ret-var-set 'tail)))
(define (schedule-gen-proc proc closed-list)
  (let* ((lbl1 (bbs-new-lbl! *bbs*))
         (lbl2 (bbs-new-lbl! *bbs*))
         (context (entry-context proc closed-list))
         (frame (context->frame
                 context
                 (set-union (free-variables (prc-body proc)) ret-var-set)))
         (bb1 (make-bb (make-label-entry
                        lbl1
                        (length (prc-parms proc))
                        (prc-min proc)
                        (prc-rest proc)
                        (not (null? closed-list))
                        frame
                        (source-comment proc))
                       *bbs*))
         (bb2 (make-bb (make-label-simple lbl2 frame (source-comment proc))
                       *bbs*)))
    (context-entry-bb-set! context bb1)
    (bb-put-branch! bb1 (make-jump (make-lbl lbl2) #f #f frame #f))
    (set! proc-queue (cons (list proc bb2 context) proc-queue))
    (make-lbl lbl1)))
(define (gen-node node live why)
  (cond ((cst? node) (gen-return (make-obj (cst-val node)) why node))
        ((ref? node)
         (let* ((var (ref-var node)) (name (var-name var)))
           (gen-return
            (cond ((eq? why 'side) (make-obj undef-object))
                  ((global? var)
                   (let ((prim (target.prim-info* name (node-decl node))))
                     (if prim (make-obj prim) (var->opnd var))))
                  (else (var->opnd var)))
            why
            node)))
        ((set? node)
         (let* ((src (gen-node
                      (set-val node)
                      (set-adjoin live (set-var node))
                      'keep))
                (dst (var->opnd (set-var node))))
           (put-copy src dst #f live (source-comment node))
           (gen-return (make-obj undef-object) why node)))
        ((def? node)
         (compiler-internal-error
          "gen-node, 'def' node not at root of parse tree"))
        ((tst? node) (gen-tst node live why))
        ((conj? node) (gen-conj/disj node live why))
        ((disj? node) (gen-conj/disj node live why))
        ((prc? node)
         (let* ((closed (not-constant-closed-vars node))
                (closed-list (sort-variables (set->list closed)))
                (proc-lbl (schedule-gen-proc node closed-list)))
           (let ((opnd (if (null? closed-list)
                           (begin
                             (add-known-proc (lbl-num proc-lbl) node)
                             proc-lbl)
                           (begin
                             (dealloc-slots
                              (- nb-slots
                                 (stk-num (highest-live-slot
                                           (set-union closed live)))))
                             (push-slot)
                             (let ((slot (make-stk nb-slots))
                                   (var (make-temp-var 'closure)))
                               (put-var slot var)
                               (bb-put-non-branch!
                                *bb*
                                (make-close
                                 (list (make-closure-parms
                                        slot
                                        (lbl-num proc-lbl)
                                        (map var->opnd closed-list)))
                                 (current-frame (set-adjoin live var))
                                 (source-comment node)))
                               slot)))))
             (gen-return opnd why node))))
        ((app? node) (gen-call node live why))
        ((fut? node) (gen-fut node live why))
        (else
         (compiler-internal-error
          "gen-node, unknown parse tree node type:"
          node))))
(define (gen-return opnd why node)
  (cond ((eq? why 'tail)
         (let ((var (make-temp-var 'result)))
           (put-copy
            opnd
            target.proc-result
            var
            ret-var-set
            (source-comment node))
           (let ((ret-opnd (var->opnd ret-var)))
             (seal-bb (intrs-enabled? (node-decl node)) 'return)
             (dealloc-slots nb-slots)
             (bb-put-branch!
              *bb*
              (make-jump
               ret-opnd
               #f
               #f
               (current-frame (set-singleton var))
               #f)))))
        (else opnd)))
(define (not-constant-closed-vars val)
  (set-keep not-constant-var? (free-variables val)))
(define (predicate node live cont)
  (define (cont* true-lbl false-lbl) (cont false-lbl true-lbl))
  (define (generic-true-test)
    (predicate-test node live **not-proc-obj '0 (list node) cont*))
  (cond ((or (conj? node) (disj? node)) (predicate-conj/disj node live cont))
        ((app? node)
         (let ((proc (node->proc (app-oper node))))
           (if proc
               (let ((spec (specialize-for-call proc (node-decl node))))
                 (if (and (proc-obj-test spec)
                          (nb-args-conforms?
                           (length (app-args node))
                           (proc-obj-call-pat spec)))
                     (if (eq? spec **not-proc-obj)
                         (predicate (car (app-args node)) live cont*)
                         (predicate-test
                          node
                          live
                          spec
                          (proc-obj-strict-pat proc)
                          (app-args node)
                          cont))
                     (generic-true-test)))
               (generic-true-test))))
        (else (generic-true-test))))
(define (predicate-conj/disj node live cont)
  (let* ((pre (if (conj? node) (conj-pre node) (disj-pre node)))
         (alt (if (conj? node) (conj-alt node) (disj-alt node)))
         (alt-live (set-union live (free-variables alt))))
    (predicate
     pre
     alt-live
     (lambda (true-lbl false-lbl)
       (let ((pre-context (current-context)))
         (set! *bb*
               (make-bb (make-label-simple
                         (if (conj? node) true-lbl false-lbl)
                         (current-frame alt-live)
                         (source-comment alt))
                        *bbs*))
         (predicate
          alt
          live
          (lambda (true-lbl2 false-lbl2)
            (let ((alt-context (current-context)))
              (restore-context pre-context)
              (set! *bb*
                    (make-bb (make-label-simple
                              (if (conj? node) false-lbl true-lbl)
                              (current-frame live)
                              (source-comment alt))
                             *bbs*))
              (merge-contexts-and-seal-bb
               alt-context
               live
               (intrs-enabled? (node-decl node))
               'internal
               (source-comment node))
              (bb-put-branch!
               *bb*
               (make-jump
                (make-lbl (if (conj? node) false-lbl2 true-lbl2))
                #f
                #f
                (current-frame live)
                #f))
              (cont true-lbl2 false-lbl2)))))))))
(define (predicate-test node live test strict-pat args cont)
  (let loop ((args* args) (liv live) (vars* '()))
    (if (not (null? args*))
        (let* ((needed (vals-live-vars liv (cdr args*)))
               (var (save-var
                     (gen-node (car args*) needed 'need)
                     (make-temp-var 'predicate)
                     needed
                     (source-comment (car args*)))))
          (loop (cdr args*) (set-adjoin liv var) (cons var vars*)))
        (let* ((true-lbl (bbs-new-lbl! *bbs*))
               (false-lbl (bbs-new-lbl! *bbs*)))
          (seal-bb (intrs-enabled? (node-decl node)) 'internal)
          (bb-put-branch!
           *bb*
           (make-ifjump
            test
            (map var->opnd (reverse vars*))
            true-lbl
            false-lbl
            #f
            (current-frame live)
            (source-comment node)))
          (cont true-lbl false-lbl)))))
(define (gen-tst node live why)
  (let ((pre (tst-pre node)) (con (tst-con node)) (alt (tst-alt node)))
    (predicate
     pre
     (set-union live (free-variables con) (free-variables alt))
     (lambda (true-lbl false-lbl)
       (let ((pre-context (current-context))
             (true-bb (make-bb (make-label-simple
                                true-lbl
                                (current-frame
                                 (set-union live (free-variables con)))
                                (source-comment con))
                               *bbs*))
             (false-bb
              (make-bb (make-label-simple
                        false-lbl
                        (current-frame (set-union live (free-variables alt)))
                        (source-comment alt))
                       *bbs*)))
         (set! *bb* true-bb)
         (let ((con-opnd (gen-node con live why)))
           (if (eq? why 'tail)
               (begin
                 (restore-context pre-context)
                 (set! *bb* false-bb)
                 (gen-node alt live why))
               (let* ((result-var (make-temp-var 'result))
                      (live-after (set-adjoin live result-var)))
                 (save-opnd-to-reg
                  con-opnd
                  target.proc-result
                  result-var
                  live
                  (source-comment con))
                 (let ((con-context (current-context)) (con-bb *bb*))
                   (restore-context pre-context)
                   (set! *bb* false-bb)
                   (save-opnd-to-reg
                    (gen-node alt live why)
                    target.proc-result
                    result-var
                    live
                    (source-comment alt))
                   (let ((next-lbl (bbs-new-lbl! *bbs*)) (alt-bb *bb*))
                     (if (> (context-nb-slots con-context) nb-slots)
                         (begin
                           (seal-bb (intrs-enabled? (node-decl node))
                                    'internal)
                           (let ((alt-context (current-context)))
                             (restore-context con-context)
                             (set! *bb* con-bb)
                             (merge-contexts-and-seal-bb
                              alt-context
                              live-after
                              (intrs-enabled? (node-decl node))
                              'internal
                              (source-comment node))))
                         (let ((alt-context (current-context)))
                           (restore-context con-context)
                           (set! *bb* con-bb)
                           (seal-bb (intrs-enabled? (node-decl node))
                                    'internal)
                           (let ((con-context* (current-context)))
                             (restore-context alt-context)
                             (set! *bb* alt-bb)
                             (merge-contexts-and-seal-bb
                              con-context*
                              live-after
                              (intrs-enabled? (node-decl node))
                              'internal
                              (source-comment node)))))
                     (let ((frame (current-frame live-after)))
                       (bb-put-branch!
                        con-bb
                        (make-jump (make-lbl next-lbl) #f #f frame #f))
                       (bb-put-branch!
                        alt-bb
                        (make-jump (make-lbl next-lbl) #f #f frame #f))
                       (set! *bb*
                             (make-bb (make-label-simple
                                       next-lbl
                                       frame
                                       (source-comment node))
                                      *bbs*))
                       target.proc-result)))))))))))
(define (nb-args-conforms? n call-pat) (pattern-member? n call-pat))
(define (merge-contexts-and-seal-bb other-context live poll? where comment)
  (let ((live-v (live-vars live))
        (other-nb-slots (context-nb-slots other-context))
        (other-regs (context-regs other-context))
        (other-slots (context-slots other-context))
        (other-poll (context-poll other-context))
        (other-entry-bb (context-entry-bb other-context)))
    (let loop1 ((i (- target.nb-regs 1)))
      (if (>= i 0)
          (let ((other-var (reg->var other-regs i)) (var (reg->var regs i)))
            (if (and (not (eq? var other-var)) (set-member? other-var live-v))
                (let ((r (make-reg i)))
                  (put-var r empty-var)
                  (if (not (or (not (set-member? var live-v))
                               (memq var regs)
                               (memq var slots)))
                      (let ((top (make-stk (+ nb-slots 1))))
                        (put-copy r top var live-v comment)))
                  (put-copy (var->opnd other-var) r other-var live-v comment)))
            (loop1 (- i 1)))))
    (let loop2 ((i 1))
      (if (<= i other-nb-slots)
          (let ((other-var (stk->var other-slots i)) (var (stk->var slots i)))
            (if (and (not (eq? var other-var)) (set-member? other-var live-v))
                (let ((s (make-stk i)))
                  (if (<= i nb-slots) (put-var s empty-var))
                  (if (not (or (not (set-member? var live-v))
                               (memq var regs)
                               (memq var slots)))
                      (let ((top (make-stk (+ nb-slots 1))))
                        (put-copy s top var live-v comment)))
                  (put-copy (var->opnd other-var) s other-var live-v comment))
                (if (> i nb-slots)
                    (let ((top (make-stk (+ nb-slots 1))))
                      (put-copy
                       (make-obj undef-object)
                       top
                       empty-var
                       live-v
                       comment))))
            (loop2 (+ i 1)))))
    (dealloc-slots (- nb-slots other-nb-slots))
    (let loop3 ((i (- target.nb-regs 1)))
      (if (>= i 0)
          (let ((other-var (reg->var other-regs i)) (var (reg->var regs i)))
            (if (not (eq? var other-var)) (put-var (make-reg i) empty-var))
            (loop3 (- i 1)))))
    (let loop4 ((i 1))
      (if (<= i other-nb-slots)
          (let ((other-var (stk->var other-slots i)) (var (stk->var slots i)))
            (if (not (eq? var other-var)) (put-var (make-stk i) empty-var))
            (loop4 (+ i 1)))))
    (seal-bb poll? where)
    (set! poll (poll-merge poll other-poll))
    (if (not (eq? entry-bb other-entry-bb))
        (compiler-internal-error
         "merge-contexts-and-seal-bb, entry-bb's do not agree"))))
(define (seal-bb poll? where)
  (define (my-last-pair l) (if (pair? (cdr l)) (my-last-pair (cdr l)) l))
  (define (poll-at split-point)
    (let loop ((i 0) (l1 (bb-non-branch-instrs *bb*)) (l2 '()))
      (if (< i split-point)
          (loop (+ i 1) (cdr l1) (cons (car l1) l2))
          (let* ((label-instr (bb-label-instr *bb*))
                 (non-branch-instrs1 (reverse l2))
                 (non-branch-instrs2 l1)
                 (frame (gvm-instr-frame
                         (car (my-last-pair
                               (cons label-instr non-branch-instrs1)))))
                 (prec-bb (make-bb label-instr *bbs*))
                 (new-lbl (bbs-new-lbl! *bbs*)))
            (bb-non-branch-instrs-set! prec-bb non-branch-instrs1)
            (bb-put-branch!
             prec-bb
             (make-jump (make-lbl new-lbl) #f #t frame #f))
            (bb-label-instr-set! *bb* (make-label-simple new-lbl frame #f))
            (bb-non-branch-instrs-set! *bb* non-branch-instrs2)
            (set! poll (make-poll #t 0))))))
  (define (poll-at-end) (poll-at (length (bb-non-branch-instrs *bb*))))
  (define (impose-polling-constraints)
    (let ((n (+ (length (bb-non-branch-instrs *bb*)) 1))
          (delta (poll-delta poll)))
      (if (> (+ delta n) poll-period)
          (begin
            (poll-at (max (- poll-period delta) 0))
            (impose-polling-constraints)))))
  (if poll? (impose-polling-constraints))
  (let* ((n (+ (length (bb-non-branch-instrs *bb*)) 1))
         (delta (+ (poll-delta poll) n))
         (since-entry? (poll-since-entry? poll)))
    (if (and poll?
             (case where
               ((call) (> delta (- poll-period poll-head)))
               ((tail-call) (> delta poll-tail))
               ((return) (and since-entry? (> delta (+ poll-head poll-tail))))
               ((internal) #f)
               (else
                (compiler-internal-error "seal-bb, unknown 'where':" where))))
        (poll-at-end)
        (set! poll (make-poll since-entry? delta)))))
(define (reg->var regs i)
  (cond ((null? regs) '())
        ((> i 0) (reg->var (cdr regs) (- i 1)))
        (else (car regs))))
(define (stk->var slots i)
  (let ((j (- (length slots) i))) (if (< j 0) '() (list-ref slots j))))
(define (gen-conj/disj node live why)
  (let ((pre (if (conj? node) (conj-pre node) (disj-pre node)))
        (alt (if (conj? node) (conj-alt node) (disj-alt node))))
    (let ((needed (set-union live (free-variables alt)))
          (bool? (boolean-value? pre))
          (predicate-var (make-temp-var 'predicate)))
      (define (general-predicate node live cont)
        (let* ((con-lbl (bbs-new-lbl! *bbs*)) (alt-lbl (bbs-new-lbl! *bbs*)))
          (save-opnd-to-reg
           (gen-node pre live 'need)
           target.proc-result
           predicate-var
           live
           (source-comment pre))
          (seal-bb (intrs-enabled? (node-decl node)) 'internal)
          (bb-put-branch!
           *bb*
           (make-ifjump
            **not-proc-obj
            (list target.proc-result)
            alt-lbl
            con-lbl
            #f
            (current-frame (set-adjoin live predicate-var))
            (source-comment node)))
          (cont con-lbl alt-lbl)))
      (define (alternative con-lbl alt-lbl)
        (let* ((pre-context (current-context))
               (result-var (make-temp-var 'result))
               (con-live (if bool? live (set-adjoin live predicate-var)))
               (alt-live (set-union live (free-variables alt)))
               (con-bb (make-bb (make-label-simple
                                 con-lbl
                                 (current-frame con-live)
                                 (source-comment alt))
                                *bbs*))
               (alt-bb (make-bb (make-label-simple
                                 alt-lbl
                                 (current-frame alt-live)
                                 (source-comment alt))
                                *bbs*)))
          (if bool?
              (begin
                (set! *bb* con-bb)
                (save-opnd-to-reg
                 (make-obj (if (conj? node) false-object #t))
                 target.proc-result
                 result-var
                 live
                 (source-comment node)))
              (put-var (var->opnd predicate-var) result-var))
          (let ((con-context (current-context)))
            (set! *bb* alt-bb)
            (restore-context pre-context)
            (let ((alt-opnd (gen-node alt live why)))
              (if (eq? why 'tail)
                  (begin
                    (restore-context con-context)
                    (set! *bb* con-bb)
                    (let ((ret-opnd (var->opnd ret-var))
                          (result-set (set-singleton result-var)))
                      (seal-bb (intrs-enabled? (node-decl node)) 'return)
                      (dealloc-slots nb-slots)
                      (bb-put-branch!
                       *bb*
                       (make-jump
                        ret-opnd
                        #f
                        #f
                        (current-frame result-set)
                        #f))))
                  (let ((alt-context* (current-context)) (alt-bb* *bb*))
                    (restore-context con-context)
                    (set! *bb* con-bb)
                    (seal-bb (intrs-enabled? (node-decl node)) 'internal)
                    (let ((con-context* (current-context))
                          (next-lbl (bbs-new-lbl! *bbs*)))
                      (restore-context alt-context*)
                      (set! *bb* alt-bb*)
                      (save-opnd-to-reg
                       alt-opnd
                       target.proc-result
                       result-var
                       live
                       (source-comment alt))
                      (merge-contexts-and-seal-bb
                       con-context*
                       (set-adjoin live result-var)
                       (intrs-enabled? (node-decl node))
                       'internal
                       (source-comment node))
                      (let ((frame (current-frame
                                    (set-adjoin live result-var))))
                        (bb-put-branch!
                         *bb*
                         (make-jump (make-lbl next-lbl) #f #f frame #f))
                        (bb-put-branch!
                         con-bb
                         (make-jump (make-lbl next-lbl) #f #f frame #f))
                        (set! *bb*
                              (make-bb (make-label-simple
                                        next-lbl
                                        frame
                                        (source-comment node))
                                       *bbs*))
                        target.proc-result))))))))
      ((if bool? predicate general-predicate)
       pre
       needed
       (lambda (true-lbl false-lbl)
         (if (conj? node)
             (alternative false-lbl true-lbl)
             (alternative true-lbl false-lbl)))))))
(define (gen-call node live why)
  (let* ((oper (app-oper node)) (args (app-args node)) (nb-args (length args)))
    (if (and (prc? oper)
             (not (prc-rest oper))
             (= (length (prc-parms oper)) nb-args))
        (gen-let (prc-parms oper) args (prc-body oper) live why)
        (if (inlinable-app? node)
            (let ((eval-order (arg-eval-order #f args))
                  (vars (map (lambda (x) (cons x #f)) args)))
              (let loop ((l eval-order) (liv live))
                (if (not (null? l))
                    (let* ((needed (vals-live-vars liv (map car (cdr l))))
                           (arg (car (car l)))
                           (pos (cdr (car l)))
                           (var (save-var
                                 (gen-node arg needed 'need)
                                 (make-temp-var pos)
                                 needed
                                 (source-comment arg))))
                      (set-cdr! (assq arg vars) var)
                      (loop (cdr l) (set-adjoin liv var)))
                    (let ((loc (if (eq? why 'side)
                                   (make-reg 0)
                                   (or (lowest-dead-reg live)
                                       (lowest-dead-slot live)))))
                      (if (and (stk? loc) (> (stk-num loc) nb-slots))
                          (push-slot))
                      (let* ((args (map var->opnd (map cdr vars)))
                             (var (make-temp-var 'result))
                             (proc (node->proc oper))
                             (strict-pat (proc-obj-strict-pat proc)))
                        (if (not (eq? why 'side)) (put-var loc var))
                        (bb-put-non-branch!
                         *bb*
                         (make-apply
                          (specialize-for-call proc (node-decl node))
                          args
                          (if (eq? why 'side) #f loc)
                          (current-frame
                           (if (eq? why 'side) live (set-adjoin live var)))
                          (source-comment node)))
                        (gen-return loc why node))))))
            (let* ((calling-local-proc?
                    (and (ref? oper)
                         (let ((opnd (var->opnd (ref-var oper))))
                           (and (lbl? opnd)
                                (let ((x (assq (lbl-num opnd) known-procs)))
                                  (and x
                                       (let ((proc (cdr x)))
                                         (and (not (prc-rest proc))
                                              (= (prc-min proc) nb-args)
                                              (= (length (prc-parms proc))
                                                 nb-args)
                                              (lbl-num opnd)))))))))
                   (jstate (get-jump-state
                            args
                            (if calling-local-proc?
                                (target.label-info nb-args nb-args #f #f)
                                (target.jump-info nb-args))))
                   (in-stk (jump-state-in-stk jstate))
                   (in-reg (jump-state-in-reg jstate))
                   (eval-order
                    (arg-eval-order (if calling-local-proc? #f oper) in-reg))
                   (live-after
                    (if (eq? why 'tail) (set-remove live ret-var) live))
                   (live-for-regs (args-live-vars live eval-order))
                   (return-lbl (if (eq? why 'tail) #f (bbs-new-lbl! *bbs*))))
              (save-regs
               (live-regs live-after)
               (stk-live-vars live-for-regs in-stk why)
               (source-comment node))
              (let ((frame-start (stk-num (highest-live-slot live-after))))
                (let loop1 ((l in-stk) (liv live-after) (i (+ frame-start 1)))
                  (if (not (null? l))
                      (let ((arg (car l))
                            (slot (make-stk i))
                            (needed (set-union
                                     (stk-live-vars liv (cdr l) why)
                                     live-for-regs)))
                        (if arg
                            (let ((var (if (and (eq? arg 'return)
                                                (eq? why 'tail))
                                           ret-var
                                           (make-temp-var (- frame-start i)))))
                              (save-opnd-to-stk
                               (if (eq? arg 'return)
                                   (if (eq? why 'tail)
                                       (var->opnd ret-var)
                                       (make-lbl return-lbl))
                                   (gen-node arg needed 'need))
                               slot
                               var
                               needed
                               (source-comment
                                (if (eq? arg 'return) node arg)))
                              (loop1 (cdr l) (set-adjoin liv var) (+ i 1)))
                            (begin
                              (if (> i nb-slots)
                                  (put-copy
                                   (make-obj undef-object)
                                   slot
                                   empty-var
                                   liv
                                   (source-comment node)))
                              (loop1 (cdr l) liv (+ i 1)))))
                      (let loop2 ((l eval-order)
                                  (liv liv)
                                  (reg-map '())
                                  (oper-var '()))
                        (if (not (null? l))
                            (let* ((arg (car (car l)))
                                   (pos (cdr (car l)))
                                   (needed (args-live-vars liv (cdr l)))
                                   (var (if (and (eq? arg 'return)
                                                 (eq? why 'tail))
                                            ret-var
                                            (make-temp-var pos)))
                                   (opnd (if (eq? arg 'return)
                                             (if (eq? why 'tail)
                                                 (var->opnd ret-var)
                                                 (make-lbl return-lbl))
                                             (gen-node arg needed 'need))))
                              (if (eq? pos 'operator)
                                  (if (and (ref? arg)
                                           (not (or (obj? opnd) (lbl? opnd))))
                                      (loop2 (cdr l)
                                             (set-adjoin liv (ref-var arg))
                                             reg-map
                                             (ref-var arg))
                                      (begin
                                        (save-arg
                                         opnd
                                         var
                                         needed
                                         (source-comment
                                          (if (eq? arg 'return) node arg)))
                                        (loop2 (cdr l)
                                               (set-adjoin liv var)
                                               reg-map
                                               var)))
                                  (let ((reg (make-reg pos)))
                                    (if (all-args-trivial? (cdr l))
                                        (save-opnd-to-reg
                                         opnd
                                         reg
                                         var
                                         needed
                                         (source-comment
                                          (if (eq? arg 'return) node arg)))
                                        (save-in-slot
                                         opnd
                                         var
                                         needed
                                         (source-comment
                                          (if (eq? arg 'return) node arg))))
                                    (loop2 (cdr l)
                                           (set-adjoin liv var)
                                           (cons (cons pos var) reg-map)
                                           oper-var))))
                            (let loop3 ((i (- target.nb-regs 1)))
                              (if (>= i 0)
                                  (let ((couple (assq i reg-map)))
                                    (if couple
                                        (let ((var (cdr couple)))
                                          (if (not (eq? (reg->var regs i) var))
                                              (save-opnd-to-reg
                                               (var->opnd var)
                                               (make-reg i)
                                               var
                                               liv
                                               (source-comment node)))))
                                    (loop3 (- i 1)))
                                  (let ((opnd (if calling-local-proc?
                                                  (make-lbl
                                                   (+ calling-local-proc? 1))
                                                  (var->opnd oper-var))))
                                    (seal-bb (intrs-enabled? (node-decl node))
                                             (if return-lbl 'call 'tail-call))
                                    (dealloc-slots
                                     (- nb-slots
                                        (+ frame-start (length in-stk))))
                                    (bb-put-branch!
                                     *bb*
                                     (make-jump
                                      opnd
                                      (if calling-local-proc? #f nb-args)
                                      #f
                                      (current-frame liv)
                                      (source-comment node)))
                                    (let ((result-var (make-temp-var 'result)))
                                      (dealloc-slots (- nb-slots frame-start))
                                      (flush-regs)
                                      (put-var target.proc-result result-var)
                                      (if return-lbl
                                          (begin
                                            (set! poll (return-poll poll))
                                            (set! *bb*
                                                  (make-bb (make-label-return
                                                            return-lbl
                                                            (current-frame
                                                             (set-adjoin
                                                              live
                                                              result-var))
                                                            (source-comment
                                                             node))
                                                           *bbs*))))
                                      target.proc-result))))))))))))))
(define (contained-reg/slot opnd)
  (cond ((reg? opnd) opnd)
        ((stk? opnd) opnd)
        ((clo? opnd) (contained-reg/slot (clo-base opnd)))
        (else #f)))
(define (opnd-needed opnd needed)
  (let ((x (contained-reg/slot opnd)))
    (if x (set-adjoin needed (get-var x)) needed)))
(define (save-opnd opnd live comment)
  (let ((slot (lowest-dead-slot live)))
    (put-copy opnd slot (get-var opnd) live comment)))
(define (save-regs regs live comment)
  (for-each
   (lambda (i) (save-opnd (make-reg i) live comment))
   (set->list regs)))
(define (save-opnd-to-reg opnd reg var live comment)
  (if (set-member? (reg-num reg) (live-regs live))
      (save-opnd reg (opnd-needed opnd live) comment))
  (put-copy opnd reg var live comment))
(define (save-opnd-to-stk opnd stk var live comment)
  (if (set-member? (stk-num stk) (live-slots live))
      (save-opnd stk (opnd-needed opnd live) comment))
  (put-copy opnd stk var live comment))
(define (all-args-trivial? l)
  (if (null? l)
      #t
      (let ((arg (car (car l))))
        (or (eq? arg 'return)
            (and (trivial? arg) (all-args-trivial? (cdr l)))))))
(define (every-trivial? l)
  (or (null? l) (and (trivial? (car l)) (every-trivial? (cdr l)))))
(define (trivial? node)
  (or (cst? node)
      (ref? node)
      (and (set? node) (trivial? (set-val node)))
      (and (inlinable-app? node) (every-trivial? (app-args node)))))
(define (inlinable-app? node)
  (if (app? node)
      (let ((proc (node->proc (app-oper node))))
        (and proc
             (let ((spec (specialize-for-call proc (node-decl node))))
               (and (proc-obj-inlinable spec)
                    (nb-args-conforms?
                     (length (app-args node))
                     (proc-obj-call-pat spec))))))
      #f))
(define (boolean-value? node)
  (or (and (conj? node)
           (boolean-value? (conj-pre node))
           (boolean-value? (conj-alt node)))
      (and (disj? node)
           (boolean-value? (disj-pre node))
           (boolean-value? (disj-alt node)))
      (boolean-app? node)))
(define (boolean-app? node)
  (if (app? node)
      (let ((proc (node->proc (app-oper node))))
        (if proc (eq? (type-name (proc-obj-type proc)) 'boolean) #f))
      #f))
(define (node->proc node)
  (cond ((cst? node) (if (proc-obj? (cst-val node)) (cst-val node) #f))
        ((ref? node)
         (if (global? (ref-var node))
             (target.prim-info* (var-name (ref-var node)) (node-decl node))
             #f))
        (else #f)))
(define (specialize-for-call proc decl) ((proc-obj-specialize proc) decl))
(define (get-jump-state args pc)
  (define (empty-node-list n)
    (if (> n 0) (cons #f (empty-node-list (- n 1))) '()))
  (let* ((fs (pcontext-fs pc))
         (slots-list (empty-node-list fs))
         (regs-list (empty-node-list target.nb-regs)))
    (define (assign-node-to-loc var loc)
      (let ((x (cond ((reg? loc)
                      (let ((i (reg-num loc)))
                        (if (<= i target.nb-regs)
                            (nth-after regs-list i)
                            (compiler-internal-error
                             "jump-state, reg out of bound in back-end's pcontext"))))
                     ((stk? loc)
                      (let ((i (stk-num loc)))
                        (if (<= i fs)
                            (nth-after slots-list (- i 1))
                            (compiler-internal-error
                             "jump-state, stk out of bound in back-end's pcontext"))))
                     (else
                      (compiler-internal-error
                       "jump-state, loc other than reg or stk in back-end's pcontext")))))
        (if (not (car x))
            (set-car! x var)
            (compiler-internal-error
             "jump-state, duplicate location in back-end's pcontext"))))
    (let loop ((l (pcontext-map pc)))
      (if (not (null? l))
          (let* ((couple (car l)) (name (car couple)) (loc (cdr couple)))
            (cond ((eq? name 'return) (assign-node-to-loc 'return loc))
                  (else (assign-node-to-loc (list-ref args (- name 1)) loc)))
            (loop (cdr l)))))
    (vector slots-list regs-list)))
(define (jump-state-in-stk x) (vector-ref x 0))
(define (jump-state-in-reg x) (vector-ref x 1))
(define (arg-eval-order oper nodes)
  (define (loop nodes pos part1 part2)
    (cond ((null? nodes)
           (let ((p1 (reverse part1)) (p2 (free-vars-order part2)))
             (cond ((not oper) (append p1 p2))
                   ((trivial? oper)
                    (append p1 p2 (list (cons oper 'operator))))
                   (else (append (cons (cons oper 'operator) p1) p2)))))
          ((not (car nodes)) (loop (cdr nodes) (+ pos 1) part1 part2))
          ((or (eq? (car nodes) 'return) (trivial? (car nodes)))
           (loop (cdr nodes)
                 (+ pos 1)
                 part1
                 (cons (cons (car nodes) pos) part2)))
          (else
           (loop (cdr nodes)
                 (+ pos 1)
                 (cons (cons (car nodes) pos) part1)
                 part2))))
  (loop nodes 0 '() '()))
(define (free-vars-order l)
  (let ((bins '()) (ordered-args '()))
    (define (free-v x) (if (eq? x 'return) (set-empty) (free-variables x)))
    (define (add-to-bin! x)
      (let ((y (assq x bins)))
        (if y (set-cdr! y (+ (cdr y) 1)) (set! bins (cons (cons x 1) bins)))))
    (define (payoff-if-removed node)
      (let ((x (free-v node)))
        (let loop ((l (set->list x)) (r 0))
          (if (null? l)
              r
              (let ((y (cdr (assq (car l) bins))))
                (loop (cdr l) (+ r (quotient 1000 (* y y)))))))))
    (define (remove-free-vars! x)
      (let loop ((l (set->list x)))
        (if (not (null? l))
            (let ((y (assq (car l) bins)))
              (set-cdr! y (- (cdr y) 1))
              (loop (cdr l))))))
    (define (find-max-payoff l thunk)
      (if (null? l)
          (thunk '() -1)
          (find-max-payoff
           (cdr l)
           (lambda (best-arg best-payoff)
             (let ((payoff (payoff-if-removed (car (car l)))))
               (if (>= payoff best-payoff)
                   (thunk (car l) payoff)
                   (thunk best-arg best-payoff)))))))
    (define (remove x l)
      (cond ((null? l) '())
            ((eq? x (car l)) (cdr l))
            (else (cons (car l) (remove x (cdr l))))))
    (for-each
     (lambda (x) (for-each add-to-bin! (set->list (free-v (car x)))))
     l)
    (let loop ((args l) (ordered-args '()))
      (if (null? args)
          (reverse ordered-args)
          (find-max-payoff
           args
           (lambda (best-arg best-payoff)
             (remove-free-vars! (free-v (car best-arg)))
             (loop (remove best-arg args) (cons best-arg ordered-args))))))))
(define (args-live-vars live order)
  (cond ((null? order) live)
        ((eq? (car (car order)) 'return)
         (args-live-vars (set-adjoin live ret-var) (cdr order)))
        (else
         (args-live-vars
          (set-union live (free-variables (car (car order))))
          (cdr order)))))
(define (stk-live-vars live slots why)
  (cond ((null? slots) live)
        ((not (car slots)) (stk-live-vars live (cdr slots) why))
        ((eq? (car slots) 'return)
         (stk-live-vars
          (if (eq? why 'tail) (set-adjoin live ret-var) live)
          (cdr slots)
          why))
        (else
         (stk-live-vars
          (set-union live (free-variables (car slots)))
          (cdr slots)
          why))))
(define (gen-let vars vals node live why)
  (let ((var-val-map (pair-up vars vals))
        (var-set (list->set vars))
        (all-live
         (set-union
          live
          (free-variables node)
          (apply set-union (map free-variables vals)))))
    (define (var->val var) (cdr (assq var var-val-map)))
    (define (proc-var? var) (prc? (var->val var)))
    (define (closed-vars var const-proc-vars)
      (set-difference
       (not-constant-closed-vars (var->val var))
       const-proc-vars))
    (define (no-closed-vars? var const-proc-vars)
      (set-empty? (closed-vars var const-proc-vars)))
    (define (closed-vars? var const-proc-vars)
      (not (no-closed-vars? var const-proc-vars)))
    (define (compute-const-proc-vars proc-vars)
      (let loop1 ((const-proc-vars proc-vars))
        (let ((new-const-proc-vars
               (set-keep
                (lambda (x) (no-closed-vars? x const-proc-vars))
                const-proc-vars)))
          (if (not (set-equal? new-const-proc-vars const-proc-vars))
              (loop1 new-const-proc-vars)
              const-proc-vars))))
    (let* ((proc-vars (set-keep proc-var? var-set))
           (const-proc-vars (compute-const-proc-vars proc-vars))
           (clo-vars
            (set-keep (lambda (x) (closed-vars? x const-proc-vars)) proc-vars))
           (clo-vars-list (set->list clo-vars)))
      (for-each
       (lambda (proc-var)
         (let ((label (schedule-gen-proc (var->val proc-var) '())))
           (add-known-proc (lbl-num label) (var->val proc-var))
           (add-constant-var proc-var label)))
       (set->list const-proc-vars))
      (let ((non-clo-vars-list
             (set->list
              (set-keep
               (lambda (var)
                 (and (not (set-member? var const-proc-vars))
                      (not (set-member? var clo-vars))))
               vars)))
            (liv (set-union
                  live
                  (apply set-union
                         (map (lambda (x) (closed-vars x const-proc-vars))
                              clo-vars-list))
                  (free-variables node))))
        (let loop2 ((vars* non-clo-vars-list))
          (if (not (null? vars*))
              (let* ((var (car vars*))
                     (val (var->val var))
                     (needed (vals-live-vars liv (map var->val (cdr vars*)))))
                (if (var-useless? var)
                    (gen-node val needed 'side)
                    (save-val
                     (gen-node val needed 'need)
                     var
                     needed
                     (source-comment val)))
                (loop2 (cdr vars*)))))
        (if (pair? clo-vars-list)
            (begin
              (dealloc-slots (- nb-slots (stk-num (highest-live-slot liv))))
              (let loop3 ((l clo-vars-list))
                (if (not (null? l))
                    (begin
                      (push-slot)
                      (let ((var (car l)) (slot (make-stk nb-slots)))
                        (put-var slot var)
                        (loop3 (cdr l))))))
              (bb-put-non-branch!
               *bb*
               (make-close
                (map (lambda (var)
                       (let ((closed-list
                              (sort-variables
                               (set->list (closed-vars var const-proc-vars)))))
                         (if (null? closed-list)
                             (compiler-internal-error
                              "gen-let, no closed variables:"
                              (var-name var))
                             (make-closure-parms
                              (var->opnd var)
                              (lbl-num (schedule-gen-proc
                                        (var->val var)
                                        closed-list))
                              (map var->opnd closed-list)))))
                     clo-vars-list)
                (current-frame liv)
                (source-comment node)))))
        (gen-node node live why)))))
(define (save-arg opnd var live comment)
  (if (glo? opnd)
      (add-constant-var var opnd)
      (save-val opnd var live comment)))
(define (save-val opnd var live comment)
  (cond ((or (obj? opnd) (lbl? opnd)) (add-constant-var var opnd))
        ((and (reg? opnd) (not (set-member? (reg-num opnd) (live-regs live))))
         (put-var opnd var))
        ((and (stk? opnd) (not (set-member? (stk-num opnd) (live-slots live))))
         (put-var opnd var))
        (else (save-in-slot opnd var live comment))))
(define (save-in-slot opnd var live comment)
  (let ((slot (lowest-dead-slot live))) (put-copy opnd slot var live comment)))
(define (save-var opnd var live comment)
  (cond ((or (obj? opnd) (lbl? opnd)) (add-constant-var var opnd) var)
        ((or (glo? opnd) (reg? opnd) (stk? opnd)) (get-var opnd))
        (else
         (let ((dest (or (highest-dead-reg live) (lowest-dead-slot live))))
           (put-copy opnd dest var live comment)
           var))))
(define (put-copy opnd loc var live comment)
  (if (and (stk? loc) (> (stk-num loc) nb-slots)) (push-slot))
  (if var (put-var loc var))
  (if (not (eq? opnd loc))
      (bb-put-non-branch!
       *bb*
       (make-copy
        opnd
        loc
        (current-frame (if var (set-adjoin live var) live))
        comment))))
(define (var-useless? var)
  (and (set-empty? (var-refs var)) (set-empty? (var-sets var))))
(define (vals-live-vars live vals)
  (if (null? vals)
      live
      (vals-live-vars
       (set-union live (free-variables (car vals)))
       (cdr vals))))
(define (gen-fut node live why)
  (let* ((val (fut-val node))
         (clo-vars (not-constant-closed-vars val))
         (clo-vars-list (set->list clo-vars))
         (ret-var* (make-temp-var 0))
         (live-after live)
         (live-starting-task
          (set-adjoin (set-union live-after clo-vars) ret-var*))
         (task-lbl (bbs-new-lbl! *bbs*))
         (return-lbl (bbs-new-lbl! *bbs*)))
    (save-regs (live-regs live-after) live-starting-task (source-comment node))
    (let ((frame-start (stk-num (highest-live-slot live-after))))
      (save-opnd-to-reg
       (make-lbl return-lbl)
       target.task-return
       ret-var*
       (set-remove live-starting-task ret-var*)
       (source-comment node))
      (let loop1 ((l clo-vars-list) (i 0))
        (if (null? l)
            (dealloc-slots (- nb-slots (+ frame-start i)))
            (let ((var (car l)) (rest (cdr l)))
              (if (memq var regs)
                  (loop1 rest i)
                  (let loop2 ((j (- target.nb-regs 1)))
                    (if (>= j 0)
                        (if (or (>= j (length regs))
                                (not (set-member?
                                      (list-ref regs j)
                                      live-starting-task)))
                            (let ((reg (make-reg j)))
                              (put-copy
                               (var->opnd var)
                               reg
                               var
                               live-starting-task
                               (source-comment node))
                              (loop1 rest i))
                            (loop2 (- j 1)))
                        (let ((slot (make-stk (+ frame-start (+ i 1))))
                              (needed (list->set rest)))
                          (if (and (or (> (stk-num slot) nb-slots)
                                       (not (memq (list-ref
                                                   slots
                                                   (- nb-slots (stk-num slot)))
                                                  regs)))
                                   (set-member?
                                    (stk-num slot)
                                    (live-slots needed)))
                              (save-opnd
                               slot
                               live-starting-task
                               (source-comment node)))
                          (put-copy
                           (var->opnd var)
                           slot
                           var
                           live-starting-task
                           (source-comment node))
                          (loop1 rest (+ i 1)))))))))
      (seal-bb (intrs-enabled? (node-decl node)) 'call)
      (bb-put-branch!
       *bb*
       (make-jump
        (make-lbl task-lbl)
        #f
        #f
        (current-frame live-starting-task)
        #f))
      (let ((task-context
             (make-context
              (- nb-slots frame-start)
              (reverse (nth-after (reverse slots) frame-start))
              (cons ret-var (cdr regs))
              '()
              poll
              entry-bb))
            (return-context
             (make-context
              frame-start
              (nth-after slots (- nb-slots frame-start))
              '()
              closed
              (return-poll poll)
              entry-bb)))
        (restore-context task-context)
        (set! *bb*
              (make-bb (make-label-task-entry
                        task-lbl
                        (current-frame live-starting-task)
                        (source-comment node))
                       *bbs*))
        (gen-node val ret-var-set 'tail)
        (let ((result-var (make-temp-var 'future)))
          (restore-context return-context)
          (put-var target.proc-result result-var)
          (set! *bb*
                (make-bb (make-label-task-return
                          return-lbl
                          (current-frame (set-adjoin live result-var))
                          (source-comment node))
                         *bbs*))
          (gen-return target.proc-result why node))))))
(define prim-procs
  '(("not" (1) #f 0 boolean)
    ("boolean?" (1) #f 0 boolean)
    ("eqv?" (2) #f 0 boolean)
    ("eq?" (2) #f 0 boolean)
    ("equal?" (2) #f 0 boolean)
    ("pair?" (1) #f 0 boolean)
    ("cons" (2) #f () pair)
    ("car" (1) #f 0 (#f))
    ("cdr" (1) #f 0 (#f))
    ("set-car!" (2) #t (1) pair)
    ("set-cdr!" (2) #t (1) pair)
    ("caar" (1) #f 0 (#f))
    ("cadr" (1) #f 0 (#f))
    ("cdar" (1) #f 0 (#f))
    ("cddr" (1) #f 0 (#f))
    ("caaar" (1) #f 0 (#f))
    ("caadr" (1) #f 0 (#f))
    ("cadar" (1) #f 0 (#f))
    ("caddr" (1) #f 0 (#f))
    ("cdaar" (1) #f 0 (#f))
    ("cdadr" (1) #f 0 (#f))
    ("cddar" (1) #f 0 (#f))
    ("cdddr" (1) #f 0 (#f))
    ("caaaar" (1) #f 0 (#f))
    ("caaadr" (1) #f 0 (#f))
    ("caadar" (1) #f 0 (#f))
    ("caaddr" (1) #f 0 (#f))
    ("cadaar" (1) #f 0 (#f))
    ("cadadr" (1) #f 0 (#f))
    ("caddar" (1) #f 0 (#f))
    ("cadddr" (1) #f 0 (#f))
    ("cdaaar" (1) #f 0 (#f))
    ("cdaadr" (1) #f 0 (#f))
    ("cdadar" (1) #f 0 (#f))
    ("cdaddr" (1) #f 0 (#f))
    ("cddaar" (1) #f 0 (#f))
    ("cddadr" (1) #f 0 (#f))
    ("cdddar" (1) #f 0 (#f))
    ("cddddr" (1) #f 0 (#f))
    ("null?" (1) #f 0 boolean)
    ("list?" (1) #f 0 boolean)
    ("list" 0 #f () list)
    ("length" (1) #f 0 integer)
    ("append" 0 #f 0 list)
    ("reverse" (1) #f 0 list)
    ("list-ref" (2) #f 0 (#f))
    ("memq" (2) #f 0 list)
    ("memv" (2) #f 0 list)
    ("member" (2) #f 0 list)
    ("assq" (2) #f 0 #f)
    ("assv" (2) #f 0 #f)
    ("assoc" (2) #f 0 #f)
    ("symbol?" (1) #f 0 boolean)
    ("symbol->string" (1) #f 0 string)
    ("string->symbol" (1) #f 0 symbol)
    ("number?" (1) #f 0 boolean)
    ("complex?" (1) #f 0 boolean)
    ("real?" (1) #f 0 boolean)
    ("rational?" (1) #f 0 boolean)
    ("integer?" (1) #f 0 boolean)
    ("exact?" (1) #f 0 boolean)
    ("inexact?" (1) #f 0 boolean)
    ("=" 0 #f 0 boolean)
    ("<" 0 #f 0 boolean)
    (">" 0 #f 0 boolean)
    ("<=" 0 #f 0 boolean)
    (">=" 0 #f 0 boolean)
    ("zero?" (1) #f 0 boolean)
    ("positive?" (1) #f 0 boolean)
    ("negative?" (1) #f 0 boolean)
    ("odd?" (1) #f 0 boolean)
    ("even?" (1) #f 0 boolean)
    ("max" 1 #f 0 number)
    ("min" 1 #f 0 number)
    ("+" 0 #f 0 number)
    ("*" 0 #f 0 number)
    ("-" 1 #f 0 number)
    ("/" 1 #f 0 number)
    ("abs" (1) #f 0 number)
    ("quotient" 1 #f 0 integer)
    ("remainder" (2) #f 0 integer)
    ("modulo" (2) #f 0 integer)
    ("gcd" 1 #f 0 integer)
    ("lcm" 1 #f 0 integer)
    ("numerator" (1) #f 0 integer)
    ("denominator" (1) #f 0 integer)
    ("floor" (1) #f 0 integer)
    ("ceiling" (1) #f 0 integer)
    ("truncate" (1) #f 0 integer)
    ("round" (1) #f 0 integer)
    ("rationalize" (2) #f 0 number)
    ("exp" (1) #f 0 number)
    ("log" (1) #f 0 number)
    ("sin" (1) #f 0 number)
    ("cos" (1) #f 0 number)
    ("tan" (1) #f 0 number)
    ("asin" (1) #f 0 number)
    ("acos" (1) #f 0 number)
    ("atan" (1 2) #f 0 number)
    ("sqrt" (1) #f 0 number)
    ("expt" (2) #f 0 number)
    ("make-rectangular" (2) #f 0 number)
    ("make-polar" (2) #f 0 number)
    ("real-part" (1) #f 0 real)
    ("imag-part" (1) #f 0 real)
    ("magnitude" (1) #f 0 real)
    ("angle" (1) #f 0 real)
    ("exact->inexact" (1) #f 0 number)
    ("inexact->exact" (1) #f 0 number)
    ("number->string" (1 2) #f 0 string)
    ("string->number" (1 2) #f 0 number)
    ("char?" (1) #f 0 boolean)
    ("char=?" 0 #f 0 boolean)
    ("char<?" 0 #f 0 boolean)
    ("char>?" 0 #f 0 boolean)
    ("char<=?" 0 #f 0 boolean)
    ("char>=?" 0 #f 0 boolean)
    ("char-ci=?" 0 #f 0 boolean)
    ("char-ci<?" 0 #f 0 boolean)
    ("char-ci>?" 0 #f 0 boolean)
    ("char-ci<=?" 0 #f 0 boolean)
    ("char-ci>=?" 0 #f 0 boolean)
    ("char-alphabetic?" (1) #f 0 boolean)
    ("char-numeric?" (1) #f 0 boolean)
    ("char-whitespace?" (1) #f 0 boolean)
    ("char-upper-case?" (1) #f 0 boolean)
    ("char-lower-case?" (1) #f 0 boolean)
    ("char->integer" (1) #f 0 integer)
    ("integer->char" (1) #f 0 char)
    ("char-upcase" (1) #f 0 char)
    ("char-downcase" (1) #f 0 char)
    ("string?" (1) #f 0 boolean)
    ("make-string" (1 2) #f 0 string)
    ("string" 0 #f 0 string)
    ("string-length" (1) #f 0 integer)
    ("string-ref" (2) #f 0 char)
    ("string-set!" (3) #t 0 string)
    ("string=?" 0 #f 0 boolean)
    ("string<?" 0 #f 0 boolean)
    ("string>?" 0 #f 0 boolean)
    ("string<=?" 0 #f 0 boolean)
    ("string>=?" 0 #f 0 boolean)
    ("string-ci=?" 0 #f 0 boolean)
    ("string-ci<?" 0 #f 0 boolean)
    ("string-ci>?" 0 #f 0 boolean)
    ("string-ci<=?" 0 #f 0 boolean)
    ("string-ci>=?" 0 #f 0 boolean)
    ("substring" (3) #f 0 string)
    ("string-append" 0 #f 0 string)
    ("vector?" (1) #f 0 boolean)
    ("make-vector" (1 2) #f (1) vector)
    ("vector" 0 #f () vector)
    ("vector-length" (1) #f 0 integer)
    ("vector-ref" (2) #f 0 (#f))
    ("vector-set!" (3) #t (1 2) vector)
    ("procedure?" (1) #f 0 boolean)
    ("apply" 2 #t 0 (#f))
    ("map" 2 #t 0 list)
    ("for-each" 2 #t 0 #f)
    ("call-with-current-continuation" (1) #t 0 (#f))
    ("call-with-input-file" (2) #t 0 (#f))
    ("call-with-output-file" (2) #t 0 (#f))
    ("input-port?" (1) #f 0 boolean)
    ("output-port?" (1) #f 0 boolean)
    ("current-input-port" (0) #f 0 port)
    ("current-output-port" (0) #f 0 port)
    ("open-input-file" (1) #t 0 port)
    ("open-output-file" (1) #t 0 port)
    ("close-input-port" (1) #t 0 #f)
    ("close-output-port" (1) #t 0 #f)
    ("eof-object?" (1) #f 0 boolean)
    ("read" (0 1) #t 0 #f)
    ("read-char" (0 1) #t 0 #f)
    ("peek-char" (0 1) #t 0 #f)
    ("write" (0 1) #t 0 #f)
    ("display" (0 1) #t 0 #f)
    ("newline" (0 1) #t 0 #f)
    ("write-char" (1 2) #t 0 #f)
    ("list-tail" (2) #f 0 (#f))
    ("string->list" (1) #f 0 list)
    ("list->string" (1) #f 0 string)
    ("string-copy" (1) #f 0 string)
    ("string-fill!" (2) #t 0 string)
    ("vector->list" (1) #f 0 list)
    ("list->vector" (1) #f 0 vector)
    ("vector-fill!" (2) #t 0 vector)
    ("force" (1) #t 0 #f)
    ("with-input-from-file" (2) #t 0 (#f))
    ("with-output-to-file" (2) #t 0 (#f))
    ("char-ready?" (0 1) #f 0 boolean)
    ("load" (1) #t 0 (#f))
    ("transcript-on" (1) #t 0 #f)
    ("transcript-off" (0) #t 0 #f)
    ("touch" (1) #t 0 #f)
    ("##type" (1) #f () integer)
    ("##type-cast" (2) #f () (#f))
    ("##subtype" (1) #f () integer)
    ("##subtype-set!" (2) #t () #f)
    ("##not" (1) #f () boolean)
    ("##null?" (1) #f () boolean)
    ("##unassigned?" (1) #f () boolean)
    ("##unbound?" (1) #f () boolean)
    ("##eq?" (2) #f () boolean)
    ("##fixnum?" (1) #f () boolean)
    ("##flonum?" (1) #f () boolean)
    ("##special?" (1) #f () boolean)
    ("##pair?" (1) #f () boolean)
    ("##subtyped?" (1) #f () boolean)
    ("##procedure?" (1) #f () boolean)
    ("##placeholder?" (1) #f () boolean)
    ("##vector?" (1) #f () boolean)
    ("##symbol?" (1) #f () boolean)
    ("##ratnum?" (1) #f () boolean)
    ("##cpxnum?" (1) #f () boolean)
    ("##string?" (1) #f () boolean)
    ("##bignum?" (1) #f () boolean)
    ("##char?" (1) #f () boolean)
    ("##closure?" (1) #f () boolean)
    ("##subprocedure?" (1) #f () boolean)
    ("##return-dynamic-env-bind?" (1) #f () boolean)
    ("##fixnum.+" 0 #f () integer)
    ("##fixnum.*" 0 #f () integer)
    ("##fixnum.-" 1 #f () integer)
    ("##fixnum.quotient" (2) #f () integer)
    ("##fixnum.remainder" (2) #f () integer)
    ("##fixnum.modulo" (2) #f () integer)
    ("##fixnum.logior" 0 #f () integer)
    ("##fixnum.logxor" 0 #f () integer)
    ("##fixnum.logand" 0 #f () integer)
    ("##fixnum.lognot" (1) #f () integer)
    ("##fixnum.ash" (2) #f () integer)
    ("##fixnum.lsh" (2) #f () integer)
    ("##fixnum.zero?" (1) #f () boolean)
    ("##fixnum.positive?" (1) #f () boolean)
    ("##fixnum.negative?" (1) #f () boolean)
    ("##fixnum.odd?" (1) #f () boolean)
    ("##fixnum.even?" (1) #f () boolean)
    ("##fixnum.=" 0 #f () boolean)
    ("##fixnum.<" 0 #f () boolean)
    ("##fixnum.>" 0 #f () boolean)
    ("##fixnum.<=" 0 #f () boolean)
    ("##fixnum.>=" 0 #f () boolean)
    ("##flonum.->fixnum" (1) #f () integer)
    ("##flonum.<-fixnum" (1) #f () real)
    ("##flonum.+" 0 #f () real)
    ("##flonum.*" 0 #f () real)
    ("##flonum.-" 1 #f () real)
    ("##flonum./" 1 #f () real)
    ("##flonum.abs" (1) #f () real)
    ("##flonum.truncate" (1) #f () real)
    ("##flonum.round" (1) #f () real)
    ("##flonum.exp" (1) #f () real)
    ("##flonum.log" (1) #f () real)
    ("##flonum.sin" (1) #f () real)
    ("##flonum.cos" (1) #f () real)
    ("##flonum.tan" (1) #f () real)
    ("##flonum.asin" (1) #f () real)
    ("##flonum.acos" (1) #f () real)
    ("##flonum.atan" (1) #f () real)
    ("##flonum.sqrt" (1) #f () real)
    ("##flonum.zero?" (1) #f () boolean)
    ("##flonum.positive?" (1) #f () boolean)
    ("##flonum.negative?" (1) #f () boolean)
    ("##flonum.=" 0 #f () boolean)
    ("##flonum.<" 0 #f () boolean)
    ("##flonum.>" 0 #f () boolean)
    ("##flonum.<=" 0 #f () boolean)
    ("##flonum.>=" 0 #f () boolean)
    ("##char=?" 0 #f () boolean)
    ("##char<?" 0 #f () boolean)
    ("##char>?" 0 #f () boolean)
    ("##char<=?" 0 #f () boolean)
    ("##char>=?" 0 #f () boolean)
    ("##cons" (2) #f () pair)
    ("##set-car!" (2) #t () pair)
    ("##set-cdr!" (2) #t () pair)
    ("##car" (1) #f () (#f))
    ("##cdr" (1) #f () (#f))
    ("##caar" (1) #f () (#f))
    ("##cadr" (1) #f () (#f))
    ("##cdar" (1) #f () (#f))
    ("##cddr" (1) #f () (#f))
    ("##caaar" (1) #f () (#f))
    ("##caadr" (1) #f () (#f))
    ("##cadar" (1) #f () (#f))
    ("##caddr" (1) #f () (#f))
    ("##cdaar" (1) #f () (#f))
    ("##cdadr" (1) #f () (#f))
    ("##cddar" (1) #f () (#f))
    ("##cdddr" (1) #f () (#f))
    ("##caaaar" (1) #f () (#f))
    ("##caaadr" (1) #f () (#f))
    ("##caadar" (1) #f () (#f))
    ("##caaddr" (1) #f () (#f))
    ("##cadaar" (1) #f () (#f))
    ("##cadadr" (1) #f () (#f))
    ("##caddar" (1) #f () (#f))
    ("##cadddr" (1) #f () (#f))
    ("##cdaaar" (1) #f () (#f))
    ("##cdaadr" (1) #f () (#f))
    ("##cdadar" (1) #f () (#f))
    ("##cdaddr" (1) #f () (#f))
    ("##cddaar" (1) #f () (#f))
    ("##cddadr" (1) #f () (#f))
    ("##cdddar" (1) #f () (#f))
    ("##cddddr" (1) #f () (#f))
    ("##make-cell" (1) #f () pair)
    ("##cell-ref" (1) #f () (#f))
    ("##cell-set!" (2) #t () pair)
    ("##vector" 0 #f () vector)
    ("##make-vector" (2) #f () vector)
    ("##vector-length" (1) #f () integer)
    ("##vector-ref" (2) #f () (#f))
    ("##vector-set!" (3) #t () vector)
    ("##vector-shrink!" (2) #t () vector)
    ("##string" 0 #f () string)
    ("##make-string" (2) #f () string)
    ("##string-length" (1) #f () integer)
    ("##string-ref" (2) #f () char)
    ("##string-set!" (3) #t () string)
    ("##string-shrink!" (2) #t () string)
    ("##vector8" 0 #f () string)
    ("##make-vector8" (2) #f () string)
    ("##vector8-length" (1) #f () integer)
    ("##vector8-ref" (2) #f () integer)
    ("##vector8-set!" (3) #t () string)
    ("##vector8-shrink!" (2) #t () string)
    ("##vector16" 0 #f () string)
    ("##make-vector16" (2) #f () string)
    ("##vector16-length" (1) #f () integer)
    ("##vector16-ref" (2) #f () integer)
    ("##vector16-set!" (3) #t () string)
    ("##vector16-shrink!" (2) #t () string)
    ("##closure-code" (1) #f () #f)
    ("##closure-ref" (2) #f () (#f))
    ("##closure-set!" (3) #t () #f)
    ("##subprocedure-id" (1) #f () #f)
    ("##subprocedure-parent" (1) #f () #f)
    ("##return-fs" (1) #f () #f)
    ("##return-link" (1) #f () #f)
    ("##procedure-info" (1) #f () #f)
    ("##pstate" (0) #f () #f)
    ("##make-placeholder" (1) #f 0 (#f))
    ("##touch" (1) #t 0 #f)
    ("##apply" (2) #t () (#f))
    ("##call-with-current-continuation" (1) #t () (#f))
    ("##global-var" (1) #t () #f)
    ("##global-var-ref" (1) #f () (#f))
    ("##global-var-set!" (2) #t () #f)
    ("##atomic-car" (1) #f () (#f))
    ("##atomic-cdr" (1) #f () (#f))
    ("##atomic-set-car!" (2) #t () pair)
    ("##atomic-set-cdr!" (2) #t () pair)
    ("##atomic-set-car-if-eq?!" (3) #t () boolean)
    ("##atomic-set-cdr-if-eq?!" (3) #t () boolean)
    ("##quasi-append" 0 #f 0 list)
    ("##quasi-list" 0 #f () list)
    ("##quasi-cons" (2) #f () pair)
    ("##quasi-list->vector" (1) #f 0 vector)
    ("##case-memv" (2) #f 0 list)))
(define ofile-version-major 5)
(define ofile-version-minor 0)
(define prim-proc-prefix 1)
(define user-proc-prefix 2)
(define pair-prefix 3)
(define flonum-prefix 4)
(define local-object-bits -524281)
(define symbol-object-bits -393209)
(define prim-proc-object-bits -262137)
(define padding-tag 0)
(define end-of-code-tag 32768)
(define m68020-proc-code-tag 32769)
(define m68881-proc-code-tag 32770)
(define stat-tag 32771)
(define global-var-ref-tag 34816)
(define global-var-set-tag 36864)
(define global-var-ref-jump-tag 38912)
(define prim-proc-ref-tag 40960)
(define local-proc-ref-tag 49152)
(define long-index-mask 16383)
(define word-index-mask 2047)
(define (ofile.begin! filename add-obj)
  (set! ofile-add-obj add-obj)
  (set! ofile-syms (queue-empty))
;  (set! *ofile-port1* (open-output-file (string-append filename ".O")))
  (if ofile-asm?
      (begin
        (set! *ofile-port2*
              (asm-open-output-file (string-append filename ".asm")))
        (set! *ofile-pos* 0)))
  (ofile-word ofile-version-major)
  (ofile-word ofile-version-minor)
  '())
(define (ofile.end!)
  (ofile-line "")
;  (close-output-port *ofile-port1*)
  (if ofile-asm? (asm-close-output-port *ofile-port2*))
  '())
(define asm-output '())
(define asm-line '())
(define (asm-open-output-file filename)
  (set! asm-output '())
  (set! asm-line '()))
(define (asm-close-output-port asm-port) #f)
(define (asm-newline asm-port) (asm-display char-newline asm-port))
(define (asm-display obj asm-port)
  (if (eqv? obj char-newline)
      (begin
        (set! asm-output
              (cons (apply string-append (reverse asm-line)) asm-output))
        (set! asm-line '()))
      (set! asm-line
            (cons (cond ((string? obj) obj)
                        ((char? obj) (if (eqv? obj char-tab) " " (string obj)))
                        ((number? obj) (number->string obj))
                        (else (compiler-internal-error "asm-display" obj)))
                  asm-line))))
(define (asm-output-get) (reverse asm-output))
(define *ofile-port1* '())
(define *ofile-port2* '())
(define *ofile-pos* '())
(define ofile-nl char-newline)
(define ofile-tab char-tab)
(define ofile-asm? '())
(set! ofile-asm? '())
(define ofile-asm-bits? '())
(set! ofile-asm-bits? #f)
(define ofile-asm-gvm? '())
(set! ofile-asm-gvm? #f)
(define ofile-stats? '())
(set! ofile-stats? '())
(define ofile-add-obj '())
(set! ofile-add-obj '())
(define ofile-syms '())
(set! ofile-syms '())
(define (ofile-word n)
  (let ((n (modulo n 65536)))
    (if (and ofile-asm? ofile-asm-bits?)
        (let ()
          (define (ofile-display x)
            (asm-display x *ofile-port2*)
            (cond ((eq? x ofile-nl) (set! *ofile-pos* 0))
                  ((eq? x ofile-tab)
                   (set! *ofile-pos* (* (quotient (+ *ofile-pos* 8) 8) 8)))
                  (else (set! *ofile-pos* (+ *ofile-pos* (string-length x))))))
          (if (> *ofile-pos* 64) (ofile-display ofile-nl))
          (if (= *ofile-pos* 0) (ofile-display " .word") (ofile-display ","))
          (ofile-display ofile-tab)
          (let ((s (make-string 6 #\0)))
            (string-set! s 1 #\x)
            (let loop ((i 5) (n n))
              (if (> n 0)
                  (begin
                    (string-set!
                     s
                     i
                     (string-ref "0123456789ABCDEF" (remainder n 16)))
                    (loop (- i 1) (quotient n 16)))))
            (ofile-display s))))
'    (write-word n *ofile-port1*)))
(define (ofile-long x) (ofile-word (upper-16bits x)) (ofile-word x))
(define (ofile-string s)
  (let ((len (string-length s)))
    (define (ref i) (if (>= i len) 0 (character-encoding (string-ref s i))))
    (let loop ((i 0))
      (if (< i len)
          (begin
            (ofile-word (+ (* (ref i) 256) (ref (+ i 1))))
            (loop (+ i 2)))))
    (if (= (remainder len 2) 0) (ofile-word 0))))
(define (ofile-wsym tag name)
  (let ((n (string-pos-in-list name (queue->list ofile-syms))))
    (if n
        (ofile-word (+ tag n))
        (let ((m (length (queue->list ofile-syms))))
          (queue-put! ofile-syms name)
          (ofile-word (+ tag word-index-mask))
          (ofile-string name)))))
(define (ofile-lsym tag name)
  (let ((n (string-pos-in-list name (queue->list ofile-syms))))
    (if n
        (ofile-long (+ tag (* n 8)))
        (let ((m (length (queue->list ofile-syms))))
          (queue-put! ofile-syms name)
          (ofile-long (+ tag (* long-index-mask 8)))
          (ofile-string name)))))
(define (ofile-ref obj)
  (let ((n (obj-encoding obj)))
    (if n
        (ofile-long n)
        (if (symbol-object? obj)
            (begin (ofile-lsym symbol-object-bits (symbol->string obj)))
            (let ((m (ofile-add-obj obj)))
              (if m
                  (ofile-long (+ local-object-bits (* m 8)))
                  (begin
                    (ofile-lsym
                     prim-proc-object-bits
                     (proc-obj-name obj)))))))))
(define (ofile-prim-proc s)
  (ofile-long prim-proc-prefix)
  (ofile-wsym 0 s)
  (ofile-comment (list "| #[primitive " s "] =")))
(define (ofile-user-proc) (ofile-long user-proc-prefix))
(define (ofile-line s)
  (if ofile-asm?
      (begin
        (if (> *ofile-pos* 0) (asm-newline *ofile-port2*))
        (asm-display s *ofile-port2*)
        (asm-newline *ofile-port2*)
        (set! *ofile-pos* 0))))
(define (ofile-tabs-to n)
  (let loop ()
    (if (< *ofile-pos* n)
        (begin
          (asm-display ofile-tab *ofile-port2*)
          (set! *ofile-pos* (* (quotient (+ *ofile-pos* 8) 8) 8))
          (loop)))))
(define (ofile-comment l)
  (if ofile-asm?
      (let ()
        (if ofile-asm-bits?
            (begin (ofile-tabs-to 32) (asm-display "|" *ofile-port2*)))
        (for-each (lambda (x) (asm-display x *ofile-port2*)) l)
        (asm-newline *ofile-port2*)
        (set! *ofile-pos* 0))))
(define (ofile-gvm-instr code)
  (if (and ofile-asm? ofile-asm-gvm?)
      (let ((gvm-instr (code-gvm-instr code)) (sn (code-slots-needed code)))
        (if (> *ofile-pos* 0)
            (begin (asm-newline *ofile-port2*) (set! *ofile-pos* 0)))
        (if ofile-asm-bits? (ofile-tabs-to 32))
        (asm-display "| GVM: [" *ofile-port2*)
        (asm-display sn *ofile-port2*)
        (asm-display "] " *ofile-port2*)
        (asm-newline *ofile-port2*)
        (set! *ofile-pos* 0))))
(define (ofile-stat stat)
  (define (obj->string x)
    (cond ((string? x) x)
          ((symbol-object? x) (symbol->string x))
          ((number? x) (number->string x))
          ((false-object? x) "#f")
          ((eq? x #t) "#t")
          ((null? x) "()")
          ((pair? x)
           (let loop ((l1 (cdr x)) (l2 (list (obj->string (car x)) "(")))
             (cond ((pair? l1)
                    (loop (cdr l1)
                          (cons (obj->string (car l1)) (cons " " l2))))
                   ((null? l1) (apply string-append (reverse (cons ")" l2))))
                   (else
                    (apply string-append
                           (reverse (cons ")"
                                          (cons (obj->string l1)
                                                (cons " . " l2)))))))))
          (else
           (compiler-internal-error
            "ofile-stat, can't convert to string 'x'"
            x))))
  (ofile-string (obj->string stat)))
(define (upper-16bits x)
  (cond ((>= x 0) (quotient x 65536))
        ((>= x (- 65536)) -1)
        (else (- (quotient (+ x 65537) 65536) 2))))
(define type-fixnum 0)
(define type-flonum 1)
(define type-special 7)
(define type-pair 4)
(define type-placeholder 5)
(define type-subtyped 3)
(define type-procedure 2)
(define subtype-vector 0)
(define subtype-symbol 1)
(define subtype-port 2)
(define subtype-ratnum 3)
(define subtype-cpxnum 4)
(define subtype-string 16)
(define subtype-bignum 17)
(define data-false (- 33686019))
(define data-null (- 67372037))
(define data-true -2)
(define data-undef -3)
(define data-unass -4)
(define data-unbound -5)
(define data-eof -6)
(define data-max-fixnum 268435455)
(define data-min-fixnum (- 268435456))
(define (make-encoding data type) (+ (* data 8) type))
(define (obj-type obj)
  (cond ((false-object? obj) 'special)
        ((undef-object? obj) 'special)
        ((symbol-object? obj) 'subtyped)
        ((proc-obj? obj) 'procedure)
        ((eq? obj #t) 'special)
        ((null? obj) 'special)
        ((pair? obj) 'pair)
        ((number? obj)
         (cond ((and (integer? obj)
                     (exact? obj)
                     (>= obj data-min-fixnum)
                     (<= obj data-max-fixnum))
                'fixnum)
               (
#t
;;                (and (inexact? (real-part obj))
;;                     (zero? (imag-part obj))
;;                     (exact? (imag-part obj)))
                'flonum)
               (else 'subtyped)))
        ((char? obj) 'special)
        (else 'subtyped)))
(define (obj-subtype obj)
  (cond ((symbol-object? obj) 'symbol)
        ((number? obj)
         (cond ((and (integer? obj) (exact? obj)) 'bignum)
               ((and (rational? obj) (exact? obj)) 'ratnum)
               (else 'cpxnum)))
        ((vector? obj) 'vector)
        ((string? obj) 'string)
        (else
         (compiler-internal-error "obj-subtype, unknown object 'obj'" obj))))
(define (obj-type-tag obj)
  (case (obj-type obj)
    ((fixnum) type-fixnum)
    ((flonum) type-flonum)
    ((special) type-special)
    ((pair) type-pair)
    ((subtyped) type-subtyped)
    ((procedure) type-procedure)
    (else (compiler-internal-error "obj-type-tag, unknown object 'obj'" obj))))
(define (obj-encoding obj)
  (case (obj-type obj)
    ((fixnum) (make-encoding obj type-fixnum))
    ((special)
     (make-encoding
      (cond ((false-object? obj) data-false)
            ((undef-object? obj) data-undef)
            ((eq? obj #t) data-true)
            ((null? obj) data-null)
            ((char? obj) (character-encoding obj))
            (else
             (compiler-internal-error
              "obj-encoding, unknown SPECIAL object 'obj'"
              obj)))
      type-special))
    (else #f)))
(define bits-false (make-encoding data-false type-special))
(define bits-null (make-encoding data-null type-special))
(define bits-true (make-encoding data-true type-special))
(define bits-unass (make-encoding data-unass type-special))
(define bits-unbound (make-encoding data-unbound type-special))
(define (asm.begin!)
  (set! asm-code-queue (queue-empty))
  (set! asm-const-queue (queue-empty))
  '())
(define (asm.end! debug-info)
  (asm-assemble! debug-info)
  (set! asm-code-queue '())
  (set! asm-const-queue '())
  '())
(define asm-code-queue '())
(define asm-const-queue '())
(define (asm-word x) (queue-put! asm-code-queue (modulo x 65536)))
(define (asm-long x) (asm-word (upper-16bits x)) (asm-word x))
(define (asm-label lbl label-descr)
  (queue-put! asm-code-queue (cons 'label (cons lbl label-descr))))
(define (asm-comment x) (queue-put! asm-code-queue (cons 'comment x)))
(define (asm-align n offset)
  (queue-put! asm-code-queue (cons 'align (cons n offset))))
(define (asm-ref-glob glob)
  (queue-put!
   asm-code-queue
   (cons 'ref-glob (symbol->string (glob-name glob)))))
(define (asm-set-glob glob)
  (queue-put!
   asm-code-queue
   (cons 'set-glob (symbol->string (glob-name glob)))))
(define (asm-ref-glob-jump glob)
  (queue-put!
   asm-code-queue
   (cons 'ref-glob-jump (symbol->string (glob-name glob)))))
(define (asm-proc-ref num offset)
  (queue-put! asm-code-queue (cons 'proc-ref (cons num offset))))
(define (asm-prim-ref proc offset)
  (queue-put!
   asm-code-queue
   (cons 'prim-ref (cons (proc-obj-name proc) offset))))
(define (asm-m68020-proc) (queue-put! asm-code-queue '(m68020-proc)))
(define (asm-m68881-proc) (queue-put! asm-code-queue '(m68881-proc)))
(define (asm-stat x) (queue-put! asm-code-queue (cons 'stat x)))
(define (asm-brel type lbl)
  (queue-put! asm-code-queue (cons 'brab (cons type lbl))))
(define (asm-wrel lbl offs)
  (queue-put! asm-code-queue (cons 'wrel (cons lbl offs))))
(define (asm-lrel lbl offs n)
  (queue-put! asm-code-queue (cons 'lrel (cons lbl (cons offs n)))))
(define (asm-assemble! debug-info)
  (define header-offset 2)
  (define ref-glob-len 2)
  (define set-glob-len 10)
  (define ref-glob-jump-len 2)
  (define proc-ref-len 4)
  (define prim-ref-len 4)
  (define stat-len 4)
  (define (padding loc n offset) (modulo (- offset loc) n))
  (queue-put! asm-const-queue debug-info)
  (asm-align 4 0)
  (emit-label const-lbl)
  (let ((code-list (queue->list asm-code-queue))
        (const-list (queue->list asm-const-queue)))
    (let* ((fix-list
            (let loop ((l code-list) (len header-offset) (x '()))
              (if (null? l)
                  (reverse x)
                  (let ((part (car l)) (rest (cdr l)))
                    (if (pair? part)
                        (case (car part)
                          ((label align brab)
                           (loop rest 0 (cons (cons len part) x)))
                          ((wrel) (loop rest (+ len 2) x))
                          ((lrel) (loop rest (+ len 4) x))
                          ((ref-glob) (loop rest (+ len ref-glob-len) x))
                          ((set-glob) (loop rest (+ len set-glob-len) x))
                          ((ref-glob-jump)
                           (loop rest (+ len ref-glob-jump-len) x))
                          ((proc-ref) (loop rest (+ len proc-ref-len) x))
                          ((prim-ref) (loop rest (+ len prim-ref-len) x))
                          ((stat) (loop rest (+ len stat-len) x))
                          ((comment m68020-proc m68881-proc) (loop rest len x))
                          (else
                           (compiler-internal-error
                            "asm-assemble!, unknown code list element"
                            part)))
                        (loop rest (+ len 2) x))))))
           (lbl-list
            (let loop ((l fix-list) (x '()))
              (if (null? l)
                  x
                  (let ((part (cdar l)) (rest (cdr l)))
                    (if (eq? (car part) 'label)
                        (loop rest (cons (cons (cadr part) part) x))
                        (loop rest x)))))))
      (define (replace-lbl-refs-by-pointer-to-label)
        (let loop ((l code-list))
          (if (not (null? l))
              (let ((part (car l)) (rest (cdr l)))
                (if (pair? part)
                    (case (car part)
                      ((brab)
                       (set-cdr! (cdr part) (cdr (assq (cddr part) lbl-list))))
                      ((wrel)
                       (set-car! (cdr part) (cdr (assq (cadr part) lbl-list))))
                      ((lrel)
                       (set-car!
                        (cdr part)
                        (cdr (assq (cadr part) lbl-list))))))
                (loop rest)))))
      (define (assign-loc-to-labels)
        (let loop ((l fix-list) (loc 0))
          (if (not (null? l))
              (let* ((first (car l))
                     (rest (cdr l))
                     (len (car first))
                     (cur-loc (+ loc len))
                     (part (cdr first)))
                (case (car part)
                  ((label)
                   (if (cddr part)
                       (vector-set!
                        (cddr part)
                        0
                        (quotient (- cur-loc header-offset) 8)))
                   (set-car! (cdr part) cur-loc)
                   (loop rest cur-loc))
                  ((align)
                   (loop rest
                         (+ cur-loc
                            (padding cur-loc (cadr part) (cddr part)))))
                  ((brab) (loop rest (+ cur-loc 2)))
                  ((braw) (loop rest (+ cur-loc 4)))
                  (else
                   (compiler-internal-error
                    "assign-loc-to-labels, unknown code list element"
                    part)))))))
      (define (branch-tensioning-pass)
        (assign-loc-to-labels)
        (let loop ((changed? #f) (l fix-list) (loc 0))
          (if (null? l)
              (if changed? (branch-tensioning-pass))
              (let* ((first (car l))
                     (rest (cdr l))
                     (len (car first))
                     (cur-loc (+ loc len))
                     (part (cdr first)))
                (case (car part)
                  ((label) (loop changed? rest cur-loc))
                  ((align)
                   (loop changed?
                         rest
                         (+ cur-loc
                            (padding cur-loc (cadr part) (cddr part)))))
                  ((brab)
                   (let ((dist (- (cadr (cddr part)) (+ cur-loc 2))))
                     (if (or (< dist -128) (> dist 127) (= dist 0))
                         (begin
                           (set-car! part 'braw)
                           (loop #t rest (+ cur-loc 2)))
                         (loop changed? rest (+ cur-loc 2)))))
                  ((braw) (loop changed? rest (+ cur-loc 4)))
                  (else
                   (compiler-internal-error
                    "branch-tensioning-pass, unknown code list element"
                    part)))))))
      (define (write-block start-loc end-loc start end)
        (if (> end-loc start-loc)
            (ofile-word (quotient (- end-loc start-loc) 2)))
        (let loop ((loc start-loc) (l start))
          (if (not (eq? l end))
              (let ((part (car l)) (rest (cdr l)))
                (if (pair? part)
                    (case (car part)
                      ((label) (loop loc rest))
                      ((align)
                       (let ((n (padding loc (cadr part) (cddr part))))
                         (let pad ((i 0))
                           (if (< i n)
                               (begin (ofile-word 0) (pad (+ i 2)))
                               (loop (+ loc n) rest)))))
                      ((brab)
                       (let ((dist (- (cadr (cddr part)) (+ loc 2))))
                         (ofile-word (+ (cadr part) (modulo dist 256)))
                         (loop (+ loc 2) rest)))
                      ((braw)
                       (let ((dist (- (cadr (cddr part)) (+ loc 2))))
                         (ofile-word (cadr part))
                         (ofile-word (modulo dist 65536))
                         (loop (+ loc 4) rest)))
                      ((wrel)
                       (let ((dist (+ (- (cadr (cadr part)) loc) (cddr part))))
                         (ofile-word (modulo dist 65536))
                         (loop (+ loc 2) rest)))
                      ((lrel)
                       (let ((dist (+ (- (cadr (cadr part)) loc)
                                      (caddr part))))
                         (ofile-long (+ (* dist 65536) (cdddr part)))
                         (loop (+ loc 4) rest)))
                      ((comment)
                       (let ((x (cdr part)))
                         (if (pair? x) (ofile-comment x) (ofile-gvm-instr x))
                         (loop loc rest))))
                    (begin (ofile-word part) (loop (+ loc 2) rest)))))))
      (define (write-code)
        (let ((proc-len
               (+ (cadr (cdr (assq const-lbl lbl-list)))
                  (* (length const-list) 4))))
          (if (>= proc-len 32768)
              (compiler-limitation-error
               "procedure is too big (32K bytes limit per procedure)"))
          (ofile-word (+ 32768 proc-len)))
        (let loop1 ((start code-list) (start-loc header-offset))
          (let loop2 ((end start) (loc start-loc))
            (if (null? end)
                (write-block start-loc loc start end)
                (let ((part (car end)) (rest (cdr end)))
                  (if (pair? part)
                      (case (car part)
                        ((label comment) (loop2 rest loc))
                        ((align)
                         (loop2 rest
                                (+ loc (padding loc (cadr part) (cddr part)))))
                        ((brab wrel) (loop2 rest (+ loc 2)))
                        ((braw) (loop2 rest (+ loc 4)))
                        ((lrel) (loop2 rest (+ loc 4)))
                        (else
                         (write-block start-loc loc start end)
                         (case (car part)
                           ((ref-glob)
                            (ofile-wsym global-var-ref-tag (cdr part))
                            (loop1 rest (+ loc ref-glob-len)))
                           ((set-glob)
                            (ofile-wsym global-var-set-tag (cdr part))
                            (loop1 rest (+ loc set-glob-len)))
                           ((ref-glob-jump)
                            (ofile-wsym global-var-ref-jump-tag (cdr part))
                            (loop1 rest (+ loc ref-glob-jump-len)))
                           ((proc-ref)
                            (ofile-word (+ local-proc-ref-tag (cadr part)))
                            (ofile-word (cddr part))
                            (loop1 rest (+ loc proc-ref-len)))
                           ((prim-ref)
                            (ofile-wsym prim-proc-ref-tag (cadr part))
                            (ofile-word (cddr part))
                            (loop1 rest (+ loc prim-ref-len)))
                           ((m68020-proc)
                            (ofile-word m68020-proc-code-tag)
                            (loop1 rest loc))
                           ((m68881-proc)
                            (ofile-word m68881-proc-code-tag)
                            (loop1 rest loc))
                           ((stat)
                            (ofile-word stat-tag)
                            (ofile-stat (cdr part))
                            (loop1 rest (+ loc stat-len))))))
                      (loop2 rest (+ loc 2)))))))
        (ofile-word end-of-code-tag)
        (for-each ofile-ref const-list)
        (ofile-long (obj-encoding (+ (length const-list) 1))))
      (replace-lbl-refs-by-pointer-to-label)
      (branch-tensioning-pass)
      (write-code))))
(define const-lbl 0)
(define (identical-opnd68? opnd1 opnd2) (eqv? opnd1 opnd2))
(define (reg68? x) (or (dreg? x) (areg? x)))
(define (make-dreg num) num)
(define (dreg? x) (and (integer? x) (>= x 0) (< x 8)))
(define (dreg-num x) x)
(define (make-areg num) (+ num 8))
(define (areg? x) (and (integer? x) (>= x 8) (< x 16)))
(define (areg-num x) (- x 8))
(define (make-ind areg) (+ areg 8))
(define (ind? x) (and (integer? x) (>= x 16) (< x 24)))
(define (ind-areg x) (- x 8))
(define (make-pinc areg) (+ areg 16))
(define (pinc? x) (and (integer? x) (>= x 24) (< x 32)))
(define (pinc-areg x) (- x 16))
(define (make-pdec areg) (+ areg 24))
(define (pdec? x) (and (integer? x) (>= x 32) (< x 40)))
(define (pdec-areg x) (- x 24))
(define (make-disp areg offset) (+ (+ areg 32) (* (modulo offset 65536) 8)))
(define (disp? x) (and (integer? x) (>= x 40) (< x 524328)))
(define (disp-areg x) (+ (remainder x 8) 8))
(define (disp-offset x)
  (- (modulo (+ (quotient (- x 40) 8) 32768) 65536) 32768))
(define (make-disp* areg offset)
  (if (= offset 0) (make-ind areg) (make-disp areg offset)))
(define (disp*? x) (or (ind? x) (disp? x)))
(define (disp*-areg x) (if (ind? x) (ind-areg x) (disp-areg x)))
(define (disp*-offset x) (if (ind? x) 0 (disp-offset x)))
(define (make-inx areg ireg offset)
  (+ (+ areg 524320) (* ireg 8) (* (modulo offset 256) 128)))
(define (inx? x) (and (integer? x) (>= x 524328) (< x 557096)))
(define (inx-areg x) (+ (remainder (- x 524328) 8) 8))
(define (inx-ireg x) (quotient (remainder (- x 524328) 128) 8))
(define (inx-offset x)
  (- (modulo (+ (quotient (- x 524328) 128) 128) 256) 128))
(define (make-freg num) (+ 557096 num))
(define (freg? x) (and (integer? x) (>= x 557096) (< x 557104)))
(define (freg-num x) (- x 557096))
(define (make-pcr lbl offset)
  (+ 557104 (+ (modulo offset 65536) (* lbl 65536))))
(define (pcr? x) (and (integer? x) (>= x 557104)))
(define (pcr-lbl x) (quotient (- x 557104) 65536))
(define (pcr-offset x) (- (modulo (- x 524336) 65536) 32768))
(define (make-imm val) (if (< val 0) (* val 2) (- -1 (* val 2))))
(define (imm? x) (and (integer? x) (< x 0)))
(define (imm-val x) (if (even? x) (quotient x 2) (- (quotient x 2))))
(define (make-glob name) name)
(define (glob? x) (symbol? x))
(define (glob-name x) x)
(define (make-frame-base-rel slot) (make-disp sp-reg slot))
(define (frame-base-rel? x)
  (and (disp? x) (identical-opnd68? sp-reg (disp-areg x))))
(define (frame-base-rel-slot x) (disp-offset x))
(define (make-reg-list regs) regs)
(define (reg-list? x) (or (pair? x) (null? x)))
(define (reg-list-regs x) x)
(define first-dtemp 0)
(define gvm-reg1 1)
(define poll-timer-reg (make-dreg 5))
(define null-reg (make-dreg 6))
(define placeholder-reg (make-dreg 6))
(define false-reg (make-dreg 7))
(define pair-reg (make-dreg 7))
(define gvm-reg0 0)
(define first-atemp 1)
(define heap-reg (make-areg 3))
(define ltq-tail-reg (make-areg 4))
(define pstate-reg (make-areg 5))
(define table-reg (make-areg 6))
(define sp-reg (make-areg 7))
(define pdec-sp (make-pdec sp-reg))
(define pinc-sp (make-pinc sp-reg))
(define dtemp1 (make-dreg first-dtemp))
(define atemp1 (make-areg first-atemp))
(define atemp2 (make-areg (+ first-atemp 1)))
(define ftemp1 (make-freg 0))
(define arg-count-reg dtemp1)
(define (trap-offset n) (+ 32768 (* (- n 32) 8)))
(define (emit-move.l opnd1 opnd2)
  (let ((src (opnd->mode/reg opnd1)) (dst (opnd->reg/mode opnd2)))
    (asm-word (+ 8192 (+ dst src)))
    (opnd-ext-rd-long opnd1)
    (opnd-ext-wr-long opnd2)
    (if ofile-asm?
        (emit-asm "movl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2)))))
(define (emit-move.w opnd1 opnd2)
  (let ((src (opnd->mode/reg opnd1)) (dst (opnd->reg/mode opnd2)))
    (asm-word (+ 12288 (+ dst src)))
    (opnd-ext-rd-word opnd1)
    (opnd-ext-wr-word opnd2)
    (if ofile-asm?
        (emit-asm "movw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2)))))
(define (emit-move.b opnd1 opnd2)
  (let ((src (opnd->mode/reg opnd1)) (dst (opnd->reg/mode opnd2)))
    (asm-word (+ 4096 (+ dst src)))
    (opnd-ext-rd-word opnd1)
    (opnd-ext-wr-word opnd2)
    (if ofile-asm?
        (emit-asm "movb" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2)))))
(define (emit-moveq n opnd)
  (asm-word (+ 28672 (+ (* (dreg-num opnd) 512) (modulo n 256))))
  (if ofile-asm? (emit-asm "moveq" ofile-tab "#" n "," (opnd-str opnd))))
(define (emit-movem.l opnd1 opnd2)
  (define (reg-mask reg-list flip-bits?)
    (let loop ((i 15) (bit 32768) (mask 0))
      (if (>= i 0)
          (loop (- i 1)
                (quotient bit 2)
                (if (memq i reg-list)
                    (+ mask (if flip-bits? (quotient 32768 bit) bit))
                    mask))
          mask)))
  (define (movem op reg-list opnd)
    (asm-word (+ op (opnd->mode/reg opnd)))
    (asm-word (reg-mask reg-list (pdec? opnd))))
  (if (reg-list? opnd1)
      (begin (movem 18624 opnd1 opnd2) (opnd-ext-wr-long opnd2))
      (begin (movem 19648 opnd2 opnd1) (opnd-ext-rd-long opnd1)))
  (if ofile-asm?
      (emit-asm "moveml" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-exg opnd1 opnd2)
  (define (exg r1 r2)
    (let ((mode (if (dreg? r2) 49472 (if (dreg? r1) 49544 49480)))
          (num1 (if (dreg? r1) (dreg-num r1) (areg-num r1)))
          (num2 (if (dreg? r2) (dreg-num r2) (areg-num r2))))
      (asm-word (+ mode (+ (* num1 512) num2)))))
  (if (dreg? opnd2) (exg opnd2 opnd1) (exg opnd1 opnd2))
  (if ofile-asm?
      (emit-asm "exg" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-eor.l opnd1 opnd2)
  (cond ((imm? opnd1)
         (asm-word (+ 2688 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (asm-word
          (+ 45440 (+ (* (dreg-num opnd1) 512) (opnd->mode/reg opnd2))))
         (opnd-ext-wr-long opnd2)))
  (if ofile-asm?
      (emit-asm "eorl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-and.l opnd1 opnd2)
  (cond ((imm? opnd1)
         (asm-word (+ 640 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 49280 49536))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (+ mode (+ (* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "andl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-and.w opnd1 opnd2)
  (cond ((imm? opnd1)
         (asm-word (+ 576 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-wr-word opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 49216 49472))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (+ mode (+ (* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-word other)
               (opnd-ext-wr-word other)))))
  (if ofile-asm?
      (emit-asm "andw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-or.l opnd1 opnd2)
  (cond ((imm? opnd1)
         (asm-word (+ 128 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 32896 33152))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (+ mode (+ (* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "orl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-addq.l n opnd)
  (let ((m (if (= n 8) 0 n)))
    (asm-word (+ 20608 (* m 512) (opnd->mode/reg opnd)))
    (opnd-ext-wr-long opnd)
    (if ofile-asm? (emit-asm "addql" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-addq.w n opnd)
  (let ((m (if (= n 8) 0 n)))
    (asm-word (+ 20544 (* m 512) (opnd->mode/reg opnd)))
    (opnd-ext-wr-word opnd)
    (if ofile-asm? (emit-asm "addqw" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-add.l opnd1 opnd2)
  (cond ((areg? opnd2)
         (asm-word
          (+ 53696 (+ (* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1))
        ((imm? opnd1)
         (asm-word (+ 1664 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 53376 53632))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (+ mode (+ (* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "addl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-add.w opnd1 opnd2)
  (cond ((areg? opnd2)
         (asm-word
          (+ 53440 (+ (* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1))
        ((imm? opnd1)
         (asm-word (+ 1600 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-wr-word opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 53312 53568))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (+ mode (+ (* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-word other)
               (opnd-ext-wr-word other)))))
  (if ofile-asm?
      (emit-asm "addw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-addx.w opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 53568 (+ (* (dreg-num opnd2) 512) (dreg-num opnd1))))
      (asm-word
       (+ 53576
          (+ (* (areg-num (pdec-areg opnd2)) 512)
             (areg-num (pdec-areg opnd1))))))
  (if ofile-asm?
      (emit-asm "addxw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-subq.l n opnd)
  (let ((m (if (= n 8) 0 n)))
    (asm-word (+ 20864 (* m 512) (opnd->mode/reg opnd)))
    (opnd-ext-wr-long opnd)
    (if ofile-asm? (emit-asm "subql" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-subq.w n opnd)
  (let ((m (if (= n 8) 0 n)))
    (asm-word (+ 20800 (* m 512) (opnd->mode/reg opnd)))
    (opnd-ext-wr-word opnd)
    (if ofile-asm? (emit-asm "subqw" ofile-tab "#" n "," (opnd-str opnd)))))
(define (emit-sub.l opnd1 opnd2)
  (cond ((areg? opnd2)
         (asm-word
          (+ 37312 (+ (* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1))
        ((imm? opnd1)
         (asm-word (+ 1152 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-wr-long opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 36992 37248))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (+ mode (+ (* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-long other)
               (opnd-ext-wr-long other)))))
  (if ofile-asm?
      (emit-asm "subl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-sub.w opnd1 opnd2)
  (cond ((areg? opnd2)
         (asm-word
          (+ 37056 (+ (* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1))
        ((imm? opnd1)
         (asm-word (+ 1088 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-wr-word opnd2))
        (else
         (let ((mode (if (dreg? opnd2) 36928 37184))
               (reg (if (dreg? opnd2) (dreg-num opnd2) (dreg-num opnd1)))
               (other (if (dreg? opnd2) opnd1 opnd2)))
           (asm-word (+ mode (+ (* reg 512) (opnd->mode/reg other))))
           (if (dreg? opnd2)
               (opnd-ext-rd-word other)
               (opnd-ext-wr-word other)))))
  (if ofile-asm?
      (emit-asm "subw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asl.l opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 57760 (+ (* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (+ 57728 (+ (* (if (= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "asll" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asl.w opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 57696 (+ (* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (+ 57664 (+ (* (if (= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "aslw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asr.l opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 57504 (+ (* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (+ 57472 (+ (* (if (= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "asrl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-asr.w opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 57440 (+ (* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (+ 57408 (+ (* (if (= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "asrw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-lsl.l opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 57768 (+ (* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (+ 57736 (+ (* (if (= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "lsll" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-lsr.l opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 57512 (+ (* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (+ 57480 (+ (* (if (= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "lsrl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-lsr.w opnd1 opnd2)
  (if (dreg? opnd1)
      (asm-word (+ 57448 (+ (* (dreg-num opnd1) 512) (dreg-num opnd2))))
      (let ((n (imm-val opnd1)))
        (asm-word (+ 57416 (+ (* (if (= n 8) 0 n) 512) (dreg-num opnd2))))))
  (if ofile-asm?
      (emit-asm "lsrw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-clr.l opnd)
  (asm-word (+ 17024 (opnd->mode/reg opnd)))
  (opnd-ext-wr-long opnd)
  (if ofile-asm? (emit-asm "clrl" ofile-tab (opnd-str opnd))))
(define (emit-neg.l opnd)
  (asm-word (+ 17536 (opnd->mode/reg opnd)))
  (opnd-ext-wr-long opnd)
  (if ofile-asm? (emit-asm "negl" ofile-tab (opnd-str opnd))))
(define (emit-not.l opnd)
  (asm-word (+ 18048 (opnd->mode/reg opnd)))
  (opnd-ext-wr-long opnd)
  (if ofile-asm? (emit-asm "notl" ofile-tab (opnd-str opnd))))
(define (emit-ext.l opnd)
  (asm-word (+ 18624 (dreg-num opnd)))
  (if ofile-asm? (emit-asm "extl" ofile-tab (opnd-str opnd))))
(define (emit-ext.w opnd)
  (asm-word (+ 18560 (dreg-num opnd)))
  (if ofile-asm? (emit-asm "extw" ofile-tab (opnd-str opnd))))
(define (emit-swap opnd)
  (asm-word (+ 18496 (dreg-num opnd)))
  (if ofile-asm? (emit-asm "swap" ofile-tab (opnd-str opnd))))
(define (emit-cmp.l opnd1 opnd2)
  (cond ((areg? opnd2)
         (asm-word
          (+ 45504 (+ (* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1))
        ((imm? opnd1)
         (asm-word (+ 3200 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-long opnd1)
         (opnd-ext-rd-long opnd2))
        (else
         (asm-word
          (+ 45184 (+ (* (dreg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-long opnd1)))
  (if ofile-asm?
      (emit-asm "cmpl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-cmp.w opnd1 opnd2)
  (cond ((areg? opnd2)
         (asm-word
          (+ 45248 (+ (* (areg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1))
        ((imm? opnd1)
         (asm-word (+ 3136 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-rd-word opnd2))
        (else
         (asm-word
          (+ 45120 (+ (* (dreg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1)))
  (if ofile-asm?
      (emit-asm "cmpw" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-cmp.b opnd1 opnd2)
  (cond ((imm? opnd1)
         (asm-word (+ 3072 (opnd->mode/reg opnd2)))
         (opnd-ext-rd-word opnd1)
         (opnd-ext-rd-word opnd2))
        (else
         (asm-word
          (+ 45056 (+ (* (dreg-num opnd2) 512) (opnd->mode/reg opnd1))))
         (opnd-ext-rd-word opnd1)))
  (if ofile-asm?
      (emit-asm "cmpb" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-tst.l opnd)
  (asm-word (+ 19072 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "tstl" ofile-tab (opnd-str opnd))))
(define (emit-tst.w opnd)
  (asm-word (+ 19008 (opnd->mode/reg opnd)))
  (opnd-ext-rd-word opnd)
  (if ofile-asm? (emit-asm "tstw" ofile-tab (opnd-str opnd))))
(define (emit-lea opnd areg)
  (asm-word (+ 16832 (+ (* (areg-num areg) 512) (opnd->mode/reg opnd))))
  (opnd-ext-rd-long opnd)
  (if ofile-asm?
      (emit-asm "lea" ofile-tab (opnd-str opnd) "," (opnd-str areg))))
(define (emit-unlk areg)
  (asm-word (+ 20056 (areg-num areg)))
  (if ofile-asm? (emit-asm "unlk" ofile-tab (opnd-str areg))))
(define (emit-move-proc num opnd)
  (let ((dst (opnd->reg/mode opnd)))
    (asm-word (+ 8192 (+ dst 60)))
    (asm-proc-ref num 0)
    (opnd-ext-wr-long opnd)
    (if ofile-asm? (emit-asm "MOVE_PROC(" num "," (opnd-str opnd) ")"))))
(define (emit-move-prim val opnd)
  (let ((dst (opnd->reg/mode opnd)))
    (asm-word (+ 8192 (+ dst 60)))
    (asm-prim-ref val 0)
    (opnd-ext-wr-long opnd)
    (if ofile-asm?
        (emit-asm "MOVE_PRIM(" (proc-obj-name val) "," (opnd-str opnd) ")"))))
(define (emit-pea opnd)
  (asm-word (+ 18496 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "pea" ofile-tab (opnd-str opnd))))
(define (emit-pea* n)
  (asm-word 18552)
  (asm-word n)
  (if ofile-asm? (emit-asm "pea" ofile-tab n)))
(define (emit-btst opnd1 opnd2)
  (asm-word (+ 256 (+ (* (dreg-num opnd1) 512) (opnd->mode/reg opnd2))))
  (opnd-ext-rd-word opnd2)
  (if ofile-asm?
      (emit-asm "btst" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-bra lbl)
  (asm-brel 24576 lbl)
  (if ofile-asm? (emit-asm "bra" ofile-tab "L" lbl)))
(define (emit-bcc lbl)
  (asm-brel 25600 lbl)
  (if ofile-asm? (emit-asm "bcc" ofile-tab "L" lbl)))
(define (emit-bcs lbl)
  (asm-brel 25856 lbl)
  (if ofile-asm? (emit-asm "bcs" ofile-tab "L" lbl)))
(define (emit-bhi lbl)
  (asm-brel 25088 lbl)
  (if ofile-asm? (emit-asm "bhi" ofile-tab "L" lbl)))
(define (emit-bls lbl)
  (asm-brel 25344 lbl)
  (if ofile-asm? (emit-asm "bls" ofile-tab "L" lbl)))
(define (emit-bmi lbl)
  (asm-brel 27392 lbl)
  (if ofile-asm? (emit-asm "bmi" ofile-tab "L" lbl)))
(define (emit-bpl lbl)
  (asm-brel 27136 lbl)
  (if ofile-asm? (emit-asm "bpl" ofile-tab "L" lbl)))
(define (emit-beq lbl)
  (asm-brel 26368 lbl)
  (if ofile-asm? (emit-asm "beq" ofile-tab "L" lbl)))
(define (emit-bne lbl)
  (asm-brel 26112 lbl)
  (if ofile-asm? (emit-asm "bne" ofile-tab "L" lbl)))
(define (emit-blt lbl)
  (asm-brel 27904 lbl)
  (if ofile-asm? (emit-asm "blt" ofile-tab "L" lbl)))
(define (emit-bgt lbl)
  (asm-brel 28160 lbl)
  (if ofile-asm? (emit-asm "bgt" ofile-tab "L" lbl)))
(define (emit-ble lbl)
  (asm-brel 28416 lbl)
  (if ofile-asm? (emit-asm "ble" ofile-tab "L" lbl)))
(define (emit-bge lbl)
  (asm-brel 27648 lbl)
  (if ofile-asm? (emit-asm "bge" ofile-tab "L" lbl)))
(define (emit-dbra dreg lbl)
  (asm-word (+ 20936 dreg))
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "dbra" ofile-tab (opnd-str dreg) ",L" lbl)))
(define (emit-trap num)
  (asm-word (+ 20032 num))
  (if ofile-asm? (emit-asm "trap" ofile-tab "#" num)))
(define (emit-trap1 num args)
  (asm-word (+ 20136 (areg-num table-reg)))
  (asm-word (trap-offset num))
  (let loop ((args args))
    (if (not (null? args)) (begin (asm-word (car args)) (loop (cdr args)))))
  (if ofile-asm?
      (let ()
        (define (words l)
          (if (null? l) (list ")") (cons "," (cons (car l) (words (cdr l))))))
        (apply emit-asm (cons "TRAP1(" (cons num (words args)))))))
(define (emit-trap2 num args)
  (asm-word (+ 20136 (areg-num table-reg)))
  (asm-word (trap-offset num))
  (asm-align 8 (modulo (- 4 (* (length args) 2)) 8))
  (let loop ((args args))
    (if (not (null? args)) (begin (asm-word (car args)) (loop (cdr args)))))
  (if ofile-asm?
      (let ()
        (define (words l)
          (if (null? l) (list ")") (cons "," (cons (car l) (words (cdr l))))))
        (apply emit-asm (cons "TRAP2(" (cons num (words args)))))))
(define (emit-trap3 num)
  (asm-word (+ 20200 (areg-num table-reg)))
  (asm-word (trap-offset num))
  (if ofile-asm? (emit-asm "TRAP3(" num ")")))
(define (emit-rts) (asm-word 20085) (if ofile-asm? (emit-asm "rts")))
(define (emit-nop) (asm-word 20081) (if ofile-asm? (emit-asm "nop")))
(define (emit-jmp opnd)
  (asm-word (+ 20160 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "jmp" ofile-tab (opnd-str opnd))))
(define (emit-jmp-glob glob)
  (asm-word 8814)
  (asm-ref-glob-jump glob)
  (asm-word 20177)
  (if ofile-asm? (emit-asm "JMP_GLOB(" (glob-name glob) ")")))
(define (emit-jmp-proc num offset)
  (asm-word 20217)
  (asm-proc-ref num offset)
  (if ofile-asm? (emit-asm "JMP_PROC(" num "," offset ")")))
(define (emit-jmp-prim val offset)
  (asm-word 20217)
  (asm-prim-ref val offset)
  (if ofile-asm? (emit-asm "JMP_PRIM(" (proc-obj-name val) "," offset ")")))
(define (emit-jsr opnd)
  (asm-word (+ 20096 (opnd->mode/reg opnd)))
  (opnd-ext-rd-long opnd)
  (if ofile-asm? (emit-asm "jsr" ofile-tab (opnd-str opnd))))
(define (emit-word n)
  (asm-word n)
  (if ofile-asm? (emit-asm ".word" ofile-tab n)))
(define (emit-label lbl)
  (asm-label lbl #f)
  (if ofile-asm? (emit-asm* "L" lbl ":")))
(define (emit-label-subproc lbl parent-lbl label-descr)
  (asm-align 8 0)
  (asm-wrel parent-lbl (- 32768 type-procedure))
  (asm-label lbl label-descr)
  (if ofile-asm?
      (begin (emit-asm "SUBPROC(L" parent-lbl ")") (emit-asm* "L" lbl ":"))))
(define (emit-label-return lbl parent-lbl fs link label-descr)
  (asm-align 8 4)
  (asm-word (* fs 4))
  (asm-word (* (- fs link) 4))
  (asm-wrel parent-lbl (- 32768 type-procedure))
  (asm-label lbl label-descr)
  (if ofile-asm?
      (begin
        (emit-asm "RETURN(L" parent-lbl "," fs "," link ")")
        (emit-asm* "L" lbl ":"))))
(define (emit-label-task-return lbl parent-lbl fs link label-descr)
  (asm-align 8 4)
  (asm-word (+ 32768 (* fs 4)))
  (asm-word (* (- fs link) 4))
  (asm-wrel parent-lbl (- 32768 type-procedure))
  (asm-label lbl label-descr)
  (if ofile-asm?
      (begin
        (emit-asm "TASK_RETURN(L" parent-lbl "," fs "," link ")")
        (emit-asm* "L" lbl ":"))))
(define (emit-lbl-ptr lbl)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "LBL_PTR(L" lbl ")")))
(define (emit-set-glob glob)
  (asm-set-glob glob)
  (if ofile-asm? (emit-asm "SET_GLOB(" (glob-name glob) ")")))
(define (emit-const obj)
  (let ((n (pos-in-list obj (queue->list asm-const-queue))))
    (if n
        (make-pcr const-lbl (* n 4))
        (let ((m (length (queue->list asm-const-queue))))
          (queue-put! asm-const-queue obj)
          (make-pcr const-lbl (* m 4))))))
(define (emit-stat stat)
  (asm-word 21177)
  (asm-stat stat)
  (if ofile-asm? (emit-asm "STAT(" stat ")")))
(define (emit-asm . l) (asm-comment (cons ofile-tab l)))
(define (emit-asm* . l) (asm-comment l))
(define (emit-muls.l opnd1 opnd2)
  (asm-m68020-proc)
  (asm-word (+ 19456 (opnd->mode/reg opnd1)))
  (asm-word (+ 2048 (* (dreg-num opnd2) 4096)))
  (opnd-ext-rd-long opnd1)
  (if ofile-asm?
      (emit-asm "mulsl" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-divsl.l opnd1 opnd2 opnd3)
  (asm-m68020-proc)
  (asm-word (+ 19520 (opnd->mode/reg opnd1)))
  (asm-word (+ 2048 (* (dreg-num opnd3) 4096) (dreg-num opnd2)))
  (opnd-ext-rd-long opnd1)
  (if ofile-asm?
      (emit-asm
       "divsll"
       ofile-tab
       (opnd-str opnd1)
       ","
       (opnd-str opnd2)
       ":"
       (opnd-str opnd3))))
(define (emit-fint.dx opnd1 opnd2) (emit-fop.dx "int" 1 opnd1 opnd2))
(define (emit-fsinh.dx opnd1 opnd2) (emit-fop.dx "sinh" 2 opnd1 opnd2))
(define (emit-fintrz.dx opnd1 opnd2) (emit-fop.dx "intrz" 3 opnd1 opnd2))
(define (emit-fsqrt.dx opnd1 opnd2) (emit-fop.dx "sqrt" 4 opnd1 opnd2))
(define (emit-flognp1.dx opnd1 opnd2) (emit-fop.dx "lognp1" 6 opnd1 opnd2))
(define (emit-fetoxm1.dx opnd1 opnd2) (emit-fop.dx "etoxm1" 8 opnd1 opnd2))
(define (emit-ftanh.dx opnd1 opnd2) (emit-fop.dx "tanh" 9 opnd1 opnd2))
(define (emit-fatan.dx opnd1 opnd2) (emit-fop.dx "atan" 10 opnd1 opnd2))
(define (emit-fasin.dx opnd1 opnd2) (emit-fop.dx "asin" 12 opnd1 opnd2))
(define (emit-fatanh.dx opnd1 opnd2) (emit-fop.dx "atanh" 13 opnd1 opnd2))
(define (emit-fsin.dx opnd1 opnd2) (emit-fop.dx "sin" 14 opnd1 opnd2))
(define (emit-ftan.dx opnd1 opnd2) (emit-fop.dx "tan" 15 opnd1 opnd2))
(define (emit-fetox.dx opnd1 opnd2) (emit-fop.dx "etox" 16 opnd1 opnd2))
(define (emit-ftwotox.dx opnd1 opnd2) (emit-fop.dx "twotox" 17 opnd1 opnd2))
(define (emit-ftentox.dx opnd1 opnd2) (emit-fop.dx "tentox" 18 opnd1 opnd2))
(define (emit-flogn.dx opnd1 opnd2) (emit-fop.dx "logn" 20 opnd1 opnd2))
(define (emit-flog10.dx opnd1 opnd2) (emit-fop.dx "log10" 21 opnd1 opnd2))
(define (emit-flog2.dx opnd1 opnd2) (emit-fop.dx "log2" 22 opnd1 opnd2))
(define (emit-fabs.dx opnd1 opnd2) (emit-fop.dx "abs" 24 opnd1 opnd2))
(define (emit-fcosh.dx opnd1 opnd2) (emit-fop.dx "cosh" 25 opnd1 opnd2))
(define (emit-fneg.dx opnd1 opnd2) (emit-fop.dx "neg" 26 opnd1 opnd2))
(define (emit-facos.dx opnd1 opnd2) (emit-fop.dx "acos" 28 opnd1 opnd2))
(define (emit-fcos.dx opnd1 opnd2) (emit-fop.dx "cos" 29 opnd1 opnd2))
(define (emit-fgetexp.dx opnd1 opnd2) (emit-fop.dx "getexp" 30 opnd1 opnd2))
(define (emit-fgetman.dx opnd1 opnd2) (emit-fop.dx "getman" 31 opnd1 opnd2))
(define (emit-fdiv.dx opnd1 opnd2) (emit-fop.dx "div" 32 opnd1 opnd2))
(define (emit-fmod.dx opnd1 opnd2) (emit-fop.dx "mod" 33 opnd1 opnd2))
(define (emit-fadd.dx opnd1 opnd2) (emit-fop.dx "add" 34 opnd1 opnd2))
(define (emit-fmul.dx opnd1 opnd2) (emit-fop.dx "mul" 35 opnd1 opnd2))
(define (emit-fsgldiv.dx opnd1 opnd2) (emit-fop.dx "sgldiv" 36 opnd1 opnd2))
(define (emit-frem.dx opnd1 opnd2) (emit-fop.dx "rem" 37 opnd1 opnd2))
(define (emit-fscale.dx opnd1 opnd2) (emit-fop.dx "scale" 38 opnd1 opnd2))
(define (emit-fsglmul.dx opnd1 opnd2) (emit-fop.dx "sglmul" 39 opnd1 opnd2))
(define (emit-fsub.dx opnd1 opnd2) (emit-fop.dx "sub" 40 opnd1 opnd2))
(define (emit-fcmp.dx opnd1 opnd2) (emit-fop.dx "cmp" 56 opnd1 opnd2))
(define (emit-fop.dx name code opnd1 opnd2)
  (asm-m68881-proc)
  (asm-word (+ 61952 (opnd->mode/reg opnd1)))
  (asm-word
   (+ (if (freg? opnd1) (* (freg-num opnd1) 1024) 21504)
      (* (freg-num opnd2) 128)
      code))
  (opnd-ext-rd-long opnd1)
  (if ofile-asm?
      (emit-asm
       "f"
       name
       (if (freg? opnd1) "x" "d")
       ofile-tab
       (opnd-str opnd1)
       ","
       (opnd-str opnd2))))
(define (emit-fmov.dx opnd1 opnd2)
  (emit-fmov
   (if (and (freg? opnd1) (freg? opnd2)) (* (freg-num opnd1) 1024) 21504)
   opnd1
   opnd2)
  (if ofile-asm?
      (emit-asm
       (if (and (freg? opnd1) (freg? opnd2)) "fmovex" "fmoved")
       ofile-tab
       (opnd-str opnd1)
       ","
       (opnd-str opnd2))))
(define (emit-fmov.l opnd1 opnd2)
  (emit-fmov 16384 opnd1 opnd2)
  (if ofile-asm?
      (emit-asm "fmovel" ofile-tab (opnd-str opnd1) "," (opnd-str opnd2))))
(define (emit-fmov code opnd1 opnd2)
  (define (fmov code opnd1 opnd2)
    (asm-m68881-proc)
    (asm-word (+ 61952 (opnd->mode/reg opnd1)))
    (asm-word (+ (* (freg-num opnd2) 128) code))
    (opnd-ext-rd-long opnd1))
  (if (freg? opnd2) (fmov code opnd1 opnd2) (fmov (+ code 8192) opnd2 opnd1)))
(define (emit-fbeq lbl)
  (asm-m68881-proc)
  (asm-word 62081)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbeq" ofile-tab "L" lbl)))
(define (emit-fbne lbl)
  (asm-m68881-proc)
  (asm-word 62094)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbne" ofile-tab "L" lbl)))
(define (emit-fblt lbl)
  (asm-m68881-proc)
  (asm-word 62100)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fblt" ofile-tab "L" lbl)))
(define (emit-fbgt lbl)
  (asm-m68881-proc)
  (asm-word 62098)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbgt" ofile-tab "L" lbl)))
(define (emit-fble lbl)
  (asm-m68881-proc)
  (asm-word 62101)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fble" ofile-tab "L" lbl)))
(define (emit-fbge lbl)
  (asm-m68881-proc)
  (asm-word 62099)
  (asm-wrel lbl 0)
  (if ofile-asm? (emit-asm "fbge" ofile-tab "L" lbl)))
(define (opnd->mode/reg opnd)
  (cond ((disp? opnd) (+ 32 (disp-areg opnd)))
        ((inx? opnd) (+ 40 (inx-areg opnd)))
        ((pcr? opnd) 58)
        ((imm? opnd) 60)
        ((glob? opnd) (+ 32 table-reg))
        ((freg? opnd) 0)
        (else opnd)))
(define (opnd->reg/mode opnd)
  (let ((x (opnd->mode/reg opnd)))
    (* (+ (* 8 (remainder x 8)) (quotient x 8)) 64)))
(define (opnd-ext-rd-long opnd) (opnd-extension opnd #f #f))
(define (opnd-ext-rd-word opnd) (opnd-extension opnd #f #t))
(define (opnd-ext-wr-long opnd) (opnd-extension opnd #t #f))
(define (opnd-ext-wr-word opnd) (opnd-extension opnd #t #t))
(define (opnd-extension opnd write? word?)
  (cond ((disp? opnd) (asm-word (disp-offset opnd)))
        ((inx? opnd)
         (asm-word
          (+ (+ (* (inx-ireg opnd) 4096) 2048)
             (modulo (inx-offset opnd) 256))))
        ((pcr? opnd) (asm-wrel (pcr-lbl opnd) (pcr-offset opnd)))
        ((imm? opnd)
         (if word? (asm-word (imm-val opnd)) (asm-long (imm-val opnd))))
        ((glob? opnd) (if write? (asm-set-glob opnd) (asm-ref-glob opnd)))))
(define (opnd-str opnd)
  (cond ((dreg? opnd)
         (vector-ref
          '#("d0" "d1" "d2" "d3" "d4" "d5" "d6" "d7")
          (dreg-num opnd)))
        ((areg? opnd)
         (vector-ref
          '#("a0" "a1" "a2" "a3" "a4" "a5" "a6" "sp")
          (areg-num opnd)))
        ((ind? opnd)
         (vector-ref
          '#("a0@" "a1@" "a2@" "a3@" "a4@" "a5@" "a6@" "sp@")
          (areg-num (ind-areg opnd))))
        ((pinc? opnd)
         (vector-ref
          '#("a0@+" "a1@+" "a2@+" "a3@+" "a4@+" "a5@+" "a6@+" "sp@+")
          (areg-num (pinc-areg opnd))))
        ((pdec? opnd)
         (vector-ref
          '#("a0@-" "a1@-" "a2@-" "a3@-" "a4@-" "a5@-" "a6@-" "sp@-")
          (areg-num (pdec-areg opnd))))
        ((disp? opnd)
         (string-append
          (opnd-str (disp-areg opnd))
          "@("
          (number->string (disp-offset opnd))
          ")"))
        ((inx? opnd)
         (string-append
          (opnd-str (inx-areg opnd))
          "@("
          (number->string (inx-offset opnd))
          ","
          (opnd-str (inx-ireg opnd))
          ":l)"))
        ((pcr? opnd)
         (let ((lbl (pcr-lbl opnd)) (offs (pcr-offset opnd)))
           (if (= offs 0)
               (string-append "L" (number->string lbl))
               (string-append
                "L"
                (number->string lbl)
                "+"
                (number->string offs)))))
        ((imm? opnd) (string-append "#" (number->string (imm-val opnd))))
        ((glob? opnd)
         (string-append "GLOB(" (symbol->string (glob-name opnd)) ")"))
        ((freg? opnd)
         (vector-ref
          '#("fp0" "fp1" "fp2" "fp3" "fp4" "fp5" "fp6" "fp7")
          (freg-num opnd)))
        ((reg-list? opnd)
         (let loop ((l (reg-list-regs opnd)) (result "[") (sep ""))
           (if (pair? l)
               (loop (cdr l) (string-append result sep (opnd-str (car l))) "/")
               (string-append result "]"))))
        (else (compiler-internal-error "opnd-str, unknown 'opnd'" opnd))))
(define (begin! info-port targ)
  (set! return-reg (make-reg 0))
  (target-end!-set! targ end!)
  (target-dump-set! targ dump)
  (target-nb-regs-set! targ nb-gvm-regs)
  (target-prim-info-set! targ prim-info)
  (target-label-info-set! targ label-info)
  (target-jump-info-set! targ jump-info)
  (target-proc-result-set! targ (make-reg 1))
  (target-task-return-set! targ return-reg)
  (set! *info-port* info-port)
  '())
(define (end!) '())
(define *info-port* '())
(define nb-gvm-regs 5)
(define nb-arg-regs 3)
(define pointer-size 4)
(define prim-proc-table
  (map (lambda (x)
         (cons (string->canonical-symbol (car x))
               (apply make-proc-obj (car x) #t #f (cdr x))))
       prim-procs))
(define (prim-info name)
  (let ((x (assq name prim-proc-table))) (if x (cdr x) #f)))
(define (get-prim-info name)
  (let ((proc (prim-info (string->canonical-symbol name))))
    (if proc
        proc
        (compiler-internal-error "get-prim-info, unknown primitive:" name))))
(define (label-info min-args nb-parms rest? closed?)
  (let ((nb-stacked (max 0 (- nb-parms nb-arg-regs))))
    (define (location-of-parms i)
      (if (> i nb-parms)
          '()
          (cons (cons i
                      (if (> i nb-stacked)
                          (make-reg (- i nb-stacked))
                          (make-stk i)))
                (location-of-parms (+ i 1)))))
    (let ((x (cons (cons 'return 0) (location-of-parms 1))))
      (make-pcontext
       nb-stacked
       (if closed?
           (cons (cons 'closure-env (make-reg (+ nb-arg-regs 1))) x)
           x)))))
(define (jump-info nb-args)
  (let ((nb-stacked (max 0 (- nb-args nb-arg-regs))))
    (define (location-of-args i)
      (if (> i nb-args)
          '()
          (cons (cons i
                      (if (> i nb-stacked)
                          (make-reg (- i nb-stacked))
                          (make-stk i)))
                (location-of-args (+ i 1)))))
    (make-pcontext
     nb-stacked
     (cons (cons 'return (make-reg 0)) (location-of-args 1)))))
(define (closed-var-offset i) (+ (* i pointer-size) 2))
(define (dump proc filename c-intf options)
  (if *info-port*
      (begin (display "Dumping:" *info-port*) (newline *info-port*)))
  (set! ofile-asm? (memq 'asm options))
  (set! ofile-stats? (memq 'stats options))
  (set! debug-info? (memq 'debug options))
  (set! object-queue (queue-empty))
  (set! objects-dumped (queue-empty))
  (ofile.begin! filename add-object)
  (queue-put! object-queue proc)
  (queue-put! objects-dumped proc)
  (let loop ((index 0))
    (if (not (queue-empty? object-queue))
        (let ((obj (queue-get! object-queue)))
          (dump-object obj index)
          (loop (+ index 1)))))
  (ofile.end!)
  (if *info-port* (newline *info-port*))
  (set! object-queue '())
  (set! objects-dumped '()))
(define debug-info? '())
(define object-queue '())
(define objects-dumped '())
(define (add-object obj)
  (if (and (proc-obj? obj) (not (proc-obj-code obj)))
      #f
      (let ((n (pos-in-list obj (queue->list objects-dumped))))
        (if n
            n
            (let ((m (length (queue->list objects-dumped))))
              (queue-put! objects-dumped obj)
              (queue-put! object-queue obj)
              m)))))
(define (dump-object obj index)
  (ofile-line "|------------------------------------------------------")
  (case (obj-type obj)
    ((pair) (dump-pair obj))
    ((flonum) (dump-flonum obj))
    ((subtyped)
     (case (obj-subtype obj)
       ((vector) (dump-vector obj))
       ((symbol) (dump-symbol obj))
;;       ((ratnum) (dump-ratnum obj))
;;       ((cpxnum) (dump-cpxnum obj))
       ((string) (dump-string obj))
       ((bignum) (dump-bignum obj))
       (else
        (compiler-internal-error
         "dump-object, can't dump object 'obj':"
         obj))))
    ((procedure) (dump-procedure obj))
    (else
     (compiler-internal-error "dump-object, can't dump object 'obj':" obj))))
(define (dump-pair pair)
  (ofile-long pair-prefix)
  (ofile-ref (cdr pair))
  (ofile-ref (car pair)))
(define (dump-vector v)
  (ofile-long (+ (* (vector-length v) 1024) (* subtype-vector 8)))
  (let ((len (vector-length v)))
    (let loop ((i 0))
      (if (< i len) (begin (ofile-ref (vector-ref v i)) (loop (+ i 1)))))))
(define (dump-symbol sym)
  (compiler-internal-error "dump-symbol, can't dump SYMBOL type"))
;;(define (dump-ratnum x)
;;  (ofile-long (+ (* 2 1024) (* subtype-ratnum 8)))
;;  (ofile-ref (numerator x))
;;  (ofile-ref (denominator x)))
;;(define (dump-cpxnum x)
;;  (ofile-long (+ (* 2 1024) (* subtype-cpxnum 8)))
;;  (ofile-ref (real-part x))
;;  (ofile-ref (imag-part x)))
(define (dump-string s)
  (ofile-long (+ (* (+ (string-length s) 1) 256) (* subtype-string 8)))
  (let ((len (string-length s)))
    (define (ref i) (if (>= i len) 0 (character-encoding (string-ref s i))))
    (let loop ((i 0))
      (if (<= i len)
          (begin
            (ofile-word (+ (* (ref i) 256) (ref (+ i 1))))
            (loop (+ i 2)))))))
(define (dump-flonum x)
  (let ((bits (flonum->bits x)))
    (ofile-long flonum-prefix)
    (ofile-long (quotient bits 4294967296))
    (ofile-long (modulo bits 4294967296))))
(define (flonum->inexact-exponential-format x)
  (define (exp-form-pos x y i)
    (let ((i*2 (+ i i)))
      (let ((z (if (and (not (< flonum-e-bias i*2)) (not (< x y)))
                   (exp-form-pos x (* y y) i*2)
                   (cons x 0))))
        (let ((a (car z)) (b (cdr z)))
          (let ((i+b (+ i b)))
            (if (and (not (< flonum-e-bias i+b)) (not (< a y)))
                (begin (set-car! z (/ a y)) (set-cdr! z i+b)))
            z)))))
  (define (exp-form-neg x y i)
    (let ((i*2 (+ i i)))
      (let ((z (if (and (< i*2 flonum-e-bias-minus-1) (< x y))
                   (exp-form-neg x (* y y) i*2)
                   (cons x 0))))
        (let ((a (car z)) (b (cdr z)))
          (let ((i+b (+ i b)))
            (if (and (< i+b flonum-e-bias-minus-1) (< a y))
                (begin (set-car! z (/ a y)) (set-cdr! z i+b)))
            z)))))
  (define (exp-form x)
    (if (< x inexact-+1)
        (let ((z (exp-form-neg x inexact-+1/2 1)))
          (set-car! z (* inexact-+2 (car z)))
          (set-cdr! z (- -1 (cdr z)))
          z)
        (exp-form-pos x inexact-+2 1)))
  (if (negative? x)
      (let ((z (exp-form (- inexact-0 x))))
        (set-car! z (- inexact-0 (car z)))
        z)
      (exp-form x)))
(define (flonum->exact-exponential-format x)
  (let ((z (flonum->inexact-exponential-format x)))
    (let ((y (car z)))
      (cond ((not (< y inexact-+2))
             (set-car! z flonum-+m-min)
             (set-cdr! z flonum-e-bias-plus-1))
            ((not (< inexact--2 y))
             (set-car! z flonum--m-min)
             (set-cdr! z flonum-e-bias-plus-1))
            (else
             (set-car!
              z
              (truncate (inexact->exact (* (car z) inexact-m-min))))))
      (set-cdr! z (- (cdr z) flonum-m-bits))
      z)))
(define (flonum->bits x)
  (define (bits a b)
    (if (< a flonum-+m-min)
        a
        (+ (- a flonum-+m-min)
           (* (+ (+ b flonum-m-bits) flonum-e-bias) flonum-+m-min))))
  (let ((z (flonum->exact-exponential-format x)))
    (let ((a (car z)) (b (cdr z)))
      (if (negative? a) (+ flonum-sign-bit (bits (- 0 a) b)) (bits a b)))))
(define flonum-m-bits 52)
(define flonum-e-bits 11)
(define flonum-sign-bit 9223372036854775808)
(define flonum-+m-min 4503599627370496)
(define flonum--m-min -4503599627370496)
(define flonum-e-bias 1023)
(define flonum-e-bias-plus-1 1024)
(define flonum-e-bias-minus-1 1022)
(define inexact-m-min (exact->inexact flonum-+m-min))
(define inexact-+2 (exact->inexact 2))
(define inexact--2 (exact->inexact -2))
(define inexact-+1 (exact->inexact 1))
(define inexact-+1/2 (/ (exact->inexact 1) (exact->inexact 2)))
(define inexact-0 (exact->inexact 0))
(define (dump-bignum x)
  (define radix 16384)
  (define (integer->digits n)
    (if (= n 0)
        '()
        (cons (remainder n radix) (integer->digits (quotient n radix)))))
  (let ((l (integer->digits (abs x))))
    (ofile-long (+ (* (+ (length l) 1) 512) (* subtype-bignum 8)))
    (if (< x 0) (ofile-word 0) (ofile-word 1))
    (for-each ofile-word l)))
(define (dump-procedure proc)
  (let ((bbs (proc-obj-code proc)))
    (set! entry-lbl-num (bbs-entry-lbl-num bbs))
    (set! label-counter (bbs-lbl-counter bbs))
    (set! var-descr-queue (queue-empty))
    (set! first-class-label-queue (queue-empty))
    (set! deferred-code-queue (queue-empty))
    (if *info-port*
        (begin
          (display "  #[" *info-port*)
          (if (proc-obj-primitive? proc)
              (display "primitive " *info-port*)
              (display "procedure " *info-port*))
          (display (proc-obj-name proc) *info-port*)
          (display "]" *info-port*)))
    (if (proc-obj-primitive? proc)
        (ofile-prim-proc (proc-obj-name proc))
        (ofile-user-proc))
    (asm.begin!)
    (let loop ((prev-bb #f) (prev-gvm-instr #f) (l (bbs->code-list bbs)))
      (if (not (null? l))
          (let ((pres-bb (code-bb (car l)))
                (pres-gvm-instr (code-gvm-instr (car l)))
                (pres-slots-needed (code-slots-needed (car l)))
                (next-gvm-instr
                 (if (null? (cdr l)) #f (code-gvm-instr (cadr l)))))
            (if ofile-asm? (asm-comment (car l)))
            (gen-gvm-instr
             prev-gvm-instr
             pres-gvm-instr
             next-gvm-instr
             pres-slots-needed)
            (loop pres-bb pres-gvm-instr (cdr l)))))
    (asm.end!
     (if debug-info?
         (vector (lst->vector (queue->list first-class-label-queue))
                 (lst->vector (queue->list var-descr-queue)))
         #f))
    (if *info-port* (newline *info-port*))
    (set! var-descr-queue '())
    (set! first-class-label-queue '())
    (set! deferred-code-queue '())
    (set! instr-source '())
    (set! entry-frame '())
    (set! exit-frame '())))
(define label-counter (lambda () 0))
(define entry-lbl-num '())
(define var-descr-queue '())
(define first-class-label-queue '())
(define deferred-code-queue '())
(define instr-source '())
(define entry-frame '())
(define exit-frame '())
(define (defer-code! thunk) (queue-put! deferred-code-queue thunk))
(define (gen-deferred-code!)
  (let loop ()
    (if (not (queue-empty? deferred-code-queue))
        (let ((thunk (queue-get! deferred-code-queue))) (thunk) (loop)))))
(define (add-var-descr! descr)
  (define (index x l)
    (let loop ((l l) (i 0))
      (cond ((not (pair? l)) #f)
            ((equal? (car l) x) i)
            (else (loop (cdr l) (+ i 1))))))
  (let ((n (index descr (queue->list var-descr-queue))))
    (if n
        n
        (let ((m (length (queue->list var-descr-queue))))
          (queue-put! var-descr-queue descr)
          m))))
(define (add-first-class-label! source slots frame)
  (let loop ((i 0) (l1 slots) (l2 '()))
    (if (pair? l1)
        (let ((var (car l1)))
          (let ((x (frame-live? var frame)))
            (if (and x (or (pair? x) (not (temp-var? x))))
                (let ((descr-index
                       (add-var-descr!
                        (if (pair? x)
                            (map (lambda (y) (add-var-descr! (var-name y))) x)
                            (var-name x)))))
                  (loop (+ i 1)
                        (cdr l1)
                        (cons (+ (* i 16384) descr-index) l2)))
                (loop (+ i 1) (cdr l1) l2))))
        (let ((label-descr (lst->vector (cons 0 (cons source l2)))))
          (queue-put! first-class-label-queue label-descr)
          label-descr))))
(define (gen-gvm-instr prev-gvm-instr gvm-instr next-gvm-instr sn)
  (set! instr-source (comment-get (gvm-instr-comment gvm-instr) 'source))
  (set! exit-frame (gvm-instr-frame gvm-instr))
  (set! entry-frame (and prev-gvm-instr (gvm-instr-frame prev-gvm-instr)))
  (case (gvm-instr-type gvm-instr)
    ((label)
     (set! entry-frame exit-frame)
     (set! current-fs (frame-size exit-frame))
     (case (label-type gvm-instr)
       ((simple) (gen-label-simple (label-lbl-num gvm-instr) sn))
       ((entry)
        (gen-label-entry
         (label-lbl-num gvm-instr)
         (label-entry-nb-parms gvm-instr)
         (label-entry-min gvm-instr)
         (label-entry-rest? gvm-instr)
         (label-entry-closed? gvm-instr)
         sn))
       ((return) (gen-label-return (label-lbl-num gvm-instr) sn))
       ((task-entry) (gen-label-task-entry (label-lbl-num gvm-instr) sn))
       ((task-return) (gen-label-task-return (label-lbl-num gvm-instr) sn))
       (else (compiler-internal-error "gen-gvm-instr, unknown label type"))))
    ((apply)
     (gen-apply
      (apply-prim gvm-instr)
      (apply-opnds gvm-instr)
      (apply-loc gvm-instr)
      sn))
    ((copy) (gen-copy (copy-opnd gvm-instr) (copy-loc gvm-instr) sn))
    ((close) (gen-close (close-parms gvm-instr) sn))
    ((ifjump)
     (gen-ifjump
      (ifjump-test gvm-instr)
      (ifjump-opnds gvm-instr)
      (ifjump-true gvm-instr)
      (ifjump-false gvm-instr)
      (ifjump-poll? gvm-instr)
      (if (and next-gvm-instr
               (memq (label-type next-gvm-instr) '(simple task-entry)))
          (label-lbl-num next-gvm-instr)
          #f)))
    ((jump)
     (gen-jump
      (jump-opnd gvm-instr)
      (jump-nb-args gvm-instr)
      (jump-poll? gvm-instr)
      (if (and next-gvm-instr
               (memq (label-type next-gvm-instr) '(simple task-entry)))
          (label-lbl-num next-gvm-instr)
          #f)))
    (else
     (compiler-internal-error
      "gen-gvm-instr, unknown 'gvm-instr':"
      gvm-instr))))
(define (reg-in-opnd68 opnd)
  (cond ((dreg? opnd) opnd)
        ((areg? opnd) opnd)
        ((ind? opnd) (ind-areg opnd))
        ((pinc? opnd) (pinc-areg opnd))
        ((pdec? opnd) (pdec-areg opnd))
        ((disp? opnd) (disp-areg opnd))
        ((inx? opnd) (inx-ireg opnd))
        (else #f)))
(define (temp-in-opnd68 opnd)
  (let ((reg (reg-in-opnd68 opnd)))
    (if reg
        (cond ((identical-opnd68? reg dtemp1) reg)
              ((identical-opnd68? reg atemp1) reg)
              ((identical-opnd68? reg atemp2) reg)
              (else #f))
        #f)))
(define (pick-atemp keep)
  (if (and keep (identical-opnd68? keep atemp1)) atemp2 atemp1))
(define return-reg '())
(define max-nb-args 1024)
(define heap-allocation-fudge (* pointer-size (+ (* 2 max-nb-args) 1024)))
(define intr-flag 0)
(define ltq-tail 1)
(define ltq-head 2)
(define heap-lim 12)
(define closure-lim 17)
(define closure-ptr 18)
(define intr-flag-slot (make-disp* pstate-reg (* pointer-size intr-flag)))
(define ltq-tail-slot (make-disp* pstate-reg (* pointer-size ltq-tail)))
(define ltq-head-slot (make-disp* pstate-reg (* pointer-size ltq-head)))
(define heap-lim-slot (make-disp* pstate-reg (* pointer-size heap-lim)))
(define closure-lim-slot (make-disp* pstate-reg (* pointer-size closure-lim)))
(define closure-ptr-slot (make-disp* pstate-reg (* pointer-size closure-ptr)))
(define touch-trap 1)
(define non-proc-jump-trap 6)
(define rest-params-trap 7)
(define rest-params-closed-trap 8)
(define wrong-nb-arg1-trap 9)
(define wrong-nb-arg1-closed-trap 10)
(define wrong-nb-arg2-trap 11)
(define wrong-nb-arg2-closed-trap 12)
(define heap-alloc1-trap 13)
(define heap-alloc2-trap 14)
(define closure-alloc-trap 15)
(define intr-trap 24)
(define cache-line-length 16)
(define polling-intermittency '())
(set! polling-intermittency 10)
(define (stat-clear!) (set! *stats* (cons 0 '())))
(define (stat-dump!) (emit-stat (cdr *stats*)))
(define (stat-add! bin count)
  (define (add! stats bin count)
    (set-car! stats (+ (car stats) count))
    (if (not (null? bin))
        (let ((x (assoc (car bin) (cdr stats))))
          (if x
              (add! (cdr x) (cdr bin) count)
              (begin
                (set-cdr! stats (cons (list (car bin) 0) (cdr stats)))
                (add! (cdadr stats) (cdr bin) count))))))
  (add! *stats* bin count))
(define (fetch-stat-add! gvm-opnd) (opnd-stat-add! 'fetch gvm-opnd))
(define (store-stat-add! gvm-opnd) (opnd-stat-add! 'store gvm-opnd))
(define (jump-stat-add! gvm-opnd) (opnd-stat-add! 'jump gvm-opnd))
(define (opnd-stat-add! type opnd)
  (cond ((reg? opnd) (stat-add! (list 'gvm-opnd 'reg type (reg-num opnd)) 1))
        ((stk? opnd) (stat-add! (list 'gvm-opnd 'stk type) 1))
        ((glo? opnd) (stat-add! (list 'gvm-opnd 'glo type (glo-name opnd)) 1))
        ((clo? opnd)
         (stat-add! (list 'gvm-opnd 'clo type) 1)
         (fetch-stat-add! (clo-base opnd)))
        ((lbl? opnd) (stat-add! (list 'gvm-opnd 'lbl type) 1))
        ((obj? opnd)
         (let ((val (obj-val opnd)))
           (if (number? val)
               (stat-add! (list 'gvm-opnd 'obj type val) 1)
               (stat-add! (list 'gvm-opnd 'obj type (obj-type val)) 1))))
        (else
         (compiler-internal-error "opnd-stat-add!, unknown 'opnd':" opnd))))
(define (opnd-stat opnd)
  (cond ((reg? opnd) 'reg)
        ((stk? opnd) 'stk)
        ((glo? opnd) 'glo)
        ((clo? opnd) 'clo)
        ((lbl? opnd) 'lbl)
        ((obj? opnd) 'obj)
        (else (compiler-internal-error "opnd-stat, unknown 'opnd':" opnd))))
(define *stats* '())
(define (move-opnd68-to-loc68 opnd loc)
  (if (not (identical-opnd68? opnd loc))
      (if (imm? opnd)
          (move-n-to-loc68 (imm-val opnd) loc)
          (emit-move.l opnd loc))))
(define (move-obj-to-loc68 obj loc)
  (let ((n (obj-encoding obj)))
    (if n (move-n-to-loc68 n loc) (emit-move.l (emit-const obj) loc))))
(define (move-n-to-loc68 n loc)
  (cond ((= n bits-null) (emit-move.l null-reg loc))
        ((= n bits-false) (emit-move.l false-reg loc))
        ((and (dreg? loc) (>= n -128) (<= n 127)) (emit-moveq n loc))
        ((and (areg? loc) (>= n -32768) (<= n 32767))
         (emit-move.w (make-imm n) loc))
        ((and (identical-opnd68? loc pdec-sp) (>= n -32768) (<= n 32767))
         (emit-pea* n))
        ((= n 0) (emit-clr.l loc))
        ((and (not (and (inx? loc) (= (inx-ireg loc) dtemp1)))
              (>= n -128)
              (<= n 127))
         (emit-moveq n dtemp1)
         (emit-move.l dtemp1 loc))
        (else (emit-move.l (make-imm n) loc))))
(define (add-n-to-loc68 n loc)
  (if (not (= n 0))
      (cond ((and (>= n -8) (<= n 8))
             (if (> n 0) (emit-addq.l n loc) (emit-subq.l (- n) loc)))
            ((and (areg? loc) (>= n -32768) (<= n 32767))
             (emit-lea (make-disp loc n) loc))
            ((and (not (identical-opnd68? loc dtemp1)) (>= n -128) (<= n 128))
             (emit-moveq (- (abs n)) dtemp1)
             (if (> n 0) (emit-sub.l dtemp1 loc) (emit-add.l dtemp1 loc)))
            (else (emit-add.l (make-imm n) loc)))))
(define (power-of-2 n)
  (let loop ((i 0) (k 1))
    (cond ((= k n) i) ((> k n) #f) (else (loop (+ i 1) (* k 2))))))
(define (mul-n-to-reg68 n reg)
  (if (= n 0)
      (emit-moveq 0 reg)
      (let ((abs-n (abs n)))
        (if (= abs-n 1)
            (if (< n 0) (emit-neg.l reg))
            (let ((shift (power-of-2 abs-n)))
              (if shift
                  (let ((m (min shift 32)))
                    (if (or (<= m 8) (identical-opnd68? reg dtemp1))
                        (let loop ((i m))
                          (if (> i 0)
                              (begin
                                (emit-asl.l (make-imm (min i 8)) reg)
                                (loop (- i 8)))))
                        (begin (emit-moveq m dtemp1) (emit-asl.l dtemp1 reg)))
                    (if (< n 0) (emit-neg.l reg)))
                  (emit-muls.l (make-imm n) reg)))))))
(define (div-n-to-reg68 n reg)
  (let ((abs-n (abs n)))
    (if (= abs-n 1)
        (if (< n 0) (emit-neg.l reg))
        (let ((shift (power-of-2 abs-n)))
          (if shift
              (let ((m (min shift 32)) (lbl (new-lbl!)))
                (emit-move.l reg reg)
                (emit-bpl lbl)
                (add-n-to-loc68 (* (- abs-n 1) 8) reg)
                (emit-label lbl)
                (if (or (<= m 8) (identical-opnd68? reg dtemp1))
                    (let loop ((i m))
                      (if (> i 0)
                          (begin
                            (emit-asr.l (make-imm (min i 8)) reg)
                            (loop (- i 8)))))
                    (begin (emit-moveq m dtemp1) (emit-asr.l dtemp1 reg)))
                (if (< n 0) (emit-neg.l reg)))
              (emit-divsl.l (make-imm n) reg reg))))))
(define (cmp-n-to-opnd68 n opnd)
  (cond ((= n bits-null) (emit-cmp.l opnd null-reg) #f)
        ((= n bits-false) (emit-cmp.l opnd false-reg) #f)
        ((or (pcr? opnd) (imm? opnd))
         (if (= n 0)
             (begin (emit-move.l opnd dtemp1) #t)
             (begin
               (move-opnd68-to-loc68 opnd atemp1)
               (if (and (>= n -32768) (<= n 32767))
                   (emit-cmp.w (make-imm n) atemp1)
                   (emit-cmp.l (make-imm n) atemp1))
               #t)))
        ((= n 0) (emit-move.l opnd dtemp1) #t)
        ((and (>= n -128) (<= n 127) (not (identical-opnd68? opnd dtemp1)))
         (emit-moveq n dtemp1)
         (emit-cmp.l opnd dtemp1)
         #f)
        (else (emit-cmp.l (make-imm n) opnd) #t)))
(define current-fs '())
(define (adjust-current-fs n) (set! current-fs (+ current-fs n)))
(define (new-lbl!) (label-counter))
(define (needed? loc sn) (and loc (if (stk? loc) (<= (stk-num loc) sn) #t)))
(define (sn-opnd opnd sn)
  (cond ((stk? opnd) (max (stk-num opnd) sn))
        ((clo? opnd) (sn-opnd (clo-base opnd) sn))
        (else sn)))
(define (sn-opnds opnds sn)
  (if (null? opnds) sn (sn-opnd (car opnds) (sn-opnds (cdr opnds) sn))))
(define (sn-opnd68 opnd sn)
  (cond ((and (disp*? opnd) (identical-opnd68? (disp*-areg opnd) sp-reg))
         (max (disp*-offset opnd) sn))
        ((identical-opnd68? opnd pdec-sp) (max (+ current-fs 1) sn))
        ((identical-opnd68? opnd pinc-sp) (max current-fs sn))
        (else sn)))
(define (resize-frame n)
  (let ((x (- n current-fs)))
    (adjust-current-fs x)
    (add-n-to-loc68 (* (- pointer-size) x) sp-reg)))
(define (shrink-frame n)
  (cond ((< n current-fs) (resize-frame n))
        ((> n current-fs)
         (compiler-internal-error "shrink-frame, can't increase frame size"))))
(define (make-top-of-frame n sn)
  (if (and (< n current-fs) (>= n sn)) (resize-frame n)))
(define (make-top-of-frame-if-stk-opnd68 opnd sn)
  (if (frame-base-rel? opnd)
      (make-top-of-frame (frame-base-rel-slot opnd) sn)))
(define (make-top-of-frame-if-stk-opnds68 opnd1 opnd2 sn)
  (if (frame-base-rel? opnd1)
      (let ((slot1 (frame-base-rel-slot opnd1)))
        (if (frame-base-rel? opnd2)
            (make-top-of-frame (max (frame-base-rel-slot opnd2) slot1) sn)
            (make-top-of-frame slot1 sn)))
      (if (frame-base-rel? opnd2)
          (make-top-of-frame (frame-base-rel-slot opnd2) sn))))
(define (opnd68->true-opnd68 opnd sn)
  (if (frame-base-rel? opnd)
      (let ((slot (frame-base-rel-slot opnd)))
        (cond ((> slot current-fs) (adjust-current-fs 1) pdec-sp)
              ((and (= slot current-fs) (< sn current-fs))
               (adjust-current-fs -1)
               pinc-sp)
              (else (make-disp* sp-reg (* pointer-size (- current-fs slot))))))
      opnd))
(define (move-opnd68-to-any-areg opnd keep sn)
  (if (areg? opnd)
      opnd
      (let ((areg (pick-atemp keep)))
        (make-top-of-frame-if-stk-opnd68 opnd sn)
        (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd sn) areg)
        areg)))
(define (clo->opnd68 opnd keep sn)
  (let ((base (clo-base opnd)) (offs (closed-var-offset (clo-index opnd))))
    (if (lbl? base) (make-pcr (lbl-num base) offs) (clo->loc68 opnd keep sn))))
(define (clo->loc68 opnd keep sn)
  (let ((base (clo-base opnd)) (offs (closed-var-offset (clo-index opnd))))
    (cond ((eq? base return-reg) (make-disp* (reg->reg68 base) offs))
          ((obj? base)
           (let ((areg (pick-atemp keep)))
             (move-obj-to-loc68 (obj-val base) areg)
             (make-disp* areg offs)))
          (else
           (let ((areg (pick-atemp keep)))
             (move-opnd-to-loc68 base areg sn)
             (make-disp* areg offs))))))
(define (reg->reg68 reg) (reg-num->reg68 (reg-num reg)))
(define (reg-num->reg68 num)
  (if (= num 0) (make-areg gvm-reg0) (make-dreg (+ (- num 1) gvm-reg1))))
(define (opnd->opnd68 opnd keep sn)
  (cond ((lbl? opnd)
         (let ((areg (pick-atemp keep)))
           (emit-lea (make-pcr (lbl-num opnd) 0) areg)
           areg))
        ((obj? opnd)
         (let ((val (obj-val opnd)))
           (if (proc-obj? val)
               (let ((num (add-object val)) (areg (pick-atemp keep)))
                 (if num (emit-move-proc num areg) (emit-move-prim val areg))
                 areg)
               (let ((n (obj-encoding val)))
                 (if n (make-imm n) (emit-const val))))))
        ((clo? opnd) (clo->opnd68 opnd keep sn))
        (else (loc->loc68 opnd keep sn))))
(define (loc->loc68 loc keep sn)
  (cond ((reg? loc) (reg->reg68 loc))
        ((stk? loc) (make-frame-base-rel (stk-num loc)))
        ((glo? loc) (make-glob (glo-name loc)))
        ((clo? loc) (clo->loc68 loc keep sn))
        (else (compiler-internal-error "loc->loc68, unknown 'loc':" loc))))
(define (move-opnd68-to-loc opnd loc sn)
  (cond ((reg? loc)
         (make-top-of-frame-if-stk-opnd68 opnd sn)
         (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd sn) (reg->reg68 loc)))
        ((stk? loc)
         (let* ((loc-slot (stk-num loc))
                (sn-after-opnd1 (if (< loc-slot sn) sn (- loc-slot 1))))
           (if (> current-fs loc-slot)
               (make-top-of-frame
                (if (frame-base-rel? opnd)
                    (let ((opnd-slot (frame-base-rel-slot opnd)))
                      (if (>= opnd-slot (- loc-slot 1)) opnd-slot loc-slot))
                    loc-slot)
                sn-after-opnd1))
           (let* ((opnd1 (opnd68->true-opnd68 opnd sn-after-opnd1))
                  (opnd2 (opnd68->true-opnd68
                          (make-frame-base-rel loc-slot)
                          sn)))
             (move-opnd68-to-loc68 opnd1 opnd2))))
        ((glo? loc)
         (make-top-of-frame-if-stk-opnd68 opnd sn)
         (move-opnd68-to-loc68
          (opnd68->true-opnd68 opnd sn)
          (make-glob (glo-name loc))))
        ((clo? loc)
         (let ((clo (clo->loc68
                     loc
                     (temp-in-opnd68 opnd)
                     (sn-opnd68 opnd sn))))
           (make-top-of-frame-if-stk-opnd68 opnd sn)
           (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd sn) clo)))
        (else
         (compiler-internal-error "move-opnd68-to-loc, unknown 'loc':" loc))))
(define (move-opnd-to-loc68 opnd loc68 sn)
  (if (and (lbl? opnd) (areg? loc68))
      (emit-lea (make-pcr (lbl-num opnd) 0) loc68)
      (let* ((sn-after-opnd68 (sn-opnd68 loc68 sn))
             (opnd68 (opnd->opnd68
                      opnd
                      (temp-in-opnd68 loc68)
                      sn-after-opnd68)))
        (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn)
        (let* ((opnd68* (opnd68->true-opnd68 opnd68 sn-after-opnd68))
               (loc68* (opnd68->true-opnd68 loc68 sn)))
          (move-opnd68-to-loc68 opnd68* loc68*)))))
(define (copy-opnd-to-loc opnd loc sn)
  (if (and (lbl? opnd) (eq? loc return-reg))
      (emit-lea (make-pcr (lbl-num opnd) 0) (reg->reg68 loc))
      (move-opnd68-to-loc (opnd->opnd68 opnd #f (sn-opnd loc sn)) loc sn)))
(define (touch-reg68-to-reg68 src dst)
  (define (trap-to-touch-handler dreg lbl)
    (if ofile-stats?
        (emit-stat
         '((touch 0
                  (determined-placeholder -1)
                  (undetermined-placeholder 1)))))
    (gen-trap
     instr-source
     entry-frame
     #t
     dreg
     (+ touch-trap (dreg-num dreg))
     lbl))
  (define (touch-dreg-to-reg src dst)
    (let ((lbl1 (new-lbl!)))
      (emit-btst src placeholder-reg)
      (emit-bne lbl1)
      (if ofile-stats?
          (emit-stat
           '((touch 0 (non-placeholder -1) (determined-placeholder 1)))))
      (trap-to-touch-handler src lbl1)
      (move-opnd68-to-loc68 src dst)))
  (define (touch-areg-to-dreg src dst)
    (let ((lbl1 (new-lbl!)))
      (emit-move.l src dst)
      (emit-btst dst placeholder-reg)
      (emit-bne lbl1)
      (if ofile-stats?
          (emit-stat
           '((touch 0 (non-placeholder -1) (determined-placeholder 1)))))
      (trap-to-touch-handler dst lbl1)))
  (if ofile-stats? (emit-stat '((touch 1 (non-placeholder 1)))))
  (cond ((dreg? src) (touch-dreg-to-reg src dst))
        ((dreg? dst) (touch-areg-to-dreg src dst))
        (else (emit-move.l src dtemp1) (touch-dreg-to-reg dtemp1 dst))))
(define (touch-opnd-to-any-reg68 opnd sn)
  (if (reg? opnd)
      (let ((reg (reg->reg68 opnd))) (touch-reg68-to-reg68 reg reg) reg)
      (let ((opnd68 (opnd->opnd68 opnd #f sn)))
        (make-top-of-frame-if-stk-opnd68 opnd68 sn)
        (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd68 sn) dtemp1)
        (touch-reg68-to-reg68 dtemp1 dtemp1)
        dtemp1)))
(define (touch-opnd-to-loc opnd loc sn)
  (if (reg? opnd)
      (let ((reg68 (reg->reg68 opnd)))
        (if (reg? loc)
            (touch-reg68-to-reg68 reg68 (reg->reg68 loc))
            (begin
              (touch-reg68-to-reg68 reg68 reg68)
              (move-opnd68-to-loc reg68 loc sn))))
      (if (reg? loc)
          (let ((reg68 (reg->reg68 loc)))
            (move-opnd-to-loc68 opnd reg68 sn)
            (touch-reg68-to-reg68 reg68 reg68))
          (let ((reg68 (touch-opnd-to-any-reg68 opnd sn)))
            (move-opnd68-to-loc reg68 loc sn)))))
(define (gen-trap source frame save-live? not-save-reg num lbl)
  (define (adjust-slots l n)
    (cond ((= n 0) (append l '()))
          ((< n 0) (adjust-slots (cdr l) (+ n 1)))
          (else (adjust-slots (cons empty-var l) (- n 1)))))
  (define (set-slot! slots i x)
    (let loop ((l slots) (n (- (length slots) i)))
      (if (> n 0) (loop (cdr l) (- n 1)) (set-car! l x))))
  (let ((ret-slot (frame-first-empty-slot frame)))
    (let loop1 ((save1 '()) (save2 #f) (regs (frame-regs frame)) (i 0))
      (if (pair? regs)
          (let ((var (car regs)))
            (if (eq? var ret-var)
                (let ((x (cons (reg->reg68 (make-reg i)) var)))
                  (if (> ret-slot current-fs)
                      (loop1 (cons x save1) save2 (cdr regs) (+ i 1))
                      (loop1 save1 x (cdr regs) (+ i 1))))
                (if (and save-live?
                         (frame-live? var frame)
                         (not (eqv? not-save-reg (reg->reg68 (make-reg i)))))
                    (loop1 (cons (cons (reg->reg68 (make-reg i)) var) save1)
                           save2
                           (cdr regs)
                           (+ i 1))
                    (loop1 save1 save2 (cdr regs) (+ i 1)))))
          (let ((order (sort-list save1 (lambda (x y) (< (car x) (car y))))))
            (let ((slots (append (map cdr order)
                                 (adjust-slots
                                  (frame-slots frame)
                                  (- current-fs (frame-size frame)))))
                  (reg-list (map car order))
                  (nb-regs (length order)))
              (define (trap)
                (emit-trap2 num '())
                (gen-label-return*
                 (new-lbl!)
                 (add-first-class-label! source slots frame)
                 slots
                 0))
              (if save2
                  (begin
                    (emit-move.l
                     (car save2)
                     (make-disp*
                      sp-reg
                      (* pointer-size (- current-fs ret-slot))))
                    (set-slot! slots ret-slot (cdr save2))))
              (if (> (length order) 2)
                  (begin
                    (emit-movem.l reg-list pdec-sp)
                    (trap)
                    (emit-movem.l pinc-sp reg-list))
                  (let loop2 ((l (reverse reg-list)))
                    (if (pair? l)
                        (let ((reg (car l)))
                          (emit-move.l reg pdec-sp)
                          (loop2 (cdr l))
                          (emit-move.l pinc-sp reg))
                        (trap))))
              (if save2
                  (emit-move.l
                   (make-disp* sp-reg (* pointer-size (- current-fs ret-slot)))
                   (car save2)))
              (emit-label lbl)))))))
(define (gen-label-simple lbl sn)
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label simple) 1)))
  (set! pointers-allocated 0)
  (emit-label lbl))
(define (gen-label-entry lbl nb-parms min rest? closed? sn)
  (if ofile-stats?
      (begin
        (stat-clear!)
        (stat-add!
         (list 'gvm-instr
               'label
               'entry
               nb-parms
               min
               (if rest? 'rest 'not-rest)
               (if closed? 'closed 'not-closed))
         1)))
  (set! pointers-allocated 0)
  (let ((label-descr (add-first-class-label! instr-source '() exit-frame)))
    (if (= lbl entry-lbl-num)
        (emit-label lbl)
        (emit-label-subproc lbl entry-lbl-num label-descr)))
  (let* ((nb-parms* (if rest? (- nb-parms 1) nb-parms))
         (dispatch-lbls (make-vector (+ (- nb-parms min) 1)))
         (optional-lbls (make-vector (+ (- nb-parms min) 1))))
    (let loop ((i min))
      (if (<= i nb-parms)
          (let ((lbl (new-lbl!)))
            (vector-set! optional-lbls (- nb-parms i) lbl)
            (vector-set!
             dispatch-lbls
             (- nb-parms i)
             (if (or (>= i nb-parms) (<= nb-parms nb-arg-regs))
                 lbl
                 (new-lbl!)))
            (loop (+ i 1)))))
    (if closed?
        (let ((closure-reg (reg-num->reg68 (+ nb-arg-regs 1))))
          (emit-move.l pinc-sp closure-reg)
          (emit-subq.l 6 closure-reg)
          (if (or (and (<= min 1) (<= 1 nb-parms*))
                  (and (<= min 2) (<= 2 nb-parms*)))
              (emit-move.w dtemp1 dtemp1))))
    (if (and (<= min 2) (<= 2 nb-parms*))
        (emit-beq (vector-ref dispatch-lbls (- nb-parms 2))))
    (if (and (<= min 1) (<= 1 nb-parms*))
        (emit-bmi (vector-ref dispatch-lbls (- nb-parms 1))))
    (let loop ((i min))
      (if (<= i nb-parms*)
          (begin
            (if (not (or (= i 1) (= i 2)))
                (begin
                  (emit-cmp.w (make-imm (encode-arg-count i)) arg-count-reg)
                  (emit-beq (vector-ref dispatch-lbls (- nb-parms i)))))
            (loop (+ i 1)))))
    (cond (rest?
           (emit-trap1
            (if closed? rest-params-closed-trap rest-params-trap)
            (list min nb-parms*))
           (if (not closed?) (emit-lbl-ptr lbl))
           (set! pointers-allocated 1)
           (gen-guarantee-fudge)
           (emit-bra (vector-ref optional-lbls 0)))
          ((= min nb-parms*)
           (emit-trap1
            (if closed? wrong-nb-arg1-closed-trap wrong-nb-arg1-trap)
            (list nb-parms*))
           (if (not closed?) (emit-lbl-ptr lbl)))
          (else
           (emit-trap1
            (if closed? wrong-nb-arg2-closed-trap wrong-nb-arg2-trap)
            (list min nb-parms*))
           (if (not closed?) (emit-lbl-ptr lbl))))
    (if (> nb-parms nb-arg-regs)
        (let loop1 ((i (- nb-parms 1)))
          (if (>= i min)
              (let ((nb-stacked (if (<= i nb-arg-regs) 0 (- i nb-arg-regs))))
                (emit-label (vector-ref dispatch-lbls (- nb-parms i)))
                (let loop2 ((j 1))
                  (if (and (<= j nb-arg-regs)
                           (<= j i)
                           (<= j (- (- nb-parms nb-arg-regs) nb-stacked)))
                      (begin
                        (emit-move.l (reg-num->reg68 j) pdec-sp)
                        (loop2 (+ j 1)))
                      (let loop3 ((k j))
                        (if (and (<= k nb-arg-regs) (<= k i))
                            (begin
                              (emit-move.l
                               (reg-num->reg68 k)
                               (reg-num->reg68 (+ (- k j) 1)))
                              (loop3 (+ k 1)))))))
                (if (> i min)
                    (emit-bra (vector-ref optional-lbls (- nb-parms i))))
                (loop1 (- i 1))))))
    (let loop ((i min))
      (if (<= i nb-parms)
          (let ((val (if (= i nb-parms*) bits-null bits-unass)))
            (emit-label (vector-ref optional-lbls (- nb-parms i)))
            (cond ((> (- nb-parms i) nb-arg-regs)
                   (move-n-to-loc68 val pdec-sp))
                  ((< i nb-parms)
                   (move-n-to-loc68
                    val
                    (reg-num->reg68 (parm->reg-num (+ i 1) nb-parms)))))
            (loop (+ i 1)))))))
(define (encode-arg-count n) (cond ((= n 1) -1) ((= n 2) 0) (else (+ n 1))))
(define (parm->reg-num i nb-parms)
  (if (<= nb-parms nb-arg-regs) i (+ i (- nb-arg-regs nb-parms))))
(define (no-arg-check-entry-offset proc nb-args)
  (let ((x (proc-obj-call-pat proc)))
    (if (and (pair? x) (null? (cdr x)))
        (let ((arg-count (car x)))
          (if (= arg-count nb-args)
              (if (or (= arg-count 1) (= arg-count 2)) 10 14)
              0))
        0)))
(define (gen-label-return lbl sn)
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label return) 1)))
  (set! pointers-allocated 0)
  (let ((slots (frame-slots exit-frame)))
    (gen-label-return*
     lbl
     (add-first-class-label! instr-source slots exit-frame)
     slots
     0)))
(define (gen-label-return* lbl label-descr slots extra)
  (let ((i (pos-in-list ret-var slots)))
    (if i
        (let* ((fs (length slots)) (link (- fs i)))
          (emit-label-return lbl entry-lbl-num (+ fs extra) link label-descr))
        (compiler-internal-error
         "gen-label-return*, no return address in frame"))))
(define (gen-label-task-entry lbl sn)
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label task-entry) 1)))
  (set! pointers-allocated 0)
  (emit-label lbl)
  (if (= current-fs 0)
      (begin
        (emit-move.l (reg->reg68 return-reg) pdec-sp)
        (emit-move.l sp-reg (make-pinc ltq-tail-reg)))
      (begin
        (emit-move.l sp-reg atemp1)
        (emit-move.l (make-pinc atemp1) pdec-sp)
        (let loop ((i (- current-fs 1)))
          (if (> i 0)
              (begin
                (emit-move.l (make-pinc atemp1) (make-disp atemp1 -8))
                (loop (- i 1)))))
        (emit-move.l (reg->reg68 return-reg) (make-pdec atemp1))
        (emit-move.l atemp1 (make-pinc ltq-tail-reg))))
  (emit-move.l ltq-tail-reg ltq-tail-slot))
(define (gen-label-task-return lbl sn)
  (if ofile-stats?
      (begin (stat-clear!) (stat-add! '(gvm-instr label task-return) 1)))
  (set! pointers-allocated 0)
  (let ((slots (frame-slots exit-frame)))
    (set! current-fs (+ current-fs 1))
    (let ((dummy-lbl (new-lbl!)) (skip-lbl (new-lbl!)))
      (gen-label-return*
       dummy-lbl
       (add-first-class-label! instr-source slots exit-frame)
       slots
       1)
      (emit-bra skip-lbl)
      (gen-label-task-return*
       lbl
       (add-first-class-label! instr-source slots exit-frame)
       slots
       1)
      (emit-subq.l pointer-size ltq-tail-reg)
      (emit-label skip-lbl))))
(define (gen-label-task-return* lbl label-descr slots extra)
  (let ((i (pos-in-list ret-var slots)))
    (if i
        (let* ((fs (length slots)) (link (- fs i)))
          (emit-label-task-return
           lbl
           entry-lbl-num
           (+ fs extra)
           link
           label-descr))
        (compiler-internal-error
         "gen-label-task-return*, no return address in frame"))))
(define (gen-apply prim opnds loc sn)
  (if ofile-stats?
      (begin
        (stat-add!
         (list 'gvm-instr
               'apply
               (string->canonical-symbol (proc-obj-name prim))
               (map opnd-stat opnds)
               (if loc (opnd-stat loc) #f))
         1)
        (for-each fetch-stat-add! opnds)
        (if loc (store-stat-add! loc))))
  (let ((x (proc-obj-inlinable prim)))
    (if (not x)
        (compiler-internal-error "gen-APPLY, unknown 'prim':" prim)
        (if (or (needed? loc sn) (car x)) ((cdr x) opnds loc sn)))))
(define (define-apply name side-effects? proc)
  (let ((prim (get-prim-info name)))
    (proc-obj-inlinable-set! prim (cons side-effects? proc))))
(define (gen-copy opnd loc sn)
  (if ofile-stats?
      (begin
        (stat-add! (list 'gvm-instr 'copy (opnd-stat opnd) (opnd-stat loc)) 1)
        (fetch-stat-add! opnd)
        (store-stat-add! loc)))
  (if (needed? loc sn) (copy-opnd-to-loc opnd loc sn)))
(define (gen-close parms sn)
  (define (size->bytes size)
    (* (quotient
        (+ (* (+ size 2) pointer-size) (- cache-line-length 1))
        cache-line-length)
       cache-line-length))
  (define (parms->bytes parms)
    (if (null? parms)
        0
        (+ (size->bytes (length (closure-parms-opnds (car parms))))
           (parms->bytes (cdr parms)))))
  (if ofile-stats?
      (begin
        (for-each
         (lambda (x)
           (stat-add!
            (list 'gvm-instr
                  'close
                  (opnd-stat (closure-parms-loc x))
                  (map opnd-stat (closure-parms-opnds x)))
            1)
           (store-stat-add! (closure-parms-loc x))
           (fetch-stat-add! (make-lbl (closure-parms-lbl x)))
           (for-each fetch-stat-add! (closure-parms-opnds x)))
         parms)))
  (let ((total-space-needed (parms->bytes parms)) (lbl1 (new-lbl!)))
    (emit-move.l closure-ptr-slot atemp2)
    (move-n-to-loc68 total-space-needed dtemp1)
    (emit-sub.l dtemp1 atemp2)
    (emit-cmp.l closure-lim-slot atemp2)
    (emit-bcc lbl1)
    (gen-trap instr-source entry-frame #f #f closure-alloc-trap lbl1)
    (emit-move.l atemp2 closure-ptr-slot)
    (let* ((opnds* (apply append (map closure-parms-opnds parms)))
           (sn* (sn-opnds opnds* sn)))
      (let loop1 ((parms parms))
        (let ((loc (closure-parms-loc (car parms)))
              (size (length (closure-parms-opnds (car parms))))
              (rest (cdr parms)))
          (if (= size 1)
              (emit-addq.l type-procedure atemp2)
              (emit-move.w
               (make-imm (+ 32768 (* (+ size 1) 4)))
               (make-pinc atemp2)))
          (move-opnd68-to-loc
           atemp2
           loc
           (sn-opnds (map closure-parms-loc rest) sn*))
          (if (null? rest)
              (add-n-to-loc68
               (+ (- (size->bytes size) total-space-needed) 2)
               atemp2)
              (begin
                (add-n-to-loc68 (- (size->bytes size) type-procedure) atemp2)
                (loop1 rest)))))
      (let loop2 ((parms parms))
        (let* ((opnds (closure-parms-opnds (car parms)))
               (lbl (closure-parms-lbl (car parms)))
               (size (length opnds))
               (rest (cdr parms)))
          (emit-lea (make-pcr lbl 0) atemp1)
          (emit-move.l atemp1 (make-pinc atemp2))
          (let loop3 ((opnds opnds))
            (if (not (null? opnds))
                (let ((sn** (sn-opnds
                             (apply append (map closure-parms-opnds rest))
                             sn)))
                  (move-opnd-to-loc68
                   (car opnds)
                   (make-pinc atemp2)
                   (sn-opnds (cdr opnds) sn**))
                  (loop3 (cdr opnds)))))
          (if (not (null? rest))
              (begin
                (add-n-to-loc68
                 (- (size->bytes size) (* (+ size 1) pointer-size))
                 atemp2)
                (loop2 rest))))))))
(define (gen-ifjump test opnds true-lbl false-lbl poll? next-lbl)
  (if ofile-stats?
      (begin
        (stat-add!
         (list 'gvm-instr
               'ifjump
               (string->canonical-symbol (proc-obj-name test))
               (map opnd-stat opnds)
               (if poll? 'poll 'not-poll))
         1)
        (for-each fetch-stat-add! opnds)
        (stat-dump!)))
  (let ((proc (proc-obj-test test)))
    (if proc
        (gen-ifjump* proc opnds true-lbl false-lbl poll? next-lbl)
        (compiler-internal-error "gen-IFJUMP, unknown 'test':" test))))
(define (gen-ifjump* proc opnds true-lbl false-lbl poll? next-lbl)
  (let ((fs (frame-size exit-frame)))
    (define (double-branch)
      (proc #t opnds false-lbl fs)
      (if ofile-stats?
          (emit-stat
           '((gvm-instr.ifjump.fall-through 1)
             (gvm-instr.ifjump.double-branch 1))))
      (emit-bra true-lbl)
      (gen-deferred-code!))
    (gen-guarantee-fudge)
    (if poll? (gen-poll))
    (if next-lbl
        (cond ((= true-lbl next-lbl)
               (proc #t opnds false-lbl fs)
               (if ofile-stats?
                   (emit-stat '((gvm-instr.ifjump.fall-through 1)))))
              ((= false-lbl next-lbl)
               (proc #f opnds true-lbl fs)
               (if ofile-stats?
                   (emit-stat '((gvm-instr.ifjump.fall-through 1)))))
              (else (double-branch)))
        (double-branch))))
(define (define-ifjump name proc)
  (define-apply
   name
   #f
   (lambda (opnds loc sn)
     (let ((true-lbl (new-lbl!))
           (cont-lbl (new-lbl!))
           (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                      (reg->reg68 loc)
                      dtemp1)))
       (proc #f opnds true-lbl current-fs)
       (move-n-to-loc68 bits-false reg68)
       (emit-bra cont-lbl)
       (emit-label true-lbl)
       (move-n-to-loc68 bits-true reg68)
       (emit-label cont-lbl)
       (move-opnd68-to-loc reg68 loc sn))))
  (proc-obj-test-set! (get-prim-info name) proc))
(define (gen-jump opnd nb-args poll? next-lbl)
  (let ((fs (frame-size exit-frame)))
    (if ofile-stats?
        (begin
          (stat-add!
           (list 'gvm-instr
                 'jump
                 (opnd-stat opnd)
                 nb-args
                 (if poll? 'poll 'not-poll))
           1)
          (jump-stat-add! opnd)
          (if (and (lbl? opnd) next-lbl (= next-lbl (lbl-num opnd)))
              (stat-add! '(gvm-instr.jump.fall-through) 1))
          (stat-dump!)))
    (gen-guarantee-fudge)
    (cond ((glo? opnd)
           (if poll? (gen-poll))
           (setup-jump fs nb-args)
           (emit-jmp-glob (make-glob (glo-name opnd)))
           (gen-deferred-code!))
          ((and (stk? opnd) (= (stk-num opnd) (+ fs 1)) (not nb-args))
           (if poll? (gen-poll))
           (setup-jump (+ fs 1) nb-args)
           (emit-rts)
           (gen-deferred-code!))
          ((lbl? opnd)
           (if (and poll?
                    (= fs current-fs)
                    (not nb-args)
                    (not (and next-lbl (= next-lbl (lbl-num opnd)))))
               (gen-poll-branch (lbl-num opnd))
               (begin
                 (if poll? (gen-poll))
                 (setup-jump fs nb-args)
                 (if (not (and next-lbl (= next-lbl (lbl-num opnd))))
                     (emit-bra (lbl-num opnd))))))
          ((obj? opnd)
           (if poll? (gen-poll))
           (let ((val (obj-val opnd)))
             (if (proc-obj? val)
                 (let ((num (add-object val))
                       (offset (no-arg-check-entry-offset val nb-args)))
                   (setup-jump fs (if (<= offset 0) nb-args #f))
                   (if num
                       (emit-jmp-proc num offset)
                       (emit-jmp-prim val offset))
                   (gen-deferred-code!))
                 (gen-jump* (opnd->opnd68 opnd #f fs) fs nb-args))))
          (else
           (if poll? (gen-poll))
           (gen-jump* (opnd->opnd68 opnd #f fs) fs nb-args)))))
(define (gen-jump* opnd fs nb-args)
  (if nb-args
      (let ((lbl (new-lbl!)))
        (make-top-of-frame-if-stk-opnd68 opnd fs)
        (move-opnd68-to-loc68 (opnd68->true-opnd68 opnd fs) atemp1)
        (shrink-frame fs)
        (emit-move.l atemp1 dtemp1)
        (emit-addq.w (modulo (- type-pair type-procedure) 8) dtemp1)
        (emit-btst dtemp1 pair-reg)
        (emit-beq lbl)
        (move-n-to-loc68 (encode-arg-count nb-args) arg-count-reg)
        (emit-trap3 non-proc-jump-trap)
        (emit-label lbl)
        (move-n-to-loc68 (encode-arg-count nb-args) arg-count-reg)
        (emit-jmp (make-ind atemp1)))
      (let ((areg (move-opnd68-to-any-areg opnd #f fs)))
        (setup-jump fs nb-args)
        (emit-jmp (make-ind areg))))
  (gen-deferred-code!))
(define (setup-jump fs nb-args)
  (shrink-frame fs)
  (if nb-args (move-n-to-loc68 (encode-arg-count nb-args) arg-count-reg)))
(define (gen-poll)
  (let ((lbl (new-lbl!)))
    (emit-dbra poll-timer-reg lbl)
    (emit-moveq (- polling-intermittency 1) poll-timer-reg)
    (emit-cmp.l intr-flag-slot sp-reg)
    (emit-bcc lbl)
    (gen-trap instr-source entry-frame #f #f intr-trap lbl)))
(define (gen-poll-branch lbl)
  (emit-dbra poll-timer-reg lbl)
  (emit-moveq (- polling-intermittency 1) poll-timer-reg)
  (emit-cmp.l intr-flag-slot sp-reg)
  (emit-bcc lbl)
  (gen-trap instr-source entry-frame #f #f intr-trap (new-lbl!))
  (emit-bra lbl))
(define (make-gen-slot-ref slot type)
  (lambda (opnds loc sn)
    (let ((sn-loc (sn-opnd loc sn)) (opnd (car opnds)))
      (move-opnd-to-loc68 opnd atemp1 sn-loc)
      (move-opnd68-to-loc
       (make-disp* atemp1 (- (* slot pointer-size) type))
       loc
       sn))))
(define (make-gen-slot-set! slot type)
  (lambda (opnds loc sn)
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let* ((first-opnd (car opnds))
             (second-opnd (cadr opnds))
             (sn-second-opnd (sn-opnd second-opnd sn-loc)))
        (move-opnd-to-loc68 first-opnd atemp1 sn-second-opnd)
        (move-opnd-to-loc68
         second-opnd
         (make-disp* atemp1 (- (* slot pointer-size) type))
         sn-loc)
        (if loc
            (if (not (eq? first-opnd loc))
                (move-opnd68-to-loc atemp1 loc sn)))))))
(define (gen-cons opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn)))
    (let ((first-opnd (car opnds)) (second-opnd (cadr opnds)))
      (gen-guarantee-space 2)
      (if (contains-opnd? loc second-opnd)
          (let ((sn-second-opnd (sn-opnd second-opnd sn-loc)))
            (move-opnd-to-loc68 first-opnd (make-pdec heap-reg) sn-second-opnd)
            (move-opnd68-to-loc68 heap-reg atemp2)
            (move-opnd-to-loc68 second-opnd (make-pdec heap-reg) sn-loc)
            (move-opnd68-to-loc atemp2 loc sn))
          (let* ((sn-second-opnd (sn-opnd second-opnd sn))
                 (sn-loc (sn-opnd loc sn-second-opnd)))
            (move-opnd-to-loc68 first-opnd (make-pdec heap-reg) sn-loc)
            (move-opnd68-to-loc heap-reg loc sn-second-opnd)
            (move-opnd-to-loc68 second-opnd (make-pdec heap-reg) sn))))))
(define (make-gen-apply-c...r pattern)
  (lambda (opnds loc sn)
    (let ((sn-loc (sn-opnd loc sn)) (opnd (car opnds)))
      (move-opnd-to-loc68 opnd atemp1 sn-loc)
      (let loop ((pattern pattern))
        (if (<= pattern 3)
            (if (= pattern 3)
                (move-opnd68-to-loc (make-pdec atemp1) loc sn)
                (move-opnd68-to-loc (make-ind atemp1) loc sn))
            (begin
              (if (odd? pattern)
                  (emit-move.l (make-pdec atemp1) atemp1)
                  (emit-move.l (make-ind atemp1) atemp1))
              (loop (quotient pattern 2))))))))
(define (gen-set-car! opnds loc sn)
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let* ((first-opnd (car opnds))
           (second-opnd (cadr opnds))
           (sn-second-opnd (sn-opnd second-opnd sn-loc)))
      (move-opnd-to-loc68 first-opnd atemp1 sn-second-opnd)
      (move-opnd-to-loc68 second-opnd (make-ind atemp1) sn-loc)
      (if (and loc (not (eq? first-opnd loc)))
          (move-opnd68-to-loc atemp1 loc sn)))))
(define (gen-set-cdr! opnds loc sn)
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let* ((first-opnd (car opnds))
           (second-opnd (cadr opnds))
           (sn-second-opnd (sn-opnd second-opnd sn-loc)))
      (move-opnd-to-loc68 first-opnd atemp1 sn-second-opnd)
      (if (and loc (not (eq? first-opnd loc)))
          (move-opnd-to-loc68
           second-opnd
           (make-disp atemp1 (- pointer-size))
           sn-loc)
          (move-opnd-to-loc68 second-opnd (make-pdec atemp1) sn-loc))
      (if (and loc (not (eq? first-opnd loc)))
          (move-opnd68-to-loc atemp1 loc sn)))))
(define (commut-oper gen opnds loc sn self? accum-self accum-other)
  (if (null? opnds)
      (gen (reverse accum-self) (reverse accum-other) loc sn self?)
      (let ((opnd (car opnds)) (rest (cdr opnds)))
        (cond ((and (not self?) (eq? opnd loc))
               (commut-oper gen rest loc sn #t accum-self accum-other))
              ((contains-opnd? loc opnd)
               (commut-oper
                gen
                rest
                loc
                sn
                self?
                (cons opnd accum-self)
                accum-other))
              (else
               (commut-oper
                gen
                rest
                loc
                sn
                self?
                accum-self
                (cons opnd accum-other)))))))
(define (gen-add-in-place opnds loc68 sn)
  (if (not (null? opnds))
      (let* ((first-opnd (car opnds))
             (other-opnds (cdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
             (opnd68 (opnd->opnd68
                      first-opnd
                      (temp-in-opnd68 loc68)
                      (sn-opnd68 loc68 sn))))
        (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn-other-opnds)
        (if (imm? opnd68)
            (add-n-to-loc68
             (imm-val opnd68)
             (opnd68->true-opnd68 loc68 sn-other-opnds))
            (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
              (if (or (dreg? opnd68) (reg68? loc68))
                  (emit-add.l
                   opnd68*
                   (opnd68->true-opnd68 loc68 sn-other-opnds))
                  (begin
                    (move-opnd68-to-loc68 opnd68* dtemp1)
                    (emit-add.l
                     dtemp1
                     (opnd68->true-opnd68 loc68 sn-other-opnds))))))
        (gen-add-in-place other-opnds loc68 sn))))
(define (gen-add self-opnds other-opnds loc sn self?)
  (let* ((opnds (append self-opnds other-opnds))
         (first-opnd (car opnds))
         (other-opnds (cdr opnds))
         (sn-other-opnds (sn-opnds other-opnds sn))
         (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
    (if (<= (length self-opnds) 1)
        (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
          (if self?
              (gen-add-in-place opnds loc68 sn)
              (begin
                (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds)
                (gen-add-in-place other-opnds loc68 sn))))
        (begin
          (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn-other-opnds))
          (gen-add-in-place other-opnds dtemp1 (sn-opnd loc sn))
          (if self?
              (let ((loc68 (loc->loc68 loc dtemp1 sn)))
                (make-top-of-frame-if-stk-opnd68 loc68 sn)
                (emit-add.l dtemp1 (opnd68->true-opnd68 loc68 sn)))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-sub-in-place opnds loc68 sn)
  (if (not (null? opnds))
      (let* ((first-opnd (car opnds))
             (other-opnds (cdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
             (opnd68 (opnd->opnd68
                      first-opnd
                      (temp-in-opnd68 loc68)
                      (sn-opnd68 loc68 sn))))
        (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn-other-opnds)
        (if (imm? opnd68)
            (add-n-to-loc68
             (- (imm-val opnd68))
             (opnd68->true-opnd68 loc68 sn-other-opnds))
            (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
              (if (or (dreg? opnd68) (reg68? loc68))
                  (emit-sub.l
                   opnd68*
                   (opnd68->true-opnd68 loc68 sn-other-opnds))
                  (begin
                    (move-opnd68-to-loc68 opnd68* dtemp1)
                    (emit-sub.l
                     dtemp1
                     (opnd68->true-opnd68 loc68 sn-other-opnds))))))
        (gen-sub-in-place other-opnds loc68 sn))))
(define (gen-sub first-opnd other-opnds loc sn self-opnds?)
  (if (null? other-opnds)
      (if (and (or (reg? loc) (stk? loc)) (not (eq? loc return-reg)))
          (begin
            (copy-opnd-to-loc first-opnd loc (sn-opnd loc sn))
            (let ((loc68 (loc->loc68 loc #f sn)))
              (make-top-of-frame-if-stk-opnd68 loc68 sn)
              (emit-neg.l (opnd68->true-opnd68 loc68 sn))))
          (begin
            (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn))
            (emit-neg.l dtemp1)
            (move-opnd68-to-loc dtemp1 loc sn)))
      (let* ((sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
        (if (and (not self-opnds?) (or (reg? loc) (stk? loc)))
            (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
              (if (not (eq? first-opnd loc))
                  (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds))
              (gen-sub-in-place other-opnds loc68 sn))
            (begin
              (move-opnd-to-loc68
               first-opnd
               dtemp1
               (sn-opnd loc sn-other-opnds))
              (gen-sub-in-place other-opnds dtemp1 (sn-opnd loc sn))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-mul-in-place opnds reg68 sn)
  (if (not (null? opnds))
      (let* ((first-opnd (car opnds))
             (other-opnds (cdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (opnd68 (opnd->opnd68 first-opnd (temp-in-opnd68 reg68) sn)))
        (make-top-of-frame-if-stk-opnd68 opnd68 sn-other-opnds)
        (if (imm? opnd68)
            (mul-n-to-reg68 (quotient (imm-val opnd68) 8) reg68)
            (begin
              (emit-asr.l (make-imm 3) reg68)
              (emit-muls.l (opnd68->true-opnd68 opnd68 sn-other-opnds) reg68)))
        (gen-mul-in-place other-opnds reg68 sn))))
(define (gen-mul self-opnds other-opnds loc sn self?)
  (let* ((opnds (append self-opnds other-opnds))
         (first-opnd (car opnds))
         (other-opnds (cdr opnds))
         (sn-other-opnds (sn-opnds other-opnds sn))
         (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
    (if (null? self-opnds)
        (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
          (if self?
              (gen-mul-in-place opnds loc68 sn)
              (begin
                (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds)
                (gen-mul-in-place other-opnds loc68 sn))))
        (begin
          (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn-other-opnds))
          (gen-mul-in-place other-opnds dtemp1 (sn-opnd loc sn))
          (if self?
              (let ((loc68 (loc->loc68 loc dtemp1 sn)))
                (make-top-of-frame-if-stk-opnd68 loc68 sn)
                (emit-asr.l (make-imm 3) dtemp1)
                (emit-muls.l dtemp1 (opnd68->true-opnd68 loc68 sn)))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-div-in-place opnds reg68 sn)
  (if (not (null? opnds))
      (let* ((first-opnd (car opnds))
             (other-opnds (cdr opnds))
             (sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
             (opnd68 (opnd->opnd68 first-opnd (temp-in-opnd68 reg68) sn)))
        (make-top-of-frame-if-stk-opnd68 opnd68 sn-other-opnds)
        (if (imm? opnd68)
            (let ((n (quotient (imm-val opnd68) 8)))
              (div-n-to-reg68 n reg68)
              (if (> (abs n) 1) (emit-and.w (make-imm -8) reg68)))
            (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
              (emit-divsl.l opnd68* reg68 reg68)
              (emit-asl.l (make-imm 3) reg68)))
        (gen-div-in-place other-opnds reg68 sn))))
(define (gen-div first-opnd other-opnds loc sn self-opnds?)
  (if (null? other-opnds)
      (begin
        (move-opnd-to-loc68 first-opnd pdec-sp (sn-opnd loc sn))
        (emit-moveq 8 dtemp1)
        (emit-divsl.l pinc-sp dtemp1 dtemp1)
        (emit-asl.l (make-imm 3) dtemp1)
        (emit-and.w (make-imm -8) dtemp1)
        (move-opnd68-to-loc dtemp1 loc sn))
      (let* ((sn-other-opnds (sn-opnds other-opnds sn))
             (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
        (if (and (reg? loc) (not self-opnds?) (not (eq? loc return-reg)))
            (let ((reg68 (reg->reg68 loc)))
              (if (not (eq? first-opnd loc))
                  (move-opnd-to-loc68 first-opnd reg68 sn-other-opnds))
              (gen-div-in-place other-opnds reg68 sn))
            (begin
              (move-opnd-to-loc68
               first-opnd
               dtemp1
               (sn-opnd loc sn-other-opnds))
              (gen-div-in-place other-opnds dtemp1 (sn-opnd loc sn))
              (move-opnd68-to-loc dtemp1 loc sn))))))
(define (gen-rem first-opnd second-opnd loc sn)
  (let* ((sn-loc (sn-opnd loc sn))
         (sn-second-opnd (sn-opnd second-opnd sn-loc)))
    (move-opnd-to-loc68 first-opnd dtemp1 sn-second-opnd)
    (let ((opnd68 (opnd->opnd68 second-opnd #f sn-loc))
          (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                     (reg->reg68 loc)
                     false-reg)))
      (make-top-of-frame-if-stk-opnd68 opnd68 sn-loc)
      (let ((opnd68* (if (areg? opnd68)
                         (begin (emit-move.l opnd68 reg68) reg68)
                         (opnd68->true-opnd68 opnd68 sn-loc))))
        (emit-divsl.l opnd68* reg68 dtemp1))
      (move-opnd68-to-loc reg68 loc sn)
      (if (not (and (reg? loc) (not (eq? loc return-reg))))
          (emit-move.l (make-imm bits-false) false-reg)))))
(define (gen-mod first-opnd second-opnd loc sn)
  (let* ((sn-loc (sn-opnd loc sn))
         (sn-first-opnd (sn-opnd first-opnd sn-loc))
         (sn-second-opnd (sn-opnd second-opnd sn-first-opnd))
         (opnd68 (opnd->opnd68 second-opnd #f sn-second-opnd)))
    (define (general-case)
      (let ((lbl1 (new-lbl!))
            (lbl2 (new-lbl!))
            (lbl3 (new-lbl!))
            (opnd68** (opnd68->true-opnd68 opnd68 sn-second-opnd))
            (opnd68* (opnd68->true-opnd68
                      (opnd->opnd68 first-opnd #f sn-second-opnd)
                      sn-second-opnd)))
        (move-opnd68-to-loc68 opnd68* dtemp1)
        (move-opnd68-to-loc68 opnd68** false-reg)
        (emit-divsl.l false-reg false-reg dtemp1)
        (emit-move.l false-reg false-reg)
        (emit-beq lbl3)
        (move-opnd68-to-loc68 opnd68* dtemp1)
        (emit-bmi lbl1)
        (move-opnd68-to-loc68 opnd68** dtemp1)
        (emit-bpl lbl3)
        (emit-bra lbl2)
        (emit-label lbl1)
        (move-opnd68-to-loc68 opnd68** dtemp1)
        (emit-bmi lbl3)
        (emit-label lbl2)
        (emit-add.l dtemp1 false-reg)
        (emit-label lbl3)
        (move-opnd68-to-loc false-reg loc sn)
        (emit-move.l (make-imm bits-false) false-reg)))
    (make-top-of-frame-if-stk-opnd68 opnd68 sn-first-opnd)
    (if (imm? opnd68)
        (let ((n (quotient (imm-val opnd68) 8)))
          (if (> n 0)
              (let ((shift (power-of-2 n)))
                (if shift
                    (let ((reg68 (if (and (reg? loc)
                                          (not (eq? loc return-reg)))
                                     (reg->reg68 loc)
                                     dtemp1)))
                      (move-opnd-to-loc68 first-opnd reg68 sn-loc)
                      (emit-and.l (make-imm (* (- n 1) 8)) reg68)
                      (move-opnd68-to-loc reg68 loc sn))
                    (general-case)))
              (general-case)))
        (general-case))))
(define (gen-op emit-op dst-ok?)
  (define (gen-op-in-place opnds loc68 sn)
    (if (not (null? opnds))
        (let* ((first-opnd (car opnds))
               (other-opnds (cdr opnds))
               (sn-other-opnds (sn-opnds other-opnds sn))
               (sn-first-opnd (sn-opnd first-opnd sn-other-opnds))
               (opnd68 (opnd->opnd68
                        first-opnd
                        (temp-in-opnd68 loc68)
                        (sn-opnd68 loc68 sn))))
          (make-top-of-frame-if-stk-opnds68 opnd68 loc68 sn-other-opnds)
          (if (imm? opnd68)
              (emit-op opnd68 (opnd68->true-opnd68 loc68 sn-other-opnds))
              (let ((opnd68* (opnd68->true-opnd68 opnd68 sn-other-opnds)))
                (if (or (dreg? opnd68) (dst-ok? loc68))
                    (emit-op opnd68*
                             (opnd68->true-opnd68 loc68 sn-other-opnds))
                    (begin
                      (move-opnd68-to-loc68 opnd68* dtemp1)
                      (emit-op dtemp1
                               (opnd68->true-opnd68 loc68 sn-other-opnds))))))
          (gen-op-in-place other-opnds loc68 sn))))
  (lambda (self-opnds other-opnds loc sn self?)
    (let* ((opnds (append self-opnds other-opnds))
           (first-opnd (car opnds))
           (other-opnds (cdr opnds))
           (sn-other-opnds (sn-opnds other-opnds sn))
           (sn-first-opnd (sn-opnd first-opnd sn-other-opnds)))
      (if (<= (length self-opnds) 1)
          (let ((loc68 (loc->loc68 loc #f sn-first-opnd)))
            (if self?
                (gen-op-in-place opnds loc68 sn)
                (begin
                  (move-opnd-to-loc68 first-opnd loc68 sn-other-opnds)
                  (gen-op-in-place other-opnds loc68 sn))))
          (begin
            (move-opnd-to-loc68 first-opnd dtemp1 (sn-opnd loc sn-other-opnds))
            (gen-op-in-place other-opnds dtemp1 (sn-opnd loc sn))
            (if self?
                (let ((loc68 (loc->loc68 loc dtemp1 sn)))
                  (make-top-of-frame-if-stk-opnd68 loc68 sn)
                  (emit-op dtemp1 (opnd68->true-opnd68 loc68 sn)))
                (move-opnd68-to-loc dtemp1 loc sn)))))))
(define gen-logior (gen-op emit-or.l dreg?))
(define gen-logxor (gen-op emit-eor.l (lambda (x) #f)))
(define gen-logand (gen-op emit-and.l dreg?))
(define (gen-shift right-shift)
  (lambda (opnds loc sn)
    (let ((sn-loc (sn-opnd loc sn)))
      (let* ((opnd1 (car opnds))
             (opnd2 (cadr opnds))
             (sn-opnd1 (sn-opnd opnd1 sn-loc))
             (o2 (opnd->opnd68 opnd2 #f sn-opnd1)))
        (make-top-of-frame-if-stk-opnd68 o2 sn-opnd1)
        (if (imm? o2)
            (let* ((reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                              (reg->reg68 loc)
                              dtemp1))
                   (n (quotient (imm-val o2) 8))
                   (emit-shft (if (> n 0) emit-lsl.l right-shift)))
              (move-opnd-to-loc68 opnd1 reg68 sn-loc)
              (let loop ((i (min (abs n) 29)))
                (if (> i 0)
                    (begin
                      (emit-shft (make-imm (min i 8)) reg68)
                      (loop (- i 8)))))
              (if (< n 0) (emit-and.w (make-imm -8) reg68))
              (move-opnd68-to-loc reg68 loc sn))
            (let* ((reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                              (reg->reg68 loc)
                              dtemp1))
                   (reg68* (if (and (reg? loc) (not (eq? loc return-reg)))
                               dtemp1
                               false-reg))
                   (lbl1 (new-lbl!))
                   (lbl2 (new-lbl!)))
              (emit-move.l (opnd68->true-opnd68 o2 sn-opnd1) reg68*)
              (move-opnd-to-loc68 opnd1 reg68 sn-loc)
              (emit-asr.l (make-imm 3) reg68*)
              (emit-bmi lbl1)
              (emit-lsl.l reg68* reg68)
              (emit-bra lbl2)
              (emit-label lbl1)
              (emit-neg.l reg68*)
              (right-shift reg68* reg68)
              (emit-and.w (make-imm -8) reg68)
              (emit-label lbl2)
              (move-opnd68-to-loc reg68 loc sn)
              (if (not (and (reg? loc) (not (eq? loc return-reg))))
                  (emit-move.l (make-imm bits-false) false-reg))))))))
(define (flo-oper oper1 oper2 opnds loc sn)
  (gen-guarantee-space 2)
  (move-opnd-to-loc68
   (car opnds)
   atemp1
   (sn-opnds (cdr opnds) (sn-opnd loc sn)))
  (oper1 (make-disp* atemp1 (- type-flonum)) ftemp1)
  (let loop ((opnds (cdr opnds)))
    (if (not (null? opnds))
        (let* ((opnd (car opnds))
               (other-opnds (cdr opnds))
               (sn-other-opnds (sn-opnds other-opnds sn)))
          (move-opnd-to-loc68 opnd atemp1 sn-other-opnds)
          (oper2 (make-disp* atemp1 (- type-flonum)) ftemp1)
          (loop (cdr opnds)))))
  (add-n-to-loc68 (* -2 pointer-size) heap-reg)
  (emit-fmov.dx ftemp1 (make-ind heap-reg))
  (let ((reg68 (if (reg? loc) (reg->reg68 loc) atemp1)))
    (emit-move.l heap-reg reg68)
    (emit-addq.l type-flonum reg68))
  (if (not (reg? loc)) (move-opnd68-to-loc atemp1 loc sn)))
(define (gen-make-placeholder opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn)))
    (let ((opnd (car opnds)))
      (gen-guarantee-space 4)
      (emit-clr.l (make-pdec heap-reg))
      (move-opnd-to-loc68 opnd (make-pdec heap-reg) sn-loc)
      (emit-move.l null-reg (make-pdec heap-reg))
      (move-opnd68-to-loc68 heap-reg atemp2)
      (emit-addq.l (modulo (- type-placeholder type-pair) 8) atemp2)
      (emit-move.l atemp2 (make-pdec heap-reg))
      (move-opnd68-to-loc atemp2 loc sn))))
(define (gen-subprocedure-id opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (car opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (move-n-to-loc68 32768 reg68)
    (emit-sub.w (make-disp* atemp1 -2) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-subprocedure-parent opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn)) (opnd (car opnds)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-add.w (make-disp* atemp1 -2) atemp1)
    (add-n-to-loc68 -32768 atemp1)
    (move-opnd68-to-loc atemp1 loc sn)))
(define (gen-return-fs opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (car opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1))
        (lbl (new-lbl!)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-moveq 0 reg68)
    (emit-move.w (make-disp* atemp1 -6) reg68)
    (emit-beq lbl)
    (emit-and.w (make-imm 32767) reg68)
    (emit-subq.l 8 reg68)
    (emit-label lbl)
    (emit-addq.l 8 reg68)
    (emit-asl.l (make-imm 1) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-return-link opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (car opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1))
        (lbl (new-lbl!)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-moveq 0 reg68)
    (emit-move.w (make-disp* atemp1 -6) reg68)
    (emit-beq lbl)
    (emit-and.w (make-imm 32767) reg68)
    (emit-subq.l 8 reg68)
    (emit-label lbl)
    (emit-addq.l 8 reg68)
    (emit-sub.w (make-disp* atemp1 -4) reg68)
    (emit-asl.l (make-imm 1) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-procedure-info opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn)) (opnd (car opnds)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-add.w (make-disp* atemp1 -2) atemp1)
    (move-opnd68-to-loc (make-disp* atemp1 (- 32768 6)) loc sn)))
(define (gen-guarantee-space n)
  (set! pointers-allocated (+ pointers-allocated n))
  (if (> pointers-allocated heap-allocation-fudge)
      (begin (gen-guarantee-fudge) (set! pointers-allocated n))))
(define (gen-guarantee-fudge)
  (if (> pointers-allocated 0)
      (let ((lbl (new-lbl!)))
        (emit-cmp.l heap-lim-slot heap-reg)
        (emit-bcc lbl)
        (gen-trap instr-source entry-frame #f #f heap-alloc1-trap lbl)
        (set! pointers-allocated 0))))
(define pointers-allocated '())
(define (gen-type opnds loc sn)
  (let* ((sn-loc (sn-opnd loc sn))
         (opnd (car opnds))
         (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                    (reg->reg68 loc)
                    dtemp1)))
    (move-opnd-to-loc68 opnd reg68 sn-loc)
    (emit-and.l (make-imm 7) reg68)
    (emit-asl.l (make-imm 3) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-type-cast opnds loc sn)
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let ((first-opnd (car opnds)) (second-opnd (cadr opnds)))
      (let* ((sn-loc (if (and loc (not (eq? first-opnd loc))) sn-loc sn))
             (o1 (opnd->opnd68 first-opnd #f (sn-opnd second-opnd sn-loc)))
             (o2 (opnd->opnd68 second-opnd (temp-in-opnd68 o1) sn-loc))
             (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                        (reg->reg68 loc)
                        dtemp1)))
        (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
        (move-opnd68-to-loc68
         (opnd68->true-opnd68 o1 (sn-opnd68 o2 sn-loc))
         reg68)
        (emit-and.w (make-imm -8) reg68)
        (if (imm? o2)
            (let ((n (quotient (imm-val o2) 8)))
              (if (> n 0) (emit-addq.w n reg68)))
            (begin
              (move-opnd68-to-loc68 (opnd68->true-opnd68 o2 sn-loc) atemp1)
              (emit-exg atemp1 reg68)
              (emit-asr.l (make-imm 3) reg68)
              (emit-add.l atemp1 reg68)))
        (move-opnd68-to-loc reg68 loc sn)))))
(define (gen-subtype opnds loc sn)
  (let ((sn-loc (sn-opnd loc sn))
        (opnd (car opnds))
        (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                   (reg->reg68 loc)
                   dtemp1)))
    (move-opnd-to-loc68 opnd atemp1 sn-loc)
    (emit-moveq 0 reg68)
    (emit-move.b (make-ind atemp1) reg68)
    (move-opnd68-to-loc reg68 loc sn)))
(define (gen-subtype-set! opnds loc sn)
  (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
    (let ((first-opnd (car opnds)) (second-opnd (cadr opnds)))
      (let* ((sn-loc (if (and loc (not (eq? first-opnd loc))) sn-loc sn))
             (o1 (opnd->opnd68 first-opnd #f (sn-opnd second-opnd sn-loc)))
             (o2 (opnd->opnd68 second-opnd (temp-in-opnd68 o1) sn-loc)))
        (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
        (move-opnd68-to-loc68
         (opnd68->true-opnd68 o1 (sn-opnd68 o2 sn-loc))
         atemp1)
        (if (imm? o2)
            (emit-move.b (make-imm (imm-val o2)) (make-ind atemp1))
            (begin
              (move-opnd68-to-loc68 (opnd68->true-opnd68 o2 sn-loc) dtemp1)
              (emit-move.b dtemp1 (make-ind atemp1))))
        (if (and loc (not (eq? first-opnd loc)))
            (move-opnd68-to-loc atemp1 loc sn))))))
(define (vector-select kind vector string vector8 vector16)
  (case kind
    ((string) string)
    ((vector8) vector8)
    ((vector16) vector16)
    (else vector)))
(define (obj-vector? kind) (vector-select kind #t #f #f #f))
(define (make-gen-vector kind)
  (lambda (opnds loc sn)
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let* ((n (length opnds))
             (bytes (+ pointer-size
                       (* (vector-select kind 4 1 1 2)
                          (+ n (if (eq? kind 'string) 1 0)))))
             (adjust (modulo (- bytes) 8)))
        (gen-guarantee-space
         (quotient (* (quotient (+ bytes (- 8 1)) 8) 8) pointer-size))
        (if (not (= adjust 0)) (emit-subq.l adjust heap-reg))
        (if (eq? kind 'string) (emit-move.b (make-imm 0) (make-pdec heap-reg)))
        (let loop ((opnds (reverse opnds)))
          (if (pair? opnds)
              (let* ((o (car opnds)) (sn-o (sn-opnds (cdr opnds) sn-loc)))
                (if (eq? kind 'vector)
                    (move-opnd-to-loc68 o (make-pdec heap-reg) sn-o)
                    (begin
                      (move-opnd-to-loc68 o dtemp1 sn-o)
                      (emit-asr.l (make-imm 3) dtemp1)
                      (if (eq? kind 'vector16)
                          (emit-move.w dtemp1 (make-pdec heap-reg))
                          (emit-move.b dtemp1 (make-pdec heap-reg)))))
                (loop (cdr opnds)))))
        (emit-move.l
         (make-imm
          (+ (* 256 (- bytes pointer-size))
             (* 8 (if (eq? kind 'vector) subtype-vector subtype-string))))
         (make-pdec heap-reg))
        (if loc
            (begin
              (emit-lea (make-disp* heap-reg type-subtyped) atemp2)
              (move-opnd68-to-loc atemp2 loc sn)))))))
(define (make-gen-vector-length kind)
  (lambda (opnds loc sn)
    (let ((sn-loc (sn-opnd loc sn))
          (opnd (car opnds))
          (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                     (reg->reg68 loc)
                     dtemp1)))
      (move-opnd-to-loc68 opnd atemp1 sn-loc)
      (move-opnd68-to-loc68 (make-disp* atemp1 (- type-subtyped)) reg68)
      (emit-lsr.l (make-imm (vector-select kind 7 5 5 6)) reg68)
      (if (not (eq? kind 'vector))
          (begin
            (emit-and.w (make-imm -8) reg68)
            (if (eq? kind 'string) (emit-subq.l 8 reg68))))
      (move-opnd68-to-loc reg68 loc sn))))
(define (make-gen-vector-ref kind)
  (lambda (opnds loc sn)
    (let ((sn-loc (sn-opnd loc sn)))
      (let ((first-opnd (car opnds))
            (second-opnd (cadr opnds))
            (reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                       (reg->reg68 loc)
                       dtemp1)))
        (let* ((o2 (opnd->opnd68 second-opnd #f (sn-opnd first-opnd sn-loc)))
               (o1 (opnd->opnd68 first-opnd (temp-in-opnd68 o2) sn-loc)))
          (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
          (let* ((offset (if (eq? kind 'closure)
                             (- pointer-size type-procedure)
                             (- pointer-size type-subtyped)))
                 (loc68 (if (imm? o2)
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-loc)
                               atemp1)
                              (make-disp*
                               atemp1
                               (+ (quotient
                                   (imm-val o2)
                                   (vector-select kind 2 8 8 4))
                                  offset)))
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o2 (sn-opnd68 o1 sn-loc))
                               dtemp1)
                              (emit-asr.l
                               (make-imm (vector-select kind 1 3 3 2))
                               dtemp1)
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-loc)
                               atemp1)
                              (if (and (identical-opnd68? reg68 dtemp1)
                                       (not (obj-vector? kind)))
                                  (begin
                                    (emit-move.l dtemp1 atemp2)
                                    (make-inx atemp1 atemp2 offset))
                                  (make-inx atemp1 dtemp1 offset))))))
            (if (not (obj-vector? kind)) (emit-moveq 0 reg68))
            (case kind
              ((string vector8) (emit-move.b loc68 reg68))
              ((vector16) (emit-move.w loc68 reg68))
              (else (emit-move.l loc68 reg68)))
            (if (not (obj-vector? kind))
                (begin
                  (emit-asl.l (make-imm 3) reg68)
                  (if (eq? kind 'string) (emit-addq.w type-special reg68))))
            (move-opnd68-to-loc reg68 loc sn)))))))
(define (make-gen-vector-set! kind)
  (lambda (opnds loc sn)
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let ((first-opnd (car opnds))
            (second-opnd (cadr opnds))
            (third-opnd (caddr opnds)))
        (let* ((sn-loc (if (and loc (not (eq? first-opnd loc)))
                           (sn-opnd first-opnd sn-loc)
                           sn))
               (sn-third-opnd (sn-opnd third-opnd sn-loc))
               (o2 (opnd->opnd68
                    second-opnd
                    #f
                    (sn-opnd first-opnd sn-third-opnd)))
               (o1 (opnd->opnd68
                    first-opnd
                    (temp-in-opnd68 o2)
                    sn-third-opnd)))
          (make-top-of-frame-if-stk-opnds68 o1 o2 sn-third-opnd)
          (let* ((offset (if (eq? kind 'closure)
                             (- pointer-size type-procedure)
                             (- pointer-size type-subtyped)))
                 (loc68 (if (imm? o2)
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-third-opnd)
                               atemp1)
                              (make-disp*
                               atemp1
                               (+ (quotient
                                   (imm-val o2)
                                   (vector-select kind 2 8 8 4))
                                  offset)))
                            (begin
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o2 (sn-opnd68 o1 sn-loc))
                               dtemp1)
                              (emit-asr.l
                               (make-imm (vector-select kind 1 3 3 2))
                               dtemp1)
                              (move-opnd68-to-loc68
                               (opnd68->true-opnd68 o1 sn-loc)
                               atemp1)
                              (if (obj-vector? kind)
                                  (make-inx atemp1 dtemp1 offset)
                                  (begin
                                    (emit-move.l dtemp1 atemp2)
                                    (make-inx atemp1 atemp2 offset)))))))
            (if (obj-vector? kind)
                (move-opnd-to-loc68 third-opnd loc68 sn-loc)
                (begin
                  (move-opnd-to-loc68 third-opnd dtemp1 sn-loc)
                  (emit-asr.l (make-imm 3) dtemp1)
                  (if (eq? kind 'vector16)
                      (emit-move.w dtemp1 loc68)
                      (emit-move.b dtemp1 loc68))))
            (if (and loc (not (eq? first-opnd loc)))
                (copy-opnd-to-loc first-opnd loc sn))))))))
(define (make-gen-vector-shrink! kind)
  (lambda (opnds loc sn)
    (let ((sn-loc (if loc (sn-opnd loc sn) sn)))
      (let ((first-opnd (car opnds)) (second-opnd (cadr opnds)))
        (let* ((sn-loc (if (and loc (not (eq? first-opnd loc)))
                           (sn-opnd first-opnd sn-loc)
                           sn))
               (o2 (opnd->opnd68 second-opnd #f (sn-opnd first-opnd sn-loc)))
               (o1 (opnd->opnd68 first-opnd (temp-in-opnd68 o2) sn-loc)))
          (make-top-of-frame-if-stk-opnds68 o1 o2 sn-loc)
          (move-opnd68-to-loc68
           (opnd68->true-opnd68 o2 (sn-opnd68 o1 sn-loc))
           dtemp1)
          (emit-move.l (opnd68->true-opnd68 o1 sn-loc) atemp1)
          (if (eq? kind 'string)
              (begin
                (emit-asr.l (make-imm 3) dtemp1)
                (emit-move.b
                 (make-imm 0)
                 (make-inx atemp1 dtemp1 (- pointer-size type-subtyped)))
                (emit-addq.l 1 dtemp1)
                (emit-asl.l (make-imm 8) dtemp1))
              (emit-asl.l (make-imm (vector-select kind 7 5 5 6)) dtemp1))
          (emit-move.b (make-ind atemp1) dtemp1)
          (emit-move.l dtemp1 (make-disp* atemp1 (- type-subtyped)))
          (if (and loc (not (eq? first-opnd loc)))
              (move-opnd68-to-loc atemp1 loc sn)))))))
(define (gen-eq-test bits not? opnds lbl fs)
  (gen-compare* (opnd->opnd68 (car opnds) #f fs) (make-imm bits) fs)
  (if not? (emit-bne lbl) (emit-beq lbl)))
(define (gen-compare opnd1 opnd2 fs)
  (let* ((o1 (opnd->opnd68 opnd1 #f (sn-opnd opnd2 fs)))
         (o2 (opnd->opnd68 opnd2 (temp-in-opnd68 o1) fs)))
    (gen-compare* o1 o2 fs)))
(define (gen-compare* o1 o2 fs)
  (make-top-of-frame-if-stk-opnds68 o1 o2 fs)
  (let ((order-1-2
         (cond ((imm? o1)
                (cmp-n-to-opnd68 (imm-val o1) (opnd68->true-opnd68 o2 fs)))
               ((imm? o2)
                (not (cmp-n-to-opnd68
                      (imm-val o2)
                      (opnd68->true-opnd68 o1 fs))))
               ((reg68? o1) (emit-cmp.l (opnd68->true-opnd68 o2 fs) o1) #f)
               ((reg68? o2) (emit-cmp.l (opnd68->true-opnd68 o1 fs) o2) #t)
               (else
                (emit-move.l (opnd68->true-opnd68 o1 (sn-opnd68 o2 fs)) dtemp1)
                (emit-cmp.l (opnd68->true-opnd68 o2 fs) dtemp1)
                #f))))
    (shrink-frame fs)
    order-1-2))
(define (gen-compares branch< branch>= branch> branch<= not? opnds lbl fs)
  (gen-compares*
   gen-compare
   branch<
   branch>=
   branch>
   branch<=
   not?
   opnds
   lbl
   fs))
(define (gen-compares*
         gen-comp
         branch<
         branch>=
         branch>
         branch<=
         not?
         opnds
         lbl
         fs)
  (define (gen-compare-sequence opnd1 opnd2 rest)
    (if (null? rest)
        (if (gen-comp opnd1 opnd2 fs)
            (if not? (branch<= lbl) (branch> lbl))
            (if not? (branch>= lbl) (branch< lbl)))
        (let ((order-1-2
               (gen-comp opnd1 opnd2 (sn-opnd opnd2 (sn-opnds rest fs)))))
          (if (= current-fs fs)
              (if not?
                  (begin
                    (if order-1-2 (branch<= lbl) (branch>= lbl))
                    (gen-compare-sequence opnd2 (car rest) (cdr rest)))
                  (let ((exit-lbl (new-lbl!)))
                    (if order-1-2 (branch<= exit-lbl) (branch>= exit-lbl))
                    (gen-compare-sequence opnd2 (car rest) (cdr rest))
                    (emit-label exit-lbl)))
              (if not?
                  (let ((next-lbl (new-lbl!)))
                    (if order-1-2 (branch> next-lbl) (branch< next-lbl))
                    (shrink-frame fs)
                    (emit-bra lbl)
                    (emit-label next-lbl)
                    (gen-compare-sequence opnd2 (car rest) (cdr rest)))
                  (let* ((next-lbl (new-lbl!)) (exit-lbl (new-lbl!)))
                    (if order-1-2 (branch> next-lbl) (branch< next-lbl))
                    (shrink-frame fs)
                    (emit-bra exit-lbl)
                    (emit-label next-lbl)
                    (gen-compare-sequence opnd2 (car rest) (cdr rest))
                    (emit-label exit-lbl)))))))
  (if (or (null? opnds) (null? (cdr opnds)))
      (begin (shrink-frame fs) (if (not not?) (emit-bra lbl)))
      (gen-compare-sequence (car opnds) (cadr opnds) (cddr opnds))))
(define (gen-compare-flo opnd1 opnd2 fs)
  (let* ((o1 (opnd->opnd68 opnd1 #f (sn-opnd opnd2 fs)))
         (o2 (opnd->opnd68 opnd2 (temp-in-opnd68 o1) fs)))
    (make-top-of-frame-if-stk-opnds68 o1 o2 fs)
    (emit-move.l (opnd68->true-opnd68 o1 (sn-opnd68 o2 fs)) atemp1)
    (emit-move.l (opnd68->true-opnd68 o2 fs) atemp2)
    (emit-fmov.dx (make-disp* atemp2 (- type-flonum)) ftemp1)
    (emit-fcmp.dx (make-disp* atemp1 (- type-flonum)) ftemp1)
    #t))
(define (gen-compares-flo branch< branch>= branch> branch<= not? opnds lbl fs)
  (gen-compares*
   gen-compare-flo
   branch<
   branch>=
   branch>
   branch<=
   not?
   opnds
   lbl
   fs))
(define (gen-type-test tag not? opnds lbl fs)
  (let ((opnd (car opnds)))
    (let ((o (opnd->opnd68 opnd #f fs)))
      (define (mask-test set-reg correction)
        (emit-btst
         (if (= correction 0)
             (if (dreg? o)
                 o
                 (begin
                   (emit-move.l (opnd68->true-opnd68 o fs) dtemp1)
                   dtemp1))
             (begin
               (if (not (eq? o dtemp1))
                   (emit-move.l (opnd68->true-opnd68 o fs) dtemp1))
               (emit-addq.w correction dtemp1)
               dtemp1))
         set-reg))
      (make-top-of-frame-if-stk-opnd68 o fs)
      (cond ((= tag 0)
             (if (eq? o dtemp1)
                 (emit-and.w (make-imm 7) dtemp1)
                 (begin
                   (emit-move.l (opnd68->true-opnd68 o fs) dtemp1)
                   (emit-and.w (make-imm 7) dtemp1))))
            ((= tag type-placeholder) (mask-test placeholder-reg 0))
            (else (mask-test pair-reg (modulo (- type-pair tag) 8))))
      (shrink-frame fs)
      (if not? (emit-bne lbl) (emit-beq lbl)))))
(define (gen-subtype-test type not? opnds lbl fs)
  (let ((opnd (car opnds)))
    (let ((o (opnd->opnd68 opnd #f fs)) (cont-lbl (new-lbl!)))
      (make-top-of-frame-if-stk-opnd68 o fs)
      (if (not (eq? o dtemp1)) (emit-move.l (opnd68->true-opnd68 o fs) dtemp1))
      (emit-move.l dtemp1 atemp1)
      (emit-addq.w (modulo (- type-pair type-subtyped) 8) dtemp1)
      (emit-btst dtemp1 pair-reg)
      (shrink-frame fs)
      (if not? (emit-bne lbl) (emit-bne cont-lbl))
      (emit-cmp.b (make-imm (* type 8)) (make-ind atemp1))
      (if not? (emit-bne lbl) (emit-beq lbl))
      (emit-label cont-lbl))))
(define (gen-even-test not? opnds lbl fs)
  (move-opnd-to-loc68 (car opnds) dtemp1 fs)
  (emit-and.w (make-imm 8) dtemp1)
  (shrink-frame fs)
  (if not? (emit-bne lbl) (emit-beq lbl)))
(define (def-spec name specializer-maker)
  (let ((proc-name (string->canonical-symbol name)))
    (let ((proc (prim-info proc-name)))
      (if proc
          (proc-obj-specialize-set! proc (specializer-maker proc proc-name))
          (compiler-internal-error "def-spec, unknown primitive:" name)))))
(define (safe name)
  (lambda (proc proc-name)
    (let ((spec (get-prim-info name))) (lambda (decls) spec))))
(define (unsafe name)
  (lambda (proc proc-name)
    (let ((spec (get-prim-info name)))
      (lambda (decls) (if (not (safe? decls)) spec proc)))))
(define (safe-arith fix-name flo-name) (arith #t fix-name flo-name))
(define (unsafe-arith fix-name flo-name) (arith #f fix-name flo-name))
(define (arith fix-safe? fix-name flo-name)
  (lambda (proc proc-name)
    (let ((fix-spec (if fix-name (get-prim-info fix-name) proc))
          (flo-spec (if flo-name (get-prim-info flo-name) proc)))
      (lambda (decls)
        (let ((arith (arith-implementation proc-name decls)))
          (cond ((eq? arith fixnum-sym)
                 (if (or fix-safe? (not (safe? decls))) fix-spec proc))
                ((eq? arith flonum-sym) (if (not (safe? decls)) flo-spec proc))
                (else proc)))))))
(define-apply "##TYPE" #f (lambda (opnds loc sn) (gen-type opnds loc sn)))
(define-apply
 "##TYPE-CAST"
 #f
 (lambda (opnds loc sn) (gen-type-cast opnds loc sn)))
(define-apply
 "##SUBTYPE"
 #f
 (lambda (opnds loc sn) (gen-subtype opnds loc sn)))
(define-apply
 "##SUBTYPE-SET!"
 #t
 (lambda (opnds loc sn) (gen-subtype-set! opnds loc sn)))
(define-ifjump
 "##NOT"
 (lambda (not? opnds lbl fs) (gen-eq-test bits-false not? opnds lbl fs)))
(define-ifjump
 "##NULL?"
 (lambda (not? opnds lbl fs) (gen-eq-test bits-null not? opnds lbl fs)))
(define-ifjump
 "##UNASSIGNED?"
 (lambda (not? opnds lbl fs) (gen-eq-test bits-unass not? opnds lbl fs)))
(define-ifjump
 "##UNBOUND?"
 (lambda (not? opnds lbl fs) (gen-eq-test bits-unbound not? opnds lbl fs)))
(define-ifjump
 "##EQ?"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-beq emit-bne emit-beq emit-bne not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM?"
 (lambda (not? opnds lbl fs) (gen-type-test type-fixnum not? opnds lbl fs)))
(define-ifjump
 "##FLONUM?"
 (lambda (not? opnds lbl fs) (gen-type-test type-flonum not? opnds lbl fs)))
(define-ifjump
 "##SPECIAL?"
 (lambda (not? opnds lbl fs) (gen-type-test type-special not? opnds lbl fs)))
(define-ifjump
 "##PAIR?"
 (lambda (not? opnds lbl fs) (gen-type-test type-pair not? opnds lbl fs)))
(define-ifjump
 "##SUBTYPED?"
 (lambda (not? opnds lbl fs) (gen-type-test type-subtyped not? opnds lbl fs)))
(define-ifjump
 "##PROCEDURE?"
 (lambda (not? opnds lbl fs) (gen-type-test type-procedure not? opnds lbl fs)))
(define-ifjump
 "##PLACEHOLDER?"
 (lambda (not? opnds lbl fs)
   (gen-type-test type-placeholder not? opnds lbl fs)))
(define-ifjump
 "##VECTOR?"
 (lambda (not? opnds lbl fs)
   (gen-subtype-test subtype-vector not? opnds lbl fs)))
(define-ifjump
 "##SYMBOL?"
 (lambda (not? opnds lbl fs)
   (gen-subtype-test subtype-symbol not? opnds lbl fs)))
(define-ifjump
 "##RATNUM?"
 (lambda (not? opnds lbl fs)
   (gen-subtype-test subtype-ratnum not? opnds lbl fs)))
(define-ifjump
 "##CPXNUM?"
 (lambda (not? opnds lbl fs)
   (gen-subtype-test subtype-cpxnum not? opnds lbl fs)))
(define-ifjump
 "##STRING?"
 (lambda (not? opnds lbl fs)
   (gen-subtype-test subtype-string not? opnds lbl fs)))
(define-ifjump
 "##BIGNUM?"
 (lambda (not? opnds lbl fs)
   (gen-subtype-test subtype-bignum not? opnds lbl fs)))
(define-ifjump
 "##CHAR?"
 (lambda (not? opnds lbl fs)
   (let ((opnd (car opnds)))
     (let ((o (opnd->opnd68 opnd #f fs)) (cont-lbl (new-lbl!)))
       (make-top-of-frame-if-stk-opnd68 o fs)
       (emit-move.l (opnd68->true-opnd68 o fs) dtemp1)
       (if not? (emit-bmi lbl) (emit-bmi cont-lbl))
       (emit-addq.w (modulo (- type-pair type-special) 8) dtemp1)
       (emit-btst dtemp1 pair-reg)
       (shrink-frame fs)
       (if not? (emit-bne lbl) (emit-beq lbl))
       (emit-label cont-lbl)))))
(define-ifjump
 "##CLOSURE?"
 (lambda (not? opnds lbl fs)
   (move-opnd-to-loc68 (car opnds) atemp1 fs)
   (shrink-frame fs)
   (emit-cmp.w (make-imm 20153) (make-ind atemp1))
   (if not? (emit-bne lbl) (emit-beq lbl))))
(define-ifjump
 "##SUBPROCEDURE?"
 (lambda (not? opnds lbl fs)
   (move-opnd-to-loc68 (car opnds) atemp1 fs)
   (shrink-frame fs)
   (emit-move.w (make-pdec atemp1) dtemp1)
   (if not? (emit-bmi lbl) (emit-bpl lbl))))
(define-ifjump
 "##RETURN-DYNAMIC-ENV-BIND?"
 (lambda (not? opnds lbl fs)
   (move-opnd-to-loc68 (car opnds) atemp1 fs)
   (shrink-frame fs)
   (emit-move.w (make-disp* atemp1 -6) dtemp1)
   (if not? (emit-bne lbl) (emit-beq lbl))))
(define-apply
 "##FIXNUM.+"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '0) loc sn))
           ((null? (cdr opnds)) (copy-opnd-to-loc (car opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-add opnds loc sn #f '() '()))
           (else (gen-add opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.-"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-sub (car opnds)
              (cdr opnds)
              loc
              sn
              (any-contains-opnd? loc (cdr opnds))))))
(define-apply
 "##FIXNUM.*"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '1) loc sn))
           ((null? (cdr opnds)) (copy-opnd-to-loc (car opnds) loc sn))
           ((and (reg? loc) (not (eq? loc return-reg)))
            (commut-oper gen-mul opnds loc sn #f '() '()))
           (else (gen-mul opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.QUOTIENT"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-div (car opnds)
              (cdr opnds)
              loc
              sn
              (any-contains-opnd? loc (cdr opnds))))))
(define-apply
 "##FIXNUM.REMAINDER"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-rem (car opnds) (cadr opnds) loc sn))))
(define-apply
 "##FIXNUM.MODULO"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (gen-mod (car opnds) (cadr opnds) loc sn))))
(define-apply
 "##FIXNUM.LOGIOR"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '0) loc sn))
           ((null? (cdr opnds)) (copy-opnd-to-loc (car opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-logior opnds loc sn #f '() '()))
           (else (gen-logior opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.LOGXOR"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '0) loc sn))
           ((null? (cdr opnds)) (copy-opnd-to-loc (car opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-logxor opnds loc sn #f '() '()))
           (else (gen-logxor opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.LOGAND"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj '-1) loc sn))
           ((null? (cdr opnds)) (copy-opnd-to-loc (car opnds) loc sn))
           ((or (reg? loc) (stk? loc))
            (commut-oper gen-logand opnds loc sn #f '() '()))
           (else (gen-logand opnds '() loc sn #f))))))
(define-apply
 "##FIXNUM.LOGNOT"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)) (opnd (car opnds)))
     (if (and (or (reg? loc) (stk? loc)) (not (eq? loc return-reg)))
         (begin
           (copy-opnd-to-loc opnd loc sn-loc)
           (let ((loc68 (loc->loc68 loc #f sn)))
             (make-top-of-frame-if-stk-opnd68 loc68 sn)
             (emit-not.l (opnd68->true-opnd68 loc68 sn))
             (emit-and.w (make-imm -8) (opnd68->true-opnd68 loc68 sn))))
         (begin
           (move-opnd-to-loc68 opnd dtemp1 (sn-opnd loc sn))
           (emit-not.l dtemp1)
           (emit-and.w (make-imm -8) dtemp1)
           (move-opnd68-to-loc dtemp1 loc sn))))))
(define-apply "##FIXNUM.ASH" #f (gen-shift emit-asr.l))
(define-apply "##FIXNUM.LSH" #f (gen-shift emit-lsr.l))
(define-ifjump
 "##FIXNUM.ZERO?"
 (lambda (not? opnds lbl fs) (gen-eq-test 0 not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.POSITIVE?"
 (lambda (not? opnds lbl fs)
   (gen-compares
    emit-bgt
    emit-ble
    emit-blt
    emit-bge
    not?
    (list (car opnds) (make-obj '0))
    lbl
    fs)))
(define-ifjump
 "##FIXNUM.NEGATIVE?"
 (lambda (not? opnds lbl fs)
   (gen-compares
    emit-blt
    emit-bge
    emit-bgt
    emit-ble
    not?
    (list (car opnds) (make-obj '0))
    lbl
    fs)))
(define-ifjump
 "##FIXNUM.ODD?"
 (lambda (not? opnds lbl fs) (gen-even-test (not not?) opnds lbl fs)))
(define-ifjump
 "##FIXNUM.EVEN?"
 (lambda (not? opnds lbl fs) (gen-even-test not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.="
 (lambda (not? opnds lbl fs)
   (gen-compares emit-beq emit-bne emit-beq emit-bne not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.<"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-blt emit-bge emit-bgt emit-ble not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.>"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-bgt emit-ble emit-blt emit-bge not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.<="
 (lambda (not? opnds lbl fs)
   (gen-compares emit-ble emit-bgt emit-bge emit-blt not? opnds lbl fs)))
(define-ifjump
 "##FIXNUM.>="
 (lambda (not? opnds lbl fs)
   (gen-compares emit-bge emit-blt emit-ble emit-bgt not? opnds lbl fs)))
(define-apply
 "##FLONUM.->FIXNUM"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (move-opnd-to-loc68 (car opnds) atemp1 sn-loc)
     (let ((reg68 (if (and (reg? loc) (not (eq? loc return-reg)))
                      (reg->reg68 loc)
                      dtemp1)))
       (emit-fmov.dx (make-disp* atemp1 (- type-flonum)) ftemp1)
       (emit-fmov.l ftemp1 reg68)
       (emit-asl.l (make-imm 3) reg68)
       (if (not (and (reg? loc) (not (eq? loc return-reg))))
           (move-opnd68-to-loc reg68 loc sn))))))
(define-apply
 "##FLONUM.<-FIXNUM"
 #f
 (lambda (opnds loc sn)
   (gen-guarantee-space 2)
   (move-opnd-to-loc68
    (car opnds)
    dtemp1
    (sn-opnds (cdr opnds) (sn-opnd loc sn)))
   (emit-asr.l (make-imm 3) dtemp1)
   (emit-fmov.l dtemp1 ftemp1)
   (add-n-to-loc68 (* -2 pointer-size) heap-reg)
   (emit-fmov.dx ftemp1 (make-ind heap-reg))
   (let ((reg68 (if (reg? loc) (reg->reg68 loc) atemp1)))
     (emit-move.l heap-reg reg68)
     (emit-addq.l type-flonum reg68))
   (if (not (reg? loc)) (move-opnd68-to-loc atemp1 loc sn))))
(define-apply
 "##FLONUM.+"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj inexact-0) loc sn))
           ((null? (cdr opnds)) (copy-opnd-to-loc (car opnds) loc sn))
           (else (flo-oper emit-fmov.dx emit-fadd.dx opnds loc sn))))))
(define-apply
 "##FLONUM.*"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (cond ((null? opnds) (copy-opnd-to-loc (make-obj inexact-+1) loc sn))
           ((null? (cdr opnds)) (copy-opnd-to-loc (car opnds) loc sn))
           (else (flo-oper emit-fmov.dx emit-fmul.dx opnds loc sn))))))
(define-apply
 "##FLONUM.-"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (if (null? (cdr opnds))
         (flo-oper emit-fneg.dx #f opnds loc sn)
         (flo-oper emit-fmov.dx emit-fsub.dx opnds loc sn)))))
(define-apply
 "##FLONUM./"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (if (null? (cdr opnds))
         (flo-oper
          emit-fmov.dx
          emit-fdiv.dx
          (cons (make-obj inexact-+1) opnds)
          loc
          sn)
         (flo-oper emit-fmov.dx emit-fdiv.dx opnds loc sn)))))
(define-apply
 "##FLONUM.ABS"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fabs.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.TRUNCATE"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn)))
     (flo-oper emit-fintrz.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ROUND"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fint.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.EXP"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fetox.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.LOG"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-flogn.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.SIN"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fsin.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.COS"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fcos.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.TAN"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-ftan.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ASIN"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fasin.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ACOS"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-facos.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.ATAN"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fatan.dx #f opnds loc sn))))
(define-apply
 "##FLONUM.SQRT"
 #f
 (lambda (opnds loc sn)
   (let ((sn-loc (sn-opnd loc sn))) (flo-oper emit-fsqrt.dx #f opnds loc sn))))
(define-ifjump
 "##FLONUM.ZERO?"
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fbeq
    emit-fbne
    emit-fbeq
    emit-fbne
    not?
    (list (car opnds) (make-obj inexact-0))
    lbl
    fs)))
(define-ifjump
 "##FLONUM.NEGATIVE?"
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fblt
    emit-fbge
    emit-fbgt
    emit-fble
    not?
    (list (car opnds) (make-obj inexact-0))
    lbl
    fs)))
(define-ifjump
 "##FLONUM.POSITIVE?"
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fbgt
    emit-fble
    emit-fblt
    emit-fbge
    not?
    (list (car opnds) (make-obj inexact-0))
    lbl
    fs)))
(define-ifjump
 "##FLONUM.="
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fbeq
    emit-fbne
    emit-fbeq
    emit-fbne
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.<"
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fblt
    emit-fbge
    emit-fbgt
    emit-fble
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.>"
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fbgt
    emit-fble
    emit-fblt
    emit-fbge
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.<="
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fble
    emit-fbgt
    emit-fbge
    emit-fblt
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##FLONUM.>="
 (lambda (not? opnds lbl fs)
   (gen-compares-flo
    emit-fbge
    emit-fblt
    emit-fble
    emit-fbgt
    not?
    opnds
    lbl
    fs)))
(define-ifjump
 "##CHAR=?"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-beq emit-bne emit-beq emit-bne not? opnds lbl fs)))
(define-ifjump
 "##CHAR<?"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-blt emit-bge emit-bgt emit-ble not? opnds lbl fs)))
(define-ifjump
 "##CHAR>?"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-bgt emit-ble emit-blt emit-bge not? opnds lbl fs)))
(define-ifjump
 "##CHAR<=?"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-ble emit-bgt emit-bge emit-blt not? opnds lbl fs)))
(define-ifjump
 "##CHAR>=?"
 (lambda (not? opnds lbl fs)
   (gen-compares emit-bge emit-blt emit-ble emit-bgt not? opnds lbl fs)))
(define-apply "##CONS" #f (lambda (opnds loc sn) (gen-cons opnds loc sn)))
(define-apply
 "##SET-CAR!"
 #t
 (lambda (opnds loc sn) (gen-set-car! opnds loc sn)))
(define-apply
 "##SET-CDR!"
 #t
 (lambda (opnds loc sn) (gen-set-cdr! opnds loc sn)))
(define-apply "##CAR" #f (make-gen-apply-c...r 2))
(define-apply "##CDR" #f (make-gen-apply-c...r 3))
(define-apply "##CAAR" #f (make-gen-apply-c...r 4))
(define-apply "##CADR" #f (make-gen-apply-c...r 5))
(define-apply "##CDAR" #f (make-gen-apply-c...r 6))
(define-apply "##CDDR" #f (make-gen-apply-c...r 7))
(define-apply "##CAAAR" #f (make-gen-apply-c...r 8))
(define-apply "##CAADR" #f (make-gen-apply-c...r 9))
(define-apply "##CADAR" #f (make-gen-apply-c...r 10))
(define-apply "##CADDR" #f (make-gen-apply-c...r 11))
(define-apply "##CDAAR" #f (make-gen-apply-c...r 12))
(define-apply "##CDADR" #f (make-gen-apply-c...r 13))
(define-apply "##CDDAR" #f (make-gen-apply-c...r 14))
(define-apply "##CDDDR" #f (make-gen-apply-c...r 15))
(define-apply "##CAAAAR" #f (make-gen-apply-c...r 16))
(define-apply "##CAAADR" #f (make-gen-apply-c...r 17))
(define-apply "##CAADAR" #f (make-gen-apply-c...r 18))
(define-apply "##CAADDR" #f (make-gen-apply-c...r 19))
(define-apply "##CADAAR" #f (make-gen-apply-c...r 20))
(define-apply "##CADADR" #f (make-gen-apply-c...r 21))
(define-apply "##CADDAR" #f (make-gen-apply-c...r 22))
(define-apply "##CADDDR" #f (make-gen-apply-c...r 23))
(define-apply "##CDAAAR" #f (make-gen-apply-c...r 24))
(define-apply "##CDAADR" #f (make-gen-apply-c...r 25))
(define-apply "##CDADAR" #f (make-gen-apply-c...r 26))
(define-apply "##CDADDR" #f (make-gen-apply-c...r 27))
(define-apply "##CDDAAR" #f (make-gen-apply-c...r 28))
(define-apply "##CDDADR" #f (make-gen-apply-c...r 29))
(define-apply "##CDDDAR" #f (make-gen-apply-c...r 30))
(define-apply "##CDDDDR" #f (make-gen-apply-c...r 31))
(define-apply
 "##MAKE-CELL"
 #f
 (lambda (opnds loc sn) (gen-cons (list (car opnds) (make-obj '())) loc sn)))
(define-apply "##CELL-REF" #f (make-gen-apply-c...r 2))
(define-apply
 "##CELL-SET!"
 #t
 (lambda (opnds loc sn) (gen-set-car! opnds loc sn)))
(define-apply "##VECTOR" #f (make-gen-vector 'vector))
(define-apply "##VECTOR-LENGTH" #f (make-gen-vector-length 'vector))
(define-apply "##VECTOR-REF" #f (make-gen-vector-ref 'vector))
(define-apply "##VECTOR-SET!" #t (make-gen-vector-set! 'vector))
(define-apply "##VECTOR-SHRINK!" #t (make-gen-vector-shrink! 'vector))
(define-apply "##STRING" #f (make-gen-vector 'string))
(define-apply "##STRING-LENGTH" #f (make-gen-vector-length 'string))
(define-apply "##STRING-REF" #f (make-gen-vector-ref 'string))
(define-apply "##STRING-SET!" #t (make-gen-vector-set! 'string))
(define-apply "##STRING-SHRINK!" #t (make-gen-vector-shrink! 'string))
(define-apply "##VECTOR8" #f (make-gen-vector 'vector8))
(define-apply "##VECTOR8-LENGTH" #f (make-gen-vector-length 'vector8))
(define-apply "##VECTOR8-REF" #f (make-gen-vector-ref 'vector8))
(define-apply "##VECTOR8-SET!" #t (make-gen-vector-set! 'vector8))
(define-apply "##VECTOR8-SHRINK!" #t (make-gen-vector-shrink! 'vector8))
(define-apply "##VECTOR16" #f (make-gen-vector 'vector16))
(define-apply "##VECTOR16-LENGTH" #f (make-gen-vector-length 'vector16))
(define-apply "##VECTOR16-REF" #f (make-gen-vector-ref 'vector16))
(define-apply "##VECTOR16-SET!" #t (make-gen-vector-set! 'vector16))
(define-apply "##VECTOR16-SHRINK!" #t (make-gen-vector-shrink! 'vector16))
(define-apply "##CLOSURE-CODE" #f (make-gen-slot-ref 1 type-procedure))
(define-apply "##CLOSURE-REF" #f (make-gen-vector-ref 'closure))
(define-apply "##CLOSURE-SET!" #t (make-gen-vector-set! 'closure))
(define-apply
 "##SUBPROCEDURE-ID"
 #f
 (lambda (opnds loc sn) (gen-subprocedure-id opnds loc sn)))
(define-apply
 "##SUBPROCEDURE-PARENT"
 #f
 (lambda (opnds loc sn) (gen-subprocedure-parent opnds loc sn)))
(define-apply
 "##RETURN-FS"
 #f
 (lambda (opnds loc sn) (gen-return-fs opnds loc sn)))
(define-apply
 "##RETURN-LINK"
 #f
 (lambda (opnds loc sn) (gen-return-link opnds loc sn)))
(define-apply
 "##PROCEDURE-INFO"
 #f
 (lambda (opnds loc sn) (gen-procedure-info opnds loc sn)))
(define-apply
 "##PSTATE"
 #f
 (lambda (opnds loc sn) (move-opnd68-to-loc pstate-reg loc sn)))
(define-apply
 "##MAKE-PLACEHOLDER"
 #f
 (lambda (opnds loc sn) (gen-make-placeholder opnds loc sn)))
(define-apply
 "##TOUCH"
 #t
 (lambda (opnds loc sn)
   (let ((opnd (car opnds)))
     (if loc
         (touch-opnd-to-loc opnd loc sn)
         (touch-opnd-to-any-reg68 opnd sn)))))
(def-spec "NOT" (safe "##NOT"))
(def-spec "NULL?" (safe "##NULL?"))
(def-spec "EQ?" (safe "##EQ?"))
(def-spec "PAIR?" (safe "##PAIR?"))
(def-spec "PROCEDURE?" (safe "##PROCEDURE?"))
(def-spec "VECTOR?" (safe "##VECTOR?"))
(def-spec "SYMBOL?" (safe "##SYMBOL?"))
(def-spec "STRING?" (safe "##STRING?"))
(def-spec "CHAR?" (safe "##CHAR?"))
(def-spec "ZERO?" (safe-arith "##FIXNUM.ZERO?" "##FLONUM.ZERO?"))
(def-spec "POSITIVE?" (safe-arith "##FIXNUM.POSITIVE?" "##FLONUM.POSITIVE?"))
(def-spec "NEGATIVE?" (safe-arith "##FIXNUM.NEGATIVE?" "##FLONUM.NEGATIVE?"))
(def-spec "ODD?" (safe-arith "##FIXNUM.ODD?" #f))
(def-spec "EVEN?" (safe-arith "##FIXNUM.EVEN?" #f))
(def-spec "+" (unsafe-arith "##FIXNUM.+" "##FLONUM.+"))
(def-spec "*" (unsafe-arith "##FIXNUM.*" "##FLONUM.*"))
(def-spec "-" (unsafe-arith "##FIXNUM.-" "##FLONUM.-"))
(def-spec "/" (unsafe-arith #f "##FLONUM./"))
(def-spec "QUOTIENT" (unsafe-arith "##FIXNUM.QUOTIENT" #f))
(def-spec "REMAINDER" (unsafe-arith "##FIXNUM.REMAINDER" #f))
(def-spec "MODULO" (unsafe-arith "##FIXNUM.MODULO" #f))
(def-spec "=" (safe-arith "##FIXNUM.=" "##FLONUM.="))
(def-spec "<" (safe-arith "##FIXNUM.<" "##FLONUM.<"))
(def-spec ">" (safe-arith "##FIXNUM.>" "##FLONUM.>"))
(def-spec "<=" (safe-arith "##FIXNUM.<=" "##FLONUM.<="))
(def-spec ">=" (safe-arith "##FIXNUM.>=" "##FLONUM.>="))
(def-spec "ABS" (unsafe-arith #f "##FLONUM.ABS"))
(def-spec "TRUNCATE" (unsafe-arith #f "##FLONUM.TRUNCATE"))
(def-spec "EXP" (unsafe-arith #f "##FLONUM.EXP"))
(def-spec "LOG" (unsafe-arith #f "##FLONUM.LOG"))
(def-spec "SIN" (unsafe-arith #f "##FLONUM.SIN"))
(def-spec "COS" (unsafe-arith #f "##FLONUM.COS"))
(def-spec "TAN" (unsafe-arith #f "##FLONUM.TAN"))
(def-spec "ASIN" (unsafe-arith #f "##FLONUM.ASIN"))
(def-spec "ACOS" (unsafe-arith #f "##FLONUM.ACOS"))
(def-spec "ATAN" (unsafe-arith #f "##FLONUM.ATAN"))
(def-spec "SQRT" (unsafe-arith #f "##FLONUM.SQRT"))
(def-spec "CHAR=?" (safe "##CHAR=?"))
(def-spec "CHAR<?" (safe "##CHAR<?"))
(def-spec "CHAR>?" (safe "##CHAR>?"))
(def-spec "CHAR<=?" (safe "##CHAR<=?"))
(def-spec "CHAR>=?" (safe "##CHAR>=?"))
(def-spec "CONS" (safe "##CONS"))
(def-spec "SET-CAR!" (unsafe "##SET-CAR!"))
(def-spec "SET-CDR!" (unsafe "##SET-CDR!"))
(def-spec "CAR" (unsafe "##CAR"))
(def-spec "CDR" (unsafe "##CDR"))
(def-spec "CAAR" (unsafe "##CAAR"))
(def-spec "CADR" (unsafe "##CADR"))
(def-spec "CDAR" (unsafe "##CDAR"))
(def-spec "CDDR" (unsafe "##CDDR"))
(def-spec "CAAAR" (unsafe "##CAAAR"))
(def-spec "CAADR" (unsafe "##CAADR"))
(def-spec "CADAR" (unsafe "##CADAR"))
(def-spec "CADDR" (unsafe "##CADDR"))
(def-spec "CDAAR" (unsafe "##CDAAR"))
(def-spec "CDADR" (unsafe "##CDADR"))
(def-spec "CDDAR" (unsafe "##CDDAR"))
(def-spec "CDDDR" (unsafe "##CDDDR"))
(def-spec "CAAAAR" (unsafe "##CAAAAR"))
(def-spec "CAAADR" (unsafe "##CAAADR"))
(def-spec "CAADAR" (unsafe "##CAADAR"))
(def-spec "CAADDR" (unsafe "##CAADDR"))
(def-spec "CADAAR" (unsafe "##CADAAR"))
(def-spec "CADADR" (unsafe "##CADADR"))
(def-spec "CADDAR" (unsafe "##CADDAR"))
(def-spec "CADDDR" (unsafe "##CADDDR"))
(def-spec "CDAAAR" (unsafe "##CDAAAR"))
(def-spec "CDAADR" (unsafe "##CDAADR"))
(def-spec "CDADAR" (unsafe "##CDADAR"))
(def-spec "CDADDR" (unsafe "##CDADDR"))
(def-spec "CDDAAR" (unsafe "##CDDAAR"))
(def-spec "CDDADR" (unsafe "##CDDADR"))
(def-spec "CDDDAR" (unsafe "##CDDDAR"))
(def-spec "CDDDDR" (unsafe "##CDDDDR"))
(def-spec "VECTOR" (safe "##VECTOR"))
(def-spec "VECTOR-LENGTH" (unsafe "##VECTOR-LENGTH"))
(def-spec "VECTOR-REF" (unsafe "##VECTOR-REF"))
(def-spec "VECTOR-SET!" (unsafe "##VECTOR-SET!"))
(def-spec "STRING" (safe "##STRING"))
(def-spec "STRING-LENGTH" (unsafe "##STRING-LENGTH"))
(def-spec "STRING-REF" (unsafe "##STRING-REF"))
(def-spec "STRING-SET!" (unsafe "##STRING-SET!"))
(def-spec "TOUCH" (safe "##TOUCH"))
(let ((targ (make-target 4 'm68000)))
  (target-begin!-set! targ (lambda (info-port) (begin! info-port targ)))
  (put-target targ))

(define input-source-code '
(begin
(declare (standard-bindings) (fixnum) (not safe) (block))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(define (create-x n)
  (define result (make-vector n))
  (do ((i 0 (+ i 1)))
      ((>= i n) result)
    (vector-set! result i i)))

(define (create-y x)
  (let* ((n (vector-length x))
         (result (make-vector n)))
    (do ((i (- n 1) (- i 1)))
        ((< i 0) result)
      (vector-set! result i (vector-ref x i)))))

(define (my-try n)
  (vector-length (create-y (create-x n))))

(define (go n)
  (let loop ((repeat 100)
             (result 0))
    (if (> repeat 0)
        (loop (- repeat 1) (my-try n))
        result)))

(+ (fib 20)
   (tak 18 12 6)
   (ack 3 9)
   (go 200000))
))

(define output-expected '(
"|------------------------------------------------------"
"| #[primitive #!program] ="
"L1:"
" cmpw #1,d0"
" beq L1000"
" TRAP1(9,0)"
" LBL_PTR(L1)"
"L1000:"
" MOVE_PROC(1,a1)"
" movl a1,GLOB(fib)"
" MOVE_PROC(2,a1)"
" movl a1,GLOB(tak)"
" MOVE_PROC(3,a1)"
" movl a1,GLOB(ack)"
" MOVE_PROC(4,a1)"
" movl a1,GLOB(create-x)"
" MOVE_PROC(5,a1)"
" movl a1,GLOB(create-y)"
" MOVE_PROC(6,a1)"
" movl a1,GLOB(my-try)"
" MOVE_PROC(7,a1)"
" movl a1,GLOB(go)"
" movl a0,sp@-"
" movl #160,d1"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
"L1001:"
" JMP_PROC(1,10)"
" RETURN(L1,1,1)"
"L2:"
" movl d1,sp@-"
" moveq #48,d3"
" moveq #96,d2"
" movl #144,d1"
" lea L3,a0"
" JMP_PROC(2,14)"
" RETURN(L1,2,1)"
"L3:"
" movl d1,sp@-"
" moveq #72,d2"
" moveq #24,d1"
" lea L4,a0"
" JMP_PROC(3,10)"
" RETURN(L1,3,1)"
"L4:"
" movl d1,sp@-"
" movl #1600000,d1"
" lea L5,a0"
" JMP_PROC(7,10)"
" RETURN(L1,4,1)"
"L5:"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,4,1)"
"L1004:"
"L1003:"
"L6:"
" addl sp@(8),d1"
" addl sp@(4),d1"
" addl sp@+,d1"
" addql #8,sp"
" rts"
"L0:"
"|------------------------------------------------------"
"| #[primitive fib] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" moveq #16,d0"
" cmpl d1,d0"
" ble L3"
" bra L4"
" RETURN(L1,2,1)"
"L2:"
" movl d1,sp@-"
" movl sp@(4),d1"
" moveq #-16,d0"
" addl d0,d1"
" lea L5,a0"
" moveq #16,d0"
" cmpl d1,d0"
" bgt L4"
"L3:"
" movl a0,sp@-"
" movl d1,sp@-"
" subql #8,d1"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1002:"
"L1001:"
" moveq #16,d0"
" cmpl d1,d0"
" ble L3"
"L4:"
" jmp a0@"
" RETURN(L1,3,1)"
"L5:"
" addl sp@+,d1"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1004:"
"L1003:"
" addql #4,sp"
" rts"
"L0:"
"|------------------------------------------------------"
"| #[primitive tak] ="
"L1:"
" cmpw #4,d0"
" beq L1000"
" TRAP1(9,3)"
" LBL_PTR(L1)"
"L1000:"
" cmpl d1,d2"
" bge L4"
" bra L3"
" RETURN(L1,6,1)"
"L2:"
" movl d1,d3"
" movl sp@(20),a0"
" movl sp@+,d2"
" movl sp@+,d1"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" movl a0,sp@(12)"
" TRAP2(24)"
" RETURN(L1,4,1)"
"L1002:"
" movl sp@(12),a0"
"L1001:"
" cmpl d1,d2"
" lea sp@(16),sp"
" bge L4"
"L3:"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,sp@-"
" movl d3,sp@-"
" subql #8,d1"
" lea L5,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,4,1)"
"L1004:"
"L1003:"
" cmpl d1,d2"
" blt L3"
"L4:"
" movl d3,d1"
" jmp a0@"
" RETURN(L1,4,1)"
"L5:"
" movl d1,sp@-"
" movl sp@(12),d3"
" movl sp@(4),d2"
" movl sp@(8),d1"
" subql #8,d1"
" lea L6,a0"
" cmpl d1,d2"
" bge L4"
" bra L3"
" RETURN(L1,5,1)"
"L6:"
" movl d1,sp@-"
" movl sp@(12),d3"
" movl sp@(16),d2"
" movl sp@(8),d1"
" subql #8,d1"
" lea L2,a0"
" cmpl d1,d2"
" bge L4"
" bra L3"
"L0:"
"|------------------------------------------------------"
"| #[primitive ack] ="
"L1:"
" beq L1000"
" TRAP1(9,2)"
" LBL_PTR(L1)"
"L1000:"
" movl d1,d0"
" bne L3"
" bra L5"
" RETURN(L1,2,1)"
"L2:"
" movl d1,d2"
" movl sp@+,d1"
" subql #8,d1"
" movl sp@+,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
" movl sp@+,a0"
"L1001:"
" movl d1,d0"
" beq L5"
"L3:"
" movl d2,d0"
" bne L6"
"L4:"
" subql #8,d1"
" moveq #8,d2"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" movl d1,d0"
" bne L3"
"L5:"
" movl d2,d1"
" addql #8,d1"
" jmp a0@"
"L6:"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,d1"
" subql #8,d1"
" movl d1,d2"
" movl sp@,d1"
" lea L2,a0"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1006:"
"L1005:"
" movl d1,d0"
" bne L3"
" bra L5"
"L0:"
"|------------------------------------------------------"
"| #[primitive create-x] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" movl a0,sp@-"
" movl d1,sp@-"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,2,1)"
"L1002:"
"L1001:"
" moveq #-1,d0"
" JMP_PRIM(make-vector,0)"
" RETURN(L1,2,1)"
"L2:"
" movl d1,d2"
" movl sp@+,d1"
" moveq #0,d3"
" movl sp@+,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" cmpl d1,d3"
" bge L4"
"L3:"
" movl d3,d0"
" asrl #1,d0"
" movl d2,a1"
" movl d3,a1@(1,d0:l)"
" addql #8,d3"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1006:"
" movl sp@+,a0"
"L1005:"
" cmpl d1,d3"
" blt L3"
"L4:"
" movl d2,d1"
" jmp a0@"
"L0:"
"|------------------------------------------------------"
"| #[primitive create-y] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" movl d1,a1"
" movl a1@(-3),d2"
" lsrl #7,d2"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,sp@-"
" movl d2,d1"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,3,1)"
"L1002:"
"L1001:"
" moveq #-1,d0"
" JMP_PRIM(make-vector,0)"
" RETURN(L1,3,1)"
"L2:"
" movl sp@+,d2"
" subql #8,d2"
" movl d2,d3"
" movl d1,d2"
" movl sp@+,d1"
" movl sp@+,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" movl d3,d0"
" blt L4"
"L3:"
" movl d3,d0"
" asrl #1,d0"
" movl d1,a1"
" movl a1@(1,d0:l),d4"
" movl d3,d0"
" asrl #1,d0"
" movl d2,a1"
" movl d4,a1@(1,d0:l)"
" subql #8,d3"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1006:"
" movl sp@+,a0"
"L1005:"
" movl d3,d0"
" bge L3"
"L4:"
" movl d2,d1"
" jmp a0@"
"L0:"
"|------------------------------------------------------"
"| #[primitive my-try] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" movl a0,sp@-"
" lea L2,a0"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
"L1001:"
" JMP_PROC(4,10)"
" RETURN(L1,1,1)"
"L2:"
" lea L3,a0"
" JMP_PROC(5,10)"
" RETURN(L1,1,1)"
"L3:"
" movl d1,a1"
" movl a1@(-3),d1"
" lsrl #7,d1"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
"L1003:"
" rts"
"L0:"
"|------------------------------------------------------"
"| #[primitive go] ="
"L1:"
" bmi L1000"
" TRAP1(9,1)"
" LBL_PTR(L1)"
"L1000:"
" moveq #0,d3"
" movl #800,d2"
" dbra d5,L1001"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1001"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1002:"
" movl sp@+,a0"
"L1001:"
" movl d2,d0"
" ble L4"
" bra L3"
" RETURN(L1,3,1)"
"L2:"
" movl d1,d3"
" movl sp@+,d1"
" subql #8,d1"
" movl d1,d2"
" movl sp@+,d1"
" movl sp@+,a0"
" dbra d5,L1003"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1003"
" movl a0,sp@-"
" TRAP2(24)"
" RETURN(L1,1,1)"
"L1004:"
" movl sp@+,a0"
"L1003:"
" movl d2,d0"
" ble L4"
"L3:"
" movl a0,sp@-"
" movl d1,sp@-"
" movl d2,sp@-"
" lea L2,a0"
" dbra d5,L1005"
" moveq #9,d5"
" cmpl a5@,sp"
" bcc L1005"
" TRAP2(24)"
" RETURN(L1,3,1)"
"L1006:"
"L1005:"
" JMP_PROC(6,10)"
"L4:"
" movl d3,d1"
" jmp a0@"
"L0:"
""))

(define (main-compiler . args)
  (run-single-benchmark
    "compiler"
    compiler-iters
    (lambda (result)
      (equal? result output-expected))
    (lambda (expr target opt) (lambda () (ce expr target opt) (asm-output-get)))
    input-source-code
    'm68000
    'asm))

;;; CONFORM -- Type checker, written by Jim Miller.

;;; Functional and unstable

(define (conform-sort-list obj pred)

  (define (loop l)
    (if (and (pair? l) (pair? (cdr l)))
        (split-list l '() '())
        l))

  (define (split-list l one two)
    (if (pair? l)
        (split-list (cdr l) two (cons (car l) one))
        (merge (loop one) (loop two))))

  (define (merge one two)
    (cond ((null? one) two)
          ((pred (car two) (car one))
           (cons (car two)
                 (merge (cdr two) one)))
          (else
           (cons (car one)
                 (merge (cdr one) two)))))

  (loop obj))

;; SET OPERATIONS
; (representation as lists with distinct elements)

(define (adjoin element set)
  (if (memq element set) set (cons element set)))

(define (eliminate element set)
  (cond ((null? set) set)
        ((eq? element (car set)) (cdr set))
        (else (cons (car set) (eliminate element (cdr set))))))

(define (intersect list1 list2)
  (let loop ((l list1))
    (cond ((null? l) '())
          ((memq (car l) list2) (cons (car l) (loop (cdr l))))
          (else (loop (cdr l))))))

(define (union list1 list2)
  (if (null? list1)
      list2
      (union (cdr list1)
             (adjoin (car list1) list2))))

;; GRAPH NODES

(define make-internal-node vector)
(define (internal-node-name node) (vector-ref node 0))
(define (internal-node-green-edges node) (vector-ref node 1))
(define (internal-node-red-edges node) (vector-ref node 2))
(define (internal-node-blue-edges node) (vector-ref node 3))
(define (set-internal-node-name! node name) (vector-set! node 0 name))
(define (set-internal-node-green-edges! node edges) (vector-set! node 1 edges))
(define (set-internal-node-red-edges! node edges) (vector-set! node 2 edges))
(define (set-internal-node-blue-edges! node edges) (vector-set! node 3 edges))

(define (make-node name . blue-edges)   ; User's constructor
  (let ((name (if (symbol? name) (symbol->string name) name))
        (blue-edges (if (null? blue-edges) 'NOT-A-NODE-YET (car blue-edges))))
    (make-internal-node name '() '() blue-edges)))

(define (copy-node node)
  (make-internal-node (name node) '() '() (blue-edges node)))

; Selectors

(define name internal-node-name)
(define (make-edge-getter selector)
  (lambda (node)
    (if (or (none-node? node) (any-node? node))
        (fatal-error "Can't get edges from the ANY or NONE nodes")
        (selector node))))
(define red-edges (make-edge-getter internal-node-red-edges))
(define green-edges (make-edge-getter internal-node-green-edges))
(define blue-edges (make-edge-getter internal-node-blue-edges))

; Mutators

(define (make-edge-setter mutator!)
  (lambda (node value)
    (cond ((any-node? node) (fatal-error "Can't set edges from the ANY node"))
          ((none-node? node) 'OK)
          (else (mutator! node value)))))
(define set-red-edges! (make-edge-setter set-internal-node-red-edges!))
(define set-green-edges! (make-edge-setter set-internal-node-green-edges!))
(define set-blue-edges! (make-edge-setter set-internal-node-blue-edges!))

;; BLUE EDGES

(define make-blue-edge vector)
(define (blue-edge-operation edge) (vector-ref edge 0))
(define (blue-edge-arg-node edge) (vector-ref edge 1))
(define (blue-edge-res-node edge) (vector-ref edge 2))
(define (set-blue-edge-operation! edge value) (vector-set! edge 0 value))
(define (set-blue-edge-arg-node! edge value) (vector-set! edge 1 value))
(define (set-blue-edge-res-node! edge value) (vector-set! edge 2 value))

; Selectors
(define operation blue-edge-operation)
(define arg-node blue-edge-arg-node)
(define res-node blue-edge-res-node)

; Mutators
(define set-arg-node! set-blue-edge-arg-node!)
(define set-res-node! set-blue-edge-res-node!)

; Higher level operations on blue edges

(define (lookup-op op node)
  (let loop ((edges (blue-edges node)))
    (cond ((null? edges) '())
          ((eq? op (operation (car edges))) (car edges))
          (else (loop (cdr edges))))))

(define (has-op? op node)
  (not (null? (lookup-op op node))))

;; GRAPHS

(define make-internal-graph vector)
(define (internal-graph-nodes graph) (vector-ref graph 0))
(define (internal-graph-already-met graph) (vector-ref graph 1))
(define (internal-graph-already-joined graph) (vector-ref graph 2))
(define (set-internal-graph-nodes! graph nodes) (vector-set! graph 0 nodes))

; Constructor

(define (make-graph . nodes)
  (make-internal-graph nodes (make-empty-table) (make-empty-table)))

; Selectors

(define graph-nodes internal-graph-nodes)
(define already-met internal-graph-already-met)
(define already-joined internal-graph-already-joined)

; Higher level functions on graphs

(define (add-graph-nodes! graph nodes)
  (set-internal-graph-nodes! graph (cons nodes (graph-nodes graph))))

(define (copy-graph g)
  (define (copy-list l) (vector->list (list->vector l)))
  (make-internal-graph
   (copy-list (graph-nodes g))
   (already-met g)
   (already-joined g)))

(define (clean-graph g)
  (define (clean-node node)
    (if (not (or (any-node? node) (none-node? node)))
        (begin
          (set-green-edges! node '())
          (set-red-edges! node '()))))
  (for-each clean-node (graph-nodes g))
  g)

(define (canonicalize-graph graph classes)
  (define (fix node)
    (define (fix-set object selector mutator)
      (mutator object
               (map (lambda (node)
                      (find-canonical-representative node classes))
                    (selector object))))
    (if (not (or (none-node? node) (any-node? node)))
        (begin
          (fix-set node green-edges set-green-edges!)
          (fix-set node red-edges set-red-edges!)
          (for-each
           (lambda (blue-edge)
             (set-arg-node! blue-edge
                            (find-canonical-representative (arg-node blue-edge) classes))
             (set-res-node! blue-edge
                            (find-canonical-representative (res-node blue-edge) classes)))
           (blue-edges node))))
    node)
  (define (fix-table table)
    (define (canonical? node) (eq? node (find-canonical-representative node classes)))
    (define (filter-and-fix predicate-fn update-fn list)
      (let loop ((list list))
        (cond ((null? list) '())
              ((predicate-fn (car list))
               (cons (update-fn (car list)) (loop (cdr list))))
              (else (loop (cdr list))))))
    (define (fix-line line)
      (filter-and-fix
       (lambda (entry) (canonical? (car entry)))
       (lambda (entry) (cons (car entry)
                             (find-canonical-representative (cdr entry) classes)))
       line))
    (if (null? table)
        '()
        (cons (car table)
              (filter-and-fix
               (lambda (entry) (canonical? (car entry)))
               (lambda (entry) (cons (car entry) (fix-line (cdr entry))))
               (cdr table)))))
  (make-internal-graph
   (map (lambda (class) (fix (car class))) classes)
   (fix-table (already-met graph))
   (fix-table (already-joined graph))))

;; USEFUL NODES

(define none-node (make-node 'none #t))
(define (none-node? node) (eq? node none-node))

(define any-node (make-node 'any '()))
(define (any-node? node) (eq? node any-node))

;; COLORED EDGE TESTS

(define (green-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
        ((none-node? from-node) #t)
        ((memq to-node (green-edges from-node)) #t)
        (else #f)))

(define (red-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
        ((none-node? from-node) #t)
        ((memq to-node (red-edges from-node)) #t)
        (else #f)))

;; SIGNATURE

; Return signature (i.e. <arg, res>) given an operation and a node

(define sig
  (let ((none-comma-any (cons none-node any-node)))
    (lambda (op node)                   ; Returns (arg, res)
      (let ((the-edge (lookup-op op node)))
        (if (not (null? the-edge))
            (cons (arg-node the-edge) (res-node the-edge))
            none-comma-any)))))

; Selectors from signature

(define (arg pair) (car pair))
(define (res pair) (cdr pair))

;; CONFORMITY

(define (conforms? t1 t2)
  (define nodes-with-red-edges-out '())
  (define (add-red-edge! from-node to-node)
    (set-red-edges! from-node (adjoin to-node (red-edges from-node)))
    (set! nodes-with-red-edges-out
          (adjoin from-node nodes-with-red-edges-out)))
  (define (greenify-red-edges! from-node)
    (set-green-edges! from-node
                      (append (red-edges from-node) (green-edges from-node)))
    (set-red-edges! from-node '()))
  (define (delete-red-edges! from-node)
    (set-red-edges! from-node '()))
  (define (does-conform t1 t2)
    (cond ((or (none-node? t1) (any-node? t2)) #t)
          ((or (any-node? t1) (none-node? t2)) #f)
          ((green-edge? t1 t2) #t)
          ((red-edge? t1 t2) #t)
          (else
           (add-red-edge! t1 t2)
           (let loop ((blues (blue-edges t2)))
             (if (null? blues)
                 #t
                 (let* ((current-edge (car blues))
                        (phi (operation current-edge)))
                   (and (has-op? phi t1)
                        (does-conform
                         (res (sig phi t1))
                         (res (sig phi t2)))
                        (does-conform
                         (arg (sig phi t2))
                         (arg (sig phi t1)))
                        (loop (cdr blues)))))))))
  (let ((result (does-conform t1 t2)))
    (for-each (if result greenify-red-edges! delete-red-edges!)
              nodes-with-red-edges-out)
    result))

(define (equivalent? a b)
  (and (conforms? a b) (conforms? b a)))

;; EQUIVALENCE CLASSIFICATION
; Given a list of nodes, return a list of equivalence classes

(define (classify nodes)
  (let node-loop ((classes '())
                  (nodes nodes))
    (if (null? nodes)
        (map (lambda (class)
               (conform-sort-list class
                          (lambda (node1 node2)
                            (< (string-length (name node1))
                               (string-length (name node2))))))
             classes)
        (let ((this-node (car nodes)))
          (define (add-node classes)
            (cond ((null? classes) (list (list this-node)))
                  ((equivalent? this-node (caar classes))
                   (cons (cons this-node (car classes))
                         (cdr classes)))
                  (else (cons (car classes)
                              (add-node (cdr classes))))))
          (node-loop (add-node classes)
                     (cdr nodes))))))

; Given a node N and a classified set of nodes,
; find the canonical member corresponding to N

(define (find-canonical-representative element classification)
  (let loop ((classes classification))
    (cond ((null? classes) (fatal-error "Can't classify" element))
          ((memq element (car classes)) (car (car classes)))
          (else (loop (cdr classes))))))

; Reduce a graph by taking only one member of each equivalence
; class and canonicalizing all outbound pointers

(define (reduce graph)
  (let ((classes (classify (graph-nodes graph))))
    (canonicalize-graph graph classes)))

;; TWO DIMENSIONAL TABLES

(define (make-empty-table) (list 'TABLE))
(define (lookup table x y)
  (let ((one (assq x (cdr table))))
    (if one
        (let ((two (assq y (cdr one))))
          (if two (cdr two) #f))
        #f)))
(define (insert! table x y value)
  (define (make-singleton-table x y)
    (list (cons x y)))
  (let ((one (assq x (cdr table))))
    (if one
        (set-cdr! one (cons (cons y value) (cdr one)))
        (set-cdr! table (cons (cons x (make-singleton-table y value))
                              (cdr table))))))

;; MEET/JOIN
; These update the graph when computing the node for node1*node2

(define (blue-edge-operate arg-fn res-fn graph op sig1 sig2)
  (make-blue-edge op
                  (arg-fn graph (arg sig1) (arg sig2))
                  (res-fn graph (res sig1) (res sig2))))

(define (meet graph node1 node2)
  (cond ((eq? node1 node2) node1)
        ((or (any-node? node1) (any-node? node2)) any-node) ; canonicalize
        ((none-node? node1) node2)
        ((none-node? node2) node1)
        ((lookup (already-met graph) node1 node2)) ; return it if found
        ((conforms? node1 node2) node2)
        ((conforms? node2 node1) node1)
        (else
         (let ((result
                (make-node (string-append "(" (name node1) " ^ " (name node2) ")"))))
           (add-graph-nodes! graph result)
           (insert! (already-met graph) node1 node2 result)
           (set-blue-edges! result
             (map
              (lambda (op)
                (blue-edge-operate join meet graph op (sig op node1) (sig op node2)))
              (intersect (map operation (blue-edges node1))
                         (map operation (blue-edges node2)))))
           result))))

(define (join graph node1 node2)
  (cond ((eq? node1 node2) node1)
        ((any-node? node1) node2)
        ((any-node? node2) node1)
        ((or (none-node? node1) (none-node? node2)) none-node) ; canonicalize
        ((lookup (already-joined graph) node1 node2)) ; return it if found
        ((conforms? node1 node2) node1)
        ((conforms? node2 node1) node2)
        (else
         (let ((result
                (make-node (string-append "(" (name node1) " v " (name node2) ")"))))
           (add-graph-nodes! graph result)
           (insert! (already-joined graph) node1 node2 result)
           (set-blue-edges! result
             (map
              (lambda (op)
                (blue-edge-operate meet join graph op (sig op node1) (sig op node2)))
              (union (map operation (blue-edges node1))
                     (map operation (blue-edges node2)))))
           result))))

;; MAKE A LATTICE FROM A GRAPH

(define (conform-make-lattice g print?)
  (define (step g)
    (let* ((copy (copy-graph g))
           (nodes (graph-nodes copy)))
      (for-each (lambda (first)
                  (for-each (lambda (second)
                              (meet copy first second) (join copy first second))
                            nodes))
                nodes)
      copy))
  (define (loop g count)
    (if print? (display count))
    (let ((lattice (step g)))
      (if print? (begin (display " -> ") (display (length (graph-nodes lattice)))))
      (let* ((new-g (reduce lattice))
             (new-count (length (graph-nodes new-g))))
        (if (= new-count count)
            (begin
              (if print? (newline))
              new-g)
            (begin
              (if print? (begin (display " -> ") (display new-count) (newline)))
              (loop new-g new-count))))))
  (let ((graph
         (apply make-graph
                (adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))))
    (loop graph (length (graph-nodes graph)))))

;; DEBUG and TEST

(define a '())
(define b '())
(define c '())
(define d '())

(define (setup)
  (set! a (make-node 'a))
  (set! b (make-node 'b))
  (set-blue-edges! a (list (make-blue-edge 'phi any-node b)))
  (set-blue-edges! b (list (make-blue-edge 'phi any-node a)
                           (make-blue-edge 'theta any-node b)))
  (set! c (make-node "c"))
  (set! d (make-node "d"))
  (set-blue-edges! c (list (make-blue-edge 'theta any-node b)))
  (set-blue-edges! d (list (make-blue-edge 'phi any-node c)
                           (make-blue-edge 'theta any-node d)))
  '(made a b c d))

(define (conform-test)
  (setup)
  (map name
       (graph-nodes (conform-make-lattice (make-graph a b c d any-node none-node) #f))))

(define (main-conform . args)
  (run-single-benchmark
    "conform"
    conform-iters
    (lambda (result)
      (equal? (map (lambda (s)
                     (list->string (map char-downcase (string->list s))))
                   result)
              '("(((b v d) ^ a) v c)"
                "(c ^ d)"
                "(b v (a ^ d))"
                "((a v d) ^ b)"
                "(b v d)"
                "(b ^ (a v c))"
                "(a v (c ^ d))"
                "((b v d) ^ a)"
                "(c v (a v d))"
                "(a v c)"
                "(d v (b ^ (a v c)))"
                "(d ^ (a v c))"
                "((a ^ d) v c)"
                "((a ^ b) v d)"
                "(((a v d) ^ b) v (a ^ d))"
                "(b ^ d)"
                "(b v (a v d))"
                "(a ^ c)"
                "(b ^ (c v d))"
                "(a ^ b)"
                "(a v b)"
                "((a ^ d) ^ b)"
                "(a ^ d)"
                "(a v d)"
                "d"
                "(c v d)"
                "a"
                "b"
                "c"
                "any"
                "none")))
    (lambda () (lambda () (conform-test)))))


;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)

  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))))

  (tak x y z (lambda (a) a)))

(define (main-cpstak . args)
  (run-single-benchmark
    "cpstak"
    cpstak-iters
    (lambda (result) (equal? result 7))
    (lambda (x y z) (lambda () (cpstak x y z)))
    18
    12
    6))


;;; DDERIV -- Table-driven symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define (dderiv-lookup key table)
  (let loop ((x table))
    (if (null? x)
      #f
      (let ((pair (car x)))
        (if (eq? (car pair) key)
          pair
          (loop (cdr x)))))))

(define properties '())

(define (get key1 key2)
  (let ((x (dderiv-lookup key1 properties)))
    (if x
      (let ((y (dderiv-lookup key2 (cdr x))))
        (if y
          (cdr y)
          #f))
      #f)))

(define (put key1 key2 val)
  (let ((x (dderiv-lookup key1 properties)))
    (if x
      (let ((y (dderiv-lookup key2 (cdr x))))
        (if y
          (set-cdr! y val)
          (set-cdr! x (cons (cons key2 val) (cdr x)))))
      (set! properties
        (cons (list key1 (cons key2 val)) properties)))))

(define (my+dderiv a)
  (cons '+
        (map dderiv (cdr a))))

(define (my-dderiv a)
  (cons '-
        (map dderiv (cdr a))))

(define (*dderiv a)
  (list '*
         a
         (cons '+
               (map (lambda (a) (list '/ (dderiv a) a)) (cdr a)))))

(define (/dderiv a)
  (list '-
        (list '/
              (dderiv (cadr a))
              (caddr a))
        (list '/
              (cadr a)
              (list '*
                    (caddr a)
                    (caddr a)
                    (dderiv (caddr a))))))

(put '+ 'dderiv my+dderiv)
(put '- 'dderiv my-dderiv)
(put '* 'dderiv *dderiv)
(put '/ 'dderiv /dderiv)

(define (dderiv a)
  (if (not (pair? a))
    (if (eq? a 'x) 1 0)
    (let ((f (get (car a) 'dderiv)))
      (if f
        (f a)
        (fatal-error "No derivation method available")))))

(define (main-dderiv . args)
  (run-single-benchmark
    "dderiv"
    dderiv-iters
    (lambda (result)
      (equal? result
              '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
                  (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
                  (* (* b x) (+ (/ 0 b) (/ 1 x)))
                  0)))
    (lambda (a) (lambda () (dderiv a)))
    '(+ (* 3 x x) (* a x x) (* b x) 5)))


;;; DERIV -- Symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.

(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+
               (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '-
               (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
                a
                (cons '+
                      (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         (fatal-error "No derivation method available"))))

(define (main-deriv . args)
  (run-single-benchmark
    "deriv"
    deriv-iters
    (lambda (result)
      (equal? result
              '(+ (* (* 3 x x) (+ (/ 0 3) (/ 1 x) (/ 1 x)))
                  (* (* a x x) (+ (/ 0 a) (/ 1 x) (/ 1 x)))
                  (* (* b x) (+ (/ 0 b) (/ 1 x)))
                  0)))
    (lambda (a) (lambda () (deriv a)))
    '(+ (* 3 x x) (* a x x) (* b x) 5)))


;;; DESTRUC -- Destructive operation benchmark.

(define (destruc-append-to-tail! x y)
  (if (null? x)
    y
    (let loop ((a x) (b (cdr x)))
      (if (null? b)
        (begin
          (set-cdr! a y)
          x)
        (loop b (cdr b))))))

(define (destructive n m)
  (let ((l (do ((i 10 (- i 1)) (a '() (cons '() a)))
               ((= i 0) a))))
    (do ((i n (- i 1)))
        ((= i 0) l)
      (cond ((null? (car l))
             (do ((l l (cdr l)))
                 ((null? l))
               (if (null? (car l)) (set-car! l (cons '() '())))
               (destruc-append-to-tail! (car l)
                                (do ((j m (- j 1)) (a '() (cons '() a)))
                                  ((= j 0) a)))))
            (else
             (do ((l1 l (cdr l1)) (l2 (cdr l) (cdr l2)))
                 ((null? l2))
               (set-cdr! (do ((j (quotient (length (car l2)) 2) (- j 1))
                              (a (car l2) (cdr a)))
                             ((zero? j) a)
                           (set-car! a i))
                         (let ((n (quotient (length (car l1)) 2)))
                           (cond ((= n 0)
                                  (set-car! l1 '())
                                  (car l1))
                                 (else
                                  (do ((j n (- j 1)) (a (car l1) (cdr a)))
                                      ((= j 1)
                                       (let ((x (cdr a)))
                                         (set-cdr! a '())
                                         x))
                                    (set-car! a i))))))))))))

(define (main-destruc . args)
  (run-single-benchmark
    "destruc"
    destruc-iters
    (lambda (result)
      (equal? result
              '((1 1 2)
                (1 1 1)
                (1 1 1 2)
                (1 1 1 1)
                (1 1 1 1 2)
                (1 1 1 1 2)
                (1 1 1 1 2)
                (1 1 1 1 2)
                (1 1 1 1 2)
                (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 3))))
    (lambda (n m) (lambda () (destructive n m)))
    600
    50))


;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

(define (create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))

(define diviter-*ll* (create-n 200))

(define (iterative-div2 l)
  (do ((l l (cddr l))
       (a '() (cons (car l) a)))
      ((null? l) a)))

(define (main-diviter . args)
  (run-single-benchmark
    "diviter"
    diviter-iters
    (lambda (result)
      (equal? result
              '(() () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ())))
    (lambda (l) (lambda () (iterative-div2 l)))
    diviter-*ll*))


;;; DIVREC -- Benchmark which divides by 2 using lists of n ()'s.

(define (divrec-create-n n)
  (do ((n n (- n 1))
       (a '() (cons '() a)))
      ((= n 0) a)))

(define *ll* (divrec-create-n 200))

(define (recursive-div2 l)
  (cond ((null? l) '())
        (else (cons (car l) (recursive-div2 (cddr l))))))

(define (main-divrec . args)
  (run-single-benchmark
    "divrec"
    divrec-iters
    (lambda (result)
      (equal? result
              '(() () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ()
                () () () () () () () () () () () () () () () () () () () ())))
    (lambda (l) (lambda () (recursive-div2 l)))
    *ll*))


;;; EARLEY -- Earley's parser, written by Marc Feeley.

; (make-parser grammar lexer) is used to create a parser from the grammar
; description `grammar' and the lexer function `lexer'.
;
; A grammar is a list of definitions.  Each definition defines a non-terminal
; by a set of rules.  Thus a definition has the form: (nt rule1 rule2...).
; A given non-terminal can only be defined once.  The first non-terminal
; defined is the grammar's goal.  Each rule is a possibly empty list of
; non-terminals.  Thus a rule has the form: (nt1 nt2...).  A non-terminal
; can be any scheme value.  Note that all grammar symbols are treated as
; non-terminals.  This is fine though because the lexer will be outputing
; non-terminals.
;
; The lexer defines what a token is and the mapping between tokens and
; the grammar's non-terminals.  It is a function of one argument, the input,
; that returns the list of tokens corresponding to the input.  Each token is
; represented by a list.  The first element is some `user-defined' information
; associated with the token and the rest represents the token's class(es) (as a
; list of non-terminals that this token corresponds to).
;
; The result of `make-parser' is a function that parses the single input it
; is given into the grammar's goal.  The result is a `parse' which can be
; manipulated with the procedures: `parse->parsed?', `parse->trees'
; and `parse->nb-trees' (see below).
;
; Let's assume that we want a parser for the grammar
;
;  S -> x = E
;  E -> E + E | V
;  V -> V y |
;
; and that the input to the parser is a string of characters.  Also, assume we
; would like to map the characters `x', `y', `+' and `=' into the corresponding
; non-terminals in the grammar.  Such a parser could be created with
;
; (make-parser
;   '(
;      (s (x = e))
;      (e (e + e) (v))
;      (v (v y) ())
;    )
;   (lambda (str)
;     (map (lambda (char)
;            (list char ; user-info = the character itself
;                  (case char
;                    ((#\x) 'x)
;                    ((#\y) 'y)
;                    ((#\+) '+)
;                    ((#\=) '=)
;                    (else (fatal-error "lexer error")))))
;          (string->list str)))
; )
;
; An alternative definition (that does not check for lexical errors) is
;
; (make-parser
;   '(
;      (s (#\x #\= e))
;      (e (e #\+ e) (v))
;      (v (v #\y) ())
;    )
;   (lambda (str) (map (lambda (char) (list char char)) (string->list str)))
; )
;
; To help with the rest of the discussion, here are a few definitions:
;
; An input pointer (for an input of `n' tokens) is a value between 0 and `n'.
; It indicates a point between two input tokens (0 = beginning, `n' = end).
; For example, if `n' = 4, there are 5 input pointers:
;
;   input                   token1     token2     token3     token4
;   input pointers       0          1          2          3          4
;
; A configuration indicates the extent to which a given rule is parsed (this
; is the common `dot notation').  For simplicity, a configuration is
; represented as an integer, with successive configurations in the same
; rule associated with successive integers.  It is assumed that the grammar
; has been extended with rules to aid scanning.  These rules are of the
; form `nt ->', and there is one such rule for every non-terminal.  Note
; that these rules are special because they only apply when the corresponding
; non-terminal is returned by the lexer.
;
; A configuration set is a configuration grouped with the set of input pointers
; representing where the head non-terminal of the configuration was predicted.
;
; Here are the rules and configurations for the grammar given above:
;
;  S -> .         \
;       0          |
;  x -> .          |
;       1          |
;  = -> .          |
;       2          |
;  E -> .          |
;       3           > special rules (for scanning)
;  + -> .          |
;       4          |
;  V -> .          |
;       5          |
;  y -> .          |
;       6         /
;  S -> .  x  .  =  .  E  .
;       7     8     9     10
;  E -> .  E  .  +  .  E  .
;       11    12    13    14
;  E -> .  V  .
;       15    16
;  V -> .  V  .  y  .
;       17    18    19
;  V -> .
;       20
;
; Starters of the non-terminal `nt' are configurations that are leftmost
; in a non-special rule for `nt'.  Enders of the non-terminal `nt' are
; configurations that are rightmost in any rule for `nt'.  Predictors of the
; non-terminal `nt' are configurations that are directly to the left of `nt'
; in any rule.
;
; For the grammar given above,
;
;   Starters of V   = (17 20)
;   Enders of V     = (5 19 20)
;   Predictors of V = (15 17)

(define (make-parser grammar lexer)

  (define (non-terminals grammar) ; return vector of non-terminals in grammar

    (define (add-nt nt nts)
      (if (member nt nts) nts (cons nt nts))) ; use equal? for equality tests

    (let def-loop ((defs grammar) (nts '()))
      (if (pair? defs)
        (let* ((def (car defs))
               (head (car def)))
          (let rule-loop ((rules (cdr def))
                          (nts (add-nt head nts)))
            (if (pair? rules)
              (let ((rule (car rules)))
                (let loop ((l rule) (nts nts))
                  (if (pair? l)
                    (let ((nt (car l)))
                      (loop (cdr l) (add-nt nt nts)))
                    (rule-loop (cdr rules) nts))))
              (def-loop (cdr defs) nts))))
        (list->vector (reverse nts))))) ; goal non-terminal must be at index 0

  (define (ind nt nts) ; return index of non-terminal `nt' in `nts'
    (let loop ((i (- (vector-length nts) 1)))
      (if (>= i 0)
        (if (equal? (vector-ref nts i) nt) i (loop (- i 1)))
        #f)))

  (define (nb-configurations grammar) ; return nb of configurations in grammar
    (let def-loop ((defs grammar) (nb-confs 0))
      (if (pair? defs)
        (let ((def (car defs)))
          (let rule-loop ((rules (cdr def)) (nb-confs nb-confs))
            (if (pair? rules)
              (let ((rule (car rules)))
                (let loop ((l rule) (nb-confs nb-confs))
                  (if (pair? l)
                    (loop (cdr l) (+ nb-confs 1))
                    (rule-loop (cdr rules) (+ nb-confs 1)))))
              (def-loop (cdr defs) nb-confs))))
      nb-confs)))

; First, associate a numeric identifier to every non-terminal in the
; grammar (with the goal non-terminal associated with 0).
;
; So, for the grammar given above we get:
;
; s -> 0   x -> 1   = -> 4   e ->3    + -> 4   v -> 5   y -> 6

  (let* ((nts (non-terminals grammar))          ; id map = list of non-terms
         (nb-nts (vector-length nts))           ; the number of non-terms
         (nb-confs (+ (nb-configurations grammar) nb-nts)) ; the nb of confs
         (starters (make-vector nb-nts '()))    ; starters for every non-term
         (enders (make-vector nb-nts '()))      ; enders for every non-term
         (predictors (make-vector nb-nts '()))  ; predictors for every non-term
         (steps (make-vector nb-confs #f))      ; what to do in a given conf
         (names (make-vector nb-confs #f)))     ; name of rules

    (define (setup-tables grammar nts starters enders predictors steps names)

      (define (add-conf conf nt nts class)
        (let ((i (ind nt nts)))
          (vector-set! class i (cons conf (vector-ref class i)))))

      (let ((nb-nts (vector-length nts)))

        (let nt-loop ((i (- nb-nts 1)))
          (if (>= i 0)
            (begin
              (vector-set! steps i (- i nb-nts))
              (vector-set! names i (list (vector-ref nts i) 0))
              (vector-set! enders i (list i))
              (nt-loop (- i 1)))))

        (let def-loop ((defs grammar) (conf (vector-length nts)))
          (if (pair? defs)
            (let* ((def (car defs))
                   (head (car def)))
              (let rule-loop ((rules (cdr def)) (conf conf) (rule-num 1))
                (if (pair? rules)
                  (let ((rule (car rules)))
                    (vector-set! names conf (list head rule-num))
                    (add-conf conf head nts starters)
                    (let loop ((l rule) (conf conf))
                      (if (pair? l)
                        (let ((nt (car l)))
                          (vector-set! steps conf (ind nt nts))
                          (add-conf conf nt nts predictors)
                          (loop (cdr l) (+ conf 1)))
                        (begin
                          (vector-set! steps conf (- (ind head nts) nb-nts))
                          (add-conf conf head nts enders)
                          (rule-loop (cdr rules) (+ conf 1) (+ rule-num 1))))))
                  (def-loop (cdr defs) conf))))))))

; Now, for each non-terminal, compute the starters, enders and predictors and
; the names and steps tables.

    (setup-tables grammar nts starters enders predictors steps names)

; Build the parser description

    (let ((parser-descr (vector lexer
                                nts
                                starters
                                enders
                                predictors
                                steps
                                names)))
      (lambda (input)

        (define (ind nt nts) ; return index of non-terminal `nt' in `nts'
          (let loop ((i (- (vector-length nts) 1)))
            (if (>= i 0)
              (if (equal? (vector-ref nts i) nt) i (loop (- i 1)))
              #f)))

        (define (comp-tok tok nts) ; transform token to parsing format
          (let loop ((l1 (cdr tok)) (l2 '()))
            (if (pair? l1)
              (let ((i (ind (car l1) nts)))
                (if i
                  (loop (cdr l1) (cons i l2))
                  (loop (cdr l1) l2)))
              (cons (car tok) (reverse l2)))))

        (define (input->tokens input lexer nts)
          (list->vector (map (lambda (tok) (comp-tok tok nts)) (lexer input))))

        (define (make-states nb-toks nb-confs)
          (let ((states (make-vector (+ nb-toks 1) #f)))
            (let loop ((i nb-toks))
              (if (>= i 0)
                (let ((v (make-vector (+ nb-confs 1) #f)))
                  (vector-set! v 0 -1)
                  (vector-set! states i v)
                  (loop (- i 1)))
                states))))

        (define (conf-set-get state conf)
          (vector-ref state (+ conf 1)))

        (define (conf-set-get* state state-num conf)
          (let ((conf-set (conf-set-get state conf)))
            (if conf-set
              conf-set
              (let ((conf-set (make-vector (+ state-num 6) #f)))
                (vector-set! conf-set 1 -3) ; old elems tail (points to head)
                (vector-set! conf-set 2 -1) ; old elems head
                (vector-set! conf-set 3 -1) ; new elems tail (points to head)
                (vector-set! conf-set 4 -1) ; new elems head
                (vector-set! state (+ conf 1) conf-set)
                conf-set))))

        (define (conf-set-merge-new! conf-set)
          (vector-set! conf-set
            (+ (vector-ref conf-set 1) 5)
            (vector-ref conf-set 4))
          (vector-set! conf-set 1 (vector-ref conf-set 3))
          (vector-set! conf-set 3 -1)
          (vector-set! conf-set 4 -1))

        (define (conf-set-head conf-set)
          (vector-ref conf-set 2))

        (define (conf-set-next conf-set i)
          (vector-ref conf-set (+ i 5)))

        (define (conf-set-member? state conf i)
          (let ((conf-set (vector-ref state (+ conf 1))))
            (if conf-set
              (conf-set-next conf-set i)
              #f)))

        (define (conf-set-adjoin state conf-set conf i)
          (let ((tail (vector-ref conf-set 3))) ; put new element at tail
            (vector-set! conf-set (+ i 5) -1)
            (vector-set! conf-set (+ tail 5) i)
            (vector-set! conf-set 3 i)
            (if (< tail 0)
              (begin
                (vector-set! conf-set 0 (vector-ref state 0))
                (vector-set! state 0 conf)))))

        (define (conf-set-adjoin* states state-num l i)
          (let ((state (vector-ref states state-num)))
            (let loop ((l1 l))
              (if (pair? l1)
                (let* ((conf (car l1))
                       (conf-set (conf-set-get* state state-num conf)))
                  (if (not (conf-set-next conf-set i))
                    (begin
                      (conf-set-adjoin state conf-set conf i)
                      (loop (cdr l1)))
                    (loop (cdr l1))))))))

        (define (conf-set-adjoin** states states* state-num conf i)
          (let ((state (vector-ref states state-num)))
            (if (conf-set-member? state conf i)
              (let* ((state* (vector-ref states* state-num))
                     (conf-set* (conf-set-get* state* state-num conf)))
                (if (not (conf-set-next conf-set* i))
                  (conf-set-adjoin state* conf-set* conf i))
                #t)
              #f)))

        (define (conf-set-union state conf-set conf other-set)
          (let loop ((i (conf-set-head other-set)))
            (if (>= i 0)
              (if (not (conf-set-next conf-set i))
                (begin
                  (conf-set-adjoin state conf-set conf i)
                  (loop (conf-set-next other-set i)))
                (loop (conf-set-next other-set i))))))

        (define (forw states state-num starters enders predictors steps nts)

          (define (predict state state-num conf-set conf nt starters enders)

            ; add configurations which start the non-terminal `nt' to the
            ; right of the dot

            (let loop1 ((l (vector-ref starters nt)))
              (if (pair? l)
                (let* ((starter (car l))
                       (starter-set (conf-set-get* state state-num starter)))
                  (if (not (conf-set-next starter-set state-num))
                    (begin
                      (conf-set-adjoin state starter-set starter state-num)
                      (loop1 (cdr l)))
                    (loop1 (cdr l))))))

            ; check for possible completion of the non-terminal `nt' to the
            ; right of the dot

            (let loop2 ((l (vector-ref enders nt)))
              (if (pair? l)
                (let ((ender (car l)))
                  (if (conf-set-member? state ender state-num)
                    (let* ((next (+ conf 1))
                           (next-set (conf-set-get* state state-num next)))
                      (conf-set-union state next-set next conf-set)
                      (loop2 (cdr l)))
                    (loop2 (cdr l)))))))

          (define (reduce states state state-num conf-set head preds)

            ; a non-terminal is now completed so check for reductions that
            ; are now possible at the configurations `preds'

            (let loop1 ((l preds))
              (if (pair? l)
                (let ((pred (car l)))
                  (let loop2 ((i head))
                    (if (>= i 0)
                      (let ((pred-set (conf-set-get (vector-ref states i) pred)))
                        (if pred-set
                          (let* ((next (+ pred 1))
                                 (next-set (conf-set-get* state state-num next)))
                            (conf-set-union state next-set next pred-set)))
                        (loop2 (conf-set-next conf-set i)))
                      (loop1 (cdr l))))))))

          (let ((state (vector-ref states state-num))
                (nb-nts (vector-length nts)))
            (let loop ()
              (let ((conf (vector-ref state 0)))
                (if (>= conf 0)
                  (let* ((step (vector-ref steps conf))
                         (conf-set (vector-ref state (+ conf 1)))
                         (head (vector-ref conf-set 4)))
                    (vector-set! state 0 (vector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (if (>= step 0)
                      (predict state state-num conf-set conf step starters enders)
                      (let ((preds (vector-ref predictors (+ step nb-nts))))
                        (reduce states state state-num conf-set head preds)))
                    (loop)))))))

        (define (forward starters enders predictors steps nts toks)
          (let* ((nb-toks (vector-length toks))
                 (nb-confs (vector-length steps))
                 (states (make-states nb-toks nb-confs))
                 (goal-starters (vector-ref starters 0)))
            (conf-set-adjoin* states 0 goal-starters 0) ; predict goal
            (forw states 0 starters enders predictors steps nts)
            (let loop ((i 0))
              (if (< i nb-toks)
                (let ((tok-nts (cdr (vector-ref toks i))))
                  (conf-set-adjoin* states (+ i 1) tok-nts i) ; scan token
                  (forw states (+ i 1) starters enders predictors steps nts)
                  (loop (+ i 1)))))
            states))

        (define (produce conf i j enders steps toks states states* nb-nts)
          (let ((prev (- conf 1)))
            (if (and (>= conf nb-nts) (>= (vector-ref steps prev) 0))
              (let loop1 ((l (vector-ref enders (vector-ref steps prev))))
                (if (pair? l)
                  (let* ((ender (car l))
                         (ender-set (conf-set-get (vector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loop2 ((k (conf-set-head ender-set)))
                        (if (>= k 0)
                          (begin
                            (and (>= k i)
                                 (conf-set-adjoin** states states* k prev i)
                                 (conf-set-adjoin** states states* j ender k))
                            (loop2 (conf-set-next ender-set k)))
                          (loop1 (cdr l))))
                      (loop1 (cdr l)))))))))

        (define (back states states* state-num enders steps nb-nts toks)
          (let ((state* (vector-ref states* state-num)))
            (let loop1 ()
              (let ((conf (vector-ref state* 0)))
                (if (>= conf 0)
                  (let* ((conf-set (vector-ref state* (+ conf 1)))
                         (head (vector-ref conf-set 4)))
                    (vector-set! state* 0 (vector-ref conf-set 0))
                    (conf-set-merge-new! conf-set)
                    (let loop2 ((i head))
                      (if (>= i 0)
                        (begin
                          (produce conf i state-num enders steps
                                   toks states states* nb-nts)
                          (loop2 (conf-set-next conf-set i)))
                        (loop1)))))))))

        (define (backward states enders steps nts toks)
          (let* ((nb-toks (vector-length toks))
                 (nb-confs (vector-length steps))
                 (nb-nts (vector-length nts))
                 (states* (make-states nb-toks nb-confs))
                 (goal-enders (vector-ref enders 0)))
            (let loop1 ((l goal-enders))
              (if (pair? l)
                (let ((conf (car l)))
                  (conf-set-adjoin** states states* nb-toks conf 0)
                  (loop1 (cdr l)))))
            (let loop2 ((i nb-toks))
              (if (>= i 0)
                (begin
                  (back states states* i enders steps nb-nts toks)
                  (loop2 (- i 1)))))
            states*))

        (define (parsed? nt i j nts enders states)
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        #t
                        (loop (cdr l))))
                    #f)))
              #f)))

        (define (deriv-trees conf i j enders steps names toks states nb-nts)
          (let ((name (vector-ref names conf)))

            (if name ; `conf' is at the start of a rule (either special or not)
              (if (< conf nb-nts)
                (list (list name (car (vector-ref toks i))))
                (list (list name)))

              (let ((prev (- conf 1)))
                (let loop1 ((l1 (vector-ref enders (vector-ref steps prev)))
                            (l2 '()))
                  (if (pair? l1)
                    (let* ((ender (car l1))
                           (ender-set (conf-set-get (vector-ref states j)
                                                    ender)))
                      (if ender-set
                        (let loop2 ((k (conf-set-head ender-set)) (l2 l2))
                          (if (>= k 0)
                            (if (and (>= k i)
                                     (conf-set-member? (vector-ref states k)
                                                       prev i))
                              (let ((prev-trees
                                      (deriv-trees prev i k enders steps names
                                                   toks states nb-nts))
                                    (ender-trees
                                      (deriv-trees ender k j enders steps names
                                                   toks states nb-nts)))
                                (let loop3 ((l3 ender-trees) (l2 l2))
                                  (if (pair? l3)
                                    (let ((ender-tree (list (car l3))))
                                      (let loop4 ((l4 prev-trees) (l2 l2))
                                        (if (pair? l4)
                                          (loop4 (cdr l4)
                                                 (cons (append (car l4)
                                                               ender-tree)
                                                       l2))
                                          (loop3 (cdr l3) l2))))
                                    (loop2 (conf-set-next ender-set k) l2))))
                              (loop2 (conf-set-next ender-set k) l2))
                            (loop1 (cdr l1) l2)))
                        (loop1 (cdr l1) l2)))
                    l2))))))

        (define (deriv-trees* nt i j nts enders steps names toks states)
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)) (trees '()))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        (loop (cdr l)
                              (append (deriv-trees conf i j enders steps names
                                                   toks states nb-nts)
                                      trees))
                        (loop (cdr l) trees)))
                    trees)))
              #f)))

        (define (nb-deriv-trees conf i j enders steps toks states nb-nts)
          (let ((prev (- conf 1)))
            (if (or (< conf nb-nts) (< (vector-ref steps prev) 0))
              1
              (let loop1 ((l (vector-ref enders (vector-ref steps prev)))
                          (n 0))
                (if (pair? l)
                  (let* ((ender (car l))
                         (ender-set (conf-set-get (vector-ref states j)
                                                  ender)))
                    (if ender-set
                      (let loop2 ((k (conf-set-head ender-set)) (n n))
                        (if (>= k 0)
                          (if (and (>= k i)
                                   (conf-set-member? (vector-ref states k)
                                                     prev i))
                            (let ((nb-prev-trees
                                    (nb-deriv-trees prev i k enders steps
                                                    toks states nb-nts))
                                  (nb-ender-trees
                                    (nb-deriv-trees ender k j enders steps
                                                    toks states nb-nts)))
                              (loop2 (conf-set-next ender-set k)
                                     (+ n (* nb-prev-trees nb-ender-trees))))
                            (loop2 (conf-set-next ender-set k) n))
                          (loop1 (cdr l) n)))
                      (loop1 (cdr l) n)))
                  n)))))

        (define (nb-deriv-trees* nt i j nts enders steps toks states)
          (let ((nt* (ind nt nts)))
            (if nt*
              (let ((nb-nts (vector-length nts)))
                (let loop ((l (vector-ref enders nt*)) (nb-trees 0))
                  (if (pair? l)
                    (let ((conf (car l)))
                      (if (conf-set-member? (vector-ref states j) conf i)
                        (loop (cdr l)
                              (+ (nb-deriv-trees conf i j enders steps
                                                 toks states nb-nts)
                                 nb-trees))
                        (loop (cdr l) nb-trees)))
                    nb-trees)))
              #f)))

        (let* ((lexer      (vector-ref parser-descr 0))
               (nts        (vector-ref parser-descr 1))
               (starters   (vector-ref parser-descr 2))
               (enders     (vector-ref parser-descr 3))
               (predictors (vector-ref parser-descr 4))
               (steps      (vector-ref parser-descr 5))
               (names      (vector-ref parser-descr 6))
               (toks       (input->tokens input lexer nts)))

          (vector nts
                  starters
                  enders
                  predictors
                  steps
                  names
                  toks
                  (backward (forward starters enders predictors steps nts toks)
                            enders steps nts toks)
                  parsed?
                  deriv-trees*
                  nb-deriv-trees*))))))

(define (parse->parsed? parse nt i j)
  (let* ((nts     (vector-ref parse 0))
         (enders  (vector-ref parse 2))
         (states  (vector-ref parse 7))
         (parsed? (vector-ref parse 8)))
    (parsed? nt i j nts enders states)))

(define (parse->trees parse nt i j)
  (let* ((nts          (vector-ref parse 0))
         (enders       (vector-ref parse 2))
         (steps        (vector-ref parse 4))
         (names        (vector-ref parse 5))
         (toks         (vector-ref parse 6))
         (states       (vector-ref parse 7))
         (deriv-trees* (vector-ref parse 9)))
    (deriv-trees* nt i j nts enders steps names toks states)))

(define (parse->nb-trees parse nt i j)
  (let* ((nts             (vector-ref parse 0))
         (enders          (vector-ref parse 2))
         (steps           (vector-ref parse 4))
         (toks            (vector-ref parse 6))
         (states          (vector-ref parse 7))
         (nb-deriv-trees* (vector-ref parse 10)))
    (nb-deriv-trees* nt i j nts enders steps toks states)))

(define (earley-test)
  (let ((p (make-parser '( (s (a) (s s)) )
                        (lambda (l) (map (lambda (x) (list x x)) l)))))
    (let ((x (p '(a a a a a a a a a))))
      (length (parse->trees x 's 0 9)))))

(define (main-earley . args)
  (run-single-benchmark
    "earley"
    earley-iters
    (lambda (result) (equal? result 1430))
    (lambda () (lambda () (earley-test)))))

;;; FFTRAD4

(define lut-table-size 512)
(define lut-table-size^2 262144)
(define lut-table-size^3 134217728)
(define log-lut-table-size 9)

(define low-lut
  (FLOATvector-const
   1. 0.
   .7071067811865476 .7071067811865476
   .9238795325112867 .3826834323650898
   .3826834323650898 .9238795325112867
   .9807852804032304 .19509032201612828
   .5555702330196022 .8314696123025452
   .8314696123025452 .5555702330196022
   .19509032201612828 .9807852804032304
   .9951847266721969 .0980171403295606
   .6343932841636455 .773010453362737
   .881921264348355 .47139673682599764
   .2902846772544624 .9569403357322088
   .9569403357322088 .2902846772544624
   .47139673682599764 .881921264348355
   .773010453362737 .6343932841636455
   .0980171403295606 .9951847266721969
   .9987954562051724 .049067674327418015
   .6715589548470184 .7409511253549591
   .9039892931234433 .4275550934302821
   .33688985339222005 .9415440651830208
   .970031253194544 .2429801799032639
   .5141027441932218 .8577286100002721
   .8032075314806449 .5956993044924334
   .14673047445536175 .989176509964781
   .989176509964781 .14673047445536175
   .5956993044924334 .8032075314806449
   .8577286100002721 .5141027441932218
   .2429801799032639 .970031253194544
   .9415440651830208 .33688985339222005
   .4275550934302821 .9039892931234433
   .7409511253549591 .6715589548470184
   .049067674327418015 .9987954562051724
   .9996988186962042 .024541228522912288
   .6895405447370669 .7242470829514669
   .9142097557035307 .40524131400498986
   .35989503653498817 .9329927988347388
   .9757021300385286 .2191012401568698
   .5349976198870973 .8448535652497071
   .8175848131515837 .5758081914178453
   .17096188876030122 .9852776423889412
   .99247953459871 .1224106751992162
   .6152315905806268 .7883464276266062
   .8700869911087115 .49289819222978404
   .26671275747489837 .9637760657954398
   .9495281805930367 .31368174039889146
   .4496113296546066 .8932243011955153
   .7572088465064846 .6531728429537768
   .07356456359966743 .9972904566786902
   .9972904566786902 .07356456359966743
   .6531728429537768 .7572088465064846
   .8932243011955153 .4496113296546066
   .31368174039889146 .9495281805930367
   .9637760657954398 .26671275747489837
   .49289819222978404 .8700869911087115
   .7883464276266062 .6152315905806268
   .1224106751992162 .99247953459871
   .9852776423889412 .17096188876030122
   .5758081914178453 .8175848131515837
   .8448535652497071 .5349976198870973
   .2191012401568698 .9757021300385286
   .9329927988347388 .35989503653498817
   .40524131400498986 .9142097557035307
   .7242470829514669 .6895405447370669
   .024541228522912288 .9996988186962042
   .9999247018391445 .012271538285719925
   .6983762494089728 .7157308252838187
   .9191138516900578 .3939920400610481
   .37131719395183754 .9285060804732156
   .9783173707196277 .20711137619221856
   .5453249884220465 .8382247055548381
   .8245893027850253 .5657318107836132
   .18303988795514095 .9831054874312163
   .9939069700023561 .11022220729388306
   .6248594881423863 .7807372285720945
   .8760700941954066 .4821837720791228
   .2785196893850531 .9604305194155658
   .9533060403541939 .3020059493192281
   .46053871095824 .8876396204028539
   .765167265622459 .6438315428897915
   .0857973123444399 .996312612182778
   .9981181129001492 .06132073630220858
   .6624157775901718 .7491363945234594
   .8986744656939538 .43861623853852766
   .3253102921622629 .9456073253805213
   .9669764710448521 .25486565960451457
   .5035383837257176 .8639728561215867
   .7958369046088836 .6055110414043255
   .1345807085071262 .99090263542778
   .9873014181578584 .15885814333386145
   .5857978574564389 .8104571982525948
   .8513551931052652 .524589682678469
   .2310581082806711 .9729399522055602
   .937339011912575 .34841868024943456
   .4164295600976372 .9091679830905224
   .7326542716724128 .680600997795453
   .03680722294135883 .9993223845883495
   .9993223845883495 .03680722294135883
   .680600997795453 .7326542716724128
   .9091679830905224 .4164295600976372
   .34841868024943456 .937339011912575
   .9729399522055602 .2310581082806711
   .524589682678469 .8513551931052652
   .8104571982525948 .5857978574564389
   .15885814333386145 .9873014181578584
   .99090263542778 .1345807085071262
   .6055110414043255 .7958369046088836
   .8639728561215867 .5035383837257176
   .25486565960451457 .9669764710448521
   .9456073253805213 .3253102921622629
   .43861623853852766 .8986744656939538
   .7491363945234594 .6624157775901718
   .06132073630220858 .9981181129001492
   .996312612182778 .0857973123444399
   .6438315428897915 .765167265622459
   .8876396204028539 .46053871095824
   .3020059493192281 .9533060403541939
   .9604305194155658 .2785196893850531
   .4821837720791228 .8760700941954066
   .7807372285720945 .6248594881423863
   .11022220729388306 .9939069700023561
   .9831054874312163 .18303988795514095
   .5657318107836132 .8245893027850253
   .8382247055548381 .5453249884220465
   .20711137619221856 .9783173707196277
   .9285060804732156 .37131719395183754
   .3939920400610481 .9191138516900578
   .7157308252838187 .6983762494089728
   .012271538285719925 .9999247018391445
   .9999811752826011 .006135884649154475
   .7027547444572253 .7114321957452164
   .9215140393420419 .3883450466988263
   .37700741021641826 .9262102421383114
   .9795697656854405 .2011046348420919
   .5504579729366048 .83486287498638
   .8280450452577558 .560661576197336
   .18906866414980622 .9819638691095552
   .9945645707342554 .10412163387205457
   .629638238914927 .7768884656732324
   .8790122264286335 .47679923006332214
   .2844075372112718 .9587034748958716
   .9551411683057707 .29615088824362384
   .4659764957679662 .8847970984309378
   .7691033376455796 .6391244448637757
   .09190895649713272 .9957674144676598
   .9984755805732948 .05519524434968994
   .6669999223036375 .745057785441466
   .901348847046022 .43309381885315196
   .33110630575987643 .9435934581619604
   .9685220942744173 .24892760574572018
   .508830142543107 .8608669386377673
   .799537269107905 .600616479383869
   .14065823933284924 .9900582102622971
   .9882575677307495 .15279718525844344
   .5907597018588743 .8068475535437992
   .8545579883654005 .5193559901655896
   .2370236059943672 .9715038909862518
   .9394592236021899 .3426607173119944
   .4220002707997997 .9065957045149153
   .7368165688773699 .6760927035753159
   .04293825693494082 .9990777277526454
   .9995294175010931 .030674803176636626
   .6850836677727004 .7284643904482252
   .9117060320054299 .41084317105790397
   .3541635254204904 .9351835099389476
   .9743393827855759 .22508391135979283
   .5298036246862947 .8481203448032972
   .8140363297059484 .5808139580957645
   .16491312048996992 .9863080972445987
   .9917097536690995 .12849811079379317
   .6103828062763095 .7921065773002124
   .8670462455156926 .49822766697278187
   .2607941179152755 .9653944416976894
   .9475855910177411 .3195020308160157
   .44412214457042926 .8959662497561851
   .7531867990436125 .6578066932970786
   .06744391956366406 .9977230666441916
   .9968202992911657 .07968243797143013
   .6485144010221124 .7612023854842618
   .8904487232447579 .45508358712634384
   .30784964004153487 .9514350209690083
   .9621214042690416 .272621355449949
   .48755016014843594 .8730949784182901
   .7845565971555752 .6200572117632892
   .11631863091190477 .9932119492347945
   .984210092386929 .17700422041214875
   .5707807458869673 .8211025149911046
   .8415549774368984 .5401714727298929
   .21311031991609136 .9770281426577544
   .9307669610789837 .36561299780477385
   .39962419984564684 .9166790599210427
   .7200025079613817 .693971460889654
   .01840672990580482 .9998305817958234
   .9998305817958234 .01840672990580482
   .693971460889654 .7200025079613817
   .9166790599210427 .39962419984564684
   .36561299780477385 .9307669610789837
   .9770281426577544 .21311031991609136
   .5401714727298929 .8415549774368984
   .8211025149911046 .5707807458869673
   .17700422041214875 .984210092386929
   .9932119492347945 .11631863091190477
   .6200572117632892 .7845565971555752
   .8730949784182901 .48755016014843594
   .272621355449949 .9621214042690416
   .9514350209690083 .30784964004153487
   .45508358712634384 .8904487232447579
   .7612023854842618 .6485144010221124
   .07968243797143013 .9968202992911657
   .9977230666441916 .06744391956366406
   .6578066932970786 .7531867990436125
   .8959662497561851 .44412214457042926
   .3195020308160157 .9475855910177411
   .9653944416976894 .2607941179152755
   .49822766697278187 .8670462455156926
   .7921065773002124 .6103828062763095
   .12849811079379317 .9917097536690995
   .9863080972445987 .16491312048996992
   .5808139580957645 .8140363297059484
   .8481203448032972 .5298036246862947
   .22508391135979283 .9743393827855759
   .9351835099389476 .3541635254204904
   .41084317105790397 .9117060320054299
   .7284643904482252 .6850836677727004
   .030674803176636626 .9995294175010931
   .9990777277526454 .04293825693494082
   .6760927035753159 .7368165688773699
   .9065957045149153 .4220002707997997
   .3426607173119944 .9394592236021899
   .9715038909862518 .2370236059943672
   .5193559901655896 .8545579883654005
   .8068475535437992 .5907597018588743
   .15279718525844344 .9882575677307495
   .9900582102622971 .14065823933284924
   .600616479383869 .799537269107905
   .8608669386377673 .508830142543107
   .24892760574572018 .9685220942744173
   .9435934581619604 .33110630575987643
   .43309381885315196 .901348847046022
   .745057785441466 .6669999223036375
   .05519524434968994 .9984755805732948
   .9957674144676598 .09190895649713272
   .6391244448637757 .7691033376455796
   .8847970984309378 .4659764957679662
   .29615088824362384 .9551411683057707
   .9587034748958716 .2844075372112718
   .47679923006332214 .8790122264286335
   .7768884656732324 .629638238914927
   .10412163387205457 .9945645707342554
   .9819638691095552 .18906866414980622
   .560661576197336 .8280450452577558
   .83486287498638 .5504579729366048
   .2011046348420919 .9795697656854405
   .9262102421383114 .37700741021641826
   .3883450466988263 .9215140393420419
   .7114321957452164 .7027547444572253
   .006135884649154475 .9999811752826011
   .9999952938095762 .003067956762965976
   .7049340803759049 .7092728264388657
   .9227011283338785 .38551605384391885
   .37984720892405116 .9250492407826776
   .9801821359681174 .1980984107179536
   .5530167055800276 .8331701647019132
   .829761233794523 .5581185312205561
   .19208039704989244 .9813791933137546
   .9948793307948056 .10106986275482782
   .6320187359398091 .7749531065948739
   .8804708890521608 .47410021465055
   .2873474595447295 .9578264130275329
   .9560452513499964 .29321916269425863
   .46868882203582796 .8833633386657316
   .7710605242618138 .6367618612362842
   .094963495329639 .9954807554919269
   .9986402181802653 .052131704680283324
   .6692825883466361 .7430079521351217
   .9026733182372588 .4303264813400826
   .3339996514420094 .9425731976014469
   .9692812353565485 .24595505033579462
   .5114688504379704 .8593018183570084
   .8013761717231402 .5981607069963423
   .14369503315029444 .9896220174632009
   .9887216919603238 .1497645346773215
   .5932322950397998 .8050313311429635
   .8561473283751945 .5167317990176499
   .2400030224487415 .9707721407289504
   .9405060705932683 .33977688440682685
   .4247796812091088 .9052967593181188
   .7388873244606151 .673829000378756
   .04600318213091463 .9989412931868569
   .9996188224951786 .027608145778965743
   .6873153408917592 .726359155084346
   .9129621904283982 .4080441628649787
   .35703096123343003 .9340925504042589
   .9750253450669941 .22209362097320354
   .532403127877198 .8464909387740521
   .8158144108067338 .5783137964116556
   .16793829497473117 .9857975091675675
   .9920993131421918 .12545498341154623
   .6128100824294097 .79023022143731
   .8685707059713409 .49556526182577254
   .2637546789748314 .9645897932898128
   .9485613499157303 .31659337555616585
   .4468688401623742 .8945994856313827
   .7552013768965365 .6554928529996153
   .07050457338961387 .9975114561403035
   .997060070339483 .07662386139203149
   .6508466849963809 .7592091889783881
   .8918407093923427 .4523495872337709
   .3107671527496115 .9504860739494817
   .9629532668736839 .2696683255729151
   .49022648328829116 .8715950866559511
   .7864552135990858 .617647307937804
   .11936521481099137 .9928504144598651
   .9847485018019042 .17398387338746382
   .5732971666980422 .819347520076797
   .8432082396418454 .5375870762956455
   .21610679707621952 .9763697313300211
   .9318842655816681 .3627557243673972
   .40243465085941843 .9154487160882678
   .7221281939292153 .6917592583641577
   .021474080275469508 .9997694053512153
   .9998823474542126 .015339206284988102
   .696177131491463 .7178700450557317
   .9179007756213905 .3968099874167103
   .3684668299533723 .9296408958431812
   .9776773578245099 .2101118368804696
   .5427507848645159 .8398937941959995
   .8228497813758263 .5682589526701316
   .18002290140569951 .9836624192117303
   .9935641355205953 .11327095217756435
   .62246127937415 .7826505961665757
   .8745866522781761 .4848692480007911
   .27557181931095814 .9612804858113206
   .9523750127197659 .30492922973540243
   .45781330359887723 .8890483558546646
   .7631884172633813 .6461760129833164
   .08274026454937569 .9965711457905548
   .997925286198596 .06438263092985747
   .6601143420674205 .7511651319096864
   .8973245807054183 .44137126873171667
   .32240767880106985 .9466009130832835
   .9661900034454125 .257831102162159
   .5008853826112408 .8655136240905691
   .7939754775543372 .6079497849677736
   .13154002870288312 .9913108598461154
   .9868094018141855 .16188639378011183
   .5833086529376983 .8122505865852039
   .8497417680008524 .5271991347819014
   .22807208317088573 .973644249650812
   .9362656671702783 .35129275608556715
   .41363831223843456 .9104412922580672
   .7305627692278276 .6828455463852481
   .03374117185137759 .9994306045554617
   .9992047586183639 .03987292758773981
   .6783500431298615 .7347388780959635
   .9078861164876663 .41921688836322396
   .34554132496398904 .9384035340631081
   .9722264970789363 .23404195858354343
   .5219752929371544 .8529606049303636
   .808656181588175 .5882815482226453
   .15582839765426523 .9877841416445722
   .9904850842564571 .13762012158648604
   .6030665985403482 .7976908409433912
   .8624239561110405 .5061866453451553
   .25189781815421697 .9677538370934755
   .9446048372614803 .32820984357909255
   .4358570799222555 .9000158920161603
   .7471006059801801 .6647109782033449
   .05825826450043576 .9983015449338929
   .996044700901252 .0888535525825246
   .6414810128085832 .7671389119358204
   .8862225301488806 .4632597835518602
   .2990798263080405 .9542280951091057
   .9595715130819845 .281464937925758
   .479493757660153 .8775452902072612
   .778816512381476 .6272518154951441
   .10717242495680884 .9942404494531879
   .9825393022874412 .18605515166344666
   .5631993440138341 .8263210628456635
   .836547727223512 .5478940591731002
   .20410896609281687 .9789481753190622
   .9273625256504011 .374164062971458
   .39117038430225387 .9203182767091106
   .7135848687807936 .7005687939432483
   .00920375478205982 .9999576445519639
   .9999576445519639 .00920375478205982
   .7005687939432483 .7135848687807936
   .9203182767091106 .39117038430225387
   .374164062971458 .9273625256504011
   .9789481753190622 .20410896609281687
   .5478940591731002 .836547727223512
   .8263210628456635 .5631993440138341
   .18605515166344666 .9825393022874412
   .9942404494531879 .10717242495680884
   .6272518154951441 .778816512381476
   .8775452902072612 .479493757660153
   .281464937925758 .9595715130819845
   .9542280951091057 .2990798263080405
   .4632597835518602 .8862225301488806
   .7671389119358204 .6414810128085832
   .0888535525825246 .996044700901252
   .9983015449338929 .05825826450043576
   .6647109782033449 .7471006059801801
   .9000158920161603 .4358570799222555
   .32820984357909255 .9446048372614803
   .9677538370934755 .25189781815421697
   .5061866453451553 .8624239561110405
   .7976908409433912 .6030665985403482
   .13762012158648604 .9904850842564571
   .9877841416445722 .15582839765426523
   .5882815482226453 .808656181588175
   .8529606049303636 .5219752929371544
   .23404195858354343 .9722264970789363
   .9384035340631081 .34554132496398904
   .41921688836322396 .9078861164876663
   .7347388780959635 .6783500431298615
   .03987292758773981 .9992047586183639
   .9994306045554617 .03374117185137759
   .6828455463852481 .7305627692278276
   .9104412922580672 .41363831223843456
   .35129275608556715 .9362656671702783
   .973644249650812 .22807208317088573
   .5271991347819014 .8497417680008524
   .8122505865852039 .5833086529376983
   .16188639378011183 .9868094018141855
   .9913108598461154 .13154002870288312
   .6079497849677736 .7939754775543372
   .8655136240905691 .5008853826112408
   .257831102162159 .9661900034454125
   .9466009130832835 .32240767880106985
   .44137126873171667 .8973245807054183
   .7511651319096864 .6601143420674205
   .06438263092985747 .997925286198596
   .9965711457905548 .08274026454937569
   .6461760129833164 .7631884172633813
   .8890483558546646 .45781330359887723
   .30492922973540243 .9523750127197659
   .9612804858113206 .27557181931095814
   .4848692480007911 .8745866522781761
   .7826505961665757 .62246127937415
   .11327095217756435 .9935641355205953
   .9836624192117303 .18002290140569951
   .5682589526701316 .8228497813758263
   .8398937941959995 .5427507848645159
   .2101118368804696 .9776773578245099
   .9296408958431812 .3684668299533723
   .3968099874167103 .9179007756213905
   .7178700450557317 .696177131491463
   .015339206284988102 .9998823474542126
   .9997694053512153 .021474080275469508
   .6917592583641577 .7221281939292153
   .9154487160882678 .40243465085941843
   .3627557243673972 .9318842655816681
   .9763697313300211 .21610679707621952
   .5375870762956455 .8432082396418454
   .819347520076797 .5732971666980422
   .17398387338746382 .9847485018019042
   .9928504144598651 .11936521481099137
   .617647307937804 .7864552135990858
   .8715950866559511 .49022648328829116
   .2696683255729151 .9629532668736839
   .9504860739494817 .3107671527496115
   .4523495872337709 .8918407093923427
   .7592091889783881 .6508466849963809
   .07662386139203149 .997060070339483
   .9975114561403035 .07050457338961387
   .6554928529996153 .7552013768965365
   .8945994856313827 .4468688401623742
   .31659337555616585 .9485613499157303
   .9645897932898128 .2637546789748314
   .49556526182577254 .8685707059713409
   .79023022143731 .6128100824294097
   .12545498341154623 .9920993131421918
   .9857975091675675 .16793829497473117
   .5783137964116556 .8158144108067338
   .8464909387740521 .532403127877198
   .22209362097320354 .9750253450669941
   .9340925504042589 .35703096123343003
   .4080441628649787 .9129621904283982
   .726359155084346 .6873153408917592
   .027608145778965743 .9996188224951786
   .9989412931868569 .04600318213091463
   .673829000378756 .7388873244606151
   .9052967593181188 .4247796812091088
   .33977688440682685 .9405060705932683
   .9707721407289504 .2400030224487415
   .5167317990176499 .8561473283751945
   .8050313311429635 .5932322950397998
   .1497645346773215 .9887216919603238
   .9896220174632009 .14369503315029444
   .5981607069963423 .8013761717231402
   .8593018183570084 .5114688504379704
   .24595505033579462 .9692812353565485
   .9425731976014469 .3339996514420094
   .4303264813400826 .9026733182372588
   .7430079521351217 .6692825883466361
   .052131704680283324 .9986402181802653
   .9954807554919269 .094963495329639
   .6367618612362842 .7710605242618138
   .8833633386657316 .46868882203582796
   .29321916269425863 .9560452513499964
   .9578264130275329 .2873474595447295
   .47410021465055 .8804708890521608
   .7749531065948739 .6320187359398091
   .10106986275482782 .9948793307948056
   .9813791933137546 .19208039704989244
   .5581185312205561 .829761233794523
   .8331701647019132 .5530167055800276
   .1980984107179536 .9801821359681174
   .9250492407826776 .37984720892405116
   .38551605384391885 .9227011283338785
   .7092728264388657 .7049340803759049
   .003067956762965976 .9999952938095762
   ))

(define med-lut
  (FLOATvector-const
   1. 0.
   .9999999999820472 5.9921124526424275e-6
   .9999999999281892 1.1984224905069707e-5
   .9999999998384257 1.7976337357066685e-5
   .9999999997127567 2.396844980841822e-5
   .9999999995511824 2.9960562258909154e-5
   .9999999993537025 3.5952674708324344e-5
   .9999999991203175 4.1944787156448635e-5
   .9999999988510269 4.793689960306688e-5
   .9999999985458309 5.3929012047963936e-5
   .9999999982047294 5.992112449092465e-5
   .9999999978277226 6.591323693173387e-5
   .9999999974148104 7.190534937017645e-5
   .9999999969659927 7.789746180603723e-5
   .9999999964812697 8.388957423910108e-5
   .9999999959606412 8.988168666915283e-5
   .9999999954041073 9.587379909597734e-5
   .999999994811668 1.0186591151935948e-4
   .9999999941833233 1.0785802393908407e-4
   .9999999935190732 1.1385013635493597e-4
   .9999999928189177 1.1984224876670004e-4
   .9999999920828567 1.2583436117416112e-4
   .9999999913108903 1.3182647357710405e-4
   .9999999905030187 1.3781858597531374e-4
   .9999999896592414 1.4381069836857496e-4
   .9999999887795589 1.498028107566726e-4
   .9999999878639709 1.5579492313939151e-4
   .9999999869124775 1.6178703551651655e-4
   .9999999859250787 1.6777914788783258e-4
   .9999999849017744 1.737712602531244e-4
   .9999999838425648 1.797633726121769e-4
   .9999999827474497 1.8575548496477492e-4
   .9999999816164293 1.9174759731070332e-4
   .9999999804495034 1.9773970964974692e-4
   .9999999792466722 2.037318219816906e-4
   .9999999780079355 2.0972393430631923e-4
   .9999999767332933 2.1571604662341763e-4
   .9999999754227459 2.2170815893277063e-4
   .9999999740762929 2.2770027123416315e-4
   .9999999726939346 2.3369238352737996e-4
   .9999999712756709 2.3968449581220595e-4
   .9999999698215016 2.45676608088426e-4
   .9999999683314271 2.5166872035582493e-4
   .9999999668054471 2.5766083261418755e-4
   .9999999652435617 2.636529448632988e-4
   .9999999636457709 2.696450571029434e-4
   .9999999620120748 2.756371693329064e-4
   .9999999603424731 2.8162928155297243e-4
   .9999999586369661 2.876213937629265e-4
   .9999999568955537 2.936135059625534e-4
   .9999999551182358 2.99605618151638e-4
   .9999999533050126 3.055977303299651e-4
   .9999999514558838 3.115898424973196e-4
   .9999999495708498 3.1758195465348636e-4
   .9999999476499103 3.235740667982502e-4
   .9999999456930654 3.2956617893139595e-4
   .9999999437003151 3.3555829105270853e-4
   .9999999416716594 3.4155040316197275e-4
   .9999999396070982 3.475425152589734e-4
   .9999999375066316 3.535346273434955e-4
   .9999999353702598 3.595267394153237e-4
   .9999999331979824 3.6551885147424295e-4
   .9999999309897996 3.7151096352003814e-4
   .9999999287457114 3.7750307555249406e-4
   .9999999264657179 3.8349518757139556e-4
   .9999999241498189 3.8948729957652753e-4
   .9999999217980144 3.954794115676748e-4
   .9999999194103046 4.0147152354462224e-4
   .9999999169866894 4.0746363550715466e-4
   .9999999145271687 4.134557474550569e-4
   .9999999120317428 4.194478593881139e-4
   .9999999095004113 4.2543997130611036e-4
   .9999999069331744 4.314320832088313e-4
   .9999999043300322 4.3742419509606144e-4
   .9999999016909845 4.4341630696758576e-4
   .9999998990160315 4.4940841882318896e-4
   .9999998963051729 4.55400530662656e-4
   .999999893558409 4.613926424857717e-4
   .9999998907757398 4.673847542923209e-4
   .9999998879571651 4.7337686608208844e-4
   .9999998851026849 4.793689778548592e-4
   .9999998822122994 4.8536108961041806e-4
   .9999998792860085 4.913532013485497e-4
   .9999998763238122 4.973453130690393e-4
   .9999998733257104 5.033374247716714e-4
   .9999998702917032 5.09329536456231e-4
   .9999998672217907 5.153216481225028e-4
   .9999998641159727 5.213137597702719e-4
   .9999998609742493 5.27305871399323e-4
   .9999998577966206 5.332979830094408e-4
   .9999998545830864 5.392900946004105e-4
   .9999998513336468 5.452822061720168e-4
   .9999998480483018 5.512743177240444e-4
   .9999998447270514 5.572664292562783e-4
   .9999998413698955 5.632585407685033e-4
   .9999998379768343 5.692506522605043e-4
   .9999998345478677 5.752427637320661e-4
   .9999998310829956 5.812348751829735e-4
   .9999998275822183 5.872269866130116e-4
   .9999998240455354 5.93219098021965e-4
   .9999998204729471 5.992112094096185e-4
   .9999998168644535 6.052033207757572e-4
   .9999998132200545 6.111954321201659e-4
   .99999980953975 6.171875434426292e-4
   .9999998058235401 6.231796547429323e-4
   .9999998020714248 6.291717660208597e-4
   .9999997982834041 6.351638772761965e-4
   .9999997944594781 6.411559885087275e-4
   .9999997905996466 6.471480997182375e-4
   .9999997867039097 6.531402109045114e-4
   .9999997827722674 6.591323220673341e-4
   .9999997788047197 6.651244332064902e-4
   .9999997748012666 6.711165443217649e-4
   .9999997707619082 6.771086554129428e-4
   .9999997666866443 6.83100766479809e-4
   .9999997625754748 6.89092877522148e-4
   .9999997584284002 6.950849885397449e-4
   .9999997542454201 7.010770995323844e-4
   .9999997500265345 7.070692104998515e-4
   .9999997457717437 7.130613214419311e-4
   .9999997414810473 7.190534323584079e-4
   .9999997371544456 7.250455432490666e-4
   .9999997327919384 7.310376541136925e-4
   .9999997283935259 7.3702976495207e-4
   .999999723959208 7.430218757639842e-4
   .9999997194889846 7.490139865492199e-4
   .9999997149828559 7.55006097307562e-4
   .9999997104408218 7.609982080387952e-4
   .9999997058628822 7.669903187427045e-4
   .9999997012490373 7.729824294190747e-4
   .9999996965992869 7.789745400676906e-4
   .9999996919136313 7.849666506883372e-4
   .99999968719207 7.909587612807992e-4
   .9999996824346035 7.969508718448614e-4
   .9999996776412315 8.029429823803089e-4
   .9999996728119542 8.089350928869263e-4
   .9999996679467715 8.149272033644986e-4
   .9999996630456833 8.209193138128106e-4
   .9999996581086897 8.269114242316472e-4
   .9999996531357909 8.329035346207931e-4
   .9999996481269865 8.388956449800333e-4
   .9999996430822767 8.448877553091527e-4
   .9999996380016616 8.508798656079359e-4
   .999999632885141 8.56871975876168e-4
   .9999996277327151 8.628640861136338e-4
   .9999996225443838 8.68856196320118e-4
   .9999996173201471 8.748483064954056e-4
   .999999612060005 8.808404166392814e-4
   .9999996067639574 8.868325267515304e-4
   .9999996014320045 8.928246368319371e-4
   .9999995960641462 8.988167468802867e-4
   .9999995906603825 9.048088568963639e-4
   .9999995852207133 9.108009668799535e-4
   .9999995797451389 9.167930768308405e-4
   .9999995742336589 9.227851867488095e-4
   .9999995686862736 9.287772966336457e-4
   .9999995631029829 9.347694064851338e-4
   .9999995574837868 9.407615163030585e-4
   .9999995518286853 9.467536260872047e-4
   .9999995461376784 9.527457358373575e-4
   .9999995404107661 9.587378455533015e-4
   .9999995346479484 9.647299552348216e-4
   .9999995288492254 9.707220648817027e-4
   .9999995230145969 9.767141744937296e-4
   .9999995171440631 9.827062840706872e-4
   .9999995112376238 9.886983936123602e-4
   .9999995052952791 9.946905031185337e-4
   .9999994993170291 .0010006826125889925
   .9999994933028736 .0010066747220235214
   .9999994872528128 .001012666831421905
   .9999994811668466 .0010186589407839286
   .999999475044975 .0010246510501093766
   .9999994688871979 .0010306431593980344
   .9999994626935156 .0010366352686496862
   .9999994564639277 .0010426273778641173
   .9999994501984345 .0010486194870411127
   .999999443897036 .0010546115961804568
   .999999437559732 .0010606037052819344
   .9999994311865227 .0010665958143453308
   .9999994247774079 .0010725879233704307
   .9999994183323877 .0010785800323570187
   .9999994118514622 .0010845721413048801
   .9999994053346313 .0010905642502137994
   .9999993987818949 .0010965563590835613
   .9999993921932533 .0011025484679139511
   .9999993855687062 .0011085405767047535
   .9999993789082536 .0011145326854557532
   .9999993722118957 .001120524794166735
   .9999993654796325 .0011265169028374842
   .9999993587114638 .0011325090114677853
   .9999993519073898 .001138501120057423
   .9999993450674104 .0011444932286061825
   .9999993381915255 .0011504853371138485
   .9999993312797354 .0011564774455802057
   .9999993243320398 .0011624695540050393
   .9999993173484387 .001168461662388134
   .9999993103289324 .0011744537707292742
   .9999993032735206 .0011804458790282454
   .9999992961822035 .0011864379872848323
   .9999992890549809 .0011924300954988195
   .999999281891853 .001198422203669992
   .9999992746928197 .0012044143117981348
   .999999267457881 .0012104064198830327
   .999999260187037 .0012163985279244702
   .9999992528802875 .0012223906359222325
   .9999992455376326 .0012283827438761045
   .9999992381590724 .0012343748517858707
   .9999992307446068 .0012403669596513162
   .9999992232942359 .001246359067472226
   .9999992158079595 .0012523511752483847
   .9999992082857777 .001258343282979577
   .9999992007276906 .001264335390665588
   .999999193133698 .0012703274983062026
   .9999991855038001 .0012763196059012057
   .9999991778379967 .001282311713450382
   .9999991701362881 .0012883038209535163
   .999999162398674 .0012942959284103935
   .9999991546251547 .0013002880358207985
   .9999991468157298 .001306280143184516
   .9999991389703996 .001312272250501331
   .999999131089164 .0013182643577710285
   .999999123172023 .0013242564649933932
   .9999991152189767 .0013302485721682098
   .9999991072300249 .001336240679295263
   .9999990992051678 .0013422327863743383
   .9999990911444054 .0013482248934052201
   .9999990830477375 .0013542170003876934
   .9999990749151643 .001360209107321543
   .9999990667466857 .0013662012142065536
   .9999990585423016 .0013721933210425101
   .9999990503020123 .0013781854278291975
   .9999990420258176 .0013841775345664006
   .9999990337137175 .0013901696412539043
   .999999025365712 .0013961617478914935
   .999999016981801 .0014021538544789526
   .9999990085619848 .001408145961016067
   .9999990001062631 .0014141380675026214
   .9999989916146361 .0014201301739384005
   .9999989830871038 .0014261222803231893
   .9999989745236659 .0014321143866567725
   .9999989659243228 .001438106492938935
   .9999989572890743 .0014440985991694619
   .9999989486179204 .0014500907053481378
   .9999989399108612 .0014560828114747475
   .9999989311678965 .0014620749175490758
   .9999989223890265 .001468067023570908
   .9999989135742512 .0014740591295400284
   .9999989047235704 .0014800512354562223
   .9999988958369843 .0014860433413192743
   .9999988869144928 .0014920354471289693
   .9999988779560959 .0014980275528850922
   .9999988689617937 .0015040196585874275
   .9999988599315861 .0015100117642357607
   .999998850865473 .0015160038698298762
   .9999988417634548 .001521995975369559
   .999998832625531 .0015279880808545937
   .9999988234517019 .0015339801862847657
   .9999988142419675 .0015399722916598592
   .9999988049963277 .0015459643969796596
   .9999987957147825 .0015519565022439512
   .9999987863973319 .0015579486074525195
   .9999987770439759 .001563940712605149
   .9999987676547146 .0015699328177016243
   .999998758229548 .0015759249227417307
   .9999987487684759 .0015819170277252528
   .9999987392714985 .0015879091326519755
   .9999987297386157 .0015939012375216837
   .9999987201698276 .0015998933423341623
   .9999987105651341 .001605885447089196
   .9999987009245352 .0016118775517865696
   .999998691248031 .0016178696564260683
   .9999986815356214 .0016238617610074765
   .9999986717873064 .0016298538655305794
   .9999986620030861 .0016358459699951618
   .9999986521829605 .0016418380744010084
   .9999986423269294 .0016478301787479041
   .999998632434993 .0016538222830356339
   .9999986225071512 .0016598143872639823
   .999998612543404 .0016658064914327345
   .9999986025437515 .0016717985955416754
   .9999985925081937 .0016777906995905894
   .9999985824367305 .0016837828035792617
   .9999985723293618 .0016897749075074774
   .999998562186088 .0016957670113750207
   .9999985520069086 .0017017591151816769
   .9999985417918239 .0017077512189272307
   .999998531540834 .001713743322611467
   .9999985212539385 .0017197354262341706
   .9999985109311378 .0017257275297951264
   .9999985005724317 .0017317196332941192
   .9999984901778203 .0017377117367309341
   .9999984797473034 .0017437038401053556
   .9999984692808812 .0017496959434171687
   .9999984587785538 .0017556880466661582
   .9999984482403208 .001761680149852109
   .9999984376661826 .0017676722529748061
   .999998427056139 .0017736643560340342
   .99999841641019 .001779656459029578
   .9999984057283358 .0017856485619612225
   .9999983950105761 .0017916406648287528
   .999998384256911 .0017976327676319532
   .9999983734673407 .001803624870370609
   .9999983626418649 .0018096169730445048
   .9999983517804839 .0018156090756534257
   .9999983408831975 .0018216011781971562
   .9999983299500057 .0018275932806754815
   .9999983189809085 .0018335853830881864
   .999998307975906 .0018395774854350557
   .9999982969349982 .001845569587715874
   .9999982858581851 .0018515616899304264
   .9999982747454665 .001857553792078498
   .9999982635968426 .001863545894159873
   .9999982524123134 .0018695379961743367
   .9999982411918789 .001875530098121674
   .9999982299355389 .0018815222000016696
   .9999982186432936 .0018875143018141083
   .999998207315143 .0018935064035587748
   .999998195951087 .0018994985052354545
   .9999981845511257 .0019054906068439318
   .9999981731152591 .0019114827083839918
   .999998161643487 .001917474809855419
   .9999981501358096 .0019234669112579987
   .999998138592227 .0019294590125915154
   .9999981270127389 .0019354511138557542
   .9999981153973455 .0019414432150504997
   .9999981037460468 .0019474353161755369
   .9999980920588427 .001953427417230651
   .9999980803357332 .001959419518215626
   .9999980685767185 .0019654116191302473
   .9999980567817984 .0019714037199743
   .9999980449509729 .0019773958207475683
   .9999980330842422 .0019833879214498375
   .999998021181606 .001989380022080892
   .9999980092430646 .0019953721226405176
   .9999979972686177 .002001364223128498
   .9999979852582656 .002007356323544619
   .9999979732120081 .002013348423888665
   .9999979611298453 .002019340524160421
   .9999979490117771 .0020253326243596715
   .9999979368578036 .0020313247244862017
   .9999979246679247 .002037316824539796
   .9999979124421405 .00204330892452024
   .999997900180451 .002049301024427318
   .9999978878828562 .0020552931242608153
   .9999978755493559 .002061285224020516
   .9999978631799504 .0020672773237062057
   .9999978507746395 .002073269423317669
   .9999978383334234 .0020792615228546903
   .9999978258563018 .002085253622317055
   .999997813343275 .0020912457217045484
   .9999978007943428 .002097237821016954
   .9999977882095052 .0021032299202540577
   .9999977755887623 .0021092220194156444
   .9999977629321142 .0021152141185014984
   .9999977502395607 .0021212062175114043
   .9999977375111019 .002127198316445148
   .9999977247467376 .0021331904153025134
   .9999977119464681 .002139182514083286
   .9999976991102932 .0021451746127872503
   .9999976862382131 .002151166711414191
   .9999976733302276 .0021571588099638934
   .9999976603863368 .0021631509084361423
   .9999976474065406 .002169143006830722
   .9999976343908391 .002175135105147418
   .9999976213392323 .0021811272033860148
   .9999976082517201 .002187119301546297
   .9999975951283027 .00219311139962805
   .9999975819689799 .0021991034976310588
   .9999975687737518 .0022050955955551076
   .9999975555426184 .0022110876933999816
   .9999975422755796 .0022170797911654654
   .9999975289726355 .002223071888851344
   .9999975156337861 .0022290639864574026
   .9999975022590314 .0022350560839834253
   .9999974888483714 .002241048181429198
   .999997475401806 .0022470402787945045
   .9999974619193353 .00225303237607913
   .9999974484009593 .0022590244732828596
   .9999974348466779 .0022650165704054784
   .9999974212564913 .0022710086674467703
   .9999974076303992 .002277000764406521
   .9999973939684019 .002282992861284515
   .9999973802704993 .0022889849580805368
   .9999973665366915 .0022949770547943723
   .9999973527669782 .0023009691514258054
   .9999973389613596 .002306961247974621
   .9999973251198357 .0023129533444406045
   .9999973112424065 .0023189454408235406
   .999997297329072 .0023249375371232135
   .9999972833798322 .002330929633339409
   .999997269394687 .0023369217294719113
   .9999972553736366 .0023429138255205055
   .9999972413166809 .0023489059214849765
   .9999972272238198 .002354898017365109
   .9999972130950534 .0023608901131606883
   .9999971989303816 .0023668822088714985
   .9999971847298047 .0023728743044973246
   .9999971704933224 .0023788664000379523
   .9999971562209347 .0023848584954931653
   .9999971419126418 .0023908505908627493
   .9999971275684435 .0023968426861464883
   .99999711318834 .002402834781344168
   .9999970987723311 .0024088268764555732
   .9999970843204169 .002414818971480488
   .9999970698325974 .002420811066418698
   .9999970553088726 .0024268031612699878
   .9999970407492426 .002432795256034142
   .9999970261537071 .002438787350710946
   .9999970115222664 .002444779445300184
   .9999969968549204 .0024507715398016418
   .9999969821516691 .002456763634215103
   .9999969674125124 .002462755728540353
   .9999969526374506 .0024687478227771774
   .9999969378264834 .00247473991692536
   .9999969229796108 .002480732010984686
   .999996908096833 .0024867241049549406
   .9999968931781499 .002492716198835908
   .9999968782235614 .0024987082926273734
   .9999968632330677 .002504700386329122
   .9999968482066687 .002510692479940938
   .9999968331443644 .0025166845734626068
   .9999968180461547 .0025226766668939127
   .9999968029120399 .002528668760234641
   .9999967877420196 .002534660853484576
   .9999967725360941 .0025406529466435036
   .9999967572942633 .002546645039711208
   .9999967420165272 .002552637132687474
   .9999967267028858 .002558629225572086
   .9999967113533391 .0025646213183648297
   .9999966959678871 .0025706134110654896
   .9999966805465298 .002576605503673851
   .9999966650892672 .0025825975961896977
   .9999966495960994 .0025885896886128153
   .9999966340670262 .0025945817809429885
   .9999966185020478 .0026005738731800024
   .9999966029011641 .0026065659653236417
   .999996587264375 .002612558057373691
   .9999965715916808 .002618550149329935
   .9999965558830811 .0026245422411921592
   .9999965401385762 .002630534332960148
   .9999965243581661 .002636526424633687
   .9999965085418506 .0026425185162125596
   .9999964926896299 .0026485106076965517
   .9999964768015038 .0026545026990854484
   .9999964608774725 .0026604947903790337
   .9999964449175359 .0026664868815770926
   .999996428921694 .0026724789726794104
   .9999964128899468 .002678471063685772
   .9999963968222944 .0026844631545959617
   .9999963807187366 .002690455245409765
   .9999963645792737 .002696447336126966
   .9999963484039053 .00270243942674735
   .9999963321926317 .002708431517270702
   .9999963159454529 .0027144236076968066
   .9999962996623687 .0027204156980254485
   .9999962833433793 .002726407788256413
   .9999962669884847 .002732399878389485
   .9999962505976846 .0027383919684244484
   .9999962341709794 .002744384058361089
   .9999962177083689 .0027503761481991913
   .999996201209853 .0027563682379385403
   .9999961846754319 .0027623603275789207
   .9999961681051056 .0027683524171201175
   .999996151498874 .002774344506561915
   .9999961348567371 .002780336595904099
   .9999961181786949 .0027863286851464537
   .9999961014647475 .0027923207742887642
   .9999960847148948 .0027983128633308155
   .9999960679291368 .002804304952272392
   .9999960511074735 .002810297041113279
   .9999960342499049 .0028162891298532606
   .9999960173564312 .0028222812184921227
   .9999960004270521 .002828273307029649
   .9999959834617678 .002834265395465626
   .9999959664605781 .0028402574837998367
   .9999959494234832 .002846249572032067
   .9999959323504831 .0028522416601621014
   .9999959152415777 .002858233748189725
   .999995898096767 .002864225836114723
   .9999958809160512 .0028702179239368793
   .9999958636994299 .0028762100116559793
   .9999958464469034 .0028822020992718077
   .9999958291584717 .0028881941867841495
   .9999958118341348 .0028941862741927895
   .9999957944738925 .0029001783614975127
   .999995777077745 .002906170448698104
   .9999957596456922 .0029121625357943475
   .9999957421777342 .002918154622786029
   .999995724673871 .0029241467096729327
   .9999957071341024 .002930138796454844
   .9999956895584287 .0029361308831315474
   .9999956719468496 .0029421229697028273
   .9999956542993652 .0029481150561684695
   .9999956366159757 .0029541071425282584
   .9999956188966809 .002960099228781979
   .9999956011414808 .002966091314929416
   .9999955833503754 .002972083400970354
   .9999955655233649 .0029780754869045785
   .9999955476604491 .0029840675727318736
   .999995529761628 .002990059658452025
   .9999955118269016 .0029960517440648163
   .99999549385627 .0030020438295700336
   .9999954758497331 .0030080359149674612
   .999995457807291 .003014028000256884
   .9999954397289438 .003020020085438087
   .9999954216146911 .0030260121705108552
   .9999954034645333 .003032004255474973
   .9999953852784702 .003037996340330225
   .9999953670565019 .003043988425076397
   .9999953487986284 .003049980509713273
   .9999953305048496 .0030559725942406386
   .9999953121751655 .003061964678658278
   ))


(define high-lut
  (FLOATvector-const
   1. 0.
   .9999999999999999 1.1703344634137277e-8
   .9999999999999998 2.3406689268274554e-8
   .9999999999999993 3.5110033902411824e-8
   .9999999999999989 4.6813378536549095e-8
   .9999999999999983 5.851672317068635e-8
   .9999999999999976 7.022006780482361e-8
   .9999999999999967 8.192341243896085e-8
   .9999999999999957 9.362675707309808e-8
   .9999999999999944 1.0533010170723531e-7
   .9999999999999931 1.170334463413725e-7
   .9999999999999917 1.287367909755097e-7
   .9999999999999901 1.4044013560964687e-7
   .9999999999999885 1.5214348024378403e-7
   .9999999999999866 1.6384682487792116e-7
   .9999999999999846 1.7555016951205827e-7
   .9999999999999825 1.8725351414619535e-7
   .9999999999999802 1.989568587803324e-7
   .9999999999999778 2.1066020341446942e-7
   .9999999999999752 2.2236354804860645e-7
   .9999999999999726 2.3406689268274342e-7
   .9999999999999698 2.4577023731688034e-7
   .9999999999999668 2.5747358195101726e-7
   .9999999999999638 2.6917692658515413e-7
   .9999999999999606 2.8088027121929094e-7
   .9999999999999571 2.9258361585342776e-7
   .9999999999999537 3.042869604875645e-7
   .99999999999995 3.159903051217012e-7
   .9999999999999463 3.276936497558379e-7
   .9999999999999424 3.3939699438997453e-7
   .9999999999999384 3.5110033902411114e-7
   .9999999999999342 3.6280368365824763e-7
   .9999999999999298 3.7450702829238413e-7
   .9999999999999254 3.8621037292652057e-7
   .9999999999999208 3.979137175606569e-7
   .9999999999999161 4.0961706219479325e-7
   .9999999999999113 4.2132040682892953e-7
   .9999999999999063 4.330237514630657e-7
   .9999999999999011 4.447270960972019e-7
   .9999999999998959 4.5643044073133796e-7
   .9999999999998904 4.68133785365474e-7
   .9999999999998849 4.7983712999961e-7
   .9999999999998792 4.915404746337459e-7
   .9999999999998733 5.032438192678817e-7
   .9999999999998674 5.149471639020175e-7
   .9999999999998613 5.266505085361531e-7
   .9999999999998551 5.383538531702888e-7
   .9999999999998487 5.500571978044243e-7
   .9999999999998422 5.617605424385598e-7
   .9999999999998356 5.734638870726952e-7
   .9999999999998288 5.851672317068305e-7
   .9999999999998219 5.968705763409657e-7
   .9999999999998148 6.085739209751009e-7
   .9999999999998076 6.202772656092359e-7
   .9999999999998003 6.319806102433709e-7
   .9999999999997928 6.436839548775058e-7
   .9999999999997853 6.553872995116406e-7
   .9999999999997775 6.670906441457753e-7
   .9999999999997696 6.7879398877991e-7
   .9999999999997616 6.904973334140445e-7
   .9999999999997534 7.02200678048179e-7
   .9999999999997452 7.139040226823132e-7
   .9999999999997368 7.256073673164475e-7
   .9999999999997282 7.373107119505817e-7
   .9999999999997194 7.490140565847157e-7
   .9999999999997107 7.607174012188497e-7
   .9999999999997017 7.724207458529835e-7
   .9999999999996926 7.841240904871172e-7
   .9999999999996834 7.958274351212508e-7
   .9999999999996739 8.075307797553844e-7
   .9999999999996644 8.192341243895178e-7
   .9999999999996547 8.309374690236511e-7
   .999999999999645 8.426408136577842e-7
   .9999999999996351 8.543441582919173e-7
   .999999999999625 8.660475029260503e-7
   .9999999999996148 8.777508475601831e-7
   .9999999999996044 8.894541921943158e-7
   .999999999999594 9.011575368284484e-7
   .9999999999995833 9.128608814625808e-7
   .9999999999995726 9.245642260967132e-7
   .9999999999995617 9.362675707308454e-7
   .9999999999995507 9.479709153649775e-7
   .9999999999995395 9.596742599991095e-7
   .9999999999995283 9.713776046332412e-7
   .9999999999995168 9.83080949267373e-7
   .9999999999995052 9.947842939015044e-7
   .9999999999994935 1.006487638535636e-6
   .9999999999994816 1.0181909831697673e-6
   .9999999999994696 1.0298943278038984e-6
   .9999999999994575 1.0415976724380293e-6
   .9999999999994453 1.0533010170721601e-6
   .9999999999994329 1.065004361706291e-6
   .9999999999994204 1.0767077063404215e-6
   .9999999999994077 1.088411050974552e-6
   .9999999999993949 1.1001143956086822e-6
   .9999999999993819 1.1118177402428122e-6
   .9999999999993688 1.1235210848769423e-6
   .9999999999993556 1.135224429511072e-6
   .9999999999993423 1.1469277741452017e-6
   .9999999999993288 1.1586311187793313e-6
   .9999999999993151 1.1703344634134605e-6
   .9999999999993014 1.1820378080475897e-6
   .9999999999992875 1.1937411526817187e-6
   .9999999999992735 1.2054444973158477e-6
   .9999999999992593 1.2171478419499764e-6
   .9999999999992449 1.2288511865841048e-6
   .9999999999992305 1.2405545312182331e-6
   .999999999999216 1.2522578758523615e-6
   .9999999999992012 1.2639612204864894e-6
   .9999999999991863 1.2756645651206173e-6
   .9999999999991713 1.287367909754745e-6
   .9999999999991562 1.2990712543888725e-6
   .9999999999991409 1.3107745990229998e-6
   .9999999999991255 1.3224779436571269e-6
   .9999999999991099 1.3341812882912537e-6
   .9999999999990943 1.3458846329253806e-6
   .9999999999990785 1.3575879775595072e-6
   .9999999999990625 1.3692913221936337e-6
   .9999999999990464 1.3809946668277597e-6
   .9999999999990302 1.3926980114618857e-6
   .9999999999990138 1.4044013560960117e-6
   .9999999999989974 1.4161047007301373e-6
   .9999999999989807 1.4278080453642627e-6
   .9999999999989639 1.439511389998388e-6
   .999999999998947 1.451214734632513e-6
   .99999999999893 1.462918079266638e-6
   .9999999999989128 1.4746214239007625e-6
   .9999999999988954 1.486324768534887e-6
   .999999999998878 1.4980281131690111e-6
   .9999999999988604 1.5097314578031353e-6
   .9999999999988426 1.5214348024372591e-6
   .9999999999988247 1.5331381470713828e-6
   .9999999999988067 1.544841491705506e-6
   .9999999999987886 1.5565448363396294e-6
   .9999999999987703 1.5682481809737524e-6
   .9999999999987519 1.579951525607875e-6
   .9999999999987333 1.5916548702419977e-6
   .9999999999987146 1.60335821487612e-6
   .9999999999986958 1.615061559510242e-6
   .9999999999986768 1.626764904144364e-6
   .9999999999986577 1.6384682487784858e-6
   .9999999999986384 1.6501715934126072e-6
   .9999999999986191 1.6618749380467283e-6
   .9999999999985996 1.6735782826808495e-6
   .9999999999985799 1.6852816273149702e-6
   .9999999999985602 1.6969849719490907e-6
   .9999999999985402 1.708688316583211e-6
   .9999999999985201 1.720391661217331e-6
   .9999999999985 1.732095005851451e-6
   .9999999999984795 1.7437983504855706e-6
   .9999999999984591 1.7555016951196899e-6
   .9999999999984385 1.767205039753809e-6
   .9999999999984177 1.778908384387928e-6
   .9999999999983968 1.7906117290220465e-6
   .9999999999983759 1.802315073656165e-6
   .9999999999983546 1.814018418290283e-6
   .9999999999983333 1.825721762924401e-6
   .9999999999983119 1.8374251075585186e-6
   .9999999999982904 1.8491284521926361e-6
   .9999999999982686 1.8608317968267533e-6
   .9999999999982468 1.8725351414608702e-6
   .9999999999982249 1.8842384860949866e-6
   .9999999999982027 1.8959418307291031e-6
   .9999999999981805 1.9076451753632194e-6
   .999999999998158 1.919348519997335e-6
   .9999999999981355 1.9310518646314507e-6
   .9999999999981128 1.942755209265566e-6
   .9999999999980901 1.954458553899681e-6
   .9999999999980671 1.966161898533796e-6
   .999999999998044 1.9778652431679103e-6
   .9999999999980208 1.9895685878020246e-6
   .9999999999979975 2.0012719324361386e-6
   .999999999997974 2.012975277070252e-6
   .9999999999979503 2.0246786217043656e-6
   .9999999999979265 2.0363819663384787e-6
   .9999999999979027 2.048085310972592e-6
   .9999999999978786 2.0597886556067045e-6
   .9999999999978545 2.0714920002408167e-6
   .9999999999978302 2.0831953448749286e-6
   .9999999999978058 2.0948986895090404e-6
   .9999999999977811 2.106602034143152e-6
   .9999999999977564 2.118305378777263e-6
   .9999999999977315 2.1300087234113738e-6
   .9999999999977065 2.1417120680454843e-6
   .9999999999976814 2.153415412679595e-6
   .9999999999976561 2.1651187573137046e-6
   .9999999999976307 2.1768221019478143e-6
   .9999999999976051 2.188525446581924e-6
   .9999999999975795 2.200228791216033e-6
   .9999999999975536 2.2119321358501417e-6
   .9999999999975278 2.22363548048425e-6
   .9999999999975017 2.2353388251183586e-6
   .9999999999974754 2.247042169752466e-6
   .999999999997449 2.2587455143865738e-6
   .9999999999974225 2.2704488590206814e-6
   .9999999999973959 2.282152203654788e-6
   .9999999999973691 2.293855548288895e-6
   .9999999999973422 2.305558892923001e-6
   .9999999999973151 2.317262237557107e-6
   .999999999997288 2.328965582191213e-6
   .9999999999972606 2.340668926825318e-6
   .9999999999972332 2.352372271459423e-6
   .9999999999972056 2.364075616093528e-6
   .9999999999971778 2.3757789607276323e-6
   .99999999999715 2.3874823053617365e-6
   .999999999997122 2.3991856499958403e-6
   .9999999999970938 2.4108889946299437e-6
   .9999999999970656 2.4225923392640466e-6
   .9999999999970371 2.4342956838981495e-6
   .9999999999970085 2.445999028532252e-6
   .9999999999969799 2.457702373166354e-6
   .999999999996951 2.4694057178004558e-6
   .999999999996922 2.4811090624345574e-6
   .9999999999968929 2.4928124070686583e-6
   .9999999999968637 2.504515751702759e-6
   .9999999999968343 2.5162190963368595e-6
   .9999999999968048 2.5279224409709594e-6
   .9999999999967751 2.5396257856050594e-6
   .9999999999967454 2.5513291302391585e-6
   .9999999999967154 2.5630324748732576e-6
   .9999999999966853 2.5747358195073563e-6
   .9999999999966551 2.5864391641414546e-6
   .9999999999966248 2.5981425087755525e-6
   .9999999999965944 2.6098458534096503e-6
   .9999999999965637 2.6215491980437473e-6
   .999999999996533 2.6332525426778443e-6
   .9999999999965021 2.644955887311941e-6
   .999999999996471 2.656659231946037e-6
   .99999999999644 2.6683625765801328e-6
   .9999999999964087 2.680065921214228e-6
   .9999999999963772 2.6917692658483234e-6
   .9999999999963456 2.703472610482418e-6
   .999999999996314 2.7151759551165123e-6
   .9999999999962821 2.7268792997506064e-6
   .9999999999962501 2.7385826443846996e-6
   .9999999999962179 2.750285989018793e-6
   .9999999999961857 2.761989333652886e-6
   .9999999999961533 2.7736926782869783e-6
   .9999999999961208 2.78539602292107e-6
   .9999999999960881 2.797099367555162e-6
   .9999999999960553 2.808802712189253e-6
   .9999999999960224 2.8205060568233443e-6
   .9999999999959893 2.832209401457435e-6
   .9999999999959561 2.8439127460915247e-6
   .9999999999959227 2.8556160907256145e-6
   .9999999999958893 2.867319435359704e-6
   .9999999999958556 2.879022779993793e-6
   .9999999999958219 2.8907261246278814e-6
   .9999999999957879 2.90242946926197e-6
   .999999999995754 2.9141328138960576e-6
   .9999999999957198 2.925836158530145e-6
   .9999999999956855 2.9375395031642317e-6
   .999999999995651 2.9492428477983186e-6
   .9999999999956164 2.9609461924324046e-6
   .9999999999955816 2.9726495370664905e-6
   .9999999999955468 2.9843528817005757e-6
   .9999999999955118 2.996056226334661e-6
   .9999999999954767 3.007759570968745e-6
   .9999999999954414 3.0194629156028294e-6
   .999999999995406 3.0311662602369133e-6
   .9999999999953705 3.0428696048709963e-6
   .9999999999953348 3.0545729495050794e-6
   .999999999995299 3.066276294139162e-6
   .999999999995263 3.0779796387732437e-6
   .9999999999952269 3.0896829834073255e-6
   .9999999999951907 3.101386328041407e-6
   .9999999999951543 3.1130896726754873e-6
   .9999999999951178 3.1247930173095678e-6
   .9999999999950812 3.136496361943648e-6
   .9999999999950444 3.148199706577727e-6
   .9999999999950075 3.1599030512118063e-6
   .9999999999949705 3.171606395845885e-6
   .9999999999949333 3.183309740479963e-6
   .999999999994896 3.195013085114041e-6
   .9999999999948584 3.206716429748118e-6
   .9999999999948209 3.218419774382195e-6
   .9999999999947832 3.2301231190162714e-6
   .9999999999947453 3.2418264636503477e-6
   .9999999999947072 3.253529808284423e-6
   .9999999999946692 3.265233152918498e-6
   .9999999999946309 3.276936497552573e-6
   .9999999999945924 3.288639842186647e-6
   .9999999999945539 3.300343186820721e-6
   .9999999999945152 3.312046531454794e-6
   .9999999999944763 3.323749876088867e-6
   .9999999999944373 3.3354532207229395e-6
   .9999999999943983 3.3471565653570115e-6
   .9999999999943591 3.358859909991083e-6
   .9999999999943197 3.370563254625154e-6
   .9999999999942801 3.3822665992592245e-6
   .9999999999942405 3.3939699438932944e-6
   .9999999999942008 3.4056732885273643e-6
   .9999999999941608 3.4173766331614334e-6
   .9999999999941207 3.429079977795502e-6
   .9999999999940805 3.4407833224295702e-6
   .9999999999940402 3.452486667063638e-6
   .9999999999939997 3.4641900116977054e-6
   .999999999993959 3.4758933563317723e-6
   .9999999999939183 3.4875967009658384e-6
   .9999999999938775 3.4993000455999045e-6
   .9999999999938364 3.5110033902339697e-6
   .9999999999937953 3.5227067348680345e-6
   .999999999993754 3.534410079502099e-6
   .9999999999937126 3.546113424136163e-6
   .999999999993671 3.5578167687702264e-6
   .9999999999936293 3.5695201134042896e-6
   .9999999999935875 3.581223458038352e-6
   .9999999999935454 3.592926802672414e-6
   .9999999999935033 3.6046301473064755e-6
   .9999999999934611 3.6163334919405365e-6
   .9999999999934187 3.628036836574597e-6
   .9999999999933762 3.639740181208657e-6
   .9999999999933334 3.6514435258427166e-6
   .9999999999932907 3.6631468704767755e-6
   .9999999999932477 3.674850215110834e-6
   .9999999999932047 3.686553559744892e-6
   .9999999999931615 3.6982569043789496e-6
   .9999999999931181 3.7099602490130064e-6
   .9999999999930747 3.7216635936470627e-6
   .999999999993031 3.733366938281119e-6
   .9999999999929873 3.745070282915174e-6
   .9999999999929433 3.756773627549229e-6
   .9999999999928992 3.768476972183284e-6
   .9999999999928552 3.7801803168173377e-6
   .9999999999928109 3.791883661451391e-6
   .9999999999927663 3.803587006085444e-6
   .9999999999927218 3.8152903507194965e-6
   .9999999999926771 3.826993695353548e-6
   .9999999999926322 3.838697039987599e-6
   .9999999999925873 3.85040038462165e-6
   .9999999999925421 3.862103729255701e-6
   .9999999999924968 3.87380707388975e-6
   .9999999999924514 3.885510418523799e-6
   .9999999999924059 3.897213763157848e-6
   .9999999999923602 3.9089171077918965e-6
   .9999999999923144 3.9206204524259435e-6
   .9999999999922684 3.9323237970599905e-6
   .9999999999922223 3.9440271416940376e-6
   .9999999999921761 3.955730486328084e-6
   .9999999999921297 3.967433830962129e-6
   .9999999999920832 3.9791371755961736e-6
   .9999999999920366 3.990840520230218e-6
   .9999999999919899 4.002543864864262e-6
   .9999999999919429 4.014247209498305e-6
   .9999999999918958 4.025950554132348e-6
   .9999999999918486 4.03765389876639e-6
   .9999999999918013 4.049357243400431e-6
   .9999999999917539 4.061060588034472e-6
   .9999999999917063 4.072763932668513e-6
   .9999999999916586 4.084467277302553e-6
   .9999999999916107 4.096170621936592e-6
   .9999999999915626 4.107873966570632e-6
   .9999999999915146 4.119577311204669e-6
   .9999999999914663 4.131280655838707e-6
   .9999999999914179 4.142984000472745e-6
   .9999999999913692 4.154687345106781e-6
   .9999999999913206 4.166390689740817e-6
   .9999999999912718 4.178094034374852e-6
   .9999999999912228 4.189797379008887e-6
   .9999999999911737 4.201500723642921e-6
   .9999999999911244 4.213204068276955e-6
   .999999999991075 4.224907412910988e-6
   .9999999999910255 4.236610757545021e-6
   .9999999999909759 4.248314102179053e-6
   .9999999999909261 4.260017446813084e-6
   .9999999999908762 4.271720791447115e-6
   .9999999999908261 4.283424136081145e-6
   .9999999999907759 4.295127480715175e-6
   .9999999999907256 4.306830825349204e-6
   .9999999999906751 4.3185341699832325e-6
   .9999999999906245 4.33023751461726e-6
   .9999999999905738 4.3419408592512875e-6
   .9999999999905229 4.353644203885314e-6
   .9999999999904718 4.36534754851934e-6
   .9999999999904207 4.377050893153365e-6
   .9999999999903694 4.38875423778739e-6
   .999999999990318 4.400457582421414e-6
   .9999999999902665 4.4121609270554384e-6
   .9999999999902147 4.423864271689461e-6
   .9999999999901629 4.435567616323483e-6
   .9999999999901109 4.447270960957506e-6
   .9999999999900587 4.458974305591527e-6
   .9999999999900065 4.470677650225547e-6
   .9999999999899541 4.482380994859567e-6
   .9999999999899016 4.494084339493587e-6
   .9999999999898489 4.5057876841276054e-6
   .9999999999897962 4.517491028761624e-6
   .9999999999897432 4.529194373395641e-6
   .9999999999896901 4.5408977180296584e-6
   .999999999989637 4.552601062663675e-6
   .9999999999895836 4.564304407297691e-6
   .99999999998953 4.5760077519317055e-6
   .9999999999894764 4.5877110965657195e-6
   .9999999999894227 4.5994144411997335e-6
   .9999999999893688 4.611117785833747e-6
   .9999999999893148 4.622821130467759e-6
   .9999999999892606 4.634524475101771e-6
   .9999999999892063 4.646227819735783e-6
   .9999999999891518 4.657931164369793e-6
   .9999999999890973 4.669634509003803e-6
   .9999999999890425 4.681337853637813e-6
   .9999999999889877 4.693041198271821e-6
   .9999999999889327 4.704744542905829e-6
   .9999999999888776 4.716447887539837e-6
   .9999999999888223 4.728151232173843e-6
   .9999999999887669 4.73985457680785e-6
   .9999999999887114 4.751557921441855e-6
   .9999999999886556 4.76326126607586e-6
   .9999999999885999 4.774964610709864e-6
   .9999999999885439 4.786667955343868e-6
   .9999999999884878 4.798371299977871e-6
   .9999999999884316 4.810074644611873e-6
   .9999999999883752 4.821777989245874e-6
   .9999999999883187 4.833481333879875e-6
   .9999999999882621 4.845184678513876e-6
   .9999999999882053 4.856888023147875e-6
   .9999999999881484 4.868591367781874e-6
   .9999999999880914 4.880294712415872e-6
   .9999999999880341 4.89199805704987e-6
   .9999999999879768 4.903701401683867e-6
   .9999999999879194 4.915404746317863e-6
   .9999999999878618 4.9271080909518585e-6
   .9999999999878041 4.938811435585853e-6
   .9999999999877462 4.9505147802198475e-6
   .9999999999876882 4.962218124853841e-6
   .99999999998763 4.973921469487834e-6
   .9999999999875717 4.985624814121826e-6
   .9999999999875133 4.997328158755817e-6
   .9999999999874548 5.009031503389808e-6
   .9999999999873961 5.0207348480237985e-6
   .9999999999873372 5.032438192657788e-6
   .9999999999872783 5.0441415372917765e-6
   .9999999999872192 5.055844881925764e-6
   .9999999999871599 5.067548226559752e-6
   .9999999999871007 5.079251571193739e-6
   .9999999999870411 5.090954915827725e-6
   .9999999999869814 5.10265826046171e-6
   .9999999999869217 5.1143616050956945e-6
   .9999999999868617 5.126064949729678e-6
   .9999999999868017 5.1377682943636615e-6
   .9999999999867415 5.149471638997644e-6
   .9999999999866811 5.161174983631626e-6
   .9999999999866207 5.172878328265607e-6
   .9999999999865601 5.184581672899587e-6
   .9999999999864994 5.196285017533567e-6
   .9999999999864384 5.2079883621675455e-6
   .9999999999863775 5.219691706801524e-6
   .9999999999863163 5.2313950514355015e-6
   .999999999986255 5.243098396069478e-6
   .9999999999861935 5.254801740703454e-6
   .999999999986132 5.266505085337429e-6
   .9999999999860703 5.278208429971404e-6
   .9999999999860084 5.289911774605378e-6
   .9999999999859465 5.301615119239351e-6
   .9999999999858843 5.313318463873323e-6
   .9999999999858221 5.325021808507295e-6
   .9999999999857597 5.336725153141267e-6
   .9999999999856971 5.3484284977752366e-6
   .9999999999856345 5.360131842409206e-6
   .9999999999855717 5.371835187043175e-6
   .9999999999855087 5.383538531677143e-6
   .9999999999854456 5.3952418763111104e-6
   .9999999999853825 5.406945220945077e-6
   .9999999999853191 5.418648565579043e-6
   .9999999999852557 5.4303519102130076e-6
   .9999999999851921 5.4420552548469724e-6
   .9999999999851282 5.453758599480936e-6
   .9999999999850644 5.465461944114899e-6
   .9999999999850003 5.47716528874886e-6
   .9999999999849362 5.488868633382822e-6
   .9999999999848719 5.500571978016782e-6
   .9999999999848074 5.512275322650742e-6
   .9999999999847429 5.523978667284702e-6
   .9999999999846781 5.53568201191866e-6
   .9999999999846133 5.547385356552617e-6
   .9999999999845482 5.5590887011865745e-6
   .9999999999844832 5.57079204582053e-6
   .9999999999844179 5.582495390454486e-6
   .9999999999843525 5.59419873508844e-6
   .9999999999842869 5.605902079722394e-6
   .9999999999842213 5.617605424356347e-6
   .9999999999841555 5.629308768990299e-6
   .9999999999840895 5.641012113624251e-6
   .9999999999840234 5.652715458258201e-6
   .9999999999839572 5.664418802892152e-6
   .9999999999838908 5.6761221475261e-6
   .9999999999838243 5.687825492160048e-6
   .9999999999837577 5.699528836793996e-6
   .9999999999836909 5.711232181427943e-6
   .999999999983624 5.722935526061889e-6
   .9999999999835569 5.734638870695834e-6
   .9999999999834898 5.746342215329779e-6
   .9999999999834225 5.758045559963722e-6
   .999999999983355 5.769748904597665e-6
   .9999999999832874 5.781452249231607e-6
   .9999999999832196 5.793155593865548e-6
   .9999999999831518 5.804858938499489e-6
   .9999999999830838 5.816562283133429e-6
   .9999999999830157 5.8282656277673675e-6
   .9999999999829474 5.839968972401306e-6
   .9999999999828789 5.851672317035243e-6
   .9999999999828104 5.86337566166918e-6
   .9999999999827417 5.875079006303115e-6
   .9999999999826729 5.88678235093705e-6
   .9999999999826039 5.898485695570985e-6
   .9999999999825349 5.910189040204917e-6
   .9999999999824656 5.92189238483885e-6
   .9999999999823962 5.933595729472782e-6
   .9999999999823267 5.945299074106713e-6
   .9999999999822571 5.957002418740643e-6
   .9999999999821872 5.9687057633745715e-6
   .9999999999821173 5.9804091080085e-6
   ))

(define (make-w log-n)
  (let* ((n (expt 2 log-n))  ;; number of complexes
	 (result (FLOATmake-vector (* 2 n))))

    (define (copy-low-lut)
      (do ((i 0 (+ i 1)))
	  ((= i lut-table-size))
	(let ((index (* i 2)))
	  (FLOATvector-set!
	   result
	   index
	   (FLOATvector-ref low-lut index))
	  (FLOATvector-set!
	   result
	   (+ index 1)
	   (FLOATvector-ref low-lut (+ index 1))))))

    (define (extend-lut multiplier-lut
                        bit-reverse-size
                        bit-reverse-multiplier
                        start
                        end)

      (define (bit-reverse x n)
	(do ((i 0 (+ i 1))
	     (x x (arithmetic-shift x -1))
	     (result 0 (+ (* result 2)
                          (bitwise-and x 1))))
	    ((= i n) result)))

      (let loop ((i start)
		 (j 1))
	(if (< i end)
	    (let* ((multiplier-index
		    (* 2
                       (* (bit-reverse j bit-reverse-size)
                          bit-reverse-multiplier)))
		   (multiplier-real
		    (FLOATvector-ref multiplier-lut multiplier-index))
		   (multiplier-imag
		    (FLOATvector-ref multiplier-lut (+ multiplier-index 1))))
	      (let inner ((i i)
			  (k 0))
		;; we copy complex multiples of all entries below
		;; start to entries starting at start
		(if (< k start)
		    (let* ((index
			    (* k 2))
			   (real
			    (FLOATvector-ref result index))
			   (imag
			    (FLOATvector-ref result (+ index 1)))
			   (result-real
			    (FLOAT- (FLOAT* multiplier-real real)
                                    (FLOAT* multiplier-imag imag)))
			   (result-imag
			    (FLOAT+ (FLOAT* multiplier-real imag)
                                    (FLOAT* multiplier-imag real)))
			   (result-index (* i 2)))
		      (FLOATvector-set! result result-index result-real)
		      (FLOATvector-set! result (+ result-index 1) result-imag)
		      (inner (+ i 1)
			     (+ k 1)))
		    (loop i
			  (+ j 1)))))
	    result)))

    (cond ((<= n lut-table-size)
	   low-lut)
	  ((<= n lut-table-size^2)
	   (copy-low-lut)
	   (extend-lut med-lut
		       (- log-n log-lut-table-size)
		       (arithmetic-shift 1 (- (* 2 log-lut-table-size) log-n))
		       lut-table-size
		       n))
	  ((<= n lut-table-size^3)
	   (copy-low-lut)
	   (extend-lut med-lut
		       log-lut-table-size
		       1
		       lut-table-size
		       lut-table-size^2)
	   (extend-lut high-lut
		       (- log-n (* 2 log-lut-table-size))
		       (arithmetic-shift 1 (- (* 3 log-lut-table-size) log-n))
		       lut-table-size^2
		       n))
	  (else
	   (error "asking for too large a table")))))

(define (direct-fft-recursive-4 a W-table)

  ;; This is a direcct complex fft, using a decimation-in-time
  ;; algorithm with inputs in natural order and outputs in
  ;; bit-reversed order.  The table of "twiddle" factors is in
  ;; bit-reversed order.

  ;; this is from page 66 of Chu and George, except that we have
  ;; combined passes in pairs to cut the number of passes through
  ;; the vector a

  (let ((W (FLOATvector 0. 0. 0. 0.)))

    (define (main-loop M N K SizeOfGroup)

      (let inner-loop ((K K)
		       (JFirst M))

	(if (< JFirst N)

	    (let* ((JLast  (+ JFirst SizeOfGroup)))

	      (if (even? K)
		  (begin
		    (FLOATvector-set! W 0 (FLOATvector-ref W-table K))
		    (FLOATvector-set! W 1 (FLOATvector-ref W-table (+ K 1))))
		  (begin
		    (FLOATvector-set! W 0 (FLOAT- 0. (FLOATvector-ref W-table K)))
		    (FLOATvector-set! W 1 (FLOATvector-ref W-table (- K 1)))))

	      ;; we know the that the next two complex roots of
	      ;; unity have index 2K and 2K+1 so that the 2K+1
	      ;; index root can be gotten from the 2K index root
	      ;; in the same way that we get W_0 and W_1 from the
	      ;; table depending on whether K is even or not

	      (FLOATvector-set! W 2 (FLOATvector-ref W-table (* K 2)))
	      (FLOATvector-set! W 3 (FLOATvector-ref W-table (+ (* K 2) 1)))

	      (let J-loop ((J0 JFirst))
		(if (< J0 JLast)

		    (let* ((J0 J0)
			   (J1 (+ J0 1))
			   (J2 (+ J0 SizeOfGroup))
			   (J3 (+ J2 1))
			   (J4 (+ J2 SizeOfGroup))
			   (J5 (+ J4 1))
			   (J6 (+ J4 SizeOfGroup))
			   (J7 (+ J6 1)))

		      (let ((W_0  (FLOATvector-ref W 0))
			    (W_1  (FLOATvector-ref W 1))
			    (W_2  (FLOATvector-ref W 2))
			    (W_3  (FLOATvector-ref W 3))
			    (a_J0 (FLOATvector-ref a J0))
			    (a_J1 (FLOATvector-ref a J1))
			    (a_J2 (FLOATvector-ref a J2))
			    (a_J3 (FLOATvector-ref a J3))
			    (a_J4 (FLOATvector-ref a J4))
			    (a_J5 (FLOATvector-ref a J5))
			    (a_J6 (FLOATvector-ref a J6))
			    (a_J7 (FLOATvector-ref a J7)))

			;; first we do the (overlapping) pairs of
			;; butterflies with entries 2*SizeOfGroup
			;; apart.

			(let ((Temp_0 (FLOAT- (FLOAT* W_0 a_J4)
                                              (FLOAT* W_1 a_J5)))
			      (Temp_1 (FLOAT+ (FLOAT* W_0 a_J5)
                                              (FLOAT* W_1 a_J4)))
			      (Temp_2 (FLOAT- (FLOAT* W_0 a_J6)
                                              (FLOAT* W_1 a_J7)))
			      (Temp_3 (FLOAT+ (FLOAT* W_0 a_J7)
                                              (FLOAT* W_1 a_J6))))

			  (let ((a_J0 (FLOAT+ a_J0 Temp_0))
				(a_J1 (FLOAT+ a_J1 Temp_1))
				(a_J2 (FLOAT+ a_J2 Temp_2))
				(a_J3 (FLOAT+ a_J3 Temp_3))
				(a_J4 (FLOAT- a_J0 Temp_0))
				(a_J5 (FLOAT- a_J1 Temp_1))
				(a_J6 (FLOAT- a_J2 Temp_2))
				(a_J7 (FLOAT- a_J3 Temp_3)))

			    ;; now we do the two (disjoint) pairs
			    ;; of butterflies distance SizeOfGroup
			    ;; apart, the first pair with W2+W3i,
			    ;; the second with -W3+W2i

			    ;; we rewrite the multipliers so I
			    ;; don't hurt my head too much when
			    ;; thinking about them.

			    (let ((W_0 W_2)
				  (W_1 W_3)
				  (W_2 (FLOAT- 0. W_3))
				  (W_3 W_2))

			      (let ((Temp_0
				     (FLOAT- (FLOAT* W_0 a_J2)
                                             (FLOAT* W_1 a_J3)))
				    (Temp_1
				     (FLOAT+ (FLOAT* W_0 a_J3)
                                             (FLOAT* W_1 a_J2)))
				    (Temp_2
				     (FLOAT- (FLOAT* W_2 a_J6)
                                             (FLOAT* W_3 a_J7)))
				    (Temp_3
				     (FLOAT+ (FLOAT* W_2 a_J7)
                                             (FLOAT* W_3 a_J6))))

				(let ((a_J0 (FLOAT+ a_J0 Temp_0))
				      (a_J1 (FLOAT+ a_J1 Temp_1))
				      (a_J2 (FLOAT- a_J0 Temp_0))
				      (a_J3 (FLOAT- a_J1 Temp_1))
				      (a_J4 (FLOAT+ a_J4 Temp_2))
				      (a_J5 (FLOAT+ a_J5 Temp_3))
				      (a_J6 (FLOAT- a_J4 Temp_2))
				      (a_J7 (FLOAT- a_J5 Temp_3)))

				  (FLOATvector-set! a J0 a_J0)
				  (FLOATvector-set! a J1 a_J1)
				  (FLOATvector-set! a J2 a_J2)
				  (FLOATvector-set! a J3 a_J3)
				  (FLOATvector-set! a J4 a_J4)
				  (FLOATvector-set! a J5 a_J5)
				  (FLOATvector-set! a J6 a_J6)
				  (FLOATvector-set! a J7 a_J7)

				  (J-loop (+ J0 2)))))))))
		    (inner-loop (+ K 1)
				(+ JFirst (* SizeOfGroup 4)))))))))

    (define (recursive-bit M N K SizeOfGroup)
      (if (<= 2 SizeOfGroup)
	  (begin
	    (main-loop M N K SizeOfGroup)
	    (if (< 2048 (- N M))
		(let ((new-size (arithmetic-shift (- N M) -2)))
		  (recursive-bit M
				 (+ M new-size)
				 (* K 4)
				 (arithmetic-shift SizeOfGroup -2))
		  (recursive-bit (+ M new-size)
				 (+ M (* new-size 2))
				 (+ (* K 4) 1)
				 (arithmetic-shift SizeOfGroup -2))
		  (recursive-bit (+ M (* new-size 2))
				 (+ M (* new-size 3))
				 (+ (* K 4) 2)
				 (arithmetic-shift SizeOfGroup -2))
		  (recursive-bit (+ M (* new-size 3))
				 N
				 (+ (* K 4) 3)
				 (arithmetic-shift SizeOfGroup -2)))
		(recursive-bit M
			       N
			       (* K 4)
			       (arithmetic-shift SizeOfGroup -2))))))

    (define (radix-2-pass a)

      ;; If we're here, the size of our (conceptually complex)
      ;; array is not a power of 4, so we need to do a basic radix
      ;; two pass with w=1 (so W[0]=1.0 and W[1] = 0.)  and then
      ;; call recursive-bit appropriately on the two half arrays.

      (let ((SizeOfGroup
	     (arithmetic-shift (FLOATvector-length a) -1)))
	(let loop ((J0 0))
	  (if (< J0 SizeOfGroup)
	      (let ((J0 J0)
		    (J2 (+ J0 SizeOfGroup)))
		(let ((J1 (+ J0 1))
		      (J3 (+ J2 1)))
		  (let ((a_J0 (FLOATvector-ref a J0))
			(a_J1 (FLOATvector-ref a J1))
			(a_J2 (FLOATvector-ref a J2))
			(a_J3 (FLOATvector-ref a J3)))
		    (let ((a_J0 (FLOAT+ a_J0 a_J2))
			  (a_J1 (FLOAT+ a_J1 a_J3))
			  (a_J2 (FLOAT- a_J0 a_J2))
			  (a_J3 (FLOAT- a_J1 a_J3)))
		      (FLOATvector-set! a J0 a_J0)
		      (FLOATvector-set! a J1 a_J1)
		      (FLOATvector-set! a J2 a_J2)
		      (FLOATvector-set! a J3 a_J3)
		      (loop (+ J0 2))))))))))

    (let* ((n (FLOATvector-length a))
	   (log_n (two^p>=m n)))

      ;; there are n/2 complex entries in a; if n/2 is not a power
      ;; of 4, then do a single radix-2 pass and do the rest of
      ;; the passes as radix-4 passes

      (if (odd? log_n)
	  (recursive-bit 0 n 0 (arithmetic-shift n -2))
	  (let ((n/2 (arithmetic-shift n -1))
		(n/8 (arithmetic-shift n -3)))
	    (radix-2-pass a)
	    (recursive-bit 0 n/2 0 n/8)
	    (recursive-bit n/2 n 1 n/8))))))

(define (inverse-fft-recursive-4 a W-table)

  ;; This is an complex fft, using a decimation-in-frequency algorithm
  ;; with inputs in bit-reversed order and outputs in natural order.

  ;; The organization of the algorithm has little to do with the the
  ;; associated algorithm on page 41 of Chu and George,
  ;; I just reversed the operations of the direct algorithm given
  ;; above (without dividing by 2 each time, so that this has to
  ;; be "normalized" by dividing by N/2 at the end.

  ;; The table of "twiddle" factors is in bit-reversed order.

  (let ((W (FLOATvector 0. 0. 0. 0.)))

    (define (main-loop M N K SizeOfGroup)
      (let inner-loop ((K K)
		       (JFirst M))
	(if (< JFirst N)
	    (let* ((JLast  (+ JFirst SizeOfGroup)))
	      (if (even? K)
		  (begin
		    (FLOATvector-set! W 0 (FLOATvector-ref W-table K))
		    (FLOATvector-set! W 1 (FLOATvector-ref W-table (+ K 1))))
		  (begin
		    (FLOATvector-set! W 0 (FLOAT- 0. (FLOATvector-ref W-table K)))
		    (FLOATvector-set! W 1 (FLOATvector-ref W-table (- K 1)))))
	      (FLOATvector-set! W 2 (FLOATvector-ref W-table (* K 2)))
	      (FLOATvector-set! W 3 (FLOATvector-ref W-table (+ (* K 2) 1)))
	      (let J-loop ((J0 JFirst))
		(if (< J0 JLast)
		    (let* ((J0 J0)
			   (J1 (+ J0 1))
			   (J2 (+ J0 SizeOfGroup))
			   (J3 (+ J2 1))
			   (J4 (+ J2 SizeOfGroup))
			   (J5 (+ J4 1))
			   (J6 (+ J4 SizeOfGroup))
			   (J7 (+ J6 1)))
		      (let ((W_0  (FLOATvector-ref W 0))
			    (W_1  (FLOATvector-ref W 1))
			    (W_2  (FLOATvector-ref W 2))
			    (W_3  (FLOATvector-ref W 3))
			    (a_J0 (FLOATvector-ref a J0))
			    (a_J1 (FLOATvector-ref a J1))
			    (a_J2 (FLOATvector-ref a J2))
			    (a_J3 (FLOATvector-ref a J3))
			    (a_J4 (FLOATvector-ref a J4))
			    (a_J5 (FLOATvector-ref a J5))
			    (a_J6 (FLOATvector-ref a J6))
			    (a_J7 (FLOATvector-ref a J7)))
			(let ((W_00 W_2)
			      (W_01 W_3)
			      (W_02 (FLOAT- 0. W_3))
			      (W_03 W_2))
			  (let ((Temp_0 (FLOAT- a_J0 a_J2))
				(Temp_1 (FLOAT- a_J1 a_J3))
				(Temp_2 (FLOAT- a_J4 a_J6))
				(Temp_3 (FLOAT- a_J5 a_J7)))
			    (let ((a_J0 (FLOAT+ a_J0 a_J2))
				  (a_J1 (FLOAT+ a_J1 a_J3))
				  (a_J4 (FLOAT+ a_J4 a_J6))
				  (a_J5 (FLOAT+ a_J5 a_J7))
				  (a_J2 (FLOAT+ (FLOAT* W_00 Temp_0)
                                                (FLOAT* W_01 Temp_1)))
				  (a_J3 (FLOAT- (FLOAT* W_00 Temp_1)
                                                (FLOAT* W_01 Temp_0)))
				  (a_J6 (FLOAT+ (FLOAT* W_02 Temp_2)
                                                (FLOAT* W_03 Temp_3)))
				  (a_J7 (FLOAT- (FLOAT* W_02 Temp_3)
                                                (FLOAT* W_03 Temp_2))))
			      (let ((Temp_0 (FLOAT- a_J0 a_J4))
				    (Temp_1 (FLOAT- a_J1 a_J5))
				    (Temp_2 (FLOAT- a_J2 a_J6))
				    (Temp_3 (FLOAT- a_J3 a_J7)))
				(let ((a_J0 (FLOAT+ a_J0 a_J4))
				      (a_J1 (FLOAT+ a_J1 a_J5))
				      (a_J2 (FLOAT+ a_J2 a_J6))
				      (a_J3 (FLOAT+ a_J3 a_J7))
				      (a_J4 (FLOAT+ (FLOAT* W_0 Temp_0)
                                                    (FLOAT* W_1 Temp_1)))
				      (a_J5 (FLOAT- (FLOAT* W_0 Temp_1)
                                                    (FLOAT* W_1 Temp_0)))
				      (a_J6 (FLOAT+ (FLOAT* W_0 Temp_2)
                                                    (FLOAT* W_1 Temp_3)))
				      (a_J7 (FLOAT- (FLOAT* W_0 Temp_3)
                                                    (FLOAT* W_1 Temp_2))))
				  (FLOATvector-set! a J0 a_J0)
				  (FLOATvector-set! a J1 a_J1)
				  (FLOATvector-set! a J2 a_J2)
				  (FLOATvector-set! a J3 a_J3)
				  (FLOATvector-set! a J4 a_J4)
				  (FLOATvector-set! a J5 a_J5)
				  (FLOATvector-set! a J6 a_J6)
				  (FLOATvector-set! a J7 a_J7)
				  (J-loop (+ J0 2)))))))))
		    (inner-loop (+ K 1)
				(+ JFirst (* SizeOfGroup 4)))))))))

    (define (recursive-bit M N K SizeOfGroup)
      (if (<= 2 SizeOfGroup)
	  (begin
	    (if (< 2048 (- N M))
		(let ((new-size (arithmetic-shift (- N M) -2)))
		  (recursive-bit M
				 (+ M new-size)
				 (* K 4)
				 (arithmetic-shift SizeOfGroup -2))
		  (recursive-bit (+ M new-size)
				 (+ M (* new-size 2))
				 (+ (* K 4) 1)
				 (arithmetic-shift SizeOfGroup -2))
		  (recursive-bit (+ M (* new-size 2))
				 (+ M (* new-size 3))
				 (+ (* K 4) 2)
				 (arithmetic-shift SizeOfGroup -2))
		  (recursive-bit (+ M (* new-size 3))
				 N
				 (+ (* K 4) 3)
				 (arithmetic-shift SizeOfGroup -2)))
		(recursive-bit M
			       N
			       (* K 4)
			       (arithmetic-shift SizeOfGroup -2)))
	    (main-loop M N K SizeOfGroup))))

    (define (radix-2-pass a)
      (let ((SizeOfGroup
	     (arithmetic-shift (FLOATvector-length a) -1)))
	(let loop ((J0 0))
	  (if (< J0 SizeOfGroup)
	      (let ((J0 J0)
		    (J2 (+ J0 SizeOfGroup)))
		(let ((J1 (+ J0 1))
		      (J3 (+ J2 1)))
		  (let ((a_J0 (FLOATvector-ref a J0))
			(a_J1 (FLOATvector-ref a J1))
			(a_J2 (FLOATvector-ref a J2))
			(a_J3 (FLOATvector-ref a J3)))
		    (let ((a_J0 (FLOAT+ a_J0 a_J2))
			  (a_J1 (FLOAT+ a_J1 a_J3))
			  (a_J2 (FLOAT- a_J0 a_J2))
			  (a_J3 (FLOAT- a_J1 a_J3)))
		      (FLOATvector-set! a J0 a_J0)
		      (FLOATvector-set! a J1 a_J1)
		      (FLOATvector-set! a J2 a_J2)
		      (FLOATvector-set! a J3 a_J3)
		      (loop (+ J0 2))))))))))

    (let* ((n (FLOATvector-length a))
	   (log_n (two^p>=m n)))
      (if (odd? log_n)
	  (recursive-bit 0 n 0 (arithmetic-shift n -2))
	  (let ((n/2 (arithmetic-shift n -1))
		(n/8 (arithmetic-shift n -3)))
	    (recursive-bit 0 n/2 0 n/8)
	    (recursive-bit n/2 n 1 n/8)
	    (radix-2-pass a))))))

(define (two^p>=m m)
  ;; returns smallest p, assumes fixnum m >= 0
  (do ((p 0 (+ p 1))
       (two^p 1 (* two^p 2)))
      ((<= m two^p) p)))

(define n 18) ;; Works on 2^n complex doubles.
(define two^n+1 (expt 2 (+ n 1)))
(define inexact-two^-n (FLOAT/ (exact->inexact (expt 2 n))))

(define data
  ;; A float vector with data[i]=i
  (let ((result (FLOATmake-vector two^n+1)))
    (do ((i 0 (+ i 1)))
        ((= i two^n+1) result)
      (FLOATvector-set! result i (exact->inexact i)))))

(define (run-fftrad4 data)
  (let ((table (make-w (- n 1))))
    (direct-fft-recursive-4 data table)
    (inverse-fft-recursive-4 data table)
    (do ((j 0 (+ j 1)))
        ((= j two^n+1))
      (FLOATvector-set! data j (FLOAT* (FLOATvector-ref data j) inexact-two^-n)))
    (FLOATvector-ref data 3)))

(define (main-fftrad4 . args)
  (run-single-benchmark
   "fftrad4"
   fftrad4-iters
   (lambda (result) (FLOAT<= (FLOATabs (FLOAT- result 3.0)) 1e-4))
   (lambda (data) (lambda () (run-fftrad4 data)))
   data))

;;; FFT - Fast Fourier Transform, translated from "Numerical Recipes in C"

(define (four1 data)
  (let ((n (FLOATvector-length data))
        (pi*2 6.28318530717959)) ; to compute the inverse, negate this value

    ; bit-reversal section

    (let loop1 ((i 0) (j 0))
      (if (< i n)
        (begin
          (if (< i j)
            (begin
              (let ((temp (FLOATvector-ref data i)))
                (FLOATvector-set! data i (FLOATvector-ref data j))
                (FLOATvector-set! data j temp))
              (let ((temp (FLOATvector-ref data (+ i 1))))
                (FLOATvector-set! data (+ i 1) (FLOATvector-ref data (+ j 1)))
                (FLOATvector-set! data (+ j 1) temp))))
          (let loop2 ((m (quotient n 2)) (j j))
            (if (and (>= m 2) (>= j m))
              (loop2 (quotient m 2) (- j m))
              (loop1 (+ i 2) (+ j m)))))))

    ; Danielson-Lanczos section

    (let loop3 ((mmax 2))
      (if (< mmax n)
        (let* ((theta
                (FLOAT/ pi*2 (exact->inexact mmax)))
               (wpr
                (let ((x (FLOATsin (FLOAT* 0.5 theta))))
                  (FLOAT* -2.0 (FLOAT* x x))))
               (wpi
                (FLOATsin theta)))
          (let loop4 ((wr 1.0) (wi 0.0) (m 0))
            (if (< m mmax)
              (begin
                (let loop5 ((i m))
                  (if (< i n)
                    (let* ((j
                            (+ i mmax))
                           (tempr
                            (FLOAT-
                              (FLOAT* wr (FLOATvector-ref data j))
                              (FLOAT* wi (FLOATvector-ref data (+ j 1)))))
                           (tempi
                            (FLOAT+
                              (FLOAT* wr (FLOATvector-ref data (+ j 1)))
                              (FLOAT* wi (FLOATvector-ref data j)))))
                      (FLOATvector-set! data j
                        (FLOAT- (FLOATvector-ref data i) tempr))
                      (FLOATvector-set! data (+ j 1)
                        (FLOAT- (FLOATvector-ref data (+ i 1)) tempi))
                      (FLOATvector-set! data i
                        (FLOAT+ (FLOATvector-ref data i) tempr))
                      (FLOATvector-set! data (+ i 1)
                        (FLOAT+ (FLOATvector-ref data (+ i 1)) tempi))
                      (loop5 (+ j mmax)));***))
                (loop4 (FLOAT+ (FLOAT- (FLOAT* wr wpr) (FLOAT* wi wpi)) wr)
                       (FLOAT+ (FLOAT+ (FLOAT* wi wpr) (FLOAT* wr wpi)) wi)
                       (+ m 2)))))
));******
          (loop3 (* mmax 2)))))))

(define data-fft
  (FLOATmake-vector 1024 0.0))

(define (run-fft data)
  (four1 data)
  (FLOATvector-ref data 0))

(define (main-fft . args)
  (run-single-benchmark
    "fft"
    fft-iters
    (lambda (result) (equal? result 0.0))
    (lambda (data) (lambda () (run-fft data)))
    data-fft))

;;; FIBFP -- Computes fib(35) using floating point

(define (fibfp n)
  (if (FLOAT< n 2.)
    n
    (FLOAT+ (fibfp (FLOAT- n 1.))
            (fibfp (FLOAT- n 2.)))))

(define (main-fibfp . args)
  (run-single-benchmark
    "fibfp"
    fibfp-iters
    (lambda (result) (equal? result 9227465.))
    (lambda (n) (lambda () (fibfp n)))
    35.))


;;; FIB -- A classic benchmark, computes fib(35) inefficiently.

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

(define (main-fib . args)
  (run-single-benchmark
    "fib"
    fib-iters
    (lambda (result) (equal? result 9227465))
    (lambda (n) (lambda () (fib n)))
    35))


;;; GRAPHS -- Obtained from Andrew Wright.

;;; ==== util.ss ====


; Fold over list elements, associating to the left.
(define fold
    (lambda (lst folder state)
;        (assert (list? lst)
;            lst)
;        (assert (procedure? folder)
;            folder)
        (do ((lst lst
                    (cdr lst))
                (state state
                    (folder (car lst)
                        state)))
            ((null? lst)
                state))))

; Given the size of a vector and a procedure which
; sends indicies to desired vector elements, create
; and return the vector.
(define proc->vector
  (lambda (size f)
;    (assert (and (integer? size)
;                 (exact? size)
;                 (>= size 0))
;      size)
;    (assert (procedure? f)
;      f)
    (if (zero? size)
        (vector)
        (let ((x (make-vector size (f 0))))
          (let loop ((i 1))
            (if (< i size)
              (begin
                (vector-set! x i (f i))
                (loop (+ i 1)))))
          x))))

(define vector-fold
    (lambda (vec folder state)
;        (assert (vector? vec)
;            vec)
;        (assert (procedure? folder)
;            folder)
        (let ((len
                    (vector-length vec)))
            (do ((i 0
                        (+ i 1))
                    (state state
                        (folder (vector-ref vec i)
                            state)))
                ((= i len)
                    state)))))

(define vector-map
    (lambda (vec proc)
        (proc->vector (vector-length vec)
            (lambda (i)
                (proc (vector-ref vec i))))))

; Given limit, return the list 0, 1, ..., limit-1.
(define giota
    (lambda (limit)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
        (let _-*-
            ((limit
                    limit)
                (res
                    '()))
            (if (zero? limit)
                res
                (let ((limit
                            (- limit 1)))
                    (_-*- limit
                        (cons limit res)))))))

; Fold over the integers [0, limit).
(define gnatural-fold
    (lambda (limit folder state)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? folder)
;            folder)
        (do ((i 0
                    (+ i 1))
                (state state
                    (folder i state)))
            ((= i limit)
                state))))

; Iterate over the integers [0, limit).
(define gnatural-for-each
    (lambda (limit proc!)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? proc!)
;            proc!)
        (do ((i 0
                    (+ i 1)))
            ((= i limit))
            (proc! i))))

(define natural-for-all?
    (lambda (limit ok?)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? ok?)
;            ok?)
        (let _-*-
            ((i 0))
            (or (= i limit)
                (and (ok? i)
                    (_-*- (+ i 1)))))))

(define natural-there-exists?
    (lambda (limit ok?)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? ok?)
;            ok?)
        (let _-*-
            ((i 0))
            (and (not (= i limit))
                (or (ok? i)
                    (_-*- (+ i 1)))))))

(define there-exists?
    (lambda (lst ok?)
;        (assert (list? lst)
;            lst)
;        (assert (procedure? ok?)
;            ok?)
        (let _-*-
            ((lst lst))
            (and (not (null? lst))
                (or (ok? (car lst))
                    (_-*- (cdr lst)))))))


;;; ==== ptfold.ss ====


; Fold over the tree of permutations of a universe.
; Each branch (from the root) is a permutation of universe.
; Each node at depth d corresponds to all permutations which pick the
; elements spelled out on the branch from the root to that node as
; the first d elements.
; Their are two components to the state:
;       The b-state is only a function of the branch from the root.
;       The t-state is a function of all nodes seen so far.
; At each node, b-folder is called via
;       (b-folder elem b-state t-state deeper accross)
; where elem is the next element of the universe picked.
; If b-folder can determine the result of the total tree fold at this stage,
; it should simply return the result.
; If b-folder can determine the result of folding over the sub-tree
; rooted at the resulting node, it should call accross via
;       (accross new-t-state)
; where new-t-state is that result.
; Otherwise, b-folder should call deeper via
;       (deeper new-b-state new-t-state)
; where new-b-state is the b-state for the new node and new-t-state is
; the new folded t-state.
; At the leaves of the tree, t-folder is called via
;       (t-folder b-state t-state accross)
; If t-folder can determine the result of the total tree fold at this stage,
; it should simply return that result.
; If not, it should call accross via
;       (accross new-t-state)
; Note, fold-over-perm-tree always calls b-folder in depth-first order.
; I.e., when b-folder is called at depth d, the branch leading to that
; node is the most recent calls to b-folder at all the depths less than d.
; This is a gross efficiency hack so that b-folder can use mutation to
; keep the current branch.
(define fold-over-perm-tree
    (lambda (universe b-folder b-state t-folder t-state)
;        (assert (list? universe)
;            universe)
;        (assert (procedure? b-folder)
;            b-folder)
;        (assert (procedure? t-folder)
;            t-folder)
        (let _-*-
            ((universe
                    universe)
                (b-state
                    b-state)
                (t-state
                    t-state)
                (accross
                    (lambda (final-t-state)
                        final-t-state)))
            (if (null? universe)
                (t-folder b-state t-state accross)
                (let _-**-
                    ((in
                            universe)
                        (out
                            '())
                        (t-state
                            t-state))
                    (let* ((first
                                (car in))
                            (rest
                                (cdr in))
                            (accross
                                (if (null? rest)
                                    accross
                                    (lambda (new-t-state)
                                        (_-**- rest
                                            (cons first out)
                                            new-t-state)))))
                        (b-folder first
                            b-state
                            t-state
                            (lambda (new-b-state new-t-state)
                                (_-*- (fold out cons rest)
                                    new-b-state
                                    new-t-state
                                    accross))
                            accross)))))))


;;; ==== minimal.ss ====


; A directed graph is stored as a connection matrix (vector-of-vectors)
; where the first index is the `from' vertex and the second is the `to'
; vertex.  Each entry is a bool indicating if the edge exists.
; The diagonal of the matrix is never examined.
; Make-minimal? returns a procedure which tests if a labelling
; of the verticies is such that the matrix is minimal.
; If it is, then the procedure returns the result of folding over
; the elements of the automoriphism group.  If not, it returns #f.
; The folding is done by calling folder via
;       (folder perm state accross)
; If the folder wants to continue, it should call accross via
;       (accross new-state)
; If it just wants the entire minimal? procedure to return something,
; it should return that.
; The ordering used is lexicographic (with #t > #f) and entries
; are examined in the following order:
;       1->0, 0->1
;
;       2->0, 0->2
;       2->1, 1->2
;
;       3->0, 0->3
;       3->1, 1->3
;       3->2, 2->3
;       ...
(define make-minimal?
    (lambda (max-size)
;        (assert (and (integer? max-size)
;                (exact? max-size)
;                (>= max-size 0))
;            max-size)
        (let ((iotas
                    (proc->vector (+ max-size 1)
                        giota))
                (perm
                    (make-vector max-size 0)))
            (lambda (size graph folder state)
;                (assert (and (integer? size)
;                        (exact? size)
;                        (<= 0 size max-size))
;                    size
;                    max-size)
;                (assert (vector? graph)
;                    graph)
;                (assert (procedure? folder)
;                    folder)
                (fold-over-perm-tree (vector-ref iotas size)
                    (lambda (perm-x x state deeper accross)
                        (case (cmp-next-vertex graph perm x perm-x)
                            ((less)
                                #f)
                            ((equal)
                                (vector-set! perm x perm-x)
                                (deeper (+ x 1)
                                    state))
                            ((more)
                                (accross state))
                            (else
;                                (assert #f)
                                (fatal-error "???"))))
                    0
                    (lambda (leaf-depth state accross)
;                        (assert (eqv? leaf-depth size)
;                            leaf-depth
;                            size)
                        (folder perm state accross))
                    state)))))

; Given a graph, a partial permutation vector, the next input and the next
; output, return 'less, 'equal or 'more depending on the lexicographic
; comparison between the permuted and un-permuted graph.
(define cmp-next-vertex
    (lambda (graph perm x perm-x)
        (let ((from-x
                    (vector-ref graph x))
                (from-perm-x
                    (vector-ref graph perm-x)))
            (let _-*-
                ((y
                        0))
                (if (= x y)
                    'equal
                    (let ((x->y?
                                (vector-ref from-x y))
                            (perm-y
                                (vector-ref perm y)))
                        (cond ((eq? x->y?
                                    (vector-ref from-perm-x perm-y))
                                (let ((y->x?
                                            (vector-ref (vector-ref graph y)
                                                x)))
                                    (cond ((eq? y->x?
                                                (vector-ref (vector-ref graph perm-y)
                                                    perm-x))
                                            (_-*- (+ y 1)))
                                        (y->x?
                                            'less)
                                        (else
                                            'more))))
                            (x->y?
                                'less)
                            (else
                                'more))))))))


;;; ==== rdg.ss ====


; Fold over rooted directed graphs with bounded out-degree.
; Size is the number of verticies (including the root).  Max-out is the
; maximum out-degree for any vertex.  Folder is called via
;       (folder edges state)
; where edges is a list of length size.  The ith element of the list is
; a list of the verticies j for which there is an edge from i to j.
; The last vertex is the root.
(define fold-over-rdg
    (lambda (size max-out folder state)
;        (assert (and (exact? size)
;                (integer? size)
;                (> size 0))
;            size)
;        (assert (and (exact? max-out)
;                (integer? max-out)
;                (>= max-out 0))
;            max-out)
;        (assert (procedure? folder)
;            folder)
        (let* ((root
                    (- size 1))
                (edge?
                    (proc->vector size
                        (lambda (from)
                            (make-vector size #f))))
                (edges
                    (make-vector size '()))
                (out-degrees
                    (make-vector size 0))
                (minimal-folder
                    (make-minimal? root))
                (non-root-minimal?
                    (let ((cont
                                (lambda (perm state accross)
;                                    (assert (eq? state #t)
;                                        state)
                                    (accross #t))))
                        (lambda (size)
                            (minimal-folder size
                                edge?
                                cont
                                #t))))
                (root-minimal?
                    (let ((cont
                                (lambda (perm state accross)
;                                    (assert (eq? state #t)
;                                        state)
                                    (case (cmp-next-vertex edge? perm root root)
                                        ((less)
                                            #f)
                                        ((equal more)
                                            (accross #t))
                                        (else
;                                            (assert #f)
                                            (fatal-error "???"))))))
                        (lambda ()
                            (minimal-folder root
                                edge?
                                cont
                                #t)))))
            (let _-*-
                ((vertex
                        0)
                    (state
                        state))
                (cond ((not (non-root-minimal? vertex))
                        state)
                    ((= vertex root)
;                        (assert
;                            (begin
;                                (gnatural-for-each root
;                                    (lambda (v)
;                                        (assert (= (vector-ref out-degrees v)
;                                                (length (vector-ref edges v)))
;                                            v
;                                            (vector-ref out-degrees v)
;                                            (vector-ref edges v))))
;                                #t))
                        (let ((reach?
                                    (make-reach? root edges))
                                (from-root
                                    (vector-ref edge? root)))
                            (let _-*-
                                ((v
                                        0)
                                    (outs
                                        0)
                                    (efr
                                        '())
                                    (efrr
                                        '())
                                    (state
                                        state))
                                (cond ((not (or (= v root)
                                                (= outs max-out)))
                                        (vector-set! from-root v #t)
                                        (let ((state
                                                    (_-*- (+ v 1)
                                                        (+ outs 1)
                                                        (cons v efr)
                                                        (cons (vector-ref reach? v)
                                                            efrr)
                                                        state)))
                                            (vector-set! from-root v #f)
                                            (_-*- (+ v 1)
                                                outs
                                                efr
                                                efrr
                                                state)))
                                    ((and (natural-for-all? root
                                                (lambda (v)
                                                    (there-exists? efrr
                                                        (lambda (r)
                                                            (vector-ref r v)))))
                                            (root-minimal?))
                                        (vector-set! edges root efr)
                                        (folder
                                            (proc->vector size
                                                (lambda (i)
                                                    (vector-ref edges i)))
                                            state))
                                    (else
                                        state)))))
                    (else
                        (let ((from-vertex
                                    (vector-ref edge? vertex)))
                            (let _-**-
                                ((sv
                                        0)
                                    (outs
                                        0)
                                    (state
                                        state))
                                (if (= sv vertex)
                                    (begin
                                        (vector-set! out-degrees vertex outs)
                                        (_-*- (+ vertex 1)
                                            state))
                                    (let* ((state
                                                ; no sv->vertex, no vertex->sv
                                                (_-**- (+ sv 1)
                                                    outs
                                                    state))
                                            (from-sv
                                                (vector-ref edge? sv))
                                            (sv-out
                                                (vector-ref out-degrees sv))
                                            (state
                                                (if (= sv-out max-out)
                                                    state
                                                    (begin
                                                        (vector-set! edges
                                                            sv
                                                            (cons vertex
                                                                (vector-ref edges sv)))
                                                        (vector-set! from-sv vertex #t)
                                                        (vector-set! out-degrees sv (+ sv-out 1))
                                                        (let* ((state
                                                                    ; sv->vertex, no vertex->sv
                                                                    (_-**- (+ sv 1)
                                                                        outs
                                                                        state))
                                                                (state
                                                                    (if (= outs max-out)
                                                                        state
                                                                        (begin
                                                                            (vector-set! from-vertex sv #t)
                                                                            (vector-set! edges
                                                                                vertex
                                                                                (cons sv
                                                                                    (vector-ref edges vertex)))
                                                                            (let ((state
                                                                                        ; sv->vertex, vertex->sv
                                                                                        (_-**- (+ sv 1)
                                                                                            (+ outs 1)
                                                                                            state)))
                                                                                (vector-set! edges
                                                                                    vertex
                                                                                    (cdr (vector-ref edges vertex)))
                                                                                (vector-set! from-vertex sv #f)
                                                                                state)))))
                                                            (vector-set! out-degrees sv sv-out)
                                                            (vector-set! from-sv vertex #f)
                                                            (vector-set! edges
                                                                sv
                                                                (cdr (vector-ref edges sv)))
                                                            state)))))
                                        (if (= outs max-out)
                                            state
                                            (begin
                                                (vector-set! edges
                                                    vertex
                                                    (cons sv
                                                        (vector-ref edges vertex)))
                                                (vector-set! from-vertex sv #t)
                                                (let ((state
                                                            ; no sv->vertex, vertex->sv
                                                            (_-**- (+ sv 1)
                                                                (+ outs 1)
                                                                state)))
                                                    (vector-set! from-vertex sv #f)
                                                    (vector-set! edges
                                                        vertex
                                                        (cdr (vector-ref edges vertex)))
                                                    state)))))))))))))

; Given a vector which maps vertex to out-going-edge list,
; return a vector  which gives reachability.
(define make-reach?
    (lambda (size vertex->out)
        (let ((res
                    (proc->vector size
                        (lambda (v)
                            (let ((from-v
                                        (make-vector size #f)))
                                (vector-set! from-v v #t)
                                (for-each
                                    (lambda (x)
                                        (vector-set! from-v x #t))
                                    (vector-ref vertex->out v))
                                from-v)))))
            (gnatural-for-each size
                (lambda (m)
                    (let ((from-m
                                (vector-ref res m)))
                        (gnatural-for-each size
                            (lambda (f)
                                (let ((from-f
                                            (vector-ref res f)))
                                    (if (vector-ref from-f m)
                                        (gnatural-for-each size
                                            (lambda (t)
                                                (if (vector-ref from-m t)
                                                    (vector-set! from-f t #t)))))))))))
            res)))


;;; ==== test input ====

; Produces all directed graphs with N verticies, distinguished root,
; and out-degree bounded by 2, upto isomorphism.

(define (run n)
  (fold-over-rdg n
    2
    cons
    '()))

(define (main-graphs)
  (run-single-benchmark
   "graphs"
   graphs-iters
   (lambda (result) (equal? (length result) 596))
   (lambda (n) (lambda () (run n)))
   5))


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

(define (lattice-run)
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

(define (main-lattice)
  (run-single-benchmark
   "lattice"
   lattice-iters
   (lambda (result) (equal? result 120549))
   (lambda () (lambda () (lattice-run)))))


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

(define (main-mazefun . args)
  (run-single-benchmark
    "mazefun"
    mazefun-iters
    (lambda (result)
      (equal? result expected-result))
    (lambda (n m) (lambda () (make-maze n m)))
    11
    11))


;;; MBROT -- Generation of Mandelbrot set fractal.

(define (count r i step x y)

  (let ((max-count 64)
        (radius^2  16.0))

    (let ((cr (FLOAT+ r (FLOAT* (exact->inexact x) step)))
          (ci (FLOAT+ i (FLOAT* (exact->inexact y) step))))

      (let loop ((zr cr)
                 (zi ci)
                 (c 0))
        (if (= c max-count)
          c
          (let ((zr^2 (FLOAT* zr zr))
                (zi^2 (FLOAT* zi zi)))
            (if (FLOAT> (FLOAT+ zr^2 zi^2) radius^2)
              c
              (let ((new-zr (FLOAT+ (FLOAT- zr^2 zi^2) cr))
                    (new-zi (FLOAT+ (FLOAT* 2.0 (FLOAT* zr zi)) ci)))
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

(define (mbrot-test n)
  (let ((matrix (make-vector n)))
    (let loop ((i (- n 1)))
      (if (>= i 0)
        (begin
          (vector-set! matrix i (make-vector n))
          (loop (- i 1)))))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (vector-ref (vector-ref matrix 0) 0)))

(define (main-mbrot . args)
  (run-single-benchmark
    "mbrot"
    mbrot-iters
    (lambda (result) (equal? result 5))
    (lambda (n) (lambda () (mbrot-test n)))
    75))

;; NBODY

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

(define (run-nbody n)
  (let ((system (list *sun* *jupiter* *saturn* *uranus* *neptune*)))
    (offset-momentum system)
    (let ((before (energy system)))
      (do ((i 1 (+ i 1)))
          ((< n i))
        (advance system 0.01))
      (let ((after (energy system)))
        (cons before after)))))

(define (main-nbody . args)
  (run-single-benchmark
    "nbody"
    nbody-iters
    (lambda (result)
      (and (< (+ 0.169075164 (car result)) 0.000000001)
           (< (+ 0.169086185 (cdr result)) 0.000000001)))
    (lambda (data) (lambda () (run-nbody data)))
    1000000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         nboyer.sch
; Description:  The Boyer benchmark
; Author:       Bob Boyer
; Created:      5-Apr-85
; Modified:     10-Apr-85 14:52:20 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
;               2-Jul-88 (Will Clinger -- distinguished #f and the empty list)
;               13-Feb-97 (Will Clinger -- fixed bugs in unifier and rules,
;                          rewrote to eliminate property lists, and added
;                          a scaling parameter suggested by Bob Boyer)
;               19-Mar-99 (Will Clinger -- cleaned up comments)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; NBOYER -- Logic programming benchmark, originally written by Bob Boyer.
;;; Fairly CONS intensive.

; Note:  The version of this benchmark that appears in Dick Gabriel's book
; contained several bugs that are corrected here.  These bugs are discussed
; by Henry Baker, "The Boyer Benchmark Meets Linear Logic", ACM SIGPLAN Lisp
; Pointers 6(4), October-December 1993, pages 3-10.  The fixed bugs are:
;
;    The benchmark now returns a boolean result.
;    FALSEP and TRUEP use TERM-MEMBER? rather than MEMV (which is called MEMBER
;         in Common Lisp)
;    ONE-WAY-UNIFY1 now treats numbers correctly
;    ONE-WAY-UNIFY1-LST now treats empty lists correctly
;    Rule 19 has been corrected (this rule was not touched by the original
;         benchmark, but is used by this version)
;    Rules 84 and 101 have been corrected (but these rules are never touched
;         by the benchmark)
;
; According to Baker, these bug fixes make the benchmark 10-25% slower.
; Please do not compare the timings from this benchmark against those of
; the original benchmark.
;
; This version of the benchmark also prints the number of rewrites as a sanity
; check, because it is too easy for a buggy version to return the correct
; boolean result.  The correct number of rewrites is
;
;     n      rewrites       peak live storage (approximate, in bytes)
;     0         95024           520,000
;     1        591777         2,085,000
;     2       1813975         5,175,000
;     3       5375678
;     4      16445406
;     5      51507739

; Nboyer is a 2-phase benchmark.
; The first phase attaches lemmas to symbols.  This phase is not timed,
; but it accounts for very little of the runtime anyway.
; The second phase creates the test problem, and tests to see
; whether it is implied by the lemmas.

(define (main-nboyer . args)
  (let ((n (if (null? args) 0 (car args))))
    (setup-nboyer)
    (run-single-benchmark
     (string-append "nboyer"
                    (number->string n))
     nboyer-iters
     (lambda (rewrites)
       (and (number? rewrites)
            (case n
              ((0)  (= rewrites 95024))
              ((1)  (= rewrites 591777))
              ((2)  (= rewrites 1813975))
              ((3)  (= rewrites 5375678))
              ((4)  (= rewrites 16445406))
              ((5)  (= rewrites 51507739))
              ; If it works for n <= 5, assume it works.
              (else #t))))
     (lambda (alist term n) (lambda () (test-nboyer alist term n)))
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
                     (implies x w)))
     n)))

(define (setup-nboyer) #t) ; assigned below
(define (test-nboyer) #t)  ; assigned below

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The first phase.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In the original benchmark, it stored a list of lemmas on the
; property lists of symbols.
; In the new benchmark, it maintains an association list of
; symbols and symbol-records, and stores the list of lemmas
; within the symbol-records.

(let ()

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
                                (f))))
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
             (equal (greatereqp x y)
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
                        (equal (t) z)
                        (equal (f) z)))
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

  (define (add-lemma-lst lst)
    (cond ((null? lst)
           #t)
          (else (add-lemma (car lst))
                (add-lemma-lst (cdr lst)))))

  (define (add-lemma term)
    (cond ((and (pair? term)
                (eq? (car term)
                     (quote equal))
                (pair? (cadr term)))
           (put (car (cadr term))
                (quote lemmas)
                (cons
                 (translate-term term)
                 (get (car (cadr term)) (quote lemmas)))))
          (else (fatal-error "ADD-LEMMA did not like term:  " term))))

  ; Translates a term by replacing its constructor symbols by symbol-records.

  (define (translate-term term)
    (cond ((not (pair? term))
           term)
          (else (cons (symbol->symbol-record (car term))
                      (translate-args (cdr term))))))

  (define (translate-args lst)
    (cond ((null? lst)
           '())
          (else (cons (translate-term (car lst))
                      (translate-args (cdr lst))))))

  ; For debugging only, so the use of MAP does not change
  ; the first-order character of the benchmark.

  (define (untranslate-term term)
    (cond ((not (pair? term))
           term)
          (else (cons (get-name (car term))
                      (map untranslate-term (cdr term))))))

  ; A symbol-record is represented as a vector with two fields:
  ; the symbol (for debugging) and
  ; the list of lemmas associated with the symbol.

  (define (put sym property value)
    (put-lemmas! (symbol->symbol-record sym) value))

  (define (get sym property)
    (get-lemmas (symbol->symbol-record sym)))

  (define (symbol->symbol-record sym)
    (let ((x (assq sym *symbol-records-alist*)))
      (if x
          (cdr x)
          (let ((r (make-symbol-record sym)))
            (set! *symbol-records-alist*
                  (cons (cons sym r)
                        *symbol-records-alist*))
            r))))

  ; Association list of symbols and symbol-records.

  (define *symbol-records-alist* '())

  ; A symbol-record is represented as a vector with two fields:
  ; the symbol (for debugging) and
  ; the list of lemmas associated with the symbol.

  (define (make-symbol-record sym)
    (vector sym '()))

  (define (put-lemmas! symbol-record lemmas)
    (vector-set! symbol-record 1 lemmas))

  (define (get-lemmas symbol-record)
    (vector-ref symbol-record 1))

  (define (get-name symbol-record)
    (vector-ref symbol-record 0))

  (define (symbol-record-equal? r1 r2)
    (eq? r1 r2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; The second phase.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (test alist term n)
    (let ((term
           (apply-subst
            (translate-alist alist)
            (translate-term
             (do ((term term (list 'or term '(f)))
                  (n n (- n 1)))
                 ((zero? n) term))))))
    (tautp term)))

  (define (translate-alist alist)
    (cond ((null? alist)
           '())
          (else (cons (cons (caar alist)
                            (translate-term (cdar alist)))
                      (translate-alist (cdr alist))))))

  (define (apply-subst alist term)
    (cond ((not (pair? term))
           (let ((temp-temp (assq term alist)))
             (if temp-temp
                 (cdr temp-temp)
                 term)))
          (else (cons (car term)
                      (apply-subst-lst alist (cdr term))))))

  (define (apply-subst-lst alist lst)
    (cond ((null? lst)
           '())
          (else (cons (apply-subst alist (car lst))
                      (apply-subst-lst alist (cdr lst))))))

  (define (tautp x)
    (tautologyp (rewrite x)
                '() '()))

  (define (tautologyp x true-lst false-lst)
    (cond ((truep x true-lst)
           #t)
          ((falsep x false-lst)
           #f)
          ((not (pair? x))
           #f)
          ((eq? (car x) if-constructor)
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

  (define if-constructor '*) ; becomes (symbol->symbol-record 'if)

  (define rewrite-count 0) ; sanity check

  (define (rewrite term)
    (set! rewrite-count (+ rewrite-count 1))
    (cond ((not (pair? term))
           term)
          (else (rewrite-with-lemmas (cons (car term)
                                           (rewrite-args (cdr term)))
                                     (get-lemmas (car term))))))

  (define (rewrite-args lst)
    (cond ((null? lst)
           '())
          (else (cons (rewrite (car lst))
                      (rewrite-args (cdr lst))))))

  (define (rewrite-with-lemmas term lst)
    (cond ((null? lst)
           term)
          ((one-way-unify term (cadr (car lst)))
           (rewrite (apply-subst unify-subst (caddr (car lst)))))
          (else (rewrite-with-lemmas term (cdr lst)))))

  (define unify-subst '*)

  (define (one-way-unify term1 term2)
    (begin (set! unify-subst '())
           (one-way-unify1 term1 term2)))

  (define (one-way-unify1 term1 term2)
    (cond ((not (pair? term2))
           (let ((temp-temp (assq term2 unify-subst)))
             (cond (temp-temp
                    (term-equal? term1 (cdr temp-temp)))
                   ((number? term2)          ; This bug fix makes
                    (equal? term1 term2))    ; nboyer 10-25% slower!
                   (else
                    (set! unify-subst (cons (cons term2 term1)
                                            unify-subst))
                    #t))))
          ((not (pair? term1))
           #f)
          ((eq? (car term1)
                (car term2))
           (one-way-unify1-lst (cdr term1)
                               (cdr term2)))
          (else #f)))

  (define (one-way-unify1-lst lst1 lst2)
    (cond ((null? lst1)
           (null? lst2))
          ((null? lst2)
           #f)
          ((one-way-unify1 (car lst1)
                           (car lst2))
           (one-way-unify1-lst (cdr lst1)
                               (cdr lst2)))
          (else #f)))

  (define (falsep x lst)
    (or (term-equal? x false-term)
        (term-member? x lst)))

  (define (truep x lst)
    (or (term-equal? x true-term)
        (term-member? x lst)))

  (define false-term '*)  ; becomes (translate-term '(f))
  (define true-term '*)   ; becomes (translate-term '(t))

  ; The next two procedures were in the original benchmark
  ; but were never used.

  (define (trans-of-implies n)
    (translate-term
     (list (quote implies)
           (trans-of-implies1 n)
           (list (quote implies)
                 0 n))))

  (define (trans-of-implies1 n)
    (cond ((equal? n 1)
           (list (quote implies)
                 0 1))
          (else (list (quote and)
                      (list (quote implies)
                            (- n 1)
                            n)
                      (trans-of-implies1 (- n 1))))))

  ; Translated terms can be circular structures, which can't be
  ; compared using Scheme's equal? and member procedures, so we
  ; use these instead.

  (define (term-equal? x y)
    (cond ((pair? x)
           (and (pair? y)
                (symbol-record-equal? (car x) (car y))
                (term-args-equal? (cdr x) (cdr y))))
          (else (equal? x y))))

  (define (term-args-equal? lst1 lst2)
    (cond ((null? lst1)
           (null? lst2))
          ((null? lst2)
           #f)
          ((term-equal? (car lst1) (car lst2))
           (term-args-equal? (cdr lst1) (cdr lst2)))
          (else #f)))

  (define (term-member? x lst)
    (cond ((null? lst)
           #f)
          ((term-equal? x (car lst))
           #t)
          (else (term-member? x (cdr lst)))))

  (set! setup-nboyer
        (lambda ()
          (set! *symbol-records-alist* '())
          (set! if-constructor (symbol->symbol-record 'if))
          (set! false-term (translate-term '(f)))
          (set! true-term  (translate-term '(t)))
          (setup)))

  (set! test-nboyer
        (lambda (alist term n)
          (set! rewrite-count 0)
          (let ((answer (test alist term n)))
;            (write rewrite-count)
;            (display " rewrites")
;            (newline)
            (if answer
                rewrite-count
                #f)))))


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

(define (main-nqueens)
  (run-single-benchmark
   "nqueens"
   nqueens-iters
   (lambda (result) (equal? result 92))
   (lambda (n) (lambda () (nqueens n)))
   8))

;;; NUCLEIC -- 3D structure determination of a nucleic acid.

; Author: Marc Feeley (feeley@iro.umontreal.ca)
;
; Last modified: January 27, 1996
;
; This program is a modified version of the program described in the paper:
;
;   M. Feeley, M. Turcotte, G. Lapalme, "Using Multilisp for Solving
;   Constraint Satisfaction Problems: an Application to Nucleic Acid 3D
;   Structure Determination" published in the journal "Lisp and Symbolic
;   Computation".
;
; The differences between this program and the original are described in
; the paper:
;
;   "???" published in the "Journal of Functional Programming".

; -- MATH UTILITIES -----------------------------------------------------------

(define constant-pi          3.14159265358979323846)
(define constant-minus-pi   -3.14159265358979323846)
(define constant-pi/2        1.57079632679489661923)
(define constant-minus-pi/2 -1.57079632679489661923)

(define (math-atan2 y x)
  (cond ((FLOAT> x 0.0)
         (FLOATatan (FLOAT/ y x)))
        ((FLOAT< y 0.0)
         (if (FLOAT= x 0.0)
           constant-minus-pi/2
           (FLOAT+ (FLOATatan (FLOAT/ y x)) constant-minus-pi)))
        (else
         (if (FLOAT= x 0.0)
           constant-pi/2
           (FLOAT+ (FLOATatan (FLOAT/ y x)) constant-pi)))))

; -- POINTS -------------------------------------------------------------------

(define (make-pt x y z)
  (FLOATvector x y z))

(define (pt-x pt) (FLOATvector-ref pt 0))
(define (pt-x-set! pt val) (FLOATvector-set! pt 0 val))
(define (pt-y pt) (FLOATvector-ref pt 1))
(define (pt-y-set! pt val) (FLOATvector-set! pt 1 val))
(define (pt-z pt) (FLOATvector-ref pt 2))
(define (pt-z-set! pt val) (FLOATvector-set! pt 2 val))

(define (pt-sub p1 p2)
  (make-pt (FLOAT- (pt-x p1) (pt-x p2))
           (FLOAT- (pt-y p1) (pt-y p2))
           (FLOAT- (pt-z p1) (pt-z p2))))

(define (pt-dist p1 p2)
  (let ((dx (FLOAT- (pt-x p1) (pt-x p2)))
        (dy (FLOAT- (pt-y p1) (pt-y p2)))
        (dz (FLOAT- (pt-z p1) (pt-z p2))))
    (FLOATsqrt (FLOAT+ (FLOAT* dx dx) (FLOAT* dy dy) (FLOAT* dz dz)))))

(define (pt-phi p)
  (let* ((x (pt-x p))
         (y (pt-y p))
         (z (pt-z p))
         (b (math-atan2 x z)))
    (math-atan2 (FLOAT+ (FLOAT* (FLOATcos b) z) (FLOAT* (FLOATsin b) x)) y)))

(define (pt-theta p)
  (math-atan2 (pt-x p) (pt-z p)))

; -- COORDINATE TRANSFORMATIONS -----------------------------------------------

; The notation for the transformations follows "Paul, R.P. (1981) Robot
; Manipulators.  MIT Press." with the exception that our transformation
; matrices don't have the perspective terms and are the transpose of
; Paul's one.  See also "M\"antyl\"a, M. (1985) An Introduction to
; Solid Modeling, Computer Science Press" Appendix A.
;
; The components of a transformation matrix are named like this:
;
;  a  b  c
;  d  e  f
;  g  h  i
; tx ty tz
;
; The components tx, ty, and tz are the translation vector.

(define (make-tfo a b c d e f g h i tx ty tz)
  (FLOATvector a b c d e f g h i tx ty tz))

(define (tfo-a tfo) (FLOATvector-ref tfo 0))
(define (tfo-a-set! tfo val) (FLOATvector-set! tfo 0 val))
(define (tfo-b tfo) (FLOATvector-ref tfo 1))
(define (tfo-b-set! tfo val) (FLOATvector-set! tfo 1 val))
(define (tfo-c tfo) (FLOATvector-ref tfo 2))
(define (tfo-c-set! tfo val) (FLOATvector-set! tfo 2 val))
(define (tfo-d tfo) (FLOATvector-ref tfo 3))
(define (tfo-d-set! tfo val) (FLOATvector-set! tfo 3 val))
(define (tfo-e tfo) (FLOATvector-ref tfo 4))
(define (tfo-e-set! tfo val) (FLOATvector-set! tfo 4 val))
(define (tfo-f tfo) (FLOATvector-ref tfo 5))
(define (tfo-f-set! tfo val) (FLOATvector-set! tfo 5 val))
(define (tfo-g tfo) (FLOATvector-ref tfo 6))
(define (tfo-g-set! tfo val) (FLOATvector-set! tfo 6 val))
(define (tfo-h tfo) (FLOATvector-ref tfo 7))
(define (tfo-h-set! tfo val) (FLOATvector-set! tfo 7 val))
(define (tfo-i tfo) (FLOATvector-ref tfo 8))
(define (tfo-i-set! tfo val) (FLOATvector-set! tfo 8 val))
(define (tfo-tx tfo) (FLOATvector-ref tfo 9))
(define (tfo-tx-set! tfo val) (FLOATvector-set! tfo 9 val))
(define (tfo-ty tfo) (FLOATvector-ref tfo 10))
(define (tfo-ty-set! tfo val) (FLOATvector-set! tfo 10 val))
(define (tfo-tz tfo) (FLOATvector-ref tfo 11))
(define (tfo-tz-set! tfo val) (FLOATvector-set! tfo 11 val))

(define tfo-id  ; the identity transformation matrix
  (FLOATvector-const
     1.0 0.0 0.0
     0.0 1.0 0.0
     0.0 0.0 1.0
     0.0 0.0 0.0))

; The function "tfo-apply" multiplies a transformation matrix, tfo, by a
; point vector, p.  The result is a new point.

(define (tfo-apply tfo p)
  (let ((x (pt-x p))
        (y (pt-y p))
        (z (pt-z p)))
    (make-pt
     (FLOAT+ (FLOAT* x (tfo-a tfo))
             (FLOAT* y (tfo-d tfo))
             (FLOAT* z (tfo-g tfo))
             (tfo-tx tfo))
     (FLOAT+ (FLOAT* x (tfo-b tfo))
             (FLOAT* y (tfo-e tfo))
             (FLOAT* z (tfo-h tfo))
             (tfo-ty tfo))
     (FLOAT+ (FLOAT* x (tfo-c tfo))
             (FLOAT* y (tfo-f tfo))
             (FLOAT* z (tfo-i tfo))
             (tfo-tz tfo)))))

; The function "tfo-combine" multiplies two transformation matrices A and B.
; The result is a new matrix which cumulates the transformations described
; by A and B.

(define (tfo-combine A B)
  (make-tfo
   (FLOAT+ (FLOAT* (tfo-a A) (tfo-a B))
           (FLOAT* (tfo-b A) (tfo-d B))
           (FLOAT* (tfo-c A) (tfo-g B)))
   (FLOAT+ (FLOAT* (tfo-a A) (tfo-b B))
           (FLOAT* (tfo-b A) (tfo-e B))
           (FLOAT* (tfo-c A) (tfo-h B)))
   (FLOAT+ (FLOAT* (tfo-a A) (tfo-c B))
           (FLOAT* (tfo-b A) (tfo-f B))
           (FLOAT* (tfo-c A) (tfo-i B)))
   (FLOAT+ (FLOAT* (tfo-d A) (tfo-a B))
           (FLOAT* (tfo-e A) (tfo-d B))
           (FLOAT* (tfo-f A) (tfo-g B)))
   (FLOAT+ (FLOAT* (tfo-d A) (tfo-b B))
           (FLOAT* (tfo-e A) (tfo-e B))
           (FLOAT* (tfo-f A) (tfo-h B)))
   (FLOAT+ (FLOAT* (tfo-d A) (tfo-c B))
           (FLOAT* (tfo-e A) (tfo-f B))
           (FLOAT* (tfo-f A) (tfo-i B)))
   (FLOAT+ (FLOAT* (tfo-g A) (tfo-a B))
           (FLOAT* (tfo-h A) (tfo-d B))
           (FLOAT* (tfo-i A) (tfo-g B)))
   (FLOAT+ (FLOAT* (tfo-g A) (tfo-b B))
           (FLOAT* (tfo-h A) (tfo-e B))
           (FLOAT* (tfo-i A) (tfo-h B)))
   (FLOAT+ (FLOAT* (tfo-g A) (tfo-c B))
           (FLOAT* (tfo-h A) (tfo-f B))
           (FLOAT* (tfo-i A) (tfo-i B)))
   (FLOAT+ (FLOAT* (tfo-tx A) (tfo-a B))
           (FLOAT* (tfo-ty A) (tfo-d B))
           (FLOAT* (tfo-tz A) (tfo-g B))
           (tfo-tx B))
   (FLOAT+ (FLOAT* (tfo-tx A) (tfo-b B))
           (FLOAT* (tfo-ty A) (tfo-e B))
           (FLOAT* (tfo-tz A) (tfo-h B))
           (tfo-ty B))
   (FLOAT+ (FLOAT* (tfo-tx A) (tfo-c B))
           (FLOAT* (tfo-ty A) (tfo-f B))
           (FLOAT* (tfo-tz A) (tfo-i B))
           (tfo-tz B))))

; The function "tfo-inv-ortho" computes the inverse of a homogeneous
; transformation matrix.

(define (tfo-inv-ortho tfo)
  (let* ((tx (tfo-tx tfo))
         (ty (tfo-ty tfo))
         (tz (tfo-tz tfo)))
    (make-tfo
     (tfo-a tfo) (tfo-d tfo) (tfo-g tfo)
     (tfo-b tfo) (tfo-e tfo) (tfo-h tfo)
     (tfo-c tfo) (tfo-f tfo) (tfo-i tfo)
     (FLOAT- (FLOAT+ (FLOAT* (tfo-a tfo) tx)
                     (FLOAT* (tfo-b tfo) ty)
                     (FLOAT* (tfo-c tfo) tz)))
     (FLOAT- (FLOAT+ (FLOAT* (tfo-d tfo) tx)
                     (FLOAT* (tfo-e tfo) ty)
                     (FLOAT* (tfo-f tfo) tz)))
     (FLOAT- (FLOAT+ (FLOAT* (tfo-g tfo) tx)
                     (FLOAT* (tfo-h tfo) ty)
                     (FLOAT* (tfo-i tfo) tz))))))

; Given three points p1, p2, and p3, the function "tfo-align" computes
; a transformation matrix such that point p1 gets mapped to (0,0,0), p2 gets
; mapped to the Y axis and p3 gets mapped to the YZ plane.

(define (tfo-align p1 p2 p3)
  (let* ((x1 (pt-x p1))       (y1 (pt-y p1))       (z1 (pt-z p1))
         (x3 (pt-x p3))       (y3 (pt-y p3))       (z3 (pt-z p3))
         (x31 (FLOAT- x3 x1)) (y31 (FLOAT- y3 y1)) (z31 (FLOAT- z3 z1))
         (rotpY (pt-sub p2 p1))
         (Phi (pt-phi rotpY))
         (Theta (pt-theta rotpY))
         (sinP (FLOATsin Phi))
         (sinT (FLOATsin Theta))
         (cosP (FLOATcos Phi))
         (cosT (FLOATcos Theta))
         (sinPsinT (FLOAT* sinP sinT))
         (sinPcosT (FLOAT* sinP cosT))
         (cosPsinT (FLOAT* cosP sinT))
         (cosPcosT (FLOAT* cosP cosT))
         (rotpZ
          (make-pt
           (FLOAT- (FLOAT* cosT x31)
                   (FLOAT* sinT z31))
           (FLOAT+ (FLOAT* sinPsinT x31)
                   (FLOAT* cosP y31)
                   (FLOAT* sinPcosT z31))
           (FLOAT+ (FLOAT* cosPsinT x31)
                   (FLOAT- (FLOAT* sinP y31))
                   (FLOAT* cosPcosT z31))))
         (Rho (pt-theta rotpZ))
         (cosR (FLOATcos Rho))
         (sinR (FLOATsin Rho))
         (x (FLOAT+ (FLOAT- (FLOAT* x1 cosT))
                    (FLOAT* z1 sinT)))
         (y (FLOAT- (FLOAT- (FLOAT- (FLOAT* x1 sinPsinT))
                            (FLOAT* y1 cosP))
                    (FLOAT* z1 sinPcosT)))
         (z (FLOAT- (FLOAT+ (FLOAT- (FLOAT* x1 cosPsinT))
                            (FLOAT* y1 sinP))
                    (FLOAT* z1 cosPcosT))))
    (make-tfo
     (FLOAT- (FLOAT* cosT cosR) (FLOAT* cosPsinT sinR))
     sinPsinT
     (FLOAT+ (FLOAT* cosT sinR) (FLOAT* cosPsinT cosR))
     (FLOAT* sinP sinR)
     cosP
     (FLOAT- (FLOAT* sinP cosR))
     (FLOAT- (FLOAT- (FLOAT* sinT cosR)) (FLOAT* cosPcosT sinR))
     sinPcosT
     (FLOAT+ (FLOAT- (FLOAT* sinT sinR)) (FLOAT* cosPcosT cosR))
     (FLOAT- (FLOAT* x cosR) (FLOAT* z sinR))
     y
     (FLOAT+ (FLOAT* x sinR) (FLOAT* z cosR)))))

; -- NUCLEIC ACID CONFORMATIONS DATA BASE -------------------------------------

; Numbering of atoms follows the paper:
;
; IUPAC-IUB Joint Commission on Biochemical Nomenclature (JCBN)
; (1983) Abbreviations and Symbols for the Description of
; Conformations of Polynucleotide Chains.  Eur. J. Biochem 131,
; 9-15.
;
; In the atom names, we have used "*" instead of "'".

; Define part common to all 4 nucleotide types.

(define (nuc-dgf-base-tfo nuc) (vector-ref nuc 0))
(define (nuc-dgf-base-tfo-set! nuc val) (vector-set! nuc 0 val))
(define (nuc-P-O3*-275-tfo nuc) (vector-ref nuc 1))
(define (nuc-P-O3*-275-tfo-set! nuc val) (vector-set! nuc 1 val))
(define (nuc-P-O3*-180-tfo nuc) (vector-ref nuc 2))
(define (nuc-P-O3*-180-tfo-set! nuc val) (vector-set! nuc 2 val))
(define (nuc-P-O3*-60-tfo nuc) (vector-ref nuc 3))
(define (nuc-P-O3*-60-tfo-set! nuc val) (vector-set! nuc 3 val))
(define (nuc-P nuc) (vector-ref nuc 4))
(define (nuc-P-set! nuc val) (vector-set! nuc 4 val))
(define (nuc-O1P nuc) (vector-ref nuc 5))
(define (nuc-O1P-set! nuc val) (vector-set! nuc 5 val))
(define (nuc-O2P nuc) (vector-ref nuc 6))
(define (nuc-O2P-set! nuc val) (vector-set! nuc 6 val))
(define (nuc-O5* nuc) (vector-ref nuc 7))
(define (nuc-O5*-set! nuc val) (vector-set! nuc 7 val))
(define (nuc-C5* nuc) (vector-ref nuc 8))
(define (nuc-C5*-set! nuc val) (vector-set! nuc 8 val))
(define (nuc-H5* nuc) (vector-ref nuc 9))
(define (nuc-H5*-set! nuc val) (vector-set! nuc 9 val))
(define (nuc-H5** nuc) (vector-ref nuc 10))
(define (nuc-H5**-set! nuc val) (vector-set! nuc 10 val))
(define (nuc-C4* nuc) (vector-ref nuc 11))
(define (nuc-C4*-set! nuc val) (vector-set! nuc 11 val))
(define (nuc-H4* nuc) (vector-ref nuc 12))
(define (nuc-H4*-set! nuc val) (vector-set! nuc 12 val))
(define (nuc-O4* nuc) (vector-ref nuc 13))
(define (nuc-O4*-set! nuc val) (vector-set! nuc 13 val))
(define (nuc-C1* nuc) (vector-ref nuc 14))
(define (nuc-C1*-set! nuc val) (vector-set! nuc 14 val))
(define (nuc-H1* nuc) (vector-ref nuc 15))
(define (nuc-H1*-set! nuc val) (vector-set! nuc 15 val))
(define (nuc-C2* nuc) (vector-ref nuc 16))
(define (nuc-C2*-set! nuc val) (vector-set! nuc 16 val))
(define (nuc-H2** nuc) (vector-ref nuc 17))
(define (nuc-H2**-set! nuc val) (vector-set! nuc 17 val))
(define (nuc-O2* nuc) (vector-ref nuc 18))
(define (nuc-O2*-set! nuc val) (vector-set! nuc 18 val))
(define (nuc-H2* nuc) (vector-ref nuc 19))
(define (nuc-H2*-set! nuc val) (vector-set! nuc 19 val))
(define (nuc-C3* nuc) (vector-ref nuc 20))
(define (nuc-C3*-set! nuc val) (vector-set! nuc 20 val))
(define (nuc-H3* nuc) (vector-ref nuc 21))
(define (nuc-H3*-set! nuc val) (vector-set! nuc 21 val))
(define (nuc-O3* nuc) (vector-ref nuc 22))
(define (nuc-O3*-set! nuc val) (vector-set! nuc 22 val))
(define (nuc-N1 nuc) (vector-ref nuc 23))
(define (nuc-N1-set! nuc val) (vector-set! nuc 23 val))
(define (nuc-N3 nuc) (vector-ref nuc 24))
(define (nuc-N3-set! nuc val) (vector-set! nuc 24 val))
(define (nuc-C2 nuc) (vector-ref nuc 25))
(define (nuc-C2-set! nuc val) (vector-set! nuc 25 val))
(define (nuc-C4 nuc) (vector-ref nuc 26))
(define (nuc-C4-set! nuc val) (vector-set! nuc 26 val))
(define (nuc-C5 nuc) (vector-ref nuc 27))
(define (nuc-C5-set! nuc val) (vector-set! nuc 27 val))
(define (nuc-C6 nuc) (vector-ref nuc 28))
(define (nuc-C6-set! nuc val) (vector-set! nuc 28 val))

; Define remaining atoms for each nucleotide type.

(define (make-rA dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
                 P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
                 H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
                 N6 N7 N9 C8 H2 H61 H62 H8)
  (vector dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
          P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
          H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
          'rA N6 N7 N9 C8 H2 H61 H62 H8))

(define (rA? nuc) (eq? (vector-ref nuc 29) 'rA))

(define (rA-N6 nuc) (vector-ref nuc 30))
(define (rA-N6-set! nuc val) (vector-set! nuc 30 val))
(define (rA-N7 nuc) (vector-ref nuc 31))
(define (rA-N7-set! nuc val) (vector-set! nuc 31 val))
(define (rA-N9 nuc) (vector-ref nuc 32))
(define (rA-N9-set! nuc val) (vector-set! nuc 32 val))
(define (rA-C8 nuc) (vector-ref nuc 33))
(define (rA-C8-set! nuc val) (vector-set! nuc 33 val))
(define (rA-H2 nuc) (vector-ref nuc 34))
(define (rA-H2-set! nuc val) (vector-set! nuc 34 val))
(define (rA-H61 nuc) (vector-ref nuc 35))
(define (rA-H61-set! nuc val) (vector-set! nuc 35 val))
(define (rA-H62 nuc) (vector-ref nuc 36))
(define (rA-H62-set! nuc val) (vector-set! nuc 36 val))
(define (rA-H8 nuc) (vector-ref nuc 37))
(define (rA-H8-set! nuc val) (vector-set! nuc 37 val))

(define (make-rC dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
                 P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
                 H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
                 N4 O2 H41 H42 H5 H6)
  (vector dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
          P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
          H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
          'rC N4 O2 H41 H42 H5 H6))

(define (rC? nuc) (eq? (vector-ref nuc 29) 'rC))

(define (rC-N4 nuc) (vector-ref nuc 30))
(define (rC-N4-set! nuc val) (vector-set! nuc 30 val))
(define (rC-O2 nuc) (vector-ref nuc 31))
(define (rC-O2-set! nuc val) (vector-set! nuc 31 val))
(define (rC-H41 nuc) (vector-ref nuc 32))
(define (rC-H41-set! nuc val) (vector-set! nuc 32 val))
(define (rC-H42 nuc) (vector-ref nuc 33))
(define (rC-H42-set! nuc val) (vector-set! nuc 33 val))
(define (rC-H5 nuc) (vector-ref nuc 34))
(define (rC-H5-set! nuc val) (vector-set! nuc 34 val))
(define (rC-H6 nuc) (vector-ref nuc 35))
(define (rC-H6-set! nuc val) (vector-set! nuc 35 val))

(define (make-rG dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
                 P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
                 H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
                 N2 N7 N9 C8 O6 H1 H21 H22 H8)
  (vector dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
          P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
          H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
          'rG N2 N7 N9 C8 O6 H1 H21 H22 H8))

(define (rG? nuc) (eq? (vector-ref nuc 29) 'rG))

(define (rG-N2 nuc) (vector-ref nuc 30))
(define (rG-N2-set! nuc val) (vector-set! nuc 30 val))
(define (rG-N7 nuc) (vector-ref nuc 31))
(define (rG-N7-set! nuc val) (vector-set! nuc 31 val))
(define (rG-N9 nuc) (vector-ref nuc 32))
(define (rG-N9-set! nuc val) (vector-set! nuc 32 val))
(define (rG-C8 nuc) (vector-ref nuc 33))
(define (rG-C8-set! nuc val) (vector-set! nuc 33 val))
(define (rG-O6 nuc) (vector-ref nuc 34))
(define (rG-O6-set! nuc val) (vector-set! nuc 34 val))
(define (rG-H1 nuc) (vector-ref nuc 35))
(define (rG-H1-set! nuc val) (vector-set! nuc 35 val))
(define (rG-H21 nuc) (vector-ref nuc 36))
(define (rG-H21-set! nuc val) (vector-set! nuc 36 val))
(define (rG-H22 nuc) (vector-ref nuc 37))
(define (rG-H22-set! nuc val) (vector-set! nuc 37 val))
(define (rG-H8 nuc) (vector-ref nuc 38))
(define (rG-H8-set! nuc val) (vector-set! nuc 38 val))

(define (make-rU dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
                 P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
                 H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
                 O2 O4 H3 H5 H6)
  (vector dgf-base-tfo P-O3*-275-tfo P-O3*-180-tfo P-O3*-60-tfo
          P O1P O2P O5* C5* H5* H5** C4* H4* O4* C1* H1* C2*
          H2** O2* H2* C3* H3* O3* N1 N3 C2 C4 C5 C6
          'rU O2 O4 H3 H5 H6))

(define (rU? nuc) (eq? (vector-ref nuc 29) 'rU))

(define (rU-O2 nuc) (vector-ref nuc 30))
(define (rU-O2-set! nuc val) (vector-set! nuc 30 val))
(define (rU-O4 nuc) (vector-ref nuc 31))
(define (rU-O4-set! nuc val) (vector-set! nuc 31 val))
(define (rU-H3 nuc) (vector-ref nuc 32))
(define (rU-H3-set! nuc val) (vector-set! nuc 32 val))
(define (rU-H5 nuc) (vector-ref nuc 33))
(define (rU-H5-set! nuc val) (vector-set! nuc 33 val))
(define (rU-H6 nuc) (vector-ref nuc 34))
(define (rU-H6-set! nuc val) (vector-set! nuc 34 val))

; Database of nucleotide conformations:

(define rA
  (nuc-const
    #( -0.0018  -0.8207   0.5714  ; dgf-base-tfo
        0.2679  -0.5509  -0.7904
        0.9634   0.1517   0.2209
        0.0073   8.4030   0.6232)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  5.4550   8.2120  -2.8810) ; C5*
    #(  5.4546   8.8508  -1.9978) ; H5*
    #(  5.7588   8.6625  -3.8259) ; H5**
    #(  6.4970   7.1480  -2.5980) ; C4*
    #(  7.4896   7.5919  -2.5214) ; H4*
    #(  6.1630   6.4860  -1.3440) ; O4*
    #(  6.5400   5.1200  -1.4190) ; C1*
    #(  7.2763   4.9681  -0.6297) ; H1*
    #(  7.1940   4.8830  -2.7770) ; C2*
    #(  6.8667   3.9183  -3.1647) ; H2**
    #(  8.5860   5.0910  -2.6140) ; O2*
    #(  8.9510   4.7626  -1.7890) ; H2*
    #(  6.5720   6.0040  -3.6090) ; C3*
    #(  5.5636   5.7066  -3.8966) ; H3*
    #(  7.3801   6.3562  -4.7350) ; O3*
    #(  4.7150   0.4910  -0.1360) ; N1
    #(  6.3490   2.1730  -0.6020) ; N3
    #(  5.9530   0.9650  -0.2670) ; C2
    #(  5.2900   2.9790  -0.8260) ; C4
    #(  3.9720   2.6390  -0.7330) ; C5
    #(  3.6770   1.3160  -0.3660) ; C6
    rA
    #(  2.4280   0.8450  -0.2360) ; N6
    #(  3.1660   3.7290  -1.0360) ; N7
    #(  5.3170   4.2990  -1.1930) ; N9
    #(  4.0100   4.6780  -1.2990) ; C8
    #(  6.6890   0.1903  -0.0518) ; H2
    #(  1.6470   1.4460  -0.4040) ; H61
    #(  2.2780  -0.1080  -0.0280) ; H62
    #(  3.4421   5.5744  -1.5482) ; H8
  ))

(define rA01
  (nuc-const
    #( -0.0043  -0.8175   0.5759  ; dgf-base-tfo
        0.2617  -0.5567  -0.7884
        0.9651   0.1473   0.2164
        0.0359   8.3929   0.5532)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  5.4352   8.2183  -2.7757) ; C5*
    #(  5.3830   8.7883  -1.8481) ; H5*
    #(  5.7729   8.7436  -3.6691) ; H5**
    #(  6.4830   7.1518  -2.5252) ; C4*
    #(  7.4749   7.5972  -2.4482) ; H4*
    #(  6.1626   6.4620  -1.2827) ; O4*
    #(  6.5431   5.0992  -1.3905) ; C1*
    #(  7.2871   4.9328  -0.6114) ; H1*
    #(  7.1852   4.8935  -2.7592) ; C2*
    #(  6.8573   3.9363  -3.1645) ; H2**
    #(  8.5780   5.1025  -2.6046) ; O2*
    #(  8.9516   4.7577  -1.7902) ; H2*
    #(  6.5522   6.0300  -3.5612) ; C3*
    #(  5.5420   5.7356  -3.8459) ; H3*
    #(  7.3487   6.4089  -4.6867) ; O3*
    #(  4.7442   0.4514  -0.1390) ; N1
    #(  6.3687   2.1459  -0.5926) ; N3
    #(  5.9795   0.9335  -0.2657) ; C2
    #(  5.3052   2.9471  -0.8125) ; C4
    #(  3.9891   2.5987  -0.7230) ; C5
    #(  3.7016   1.2717  -0.3647) ; C6
    rA
    #(  2.4553   0.7925  -0.2390) ; N6
    #(  3.1770   3.6859  -1.0198) ; N7
    #(  5.3247   4.2695  -1.1710) ; N9
    #(  4.0156   4.6415  -1.2759) ; C8
    #(  6.7198   0.1618  -0.0547) ; H2
    #(  1.6709   1.3900  -0.4039) ; H61
    #(  2.3107  -0.1627  -0.0373) ; H62
    #(  3.4426   5.5361  -1.5199) ; H8
  ))

(define rA02
  (nuc-const
    #(  0.5566   0.0449   0.8296  ; dgf-base-tfo
        0.5125   0.7673  -0.3854
       -0.6538   0.6397   0.4041
       -9.1161  -3.7679  -2.9968)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.5778   6.6594  -4.0364) ; C5*
    #(  4.9220   7.1963  -4.9204) ; H5*
    #(  3.7996   5.9091  -4.1764) ; H5**
    #(  5.7873   5.8869  -3.5482) ; C4*
    #(  6.0405   5.0875  -4.2446) ; H4*
    #(  6.9135   6.8036  -3.4310) ; O4*
    #(  7.7293   6.4084  -2.3392) ; C1*
    #(  8.7078   6.1815  -2.7624) ; H1*
    #(  7.1305   5.1418  -1.7347) ; C2*
    #(  7.2040   5.1982  -0.6486) ; H2**
    #(  7.7417   4.0392  -2.3813) ; O2*
    #(  8.6785   4.1443  -2.5630) ; H2*
    #(  5.6666   5.2728  -2.1536) ; C3*
    #(  5.1747   5.9805  -1.4863) ; H3*
    #(  4.9997   4.0086  -2.1973) ; O3*
    #( 10.3245   8.5459   1.5467) ; N1
    #(  9.8051   6.9432  -0.1497) ; N3
    #( 10.5175   7.4328   0.8408) ; C2
    #(  8.7523   7.7422  -0.4228) ; C4
    #(  8.4257   8.9060   0.2099) ; C5
    #(  9.2665   9.3242   1.2540) ; C6
    rA
    #(  9.0664  10.4462   1.9610) ; N6
    #(  7.2750   9.4537  -0.3428) ; N7
    #(  7.7962   7.5519  -1.3859) ; N9
    #(  6.9479   8.6157  -1.2771) ; C8
    #( 11.4063   6.9047   1.1859) ; H2
    #(  8.2845  11.0341   1.7552) ; H61
    #(  9.6584  10.6647   2.7198) ; H62
    #(  6.0430   8.9853  -1.7594) ; H8
  ))

(define rA03
  (nuc-const
    #( -0.5021   0.0731   0.8617  ; dgf-base-tfo
       -0.8112   0.3054  -0.4986
       -0.2996  -0.9494  -0.0940
        6.4273  -5.1944  -3.7807)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.1214   6.7116  -1.9049) ; C5*
    #(  3.3465   5.9610  -2.0607) ; H5*
    #(  4.0789   7.2928  -0.9837) ; H5**
    #(  5.4170   5.9293  -1.8186) ; C4*
    #(  5.4506   5.3400  -0.9023) ; H4*
    #(  5.5067   5.0417  -2.9703) ; O4*
    #(  6.8650   4.9152  -3.3612) ; C1*
    #(  7.1090   3.8577  -3.2603) ; H1*
    #(  7.7152   5.7282  -2.3894) ; C2*
    #(  8.5029   6.2356  -2.9463) ; H2**
    #(  8.1036   4.8568  -1.3419) ; O2*
    #(  8.3270   3.9651  -1.6184) ; H2*
    #(  6.7003   6.7565  -1.8911) ; C3*
    #(  6.5898   7.5329  -2.6482) ; H3*
    #(  7.0505   7.2878  -0.6105) ; O3*
    #(  9.6740   4.7656  -7.6614) ; N1
    #(  9.0739   4.3013  -5.3941) ; N3
    #(  9.8416   4.2192  -6.4581) ; C2
    #(  7.9885   5.0632  -5.6446) ; C4
    #(  7.6822   5.6856  -6.8194) ; C5
    #(  8.5831   5.5215  -7.8840) ; C6
    rA
    #(  8.4084   6.0747  -9.0933) ; N6
    #(  6.4857   6.3816  -6.7035) ; N7
    #(  6.9740   5.3703  -4.7760) ; N9
    #(  6.1133   6.1613  -5.4808) ; C8
    #( 10.7627   3.6375  -6.4220) ; H2
    #(  7.6031   6.6390  -9.2733) ; H61
    #(  9.1004   5.9708  -9.7893) ; H62
    #(  5.1705   6.6830  -5.3167) ; H8
  ))

(define rA04
  (nuc-const
    #( -0.5426  -0.8175   0.1929  ; dgf-base-tfo
        0.8304  -0.5567  -0.0237
        0.1267   0.1473   0.9809
       -0.5075   8.3929   0.2229)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  5.4352   8.2183  -2.7757) ; C5*
    #(  5.3830   8.7883  -1.8481) ; H5*
    #(  5.7729   8.7436  -3.6691) ; H5**
    #(  6.4830   7.1518  -2.5252) ; C4*
    #(  7.4749   7.5972  -2.4482) ; H4*
    #(  6.1626   6.4620  -1.2827) ; O4*
    #(  6.5431   5.0992  -1.3905) ; C1*
    #(  7.2871   4.9328  -0.6114) ; H1*
    #(  7.1852   4.8935  -2.7592) ; C2*
    #(  6.8573   3.9363  -3.1645) ; H2**
    #(  8.5780   5.1025  -2.6046) ; O2*
    #(  8.9516   4.7577  -1.7902) ; H2*
    #(  6.5522   6.0300  -3.5612) ; C3*
    #(  5.5420   5.7356  -3.8459) ; H3*
    #(  7.3487   6.4089  -4.6867) ; O3*
    #(  3.6343   2.6680   2.0783) ; N1
    #(  5.4505   3.9805   1.2446) ; N3
    #(  4.7540   3.3816   2.1851) ; C2
    #(  4.8805   3.7951   0.0354) ; C4
    #(  3.7416   3.0925  -0.2305) ; C5
    #(  3.0873   2.4980   0.8606) ; C6
    rA
    #(  1.9600   1.7805   0.7462) ; N6
    #(  3.4605   3.1184  -1.5906) ; N7
    #(  5.3247   4.2695  -1.1710) ; N9
    #(  4.4244   3.8244  -2.0953) ; C8
    #(  5.0814   3.4352   3.2234) ; H2
    #(  1.5423   1.6454  -0.1520) ; H61
    #(  1.5716   1.3398   1.5392) ; H62
    #(  4.2675   3.8876  -3.1721) ; H8
  ))

(define rA05
  (nuc-const
    #( -0.5891   0.0449   0.8068  ; dgf-base-tfo
        0.5375   0.7673   0.3498
       -0.6034   0.6397  -0.4762
       -0.3019  -3.7679  -9.5913)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.5778   6.6594  -4.0364) ; C5*
    #(  4.9220   7.1963  -4.9204) ; H5*
    #(  3.7996   5.9091  -4.1764) ; H5**
    #(  5.7873   5.8869  -3.5482) ; C4*
    #(  6.0405   5.0875  -4.2446) ; H4*
    #(  6.9135   6.8036  -3.4310) ; O4*
    #(  7.7293   6.4084  -2.3392) ; C1*
    #(  8.7078   6.1815  -2.7624) ; H1*
    #(  7.1305   5.1418  -1.7347) ; C2*
    #(  7.2040   5.1982  -0.6486) ; H2**
    #(  7.7417   4.0392  -2.3813) ; O2*
    #(  8.6785   4.1443  -2.5630) ; H2*
    #(  5.6666   5.2728  -2.1536) ; C3*
    #(  5.1747   5.9805  -1.4863) ; H3*
    #(  4.9997   4.0086  -2.1973) ; O3*
    #( 10.2594  10.6774  -1.0056) ; N1
    #(  9.7528   8.7080  -2.2631) ; N3
    #( 10.4471   9.7876  -1.9791) ; C2
    #(  8.7271   8.5575  -1.3991) ; C4
    #(  8.4100   9.3803  -0.3580) ; C5
    #(  9.2294  10.5030  -0.1574) ; C6
    rA
    #(  9.0349  11.3951   0.8250) ; N6
    #(  7.2891   8.9068   0.3121) ; N7
    #(  7.7962   7.5519  -1.3859) ; N9
    #(  6.9702   7.8292  -0.3353) ; C8
    #( 11.3132  10.0537  -2.5851) ; H2
    #(  8.2741  11.2784   1.4629) ; H61
    #(  9.6733  12.1368   0.9529) ; H62
    #(  6.0888   7.3990   0.1403) ; H8
  ))

(define rA06
  (nuc-const
    #( -0.9815   0.0731  -0.1772  ; dgf-base-tfo
        0.1912   0.3054  -0.9328
       -0.0141  -0.9494  -0.3137
        5.7506  -5.1944   4.7470)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.1214   6.7116  -1.9049) ; C5*
    #(  3.3465   5.9610  -2.0607) ; H5*
    #(  4.0789   7.2928  -0.9837) ; H5**
    #(  5.4170   5.9293  -1.8186) ; C4*
    #(  5.4506   5.3400  -0.9023) ; H4*
    #(  5.5067   5.0417  -2.9703) ; O4*
    #(  6.8650   4.9152  -3.3612) ; C1*
    #(  7.1090   3.8577  -3.2603) ; H1*
    #(  7.7152   5.7282  -2.3894) ; C2*
    #(  8.5029   6.2356  -2.9463) ; H2**
    #(  8.1036   4.8568  -1.3419) ; O2*
    #(  8.3270   3.9651  -1.6184) ; H2*
    #(  6.7003   6.7565  -1.8911) ; C3*
    #(  6.5898   7.5329  -2.6482) ; H3*
    #(  7.0505   7.2878  -0.6105) ; O3*
    #(  6.6624   3.5061  -8.2986) ; N1
    #(  6.5810   3.2570  -5.9221) ; N3
    #(  6.5151   2.8263  -7.1625) ; C2
    #(  6.8364   4.5817  -5.8882) ; C4
    #(  7.0116   5.4064  -6.9609) ; C5
    #(  6.9173   4.8260  -8.2361) ; C6
    rA
    #(  7.0668   5.5163  -9.3763) ; N6
    #(  7.2573   6.7070  -6.5394) ; N7
    #(  6.9740   5.3703  -4.7760) ; N9
    #(  7.2238   6.6275  -5.2453) ; C8
    #(  6.3146   1.7741  -7.3641) ; H2
    #(  7.2568   6.4972  -9.3456) ; H61
    #(  7.0437   5.0478 -10.2446) ; H62
    #(  7.4108   7.6227  -4.8418) ; H8
  ))

(define rA07
  (nuc-const
    #(  0.2379   0.1310  -0.9624  ; dgf-base-tfo
       -0.5876  -0.7696  -0.2499
       -0.7734   0.6249  -0.1061
       30.9870 -26.9344  42.6416)
    #(  0.7529   0.1548   0.6397  ; P-O3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; P-O3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; P-O3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; P
    #( 42.5400   8.0450  44.8330) ; O1P
    #( 42.2470   9.6920  42.9910) ; O2P
    #( 40.2550   8.2030  43.7340) ; O5*
    #( 39.3505   8.4697  42.6565) ; C5*
    #( 39.1377   7.5433  42.1230) ; H5*
    #( 39.7203   9.3119  42.0717) ; H5**
    #( 38.0405   8.9195  43.2869) ; C4*
    #( 37.3687   9.3036  42.5193) ; H4*
    #( 37.4319   7.8146  43.9387) ; O4*
    #( 37.1959   8.1354  45.3237) ; C1*
    #( 36.1788   8.5202  45.3970) ; H1*
    #( 38.1721   9.2328  45.6504) ; C2*
    #( 39.1555   8.7939  45.8188) ; H2**
    #( 37.7862  10.0617  46.7013) ; O2*
    #( 37.3087   9.6229  47.4092) ; H2*
    #( 38.1844  10.0268  44.3367) ; C3*
    #( 39.1578  10.5054  44.2289) ; H3*
    #( 37.0547  10.9127  44.3441) ; O3*
    #( 34.8811   4.2072  47.5784) ; N1
    #( 35.1084   6.1336  46.1818) ; N3
    #( 34.4108   5.1360  46.7207) ; C2
    #( 36.3908   6.1224  46.6053) ; C4
    #( 36.9819   5.2334  47.4697) ; C5
    #( 36.1786   4.1985  48.0035) ; C6
    rA
    #( 36.6103   3.2749  48.8452) ; N6
    #( 38.3236   5.5522  47.6595) ; N7
    #( 37.3887   7.0024  46.2437) ; N9
    #( 38.5055   6.6096  46.9057) ; C8
    #( 33.3553   5.0152  46.4771) ; H2
    #( 37.5730   3.2804  49.1507) ; H61
    #( 35.9775   2.5638  49.1828) ; H62
    #( 39.5461   6.9184  47.0041) ; H8
  ))

(define rA08
  (nuc-const
    #(  0.1084  -0.0895  -0.9901  ; dgf-base-tfo
        0.9789  -0.1638   0.1220
       -0.1731  -0.9824   0.0698
       -2.9039  47.2655  33.0094)
    #(  0.7529   0.1548   0.6397  ; P-O3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; P-O3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; P-O3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; P
    #( 42.5400   8.0450  44.8330) ; O1P
    #( 42.2470   9.6920  42.9910) ; O2P
    #( 40.2550   8.2030  43.7340) ; O5*
    #( 39.4850   8.9301  44.6977) ; C5*
    #( 39.0638   9.8199  44.2296) ; H5*
    #( 40.0757   9.0713  45.6029) ; H5**
    #( 38.3102   8.0414  45.0789) ; C4*
    #( 37.7842   8.4637  45.9351) ; H4*
    #( 37.4200   7.9453  43.9769) ; O4*
    #( 37.2249   6.5609  43.6273) ; C1*
    #( 36.3360   6.2168  44.1561) ; H1*
    #( 38.4347   5.8414  44.1590) ; C2*
    #( 39.2688   5.9974  43.4749) ; H2**
    #( 38.2344   4.4907  44.4348) ; O2*
    #( 37.6374   4.0386  43.8341) ; H2*
    #( 38.6926   6.6079  45.4637) ; C3*
    #( 39.7585   6.5640  45.6877) ; H3*
    #( 37.8238   6.0705  46.4723) ; O3*
    #( 33.9162   6.2598  39.7758) ; N1
    #( 34.6709   6.5759  42.0215) ; N3
    #( 33.7257   6.5186  41.0858) ; C2
    #( 35.8935   6.3324  41.5018) ; C4
    #( 36.2105   6.0601  40.1932) ; C5
    #( 35.1538   6.0151  39.2537) ; C6
    rA
    #( 35.3088   5.7642  37.9649) ; N6
    #( 37.5818   5.8677  40.0507) ; N7
    #( 37.0932   6.3197  42.1810) ; N9
    #( 38.0509   6.0354  41.2635) ; C8
    #( 32.6830   6.6898  41.3532) ; H2
    #( 36.2305   5.5855  37.5925) ; H61
    #( 34.5056   5.7512  37.3528) ; H62
    #( 39.1318   5.8993  41.2285) ; H8
  ))

(define rA09
  (nuc-const
    #(  0.8467   0.4166  -0.3311  ; dgf-base-tfo
       -0.3962   0.9089   0.1303
        0.3552   0.0209   0.9346
      -42.7319 -26.6223 -29.8163)
    #(  0.7529   0.1548   0.6397  ; P-O3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; P-O3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; P-O3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; P
    #( 42.5400   8.0450  44.8330) ; O1P
    #( 42.2470   9.6920  42.9910) ; O2P
    #( 40.2550   8.2030  43.7340) ; O5*
    #( 39.3505   8.4697  42.6565) ; C5*
    #( 39.1377   7.5433  42.1230) ; H5*
    #( 39.7203   9.3119  42.0717) ; H5**
    #( 38.0405   8.9195  43.2869) ; C4*
    #( 37.6479   8.1347  43.9335) ; H4*
    #( 38.2691  10.0933  44.0524) ; O4*
    #( 37.3999  11.1488  43.5973) ; C1*
    #( 36.5061  11.1221  44.2206) ; H1*
    #( 37.0364  10.7838  42.1836) ; C2*
    #( 37.8636  11.0489  41.5252) ; H2**
    #( 35.8275  11.3133  41.7379) ; O2*
    #( 35.6214  12.1896  42.0714) ; H2*
    #( 36.9316   9.2556  42.2837) ; C3*
    #( 37.1778   8.8260  41.3127) ; H3*
    #( 35.6285   8.9334  42.7926) ; O3*
    #( 38.1482  15.2833  46.4641) ; N1
    #( 37.3641  13.0968  45.9007) ; N3
    #( 37.5032  14.1288  46.7300) ; C2
    #( 37.9570  13.3377  44.7113) ; C4
    #( 38.6397  14.4660  44.3267) ; C5
    #( 38.7473  15.5229  45.2609) ; C6
    rA
    #( 39.3720  16.6649  45.0297) ; N6
    #( 39.1079  14.3351  43.0223) ; N7
    #( 38.0132  12.4868  43.6280) ; N9
    #( 38.7058  13.1402  42.6620) ; C8
    #( 37.0731  14.0857  47.7306) ; H2
    #( 39.8113  16.8281  44.1350) ; H61
    #( 39.4100  17.3741  45.7478) ; H62
    #( 39.0412  12.9660  41.6397) ; H8
  ))

(define rA10
  (nuc-const
    #(  0.7063   0.6317  -0.3196  ; dgf-base-tfo
       -0.0403  -0.4149  -0.9090
       -0.7068   0.6549  -0.2676
        6.4402 -52.1496  30.8246)
    #(  0.7529   0.1548   0.6397  ; P-O3*-275-tfo
        0.2952  -0.9481  -0.1180
        0.5882   0.2777  -0.7595
      -58.8919 -11.3095   6.0866)
    #( -0.0239   0.9667  -0.2546  ; P-O3*-180-tfo
        0.9731  -0.0359  -0.2275
       -0.2290  -0.2532  -0.9399
        3.5401 -29.7913  52.2796)
    #( -0.8912  -0.4531   0.0242  ; P-O3*-60-tfo
       -0.1183   0.1805  -0.9764
        0.4380  -0.8730  -0.2145
       19.9023  54.8054  15.2799)
    #( 41.8210   8.3880  43.5890) ; P
    #( 42.5400   8.0450  44.8330) ; O1P
    #( 42.2470   9.6920  42.9910) ; O2P
    #( 40.2550   8.2030  43.7340) ; O5*
    #( 39.4850   8.9301  44.6977) ; C5*
    #( 39.0638   9.8199  44.2296) ; H5*
    #( 40.0757   9.0713  45.6029) ; H5**
    #( 38.3102   8.0414  45.0789) ; C4*
    #( 37.7099   7.8166  44.1973) ; H4*
    #( 38.8012   6.8321  45.6380) ; O4*
    #( 38.2431   6.6413  46.9529) ; C1*
    #( 37.3505   6.0262  46.8385) ; H1*
    #( 37.8484   8.0156  47.4214) ; C2*
    #( 38.7381   8.5406  47.7690) ; H2**
    #( 36.8286   8.0368  48.3701) ; O2*
    #( 36.8392   7.3063  48.9929) ; H2*
    #( 37.3576   8.6512  46.1132) ; C3*
    #( 37.5207   9.7275  46.1671) ; H3*
    #( 35.9985   8.2392  45.9032) ; O3*
    #( 39.9117   2.2278  48.8527) ; N1
    #( 38.6207   3.6941  47.4757) ; N3
    #( 38.9872   2.4888  47.9057) ; C2
    #( 39.2961   4.6720  48.1174) ; C4
    #( 40.2546   4.5307  49.0912) ; C5
    #( 40.5932   3.2189  49.4985) ; C6
    rA
    #( 41.4938   2.9317  50.4229) ; N6
    #( 40.7195   5.7755  49.5060) ; N7
    #( 39.1730   6.0305  47.9170) ; N9
    #( 40.0413   6.6250  48.7728) ; C8
    #( 38.5257   1.5960  47.4838) ; H2
    #( 41.9907   3.6753  50.8921) ; H61
    #( 41.6848   1.9687  50.6599) ; H62
    #( 40.3571   7.6321  49.0452) ; H8
  ))

(define rAs
  (list rA01 rA02 rA03 rA04 rA05 rA06 rA07 rA08 rA09 rA10))

(define rC
  (nuc-const
    #( -0.0359  -0.8071   0.5894  ; dgf-base-tfo
       -0.2669   0.5761   0.7726
       -0.9631  -0.1296  -0.2361
        0.1584   8.3434   0.5434)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  5.2430  -8.2420   2.8260) ; C5*
    #(  5.1974  -8.8497   1.9223) ; H5*
    #(  5.5548  -8.7348   3.7469) ; H5**
    #(  6.3140  -7.2060   2.5510) ; C4*
    #(  7.2954  -7.6762   2.4898) ; H4*
    #(  6.0140  -6.5420   1.2890) ; O4*
    #(  6.4190  -5.1840   1.3620) ; C1*
    #(  7.1608  -5.0495   0.5747) ; H1*
    #(  7.0760  -4.9560   2.7270) ; C2*
    #(  6.7770  -3.9803   3.1099) ; H2**
    #(  8.4500  -5.1930   2.5810) ; O2*
    #(  8.8309  -4.8755   1.7590) ; H2*
    #(  6.4060  -6.0590   3.5580) ; C3*
    #(  5.4021  -5.7313   3.8281) ; H3*
    #(  7.1570  -6.4240   4.7070) ; O3*
    #(  5.2170  -4.3260   1.1690) ; N1
    #(  4.2960  -2.2560   0.6290) ; N3
    #(  5.4330  -3.0200   0.7990) ; C2
    #(  2.9930  -2.6780   0.7940) ; C4
    #(  2.8670  -4.0630   1.1830) ; C5
    #(  3.9570  -4.8300   1.3550) ; C6
    rC
    #(  2.0187  -1.8047   0.5874) ; N4
    #(  6.5470  -2.5560   0.6290) ; O2
    #(  1.0684  -2.1236   0.7109) ; H41
    #(  2.2344  -0.8560   0.3162) ; H42
    #(  1.8797  -4.4972   1.3404) ; H5
    #(  3.8479  -5.8742   1.6480) ; H6
  ))

(define rC01
  (nuc-const
    #( -0.0137  -0.8012   0.5983  ; dgf-base-tfo
       -0.2523   0.5817   0.7733
       -0.9675  -0.1404  -0.2101
        0.2031   8.3874   0.4228)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  5.2416  -8.2422   2.8181) ; C5*
    #(  5.2050  -8.8128   1.8901) ; H5*
    #(  5.5368  -8.7738   3.7227) ; H5**
    #(  6.3232  -7.2037   2.6002) ; C4*
    #(  7.3048  -7.6757   2.5577) ; H4*
    #(  6.0635  -6.5092   1.3456) ; O4*
    #(  6.4697  -5.1547   1.4629) ; C1*
    #(  7.2354  -5.0043   0.7018) ; H1*
    #(  7.0856  -4.9610   2.8521) ; C2*
    #(  6.7777  -3.9935   3.2487) ; H2**
    #(  8.4627  -5.1992   2.7423) ; O2*
    #(  8.8693  -4.8638   1.9399) ; H2*
    #(  6.3877  -6.0809   3.6362) ; C3*
    #(  5.3770  -5.7562   3.8834) ; H3*
    #(  7.1024  -6.4754   4.7985) ; O3*
    #(  5.2764  -4.2883   1.2538) ; N1
    #(  4.3777  -2.2062   0.7229) ; N3
    #(  5.5069  -2.9779   0.9088) ; C2
    #(  3.0693  -2.6246   0.8500) ; C4
    #(  2.9279  -4.0146   1.2149) ; C5
    #(  4.0101  -4.7892   1.4017) ; C6
    rC
    #(  2.1040  -1.7437   0.6331) ; N4
    #(  6.6267  -2.5166   0.7728) ; O2
    #(  1.1496  -2.0600   0.7287) ; H41
    #(  2.3303  -0.7921   0.3815) ; H42
    #(  1.9353  -4.4465   1.3419) ; H5
    #(  3.8895  -5.8371   1.6762) ; H6
  ))

(define rC02
  (nuc-const
    #(  0.5141   0.0246   0.8574  ; dgf-base-tfo
       -0.5547  -0.7529   0.3542
        0.6542  -0.6577  -0.3734
       -9.1111  -3.4598  -3.2939)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  4.3825  -6.6585   4.0489) ; C5*
    #(  4.6841  -7.2019   4.9443) ; H5*
    #(  3.6189  -5.8889   4.1625) ; H5**
    #(  5.6255  -5.9175   3.5998) ; C4*
    #(  5.8732  -5.1228   4.3034) ; H4*
    #(  6.7337  -6.8605   3.5222) ; O4*
    #(  7.5932  -6.4923   2.4548) ; C1*
    #(  8.5661  -6.2983   2.9064) ; H1*
    #(  7.0527  -5.2012   1.8322) ; C2*
    #(  7.1627  -5.2525   0.7490) ; H2**
    #(  7.6666  -4.1249   2.4880) ; O2*
    #(  8.5944  -4.2543   2.6981) ; H2*
    #(  5.5661  -5.3029   2.2009) ; C3*
    #(  5.0841  -6.0018   1.5172) ; H3*
    #(  4.9062  -4.0452   2.2042) ; O3*
    #(  7.6298  -7.6136   1.4752) ; N1
    #(  8.6945  -8.7046  -0.2857) ; N3
    #(  8.6943  -7.6514   0.6066) ; C2
    #(  7.7426  -9.6987  -0.3801) ; C4
    #(  6.6642  -9.5742   0.5722) ; C5
    #(  6.6391  -8.5592   1.4526) ; C6
    rC
    #(  7.9033 -10.6371  -1.3010) ; N4
    #(  9.5840  -6.8186   0.6136) ; O2
    #(  7.2009 -11.3604  -1.3619) ; H41
    #(  8.7058 -10.6168  -1.9140) ; H42
    #(  5.8585 -10.3083   0.5822) ; H5
    #(  5.8197  -8.4773   2.1667) ; H6
  ))

(define rC03
  (nuc-const
    #( -0.4993   0.0476   0.8651  ; dgf-base-tfo
        0.8078  -0.3353   0.4847
        0.3132   0.9409   0.1290
        6.2989  -5.2303  -3.8577)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  3.9938  -6.7042   1.9023) ; C5*
    #(  3.2332  -5.9343   2.0319) ; H5*
    #(  3.9666  -7.2863   0.9812) ; H5**
    #(  5.3098  -5.9546   1.8564) ; C4*
    #(  5.3863  -5.3702   0.9395) ; H4*
    #(  5.3851  -5.0642   3.0076) ; O4*
    #(  6.7315  -4.9724   3.4462) ; C1*
    #(  7.0033  -3.9202   3.3619) ; H1*
    #(  7.5997  -5.8018   2.4948) ; C2*
    #(  8.3627  -6.3254   3.0707) ; H2**
    #(  8.0410  -4.9501   1.4724) ; O2*
    #(  8.2781  -4.0644   1.7570) ; H2*
    #(  6.5701  -6.8129   1.9714) ; C3*
    #(  6.4186  -7.5809   2.7299) ; H3*
    #(  6.9357  -7.3841   0.7235) ; O3*
    #(  6.8024  -5.4718   4.8475) ; N1
    #(  7.9218  -5.5700   6.8877) ; N3
    #(  7.8908  -5.0886   5.5944) ; C2
    #(  6.9789  -6.3827   7.4823) ; C4
    #(  5.8742  -6.7319   6.6202) ; C5
    #(  5.8182  -6.2769   5.3570) ; C6
    rC
    #(  7.1702  -6.7511   8.7402) ; N4
    #(  8.7747  -4.3728   5.1568) ; O2
    #(  6.4741  -7.3461   9.1662) ; H41
    #(  7.9889  -6.4396   9.2429) ; H42
    #(  5.0736  -7.3713   6.9922) ; H5
    #(  4.9784  -6.5473   4.7170) ; H6
  ))

(define rC04
  (nuc-const
    #( -0.5669  -0.8012   0.1918  ; dgf-base-tfo
       -0.8129   0.5817   0.0273
       -0.1334  -0.1404  -0.9811
       -0.3279   8.3874   0.3355)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  5.2416  -8.2422   2.8181) ; C5*
    #(  5.2050  -8.8128   1.8901) ; H5*
    #(  5.5368  -8.7738   3.7227) ; H5**
    #(  6.3232  -7.2037   2.6002) ; C4*
    #(  7.3048  -7.6757   2.5577) ; H4*
    #(  6.0635  -6.5092   1.3456) ; O4*
    #(  6.4697  -5.1547   1.4629) ; C1*
    #(  7.2354  -5.0043   0.7018) ; H1*
    #(  7.0856  -4.9610   2.8521) ; C2*
    #(  6.7777  -3.9935   3.2487) ; H2**
    #(  8.4627  -5.1992   2.7423) ; O2*
    #(  8.8693  -4.8638   1.9399) ; H2*
    #(  6.3877  -6.0809   3.6362) ; C3*
    #(  5.3770  -5.7562   3.8834) ; H3*
    #(  7.1024  -6.4754   4.7985) ; O3*
    #(  5.2764  -4.2883   1.2538) ; N1
    #(  3.8961  -3.0896  -0.1893) ; N3
    #(  5.0095  -3.8907  -0.0346) ; C2
    #(  3.0480  -2.6632   0.8116) ; C4
    #(  3.4093  -3.1310   2.1292) ; C5
    #(  4.4878  -3.9124   2.3088) ; C6
    rC
    #(  2.0216  -1.8941   0.4804) ; N4
    #(  5.7005  -4.2164  -0.9842) ; O2
    #(  1.4067  -1.5873   1.2205) ; H41
    #(  1.8721  -1.6319  -0.4835) ; H42
    #(  2.8048  -2.8507   2.9918) ; H5
    #(  4.7491  -4.2593   3.3085) ; H6
  ))

(define rC05
  (nuc-const
    #( -0.6298   0.0246   0.7763  ; dgf-base-tfo
       -0.5226  -0.7529  -0.4001
        0.5746  -0.6577   0.4870
       -0.0208  -3.4598  -9.6882)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  4.3825  -6.6585   4.0489) ; C5*
    #(  4.6841  -7.2019   4.9443) ; H5*
    #(  3.6189  -5.8889   4.1625) ; H5**
    #(  5.6255  -5.9175   3.5998) ; C4*
    #(  5.8732  -5.1228   4.3034) ; H4*
    #(  6.7337  -6.8605   3.5222) ; O4*
    #(  7.5932  -6.4923   2.4548) ; C1*
    #(  8.5661  -6.2983   2.9064) ; H1*
    #(  7.0527  -5.2012   1.8322) ; C2*
    #(  7.1627  -5.2525   0.7490) ; H2**
    #(  7.6666  -4.1249   2.4880) ; O2*
    #(  8.5944  -4.2543   2.6981) ; H2*
    #(  5.5661  -5.3029   2.2009) ; C3*
    #(  5.0841  -6.0018   1.5172) ; H3*
    #(  4.9062  -4.0452   2.2042) ; O3*
    #(  7.6298  -7.6136   1.4752) ; N1
    #(  8.5977  -9.5977   0.7329) ; N3
    #(  8.5951  -8.5745   1.6594) ; C2
    #(  7.7372  -9.7371  -0.3364) ; C4
    #(  6.7596  -8.6801  -0.4476) ; C5
    #(  6.7338  -7.6721   0.4408) ; C6
    rC
    #(  7.8849 -10.7881  -1.1289) ; N4
    #(  9.3993  -8.5377   2.5743) ; O2
    #(  7.2499 -10.8809  -1.9088) ; H41
    #(  8.6122 -11.4649  -0.9468) ; H42
    #(  6.0317  -8.6941  -1.2588) ; H5
    #(  5.9901  -6.8809   0.3459) ; H6
  ))

(define rC06
  (nuc-const
    #( -0.9837   0.0476  -0.1733  ; dgf-base-tfo
       -0.1792  -0.3353   0.9249
       -0.0141   0.9409   0.3384
        5.7793  -5.2303   4.5997)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  3.9938  -6.7042   1.9023) ; C5*
    #(  3.2332  -5.9343   2.0319) ; H5*
    #(  3.9666  -7.2863   0.9812) ; H5**
    #(  5.3098  -5.9546   1.8564) ; C4*
    #(  5.3863  -5.3702   0.9395) ; H4*
    #(  5.3851  -5.0642   3.0076) ; O4*
    #(  6.7315  -4.9724   3.4462) ; C1*
    #(  7.0033  -3.9202   3.3619) ; H1*
    #(  7.5997  -5.8018   2.4948) ; C2*
    #(  8.3627  -6.3254   3.0707) ; H2**
    #(  8.0410  -4.9501   1.4724) ; O2*
    #(  8.2781  -4.0644   1.7570) ; H2*
    #(  6.5701  -6.8129   1.9714) ; C3*
    #(  6.4186  -7.5809   2.7299) ; H3*
    #(  6.9357  -7.3841   0.7235) ; O3*
    #(  6.8024  -5.4718   4.8475) ; N1
    #(  6.6920  -5.0495   7.1354) ; N3
    #(  6.6201  -4.5500   5.8506) ; C2
    #(  6.9254  -6.3614   7.4926) ; C4
    #(  7.1046  -7.2543   6.3718) ; C5
    #(  7.0391  -6.7951   5.1106) ; C6
    rC
    #(  6.9614  -6.6648   8.7815) ; N4
    #(  6.4083  -3.3696   5.6340) ; O2
    #(  7.1329  -7.6280   9.0324) ; H41
    #(  6.8204  -5.9469   9.4777) ; H42
    #(  7.2954  -8.3135   6.5440) ; H5
    #(  7.1753  -7.4798   4.2735) ; H6
  ))

(define rC07
  (nuc-const
    #(  0.0033   0.2720  -0.9623  ; dgf-base-tfo
        0.3013  -0.9179  -0.2584
       -0.9535  -0.2891  -0.0850
       43.0403  13.7233  34.5710)
    #(  0.9187   0.2887   0.2694  ; P-O3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; P-O3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; P-O3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; P
    #( 34.5130  10.2320  46.4660) ; O1P
    #( 33.4130  12.3960  46.9340) ; O2P
    #( 31.9810  10.3390  46.4820) ; O5*
    #( 30.8152  11.1619  46.2003) ; C5*
    #( 30.4519  10.9454  45.1957) ; H5*
    #( 31.0379  12.2016  46.4400) ; H5**
    #( 29.7081  10.7448  47.1428) ; C4*
    #( 28.8710  11.4416  47.0982) ; H4*
    #( 29.2550   9.4394  46.8162) ; O4*
    #( 29.3907   8.5625  47.9460) ; C1*
    #( 28.4416   8.5669  48.4819) ; H1*
    #( 30.4468   9.2031  48.7952) ; C2*
    #( 31.4222   8.9651  48.3709) ; H2**
    #( 30.3701   8.9157  50.1624) ; O2*
    #( 30.0652   8.0304  50.3740) ; H2*
    #( 30.1622  10.6879  48.6120) ; C3*
    #( 31.0952  11.2399  48.7254) ; H3*
    #( 29.1076  11.1535  49.4702) ; O3*
    #( 29.7883   7.2209  47.5235) ; N1
    #( 29.1825   5.0438  46.8275) ; N3
    #( 28.8008   6.2912  47.2263) ; C2
    #( 30.4888   4.6890  46.7186) ; C4
    #( 31.5034   5.6405  47.0249) ; C5
    #( 31.1091   6.8691  47.4156) ; C6
    rC
    #( 30.8109   3.4584  46.3336) ; N4
    #( 27.6171   6.5989  47.3189) ; O2
    #( 31.7923   3.2301  46.2638) ; H41
    #( 30.0880   2.7857  46.1215) ; H42
    #( 32.5542   5.3634  46.9395) ; H5
    #( 31.8523   7.6279  47.6603) ; H6
  ))

(define rC08
  (nuc-const
    #(  0.0797  -0.6026  -0.7941  ; dgf-base-tfo
        0.7939   0.5201  -0.3150
        0.6028  -0.6054   0.5198
      -36.8341  41.5293   1.6628)
    #(  0.9187   0.2887   0.2694  ; P-O3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; P-O3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; P-O3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; P
    #( 34.5130  10.2320  46.4660) ; O1P
    #( 33.4130  12.3960  46.9340) ; O2P
    #( 31.9810  10.3390  46.4820) ; O5*
    #( 31.8779   9.9369  47.8760) ; C5*
    #( 31.3239  10.6931  48.4322) ; H5*
    #( 32.8647   9.6624  48.2489) ; H5**
    #( 31.0429   8.6773  47.9401) ; C4*
    #( 31.0779   8.2331  48.9349) ; H4*
    #( 29.6956   8.9669  47.5983) ; O4*
    #( 29.2784   8.1700  46.4782) ; C1*
    #( 28.8006   7.2731  46.8722) ; H1*
    #( 30.5544   7.7940  45.7875) ; C2*
    #( 30.8837   8.6410  45.1856) ; H2**
    #( 30.5100   6.6007  45.0582) ; O2*
    #( 29.6694   6.4168  44.6326) ; H2*
    #( 31.5146   7.5954  46.9527) ; C3*
    #( 32.5255   7.8261  46.6166) ; H3*
    #( 31.3876   6.2951  47.5516) ; O3*
    #( 28.3976   8.9302  45.5933) ; N1
    #( 26.2155   9.6135  44.9910) ; N3
    #( 27.0281   8.8961  45.8192) ; C2
    #( 26.7044  10.3489  43.9595) ; C4
    #( 28.1088  10.3837  43.7247) ; C5
    #( 28.8978   9.6708  44.5535) ; C6
    rC
    #( 25.8715  11.0249  43.1749) ; N4
    #( 26.5733   8.2371  46.7484) ; O2
    #( 26.2707  11.5609  42.4177) ; H41
    #( 24.8760  10.9939  43.3427) ; H42
    #( 28.5089  10.9722  42.8990) ; H5
    #( 29.9782   9.6687  44.4097) ; H6
  ))

(define rC09
  (nuc-const
    #(  0.8727   0.4760  -0.1091  ; dgf-base-tfo
       -0.4188   0.6148  -0.6682
       -0.2510   0.6289   0.7359
       -8.1687 -52.0761 -25.0726)
    #(  0.9187   0.2887   0.2694  ; P-O3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; P-O3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; P-O3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; P
    #( 34.5130  10.2320  46.4660) ; O1P
    #( 33.4130  12.3960  46.9340) ; O2P
    #( 31.9810  10.3390  46.4820) ; O5*
    #( 30.8152  11.1619  46.2003) ; C5*
    #( 30.4519  10.9454  45.1957) ; H5*
    #( 31.0379  12.2016  46.4400) ; H5**
    #( 29.7081  10.7448  47.1428) ; C4*
    #( 29.4506   9.6945  47.0059) ; H4*
    #( 30.1045  10.9634  48.4885) ; O4*
    #( 29.1794  11.8418  49.1490) ; C1*
    #( 28.4388  11.2210  49.6533) ; H1*
    #( 28.5211  12.6008  48.0367) ; C2*
    #( 29.1947  13.3949  47.7147) ; H2**
    #( 27.2316  13.0683  48.3134) ; O2*
    #( 27.0851  13.3391  49.2227) ; H2*
    #( 28.4131  11.5507  46.9391) ; C3*
    #( 28.4451  12.0512  45.9713) ; H3*
    #( 27.2707  10.6955  47.1097) ; O3*
    #( 29.8751  12.7405  50.0682) ; N1
    #( 30.7172  13.1841  52.2328) ; N3
    #( 30.0617  12.3404  51.3847) ; C2
    #( 31.1834  14.3941  51.8297) ; C4
    #( 30.9913  14.8074  50.4803) ; C5
    #( 30.3434  13.9610  49.6548) ; C6
    rC
    #( 31.8090  15.1847  52.6957) ; N4
    #( 29.6470  11.2494  51.7616) ; O2
    #( 32.1422  16.0774  52.3606) ; H41
    #( 31.9392  14.8893  53.6527) ; H42
    #( 31.3632  15.7771  50.1491) ; H5
    #( 30.1742  14.2374  48.6141) ; H6
  ))

(define rC10
  (nuc-const
    #(  0.1549   0.8710  -0.4663  ; dgf-base-tfo
        0.6768  -0.4374  -0.5921
       -0.7197  -0.2239  -0.6572
       25.2447 -14.1920  50.3201)
    #(  0.9187   0.2887   0.2694  ; P-O3*-275-tfo
        0.0302  -0.7316   0.6811
        0.3938  -0.6176  -0.6808
      -48.4330  26.3254  13.6383)
    #( -0.1504   0.7744  -0.6145  ; P-O3*-180-tfo
        0.7581   0.4893   0.4311
        0.6345  -0.4010  -0.6607
      -31.9784 -13.4285  44.9650)
    #( -0.6236  -0.7810  -0.0337  ; P-O3*-60-tfo
       -0.6890   0.5694  -0.4484
        0.3694  -0.2564  -0.8932
       12.1105  30.8774  46.0946)
    #( 33.3400  11.0980  46.1750) ; P
    #( 34.5130  10.2320  46.4660) ; O1P
    #( 33.4130  12.3960  46.9340) ; O2P
    #( 31.9810  10.3390  46.4820) ; O5*
    #( 31.8779   9.9369  47.8760) ; C5*
    #( 31.3239  10.6931  48.4322) ; H5*
    #( 32.8647   9.6624  48.2489) ; H5**
    #( 31.0429   8.6773  47.9401) ; C4*
    #( 30.0440   8.8473  47.5383) ; H4*
    #( 31.6749   7.6351  47.2119) ; O4*
    #( 31.9159   6.5022  48.0616) ; C1*
    #( 31.0691   5.8243  47.9544) ; H1*
    #( 31.9300   7.0685  49.4493) ; C2*
    #( 32.9024   7.5288  49.6245) ; H2**
    #( 31.5672   6.1750  50.4632) ; O2*
    #( 31.8416   5.2663  50.3200) ; H2*
    #( 30.8618   8.1514  49.3749) ; C3*
    #( 31.1122   8.9396  50.0850) ; H3*
    #( 29.5351   7.6245  49.5409) ; O3*
    #( 33.1890   5.8629  47.7343) ; N1
    #( 34.4004   4.2636  46.4828) ; N3
    #( 33.2062   4.8497  46.7851) ; C2
    #( 35.5600   4.6374  47.0822) ; C4
    #( 35.5444   5.6751  48.0577) ; C5
    #( 34.3565   6.2450  48.3432) ; C6
    rC
    #( 36.6977   4.0305  46.7598) ; N4
    #( 32.1661   4.5034  46.2348) ; O2
    #( 37.5405   4.3347  47.2259) ; H41
    #( 36.7033   3.2923  46.0706) ; H42
    #( 36.4713   5.9811  48.5428) ; H5
    #( 34.2986   7.0426  49.0839) ; H6
  ))

(define rCs
  (list rC01 rC02 rC03 rC04 rC05 rC06 rC07 rC08 rC09 rC10))

(define rG
  (nuc-const
    #( -0.0018  -0.8207   0.5714  ; dgf-base-tfo
        0.2679  -0.5509  -0.7904
        0.9634   0.1517   0.2209
        0.0073   8.4030   0.6232)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  5.4550   8.2120  -2.8810) ; C5*
    #(  5.4546   8.8508  -1.9978) ; H5*
    #(  5.7588   8.6625  -3.8259) ; H5**
    #(  6.4970   7.1480  -2.5980) ; C4*
    #(  7.4896   7.5919  -2.5214) ; H4*
    #(  6.1630   6.4860  -1.3440) ; O4*
    #(  6.5400   5.1200  -1.4190) ; C1*
    #(  7.2763   4.9681  -0.6297) ; H1*
    #(  7.1940   4.8830  -2.7770) ; C2*
    #(  6.8667   3.9183  -3.1647) ; H2**
    #(  8.5860   5.0910  -2.6140) ; O2*
    #(  8.9510   4.7626  -1.7890) ; H2*
    #(  6.5720   6.0040  -3.6090) ; C3*
    #(  5.5636   5.7066  -3.8966) ; H3*
    #(  7.3801   6.3562  -4.7350) ; O3*
    #(  4.7150   0.4910  -0.1360) ; N1
    #(  6.3490   2.1730  -0.6020) ; N3
    #(  5.9530   0.9650  -0.2670) ; C2
    #(  5.2900   2.9790  -0.8260) ; C4
    #(  3.9720   2.6390  -0.7330) ; C5
    #(  3.6770   1.3160  -0.3660) ; C6
    rG
    #(  6.8426   0.0056  -0.0019) ; N2
    #(  3.1660   3.7290  -1.0360) ; N7
    #(  5.3170   4.2990  -1.1930) ; N9
    #(  4.0100   4.6780  -1.2990) ; C8
    #(  2.4280   0.8450  -0.2360) ; O6
    #(  4.6151  -0.4677   0.1305) ; H1
    #(  6.6463  -0.9463   0.2729) ; H21
    #(  7.8170   0.2642  -0.0640) ; H22
    #(  3.4421   5.5744  -1.5482) ; H8
  ))

(define rG01
  (nuc-const
    #( -0.0043  -0.8175   0.5759  ; dgf-base-tfo
        0.2617  -0.5567  -0.7884
        0.9651   0.1473   0.2164
        0.0359   8.3929   0.5532)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  5.4352   8.2183  -2.7757) ; C5*
    #(  5.3830   8.7883  -1.8481) ; H5*
    #(  5.7729   8.7436  -3.6691) ; H5**
    #(  6.4830   7.1518  -2.5252) ; C4*
    #(  7.4749   7.5972  -2.4482) ; H4*
    #(  6.1626   6.4620  -1.2827) ; O4*
    #(  6.5431   5.0992  -1.3905) ; C1*
    #(  7.2871   4.9328  -0.6114) ; H1*
    #(  7.1852   4.8935  -2.7592) ; C2*
    #(  6.8573   3.9363  -3.1645) ; H2**
    #(  8.5780   5.1025  -2.6046) ; O2*
    #(  8.9516   4.7577  -1.7902) ; H2*
    #(  6.5522   6.0300  -3.5612) ; C3*
    #(  5.5420   5.7356  -3.8459) ; H3*
    #(  7.3487   6.4089  -4.6867) ; O3*
    #(  4.7442   0.4514  -0.1390) ; N1
    #(  6.3687   2.1459  -0.5926) ; N3
    #(  5.9795   0.9335  -0.2657) ; C2
    #(  5.3052   2.9471  -0.8125) ; C4
    #(  3.9891   2.5987  -0.7230) ; C5
    #(  3.7016   1.2717  -0.3647) ; C6
    rG
    #(  6.8745  -0.0224  -0.0058) ; N2
    #(  3.1770   3.6859  -1.0198) ; N7
    #(  5.3247   4.2695  -1.1710) ; N9
    #(  4.0156   4.6415  -1.2759) ; C8
    #(  2.4553   0.7925  -0.2390) ; O6
    #(  4.6497  -0.5095   0.1212) ; H1
    #(  6.6836  -0.9771   0.2627) ; H21
    #(  7.8474   0.2424  -0.0653) ; H22
    #(  3.4426   5.5361  -1.5199) ; H8
  ))

(define rG02
  (nuc-const
    #(  0.5566   0.0449   0.8296  ; dgf-base-tfo
        0.5125   0.7673  -0.3854
       -0.6538   0.6397   0.4041
       -9.1161  -3.7679  -2.9968)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.5778   6.6594  -4.0364) ; C5*
    #(  4.9220   7.1963  -4.9204) ; H5*
    #(  3.7996   5.9091  -4.1764) ; H5**
    #(  5.7873   5.8869  -3.5482) ; C4*
    #(  6.0405   5.0875  -4.2446) ; H4*
    #(  6.9135   6.8036  -3.4310) ; O4*
    #(  7.7293   6.4084  -2.3392) ; C1*
    #(  8.7078   6.1815  -2.7624) ; H1*
    #(  7.1305   5.1418  -1.7347) ; C2*
    #(  7.2040   5.1982  -0.6486) ; H2**
    #(  7.7417   4.0392  -2.3813) ; O2*
    #(  8.6785   4.1443  -2.5630) ; H2*
    #(  5.6666   5.2728  -2.1536) ; C3*
    #(  5.1747   5.9805  -1.4863) ; H3*
    #(  4.9997   4.0086  -2.1973) ; O3*
    #( 10.3245   8.5459   1.5467) ; N1
    #(  9.8051   6.9432  -0.1497) ; N3
    #( 10.5175   7.4328   0.8408) ; C2
    #(  8.7523   7.7422  -0.4228) ; C4
    #(  8.4257   8.9060   0.2099) ; C5
    #(  9.2665   9.3242   1.2540) ; C6
    rG
    #( 11.6077   6.7966   1.2752) ; N2
    #(  7.2750   9.4537  -0.3428) ; N7
    #(  7.7962   7.5519  -1.3859) ; N9
    #(  6.9479   8.6157  -1.2771) ; C8
    #(  9.0664  10.4462   1.9610) ; O6
    #( 10.9838   8.7524   2.2697) ; H1
    #( 12.2274   7.0896   2.0170) ; H21
    #( 11.8502   5.9398   0.7984) ; H22
    #(  6.0430   8.9853  -1.7594) ; H8
  ))

(define rG03
  (nuc-const
    #( -0.5021   0.0731   0.8617  ; dgf-base-tfo
       -0.8112   0.3054  -0.4986
       -0.2996  -0.9494  -0.0940
        6.4273  -5.1944  -3.7807)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.1214   6.7116  -1.9049) ; C5*
    #(  3.3465   5.9610  -2.0607) ; H5*
    #(  4.0789   7.2928  -0.9837) ; H5**
    #(  5.4170   5.9293  -1.8186) ; C4*
    #(  5.4506   5.3400  -0.9023) ; H4*
    #(  5.5067   5.0417  -2.9703) ; O4*
    #(  6.8650   4.9152  -3.3612) ; C1*
    #(  7.1090   3.8577  -3.2603) ; H1*
    #(  7.7152   5.7282  -2.3894) ; C2*
    #(  8.5029   6.2356  -2.9463) ; H2**
    #(  8.1036   4.8568  -1.3419) ; O2*
    #(  8.3270   3.9651  -1.6184) ; H2*
    #(  6.7003   6.7565  -1.8911) ; C3*
    #(  6.5898   7.5329  -2.6482) ; H3*
    #(  7.0505   7.2878  -0.6105) ; O3*
    #(  9.6740   4.7656  -7.6614) ; N1
    #(  9.0739   4.3013  -5.3941) ; N3
    #(  9.8416   4.2192  -6.4581) ; C2
    #(  7.9885   5.0632  -5.6446) ; C4
    #(  7.6822   5.6856  -6.8194) ; C5
    #(  8.5831   5.5215  -7.8840) ; C6
    rG
    #( 10.9733   3.5117  -6.4286) ; N2
    #(  6.4857   6.3816  -6.7035) ; N7
    #(  6.9740   5.3703  -4.7760) ; N9
    #(  6.1133   6.1613  -5.4808) ; C8
    #(  8.4084   6.0747  -9.0933) ; O6
    #( 10.3759   4.5855  -8.3504) ; H1
    #( 11.6254   3.3761  -7.1879) ; H21
    #( 11.1917   3.0460  -5.5593) ; H22
    #(  5.1705   6.6830  -5.3167) ; H8
  ))

(define rG04
  (nuc-const
    #( -0.5426  -0.8175   0.1929  ; dgf-base-tfo
        0.8304  -0.5567  -0.0237
        0.1267   0.1473   0.9809
       -0.5075   8.3929   0.2229)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  5.4352   8.2183  -2.7757) ; C5*
    #(  5.3830   8.7883  -1.8481) ; H5*
    #(  5.7729   8.7436  -3.6691) ; H5**
    #(  6.4830   7.1518  -2.5252) ; C4*
    #(  7.4749   7.5972  -2.4482) ; H4*
    #(  6.1626   6.4620  -1.2827) ; O4*
    #(  6.5431   5.0992  -1.3905) ; C1*
    #(  7.2871   4.9328  -0.6114) ; H1*
    #(  7.1852   4.8935  -2.7592) ; C2*
    #(  6.8573   3.9363  -3.1645) ; H2**
    #(  8.5780   5.1025  -2.6046) ; O2*
    #(  8.9516   4.7577  -1.7902) ; H2*
    #(  6.5522   6.0300  -3.5612) ; C3*
    #(  5.5420   5.7356  -3.8459) ; H3*
    #(  7.3487   6.4089  -4.6867) ; O3*
    #(  3.6343   2.6680   2.0783) ; N1
    #(  5.4505   3.9805   1.2446) ; N3
    #(  4.7540   3.3816   2.1851) ; C2
    #(  4.8805   3.7951   0.0354) ; C4
    #(  3.7416   3.0925  -0.2305) ; C5
    #(  3.0873   2.4980   0.8606) ; C6
    rG
    #(  5.1433   3.4373   3.4609) ; N2
    #(  3.4605   3.1184  -1.5906) ; N7
    #(  5.3247   4.2695  -1.1710) ; N9
    #(  4.4244   3.8244  -2.0953) ; C8
    #(  1.9600   1.7805   0.7462) ; O6
    #(  3.2489   2.2879   2.9191) ; H1
    #(  4.6785   3.0243   4.2568) ; H21
    #(  5.9823   3.9654   3.6539) ; H22
    #(  4.2675   3.8876  -3.1721) ; H8
  ))

(define rG05
  (nuc-const
    #( -0.5891   0.0449   0.8068  ; dgf-base-tfo
        0.5375   0.7673   0.3498
       -0.6034   0.6397  -0.4762
       -0.3019  -3.7679  -9.5913)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.5778   6.6594  -4.0364) ; C5*
    #(  4.9220   7.1963  -4.9204) ; H5*
    #(  3.7996   5.9091  -4.1764) ; H5**
    #(  5.7873   5.8869  -3.5482) ; C4*
    #(  6.0405   5.0875  -4.2446) ; H4*
    #(  6.9135   6.8036  -3.4310) ; O4*
    #(  7.7293   6.4084  -2.3392) ; C1*
    #(  8.7078   6.1815  -2.7624) ; H1*
    #(  7.1305   5.1418  -1.7347) ; C2*
    #(  7.2040   5.1982  -0.6486) ; H2**
    #(  7.7417   4.0392  -2.3813) ; O2*
    #(  8.6785   4.1443  -2.5630) ; H2*
    #(  5.6666   5.2728  -2.1536) ; C3*
    #(  5.1747   5.9805  -1.4863) ; H3*
    #(  4.9997   4.0086  -2.1973) ; O3*
    #( 10.2594  10.6774  -1.0056) ; N1
    #(  9.7528   8.7080  -2.2631) ; N3
    #( 10.4471   9.7876  -1.9791) ; C2
    #(  8.7271   8.5575  -1.3991) ; C4
    #(  8.4100   9.3803  -0.3580) ; C5
    #(  9.2294  10.5030  -0.1574) ; C6
    rG
    #( 11.5110  10.1256  -2.7114) ; N2
    #(  7.2891   8.9068   0.3121) ; N7
    #(  7.7962   7.5519  -1.3859) ; N9
    #(  6.9702   7.8292  -0.3353) ; C8
    #(  9.0349  11.3951   0.8250) ; O6
    #( 10.9013  11.4422  -0.9512) ; H1
    #( 12.1031  10.9341  -2.5861) ; H21
    #( 11.7369   9.5180  -3.4859) ; H22
    #(  6.0888   7.3990   0.1403) ; H8
  ))

(define rG06
  (nuc-const
    #( -0.9815   0.0731  -0.1772  ; dgf-base-tfo
        0.1912   0.3054  -0.9328
       -0.0141  -0.9494  -0.3137
        5.7506  -5.1944   4.7470)
    #( -0.8143  -0.5091  -0.2788  ; P-O3*-275-tfo
       -0.0433  -0.4257   0.9038
       -0.5788   0.7480   0.3246
        1.5227   6.9114  -7.0765)
    #(  0.3822  -0.7477   0.5430  ; P-O3*-180-tfo
        0.4552   0.6637   0.5935
       -0.8042   0.0203   0.5941
       -6.9472  -4.1186  -5.9108)
    #(  0.5640   0.8007  -0.2022  ; P-O3*-60-tfo
       -0.8247   0.5587  -0.0878
        0.0426   0.2162   0.9754
        6.2694  -7.0540   3.3316)
    #(  2.8930   8.5380  -3.3280) ; P
    #(  1.6980   7.6960  -3.5570) ; O1P
    #(  3.2260   9.5010  -4.4020) ; O2P
    #(  4.1590   7.6040  -3.0340) ; O5*
    #(  4.1214   6.7116  -1.9049) ; C5*
    #(  3.3465   5.9610  -2.0607) ; H5*
    #(  4.0789   7.2928  -0.9837) ; H5**
    #(  5.4170   5.9293  -1.8186) ; C4*
    #(  5.4506   5.3400  -0.9023) ; H4*
    #(  5.5067   5.0417  -2.9703) ; O4*
    #(  6.8650   4.9152  -3.3612) ; C1*
    #(  7.1090   3.8577  -3.2603) ; H1*
    #(  7.7152   5.7282  -2.3894) ; C2*
    #(  8.5029   6.2356  -2.9463) ; H2**
    #(  8.1036   4.8568  -1.3419) ; O2*
    #(  8.3270   3.9651  -1.6184) ; H2*
    #(  6.7003   6.7565  -1.8911) ; C3*
    #(  6.5898   7.5329  -2.6482) ; H3*
    #(  7.0505   7.2878  -0.6105) ; O3*
    #(  6.6624   3.5061  -8.2986) ; N1
    #(  6.5810   3.2570  -5.9221) ; N3
    #(  6.5151   2.8263  -7.1625) ; C2
    #(  6.8364   4.5817  -5.8882) ; C4
    #(  7.0116   5.4064  -6.9609) ; C5
    #(  6.9173   4.8260  -8.2361) ; C6
    rG
    #(  6.2717   1.5402  -7.4250) ; N2
    #(  7.2573   6.7070  -6.5394) ; N7
    #(  6.9740   5.3703  -4.7760) ; N9
    #(  7.2238   6.6275  -5.2453) ; C8
    #(  7.0668   5.5163  -9.3763) ; O6
    #(  6.5754   2.9964  -9.1545) ; H1
    #(  6.1908   1.1105  -8.3354) ; H21
    #(  6.1346   0.9352  -6.6280) ; H22
    #(  7.4108   7.6227  -4.8418) ; H8
  ))

(define rG07
  (nuc-const
    #(  0.0894  -0.6059   0.7905  ; dgf-base-tfo
       -0.6810   0.5420   0.4924
       -0.7268  -0.5824  -0.3642
       34.1424  45.9610 -11.8600)
    #( -0.8644  -0.4956  -0.0851  ; P-O3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; P-O3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; P-O3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; P
    #( 29.9860   0.6630  47.6290) ; O1P
    #( 31.7210  -0.6460  48.8090) ; O2P
    #( 32.4940   1.2540  47.2740) ; O5*
    #( 33.8709   0.7918  47.2113) ; C5*
    #( 34.1386   0.5870  46.1747) ; H5*
    #( 34.0186  -0.0095  47.9353) ; H5**
    #( 34.7297   1.9687  47.6685) ; C4*
    #( 35.7723   1.6845  47.8113) ; H4*
    #( 34.6455   2.9768  46.6660) ; O4*
    #( 34.1690   4.1829  47.2627) ; C1*
    #( 35.0437   4.7633  47.5560) ; H1*
    #( 33.4145   3.7532  48.4954) ; C2*
    #( 32.4340   3.3797  48.2001) ; H2**
    #( 33.3209   4.6953  49.5217) ; O2*
    #( 33.2374   5.6059  49.2295) ; H2*
    #( 34.2724   2.5970  48.9773) ; C3*
    #( 33.6373   1.8935  49.5157) ; H3*
    #( 35.3453   3.1884  49.7285) ; O3*
    #( 34.0511   7.8930  43.7791) ; N1
    #( 34.9937   6.3369  45.3199) ; N3
    #( 35.0882   7.3126  44.4200) ; C2
    #( 33.7190   5.9650  45.5374) ; C4
    #( 32.5845   6.4770  44.9458) ; C5
    #( 32.7430   7.5179  43.9914) ; C6
    rG
    #( 36.3030   7.7827  44.1036) ; N2
    #( 31.4499   5.8335  45.4368) ; N7
    #( 33.2760   4.9817  46.4043) ; N9
    #( 31.9235   4.9639  46.2934) ; C8
    #( 31.8602   8.1000  43.3695) ; O6
    #( 34.2623   8.6223  43.1283) ; H1
    #( 36.5188   8.5081  43.4347) ; H21
    #( 37.0888   7.3524  44.5699) ; H22
    #( 31.0815   4.4201  46.7218) ; H8
  ))

(define rG08
  (nuc-const
    #(  0.2224   0.6335   0.7411  ; dgf-base-tfo
       -0.3644  -0.6510   0.6659
        0.9043  -0.4181   0.0861
      -47.6824  -0.5823 -31.7554)
    #( -0.8644  -0.4956  -0.0851  ; P-O3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; P-O3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; P-O3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; P
    #( 29.9860   0.6630  47.6290) ; O1P
    #( 31.7210  -0.6460  48.8090) ; O2P
    #( 32.4940   1.2540  47.2740) ; O5*
    #( 32.5924   2.3488  48.2255) ; C5*
    #( 33.3674   2.1246  48.9584) ; H5*
    #( 31.5994   2.5917  48.6037) ; H5**
    #( 33.0722   3.5577  47.4258) ; C4*
    #( 33.0310   4.4778  48.0089) ; H4*
    #( 34.4173   3.3055  47.0316) ; O4*
    #( 34.5056   3.3910  45.6094) ; C1*
    #( 34.7881   4.4152  45.3663) ; H1*
    #( 33.1122   3.1198  45.1010) ; C2*
    #( 32.9230   2.0469  45.1369) ; H2**
    #( 32.7946   3.6590  43.8529) ; O2*
    #( 33.5170   3.6707  43.2207) ; H2*
    #( 32.2730   3.8173  46.1566) ; C3*
    #( 31.3094   3.3123  46.2244) ; H3*
    #( 32.2391   5.2039  45.7807) ; O3*
    #( 39.3337   2.7157  44.1441) ; N1
    #( 37.4430   3.8242  45.0824) ; N3
    #( 38.7276   3.7646  44.7403) ; C2
    #( 36.7791   2.6963  44.7704) ; C4
    #( 37.2860   1.5653  44.1678) ; C5
    #( 38.6647   1.5552  43.8235) ; C6
    rG
    #( 39.5123   4.8216  44.9936) ; N2
    #( 36.2829   0.6110  44.0078) ; N7
    #( 35.4394   2.4314  44.9931) ; N9
    #( 35.2180   1.1815  44.5128) ; C8
    #( 39.2907   0.6514  43.2796) ; O6
    #( 40.3076   2.8048  43.9352) ; H1
    #( 40.4994   4.9066  44.7977) ; H21
    #( 39.0738   5.6108  45.4464) ; H22
    #( 34.3856   0.4842  44.4185) ; H8
  ))

(define rG09
  (nuc-const
    #( -0.9699  -0.1688  -0.1753  ; dgf-base-tfo
       -0.1050  -0.3598   0.9271
       -0.2196   0.9176   0.3312
       45.6217 -38.9484 -12.3208)
    #( -0.8644  -0.4956  -0.0851  ; P-O3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; P-O3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; P-O3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; P
    #( 29.9860   0.6630  47.6290) ; O1P
    #( 31.7210  -0.6460  48.8090) ; O2P
    #( 32.4940   1.2540  47.2740) ; O5*
    #( 33.8709   0.7918  47.2113) ; C5*
    #( 34.1386   0.5870  46.1747) ; H5*
    #( 34.0186  -0.0095  47.9353) ; H5**
    #( 34.7297   1.9687  47.6685) ; C4*
    #( 34.5880   2.8482  47.0404) ; H4*
    #( 34.3575   2.2770  49.0081) ; O4*
    #( 35.5157   2.1993  49.8389) ; C1*
    #( 35.9424   3.2010  49.8893) ; H1*
    #( 36.4701   1.2820  49.1169) ; C2*
    #( 36.1545   0.2498  49.2683) ; H2**
    #( 37.8262   1.4547  49.4008) ; O2*
    #( 38.0227   1.6945  50.3094) ; H2*
    #( 36.2242   1.6797  47.6725) ; C3*
    #( 36.4297   0.8197  47.0351) ; H3*
    #( 37.0289   2.8480  47.4426) ; O3*
    #( 34.3005   3.5042  54.6070) ; N1
    #( 34.7693   3.7936  52.2874) ; N3
    #( 34.4484   4.2541  53.4939) ; C2
    #( 34.9354   2.4584  52.2785) ; C4
    #( 34.8092   1.5915  53.3422) ; C5
    #( 34.4646   2.1367  54.6085) ; C6
    rG
    #( 34.2514   5.5708  53.6503) ; N2
    #( 35.0641   0.2835  52.9337) ; N7
    #( 35.2669   1.6690  51.1915) ; N9
    #( 35.3288   0.3954  51.6563) ; C8
    #( 34.3151   1.5317  55.6650) ; O6
    #( 34.0623   3.9797  55.4539) ; H1
    #( 33.9950   6.0502  54.5016) ; H21
    #( 34.3512   6.1432  52.8242) ; H22
    #( 35.5414  -0.6006  51.2679) ; H8
  ))

(define rG10
  (nuc-const
    #( -0.0980  -0.9723   0.2122  ; dgf-base-tfo
       -0.9731   0.1383   0.1841
       -0.2083  -0.1885  -0.9597
       17.8469  38.8265  37.0475)
    #( -0.8644  -0.4956  -0.0851  ; P-O3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; P-O3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; P-O3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; P
    #( 29.9860   0.6630  47.6290) ; O1P
    #( 31.7210  -0.6460  48.8090) ; O2P
    #( 32.4940   1.2540  47.2740) ; O5*
    #( 32.5924   2.3488  48.2255) ; C5*
    #( 33.3674   2.1246  48.9584) ; H5*
    #( 31.5994   2.5917  48.6037) ; H5**
    #( 33.0722   3.5577  47.4258) ; C4*
    #( 34.0333   3.3761  46.9447) ; H4*
    #( 32.0890   3.8338  46.4332) ; O4*
    #( 31.6377   5.1787  46.5914) ; C1*
    #( 32.2499   5.8016  45.9392) ; H1*
    #( 31.9167   5.5319  48.0305) ; C2*
    #( 31.1507   5.0820  48.6621) ; H2**
    #( 32.0865   6.8890  48.3114) ; O2*
    #( 31.5363   7.4819  47.7942) ; H2*
    #( 33.2398   4.8224  48.2563) ; C3*
    #( 33.3166   4.5570  49.3108) ; H3*
    #( 34.2528   5.7056  47.7476) ; O3*
    #( 28.2782   6.3049  42.9364) ; N1
    #( 30.4001   5.8547  43.9258) ; N3
    #( 29.6195   6.1568  42.8913) ; C2
    #( 29.7005   5.7006  45.0649) ; C4
    #( 28.3383   5.8221  45.2343) ; C5
    #( 27.5519   6.1461  44.0958) ; C6
    rG
    #( 30.1838   6.3385  41.6890) ; N2
    #( 27.9936   5.5926  46.5651) ; N7
    #( 30.2046   5.3825  46.3136) ; N9
    #( 29.1371   5.3398  47.1506) ; C8
    #( 26.3361   6.3024  44.0495) ; O6
    #( 27.8122   6.5394  42.0833) ; H1
    #( 29.7125   6.5595  40.8235) ; H21
    #( 31.1859   6.2231  41.6389) ; H22
    #( 28.9406   5.1504  48.2059) ; H8
  ))

(define rGs
  (list rG01 rG02 rG03 rG04 rG05 rG06 rG07 rG08 rG09 rG10))

(define rU
  (nuc-const
    #( -0.0359  -0.8071   0.5894  ; dgf-base-tfo
       -0.2669   0.5761   0.7726
       -0.9631  -0.1296  -0.2361
        0.1584   8.3434   0.5434)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  5.2430  -8.2420   2.8260) ; C5*
    #(  5.1974  -8.8497   1.9223) ; H5*
    #(  5.5548  -8.7348   3.7469) ; H5**
    #(  6.3140  -7.2060   2.5510) ; C4*
    #(  7.2954  -7.6762   2.4898) ; H4*
    #(  6.0140  -6.5420   1.2890) ; O4*
    #(  6.4190  -5.1840   1.3620) ; C1*
    #(  7.1608  -5.0495   0.5747) ; H1*
    #(  7.0760  -4.9560   2.7270) ; C2*
    #(  6.7770  -3.9803   3.1099) ; H2**
    #(  8.4500  -5.1930   2.5810) ; O2*
    #(  8.8309  -4.8755   1.7590) ; H2*
    #(  6.4060  -6.0590   3.5580) ; C3*
    #(  5.4021  -5.7313   3.8281) ; H3*
    #(  7.1570  -6.4240   4.7070) ; O3*
    #(  5.2170  -4.3260   1.1690) ; N1
    #(  4.2960  -2.2560   0.6290) ; N3
    #(  5.4330  -3.0200   0.7990) ; C2
    #(  2.9930  -2.6780   0.7940) ; C4
    #(  2.8670  -4.0630   1.1830) ; C5
    #(  3.9570  -4.8300   1.3550) ; C6
    rU
    #(  6.5470  -2.5560   0.6290) ; O2
    #(  2.0540  -1.9000   0.6130) ; O4
    #(  4.4300  -1.3020   0.3600) ; H3
    #(  1.9590  -4.4570   1.3250) ; H5
    #(  3.8460  -5.7860   1.6240) ; H6
  ))

(define rU01
  (nuc-const
    #( -0.0137  -0.8012   0.5983  ; dgf-base-tfo
       -0.2523   0.5817   0.7733
       -0.9675  -0.1404  -0.2101
        0.2031   8.3874   0.4228)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  5.2416  -8.2422   2.8181) ; C5*
    #(  5.2050  -8.8128   1.8901) ; H5*
    #(  5.5368  -8.7738   3.7227) ; H5**
    #(  6.3232  -7.2037   2.6002) ; C4*
    #(  7.3048  -7.6757   2.5577) ; H4*
    #(  6.0635  -6.5092   1.3456) ; O4*
    #(  6.4697  -5.1547   1.4629) ; C1*
    #(  7.2354  -5.0043   0.7018) ; H1*
    #(  7.0856  -4.9610   2.8521) ; C2*
    #(  6.7777  -3.9935   3.2487) ; H2**
    #(  8.4627  -5.1992   2.7423) ; O2*
    #(  8.8693  -4.8638   1.9399) ; H2*
    #(  6.3877  -6.0809   3.6362) ; C3*
    #(  5.3770  -5.7562   3.8834) ; H3*
    #(  7.1024  -6.4754   4.7985) ; O3*
    #(  5.2764  -4.2883   1.2538) ; N1
    #(  4.3777  -2.2062   0.7229) ; N3
    #(  5.5069  -2.9779   0.9088) ; C2
    #(  3.0693  -2.6246   0.8500) ; C4
    #(  2.9279  -4.0146   1.2149) ; C5
    #(  4.0101  -4.7892   1.4017) ; C6
    rU
    #(  6.6267  -2.5166   0.7728) ; O2
    #(  2.1383  -1.8396   0.6581) ; O4
    #(  4.5223  -1.2489   0.4716) ; H3
    #(  2.0151  -4.4065   1.3290) ; H5
    #(  3.8886  -5.7486   1.6535) ; H6
  ))

(define rU02
  (nuc-const
    #(  0.5141   0.0246   0.8574  ; dgf-base-tfo
       -0.5547  -0.7529   0.3542
        0.6542  -0.6577  -0.3734
       -9.1111  -3.4598  -3.2939)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  4.3825  -6.6585   4.0489) ; C5*
    #(  4.6841  -7.2019   4.9443) ; H5*
    #(  3.6189  -5.8889   4.1625) ; H5**
    #(  5.6255  -5.9175   3.5998) ; C4*
    #(  5.8732  -5.1228   4.3034) ; H4*
    #(  6.7337  -6.8605   3.5222) ; O4*
    #(  7.5932  -6.4923   2.4548) ; C1*
    #(  8.5661  -6.2983   2.9064) ; H1*
    #(  7.0527  -5.2012   1.8322) ; C2*
    #(  7.1627  -5.2525   0.7490) ; H2**
    #(  7.6666  -4.1249   2.4880) ; O2*
    #(  8.5944  -4.2543   2.6981) ; H2*
    #(  5.5661  -5.3029   2.2009) ; C3*
    #(  5.0841  -6.0018   1.5172) ; H3*
    #(  4.9062  -4.0452   2.2042) ; O3*
    #(  7.6298  -7.6136   1.4752) ; N1
    #(  8.6945  -8.7046  -0.2857) ; N3
    #(  8.6943  -7.6514   0.6066) ; C2
    #(  7.7426  -9.6987  -0.3801) ; C4
    #(  6.6642  -9.5742   0.5722) ; C5
    #(  6.6391  -8.5592   1.4526) ; C6
    rU
    #(  9.5840  -6.8186   0.6136) ; O2
    #(  7.8505 -10.5925  -1.2223) ; O4
    #(  9.4601  -8.7514  -0.9277) ; H3
    #(  5.9281 -10.2509   0.5782) ; H5
    #(  5.8831  -8.4931   2.1028) ; H6
  ))

(define rU03
  (nuc-const
    #( -0.4993   0.0476   0.8651  ; dgf-base-tfo
        0.8078  -0.3353   0.4847
        0.3132   0.9409   0.1290
        6.2989  -5.2303  -3.8577)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  3.9938  -6.7042   1.9023) ; C5*
    #(  3.2332  -5.9343   2.0319) ; H5*
    #(  3.9666  -7.2863   0.9812) ; H5**
    #(  5.3098  -5.9546   1.8564) ; C4*
    #(  5.3863  -5.3702   0.9395) ; H4*
    #(  5.3851  -5.0642   3.0076) ; O4*
    #(  6.7315  -4.9724   3.4462) ; C1*
    #(  7.0033  -3.9202   3.3619) ; H1*
    #(  7.5997  -5.8018   2.4948) ; C2*
    #(  8.3627  -6.3254   3.0707) ; H2**
    #(  8.0410  -4.9501   1.4724) ; O2*
    #(  8.2781  -4.0644   1.7570) ; H2*
    #(  6.5701  -6.8129   1.9714) ; C3*
    #(  6.4186  -7.5809   2.7299) ; H3*
    #(  6.9357  -7.3841   0.7235) ; O3*
    #(  6.8024  -5.4718   4.8475) ; N1
    #(  7.9218  -5.5700   6.8877) ; N3
    #(  7.8908  -5.0886   5.5944) ; C2
    #(  6.9789  -6.3827   7.4823) ; C4
    #(  5.8742  -6.7319   6.6202) ; C5
    #(  5.8182  -6.2769   5.3570) ; C6
    rU
    #(  8.7747  -4.3728   5.1568) ; O2
    #(  7.1154  -6.7509   8.6509) ; O4
    #(  8.7055  -5.3037   7.4491) ; H3
    #(  5.1416  -7.3178   6.9665) ; H5
    #(  5.0441  -6.5310   4.7784) ; H6
  ))

(define rU04
  (nuc-const
    #( -0.5669  -0.8012   0.1918  ; dgf-base-tfo
       -0.8129   0.5817   0.0273
       -0.1334  -0.1404  -0.9811
       -0.3279   8.3874   0.3355)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  5.2416  -8.2422   2.8181) ; C5*
    #(  5.2050  -8.8128   1.8901) ; H5*
    #(  5.5368  -8.7738   3.7227) ; H5**
    #(  6.3232  -7.2037   2.6002) ; C4*
    #(  7.3048  -7.6757   2.5577) ; H4*
    #(  6.0635  -6.5092   1.3456) ; O4*
    #(  6.4697  -5.1547   1.4629) ; C1*
    #(  7.2354  -5.0043   0.7018) ; H1*
    #(  7.0856  -4.9610   2.8521) ; C2*
    #(  6.7777  -3.9935   3.2487) ; H2**
    #(  8.4627  -5.1992   2.7423) ; O2*
    #(  8.8693  -4.8638   1.9399) ; H2*
    #(  6.3877  -6.0809   3.6362) ; C3*
    #(  5.3770  -5.7562   3.8834) ; H3*
    #(  7.1024  -6.4754   4.7985) ; O3*
    #(  5.2764  -4.2883   1.2538) ; N1
    #(  3.8961  -3.0896  -0.1893) ; N3
    #(  5.0095  -3.8907  -0.0346) ; C2
    #(  3.0480  -2.6632   0.8116) ; C4
    #(  3.4093  -3.1310   2.1292) ; C5
    #(  4.4878  -3.9124   2.3088) ; C6
    rU
    #(  5.7005  -4.2164  -0.9842) ; O2
    #(  2.0800  -1.9458   0.5503) ; O4
    #(  3.6834  -2.7882  -1.1190) ; H3
    #(  2.8508  -2.8721   2.9172) ; H5
    #(  4.7188  -4.2247   3.2295) ; H6
  ))

(define rU05
  (nuc-const
    #( -0.6298   0.0246   0.7763  ; dgf-base-tfo
       -0.5226  -0.7529  -0.4001
        0.5746  -0.6577   0.4870
       -0.0208  -3.4598  -9.6882)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  4.3825  -6.6585   4.0489) ; C5*
    #(  4.6841  -7.2019   4.9443) ; H5*
    #(  3.6189  -5.8889   4.1625) ; H5**
    #(  5.6255  -5.9175   3.5998) ; C4*
    #(  5.8732  -5.1228   4.3034) ; H4*
    #(  6.7337  -6.8605   3.5222) ; O4*
    #(  7.5932  -6.4923   2.4548) ; C1*
    #(  8.5661  -6.2983   2.9064) ; H1*
    #(  7.0527  -5.2012   1.8322) ; C2*
    #(  7.1627  -5.2525   0.7490) ; H2**
    #(  7.6666  -4.1249   2.4880) ; O2*
    #(  8.5944  -4.2543   2.6981) ; H2*
    #(  5.5661  -5.3029   2.2009) ; C3*
    #(  5.0841  -6.0018   1.5172) ; H3*
    #(  4.9062  -4.0452   2.2042) ; O3*
    #(  7.6298  -7.6136   1.4752) ; N1
    #(  8.5977  -9.5977   0.7329) ; N3
    #(  8.5951  -8.5745   1.6594) ; C2
    #(  7.7372  -9.7371  -0.3364) ; C4
    #(  6.7596  -8.6801  -0.4476) ; C5
    #(  6.7338  -7.6721   0.4408) ; C6
    rU
    #(  9.3993  -8.5377   2.5743) ; O2
    #(  7.8374 -10.6990  -1.1008) ; O4
    #(  9.2924 -10.3081   0.8477) ; H3
    #(  6.0932  -8.6982  -1.1929) ; H5
    #(  6.0481  -6.9515   0.3446) ; H6
  ))

(define rU06
  (nuc-const
    #( -0.9837   0.0476  -0.1733  ; dgf-base-tfo
       -0.1792  -0.3353   0.9249
       -0.0141   0.9409   0.3384
        5.7793  -5.2303   4.5997)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  3.9938  -6.7042   1.9023) ; C5*
    #(  3.2332  -5.9343   2.0319) ; H5*
    #(  3.9666  -7.2863   0.9812) ; H5**
    #(  5.3098  -5.9546   1.8564) ; C4*
    #(  5.3863  -5.3702   0.9395) ; H4*
    #(  5.3851  -5.0642   3.0076) ; O4*
    #(  6.7315  -4.9724   3.4462) ; C1*
    #(  7.0033  -3.9202   3.3619) ; H1*
    #(  7.5997  -5.8018   2.4948) ; C2*
    #(  8.3627  -6.3254   3.0707) ; H2**
    #(  8.0410  -4.9501   1.4724) ; O2*
    #(  8.2781  -4.0644   1.7570) ; H2*
    #(  6.5701  -6.8129   1.9714) ; C3*
    #(  6.4186  -7.5809   2.7299) ; H3*
    #(  6.9357  -7.3841   0.7235) ; O3*
    #(  6.8024  -5.4718   4.8475) ; N1
    #(  6.6920  -5.0495   7.1354) ; N3
    #(  6.6201  -4.5500   5.8506) ; C2
    #(  6.9254  -6.3614   7.4926) ; C4
    #(  7.1046  -7.2543   6.3718) ; C5
    #(  7.0391  -6.7951   5.1106) ; C6
    rU
    #(  6.4083  -3.3696   5.6340) ; O2
    #(  6.9679  -6.6901   8.6800) ; O4
    #(  6.5626  -4.3957   7.8812) ; H3
    #(  7.2781  -8.2254   6.5350) ; H5
    #(  7.1657  -7.4312   4.3503) ; H6
  ))

(define rU07
  (nuc-const
    #( -0.9434   0.3172   0.0971  ; dgf-base-tfo
        0.2294   0.4125   0.8816
        0.2396   0.8539  -0.4619
        8.3625 -52.7147   1.3745)
    #(  0.2765  -0.1121  -0.9545  ; P-O3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; P-O3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; P-O3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; P
    #( 21.9980  14.5500  46.8210) ; O1P
    #( 21.1450  14.0270  44.5420) ; O2P
    #( 22.1250  16.3600  44.9460) ; O5*
    #( 21.5037  16.8594  43.7323) ; C5*
    #( 20.8147  17.6663  43.9823) ; H5*
    #( 21.1086  16.0230  43.1557) ; H5**
    #( 22.5654  17.4874  42.8616) ; C4*
    #( 22.1584  17.7243  41.8785) ; H4*
    #( 23.0557  18.6826  43.4751) ; O4*
    #( 24.4788  18.6151  43.6455) ; C1*
    #( 24.9355  19.0840  42.7739) ; H1*
    #( 24.7958  17.1427  43.6474) ; C2*
    #( 24.5652  16.7400  44.6336) ; H2**
    #( 26.1041  16.8773  43.2455) ; O2*
    #( 26.7516  17.5328  43.5149) ; H2*
    #( 23.8109  16.5979  42.6377) ; C3*
    #( 23.5756  15.5686  42.9084) ; H3*
    #( 24.2890  16.7447  41.2729) ; O3*
    #( 24.9420  19.2174  44.8923) ; N1
    #( 25.2655  20.5636  44.8883) ; N3
    #( 25.1663  21.2219  43.8561) ; C2
    #( 25.6911  21.1219  46.0494) ; C4
    #( 25.8051  20.4068  47.2048) ; C5
    #( 26.2093  20.9962  48.2534) ; C6
    rU
    #( 25.4692  19.0221  47.2053) ; O2
    #( 25.0502  18.4827  46.0370) ; O4
    #( 25.9599  22.1772  46.0966) ; H3
    #( 25.5545  18.4409  48.1234) ; H5
    #( 24.7854  17.4265  45.9883) ; H6
  ))

(define rU08
  (nuc-const
    #( -0.0080  -0.7928   0.6094  ; dgf-base-tfo
       -0.7512   0.4071   0.5197
       -0.6601  -0.4536  -0.5988
       44.1482  30.7036   2.1088)
    #(  0.2765  -0.1121  -0.9545  ; P-O3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; P-O3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; P-O3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; P
    #( 21.9980  14.5500  46.8210) ; O1P
    #( 21.1450  14.0270  44.5420) ; O2P
    #( 22.1250  16.3600  44.9460) ; O5*
    #( 23.5096  16.1227  44.5783) ; C5*
    #( 23.5649  15.8588  43.5222) ; H5*
    #( 23.9621  15.4341  45.2919) ; H5**
    #( 24.2805  17.4138  44.7151) ; C4*
    #( 25.3492  17.2309  44.6030) ; H4*
    #( 23.8497  18.3471  43.7208) ; O4*
    #( 23.4090  19.5681  44.3321) ; C1*
    #( 24.2595  20.2496  44.3524) ; H1*
    #( 23.0418  19.1813  45.7407) ; C2*
    #( 22.0532  18.7224  45.7273) ; H2**
    #( 23.1307  20.2521  46.6291) ; O2*
    #( 22.8888  21.1051  46.2611) ; H2*
    #( 24.0799  18.1326  46.0700) ; C3*
    #( 23.6490  17.4370  46.7900) ; H3*
    #( 25.3329  18.7227  46.5109) ; O3*
    #( 22.2515  20.1624  43.6698) ; N1
    #( 22.4760  21.0609  42.6406) ; N3
    #( 23.6229  21.3462  42.3061) ; C2
    #( 21.3986  21.6081  42.0236) ; C4
    #( 20.1189  21.3012  42.3804) ; C5
    #( 19.1599  21.8516  41.7578) ; C6
    rU
    #( 19.8919  20.3745  43.4387) ; O2
    #( 20.9790  19.8423  44.0440) ; O4
    #( 21.5235  22.3222  41.2097) ; H3
    #( 18.8732  20.1200  43.7312) ; H5
    #( 20.8545  19.1313  44.8608) ; H6
  ))

(define rU09
  (nuc-const
    #( -0.0317   0.1374   0.9900  ; dgf-base-tfo
       -0.3422  -0.9321   0.1184
        0.9391  -0.3351   0.0765
      -32.1929  25.8198 -28.5088)
    #(  0.2765  -0.1121  -0.9545  ; P-O3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; P-O3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; P-O3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; P
    #( 21.9980  14.5500  46.8210) ; O1P
    #( 21.1450  14.0270  44.5420) ; O2P
    #( 22.1250  16.3600  44.9460) ; O5*
    #( 21.5037  16.8594  43.7323) ; C5*
    #( 20.8147  17.6663  43.9823) ; H5*
    #( 21.1086  16.0230  43.1557) ; H5**
    #( 22.5654  17.4874  42.8616) ; C4*
    #( 23.0565  18.3036  43.3915) ; H4*
    #( 23.5375  16.5054  42.4925) ; O4*
    #( 23.6574  16.4257  41.0649) ; C1*
    #( 24.4701  17.0882  40.7671) ; H1*
    #( 22.3525  16.9643  40.5396) ; C2*
    #( 21.5993  16.1799  40.6133) ; H2**
    #( 22.4693  17.4849  39.2515) ; O2*
    #( 23.0899  17.0235  38.6827) ; H2*
    #( 22.0341  18.0633  41.5279) ; C3*
    #( 20.9509  18.1709  41.5846) ; H3*
    #( 22.7249  19.3020  41.2100) ; O3*
    #( 23.8580  15.0648  40.5757) ; N1
    #( 25.1556  14.5982  40.4523) ; N3
    #( 26.1047  15.3210  40.7448) ; C2
    #( 25.3391  13.3315  40.0020) ; C4
    #( 24.2974  12.5148  39.6749) ; C5
    #( 24.5450  11.3410  39.2610) ; C6
    rU
    #( 22.9633  12.9979  39.8053) ; O2
    #( 22.8009  14.2648  40.2524) ; O4
    #( 26.3414  12.9194  39.8855) ; H3
    #( 22.1227  12.3533  39.5486) ; H5
    #( 21.7989  14.6788  40.3650) ; H6
  ))

(define rU10
  (nuc-const
    #( -0.9674   0.1021  -0.2318  ; dgf-base-tfo
       -0.2514  -0.2766   0.9275
        0.0306   0.9555   0.2933
       27.8571 -42.1305 -24.4563)
    #(  0.2765  -0.1121  -0.9545  ; P-O3*-275-tfo
       -0.8297   0.4733  -0.2959
        0.4850   0.8737   0.0379
      -14.7774 -45.2464  21.9088)
    #(  0.1063  -0.6334  -0.7665  ; P-O3*-180-tfo
       -0.5932  -0.6591   0.4624
       -0.7980   0.4055  -0.4458
       43.7634   4.3296  28.4890)
    #(  0.7136  -0.5032  -0.4873  ; P-O3*-60-tfo
        0.6803   0.3317   0.6536
       -0.1673  -0.7979   0.5791
      -17.1858  41.4390 -27.0751)
    #( 21.3880  15.0780  45.5770) ; P
    #( 21.9980  14.5500  46.8210) ; O1P
    #( 21.1450  14.0270  44.5420) ; O2P
    #( 22.1250  16.3600  44.9460) ; O5*
    #( 23.5096  16.1227  44.5783) ; C5*
    #( 23.5649  15.8588  43.5222) ; H5*
    #( 23.9621  15.4341  45.2919) ; H5**
    #( 24.2805  17.4138  44.7151) ; C4*
    #( 23.8509  18.1819  44.0720) ; H4*
    #( 24.2506  17.8583  46.0741) ; O4*
    #( 25.5830  18.0320  46.5775) ; C1*
    #( 25.8569  19.0761  46.4256) ; H1*
    #( 26.4410  17.1555  45.7033) ; C2*
    #( 26.3459  16.1253  46.0462) ; H2**
    #( 27.7649  17.5888  45.6478) ; O2*
    #( 28.1004  17.9719  46.4616) ; H2*
    #( 25.7796  17.2997  44.3513) ; C3*
    #( 25.9478  16.3824  43.7871) ; H3*
    #( 26.2154  18.4984  43.6541) ; O3*
    #( 25.7321  17.6281  47.9726) ; N1
    #( 25.5136  18.5779  48.9560) ; N3
    #( 25.2079  19.7276  48.6503) ; C2
    #( 25.6482  18.1987  50.2518) ; C4
    #( 25.9847  16.9266  50.6092) ; C5
    #( 26.0918  16.6439  51.8416) ; C6
    rU
    #( 26.2067  15.9515  49.5943) ; O2
    #( 26.0713  16.3497  48.3080) ; O4
    #( 25.4890  18.9105  51.0618) ; H3
    #( 26.4742  14.9310  49.8682) ; H5
    #( 26.2346  15.6394  47.4975) ; H6
  ))

(define rUs
  (list rU01 rU02 rU03 rU04 rU05 rU06 rU07 rU08 rU09 rU10))

(define rG*
  (nuc-const
    #( -0.2067  -0.0264   0.9780  ; dgf-base-tfo
        0.9770  -0.0586   0.2049
        0.0519   0.9979   0.0379
        1.0331 -46.8078 -36.4742)
    #( -0.8644  -0.4956  -0.0851  ; P-O3*-275-tfo
       -0.0427   0.2409  -0.9696
        0.5010  -0.8345  -0.2294
        4.0167  54.5377  12.4779)
    #(  0.3706  -0.6167   0.6945  ; P-O3*-180-tfo
       -0.2867  -0.7872  -0.5460
        0.8834   0.0032  -0.4686
      -52.9020  18.6313  -0.6709)
    #(  0.4155   0.9025  -0.1137  ; P-O3*-60-tfo
        0.9040  -0.4236  -0.0582
       -0.1007  -0.0786  -0.9918
       -7.6624 -25.2080  49.5181)
    #( 31.3810   0.1400  47.5810) ; P
    #( 29.9860   0.6630  47.6290) ; O1P
    #( 31.7210  -0.6460  48.8090) ; O2P
    #( 32.4940   1.2540  47.2740) ; O5*
    #( 32.1610   2.2370  46.2560) ; C5*
    #( 31.2986   2.8190  46.5812) ; H5*
    #( 32.0980   1.7468  45.2845) ; H5**
    #( 33.3476   3.1959  46.1947) ; C4*
    #( 33.2668   3.8958  45.3630) ; H4*
    #( 33.3799   3.9183  47.4216) ; O4*
    #( 34.6515   3.7222  48.0398) ; C1*
    #( 35.2947   4.5412  47.7180) ; H1*
    #( 35.1756   2.4228  47.4827) ; C2*
    #( 34.6778   1.5937  47.9856) ; H2**
    #( 36.5631   2.2672  47.4798) ; O2*
    #( 37.0163   2.6579  48.2305) ; H2*
    #( 34.6953   2.5043  46.0448) ; C3*
    #( 34.5444   1.4917  45.6706) ; H3*
    #( 35.6679   3.3009  45.3487) ; O3*
    #( 37.4804   4.0914  52.2559) ; N1
    #( 36.9670   4.1312  49.9281) ; N3
    #( 37.8045   4.2519  50.9550) ; C2
    #( 35.7171   3.8264  50.3222) ; C4
    #( 35.2668   3.6420  51.6115) ; C5
    #( 36.2037   3.7829  52.6706) ; C6
    rG
    #( 39.0869   4.5552  50.7092) ; N2
    #( 33.9075   3.3338  51.6102) ; N7
    #( 34.6126   3.6358  49.5108) ; N9
    #( 33.5805   3.3442  50.3425) ; C8
    #( 35.9958   3.6512  53.8724) ; O6
    #( 38.2106   4.2053  52.9295) ; H1
    #( 39.8218   4.6863  51.3896) ; H21
    #( 39.3420   4.6857  49.7407) ; H22
    #( 32.5194   3.1070  50.2664) ; H8
  ))

(define rU*
  (nuc-const
    #( -0.0109   0.5907   0.8068  ; dgf-base-tfo
        0.2217  -0.7853   0.5780
        0.9751   0.1852  -0.1224
       -1.4225 -11.0956  -2.5217)
    #( -0.8313  -0.4738  -0.2906  ; P-O3*-275-tfo
        0.0649   0.4366  -0.8973
        0.5521  -0.7648  -0.3322
        1.6833   6.8060  -7.0011)
    #(  0.3445  -0.7630   0.5470  ; P-O3*-180-tfo
       -0.4628  -0.6450  -0.6082
        0.8168  -0.0436  -0.5753
       -6.8179  -3.9778  -5.9887)
    #(  0.5855   0.7931  -0.1682  ; P-O3*-60-tfo
        0.8103  -0.5790   0.0906
       -0.0255  -0.1894  -0.9816
        6.1203  -7.1051   3.1984)
    #(  2.6760  -8.4960   3.2880) ; P
    #(  1.4950  -7.6230   3.4770) ; O1P
    #(  2.9490  -9.4640   4.3740) ; O2P
    #(  3.9730  -7.5950   3.0340) ; O5*
    #(  5.2430  -8.2420   2.8260) ; C5*
    #(  5.1974  -8.8497   1.9223) ; H5*
    #(  5.5548  -8.7348   3.7469) ; H5**
    #(  6.3140  -7.2060   2.5510) ; C4*
    #(  5.8744  -6.2116   2.4731) ; H4*
    #(  7.2798  -7.2260   3.6420) ; O4*
    #(  8.5733  -6.9410   3.1329) ; C1*
    #(  8.9047  -6.0374   3.6446) ; H1*
    #(  8.4429  -6.6596   1.6327) ; C2*
    #(  9.2880  -7.1071   1.1096) ; H2**
    #(  8.2502  -5.2799   1.4754) ; O2*
    #(  8.7676  -4.7284   2.0667) ; H2*
    #(  7.1642  -7.4416   1.3021) ; C3*
    #(  7.4125  -8.5002   1.2260) ; H3*
    #(  6.5160  -6.9772   0.1267) ; O3*
    #(  9.4531  -8.1107   3.4087) ; N1
    #( 11.5931  -9.0015   3.6357) ; N3
    #( 10.8101  -7.8950   3.3748) ; C2
    #( 11.1439 -10.2744   3.9206) ; C4
    #(  9.7056 -10.4026   3.9332) ; C5
    #(  8.9192  -9.3419   3.6833) ; C6
    rU
    #( 11.3013  -6.8063   3.1326) ; O2
    #( 11.9431 -11.1876   4.1375) ; O4
    #( 12.5840  -8.8673   3.6158) ; H3
    #(  9.2891 -11.2898   4.1313) ; H5
    #(  7.9263  -9.4537   3.6977) ; H6
  ))



; -- PARTIAL INSTANTIATIONS ---------------------------------------------------

(define (nuc-make-var id tfo nuc)
  (vector id tfo nuc))

(define (var-id var) (vector-ref var 0))
(define (var-id-set! var val) (vector-set! var 0 val))
(define (var-tfo var) (vector-ref var 1))
(define (var-tfo-set! var val) (vector-set! var 1 val))
(define (var-nuc var) (vector-ref var 2))
(define (var-nuc-set! var val) (vector-set! var 2 val))

(define (atom-pos atom var)
  (tfo-apply (var-tfo var) (atom (var-nuc var))))

(define (nuc-get-var id lst)
  (let ((v (car lst)))
    (if (= id (var-id v))
      v
      (nuc-get-var id (cdr lst)))))

(define (make-relative-nuc tfo n)
  (cond ((rA? n)
         (make-rA
           (nuc-dgf-base-tfo  n)
           (nuc-P-O3*-275-tfo n)
           (nuc-P-O3*-180-tfo n)
           (nuc-P-O3*-60-tfo  n)
           (tfo-apply tfo (nuc-P    n))
           (tfo-apply tfo (nuc-O1P  n))
           (tfo-apply tfo (nuc-O2P  n))
           (tfo-apply tfo (nuc-O5*  n))
           (tfo-apply tfo (nuc-C5*  n))
           (tfo-apply tfo (nuc-H5*  n))
           (tfo-apply tfo (nuc-H5** n))
           (tfo-apply tfo (nuc-C4*  n))
           (tfo-apply tfo (nuc-H4*  n))
           (tfo-apply tfo (nuc-O4*  n))
           (tfo-apply tfo (nuc-C1*  n))
           (tfo-apply tfo (nuc-H1*  n))
           (tfo-apply tfo (nuc-C2*  n))
           (tfo-apply tfo (nuc-H2** n))
           (tfo-apply tfo (nuc-O2*  n))
           (tfo-apply tfo (nuc-H2*  n))
           (tfo-apply tfo (nuc-C3*  n))
           (tfo-apply tfo (nuc-H3*  n))
           (tfo-apply tfo (nuc-O3*  n))
           (tfo-apply tfo (nuc-N1   n))
           (tfo-apply tfo (nuc-N3   n))
           (tfo-apply tfo (nuc-C2   n))
           (tfo-apply tfo (nuc-C4   n))
           (tfo-apply tfo (nuc-C5   n))
           (tfo-apply tfo (nuc-C6   n))
           (tfo-apply tfo (rA-N6    n))
           (tfo-apply tfo (rA-N7    n))
           (tfo-apply tfo (rA-N9    n))
           (tfo-apply tfo (rA-C8    n))
           (tfo-apply tfo (rA-H2    n))
           (tfo-apply tfo (rA-H61   n))
           (tfo-apply tfo (rA-H62   n))
           (tfo-apply tfo (rA-H8    n))))
        ((rC? n)
         (make-rC
           (nuc-dgf-base-tfo  n)
           (nuc-P-O3*-275-tfo n)
           (nuc-P-O3*-180-tfo n)
           (nuc-P-O3*-60-tfo  n)
           (tfo-apply tfo (nuc-P    n))
           (tfo-apply tfo (nuc-O1P  n))
           (tfo-apply tfo (nuc-O2P  n))
           (tfo-apply tfo (nuc-O5*  n))
           (tfo-apply tfo (nuc-C5*  n))
           (tfo-apply tfo (nuc-H5*  n))
           (tfo-apply tfo (nuc-H5** n))
           (tfo-apply tfo (nuc-C4*  n))
           (tfo-apply tfo (nuc-H4*  n))
           (tfo-apply tfo (nuc-O4*  n))
           (tfo-apply tfo (nuc-C1*  n))
           (tfo-apply tfo (nuc-H1*  n))
           (tfo-apply tfo (nuc-C2*  n))
           (tfo-apply tfo (nuc-H2** n))
           (tfo-apply tfo (nuc-O2*  n))
           (tfo-apply tfo (nuc-H2*  n))
           (tfo-apply tfo (nuc-C3*  n))
           (tfo-apply tfo (nuc-H3*  n))
           (tfo-apply tfo (nuc-O3*  n))
           (tfo-apply tfo (nuc-N1   n))
           (tfo-apply tfo (nuc-N3   n))
           (tfo-apply tfo (nuc-C2   n))
           (tfo-apply tfo (nuc-C4   n))
           (tfo-apply tfo (nuc-C5   n))
           (tfo-apply tfo (nuc-C6   n))
           (tfo-apply tfo (rC-N4    n))
           (tfo-apply tfo (rC-O2    n))
           (tfo-apply tfo (rC-H41   n))
           (tfo-apply tfo (rC-H42   n))
           (tfo-apply tfo (rC-H5    n))
           (tfo-apply tfo (rC-H6    n))))
        ((rG? n)
         (make-rG
           (nuc-dgf-base-tfo  n)
           (nuc-P-O3*-275-tfo n)
           (nuc-P-O3*-180-tfo n)
           (nuc-P-O3*-60-tfo  n)
           (tfo-apply tfo (nuc-P    n))
           (tfo-apply tfo (nuc-O1P  n))
           (tfo-apply tfo (nuc-O2P  n))
           (tfo-apply tfo (nuc-O5*  n))
           (tfo-apply tfo (nuc-C5*  n))
           (tfo-apply tfo (nuc-H5*  n))
           (tfo-apply tfo (nuc-H5** n))
           (tfo-apply tfo (nuc-C4*  n))
           (tfo-apply tfo (nuc-H4*  n))
           (tfo-apply tfo (nuc-O4*  n))
           (tfo-apply tfo (nuc-C1*  n))
           (tfo-apply tfo (nuc-H1*  n))
           (tfo-apply tfo (nuc-C2*  n))
           (tfo-apply tfo (nuc-H2** n))
           (tfo-apply tfo (nuc-O2*  n))
           (tfo-apply tfo (nuc-H2*  n))
           (tfo-apply tfo (nuc-C3*  n))
           (tfo-apply tfo (nuc-H3*  n))
           (tfo-apply tfo (nuc-O3*  n))
           (tfo-apply tfo (nuc-N1   n))
           (tfo-apply tfo (nuc-N3   n))
           (tfo-apply tfo (nuc-C2   n))
           (tfo-apply tfo (nuc-C4   n))
           (tfo-apply tfo (nuc-C5   n))
           (tfo-apply tfo (nuc-C6   n))
           (tfo-apply tfo (rG-N2    n))
           (tfo-apply tfo (rG-N7    n))
           (tfo-apply tfo (rG-N9    n))
           (tfo-apply tfo (rG-C8    n))
           (tfo-apply tfo (rG-O6    n))
           (tfo-apply tfo (rG-H1    n))
           (tfo-apply tfo (rG-H21   n))
           (tfo-apply tfo (rG-H22   n))
           (tfo-apply tfo (rG-H8    n))))
        (else
         (make-rU
           (nuc-dgf-base-tfo  n)
           (nuc-P-O3*-275-tfo n)
           (nuc-P-O3*-180-tfo n)
           (nuc-P-O3*-60-tfo  n)
           (tfo-apply tfo (nuc-P    n))
           (tfo-apply tfo (nuc-O1P  n))
           (tfo-apply tfo (nuc-O2P  n))
           (tfo-apply tfo (nuc-O5*  n))
           (tfo-apply tfo (nuc-C5*  n))
           (tfo-apply tfo (nuc-H5*  n))
           (tfo-apply tfo (nuc-H5** n))
           (tfo-apply tfo (nuc-C4*  n))
           (tfo-apply tfo (nuc-H4*  n))
           (tfo-apply tfo (nuc-O4*  n))
           (tfo-apply tfo (nuc-C1*  n))
           (tfo-apply tfo (nuc-H1*  n))
           (tfo-apply tfo (nuc-C2*  n))
           (tfo-apply tfo (nuc-H2** n))
           (tfo-apply tfo (nuc-O2*  n))
           (tfo-apply tfo (nuc-H2*  n))
           (tfo-apply tfo (nuc-C3*  n))
           (tfo-apply tfo (nuc-H3*  n))
           (tfo-apply tfo (nuc-O3*  n))
           (tfo-apply tfo (nuc-N1   n))
           (tfo-apply tfo (nuc-N3   n))
           (tfo-apply tfo (nuc-C2   n))
           (tfo-apply tfo (nuc-C4   n))
           (tfo-apply tfo (nuc-C5   n))
           (tfo-apply tfo (nuc-C6   n))
           (tfo-apply tfo (rU-O2    n))
           (tfo-apply tfo (rU-O4    n))
           (tfo-apply tfo (rU-H3    n))
           (tfo-apply tfo (rU-H5    n))
           (tfo-apply tfo (rU-H6    n))))))

; -- SEARCH -------------------------------------------------------------------

; Sequential backtracking algorithm

(define (search partial-inst domains constraint?)
  (if (null? domains)
    (list partial-inst)
    (let ((remaining-domains (cdr domains)))

      (define (try-assignments lst)
        (if (null? lst)
          '()
          (let ((var (car lst)))
            (if (constraint? var partial-inst)
              (let* ((subsols1
                       (search
                         (cons var partial-inst)
                         remaining-domains
                         constraint?))
                     (subsols2
                       (try-assignments (cdr lst))))
                (append subsols1 subsols2))
              (try-assignments (cdr lst))))))

      (try-assignments ((car domains) partial-inst)))))

; -- DOMAINS ------------------------------------------------------------------

; Primary structure:   strand A CUGCCACGUCUG, strand B CAGACGUGGCAG
;
; Secondary structure: strand A CUGCCACGUCUG
;                               ||||||||||||
;                               GACGGUGCAGAC strand B
;
; Tertiary structure:
;
;    5' end of strand A C1----G12 3' end of strand B
;                     U2-------A11
;                    G3-------C10
;                    C4-----G9
;                     C5---G8
;                        A6
;                      G6-C7
;                     C5----G8
;                    A4-------U9
;                    G3--------C10
;                     A2-------U11
;   5' end of strand B C1----G12 3' end of strand A
;
; "helix", "stacked" and "connected" describe the spatial relationship
; between two consecutive nucleotides. E.g. the nucleotides C1 and U2
; from the strand A.
;
; "wc" (stands for Watson-Crick and is a type of base-pairing),
; and "wc-dumas" describe the spatial relationship between
; nucleotides from two chains that are growing in opposite directions.
; E.g. the nucleotides C1 from strand A and G12 from strand B.

; Dynamic Domains

; Given,
;   "ref" a nucleotide which is already positioned,
;   "nuc" the nucleotide to be placed,
;   and "tfo" a transformation matrix which expresses the desired
;   relationship between "ref" and "nuc",
; the function "dgf-base" computes the transformation matrix that
; places the nucleotide "nuc" in the given relationship to "ref".

(define (dgf-base tfo ref nuc)
  (let* ((ref-nuc (var-nuc ref))
         (align
          (tfo-inv-ortho
            (cond ((rA? ref-nuc)
                   (tfo-align (atom-pos nuc-C1* ref)
                              (atom-pos rA-N9   ref)
                              (atom-pos nuc-C4  ref)))
                  ((rC? ref-nuc)
                   (tfo-align (atom-pos nuc-C1* ref)
                              (atom-pos nuc-N1  ref)
                              (atom-pos nuc-C2  ref)))
                  ((rG? ref-nuc)
                   (tfo-align (atom-pos nuc-C1* ref)
                              (atom-pos rG-N9   ref)
                              (atom-pos nuc-C4  ref)))
                  (else
                   (tfo-align (atom-pos nuc-C1* ref)
                              (atom-pos nuc-N1  ref)
                              (atom-pos nuc-C2  ref)))))))
    (tfo-combine (nuc-dgf-base-tfo nuc)
                 (tfo-combine tfo align))))

; Placement of first nucleotide.

(define (reference nuc i)
  (lambda (partial-inst)
    (list (nuc-make-var i tfo-id nuc))))

; The transformation matrix for wc is from:
;
; Chandrasekaran R. et al (1989) A Re-Examination of the Crystal
; Structure of A-DNA Using Fiber Diffraction Data. J. Biomol.
; Struct. & Dynamics 6(6):1189-1202.

(define wc-tfo
  (FLOATvector-const
     -1.0000  0.0028 -0.0019
      0.0028  0.3468 -0.9379
     -0.0019 -0.9379 -0.3468
     -0.0080  6.0730  8.7208))

(define (wc nuc i j)
  (lambda (partial-inst)
    (let* ((ref (nuc-get-var j partial-inst))
           (tfo (dgf-base wc-tfo ref nuc)))
      (list (nuc-make-var i tfo nuc)))))

(define wc-Dumas-tfo
  (FLOATvector-const
     -0.9737 -0.1834  0.1352
     -0.1779  0.2417 -0.9539
      0.1422 -0.9529 -0.2679
      0.4837  6.2649  8.0285))

(define (wc-Dumas nuc i j)
  (lambda (partial-inst)
    (let* ((ref (nuc-get-var j partial-inst))
           (tfo (dgf-base wc-Dumas-tfo ref nuc)))
      (list (nuc-make-var i tfo nuc)))))

(define helix5*-tfo
  (FLOATvector-const
      0.9886 -0.0961  0.1156
      0.1424  0.8452 -0.5152
     -0.0482  0.5258  0.8492
     -3.8737  0.5480  3.8024))

(define (helix5* nuc i j)
  (lambda (partial-inst)
    (let* ((ref (nuc-get-var j partial-inst))
           (tfo (dgf-base helix5*-tfo ref nuc)))
      (list (nuc-make-var i tfo nuc)))))

(define helix3*-tfo
  (FLOATvector-const
      0.9886  0.1424 -0.0482
     -0.0961  0.8452  0.5258
      0.1156 -0.5152  0.8492
      3.4426  2.0474 -3.7042))

(define (helix3* nuc i j)
  (lambda (partial-inst)
    (let* ((ref (nuc-get-var j partial-inst))
           (tfo (dgf-base helix3*-tfo ref nuc)))
      (list (nuc-make-var i tfo nuc)))))

(define G37-A38-tfo
  (FLOATvector-const
      0.9991  0.0164 -0.0387
     -0.0375  0.7616 -0.6470
      0.0189  0.6478  0.7615
     -3.3018  0.9975  2.5585))

(define (G37-A38 nuc i j)
  (lambda (partial-inst)
    (let* ((ref (nuc-get-var j partial-inst))
           (tfo (dgf-base G37-A38-tfo ref nuc)))
      (nuc-make-var i tfo nuc))))

(define (stacked5* nuc i j)
  (lambda (partial-inst)
    (cons ((G37-A38 nuc i j) partial-inst)
          ((helix5* nuc i j) partial-inst))))

(define A38-G37-tfo
  (FLOATvector-const
      0.9991 -0.0375  0.0189
      0.0164  0.7616  0.6478
     -0.0387 -0.6470  0.7615
      3.3819  0.7718 -2.5321))

(define (A38-G37 nuc i j)
  (lambda (partial-inst)
    (let* ((ref (nuc-get-var j partial-inst))
           (tfo (dgf-base A38-G37-tfo ref nuc)))
      (nuc-make-var i tfo nuc))))

(define (stacked3* nuc i j)
  (lambda (partial-inst)
    (cons ((A38-G37 nuc i j) partial-inst)
          ((helix3* nuc i j) partial-inst))))

(define (P-O3* nucs i j)
  (lambda (partial-inst)
    (let* ((ref (nuc-get-var j partial-inst))
           (align
             (tfo-inv-ortho
               (tfo-align (atom-pos nuc-O3* ref)
                          (atom-pos nuc-C3* ref)
                          (atom-pos nuc-C4* ref)))))
      (let loop ((lst nucs) (domains '()))
        (if (null? lst)
          domains
          (let ((nuc (car lst)))
            (let ((tfo-60 (tfo-combine (nuc-P-O3*-60-tfo nuc) align))
                  (tfo-180 (tfo-combine (nuc-P-O3*-180-tfo nuc) align))
                  (tfo-275 (tfo-combine (nuc-P-O3*-275-tfo nuc) align)))
              (loop (cdr lst)
                    (cons (nuc-make-var i tfo-60 nuc)
                          (cons (nuc-make-var i tfo-180 nuc)
                                (cons (nuc-make-var i tfo-275 nuc) domains)))))))))))

; -- PROBLEM STATEMENT --------------------------------------------------------

; Define anticodon problem -- Science 253:1255 Figure 3a, 3b and 3c

(define anticodon-domains
  (list
   (reference rC  27   )
   (helix5*   rC  28 27)
   (helix5*   rA  29 28)
   (helix5*   rG  30 29)
   (helix5*   rA  31 30)
   (wc        rU  39 31)
   (helix5*   rC  40 39)
   (helix5*   rU  41 40)
   (helix5*   rG  42 41)
   (helix5*   rG  43 42)
   (stacked3* rA  38 39)
   (stacked3* rG  37 38)
   (stacked3* rA  36 37)
   (stacked3* rA  35 36)
   (stacked3* rG  34 35);<-. Distance
   (P-O3*     rCs 32 31);  | Constraint
   (P-O3*     rUs 33 32);<-' 3.0 Angstroms
   ))

; Anticodon constraint

(define (anticodon-constraint? v partial-inst)
  (if (= (var-id v) 33)
    (let ((p   (atom-pos nuc-P (nuc-get-var 34 partial-inst))) ; P in nucleotide 34
          (o3* (atom-pos nuc-O3* v)))                      ; O3' in nucl. 33
      (FLOAT<= (pt-dist p o3*) 3.0))                       ; check distance
    #t))

(define (anticodon)
  (search '() anticodon-domains anticodon-constraint?))

; Define pseudoknot problem -- Science 253:1255 Figure 4a and 4b

(define pseudoknot-domains
  (list
   (reference rA  23   )
   (wc-Dumas  rU   8 23)
   (helix3*   rG  22 23)
   (wc-Dumas  rC   9 22)
   (helix3*   rG  21 22)
   (wc-Dumas  rC  10 21)
   (helix3*   rC  20 21)
   (wc-Dumas  rG  11 20)
   (helix3*   rU* 19 20);<-.
   (wc-Dumas  rA  12 19);  | Distance
;                       ;  | Constraint
; Helix 1               ;  | 4.0 Angstroms
   (helix3*   rC   3 19);  |
   (wc-Dumas  rG  13  3);  |
   (helix3*   rC   2  3);  |
   (wc-Dumas  rG  14  2);  |
   (helix3*   rC   1  2);  |
   (wc-Dumas  rG* 15  1);  |
;                       ;  |
; L2 LOOP               ;  |
   (P-O3*     rUs 16 15);  |
   (P-O3*     rCs 17 16);  |
   (P-O3*     rAs 18 17);<-'
;
; L1 LOOP
   (helix3*   rU   7  8);<-.
   (P-O3*     rCs  4  3);  | Constraint
   (stacked5* rU   5  4);  | 4.5 Angstroms
   (stacked5* rC   6  5);<-'
   ))

; Pseudoknot constraint

(define (pseudoknot-constraint? v partial-inst)
  (case (var-id v)
    ((18)
     (let ((p   (atom-pos nuc-P (nuc-get-var 19 partial-inst)))
           (o3* (atom-pos nuc-O3* v)))
       (FLOAT<= (pt-dist p o3*) 4.0)))
    ((6)
     (let ((p   (atom-pos nuc-P (nuc-get-var 7 partial-inst)))
           (o3* (atom-pos nuc-O3* v)))
       (FLOAT<= (pt-dist p o3*) 4.5)))
    (else
     #t)))

(define (pseudoknot)
  (search '() pseudoknot-domains pseudoknot-constraint?))

; -- TESTING -----------------------------------------------------------------

(define (list-of-atoms n)
  (append (list-of-common-atoms n)
          (list-of-specific-atoms n)))

(define (list-of-common-atoms n)
  (list
    (nuc-P    n)
    (nuc-O1P  n)
    (nuc-O2P  n)
    (nuc-O5*  n)
    (nuc-C5*  n)
    (nuc-H5*  n)
    (nuc-H5** n)
    (nuc-C4*  n)
    (nuc-H4*  n)
    (nuc-O4*  n)
    (nuc-C1*  n)
    (nuc-H1*  n)
    (nuc-C2*  n)
    (nuc-H2** n)
    (nuc-O2*  n)
    (nuc-H2*  n)
    (nuc-C3*  n)
    (nuc-H3*  n)
    (nuc-O3*  n)
    (nuc-N1   n)
    (nuc-N3   n)
    (nuc-C2   n)
    (nuc-C4   n)
    (nuc-C5   n)
    (nuc-C6   n)))

(define (list-of-specific-atoms n)
  (cond ((rA? n)
         (list
           (rA-N6   n)
           (rA-N7   n)
           (rA-N9   n)
           (rA-C8   n)
           (rA-H2   n)
           (rA-H61  n)
           (rA-H62  n)
           (rA-H8   n)))
        ((rC? n)
         (list
           (rC-N4   n)
           (rC-O2   n)
           (rC-H41  n)
           (rC-H42  n)
           (rC-H5   n)
           (rC-H6   n)))
        ((rG? n)
         (list
           (rG-N2   n)
           (rG-N7   n)
           (rG-N9   n)
           (rG-C8   n)
           (rG-O6   n)
           (rG-H1   n)
           (rG-H21  n)
           (rG-H22  n)
           (rG-H8   n)))
        (else
         (list
           (rU-O2   n)
           (rU-O4   n)
           (rU-H3   n)
           (rU-H5   n)
           (rU-H6   n)))))

(define (var-most-distant-atom v)

  (define (distance pos)
    (let ((abs-pos (tfo-apply (var-tfo v) pos)))
      (let ((x (pt-x abs-pos)) (y (pt-y abs-pos)) (z (pt-z abs-pos)))
        (FLOATsqrt (FLOAT+ (FLOAT* x x) (FLOAT* y y) (FLOAT* z z))))))

  (maximum (map distance (list-of-atoms (var-nuc v)))))

(define (sol-most-distant-atom s)
  (maximum (map var-most-distant-atom s)))

(define (most-distant-atom sols)
  (maximum (map sol-most-distant-atom sols)))

(define (maximum lst)
  (let loop ((m (car lst)) (l (cdr lst)))
    (if (null? l)
      m
      (let ((x (car l)))
        (loop (if (FLOAT> x m) x m) (cdr l))))))

(define (run-nucleic)
  (most-distant-atom (pseudoknot)))

(define (main-nucleic . args)
  (run-single-benchmark
    "nucleic"
    nucleic-iters
    (lambda (result)
      (and (number? result)
           (let ((x (FLOAT/ result 33.797594890762724)))
             (and (FLOAT> x 0.999999) (FLOAT< x 1.000001)))))
    (lambda () (lambda () (run-nucleic)))))

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

(define (main-paraffins . args)
  (run-single-benchmark
   "paraffins"
   paraffins-iters
   (lambda (result) (equal? result 24894))
   (lambda (n) (lambda () (nb n)))
   17))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         perm9.sch
; Description:  memory system benchmark using Zaks's permutation generator
; Author:       Lars Hansen, Will Clinger, and Gene Luks
; Created:      18-Mar-94
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 940720 / lth Added some more benchmarks for the thesis paper.
; 970215 / wdc Increased problem size from 8 to 9; improved tenperm9-benchmark.
; 970531 / wdc Cleaned up for public release.
; 981116 / wdc Simplified to fit in with Feeley's benchmark suite.

; The perm9 benchmark generates a list of all 362880 permutations of
; the first 9 integers, allocating 1349288 pairs (typically 10,794,304
; bytes), all of which goes into the generated list.  (That is, the
; perm9 benchmark generates absolutely no garbage.)  This represents
; a savings of about 63% over the storage that would be required by
; an unshared list of permutations.  The generated permutations are
; in order of a grey code that bears no obvious relationship to a
; lexicographic order.
;
; The 10perm9 benchmark repeats the perm9 benchmark 10 times, so it
; allocates and reclaims 13492880 pairs (typically 107,943,040 bytes).
; The live storage peaks at twice the storage that is allocated by the
; perm9 benchmark.  At the end of each iteration, the oldest half of
; the live storage becomes garbage.  Object lifetimes are distributed
; uniformly between 10.3 and 20.6 megabytes.

; Date: Thu, 17 Mar 94 19:43:32 -0800
; From: luks@sisters.cs.uoregon.edu
; To: will
; Subject: Pancake flips
;
; Procedure P_n generates a grey code of all perms of n elements
; on top of stack ending with reversal of starting sequence
;
; F_n is flip of top n elements.
;
;
; procedure P_n
;
;   if n>1 then
;     begin
;        repeat   P_{n-1},F_n   n-1 times;
;        P_{n-1}
;     end
;

(define (permutations x)
  (let ((x x)
        (perms (list x)))
    (define (P n)
      (if (> n 1)
          (do ((j (- n 1) (- j 1)))
              ((zero? j)
               (P (- n 1)))
              (P (- n 1))
              (F n))))
    (define (F n)
      (set! x (revloop x n (list-tail x n)))
      (set! perms (cons x perms)))
    (define (revloop x n y)
      (if (zero? n)
          y
          (revloop (cdr x)
                   (- n 1)
                   (cons (car x) y))))
    (define (list-tail x n)
      (if (zero? n)
          x
          (list-tail (cdr x) (- n 1))))
    (P (length x))
    perms))

; Given a list of lists of numbers, returns the sum of the sums
; of those lists.
;
; for (; x != NULL; x = x->rest)
;     for (y = x->first; y != NULL; y = y->rest)
;         sum = sum + y->first;

(define (sumlists x)
  (do ((x x (cdr x))
       (sum 0 (do ((y (car x) (cdr y))
                   (sum sum (+ sum (car y))))
                  ((null? y) sum))))
      ((null? x) sum)))

(define (one..n n)
  (do ((n n (- n 1))
       (p '() (cons n p)))
      ((zero? n) p)))

(define (main-perm9 . args)
  (let ((n 9))
    (define (factorial n)
      (if (zero? n)
          1
          (* n (factorial (- n 1)))))
    (run-single-benchmark
      (string-append "perm" (number->string n))
      perm9-iters
      (lambda (result)
        (= (sumlists result)
           (* (quotient (* n (+ n 1)) 2) (factorial n))))
      (lambda (lst)
        (lambda ()
          (permutations lst)))
      (one..n n))))


;;; PEVAL -- A simple partial evaluator for Scheme, written by Marc Feeley.

;------------------------------------------------------------------------------

; Utilities

(define (every? pred? l)
  (let loop ((l l))
    (or (null? l) (and (pred? (car l)) (loop (cdr l))))))

(define (some? pred? l)
  (let loop ((l l))
    (if (null? l) #f (or (pred? (car l)) (loop (cdr l))))))

(define (map2 f l1 l2)
  (let loop ((l1 l1) (l2 l2))
    (if (pair? l1)
      (cons (f (car l1) (car l2)) (loop (cdr l1) (cdr l2)))
      '())))

(define (get-last-pair l)
  (let loop ((l l))
    (let ((x (cdr l))) (if (pair? x) (loop x) l))))

;------------------------------------------------------------------------------
;
; The partial evaluator.

(define (peval-partial-evaluate proc args)
  (peval (alphatize proc '()) args))

(define (alphatize exp env) ; return a copy of 'exp' where each bound var has
  (define (alpha exp)       ; been renamed (to prevent aliasing problems)
    (cond ((const-expr? exp)
           (quot (const-value exp)))
          ((symbol? exp)
           (let ((x (assq exp env))) (if x (cdr x) exp)))
          ((or (eq? (car exp) 'if) (eq? (car exp) 'begin))
           (cons (car exp) (map alpha (cdr exp))))
          ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
           (let ((new-env (peval-new-variables (map car (cadr exp)) env)))
             (list (car exp)
                   (map (lambda (x)
                          (list (cdr (assq (car x) new-env))
                                (if (eq? (car exp) 'let)
                                  (alpha (cadr x))
                                  (alphatize (cadr x) new-env))))
                        (cadr exp))
                   (alphatize (caddr exp) new-env))))
          ((eq? (car exp) 'lambda)
           (let ((new-env (peval-new-variables (cadr exp) env)))
             (list 'lambda
                   (map (lambda (x) (cdr (assq x new-env))) (cadr exp))
                   (alphatize (caddr exp) new-env))))
          (else
           (map alpha exp))))
  (alpha exp))

(define (const-expr? expr) ; is 'expr' a constant expression?
  (and (not (symbol? expr))
       (or (not (pair? expr))
           (eq? (car expr) 'quote))))

(define (const-value expr) ; return the value of a constant expression
  (if (pair? expr) ; then it must be a quoted constant
    (cadr expr)
    expr))

(define (quot val) ; make a quoted constant whose value is 'val'
  (list 'quote val))

(define (peval-new-variables parms env)
  (append (map (lambda (x) (cons x (new-variable x))) parms) env))

(define *current-num* 0)

(define (new-variable name)
  (set! *current-num* (+ *current-num* 1))
  (string->symbol
    (string-append (symbol->string name)
                   "_"
                   (number->string *current-num*))))

;------------------------------------------------------------------------------
;
; (peval proc args) will transform a procedure that is known to be called
; with constants as some of its arguments into a specialized procedure that
; is 'equivalent' but accepts only the non-constant parameters.  'proc' is the
; list representation of a lambda-expression and 'args' is a list of values,
; one for each parameter of the lambda-expression.  A special value (i.e.
; 'not-constant') is used to indicate an argument that is not a constant.
; The returned procedure is one that has as parameters the parameters of the
; original procedure which are NOT passed constants.  Constants will have been
; substituted for the constant parameters that are referenced in the body
; of the procedure.
;
; For example:
;
;   (peval
;     '(lambda (x y z) (f z x y)) ; the procedure
;     (list 1 not-constant #t))   ; the knowledge about x, y and z
;
; will return: (lambda (y) (f '#t '1 y))

(define (peval proc args)
  (simplify!
    (let ((parms (cadr proc))  ; get the parameter list
          (body (caddr proc))) ; get the body of the procedure
      (list 'lambda
            (remove-constant parms args) ; remove the constant parameters
            (beta-subst ; in the body, replace variable refs to the constant
              body      ; parameters by the corresponding constant
              (map2 (lambda (x y) (if (not-constant? y) '(()) (cons x (quot y))))
                    parms
                    args))))))

(define not-constant (list '?)) ; special value indicating non-constant parms.

(define (not-constant? x) (eq? x not-constant))

(define (remove-constant l a) ; remove from list 'l' all elements whose
  (cond ((null? l)            ; corresponding element in 'a' is a constant
         '())
        ((not-constant? (car a))
         (cons (car l) (remove-constant (cdr l) (cdr a))))
        (else
         (remove-constant (cdr l) (cdr a)))))

(define (extract-constant l a) ; extract from list 'l' all elements whose
  (cond ((null? l)             ; corresponding element in 'a' is a constant
         '())
        ((not-constant? (car a))
         (extract-constant (cdr l) (cdr a)))
        (else
         (cons (car l) (extract-constant (cdr l) (cdr a))))))

(define (beta-subst exp env) ; return a modified 'exp' where each var named in
  (define (bs exp)           ; 'env' is replaced by the corresponding expr (it
    (cond ((const-expr? exp) ; is assumed that the code has been alphatized)
           (quot (const-value exp)))
          ((symbol? exp)
           (let ((x (assq exp env)))
             (if x (cdr x) exp)))
          ((or (eq? (car exp) 'if) (eq? (car exp) 'begin))
           (cons (car exp) (map bs (cdr exp))))
          ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
           (list (car exp)
                 (map (lambda (x) (list (car x) (bs (cadr x)))) (cadr exp))
                 (bs (caddr exp))))
          ((eq? (car exp) 'lambda)
           (list 'lambda
                 (cadr exp)
                 (bs (caddr exp))))
          (else
           (map bs exp))))
  (bs exp))

;------------------------------------------------------------------------------
;
; The expression simplifier.

(define (simplify! exp)     ; simplify the expression 'exp' destructively (it
                            ; is assumed that the code has been alphatized)
  (define (simp! where env)

    (define (s! where)
      (let ((exp (car where)))

        (cond ((const-expr? exp))  ; leave constants the way they are

              ((symbol? exp))      ; leave variable references the way they are

              ((eq? (car exp) 'if) ; dead code removal for conditionals
               (s! (cdr exp))      ; simplify the predicate
               (if (const-expr? (cadr exp)) ; is the predicate a constant?
                 (begin
                   (set-car! where
                     (if (memq (const-value (cadr exp)) '(#f ())) ; false?
                       (if (= (length exp) 3) ''() (cadddr exp))
                       (caddr exp)))
                   (s! where))
                 (for-each! s! (cddr exp)))) ; simplify consequent and alt.

              ((eq? (car exp) 'begin)
               (for-each! s! (cdr exp))
               (let loop ((exps exp)) ; remove all useless expressions
                 (if (not (null? (cddr exps))) ; not last expression?
                   (let ((x (cadr exps)))
                     (loop (if (or (const-expr? x)
                                   (symbol? x)
                                   (and (pair? x) (eq? (car x) 'lambda)))
                             (begin (set-cdr! exps (cddr exps)) exps)
                             (cdr exps))))))
               (if (null? (cddr exp)) ; only one expression in the begin?
                 (set-car! where (cadr exp))))

              ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
               (let ((new-env (cons exp env)))
                 (define (keep i)
                   (if (>= i (length (cadar where)))
                     '()
                     (let* ((var (car (list-ref (cadar where) i)))
                            (val (cadr (assq var (cadar where))))
                            (refs (ref-count (car where) var))
                            (self-refs (ref-count val var))
                            (total-refs (- (car refs) (car self-refs)))
                            (oper-refs (- (cadr refs) (cadr self-refs))))
                       (cond ((= total-refs 0)
                              (keep (+ i 1)))
                             ((or (const-expr? val)
                                  (symbol? val)
                                  (and (pair? val)
                                       (eq? (car val) 'lambda)
                                       (= total-refs 1)
                                       (= oper-refs 1)
                                       (= (car self-refs) 0))
                                  (and (caddr refs)
                                       (= total-refs 1)))
                              (set-car! where
                                (beta-subst (car where)
                                            (list (cons var val))))
                              (keep (+ i 1)))
                             (else
                              (cons var (keep (+ i 1))))))))
                 (simp! (cddr exp) new-env)
                 (for-each! (lambda (x) (simp! (cdar x) new-env)) (cadr exp))
                 (let ((to-keep (keep 0)))
                   (if (< (length to-keep) (length (cadar where)))
                     (begin
                       (if (null? to-keep)
                         (set-car! where (caddar where))
                         (set-car! (cdar where)
                           (map (lambda (v) (assq v (cadar where))) to-keep)))
                       (s! where))
                     (if (null? to-keep)
                       (set-car! where (caddar where)))))))

              ((eq? (car exp) 'lambda)
               (simp! (cddr exp) (cons exp env)))

              (else
               (for-each! s! exp)
               (cond ((symbol? (car exp)) ; is the operator position a var ref?
                      (let ((frame (binding-frame (car exp) env)))
                        (if frame ; is it a bound variable?
                          (let ((proc (bound-expr (car exp) frame)))
                            (if (and (pair? proc)
                                     (eq? (car proc) 'lambda)
                                     (some? const-expr? (cdr exp)))
                              (let* ((args (arg-pattern (cdr exp)))
                                     (new-proc (peval proc args))
                                     (new-args (remove-constant (cdr exp) args)))
                                (set-car! where
                                  (cons (add-binding new-proc frame (car exp))
                                        new-args)))))
                          (set-car! where
                            (constant-fold-global (car exp) (cdr exp))))))
                     ((not (pair? (car exp))))
                     ((eq? (caar exp) 'lambda)
                      (set-car! where
                        (list 'let
                              (map2 list (cadar exp) (cdr exp))
                              (caddar exp)))
                      (s! where)))))))

    (s! where))

  (define (remove-empty-calls! where env)

    (define (rec! where)
      (let ((exp (car where)))

        (cond ((const-expr? exp))
              ((symbol? exp))
              ((eq? (car exp) 'if)
               (rec! (cdr exp))
               (rec! (cddr exp))
               (rec! (cdddr exp)))
              ((eq? (car exp) 'begin)
               (for-each! rec! (cdr exp)))
              ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
               (let ((new-env (cons exp env)))
                 (remove-empty-calls! (cddr exp) new-env)
                 (for-each! (lambda (x) (remove-empty-calls! (cdar x) new-env))
                            (cadr exp))))
              ((eq? (car exp) 'lambda)
               (rec! (cddr exp)))
              (else
               (for-each! rec! (cdr exp))
               (if (and (null? (cdr exp)) (symbol? (car exp)))
                 (let ((frame (binding-frame (car exp) env)))
                   (if frame ; is it a bound variable?
                     (let ((proc (bound-expr (car exp) frame)))
                       (if (and (pair? proc)
                                (eq? (car proc) 'lambda))
                         (begin
                           (set! changed? #t)
                           (set-car! where (caddr proc))))))))))))

    (rec! where))

  (define changed? #f)

  (let ((x (list exp)))
    (let loop ()
      (set! changed? #f)
      (simp! x '())
      (remove-empty-calls! x '())
      (if changed? (loop) (car x)))))

(define (ref-count exp var) ; compute how many references to variable 'var'
  (let ((total 0)           ; are contained in 'exp'
        (oper 0)
        (always-evaled #t))
    (define (rc exp ae)
      (cond ((const-expr? exp))
            ((symbol? exp)
             (if (eq? exp var)
               (begin
                 (set! total (+ total 1))
                 (set! always-evaled (and ae always-evaled)))))
            ((eq? (car exp) 'if)
             (rc (cadr exp) ae)
             (for-each (lambda (x) (rc x #f)) (cddr exp)))
            ((eq? (car exp) 'begin)
             (for-each (lambda (x) (rc x ae)) (cdr exp)))
            ((or (eq? (car exp) 'let) (eq? (car exp) 'letrec))
             (for-each (lambda (x) (rc (cadr x) ae)) (cadr exp))
             (rc (caddr exp) ae))
            ((eq? (car exp) 'lambda)
             (rc (caddr exp) #f))
            (else
             (for-each (lambda (x) (rc x ae)) exp)
             (if (symbol? (car exp))
               (if (eq? (car exp) var) (set! oper (+ oper 1)))))))
    (rc exp #t)
    (list total oper always-evaled)))

(define (binding-frame var env)
  (cond ((null? env) #f)
        ((or (eq? (caar env) 'let) (eq? (caar env) 'letrec))
         (if (assq var (cadar env)) (car env) (binding-frame var (cdr env))))
        ((eq? (caar env) 'lambda)
         (if (memq var (cadar env)) (car env) (binding-frame var (cdr env))))
        (else
         (fatal-error "ill-formed environment"))))

(define (bound-expr var frame)
  (cond ((or (eq? (car frame) 'let) (eq? (car frame) 'letrec))
         (cadr (assq var (cadr frame))))
        ((eq? (car frame) 'lambda)
         not-constant)
        (else
         (fatal-error "ill-formed frame"))))

(define (add-binding val frame name)
  (define (find-val val bindings)
    (cond ((null? bindings) #f)
          ((equal? val (cadar bindings)) ; *kludge* equal? is not exactly what
           (caar bindings))              ; we want...
          (else
           (find-val val (cdr bindings)))))
  (or (find-val val (cadr frame))
      (let ((var (new-variable name)))
        (set-cdr! (get-last-pair (cadr frame)) (list (list var val)))
        var)))

(define (for-each! proc! l) ; call proc! on each CONS CELL in the list 'l'
  (if (not (null? l))
    (begin (proc! l) (for-each! proc! (cdr l)))))

(define (arg-pattern exps) ; return the argument pattern (i.e. the list of
  (if (null? exps)         ; constants in 'exps' but with the not-constant
    '()                    ; value wherever the corresponding expression in
    (cons (if (const-expr? (car exps)) ; 'exps' is not a constant)
            (const-value (car exps))
            not-constant)
          (arg-pattern (cdr exps)))))

;------------------------------------------------------------------------------
;
; Knowledge about primitive procedures.

(define *primitives*
  (list
    (cons 'car (lambda (args)
                 (and (= (length args) 1)
                      (pair? (car args))
                      (quot (car (car args))))))
    (cons 'cdr (lambda (args)
                 (and (= (length args) 1)
                      (pair? (car args))
                      (quot (cdr (car args))))))
    (cons '+ (lambda (args)
               (and (every? number? args)
                    (quot (peval-sum args 0)))))
    (cons '* (lambda (args)
               (and (every? number? args)
                    (quot (product args 1)))))
    (cons '- (lambda (args)
               (and (> (length args) 0)
                    (every? number? args)
                    (quot (if (null? (cdr args))
                            (- (car args))
                            (- (car args) (peval-sum (cdr args) 0)))))))
    (cons '/ (lambda (args)
               (and (> (length args) 1)
                    (every? number? args)
                    (quot (if (null? (cdr args))
                            (/ (car args))
                            (/ (car args) (product (cdr args) 1)))))))
    (cons '< (lambda (args)
               (and (= (length args) 2)
                    (every? number? args)
                    (quot (< (car args) (cadr args))))))
    (cons '= (lambda (args)
               (and (= (length args) 2)
                    (every? number? args)
                    (quot (= (car args) (cadr args))))))
    (cons '> (lambda (args)
               (and (= (length args) 2)
                    (every? number? args)
                    (quot (> (car args) (cadr args))))))
    (cons 'eq? (lambda (args)
                 (and (= (length args) 2)
                      (quot (eq? (car args) (cadr args))))))
    (cons 'not (lambda (args)
                 (and (= (length args) 1)
                      (quot (not (car args))))))
    (cons 'null? (lambda (args)
                   (and (= (length args) 1)
                        (quot (null? (car args))))))
    (cons 'pair? (lambda (args)
                   (and (= (length args) 1)
                        (quot (pair? (car args))))))
    (cons 'symbol? (lambda (args)
                     (and (= (length args) 1)
                          (quot (symbol? (car args))))))
  )
)

(define (peval-sum lst n)
  (if (null? lst)
    n
    (peval-sum (cdr lst) (+ n (car lst)))))

(define (product lst n)
  (if (null? lst)
    n
    (product (cdr lst) (* n (car lst)))))

(define (reduce-global name args)
  (let ((x (assq name *primitives*)))
    (and x ((cdr x) args))))

(define (constant-fold-global name exprs)

  (define (flatten args op)
    (cond ((null? args)
           '())
          ((and (pair? (car args)) (eq? (caar args) op))
           (append (flatten (cdar args) op) (flatten (cdr args) op)))
          (else
           (cons (car args) (flatten (cdr args) op)))))

  (let ((args (if (or (eq? name '+) (eq? name '*)) ; associative ops
                (flatten exprs name)
                exprs)))
    (or (and (every? const-expr? args)
             (reduce-global name (map const-value args)))
        (let ((pattern (arg-pattern args)))
          (let ((non-const (remove-constant args pattern))
                (const (map const-value (extract-constant args pattern))))
            (cond ((eq? name '+) ; + is commutative
                   (let ((x (reduce-global '+ const)))
                     (if x
                       (let ((y (const-value x)))
                         (cons '+
                               (if (= y 0) non-const (cons x non-const))))
                       (cons name args))))
                  ((eq? name '*) ; * is commutative
                   (let ((x (reduce-global '* const)))
                     (if x
                       (let ((y (const-value x)))
                         (cons '*
                               (if (= y 1) non-const (cons x non-const))))
                       (cons name args))))
                  ((eq? name 'cons)
                   (cond ((and (const-expr? (cadr args))
                               (null? (const-value (cadr args))))
                          (list 'list (car args)))
                         ((and (pair? (cadr args))
                               (eq? (car (cadr args)) 'list))
                          (cons 'list (cons (car args) (cdr (cadr args)))))
                         (else
                          (cons name args))))
                  (else
                   (cons name args))))))))

;------------------------------------------------------------------------------
;
; Examples:

(define (try-peval proc args)
  (peval-partial-evaluate proc args))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example1
  '(lambda (a b c)
     (if (null? a) b (+ (car a) c))))

;(try-peval example1 (list '(10 11) not-constant '1))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example2
  '(lambda (x y)
     (let ((q (lambda (a b) (if (< a 0) b (- 10 b)))))
       (if (< x 0) (q (- y) (- x)) (q y x)))))

;(try-peval example2 (list not-constant '1))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example3
  '(lambda (l n)
     (letrec ((add-list
               (lambda (l n)
                 (if (null? l)
                   '()
                   (cons (+ (car l) n) (add-list (cdr l) n))))))
       (add-list l n))))

;(try-peval example3 (list not-constant '1))

;(try-peval example3 (list '(1 2 3) not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example4
  '(lambda (exp env)
     (letrec ((eval
               (lambda (exp env)
                 (letrec ((eval-list
                            (lambda (l env)
                              (if (null? l)
                                '()
                                (cons (eval (car l) env)
                                      (eval-list (cdr l) env))))))
                   (if (symbol? exp) (lookup exp env)
                     (if (not (pair? exp)) exp
                       (if (eq? (car exp) 'quote) (car (cdr exp))
                         (apply (eval (car exp) env)
                                (eval-list (cdr exp) env)))))))))
       (eval exp env))))

;(try-peval example4 (list 'x not-constant))

;(try-peval example4 (list '(f 1 2 3) not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example5
  '(lambda (a b)
     (letrec ((funct
               (lambda (x)
                 (+ x b (if (< x 1) 0 (funct (- x 1)))))))
       (funct a))))

;(try-peval example5 (list '5 not-constant))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example6
  '(lambda ()
     (letrec ((fib
               (lambda (x)
                 (if (< x 2) x (+ (fib (- x 1)) (fib (- x 2)))))))
       (fib 10))))

;(try-peval example6 '())

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example7
  '(lambda (input)
     (letrec ((copy (lambda (in)
                      (if (pair? in)
                        (cons (copy (car in))
                              (copy (cdr in)))
                        in))))
       (copy input))))

;(try-peval example7 (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define example8
  '(lambda (input)
     (letrec ((reverse (lambda (in result)
                         (if (pair? in)
                           (reverse (cdr in) (cons (car in) result))
                           result))))
       (reverse input '()))))

;(try-peval example8 (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

(define (peval-test)
  (set! *current-num* 0)
  (list (try-peval example1 (list '(10 11) not-constant '1))
        (try-peval example2 (list not-constant '1))
        (try-peval example3 (list not-constant '1))
        (try-peval example3 (list '(1 2 3) not-constant))
        (try-peval example4 (list 'x not-constant))
        (try-peval example4 (list '(f 1 2 3) not-constant))
        (try-peval example5 (list '5 not-constant))
        (try-peval example6 '())
        (try-peval
         example7
         (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))
        (try-peval
         example8
         (list '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))))

(define (main-peval . args)
  (run-single-benchmark
    "peval"
    peval-iters
    (lambda (result)
      (and (list? result)
           (= (length result) 10)
           (equal? (list-ref result 9)
                   '(lambda ()
                      (list 'z 'y 'x 'w 'v 'u 't 's 'r 'q 'p 'o 'n
                            'm 'l 'k 'j 'i 'h 'g 'f 'e 'd 'c 'b 'a)))))
    (lambda () (lambda () (peval-test)))))


;;; PNPOLY - Test if a point is contained in a 2D polygon.

(define (pt-in-poly2 xp yp x y)
  (let loop ((c #f) (i (- (FLOATvector-length xp) 1)) (j 0))
    (if (< i 0)
      c
      (if (or (and (or (FLOAT> (FLOATvector-ref yp i) y)
                       (FLOAT>= y (FLOATvector-ref yp j)))
                   (or (FLOAT> (FLOATvector-ref yp j) y)
                       (FLOAT>= y (FLOATvector-ref yp i))))
              (FLOAT>= x
                       (FLOAT+ (FLOATvector-ref xp i)
                               (FLOAT/ (FLOAT*
                                        (FLOAT- (FLOATvector-ref xp j)
                                                (FLOATvector-ref xp i))
                                        (FLOAT- y (FLOATvector-ref yp i)))
                                       (FLOAT- (FLOATvector-ref yp j)
                                               (FLOATvector-ref yp i))))))
        (loop c (- i 1) i)
        (loop (not c) (- i 1) i)))))

(define (pnpoly-run)
  (let ((count 0)
        (xp (FLOATvector-const 0. 1. 1. 0. 0. 1. -.5 -1. -1. -2. -2.5
                               -2. -1.5 -.5 1. 1. 0. -.5 -1. -.5))
        (yp (FLOATvector-const 0. 0. 1. 1. 2. 3. 2. 3. 0. -.5 -1.
                               -1.5 -2. -2. -1.5 -1. -.5 -1. -1. -.5)))
    (if (pt-in-poly2 xp yp .5 .5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 1.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 1.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .75 2.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp 0. 2.01) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 2.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -1. -.5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -1.5 .5) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -2.25 -1.) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 -.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp .5 -1.25) (set! count (+ count 1)))
    (if (pt-in-poly2 xp yp -.5 -2.5) (set! count (+ count 1)))
    count))

(define (main-pnpoly . args)
  (run-single-benchmark
    "pnpoly"
    pnpoly-iters
    (lambda (result)
      (and (number? result) (= result 6)))
    (lambda () (lambda () (pnpoly-run)))))


;;; PRIMES -- Compute primes less than 100, written by Eric Mohr.

(define  (interval-list m n)
  (if (> m n)
    '()
    (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ((remove-multiples
            (lambda (n l)
              (if (null? l)
                '()
                (if (= (modulo (car l) n) 0)
                  (remove-multiples n (cdr l))
                  (cons (car l)
                        (remove-multiples n (cdr l))))))))
    (if (null? l)
      '()
      (cons (car l)
            (sieve (remove-multiples (car l) (cdr l)))))))

(define (primes<= n)
  (sieve (interval-list 2 n)))

(define (main-primes)
  (run-single-benchmark
   "primes"
   primes-iters
   (lambda (result)
     (equal? result
             '(2 3 5 7 11 13 17 19 23 29 31 37 41
                 43 47 53 59 61 67 71 73 79 83 89 97)))
   (lambda (n) (lambda () (primes<= n)))
   100))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; File:         sboyer.sch
; Description:  The Boyer benchmark
; Author:       Bob Boyer
; Created:      5-Apr-85
; Modified:     10-Apr-85 14:52:20 (Bob Shaw)
;               22-Jul-87 (Will Clinger)
;               2-Jul-88 (Will Clinger -- distinguished #f and the empty list)
;               13-Feb-97 (Will Clinger -- fixed bugs in unifier and rules,
;                          rewrote to eliminate property lists, and added
;                          a scaling parameter suggested by Bob Boyer)
;               19-Mar-99 (Will Clinger -- cleaned up comments)
; Language:     Scheme
; Status:       Public Domain
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; SBOYER -- Logic programming benchmark, originally written by Bob Boyer.
;;; Much less CONS-intensive than NBOYER because it uses Henry Baker's
;;; "sharing cons".

; Note:  The version of this benchmark that appears in Dick Gabriel's book
; contained several bugs that are corrected here.  These bugs are discussed
; by Henry Baker, "The Boyer Benchmark Meets Linear Logic", ACM SIGPLAN Lisp
; Pointers 6(4), October-December 1993, pages 3-10.  The fixed bugs are:
;
;    The benchmark now returns a boolean result.
;    FALSEP and TRUEP use TERM-MEMBER? rather than MEMV (which is called MEMBER
;         in Common Lisp)
;    ONE-WAY-UNIFY1 now treats numbers correctly
;    ONE-WAY-UNIFY1-LST now treats empty lists correctly
;    Rule 19 has been corrected (this rule was not touched by the original
;         benchmark, but is used by this version)
;    Rules 84 and 101 have been corrected (but these rules are never touched
;         by the benchmark)
;
; According to Baker, these bug fixes make the benchmark 10-25% slower.
; Please do not compare the timings from this benchmark against those of
; the original benchmark.
;
; This version of the benchmark also prints the number of rewrites as a sanity
; check, because it is too easy for a buggy version to return the correct
; boolean result.  The correct number of rewrites is
;
;     n      rewrites       peak live storage (approximate, in bytes)
;     0         95024
;     1        591777
;     2       1813975
;     3       5375678
;     4      16445406
;     5      51507739

; Sboyer is a 2-phase benchmark.
; The first phase attaches lemmas to symbols.  This phase is not timed,
; but it accounts for very little of the runtime anyway.
; The second phase creates the test problem, and tests to see
; whether it is implied by the lemmas.

(define (main-sboyer . args)
  (let ((n (if (null? args) 0 (car args))))
    (setup-boyer)
    (run-single-benchmark
     (string-append "sboyer"
                    (number->string n))
     sboyer-iters
     (lambda (rewrites)
       (and (number? rewrites)
            (case n
              ((0)  (= rewrites 95024))
              ((1)  (= rewrites 591777))
              ((2)  (= rewrites 1813975))
              ((3)  (= rewrites 5375678))
              ((4)  (= rewrites 16445406))
              ((5)  (= rewrites 51507739))
              ; If it works for n <= 5, assume it works.
              (else #t))))
     (lambda (alist term n) (lambda () (test-boyer alist term n)))
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
                     (implies x w)))
     n)))

(define (setup-boyer) #t) ; assigned below
(define (test-boyer) #t)  ; assigned below

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The first phase.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; In the original benchmark, it stored a list of lemmas on the
; property lists of symbols.
; In the new benchmark, it maintains an association list of
; symbols and symbol-records, and stores the list of lemmas
; within the symbol-records.

(let ()

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
                                (f))))
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
             (equal (greatereqp x y)
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
                        (equal (t) z)
                        (equal (f) z)))
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

  (define (add-lemma-lst lst)
    (cond ((null? lst)
           #t)
          (else (add-lemma (car lst))
                (add-lemma-lst (cdr lst)))))

  (define (add-lemma term)
    (cond ((and (pair? term)
                (eq? (car term)
                     (quote equal))
                (pair? (cadr term)))
           (put (car (cadr term))
                (quote lemmas)
                (cons
                 (translate-term term)
                 (get (car (cadr term)) (quote lemmas)))))
          (else (fatal-error "ADD-LEMMA did not like term:  " term))))

  ; Translates a term by replacing its constructor symbols by symbol-records.

  (define (translate-term term)
    (cond ((not (pair? term))
           term)
          (else (cons (symbol->symbol-record (car term))
                      (translate-args (cdr term))))))

  (define (translate-args lst)
    (cond ((null? lst)
           '())
          (else (cons (translate-term (car lst))
                      (translate-args (cdr lst))))))

  ; For debugging only, so the use of MAP does not change
  ; the first-order character of the benchmark.

  (define (untranslate-term term)
    (cond ((not (pair? term))
           term)
          (else (cons (get-name (car term))
                      (map untranslate-term (cdr term))))))

  ; A symbol-record is represented as a vector with two fields:
  ; the symbol (for debugging) and
  ; the list of lemmas associated with the symbol.

  (define (put sym property value)
    (put-lemmas! (symbol->symbol-record sym) value))

  (define (get sym property)
    (get-lemmas (symbol->symbol-record sym)))

  (define (symbol->symbol-record sym)
    (let ((x (assq sym *symbol-records-alist*)))
      (if x
          (cdr x)
          (let ((r (make-symbol-record sym)))
            (set! *symbol-records-alist*
                  (cons (cons sym r)
                        *symbol-records-alist*))
            r))))

  ; Association list of symbols and symbol-records.

  (define *symbol-records-alist* '())

  ; A symbol-record is represented as a vector with two fields:
  ; the symbol (for debugging) and
  ; the list of lemmas associated with the symbol.

  (define (make-symbol-record sym)
    (vector sym '()))

  (define (put-lemmas! symbol-record lemmas)
    (vector-set! symbol-record 1 lemmas))

  (define (get-lemmas symbol-record)
    (vector-ref symbol-record 1))

  (define (get-name symbol-record)
    (vector-ref symbol-record 0))

  (define (symbol-record-equal? r1 r2)
    (eq? r1 r2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;
  ; The second phase.
  ;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (test alist term n)
    (let ((term
           (apply-subst
            (translate-alist alist)
            (translate-term
             (do ((term term (list 'or term '(f)))
                  (n n (- n 1)))
                 ((zero? n) term))))))
    (tautp term)))

  (define (translate-alist alist)
    (cond ((null? alist)
           '())
          (else (cons (cons (caar alist)
                            (translate-term (cdar alist)))
                      (translate-alist (cdr alist))))))

  (define (apply-subst alist term)
    (cond ((not (pair? term))
           (let ((temp-temp (assq term alist)))
             (if temp-temp
                 (cdr temp-temp)
                 term)))
          (else (cons (car term)
                      (apply-subst-lst alist (cdr term))))))

  (define (apply-subst-lst alist lst)
    (cond ((null? lst)
           '())
          (else (cons (apply-subst alist (car lst))
                      (apply-subst-lst alist (cdr lst))))))

  (define (tautp x)
    (tautologyp (rewrite x)
                '() '()))

  (define (tautologyp x true-lst false-lst)
    (cond ((truep x true-lst)
           #t)
          ((falsep x false-lst)
           #f)
          ((not (pair? x))
           #f)
          ((eq? (car x) if-constructor)
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

  (define if-constructor '*) ; becomes (symbol->symbol-record 'if)

  (define rewrite-count 0) ; sanity check

  ; The next procedure is Henry Baker's sharing CONS, which avoids
  ; allocation if the result is already in hand.
  ; The REWRITE and REWRITE-ARGS procedures have been modified to
  ; use SCONS instead of CONS.

  (define (scons x y original)
    (if (and (eq? x (car original))
             (eq? y (cdr original)))
        original
        (cons x y)))

  (define (rewrite term)
    (set! rewrite-count (+ rewrite-count 1))
    (cond ((not (pair? term))
           term)
          (else (rewrite-with-lemmas (scons (car term)
                                            (rewrite-args (cdr term))
                                            term)
                                     (get-lemmas (car term))))))

  (define (rewrite-args lst)
    (cond ((null? lst)
           '())
          (else (scons (rewrite (car lst))
                       (rewrite-args (cdr lst))
                       lst))))

  (define (rewrite-with-lemmas term lst)
    (cond ((null? lst)
           term)
          ((one-way-unify term (cadr (car lst)))
           (rewrite (apply-subst unify-subst (caddr (car lst)))))
          (else (rewrite-with-lemmas term (cdr lst)))))

  (define unify-subst '*)

  (define (one-way-unify term1 term2)
    (begin (set! unify-subst '())
           (one-way-unify1 term1 term2)))

  (define (one-way-unify1 term1 term2)
    (cond ((not (pair? term2))
           (let ((temp-temp (assq term2 unify-subst)))
             (cond (temp-temp
                    (term-equal? term1 (cdr temp-temp)))
                   ((number? term2)          ; This bug fix makes
                    (equal? term1 term2))    ; nboyer 10-25% slower!
                   (else
                    (set! unify-subst (cons (cons term2 term1)
                                            unify-subst))
                    #t))))
          ((not (pair? term1))
           #f)
          ((eq? (car term1)
                (car term2))
           (one-way-unify1-lst (cdr term1)
                               (cdr term2)))
          (else #f)))

  (define (one-way-unify1-lst lst1 lst2)
    (cond ((null? lst1)
           (null? lst2))
          ((null? lst2)
           #f)
          ((one-way-unify1 (car lst1)
                           (car lst2))
           (one-way-unify1-lst (cdr lst1)
                               (cdr lst2)))
          (else #f)))

  (define (falsep x lst)
    (or (term-equal? x false-term)
        (term-member? x lst)))

  (define (truep x lst)
    (or (term-equal? x true-term)
        (term-member? x lst)))

  (define false-term '*)  ; becomes (translate-term '(f))
  (define true-term '*)   ; becomes (translate-term '(t))

  ; The next two procedures were in the original benchmark
  ; but were never used.

  (define (trans-of-implies n)
    (translate-term
     (list (quote implies)
           (trans-of-implies1 n)
           (list (quote implies)
                 0 n))))

  (define (trans-of-implies1 n)
    (cond ((equal? n 1)
           (list (quote implies)
                 0 1))
          (else (list (quote and)
                      (list (quote implies)
                            (- n 1)
                            n)
                      (trans-of-implies1 (- n 1))))))

  ; Translated terms can be circular structures, which can't be
  ; compared using Scheme's equal? and member procedures, so we
  ; use these instead.

  (define (term-equal? x y)
    (cond ((pair? x)
           (and (pair? y)
                (symbol-record-equal? (car x) (car y))
                (term-args-equal? (cdr x) (cdr y))))
          (else (equal? x y))))

  (define (term-args-equal? lst1 lst2)
    (cond ((null? lst1)
           (null? lst2))
          ((null? lst2)
           #f)
          ((term-equal? (car lst1) (car lst2))
           (term-args-equal? (cdr lst1) (cdr lst2)))
          (else #f)))

  (define (term-member? x lst)
    (cond ((null? lst)
           #f)
          ((term-equal? x (car lst))
           #t)
          (else (term-member? x (cdr lst)))))

  (set! setup-boyer
        (lambda ()
          (set! *symbol-records-alist* '())
          (set! if-constructor (symbol->symbol-record 'if))
          (set! false-term (translate-term '(f)))
          (set! true-term  (translate-term '(t)))
          (setup)))

  (set! test-boyer
        (lambda (alist term n)
          (set! rewrite-count 0)
          (let ((answer (test alist term n)))
;            (write rewrite-count)
;            (display " rewrites")
;            (newline)
            (if answer
                rewrite-count
                #f)))))


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

(define (simplex-test)
 (simplex (vector (FLOATvector 0.0 1.0 1.0 3.0 -0.5)
                  (FLOATvector 740.0 -1.0 0.0 -2.0 0.0)
                  (FLOATvector 0.0 0.0 -2.0 0.0 7.0)
                  (FLOATvector 0.5 0.0 -1.0 1.0 -2.0)
                  (FLOATvector 9.0 -1.0 -1.0 -1.0 -1.0)
                  (FLOATvector 0.0 0.0 0.0 0.0 0.0))
          2 1 1))

(define (main-simplex . args)
  (run-single-benchmark
    "simplex"
    simplex-iters
    (lambda (result) (equal? result '(#(4 1 3 2) #(0 5 7 6))))
    (lambda () (lambda () (simplex-test)))))


;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

(define (sumfp-run n)
  (let loop ((i n) (sum 0.))
    (if (FLOAT< i 0.)
      sum
      (loop (FLOAT- i 1.) (FLOAT+ i sum)))))

(define (main-sumfp . args)
  (run-single-benchmark
    "sumfp"
    sumfp-iters
    (lambda (result) (equal? result 50005000.))
    (lambda (n) (lambda () (sumfp-run n)))
    10000.))


;;; SUMLOOP -- One of the Kernighan and Van Wyk benchmarks.

(define lsum 0)

(define (tail-rec-aux i n)
  (if (< i n)
      (begin (set! lsum (+ lsum 1)) (tail-rec-aux (+ i 1) n))
      lsum))

(define (tail-rec-loop n)
  (set! lsum 0)
  (tail-rec-aux 0 n)
  lsum)

(define (do-loop n)
  (set! lsum 0)
  (do ((i 0 (+ i 1)))
      ((>= i n) lsum)
    (set! lsum (+ lsum 1))))

(define (main-sumloop . args)
  (run-single-benchmark
   "sumloop"
   sumloop-iters
   (lambda (result) (equal? result 100000000))
   (lambda (n) (lambda () (do-loop n)))
   100000000))


;;; SUM -- Compute sum of integers from 0 to 10000

(define (sum-run n)
  (let loop ((i n) (sum 0))
    (if (< i 0)
      sum
      (loop (- i 1) (+ i sum)))))

(define (main-sum . args)
  (run-single-benchmark
    "sum"
    sum-iters
    (lambda (result) (equal? result 50005000))
    (lambda (n) (lambda () (sum-run n)))
    10000))


;;; TAKL -- The TAKeuchi function using lists as counters.

(define (listn n)
  (if (= n 0)
    '()
    (cons n (listn (- n 1)))))

(define l18 (listn 18))
(define l12 (listn 12))
(define  l6 (listn 6))

(define (mas x y z)
  (if (not (shorterp y x))
      z
      (mas (mas (cdr x) y z)
           (mas (cdr y) z x)
           (mas (cdr z) x y))))

(define (shorterp x y)
  (and (not (null? y))
       (or (null? x)
           (shorterp (cdr x)
                     (cdr y)))))

(define (main-takl . args)
  (run-single-benchmark
    "takl"
    takl-iters
    (lambda (result) (equal? result '(7 6 5 4 3 2 1)))
    (lambda (x y z) (lambda () (mas x y z)))
    l18
    l12
    l6))


;;; TAK -- A vanilla version of the TAKeuchi function.

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (main-tak . args)
  (run-single-benchmark
    "tak"
    tak-iters
    (lambda (result) (equal? result 7))
    (lambda (x y z) (lambda () (tak x y z)))
    18
    12
    6))


;;; TRIANGL -- Board game benchmark.

(define *board*
  (list->vector '(1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1)))

(define *sequence*
  (list->vector '(0 0 0 0 0 0 0 0 0 0 0 0 0 0)))

(define *a*
  (list->vector '(1 2 4 3 5 6 1 3 6 2 5 4 11 12
                  13 7 8 4 4 7 11 8 12 13 6 10
                  15 9 14 13 13 14 15 9 10
                  6 6)))

(define *b*
  (list->vector '(2 4 7 5 8 9 3 6 10 5 9 8
                  12 13 14 8 9 5 2 4 7 5 8
                  9 3 6 10 5 9 8 12 13 14
                  8 9 5 5)))

(define *c*
  (list->vector '(4 7 11 8 12 13 6 10 15 9 14 13
                  13 14 15 9 10 6 1 2 4 3 5 6 1
                  3 6 2 5 4 11 12 13 7 8 4 4)))

(define *answer* '())

(define (attempt i depth)
  (cond ((= depth 14)
         (set! *answer*
               (cons (cdr (vector->list *sequence*)) *answer*))
         #t)
        ((and (= 1 (vector-ref *board* (vector-ref *a* i)))
              (= 1 (vector-ref *board* (vector-ref *b* i)))
              (= 0 (vector-ref *board* (vector-ref *c* i))))
         (vector-set! *board* (vector-ref *a* i) 0)
         (vector-set! *board* (vector-ref *b* i) 0)
         (vector-set! *board* (vector-ref *c* i) 1)
         (vector-set! *sequence* depth i)
         (do ((j 0 (+ j 1))
              (depth (+ depth 1)))
             ((or (= j 36) (attempt j depth)) #f))
         (vector-set! *board* (vector-ref *a* i) 1)
         (vector-set! *board* (vector-ref *b* i) 1)
         (vector-set! *board* (vector-ref *c* i) 0) #f)
        (else #f)))

(define (triangl-test i depth)
  (set! *answer* '())
  (attempt i depth)
  (car *answer*))

(define (main-triangl . args)
  (run-single-benchmark
    "triangl"
    triangl-iters
    (lambda (result) (equal? result '(22 34 31 15 7 1 20 17 25 6 5 13 32)))
    (lambda (i depth) (lambda () (triangl-test i depth)))
    22
    1))

(define (run-all)
  (main-ack)
  (main-array1)
  (main-boyer)
  (main-browse)
  (main-compiler)
  (main-conform)
  (main-cpstak)
  (main-dderiv)
  (main-deriv)
  (main-destruc)
  (main-diviter)
  (main-divrec)
  (main-earley)
  (main-fftrad4)
  (main-fft)
  (main-fibfp)
  (main-fib)
  (main-graphs)
  (main-lattice)
  (main-mazefun)
  (main-mbrot)
  (main-nbody)
  (main-nboyer)
  (main-nqueens)
  (main-nucleic)
  (main-paraffins)
  (main-perm9)
  (main-peval)
  (main-pnpoly)
  (main-primes)
  (main-sboyer)
  (main-simplex)
  (main-sumfp)
  (main-sumloop)
  (main-sum)
  (main-takl)
  (main-tak)
  (main-triangl)
  #t)

(define (main)
  (run-benchmark
    "all"
    1
    (lambda (result) (equal? result #t))
    (lambda () (lambda () (run-all)))))
