;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err
;; Test bind define
;; Test appel terminal (+ ajouter if then else comme position terminale, ... ?)
;; Test eq? eqv? equal?
;; Test list-ref
;; Test map
;; Test reverse
;; Test for-each
;; Test ERROR
;; Ajouter port au GC
;; Test open-input-file, read-char, eof-object ?
;; Test print symbol, print eof
;; Test odd even negative positive zero?
;; Test memq, member, remaider!!!
;; +++ Check-err de tout ça
;; PB Registre RCX. Pour le moment, remplacé partout par R15 (A MODIFIER)
;; ATTENTION A LA COPIE DES CTX. A revoir dans patch-closure (et voir les set car set cdr)
;;     + ajouter des fonctions de copie etc de ctx
;;     ctx-stack-set!
;;     avec segfault ctx id
;; Revoir test rest avec des cts existants
;; Enlever les deux TODO dans ast.scm au moment de la création des deux nouveaux contextes

;; Todo : pb quand fonction à un seul arg qui est le rest, et aucun donné
;; Todo : ajouter print port quand input-port? et output-port? sont implémentées

; (define (rch port)
;   (let ((r (read-char port)))
;     (println (eof-object? r))
;     (pp (char? r))
;     (pp r)))

; (let ((f (open-input-file "in")))
;   (println f)
;   (pp (port? f))
;   (println "-----")
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f))


;=============================================================================










;;; BROWSE -- Benchmark to create and browse through
;;; an AI-like data base of units.

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
            (put name (generate-symbol) #f))
        (put name
             'pattern
             (do ((i npats (- i 1))
                  (ipats ipats (cdr ipats))
                  (a '()))
                 ((zero? i) a)
                 (set! a (cons (car ipats) a))))
        (do ((j (- m i) (- j 1)))
            ((zero? j))
            (put name (generate-symbol) #f)))))

(define (browse-random)
  (set! *rand* (remainder (* *rand* 17) 251))
  *rand*)

(define (randomize l)
  (do ((a '()))
      ((null? l) a)
      (let ((n (remainder (browse-random) (length l))))
        (cond ((zero? n)
               (begin (set! a (cons (car l) a))
               (set! l (cdr l))
               l))
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
          (do ((p (get (car units) 'pattern)
                  (cdr p)))
              ((null? p))
              (my-match (car pats) (car p) '())))))

(browse '((*a ?b *b ?b a *a a *b *a)
      (*a *b *b *a (*a) (*b))
      (? ? * (b a) * ? ?)))

; (define (main . args)
;   (run-benchmark
;     "browse"
;     browse-iters
;     (lambda (result) #t)
;     (lambda (pats) (lambda () (browse pats)))
;     '((*a ?b *b ?b a *a a *b *a)
;       (*a *b *b *a (*a) (*b))
;       (? ? * (b a) * ? ?))))
