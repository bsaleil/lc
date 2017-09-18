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

;-----

(define (main)
  (setup-boyer)
  (pp "K"))

(main)

;95024
;591777
;1813975
;5375678
