#! gsi-script
;;---------------------------------------------------------------------------
;;
;;  Copyright (c) 2015, Baptiste Saleil. All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;   3. The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior written
;;      permission.
;;
;;  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
;;  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;---------------------------------------------------------------------------

(include "~~lib/_x86#.scm")
(include "./extern/Sort.scm")

(define pp pretty-print)
(define repl-print-lco #f)

(define-macro (string-bold str)
    `(string-append "\033[1m" ,str "\033[0m"))

;;--------------------------------------------------------------------------------
;; Compiler options

;; Contains all compiler options
;; An option contains (option-text help-text option-lambda)
(define compiler-options `(
  (--all-tests
    "DEPRECATED"
    ,(lambda (args) (set! opt-all-tests #t) args))

  (--cctable-maxsize
    "Set the maximum size of the global entry points table"
    ,(lambda (args) (set! global-cc-table-maxsize (string->number (cadr args))) (cdr args))) ;; TODO: use a variable opt-cctable-maxsize

  (--count-calls
    "DEPRECATED"
    ,(lambda (args) (set! opt-count-calls (string->symbol (cadr args)))
                    (set! args (cdr args)) ;; Remove one more arg
                    args))

  (--disable-ccoverflow-fallback
    "Disable automatic fallback to generic entry point when cctable overflows, throw an error instead"
    ,(lambda (args) (set! opt-overflow-fallback #f) args))

  (--disable-entry-points
    "Disable the use of multiple entry points use only one generic entry point"
    ,(lambda (args) (set! opt-entry-points #f) args))

  (--disable-return-points
      "Disable the use of multiple return points use only one generic return point"
      ,(lambda (args) (set! opt-return-points #f) args))

  (--enable-functionid-propagation
    "Disable the propagation of function identities"
    ,(lambda (args) (set! opt-propagate-functionid #t) args))

  (--heap-max
    "Set maximum heap size in kilobytes"
    ,(lambda (args) (set! space-len (* 1000 (string->number (cadr args)))) (cdr args))) ;; TODO: use a variable opt-space-len

  (--help
    "Print help"
    ,(lambda (args)
      (newline)
      (println (string-bold "NAME"))
      (println "       lazy-comp - Scheme JIT compiler")
      (newline)
      (println (string-bold "SYNOPSIS"))
      (println "       ./lazy-comp [files] [options]")
      (newline)
      (println (string-bold "OPTIONS"))
      (for-each (lambda (option) (println "       " (car option))
                                 (println "       " "       " (cadr option))
                                 (newline))
                compiler-options)
      (newline)
      (exit 0)))

  (--max-versions
    "Set a limit on the number of versions of lazy code objects"
    ,(lambda (args) (set! opt-max-versions (string->number (cadr args)))
                    (set! args (cdr args))
                    args))

  (--stats
    "Print stats about execution"
    ,(lambda (args) (assert (not opt-time) "--stats option can't be used with --time")
                    (set! opt-stats #t)
                    args))

  (--time
    "Print exec time information"
    ,(lambda (args) (assert (not opt-stats) "--time option can't be used with --stats")
                    (assert (not opt-count-calls) "--time option can't be used with --count-calls")
                    (set! opt-time #t)
                    args))

  (--verbose
    "Full verbose"
    ,(lambda (args) (set! opt-verbose-jit #t)
                    (set! opt-verbose-gc  #t)
                    args))

  (--verbose-jit
    "Set jit as verbose"
    ,(lambda (args) (set! opt-verbose-jit #t) args))

  (--verbose-gc
    "Set gc as verbose"
    ,(lambda (args) (set! opt-verbose-gc #t) args))
))

(define (parse-args args)
    (cond ;;
          ((null? args) '())
          ;;
          ((not (eq? (string-ref (car args) 0) #\-))
            (cons (car args) (parse-args (cdr args))))
          ;;
          (else
            (let ((opt (assq (string->symbol (car args)) compiler-options)))
              (if opt
                (let ((fn (caddr opt)))
                 (set! args (fn args)))
                (error "Unknown argument " (car args))))
            (parse-args (cdr args)))))

;;-----------------------------------------------------------------------------

(define (lazy-exprs exprs succ)
  (if (null? exprs)
      (or succ
          (make-lazy-code
            (lambda (cgc ctx)
              ;(x86-pop cgc (x86-rax))

              ;; Set time slot with total time if opt-time is #t
              (if opt-time
                 (begin
                    (x86-mov   cgc (x86-rbx) (x86-imm-int (+ block-addr (* 9 8)))) ;; Get slot addr in rbx
                    (x86-mov cgc (x86-rcx) (x86-mem 0 (x86-rbx))) ;; rcx = before
                    (x86-rdtsc cgc)
                    (x86-shl   cgc (x86-rdx) (x86-imm-int 32))
                    (x86-or    cgc (x86-rax) (x86-rdx))               ;; rax = after
                    (x86-sub cgc (x86-rax) (x86-rcx))
                    (x86-mov cgc (x86-mem 0 (x86-rbx)) (x86-rax))))

              ;; TODO regalloc code before regalloc
              ;(x86-add cgc (x86-rsp) (x86-imm-int (* (- (length (ctx-stack ctx)) 1) 8)))

              ;; TODO regalloc
              ;; 1 - mov top of vritual stack to rax
              (let ((loc (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))) ;; Get loc of value in top of stack
                (if (ctx-loc-is-register? loc)
                    (x86-mov cgc (x86-rax) (codegen-reg-to-x86reg loc))
                    (error "NYI regalloc")))
              ;; 2 - TODO: clean stack

              ;; TODO regalloc debug print result (rax)
              (gen-breakpoint cgc)

              (pop-regs-reverse cgc all-regs)
              (x86-ret cgc))))
      (gen-ast (car exprs)
               (lazy-exprs (cdr exprs) succ))))

;;-----------------------------------------------------------------------------
;; Interactive mode (REPL)
(define (repl lib)

  (println "  _     ____       ")
  (println " | |   / ___|      ")
  (println " | |  | |          ")
  (println " | |__| |___       ")
  (println " |_____\\____| REPL")
  (println "")

  (set! mode-repl #t)

  (init)

  (letrec (;; Lazy return
           (lazy-ret (make-lazy-code
                        (lambda (cgc ctx)
                           (x86-pop cgc (x86-rax))
                           (x86-add cgc (x86-rsp) (x86-imm-int (* (- (length (ctx-stack ctx)) 1) 8)))
                           (pop-regs-reverse cgc all-regs)
                           (x86-ret cgc))))
           ;; Lazy lib end
           (lazy-lib-end
               (make-lazy-code
                   (lambda (cgc ctx)
                     (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (length lib))))
                     (jump-to-version cgc lazy-read-eval (ctx-pop ctx (length lib))))))
           ;; Lazy lib
           (lazy-lib (lazy-exprs lib lazy-lib-end))
           ;; Lazy print
           (lazy-print (make-lazy-code
              (lambda (cgc ctx)
                 (x86-pop cgc (x86-rax))
                 (x86-mov cgc (x86-mem 0 (x86-r10)) (x86-rax))
                 (jump-to-version cgc (gen-ast '(pp $$REPL-RES) lazy-read-eval) (ctx-pop ctx)))))
           ;; Lazy read-eval
           (lazy-read-eval (make-lazy-code
              (lambda (cgc ctx)
                (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
                (x86-mov cgc (x86-rsp) (x86-mem 0 (x86-rax)))
                (gen-repl-call cgc)
                (x86-jmp cgc (x86-rbx))))))
    (set! repl-print-lco lazy-print)
    ;; Global var for print step
    (set! globals '(($$REPL-RES . 0)))
    ;; Gen
    (gen-version code-alloc lazy-lib (make-ctx '() '() -1)))

  ;; Execute mcb
  (time (##machine-code-block-exec mcb)))

;;-----------------------------------------------------------------------------
;; Bash mode

(define (exec lib prog)

    (define (one-exec)
      (set! from-space init-from-space)
      (set! to-space   init-to-space)
      (##machine-code-block-exec mcb))

  (init)

  (let* ((lazy-prog (lazy-exprs prog #f))
         (lazy-lib-end
           (make-lazy-code
             (lambda (cgc ctx)
               ;; TODO regalloc LIB
               (jump-to-version cgc lazy-prog ctx))))
               ;(x86-add cgc (x86-rsp) (x86-imm-int (* 8 (length lib))))
               ;(jump-to-version cgc lazy-prog (ctx-pop ctx (length lib))))))
         (lazy-lib  (lazy-exprs lib  lazy-lib-end)))

     (gen-version code-alloc
                  lazy-lib
                  (ctx-init)))

  (if opt-time
      (begin (##machine-code-block-exec mcb)
             (let ((before #f)
                   (after  #f))
                (set! before (real-time))
                ;; 10 execs
                (one-exec) (one-exec)
                ;(one-exec) (one-exec)
                ;(one-exec) (one-exec)
                ;(one-exec) (one-exec)
                ;(one-exec) (one-exec)
                (set! after (real-time))
                (println "Time: " (* (/ (- after before) 10) 1000))))
      (##machine-code-block-exec mcb)))

;;-----------------------------------------------------------------------------
;; Main

(define (main . args)

  ;; Get library
  (define lib '())
  ;(define lib (expand-tl (read-all (open-input-file "./lib.scm"))))
  ;; Set options and get files from cl args
  (define files (parse-args args))

    (cond ;; If no files specified then start REPL
          ((null? files)
            (repl lib))
          ;; Can only exec 1 file
          ((= (length files) 1)
            (let ((content (read-all (open-input-file (car args)))))
                ;; TODO
                (define (get-global-type g)
                  (cond ((symbol? (cadr g))
                            (cond ((symbol?  (caddr g)) CTX_UNK) ;; TODO si globale connue, mettre type
                                  ((number?  (caddr g)) CTX_NUM)
                                  ((char?    (caddr g)) CTX_CHAR)
                                  ((string?  (caddr g)) CTX_STR)
                                  ((boolean? (caddr g)) CTX_BOOL)
                                  ((eq? (caaddr g) 'lambda) CTX_CLO)
                                  ((pair? (caddr g)) CTX_UNK)
                                  (else (error "NYI"))))
                        ((pair? (cadr g)) CTX_CLO)
                        (else (error "NYI"))))

                ;; TODO
                (define (get-gids lib base)
                  (if (null? lib)
                     base
                     (let ((el (car lib)))
                       (if (and (pair? el)
                                (eq? (car el) 'define))
                          (if (pair? (cadr el))
                             (cons (cons (caadr el) (get-global-type el)) (get-gids (cdr lib) base))
                             (cons (cons (cadr el)  (get-global-type el)) (get-gids (cdr lib) base)))
                          (get-gids (cdr lib) base)))))

                (set! gids (get-gids lib '()))
                (set! gids (get-gids content gids))

                (let ((exp-content (expand-tl content)))
                   ;(pp exp-content))))
                   (exec lib exp-content))))
          (else (error "NYI")))

    (rt-print-opts)
    (print-opts))

;; TODO
(define (get-versions-info lazy-codes)

  (define (get-versions-info-h lcs min max)
    (if (null? lcs)
       (cons min max)
       (let* ((lc (car lcs))
              (nb (table-length (lazy-code-versions lc))))
         (cond ((< nb min)
                   (get-versions-info-h (cdr lcs) nb max))
               ((> nb max)
                   (get-versions-info-h (cdr lcs) min nb))
               (else
                   (get-versions-info-h (cdr lcs) min max))))))

  (if (null? lazy-codes)
     (cons 0 0)
     (let* ((lc (car lazy-codes))
            (nb (table-length (lazy-code-versions lc))))
       (get-versions-info-h (cdr lazy-codes) nb nb))))

;; TODO
(define (get-versions-info-full lazy-codes)

  (define table (make-table))

  (define (get-versions-info-full-h lcs)
    (if (null? lcs)
       #t
       (let* ((lc (car lcs))
              (nb (table-length (lazy-code-versions lc)))
              (r  (table-ref table nb #f)))
        (if r
           (table-set! table nb (cons (+ (car r) 1)
                                      (cons (lazy-code-flags lc) (cdr r))))
           (table-set! table nb (cons 1
                                      (list (lazy-code-flags lc)))))
        (get-versions-info-full-h (cdr lcs)))))
        ;(table-set! table nb (if r (+ r 1) 1))
        ;(get-versions-info-full-h (cdr lcs)))))

  (get-versions-info-full-h lazy-codes)
  (table->list table))

;;-----------------------------------------------------------------------------

(define (rt-print-opts)
  (if opt-count-calls
    (println "Calls '" opt-count-calls "': " (get-slot 'calls)))
  (if opt-stats
    (begin (println "Closures: " (get-slot 'closures))
           (println "Executed tests: " (get-slot 'tests)))))

(define (print-opts)
  (if opt-stats
     (print-stats)))

(define (print-stats)
  ;; Print stats report
  (let ((code-bytes (- code-alloc code-addr))
        (stub-bytes (- (+ code-addr code-len) stub-alloc)))
    ;; Code size
    (println "Code size (bytes): " code-bytes)
    ;; Stub size
    (println "Stub size (bytes): " stub-bytes)
    ;; Code + Stub size
    (println "Total size (bytes): " (+ code-bytes stub-bytes))
    ;; Global cc-table size
    (println "Global table size: " (table-length global-cc-table))
    ;; Number of cc tables
    (println "Number of cctables: " (table-length cctables))
    ;; Number of cr tables
    (println "Number of crtables: " (table-length crtables))
    ;; CC table space (kb)
    (print "CC table space: ")
    (pp-flonum (/ (* (table-length global-cc-table) 8 (table-length cctables)) 1000) 5)
    ;; CR table space (kb)
    (print "CR table space: ")
    (pp-flonum (/ (* 16 8 (table-length crtables)) 1000) 5)
    ;; Min/Max versions number of stubs
    (let ((versions-info (get-versions-info all-lazy-code)))
      (println "Min versions number: " (car versions-info))
      (println "Max versions number: " (cdr versions-info)))
    ;; Number of stubs, number of return stubs, and number of entry stubs for each number of versions
    (println "-------------------------")
    (println "Number of stubs for each number of version")
    (println "#versions;#stubs;#ret;#entry;#cont")
    (let ((versions-info-full (get-versions-info-full all-lazy-code)))
      (for-each (lambda (n)
                  (println (car n) ";"
                           (cadr n) ";"
                           (count (cddr n) (lambda (n) (member 'ret n))) ";"
                           (count (cddr n) (lambda (n) (member 'entry n))) ";"
                           (count (cddr n) (lambda (n) (member 'cont n)))))
                (sort versions-info-full (lambda (n m) (< (car n) (car m)))))
      (println "-------------------------"))))
