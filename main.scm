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

  (--disable-entry-points
    "Disable the use of multiple entry points use only one generic entry point"
    ,(lambda (args) (set! opt-entry-points #f) args))

  (--disable-regalloc-vers
    "Do not use register allocation information to specialize generated code"
    ,(lambda (args) (set! opt-vers-regalloc #f) args))

  (--disable-return-points
      "Disable the use of multiple return points use only one generic return point"
      ,(lambda (args) (set! opt-return-points #f) args))

  (--enable-ccoverflow-fallback
    "Enable automatic fallback to generic entry point when cctable overflows, default throws an error"
    ,(lambda (args) (set! opt-overflow-fallback #t) args))

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

  (--dump-binary
    "Dump machine code block"
    ,(lambda (args) (set! opt-dump-bin #t)
                    args))

  (--max-versions
    "Set a limit on the number of versions of lazy code objects"
    ,(lambda (args) (set! opt-max-versions (string->number (cadr args)))
                    (set! args (cdr args))
                    args))

  (--nolib
     "Do not include the standard library"
     ,(lambda (args) (set! opt-use-lib #f)
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

  (let ((lazy-final
          (make-lazy-code
            (lambda (cgc ctx)

              ;; Update gambit heap ptr from LC heap ptr
              (x86-mov cgc (x86-mem (get-hp-addr)) alloc-ptr)

              ;; TODO regalloc: opt-time
              (let ((loc (ctx-get-loc ctx 0))) ;; Get loc of value in top of stack
                (if (ctx-loc-is-register? loc)
                    (x86-mov cgc (x86-rax) (codegen-reg-to-x86reg loc))
                    (error "NYI regalloc")))

              ;; Set rsp to pstack top (rsp points to the last saved registers)
              (x86-mov cgc (x86-rbx) (x86-imm-int block-addr))
              (x86-mov cgc (x86-rsp) (x86-mem 0 (x86-rbx)))

              ;; Restore registers values from pstack
              (pop-regs-reverse cgc all-regs)
              (x86-ret cgc)))))


    (cond ((null? exprs) succ)
          ((and (= (length exprs) 1)
                (not succ))
             (gen-ast (car exprs) lazy-final))
          (else
            (let ((next (lazy-exprs (cdr exprs) succ)))
              (gen-ast (car exprs)
                       (make-lazy-code
                         (lambda (cgc ctx)
                           (jump-to-version cgc
                                            next
                                            (ctx-pop ctx))))))))))

;;-----------------------------------------------------------------------------
;; Interactive mode (REPL)
(define (repl lib)

  (error "NYI"))

;;-----------------------------------------------------------------------------
;; Bash mode

(define (exec lib prog)

    (define (one-exec)
      (set! from-space init-from-space)
      (set! to-space   init-to-space)
      (##machine-code-block-exec mcb))

  (init)

  (let ((lazy-lib (lazy-exprs (append lib prog) #f)))
    (gen-version-first lazy-lib (ctx-init)))

  (if opt-time
      (begin (##machine-code-block-exec mcb)
             (set! all-lazy-code #f)
             (let loop ((i 0))
               (if (< i (/ ustack-len 8))
                   (begin (vector-set! ustack i 0)
                          (loop (+ i 1)))))
             (##gc)
             (time (##machine-code-block-exec mcb)
                   (current-output-port)))
      (begin (##machine-code-block-exec mcb))))

;;-----------------------------------------------------------------------------
;; Main

;; Run command
(define (run path . args)
  (let* ((port
          (open-process (list path: path
                              arguments: args
                              ;;stderr-redirection: #t
                              )))
         (output
          (read-line port #f))
         (status
          (process-status port)))
    (close-port port)
    (cons status output)))

(define (copy-with-declare src dst)
  (run "./copy-with-declare.sh" src dst))

(define (main . args)

  ;; Set options and get files from cl args
  (define files (parse-args args))

  ;; Get library
  (define lib
    (if opt-use-lib
        (expand-tl (read-all (open-input-file "./lib.scm")))
        '()))

    (cond ;; If no files specified then start REPL
          ((null? files)
            (repl lib))
          ;; Can only exec 1 file
          ((= (length files) 1)
            (copy-with-declare (car files) "./tmp")
            (let ((content (read-all (open-input-file (car files)))));;(c#expand-program "./tmp")))
                (define (get-global-type g)
                  (cond ((symbol? (cadr g))
                            (cond ((symbol?  (caddr g)) CTX_UNK) ;; TODO si globale connue, mettre type
                                  ((integer? (caddr g)) CTX_INT)
                                  ((flonum?  (caddr g)) CTX_FLO)
                                  ((char?    (caddr g)) CTX_CHAR)
                                  ((string?  (caddr g)) CTX_STR)
                                  ((boolean? (caddr g)) CTX_BOOL)
                                  ((eq? (caaddr g) 'lambda) CTX_CLO)
                                  ((pair? (caddr g)) CTX_UNK)
                                  (else (error "NYI"))))
                        ((pair? (cadr g)) CTX_CLO)
                        (else (error "NYI"))))

                ;; TODO
                (define (get-gids lib)
                  (if (null? lib)
                     #t
                     (let ((el (car lib)))
                       (if (and (pair? el)
                                (eq? (car el) 'define))
                          (if (pair? (cadr el))
                              (table-set! gids (caadr el) (get-global-type el))
                              (table-set! gids (cadr el)  (get-global-type el))))
                       (get-gids (cdr lib)))))

                (get-gids lib)
                (get-gids content)

                (let ((exp-content (expand-tl content)))
                   ;(pp exp-content))))
                   (exec lib exp-content))))
          (else (error "NYI")))

    (rt-print-opts)
    (print-opts)
    0)

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
      (print-stats))
  (if opt-dump-bin
      (print-mcb)))

(define (print-mcb)
  (define (print-mcb-h pos lim)
    (if (= pos lim)
        (newline)
        (let ((byte (get-u8 pos)))
          (if (< byte #x10)
              (print "0" (number->string (get-u8 pos) 16) " ")
              (print (number->string (get-u8 pos) 16) " "))
          (print-mcb-h (+ pos 1) lim))))
  (print-mcb-h code-addr code-alloc))

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
