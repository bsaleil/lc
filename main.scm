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

  (--show-locat-versions
    "Pretty print number of versions for each locat object"
    ,(lambda (args) (set! opt-show-locat-versions #t)
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
            #f
            (lambda (cgc ctx)

              ;; Update gambit heap ptr from LC heap ptr
              (x86-mov cgc (x86-mem (get-hp-addr)) alloc-ptr)

              ;; TODO regalloc: opt-time

              (let ((loc (ctx-get-loc ctx 0))) ;; Get loc of value in top of stack
                (cond ;; No return value
                      ((and (not loc)
                            (not (ctx-get-type ctx 0)))
                        (x86-mov cgc (x86-rax) (x86-imm-int 0)))
                      ((not loc)
                        (let* ((type (ctx-get-type ctx 0))
                               (cst  (ctx-type-cst type)))
                          (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding cst)))))
                      ((ctx-loc-is-register? loc)
                        (x86-mov cgc (x86-rax) (codegen-reg-to-x86reg loc)))
                      (else (error "NYI main"))))

              ;; Restore registers values from pstack
              (ppop-regs-reverse cgc all-regs)
              (x86-ret cgc)))))


    (cond ((null? exprs) succ)
          ((and (= (length exprs) 1)
                (not succ))
             (gen-ast (car exprs) lazy-final))
          (else
            (let ((next (lazy-exprs (cdr exprs) succ)))
              (gen-ast (car exprs)
                       (make-lazy-code
                         #f
                         (lambda (cgc ctx)
                           (jump-to-version cgc
                                            next
                                            (ctx-pop ctx))))))))))

;;-----------------------------------------------------------------------------
;; Interactive mode (REPL)

(define (repl prog)

  (init-backend)

  (println "  _     ____       ")
  (println " | |   / ___|      ")
  (println " | |  | |          ")
  (println " | |__| |___       ")
  (println " |_____\\____| REPL")
  (println "")

  (let ((lco (lazy-exprs prog lazy-repl-call)))
    (gen-version-first lco (ctx-init)))

  (##machine-code-block-exec mcb))

(define lazy-repl-call
  (make-lazy-code
    #f
    (lambda (cgc ctx)
      ;; Generate call to repl handler defined in core.scm
      ;; This handler read from stdin, build lco chain,
      ;; and generate a version of the first lco of the chain.
      ;; The address of this version is returned in rax
      ;; then, jump to the version
      (x86-pcall cgc label-repl-handler)
      (x86-jmp cgc (x86-rax)))))

;;-----------------------------------------------------------------------------
;; Bash mode

(define (exec prog)

    (define (one-exec)
      (set! from-space init-from-space)
      (set! to-space   init-to-space)
      (##machine-code-block-exec mcb))

  (init-backend)

  ;(liveness-prog prog)
  ;(println "--------------- PROG:")
  ;(pp prog)
  ;(println "--------------- TABLE:")
  ;(liveness-prog prog)
  ;(for-each (lambda (el)
  ;            (pp el))
  ;          (table->list liveness-out))
  ;(error "OK")

  (let ((lco (lazy-exprs prog #f)))
    (gen-version-first lco (ctx-init)))

  (if opt-time
      (begin (##machine-code-block-exec mcb)
             (set! lco #f)
             (set! all-lazy-code #f)
             (set! asc-cc-stub #f)
             (set! asc-entry-load #f)
             (set! ctime-entries #f)
             (set! stub-freelist #f)
             (let loop ((i 0))
               (if (< i (/ ustack-len 8))
                   (begin (vector-set! ustack i 0)
                          (loop (+ i 1)))))
             (let loop ((i 0))
               (if (< i globals-len)
                   (begin (vector-set! globals-space i 0)
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
  (run "./copy-with-declare.sh" src dst (if opt-use-lib "lib" "nolib")))

(define (main . args)

  ;; Set options and get files from cl args
  (define files (parse-args args))

  (init-frontend)

  (cond ;; If no files specified then start REPL
        ((null? files)
          (copy-with-declare "" "./tmp")
          (let ((content (c#expand-program "./tmp" #f locat-table)))
            (repl (expand-tl content))))
        ;; Can only exec 1 file
        ((= (length files) 1)
          (copy-with-declare (car files) "./tmp")
        (let ((content (c#expand-program "./tmp" #f locat-table))) ;(read-all (open-input-file (car files)))))

              (let ((exp-content (expand-tl content)))
                (analyses-find-global-types! exp-content)
                ;(pp exp-content))))
                ;(pp content))))
                ;(exec content))))
                (exec exp-content))))
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
       (let* ((lco (car lcs))
              (nb-versions (lazy-code-nb-real-versions lco))
              (r  (table-ref table nb-versions (cons 0 '()))))
        (table-set!
          table nb-versions
          (cons (+ (car r) 1)
                (cons (lazy-code-flags lco) (cdr r))))
        (get-versions-info-full-h (cdr lcs)))))

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
  (if opt-show-locat-versions
      (print-locat-versions))
  (if opt-dump-bin
      (print-mcb)))

(define (print-mcb)
  (define (print-mcb-h pos lim)
    (if (= pos lim)
        (newline)
        (let ((byte (get-u8 pos)))
          (if (< byte #x10)
              (print "\\x0" (number->string (get-u8 pos) 16) "")
              (print "\\x"  (number->string (get-u8 pos) 16) ""))
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
    (println "#versions;#stubs;#ret;#entry;#cont;#cond")
    (let ((versions-info-full (get-versions-info-full all-lazy-code)))
      (for-each (lambda (n)
                  (println (car n) ";"
                           (cadr n) ";"
                           (count (cddr n) (lambda (n) (member 'ret n))) ";"
                           (count (cddr n) (lambda (n) (member 'entry n))) ";"
                           (count (cddr n) (lambda (n) (member 'cont n))) ";"
                           (count (cddr n) (lambda (n) (member 'cond n)))))
                (sort versions-info-full (lambda (n m) (< (car n) (car m)))))
      (println "-------------------------"))))

;;-----------------------------------------------------------------------------
;; Locat infos

(define (print-locat-versions)

  (define restable (make-table))

  (define (show-locat-info locat info)
  (let ((port (current-output-port)))
    (##display-locat locat #t port)
    (println port: port ": " info)))

  (define (restable-add locat lco)
    (let ((key (list locat
                     lco
                     (object->serial-number (lazy-code-ast lco)))))
      (let ((r (table-ref restable key 0)))
        (table-set! restable key (+ r (length (table->list (lazy-code-versions lco))))))))

  ;; For each lco
  (for-each
    (lambda (x)
      (let ((ast (lazy-code-ast x)))
        ;; If an ast is associated to the lco and there is 1 or more versions
        (if (and ast (> (length (table->list (lazy-code-versions x))) 0))
            ;; Then, if a locat object is associated to this ast, add versions number
            (let ((r (table-ref locat-table ast #f)))
              (if r
                  (restable-add r x))))))
    all-lazy-code)

  ;; For each entry (locat) in restable
  (for-each
    (lambda (x)
      ;; Pretty print "locat nb-version"
      (show-locat-info
        (caar x) ;; locat
        (with-output-to-string ""
          (lambda () (print (cdr x) " ")
                     (let ((lco (cadar x)))
                       (pretty-print (lazy-code-ast lco)))))))
    (table->list restable)))
