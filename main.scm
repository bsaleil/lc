#! gsi-script

(include "~~lib/_x86#.scm")

;;-----------------------------------------------------------------------------

(define (lazy-exprs exprs succ)
  (if (null? exprs)
      (or succ
          (make-lazy-code
            (lambda (cgc ctx)              
              (x86-pop cgc (x86-rax))
              (x86-add cgc (x86-rsp) (x86-imm-int (* (- (length (ctx-stack ctx)) 1) 8)))
              (pop-regs-reverse cgc prog-regs)
              (x86-ret cgc))))
      (gen-ast (car exprs)
               (lazy-exprs (cdr exprs) succ))))

;;-----------------------------------------------------------------------------
;; Interactive mode (REPL)

(define (repl lib)

  (println "REPL")

  (init)

  (letrec (;; Lazy return
           (lazy-ret (make-lazy-code
                        (lambda (cgc ctx)
                           (x86-pop cgc (x86-rax))
                           (x86-add cgc (x86-rsp) (x86-imm-int (* (- (length (ctx-stack ctx)) 1) 8)))
                           (pop-regs-reverse cgc prog-regs)
                           (x86-ret cgc))))
           ;; Lazy lib
           (lazy-lib (lazy-exprs lib lazy-read-eval))
           ;; Lazy print
           (lazy-print (make-lazy-code
              (lambda (cgc ctx)
                 (x86-pop cgc (x86-rax))
                 (x86-mov cgc (x86-mem 0 (x86-r10)) (x86-rax))
                 (jump-to-version cgc (gen-ast '(pp $$REPL-RES) lazy-read-eval #f) (ctx-pop ctx)))))
           ;; Lazy read-eval
           (lazy-read-eval (make-lazy-code
              (lambda (cgc ctx)
                 (print "> ")
                 (let ((r (expand-tl (read))))
                    (cond ((equal? r '(unquote LC))
                              (let ((r (read)))
                                (eval r)
                                (jump-to-version cgc (gen-ast #f lazy-print) ctx)))
                          ((equal? r '(unquote q))
                              (jump-to-version cgc lazy-ret ctx))
                          (else
                              (jump-to-version cgc (gen-ast r lazy-print) ctx))))))))
    ;; Global var for print step
    (set! globals '(($$REPL-RES . 0)))
    ;; Gen
    (gen-version code-alloc lazy-lib (make-ctx '() '() -1)))

  ;; Execute mcb
  (##machine-code-block-exec mcb))

;;-----------------------------------------------------------------------------
;; Bash mode

(define (exec lib prog)

  (init)

  (let* ((lazy-prog (lazy-exprs prog #f))
         (lazy-lib  (lazy-exprs lib  lazy-prog)))
    
     (gen-version code-alloc
                  lazy-lib
                  (make-ctx '() '() -1)))
  
  (##machine-code-block-exec mcb))

;;-----------------------------------------------------------------------------
;; Command line args parser

;; Set options to #t and return list of files to execute
(define (parse-args args)
  (parse-args-h args '()))

(define (parse-args-h args files)
  (if (null? args)
      files
      (let ((arg (car args)))
        (if (eq? (string-ref arg 0) #\-)
          ;; Arg is an option
          (begin (cond ;; '-v-jit' to enable JIT verbose debugging
                       ((equal? arg "-v-jit") (set! verbose-jit #t))
                       ;; '-v-gc' to enable GC verbose debugging
                       ((equal? arg "-v-gc")  (set! verbose-gc  #t))
                       ;; '-v' to enable both JIT and GC verbose debugging
                       ((equal? arg "-v")     (set! verbose-jit #t)
                                              (set! verbose-gc  #t))
                       ;; Other option stops exec
                       (else (print "Unknown option ")
                             (println arg)
                             (exit 1)))
                 (parse-args-h (cdr args) files))
          ;; Arg is a file to execute
          (parse-args-h (cdr args) (cons arg files))))))
  
;;-----------------------------------------------------------------------------
;; Main

(define (main . args)
    
  ;; Get library
  ;(define lib '())
  (define lib (expand-tl (read-all (open-input-file "./lib.scm"))))
  ;; Get options and files from cl args
  (define files (parse-args args))
  
    (cond ;; If no files specified then start REPL
          ((null? files)
            (repl lib))
          ;; Can only exec 1 file
          ((= (length files) 1)
            (let ((file-content (expand-tl (read-all (open-input-file (car args))))))
               ;(pp file-content)))
               (exec lib file-content)))
          (else (error "NYI")))
  (pp "Global cctable size=")
  (pp (length global-cc-table)))