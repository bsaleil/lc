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
;; REPL

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
                 (jump-to-version cgc (gen-ast '(pp $$REPL-RES) lazy-read-eval) (ctx-pop ctx)))))
           ;; Lazy read-eval
           (lazy-read-eval (make-lazy-code
              (lambda (cgc ctx)
                 (print "> ")
                 (let ((r (expand (read))))
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
;; FILE EXEC

(define (exec lib prog)

  (init)

  (let* ((lazy-prog (lazy-exprs prog #f))
         (lazy-lib  (lazy-exprs lib  lazy-prog)))
    
     (gen-version code-alloc
                  lazy-lib
                  (make-ctx '() '() -1)))
  
  (##machine-code-block-exec mcb))

;;-----------------------------------------------------------------------------
;; Get lib and args

(define lib (expand (read-all (open-input-file "./lib.scm"))))

(define args (list-tail (command-line) 1))

(define opts  '())
(define files '())

(define (handle-args args)
  (if (not (null? args))
     (let ((e (car args)))
        (if (eq? (string-ref e 0) #\-)
           (set! opts  (cons e opts))
           (set! files (cons e files)))
        (handle-args (cdr args)))))

(handle-args args)

(if (member "-d" opts)
   (set! dev-log #t))

;;-----------------------------------------------------------------------------
;; MAIN

(cond ((null? files)
          (repl lib))
      ((= (length files) 1)
          (let ((file-content (expand (read-all (open-input-file (car args))))))
             (exec lib file-content)))
      (else (error "NYI")))