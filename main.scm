#! gsi-script

(include "~~lib/_x86#.scm")

(define pp pretty-print)

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
                 (jump-to-version cgc (gen-ast '(pp $$REPL-RES) lazy-read-eval) (ctx-pop ctx)))))
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
  (time (##machine-code-block-exec mcb)))

;;-----------------------------------------------------------------------------
;; Bash mode

(define (exec lib prog)

  ;; Récupérer les noms des fonctions de librairie
  ;; Simplement détecter les appels à la librairie standard, opti
  
  ;; TODO
  ;; TODO
  ;; Idée : Stocker une table spéciale pour les fonctions de librairie avec chaque contexte
  ;; (on garde l'addresse de la cc table de chaque fonction de lib) pour faire un appel sans indirection
  ;; donc, quand on appelle une fonction de lib, pas d'indirection
  ;; AU GC, MAJ des emplacement des fonctions, ou sinon, allouer les fonctions de lib dans un endroit special
  (define (mytest lib accu)
    (if (null? lib)
       accu
       (let ((entry (car lib)))
         
         (if (eq? (car entry) 'define)
            (let ((cell (alloc-still-vector 2)))
              (vector-set! cell 0 #f)
              (vector-set! cell 1 #f)
              (if (list? (cadr entry))
                 (mytest (cdr lib) (cons (cons (caadr entry) cell) accu))
                 (mytest (cdr lib) (cons (cons (cadr entry)  cell) accu))))
            (mytest (cdr lib) accu)))))
  
  (init)
  
  (let* ((lazy-prog (lazy-exprs prog #f))
         (lazy-lib  (lazy-exprs lib  lazy-prog)))
    
     (gen-version code-alloc
                  lazy-lib
                  (make-ctx '() '() -1)))
  
  ;(time (##machine-code-block-exec mcb))
  ;(time (##machine-code-block-exec mcb)))
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
  (pp (table-length global-cc-table)))
  ;(pp (table->list global-cc-table)))
  