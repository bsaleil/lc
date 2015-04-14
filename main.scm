#! gsi-script

(include "~~lib/_x86#.scm")
(include "./extern/Sort.scm")

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
                 (let ((r (car (expand-tl (list (read))))))
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
;; Main
(define (main . args)
    
  ;; Get library
  ;(define lib '())
  (define lib (expand-tl (read-all (open-input-file "./lib.scm"))))
  ;; Get options and files from cl args
  (define files (parse-cl-args args))
  
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
                       (if (eq? (car el) 'define)
                          (if (pair? (cadr el))
                             (cons (cons (caadr el) (get-global-type el)) (get-gids (cdr lib) base))
                             (cons (cons (cadr el)  (get-global-type el)) (get-gids (cdr lib) base)))
                          (get-gids (cdr lib) base)))))
                
                (set! gids (get-gids lib '()))
                (set! gids (get-gids content gids))
                
                (let ((exp-content (expand-tl content)))
                   ;(pp file-content)))
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

(define (count lst fn)
  (foldr (lambda (n r) (if (fn n) (+ 1 r) r)) 0 lst))

(define-macro (case-equal key clause . clauses)
    (cond ((and (null? clauses)
                (eq? (car clause) 'else))
             `(begin ,@(cdr clause)))
          ((null? clauses)
             `(if (member ,key (quote ,(car clause)))
                 (begin ,@(cdr clause))))
          (else
             `(if (member ,key (quote ,(car clause)))
                 (begin ,@(cdr clause))
                 (case-equal ,key ,(car clauses) ,@(cdr clauses))))))

;;-----------------------------------------------------------------------------
;; Command line arguments

;; Parser
(define (parse-cl-args args)
  
  (define (parse-cl-args-h args files)
    (cond ;; No args, return list of files
          ((null? args)
            files)
          ;; File (does not begin with #\-)
          ((not (eq? (string-ref (car args) 0) #\-))
            (parse-cl-args-h (cdr args) (cons (car args) files)))
          ;; Else it's an option
          (else
            (let ((first (car args)))
              (case-equal first
                (("--verbose-jit") (set! opt-verbose-jit #t))
                (("--verbose-gc")  (set! opt-verbose-gc  #t))
                (("--verbose")     (set! opt-verbose-jit #t)
                                   (set! opt-verbose-gc  #t))
                (("--all-tests")   (set! opt-all-tests #t))
                (("--stats")       (set! opt-stats #t))
                (("--count-calls") (set! opt-count-calls (string->symbol (cadr args)))
                                   (set! args (cdr args))) ;; Remove one more arg
                (else (error "Unknown option" first)))
              (parse-cl-args-h (cdr args) files)))))
  
  (parse-cl-args-h args '()))
    
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
  (let ((code-bytes (- code-alloc mcb-addr))
        (stub-bytes (- (+ mcb-addr mcb-len) stub-alloc)))
    ;; Code size
    (println "Code size (bytes): " code-bytes)
    ;; Stub size
    (println "Stub size (bytes): " stub-bytes)
    ;; Code + Stub size
    (println "Total size (bytes): " (+ code-bytes stub-bytes))
    ;; Global cc-table size
    (println "Global table size: " (table-length global-cc-table))
    ;; Min/Max versions number of stubs
    (let ((versions-info (get-versions-info all-lazy-code)))
      (println "Min versions number: " (car versions-info))
      (println "Max versions number: " (cdr versions-info)))
    ;; Number of stubs, number of return stubs, and number of entry stubs for each number of versions
    (println "-------------------------")
    (println "#versions;#stubs;#ret;#entry")
    (let ((versions-info-full (get-versions-info-full all-lazy-code)))
      (for-each (lambda (n)
                  (println (car n) ";"
                           (cadr n) ";"
                           (count (cddr n) (lambda (n) (member 'ret n))) ";"
                           (count (cddr n) (lambda (n) (member 'entry n)))))
                (sort versions-info-full (lambda (n m) (< (car n) (car m)))))
      (println "-------------------------"))))