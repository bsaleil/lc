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
          
              (if count-calls
                (begin (gen-print-msg cgc "-----------------------------------")
                       (gen-print-slot cgc
                                       'calls
                                       (string-append "Calls '"
                                                      (symbol->string count-calls)
                                                      "' = "))))
              
              (if count-tests
                (begin (gen-print-msg cgc "-----------------------------------")
                       (gen-print-slot cgc
                                       'tests
                                       "#Tests = ")))
              
              (if count-closures
                (begin (gen-print-msg cgc "-----------------------------------")
                       (gen-print-slot cgc
                                       'closures
                                       "#Closures = ")))
              
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
                       ((equal? arg "-v-jit")
                          (set! verbose-jit #t)
                          (parse-args-h (cdr args) files))
                       ;; '-v-gc' to enable GC verbose debugging
                       ((equal? arg "-v-gc")
                          (set! verbose-gc  #t)
                          (parse-args-h (cdr args) files))
                       ;; '-v' to enable both JIT and GC verbose debugging
                       ((equal? arg "-v")
                          (set! verbose-jit #t)
                          (set! verbose-gc  #t)
                          (parse-args-h (cdr args) files))
                       ((equal? arg "--count-calls")
                          (let ((call-name (cadr args)))
                            (if (char=? (string-ref call-name 0) #\-)
                               (error "Invalid argument --count-calls " call-name))
                            (set! count-calls (string->symbol call-name))
                            (parse-args-h (cddr args) files)))
                       ((equal? arg "--print-ccsize")
                          (set! print-ccsize #t)
                          (parse-args-h (cdr args) files))
                       ((equal? arg "--count-tests")
                          (set! count-tests #t)
                          (parse-args-h (cdr args) files))
                       ((equal? arg "--count-closures")
                          (set! count-closures #t)
                          (parse-args-h (cdr args) files))
                       ((equal? arg "--all-tests")
                          (set! all-tests #t)
                          (parse-args-h (cdr args) files))
                       ((equal? arg "--print-versions")
                          (set! print-versions #t)
                          (parse-args-h (cdr args) files))
                       ((equal? arg "--print-versions-full")
                          (set! print-versions-full #t)
                          (parse-args-h (cdr args) files))
                       ;; Other option stops exec
                       (else (print "Unknown option ")
                             (println arg)
                             (exit 1))))
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
    
    (if print-versions
       (let ((info (get-versions-info all-lazy-code)))
         (println "-----------------------------------")
         (println "Number of versions:")
         (println "   Min: " (car info))
         (println "   Max: " (cdr info))))

    (if print-versions-full
       (let ((info (get-versions-info-full all-lazy-code)))
         (println "------------------------------------")
         (println "#versions   #stubs     #ret   #entry")
         (println "------------------------------------")
         (for-each (lambda (n) (print
                                 ;; #versions
                                 (make-string (- 9 (string-length (number->string (car n)))) #\space)
                                 (car n)
                                 ;; #stubs
                                 (make-string (- 9 (string-length (number->string (cadr n)))) #\space)
                                 (cadr n))
                               ;; # with ret flag
                               (let ((rf (count (cddr n) (lambda (n) (member 'ret n)))))
                                (print (make-string (- 9 (string-length (number->string rf))) #\space)
                                       rf))
                               ;; # with entry flag
                               (let ((rf (count (cddr n) (lambda (n) (member 'entry n)))))
                                (println (make-string (- 9 (string-length (number->string rf))) #\space)
                                         rf)))
                   (sort info (lambda (n m) (< (car n) (car m)))))))

    (if print-ccsize
       (begin (println "-----------------------------------")
              (println "Global cctable size = " (table-length global-cc-table)))))

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

;;-----------------------------------------------------------------------------