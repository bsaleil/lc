(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

(include "codegen.scm") ;; TODO Create module after refactoring

;;-----------------------------------------------------------------------------
;; Macros

(define-macro (assert c err)
  `(if (not ,c)
       (begin (pp ast)
	      (println "!!! ERROR : " ,err)
	      (exit 1))))

;; Generate primitive types lists from types pattern (used in 'primitives' set)
(define-macro (prim-types . args)
  (define (list-head l n)
    (if (= n 0)
       '()
       (cons (car l)
             (list-head (cdr l) (- n 1)))))
  (if (= (length args) (+ (car args) 1))
     `(list (list ,(car args) ,@(cdr args)))
     `(cons (list ,(car args) ,@(list-head (cdr args) (car args)))
            (prim-types ,@(list-tail args (+ (car args) 1))))))

;;-----------------------------------------------------------------------------
;; Type predicates

(define type-predicates `(
  (output-port? . ,CTX_OPORT)
  (input-port?  . ,CTX_IPORT)
  (symbol?      . ,CTX_SYM)
  (string?      . ,CTX_STR)
  (char?        . ,CTX_CHAR)
  (vector?      . ,CTX_VECT)
  (fixnum?      . ,CTX_NUM)
  (flonum?      . ,CTX_FLO)
  (procedure?   . ,CTX_CLO)
  (pair?        . ,CTX_PAI)
))

(define (type-predicate? sym)
  (assq sym type-predicates))

(define (predicate-to-ctxtype predicate)
  (let ((r (assq predicate type-predicates)))
    (if r
      (cdr r)
      (error ERR_INTERNAL))))

;;-----------------------------------------------------------------------------
;; Primitives

;; Primitives: name, nb args min, nb args max
(define primitives `(
   (car                 1  1  ,(prim-types 1 CTX_PAI))
   (cdr                 1  1  ,(prim-types 1 CTX_PAI))
   (eq?                 2  2  ,(prim-types 2 CTX_ALL CTX_ALL))
   (char=?              2  2  ,(prim-types 2 CTX_CHAR CTX_CHAR))
   (null?               1  1  ,(prim-types 1 CTX_ALL))
   (not                 1  1  ,(prim-types 1 CTX_ALL))
   (set-car!            2  2  ,(prim-types 2 CTX_PAI CTX_ALL))
   (set-cdr!            2  2  ,(prim-types 2 CTX_PAI CTX_ALL))
   (cons                2  2  ,(prim-types 2 CTX_ALL CTX_ALL))
   (vector-length       1  1  ,(prim-types 1 CTX_VECT))
   (vector-ref          2  2  ,(prim-types 2 CTX_VECT CTX_NUM))
   (char->integer       1  1  ,(prim-types 1 CTX_CHAR))
   (integer->char       1  1  ,(prim-types 1 CTX_NUM))
   (string-ref          2  2  ,(prim-types 2 CTX_STR CTX_NUM))
   (string->symbol      1  1  ,(prim-types 1 CTX_STR))
   (symbol->string      1  1  ,(prim-types 1 CTX_SYM))
   (close-output-port   1  1  ,(prim-types 1 CTX_OPORT))
   (close-input-port    1  1  ,(prim-types 1 CTX_IPORT))
   (open-output-file    1  1  ,(prim-types 1 CTX_STR))
   (open-input-file     1  1  ,(prim-types 1 CTX_STR))
   (string-set!         3  3  ,(prim-types 3 CTX_STR CTX_NUM CTX_CHAR))
   (vector-set!         3  3  ,(prim-types 3 CTX_VECT CTX_NUM CTX_ALL))
   (string-length       1  1  ,(prim-types 1 CTX_STR))
   (read-char           1  1  ,(prim-types 1 CTX_IPORT))
   (exit                0  0  ,(prim-types 0 ))
   (make-vector         1  2  ,(prim-types 1 CTX_NUM 2 CTX_NUM CTX_ALL))
   (make-string         1  2  ,(prim-types 1 CTX_NUM 2 CTX_NUM CTX_CHAR))
   (eof-object?         1  1  ,(prim-types 1 CTX_ALL))
   (write-char          2  2  ,(prim-types 2 CTX_CHAR CTX_OPORT))
   (current-output-port 0  0  ,(prim-types 0 ))
   (current-input-port  0  0  ,(prim-types 0 ))
))

(define (assert-p-nbargs ast)
  (assert (and (>= (length (cdr ast))
                   (cadr (assoc (car ast) primitives)))
               (<= (length (cdr ast))
                   (caddr (assoc (car ast) primitives))))
          ERR_WRONG_NUM_ARGS))

;;-----------------------------------------------------------------------------
;; AST DISPATCH

;; Gen lazy code from a list of exprs
(define (gen-ast-l lst succ)
  (foldr (lambda (el r) (gen-ast el r)) succ lst))

;; Gen lazy code from ast
(define (gen-ast ast succ)
  (cond ;; String
        ((string? ast)  (mlc-string ast succ))
        ;; Symbol
        ((symbol? ast) (mlc-identifier ast succ))
        ;; Flonum
        ((flonum? ast)  (mlc-flonum ast succ))
        ;; Other literal
        ((literal? ast) (mlc-literal ast succ))
        ;; Pair
        ((pair? ast)
         (let ((op (car ast)))
           (cond ;; Special
                 ((member op '(breakpoint list)) (mlc-special ast succ))
                 ;; TODO special function
                 ((eq? op '$$print-flonum) (mlc-printflonum ast succ))
                 ;; Inlined primitive
                 ((assoc op primitives) (mlc-primitive ast succ))
                 ;; Quote
                 ((eq? 'quote (car ast)) (mlc-quote (cadr ast) succ))
                 ;; Set!
                 ((eq? 'set! (car ast)) (mlc-set! ast succ))
                 ;; Lambda
                 ((eq? op 'lambda) (mlc-lambda ast succ #f))
                 ;; Begin
                 ((eq? op 'begin) (mlc-begin ast succ))
                 ;; Do
                 ((eq? op 'do) (mlc-do ast succ))
                 ;; Binding
                 ((member op '(let let* letrec)) (mlc-binding ast succ op))
                 ;; Operator num
                 ((member op '(FLOAT+ FLOAT- FLOAT* FLOAT/ FLOAT< FLOAT> FLOAT<= FLOAT>= FLOAT=))
                   (let ((generic-op (list->symbol (list-tail (symbol->list op) 5))))
                     (gen-ast (cons generic-op (cdr ast))
                              succ)))
                 ((member op '(+ - * < > <= >= = /))         (mlc-op-n ast succ op))   ;; nary operator
                 ((member op '(quotient modulo remainder)) (mlc-op-bin ast succ op)) ;; binary operator
                 ;; Type predicate
                 ((type-predicate? op) (mlc-test ast succ))
                 ;; If
                 ((eq? op 'if) (mlc-if ast succ))
                 ;; Define
                 ((eq? op 'define) (mlc-define ast succ))
                 ;; Apply
                 ((eq? op '$apply) (mlc-apply ast succ))
                 ;; Call expr
                 (else (mlc-call ast succ)))))
        ;; *unknown*
        (else
         (error "unknown ast" ast))))

;;-----------------------------------------------------------------------------
;; LITERALS

;;
;; Make lazy code from flonum literal
;;
(define (mlc-flonum ast succ)
  (make-lazy-code
      (lambda (cgc ctx)
        (let ((immediate
                (if (< ast 0)
                    (let* ((ieee-rep (ieee754 (abs ast) 'double))
                           (64-mod   (bitwise-not (- ieee-rep 1)))
                           (64-modl  (bitwise-and (- (expt 2 63) 1) 64-mod)))
                      (* -1 64-modl))
                    (ieee754 ast 'double))))
          (x86-codegen-flonum cgc immediate)
          (jump-to-version cgc succ (ctx-push ctx CTX_FLO))))))

;;
;; Make lazy code from symbol literal
;;
(define (mlc-symbol ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (let ((qword (get-symbol-qword ast)))
        (x86-codegen-symbol cgc ast)
        (jump-to-version cgc succ (ctx-push ctx CTX_SYM))))))

;;
;; Make lazy code from vector literal
;;
(define (mlc-vector ast succ)

  ;; Generate lazy object which pop object from stack
  ;; and mov it to vector slot and jump to next element
  (define (lazy-vector-set-gen idx)
    (make-lazy-code
      (lambda (cgc ctx)
        (x86-pop cgc (x86-rax)) ;; el
        (x86-pop cgc (x86-rbx)) ;; vector
        (x86-mov cgc (x86-mem (- (+ 16 (* idx 8)) TAG_MEMOBJ) (x86-rbx)) (x86-rax))
        (x86-push cgc (x86-rbx))
        (if (= idx (- (vector-length ast) 1))
           (jump-to-version cgc
                            succ
                            (ctx-pop ctx))
           (jump-to-version cgc
                            (lazy-el-gen (+ idx 1))
                            (ctx-pop ctx))))))

  ;; Generate lazy-object which gen and push the value
  ;; at vector[idx].
  (define (lazy-el-gen idx)
    (gen-ast (vector-ref ast idx)
             (lazy-vector-set-gen idx)))

  ;; Main lazy code
  (make-lazy-code
    (lambda (cgc ctx)
      (let ((header-word (mem-header (+ 2 (vector-length ast)) STAG_VECTOR)))
        ;; Alloc
        (gen-allocation cgc ctx STAG_VECTOR (+ (vector-length ast) 2))
        ;; Write header
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
        ;; Write length
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (vector-length ast))))
        (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
        ;; Push vector
        (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
        (x86-push cgc (x86-rax))
        (if (> (vector-length ast) 0)
          (jump-to-version cgc
                           (lazy-el-gen 0)
                           (ctx-push ctx CTX_VECT))
          (jump-to-version cgc
                           succ
                           (ctx-push ctx CTX_VECT)))))))

;;
;; Make lazy code from string literal
;;
(define (mlc-string ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (x86-codegen-string cgc ast)
      (jump-to-version cgc succ (ctx-push ctx CTX_STR)))))

;;
;; Make lazy code from QUOTE
;;
(define (mlc-quote ast succ)
  (cond ((pair? ast)
         (let* ((lazy-pair (mlc-pair succ))
                (lazy-cdr  (mlc-quote (cdr ast) lazy-pair)))
           (mlc-quote (car ast) lazy-cdr)))
        ((symbol? ast)
            (mlc-symbol ast succ))
        ((vector? ast)
            (mlc-vector ast succ))
        (else (gen-ast ast succ))))

;;
;; Make lazy code from SYMBOL
;;
(define (mlc-identifier ast succ)

 (make-lazy-code
   (lambda (cgc ctx)

     (let ((local  (assoc ast (ctx-env ctx)))
           (global (assoc ast globals)))
     ;; If local or global (not primitive nor type predicate)
     (if (or local global)
         ;; Id lookup
         (let ((ctx-type (cond (local
                                  (if (eq? (identifier-type (cdr local)) 'free)
                                     ;; Free var
                                     (gen-get-freevar  cgc ctx local 'stack #f)
                                     ;; Local var
                                     (gen-get-localvar cgc ctx local 'stack #f)))
                               (global
                                  (gen-get-globalvar cgc ctx global 'stack))
                               (else
                                  (gen-error cgc (ERR_UNKNOWN_VAR ast))))))

            (let* ((nctx (if (eq? ctx-type CTX_MOBJ)
                           (ctx-push ctx CTX_UNK ast)
                           (ctx-push ctx ctx-type ast))))
              (jump-to-version cgc succ nctx)))
         ;; Else it is a primitive / type predicate
         (let ((r (assoc ast primitives)))
            (if r
               ;; Create and return function calling primitive
               (let ((args (build-list (cadr r) (lambda (x) (string->symbol (string-append "arg" (number->string x)))))))
                 (jump-to-version cgc
                                  (gen-ast `(lambda ,args
                                              (,ast ,@args))
                                           succ)
                                  ctx))
               ;; Type predicate
               (if (type-predicate? ast)
                 (jump-to-version cgc
                                  (gen-ast `(lambda (a) (,ast a)) succ)
                                  ctx)
                 (gen-error cgc (ERR_UNKNOWN_VAR ast))))))))))

;;
;; Make lazy code from num/bool/char/null literal
;;
(define (mlc-literal ast succ)
  (if (and (number? ast)
           (or (>= ast (expt 2 61))
               (<  ast (* -1  (expt 2 60)))))
    (mlc-flonum ast succ)
    (make-lazy-code
      (lambda (cgc ctx)
        (x86-codegen-literal cgc ast)
        (jump-to-version cgc
                         succ
                         (ctx-push ctx
                                   (cond ((integer? ast) CTX_NUM)
                                         ((boolean? ast) CTX_BOOL)
                                         ((char? ast)    CTX_CHAR)
                                         ((null? ast)    CTX_NULL)
                                         (else (error ERR_INTERNAL)))))))))

;;-----------------------------------------------------------------------------
;; INTERNAL FORMS

;;
;; Make lazy code from SET!
;;
(define (mlc-set! ast succ)

  (let* ((id (cadr ast))
         (lazy-set
            (make-lazy-code
               (lambda (cgc ctx)
                  (let* ((glookup-res (assoc id globals))
                         (nctx
                           (if glookup-res
                              ;; Global var
                              (gen-set-globalvar cgc ctx glookup-res)
                                 (let ((res (assoc id (ctx-env ctx))))
                                    (if res
                                       (if (eq? (identifier-type (cdr res)) 'free)
                                          (gen-set-freevar  cgc ctx res)  ;; Free var
                                          (gen-set-localvar cgc ctx res)) ;; Local var
                                      #f)))))
                      (if (not nctx)
                          (gen-error cgc (ERR_UNKNOWN_VAR id))
                          (begin
                            (x86-codegen-void cgc)
                            (jump-to-version cgc succ (ctx-push (ctx-pop nctx) CTX_VOID)))))))))

     (gen-ast (caddr ast) lazy-set)))

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)

  (let* ((identifier (cadr ast))
         (lazy-bind (make-lazy-code
                      (lambda (cgc ctx)
                        (let* ((res (assoc identifier globals)) ;; Lookup in globals
                               (pos (cdr res)))                 ;; Get global pos
                          (x86-codegen-define-bind cgc pos)
                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID))))))
         (lazy-val (gen-ast (caddr ast) lazy-bind)))

    (make-lazy-code
      (lambda (cgc ctx)
        (x86-codegen-define-id cgc)
        (set! globals (cons (cons identifier (length globals)) globals))
        (jump-to-version cgc lazy-val ctx)))))

;;
;; Make lazy code from LAMBDA
;;

;; Store the cc table associated to each lambda (ast -> cctable)
;; cctable is a still vector
(define cctables (make-table test: (lambda (a b) (and (eq?    (car a) (car b))     ;; eq? on ast
                                                      (equal? (cdr a) (cdr b)))))) ;; equal? on ctx information

;; Store the cr table associated to each lambda (ast -> crtable)
;; crtable is a still vector
(define crtables (make-table test: (lambda (a b) (and (eq?    (car a) (car b))
                                                      (equal? (cdr a) (cdr b))))))

;; TODO
;; TODO
(define entry-points-locs (make-table test: eq?))

(define (get-entry-points-loc ast stub-addr)
  (let ((r (table-ref entry-points-locs ast #f)))
    (if r
        r
        (let ((v (alloc-still-vector 1)))
          (vector-set! v 0 (quotient stub-addr 4)) ;; quotient 4 because vector-set write the encoded value (bug when using put-i64?)
          (table-set! entry-points-locs ast v)
          v))))
;; TODO
;; TODO

;; Return cctable from cctable-key
;; Return the existing table if already created or create one, add entry, and return it
(define (get-cctable ast cctable-key stub-addr generic-addr)
  (let ((cctable (table-ref cctables cctable-key #f)))
    (if cctable
      cctable
      (let ((t (make-cc (+ 1 global-cc-table-maxsize) stub-addr generic-addr)))
        (table-set! cctables cctable-key t)
        (set! cctables-loc-code (cons (cons (- (obj-encoding t) 1)
                                            ast)
                                      cctables-loc-code))
        t))))

;; Return crtable from crtable-key
;; Return the existing table if already created or create one, add entry, and return it
(define (get-crtable ast crtable-key stub-addr)
  (let ((crtable (table-ref crtables crtable-key #f)))
    (if crtable
        crtable
        (let ((t (make-cr global-cr-table-maxsize stub-addr)))
          (table-set! crtables crtable-key t)
          t))))

;; Return crtable key from ast and ctx
;; The key contains ast, stack types, and a list of known identifier with types
(define (get-crtable-key ast ctx)
  (cons ast
        (cons (ctx-stack ctx)
          (map (lambda (el)
                 (cons (car el)
                       (identifier-stype (cdr el))))
               (ctx-env ctx)))))

;; TODO: change to table, merge with cctables ? (change cctable-key to something better)
;; Store pairs associating cctable address to the code of the corresponding function
(define cctables-loc-code '())

(define (mlc-lambda ast succ lib-define)

  (let* (;; Lambda free vars
         (fvars #f)
         ;; Flatten list of param (include rest param)
         (all-params (flatten (cadr ast)))
         ;; Lambda mutable vars
         (mvars (mutable-vars (caddr ast) all-params))
         ;; Rest param ?
         (rest-param (or (and (not (list? (cadr ast))) (not (pair? (cadr ast)))) ;; (foo . rest)
                         (and (pair? (cadr ast)) (not (list? (cadr ast)))))) ;; (foo a..z . rest)
         ;; Params list
         (params
           (if rest-param
              (formal-params (cadr ast))
              (cadr ast)))
         ;; Lazy lambda return
         (lazy-ret (make-lazy-code-ret ;; Lazy-code with 'ret flag
                     (lambda (cgc ctx)
                       ;; Stack:
                       ;;         RSP
                       ;;     | ret-val | closure | arg n |  ...  | arg 1 | ret-addr |
                       ;; Or if rest :
                       ;;     | ret-val | closure |  rest | arg n |  ...  | arg 1 | ret-addr |
                       (let ((retval-offset
                                (if rest-param
                                   (* 8 (+ 2 (length params)))
                                   (* 8 (+ 1 (length params))))))
                         ;; Pop return value
                         (x86-pop  cgc (x86-rax))
                         ;; Update SP to ret addr
                         (x86-add  cgc (x86-rsp) (x86-imm-int retval-offset))

                         (if opt-return-points
                             ;; Get return point from cr table and jump to it
                             (let* ((ret-type (car (ctx-stack ctx)))
                                    (cridx (type-to-cridx ret-type)))
                               (x86-pop cgc (x86-rdx))
                               (x86-mov cgc (x86-rbx) (x86-mem cridx (x86-rdx)))
                               (x86-mov cgc (x86-r11) (x86-imm-int cridx)) ;; TODO: use rcx (selector)
                               (x86-jmp cgc (x86-rbx)))
                            (begin ;; Do not use ret
                                (x86-pop cgc (x86-rdx))
                                (x86-jmp cgc (x86-rdx))))))))
         ;; Lazy lambda body
         (lazy-body (gen-ast (caddr ast) lazy-ret))
         ;; Lazy function prologue : creates rest param if any, transforms mutable vars, ...
         (lazy-prologue (get-lazy-prologue ast lazy-body rest-param mvars))
         ;; Same as lazy-prologue but generate a generic prologue (no matter what the arguments are)
         (lazy-generic-prologue (get-lazy-generic-prologue ast lazy-body rest-param mvars params)))

    ;; Lazy closure generation
    (make-lazy-code
      (lambda (cgc ctx)

        (let* (;; Lambda stub
               (stub-labels (add-fn-callback cgc
                                             1
                                             (lambda (sp sctx ret-addr selector closure)

                                              (cond ;; CASE 1 - Use multiple entry points AND use max-versions limit AND this limit is reached
                                                    ((and (= selector 0)
                                                          opt-max-versions
                                                          (>= (lazy-code-nb-versions lazy-prologue) opt-max-versions))

                                                       ;; Then, generate a generic version and patch cc-table entry corresponding to call-ctx with the generic version
                                                       (let* ((env     (build-env mvars all-params 0 (build-fenv (ctx-stack ctx) (ctx-env ctx) mvars fvars 0)))
                                                              (nb-args (length params))
                                                              ;; Call ctx used to patch cc-table
                                                              (call-ctx (make-ctx (ctx-stack sctx) env nb-args))
                                                              ;; Generic ctx used to generate version
                                                              (gen-ctx  (make-ctx (append (cons CTX_CLO (make-list (length params) CTX_UNK)) (list CTX_RETAD)) env nb-args)))
                                                          (gen-version-fn ast closure lazy-generic-prologue gen-ctx call-ctx #f)))

                                                    ;; CASE 2 - Don't use multiple entry points
                                                    ((= selector 1)
                                                        ;; Then, generate a generic version and patch generic ptr in closure
                                                        (let ((ctx (make-ctx (append (cons CTX_CLO (make-list (length params) CTX_UNK)) (list CTX_RETAD))
                                                                             (build-env mvars all-params 0 (build-fenv (ctx-stack ctx) (ctx-env ctx) mvars fvars 0))
                                                                             (length params))))
                                                          (gen-version-fn ast closure lazy-generic-prologue ctx ctx #t)))

                                                    ;; CASE 3 - Use multiple entry points AND limit is not reached or there is no limit
                                                    (else
                                                       ;; Then, generate a specified version and patch cc-table in closure
                                                       (let ((ctx (make-ctx (ctx-stack sctx)
                                                                            (build-env mvars all-params 0 (build-fenv (ctx-stack ctx) (ctx-env ctx) mvars fvars 0))
                                                                            (length params))))
                                                         (gen-version-fn ast closure lazy-prologue ctx ctx #f)))))))
               (stub-addr (vector-ref (list-ref stub-labels 0) 1))
               (generic-addr (vector-ref (list-ref stub-labels 1) 1)))

          ;; Get free vars from ast
          (set! fvars (free-vars (caddr ast) all-params ctx))

          ;; If 'stats' option, then inc closures slot
          (if opt-stats
            (gen-inc-slot cgc 'closures))

          (let* ((total-size  (+ 2 (length fvars))) ;; Header,CCTable
                 (header-word (mem-header total-size STAG_PROCEDURE))
                 (cctable-key (get-cctable-key ast ctx fvars))
                 (cctable (get-cctable ast cctable-key stub-addr generic-addr))
                 (cctable-loc (- (obj-encoding cctable) 1)))

            ;; Alloc closure
            (gen-allocation cgc ctx STAG_PROCEDURE total-size)

            ;; 1 - Write closure header
            (x86-mov cgc (x86-rax) (x86-imm-int header-word))
            (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))

            ;; 2 - Write cctable-ptr or entry point
            (if opt-entry-points
                (begin (x86-mov cgc (x86-rax) (x86-imm-int cctable-loc))
                       (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
                (let ((eploc (get-entry-points-loc ast stub-addr))) ;; TODOOO
                  (x86-mov cgc (x86-rax) (x86-mem (+ 8 (- (obj-encoding eploc) 1))))
                  (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))))

            ;; 3 - Write free vars
            (gen-free-vars cgc fvars ctx 16)

            ;; Tag and push closure
            (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
            (x86-push cgc (x86-rax))

            ;; Jump to next
            (jump-to-version cgc
                             succ
                             (ctx-push ctx (CTX_CLOi cctable-loc)))))))))

;; This is the key used in hash table to find the cc-table for this closure.
;; The key is (ast . free-vars-inf) with ast the s-expression of the lambda
;; and free-vars-inf the type information of free vars ex. ((a . number) (b . char))
;; The hash function uses eq? on ast, and equal? on free-vars-inf.
;; This allows us to use different cctable if types of free vars are not the same.
;; (to properly handle type checks)
(define (get-cctable-key ast ctx fvars)
  (cons ast
        (foldr (lambda (n r)
                 (if (member (car n) fvars) ;; If this id is a free var of future lambda
                     (cons (cons (car n)
                                 (if (eq? (identifier-type (cdr n)) 'local)
                                     ;; If local, get type from stack
                                     (list-ref (ctx-stack ctx) (- (length (ctx-stack ctx)) (identifier-offset (cdr n)) 2))
                                     ;; If free, get type from env
                                     (identifier-stype (cdr n))))
                           r)
                     r))
               '()
               (ctx-env ctx))))

;; Create and return a generic lazy prologue
(define (get-lazy-generic-prologue ast succ rest-param mvars params)
    (make-lazy-code-entry
       (lambda (cgc ctx)

          (let ((formal-p (length params))
                (err-labels (add-callback #f 0 (lambda (ret-addr selector)
                                                  (error ERR_WRONG_NUM_ARGS)))))

            (if (not rest-param)
              ;; If there is no rest param
              ;; Then we only have to check the number of argumentssss
              (begin
                (x86-cmp cgc (x86-rdi) (x86-imm-int (* formal-p 4)))
                (x86-jne cgc (list-ref err-labels 0)))
              ;; If there is a rest param
              ;; Then we have to handle 3 cases: actual>formal, actual=formal, actual<formal
              (let ((label-loop-end  (asm-make-label #f (new-sym 'rest-param-loop-end)))
                    (label-end       (asm-make-label #f (new-sym 'rest-param-end)))
                    (label-loop      (asm-make-label #f (new-sym 'rest-param-loop)))
                    (label-eq        (asm-make-label #f (new-sym 'rest-param-eq)))
                    (header-word     (mem-header 3 STAG_PAIR)))

                 ;; If there is a rest param then we need to change the context to include it
                 ;; CTX_UNK because it could be NULL
                 (set! ctx (make-ctx (cons CTX_CLO (cons CTX_UNK (list-tail (ctx-stack ctx) (- (length (ctx-stack ctx)) formal-p 1))))
                                     (ctx-env ctx)
                                     (+ (ctx-nb-args ctx) 1)))

                 ;; Compare actual and formal
                 (x86-cmp cgc (x86-rdi) (x86-imm-int (* formal-p 4)))
                 (x86-jl cgc (list-ref err-labels 0)) ;; actual<formal, ERROR
                 (x86-je cgc label-eq)                ;; actual=formal, jump to label-eq
                                                      ;; actual>formal, continue

                 ;; CASE 1 - Actual > Formal

                 ;; Loop-init
                 (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                 (x86-mov cgc (x86-rbx) (x86-imm-int 8)) ;; rbx = arg-offset (first arg to copy is at [rsp+8])
                 (x86-mov cgc (x86-rdx) (x86-rdi))       ;; Save args number in rdx

                 ;; Loop-cond (if there is at least 1 arg to copy)
                 (x86-label cgc label-loop)
                 (x86-cmp cgc (x86-rdi) (x86-imm-int (* formal-p 4)))
                 (x86-je cgc label-loop-end)

                    ;; Loop-body
                    (x86-push cgc (x86-rax)) ;; TODO
                    (gen-allocation cgc ctx STAG_PAIR 3)                    ;; alloc pair p
                    (x86-pop cgc (x86-rax))                                 ;;
                    (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rax))          ;; p.cdr = rax (last pair)
                    (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp) (x86-rbx))) ;; p.car = stack[arg-offset]
                    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))         ;;
                    (x86-mov cgc (x86-rax) (x86-imm-int header-word))       ;; p.header = header-word
                    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))         ;;
                    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))   ;; rax = p (tagged)
                    (x86-add cgc (x86-rbx) (x86-imm-int 8))                 ;; offset += 8 (to next arg)
                    (x86-sub cgc (x86-rdi) (x86-imm-int 4))                 ;; rdi    -= 4 (update nb args to copy)
                    (x86-jmp cgc label-loop)                                ;; goto loop

                 ;; Loop-end
                 (x86-label cgc label-loop-end)
                 (x86-mov cgc (x86-mem (* -8 formal-p) (x86-rsp) (x86-rdx) 1) (x86-rax)) ;; Mov rest list to stack
                 (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; Update closure position
                 (x86-mov cgc (x86-mem (* -8 (+ formal-p 1)) (x86-rsp) (x86-rdx) 1) (x86-rax))
                 (x86-lea cgc (x86-rsp) (x86-mem (* -8 (+ formal-p 1)) (x86-rsp) (x86-rdx) 1)) ;; Update rsp
                 (x86-jmp cgc label-end) ;; goto end

                 ;; CASE 2 - Actual == Formal

                 (x86-label cgc label-eq)
                 (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; Update closure position
                 (x86-push cgc (x86-rax))
                 (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '()))) ;; Insert rest list (null) in stack
                 (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-rax))

                 ;; END
                 (x86-label cgc label-end)))

            ;; Gen mutable vars and jump to function body
            (jump-to-version cgc succ (gen-mutable cgc ctx mvars))))))

;; Create and return a lazy prologue
(define (get-lazy-prologue ast succ rest-param mvars)
  (make-lazy-code-entry
    (lambda (cgc ctx)

       (let* ((actual-p (- (length (ctx-stack ctx)) 2))
              (formal-p (ctx-nb-args ctx)))

         ;; Wrong number of arguments, ERROR
         (if (or (and (not rest-param) (not (= actual-p formal-p)))
                 (and rest-param (< actual-p formal-p)))
           (gen-error cgc ERR_WRONG_NUM_ARGS)
         ;; Right number of arguments
          (let ((nstack  (ctx-stack ctx))    ;; New stack  (change if rest-param)
                (nnbargs (ctx-nb-args ctx))) ;; New nbargs (change if rest-param)
            (if rest-param
                (cond ((= actual-p formal-p)
                         ;; Shift closure
                         (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                         (x86-push cgc (x86-rax))
                         ;; Mov '() in rest param slot
                         (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                         (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-rax))
                         ;; Update ctx information
                         (set! nstack (cons CTX_CLO (cons CTX_NULL (cdr (ctx-stack ctx)))))
                         (set! nnbargs (+ (ctx-nb-args ctx) 1)))
                      ((> actual-p formal-p)
                         ;; Build rest argument
                         (gen-rest-lst cgc ctx (- actual-p formal-p))
                         ;; Update ctx information
                         (set! nstack (cons CTX_CLO (cons CTX_PAI (list-tail (ctx-stack ctx) (- (length (ctx-stack ctx)) formal-p 1)))))
                         (set! nnbargs (+ (ctx-nb-args ctx) 1)))))
            (let* ((nctx (make-ctx nstack (ctx-env ctx) nnbargs))
                   (mctx (gen-mutable cgc nctx mvars)))
              (jump-to-version cgc succ mctx))))))))

;; Create a new cc table with 'init' as stub value
(define (make-cc len init generic)
  (let ((v (alloc-still-vector len)))
    (put-i64 (+ 8 (- (obj-encoding v) 1)) generic) ;; Write generic after header
    (let loop ((i 1))
      (if (< i (vector-length v))
        (begin (put-i64 (+ 8 (* 8 i) (- (obj-encoding v) 1)) init)
               (loop (+ i 1)))
        v))))

;; Create a new cr table with 'init' as stub value
(define (make-cr len init)
  (let ((v (alloc-still-vector len)))
    (let loop ((i 0))
      (if (< i (vector-length v))
        (begin (put-i64 (+ 8 (* 8 i) (- (obj-encoding v) 1)) init)
               (loop (+ i 1)))
        v))))

;;
;; Make lazy code from BEGIN
;;
(define (mlc-begin ast succ)
  (cond ;; There is no body
        ((null? (cdr ast))
           (if (member 'ret (lazy-code-flags succ))
             ;; No body and succ is a ret object
             (error ERR_BEGIN)
             ;; No body and succ is *not* a ret object
             (make-lazy-code
               (lambda (cgc ctx)
                 (x86-codegen-void cgc)
                 (jump-to-version cgc succ (ctx-push ctx CTX_VOID))))))
        ;; Only one body
        ((= (length (cdr ast)) 1)
           (gen-ast (cadr ast) succ))
        ;; >1 body
        (else
           (let (;; LAZY BEGIN OUT
                 (lazy-begin-out
                  (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                          make-lazy-code-ret
                          make-lazy-code)))
                    (make-lc
                      (lambda (cgc ctx)
                        (let* ((nctx (ctx-move ctx 0 (- (length (cdr ast)) 1)))
                               (mctx (ctx-pop-nb nctx (- (length (cdr ast)) 1))))
                          (x86-codegen-begin-out cgc (- (length (cdr ast)) 1))
                          (jump-to-version cgc succ mctx)))))))
             ;; LAZY BODIES
             (gen-ast-l (cdr ast) lazy-begin-out)))))

;;-----------------------------------------------------------------------------
;; Bindings (let, letrec, let*)

;; NOTE: Letrec: All ids are considered as mutable. Analysis to detect recursive use of ids?

;; Entry point to compile let, letrec
(define (mlc-binding ast succ op)
  (let ((ids (map car (cadr ast)))
        (bodies (cddr ast)))

    (cond ;; No bindings, it is a begin
          ((null? ids) (mlc-begin (cons 'begin bodies) succ))
          ;; No body, error
          ((null? bodies) (error (cond ((eq? op 'let)    ERR_LET)
                                       ((eq? op 'letrec) ERR_LETREC)
                                       ((eq? op 'let*)   ERR_LET*))))
          ;; >= 1 body
          (else
            (let* (;; LAZY LET OUT
                   ;; Clean ctx stack and env, update rsp
                   (lazy-let-out
                     (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                                      make-lazy-code-ret
                                      make-lazy-code)))
                       (make-lc ;; If succ is a ret object, then last object of begin is also a ret object
                         (lambda (cgc ctx)
                           (let* ((nctx (ctx-move ctx 0 (- (+ (length ids) (length bodies)) 1)))
                                  (mctx (ctx-pop-nb nctx (- (+ (length ids) (length bodies)) 1)))
                                  (env  (list-tail (ctx-env mctx) (length ids)))
                                  (ctx  (make-ctx (ctx-stack mctx) env (ctx-nb-args mctx))))
                            (x86-codegen-binding-clear cgc (+ (length ids) (length bodies) -1))
                            (jump-to-version cgc succ ctx))))))
                   ;; LAZY BODIES
                   (lazy-bodies (gen-ast-l bodies lazy-let-out)))

            ;; Delegate with bodies as successor
            (cond ((or (eq? op 'let)
                       (and (eq? op 'letrec)
                            ;; BAD HACK !
                            ;; If the bindings do not not contains the symcol 'lambda, use a let
                            (not (contains (map cadr (cadr ast)) 'lambda))))
                     (mlc-let ast lazy-bodies))
                  ((eq? op 'letrec) (mlc-letrec ast lazy-bodies))
                  ((eq? op 'let*)   (mlc-let* ast lazy-bodies))
                  (else (error "Unknown ast"))))))))

;;
;; Make lazy code from LET*
;;
(define (mlc-let* ast succ)
  ;; Build lazy objects chain for let* bindings
  (define (gen-let*-bindings ids values mvars)
    (if (null? ids)
      succ
      (let ((lazy-bind
              (make-lazy-code
                (lambda (cgc ctx)
                  (let* ((start (- (length (ctx-stack ctx)) 2))
                         (env (build-env mvars (list (car ids)) start (ctx-env ctx)))
                         (nctx (make-ctx (ctx-stack ctx) env (ctx-nb-args ctx)))
                         ;; If this id is mutable then gen mobject
                         (mctx (if (member (car ids) mvars)
                                  (gen-mutable cgc nctx (list (car ids)))
                                  nctx)))
                      ;; Jump to next id (or succ) with new ctx
                      (jump-to-version cgc
                                       (gen-let*-bindings (cdr ids) (cdr values) mvars)
                                       mctx))))))
        ;; Gen value
        (gen-ast (car values) lazy-bind))))

  (let* ((ids (map car (cadr ast)))
         (values (map cadr (cadr ast)))
         (mvars (mutable-vars ast ids)))
    ;; Init call with all ids
    (gen-let*-bindings ids values mvars)))

;;
;; Make lazy code from LETREC
;;
(define (mlc-letrec ast succ)
  (let* ((ids (map car (cadr ast)))
         (values (map cadr (cadr ast)))
         ;; 3 - Bind values to their locations and jump to bodies
         (lazy-let-mid
            (make-lazy-code
               (lambda (cgc ctx)
                  (let ((ctx (gen-letrec-binds cgc ctx ids)))
                     (jump-to-version cgc succ ctx)))))
         ;; 2 - Gen all bound values
         (lazy-values (gen-ast-l values lazy-let-mid)))

    ;; 1 - Push initial values, Update env, ctx,
    ;;     gen mutable and jump to values
    (make-lazy-code
       (lambda (cgc ctx)
          (let* ((stack (ctx-stack (ctx-push-nb ctx CTX_BOOL (length ids))))
                 (start (- (length stack) (length ids) 1))
                 (env (build-env ids ids start (ctx-env ctx)))
                 (nctx (make-ctx stack env (ctx-nb-args ctx))))

             ;; Create values on stack (initial value is #f)
             (call-n (length ids) x86-push cgc (x86-imm-int (obj-encoding #f)))

             ;; All ids are considered mutable except those that are both
             ;; non-mutable AND lambda expr. This allows the compiler to keep
             ;; the type of non mutable lambdas which represents a large part
             ;; of letrec uses.
             (let* ((mvars (mutable-vars ast ids))
                    (stack-types
                      (foldr (lambda (el r)
                               (if (and (pair? (cadr el))
                                        (eq? (caadr el) 'lambda)
                                        (not (member (car el) mvars)))
                                 (cons CTX_CLO r)    ;; Non mutable and lambda, keep the type
                                 (cons CTX_MOBJ r)))
                             '()
                             (cadr ast)))
                    (mctx (gen-mutable cgc nctx ids))
                    (zctx (make-ctx (append (reverse stack-types) (list-tail (ctx-stack mctx) (length stack-types)))
                                    (ctx-env mctx)
                                    (ctx-nb-args mctx))))

              (jump-to-version cgc lazy-values zctx)))))))

;; Mov values to their locations (letrec bind)
(define (gen-letrec-binds cgc ctx all-ids)

  (define (gen-letrec-binds-h cgc ctx ids from to)
    (if (null? ids)
      ;; No more id, update rsp and return new ctx
      (begin (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (length all-ids))))
             (ctx-pop-nb ctx (length all-ids)))
      ;; Bind id
      (let* ((nctx (ctx-move ctx from to #f)))
        ;; Get val and mov to location
        (x86-codegen-letrec-bind cgc from to)
        (gen-letrec-binds-h cgc nctx (cdr ids) (+ from 1) (+ to 1)))))

  ;; Initial call
  (gen-letrec-binds-h cgc ctx all-ids 0 (length all-ids)))

;;
;; Make lazy code from LET
;;
(define (mlc-let ast succ)
  (let* ((ids (map car (cadr ast)))
         (values (map cadr (cadr ast)))
         ;; 2 - Update env, ctx, gen mutable and jump to bodies
         (lazy-let-in
            (make-lazy-code
               (lambda (cgc ctx)
                  (let* ((mvars (mutable-vars ast ids))
                         (start (- (length (ctx-stack ctx)) (length ids) 1))
                         (env (build-env mvars ids start (ctx-env ctx)))
                         (nctx (make-ctx (ctx-stack ctx) env (ctx-nb-args ctx)))
                         ;; Gen mutable vars
                         (mctx (gen-mutable cgc nctx mvars)))
                     ;; Jump to first body
                     (jump-to-version cgc succ mctx))))))
      ;; 1 - Gen all bound values
      (gen-ast-l values lazy-let-in)))

;;-----------------------------------------------------------------------------
;; SPECIAL & PRIMITIVES

;;
;; Make lazy code from special id $$print-flonum
;;
(define (mlc-printflonum ast succ)
  (let ((spec
          (make-lazy-code
            (lambda (cgc ctx)
              (x86-pop cgc (x86-rax))
              ;; NOTE: This uses Gambit function to print a flonum (because LC uses the same encoding)
              (gen-print-msg cgc (x86-rax) #f #f)
              (x86-push cgc (x86-imm-int ENCODING_VOID))
              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID))))))
    (gen-ast (cadr ast) spec)))

;;
;; Make lazy code from SPECIAL FORM
;;
(define (mlc-special ast succ)
  (cond ((eq? (car ast) 'breakpoint)
           (make-lazy-code
             (lambda (cgc ctx)
               (gen-breakpoint cgc)
               (x86-codegen-void cgc)
               (jump-to-version cgc succ (ctx-push ctx CTX_VOID)))))
        ((eq? (car ast) 'list)
           (let ((lazy-list
                   (make-lazy-code
                     (lambda (cgc ctx)
                       (let ((label-list-loop (asm-make-label #f (new-sym 'list-loop)))
                             (label-list-end  (asm-make-label #f (new-sym 'list-end))))
                         ;; Remainging length in rdi
                         (x86-mov cgc (x86-rdi) (x86-imm-int (length (cdr ast))))
                         ;; cdr on top of stack
                         (x86-push cgc (x86-imm-int (obj-encoding '())))
                         ;; LOOP
                         (x86-label cgc label-list-loop)
                         (x86-cmp cgc (x86-rdi) (x86-imm-int 0))
                         (x86-je cgc label-list-end)

                            (gen-allocation cgc #f STAG_PAIR 3)
                            (x86-pop cgc (x86-rbx)) ;; pop cdr
                            (x86-pop cgc (x86-rdx)) ;; pop car
                            (x86-mov cgc (x86-mem  8 alloc-ptr) (x86-rdx))
                            (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rbx))
                            (x86-mov cgc (x86-rbx) (x86-imm-int (mem-header 3 STAG_PAIR)))
                            (x86-mov cgc (x86-mem  0 alloc-ptr) (x86-rbx))
                            (x86-lea cgc (x86-rbx) (x86-mem TAG_MEMOBJ alloc-ptr))
                            (x86-push cgc (x86-rbx))
                            (x86-sub cgc (x86-rdi) (x86-imm-int 1))
                            (x86-jmp cgc label-list-loop)

                         (x86-label cgc label-list-end)
                         (jump-to-version cgc
                                          succ
                                          (ctx-push (ctx-pop-nb ctx (length (cdr ast)))
                                                    (if (null? (cdr ast))
                                                        CTX_NULL
                                                        CTX_PAI))))))))
              ;; list operands
              (gen-ast-l (cdr ast) lazy-list)))))



;;
;; Make lazy code from SPECIAL FORM (inlined specials)
;;
(define (mlc-primitive ast succ)

  ;; Adjust args for some primitives
  (cond ((and (eq? (car ast) 'write-char)
              (= (length ast) 2))
          (set! ast (append ast '((current-output-port))))))

  ;; Assert nb args primitive
  (assert-p-nbargs ast)

  ;; Manage fake implementation. NOTE: remove when all implemented
  ;; TODO: implement 'char=?'
  (cond ((eq? (car ast) 'char=?)
            (gen-ast (cons 'eq? (cdr ast)) succ))
        (else
          (let* ((special (car ast))
                 (lazy-special
                   (cond ;; EXIT
                         ((eq? special 'exit)
                            (get-lazy-error ""))
                         ;; CONS
                         ((eq? special 'cons) (mlc-pair succ))
                         ;; NOT
                         ((eq? special 'not)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-not cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))
                         ;; NULL?
                         ((eq? special 'null?)
                           (let ((lazy-null?
                                   (make-lazy-code
                                     (lambda (cgc ctx)

                                       ;; If 'opt-stats' option, then inc tests slot
                                       (if opt-stats
                                         (gen-inc-slot cgc 'tests))

                                       (let ((label-done (asm-make-label cgc (new-sym 'done))))
                                         (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                                         (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                         (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                         (x86-je  cgc label-done)
                                         (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                         (x86-label cgc label-done)
                                         (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                         (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))))
                             (make-lazy-code
                               (lambda (cgc ctx)
                                 (let ((type (car (ctx-stack ctx))))
                                   (cond ((eq? type CTX_NULL)
                                            (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                            (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                            (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))
                                         ((eq? type CTX_UNK)
                                            (jump-to-version cgc lazy-null? ctx))
                                         (else
                                            (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                            (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                            (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))))))

                         ;; EQ?
                         ((eq? special 'eq?)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-eq? cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_BOOL)))))
                         ;; CAR & CDR
                         ((member special '(car cdr))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-car/cdr cgc special)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_UNK)))))
                         ;; SET-CAR! & SET-CDR!
                         ((member special '(set-car! set-cdr!))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-scar/scdr cgc special)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_VOID)))))
                         ;; CURRENT-INPUT-PORT / CURRENT-OUTPUT-PORT
                         ((member special '(current-input-port current-output-port))
                           (make-lazy-code
                             (lambda (cgc ctx)
                               (x86-codegen-current-io-port cgc special)
                               (jump-to-version cgc succ (ctx-push ctx
                                                                   (if (eq? special 'current-output-port)
                                                                       CTX_OPORT
                                                                       CTX_IPORT))))))
                         ;; CLOSE-INPUT-PORT / CLOSE-OUTPUT-PORT
                         ((member special '(close-output-port close-input-port))
                           (make-lazy-code
                             (lambda (cgc ctx)
                               (x86-codegen-close-io-port cgc)
                               (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID)))))
                         ;; OPEN-INPUT-FILE / OPEN-OUTPUT-FILE
                         ((member special '(open-output-file open-input-file))
                           (make-lazy-code
                             (lambda (cgc ctx)
                               (x86-codegen-open-io-file cgc special)
                               (jump-to-version cgc succ (ctx-push (ctx-pop ctx)
                                                                   (if (eq? special 'open-input-file)
                                                                       CTX_IPORT
                                                                       CTX_OPORT))))))
                         ;; EOF-OBJECT?
                         ((eq? special 'eof-object?)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-eof? cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))
                         ;; READ-CHAR
                         ((eq? special 'read-char)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-read-char cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_CHAR)))))
                         ;; WRITE-CHAR
                         ((eq? special 'write-char)
                            (make-lazy-code
                              (lambda (cgc ctx)
                                (x86-codegen-write-char cgc)
                                (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_VOID)))))
                         ;; CHAR<->INTEGER
                         ((member special '(char->integer integer->char))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-ch<->int cgc special)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx)
                                                                  (if (eq? special 'char->integer)
                                                                      CTX_NUM
                                                                      CTX_CHAR))))))
                         ;; MAKE-STRING
                         ((eq? special 'make-string)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let ((init-value? (= (length (cdr ast)) 2)))
                                (x86-codegen-make-string cgc init-value?)
                                (jump-to-version cgc succ (ctx-push (if init-value?
                                                                        (ctx-pop-nb ctx 2)
                                                                        (ctx-pop ctx))
                                                                    CTX_STR))))))
                         ;; MAKE-VECTOR
                         ((eq? special 'make-vector)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let ((init-value? (= (length (cdr ast)) 2)))
                                (x86-codegen-make-vector cgc (= (length (cdr ast)) 2))
                                (jump-to-version cgc succ (ctx-push (if init-value?
                                                                       (ctx-pop-nb ctx 2)
                                                                       (ctx-pop ctx))
                                                                    CTX_VECT))))))
                         ;; STRING->SYMBOL
                         ((eq? special 'string->symbol)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-str->sym cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_SYM)))))
                         ;; SYMBOL->STRING
                         ((eq? special 'symbol->string)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-sym->str cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_STR)))))
                         ;; VECTOR-LENGTH & STRING-LENGTH
                         ((member special '(vector-length string-length))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-vec/str-length cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_NUM)))))
                         ;; VECTOR-REF
                         ((eq? special 'vector-ref)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-vector-ref cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_UNK)))))
                         ;; STRING-REF
                         ((eq? special 'string-ref)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-string-ref cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_CHAR)))))
                         ;; VECTOR-SET!
                         ((eq? special 'vector-set!)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-vector-set! cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 3) CTX_VOID)))))
                         ;; VECTOR-SET!
                         ((eq? special 'string-set!)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (x86-codegen-string-set! cgc)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 3) CTX_VOID)))))
                         ;; OTHERS
                         (else (error "NYI")))))

            (let* ((primitive (assoc (car ast) primitives))
                   ;; Get list of types required by this primitive
                   (types (cdr (assoc (length (cdr ast))
                                      (cadddr primitive)))))
              (assert (= (length types)
                         (length (cdr ast)))
                      "Compiler: primitive error")
              ;; Build args lazy object chain (with type checks)
              (check-types types (cdr ast) lazy-special ast))))))

;; Build lazy objects chain of 'args' list
;; and insert type check for corresponding 'types'
(define (check-types types args succ ast)
  (if (null? types)
     succ
     (let ((next-arg (check-types (cdr types) (cdr args) succ ast)))
        (if (eq? (car types) '*)
           (gen-ast (car args) next-arg)
           (gen-ast (car args)
                    (gen-fatal-type-test (car types) 0 next-arg ast))))))

;;-----------------------------------------------------------------------------
;; Conditionals

;;
;; Make lazy code from IF
;;
(define (mlc-if ast succ)
  (let* ((lazy-code0
           (gen-ast (cadddr ast) succ))
         (lazy-code1
           (gen-ast (caddr ast) succ))
         (lazy-code-test
           (make-lazy-code
             (lambda (cgc ctx)
               (let* ((ctx0
                        (ctx-pop ctx))

                      (ctx1
                        ctx0)

                      (label-jump
                        (asm-make-label
                          cgc
                          (new-sym 'patchable_jump)))

                      (stub-labels
                        (add-callback
                          cgc
                          1
                          (let ((prev-action #f))
                            (lambda (ret-addr selector)
                              (let ((stub-addr
                                      (- ret-addr 5 2))
                                    (jump-addr
                                      (asm-label-pos label-jump)))

                                (if opt-verbose-jit
                                    (begin
                                      (println ">>> selector= " selector)
                                      (println ">>> prev-action= " prev-action)))

                                (if (not prev-action)

                                    (begin

                                      (set! prev-action 'no-swap)

                                      (if (= selector 1)

                                          ;; overwrite unconditional jump
                                          (gen-version
                                            (+ jump-addr 6)
                                            lazy-code1
                                            ctx1)

                                          (if (= (+ jump-addr 6 5) code-alloc)

                                              (begin

                                                (if opt-verbose-jit (println ">>> swapping-branches"))

                                                (set! prev-action 'swap)

                                                ;; invert jump direction
                                                (put-u8 (+ jump-addr 1)
                                                        (fxxor 1 (get-u8 (+ jump-addr 1))))

                                                ;; make conditional jump to stub
                                                (patch-jump jump-addr stub-addr)

                                                ;; overwrite unconditional jump
                                                (gen-version
                                                  (+ jump-addr 6)
                                                  lazy-code0
                                                  ctx0))

                                              ;; make conditional jump to new version
                                              (gen-version
                                                jump-addr
                                                lazy-code0
                                                ctx0))))

                                    (begin

                                      ;; one branch has already been patched

                                      ;; reclaim the stub
                                      (release-still-vector
                                        (get-scmobj ret-addr))
                                      (stub-reclaim stub-addr)

                                      (if (= selector 0)

                                          (gen-version
                                            (if (eq? prev-action 'swap)
                                                (+ jump-addr 6)
                                                jump-addr)
                                            lazy-code0
                                            ctx0)

                                          (gen-version
                                            (if (eq? prev-action 'swap)
                                                jump-addr
                                                (+ jump-addr 6))
                                            lazy-code1
                                            ctx1))))))))))

                 (x86-pop cgc (x86-rax))
                 (x86-cmp cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                 (x86-label cgc label-jump)
                 (x86-je  cgc (list-ref stub-labels 0))
                 (x86-jmp cgc (list-ref stub-labels 1)))))))
    (gen-ast
      (cadr ast)
      lazy-code-test)))

;;-----------------------------------------------------------------------------
;;
;; Make lazy code from DO
;;
;; NOTE: optimization: if <step> is <variable> it is useless to compile
;;       and move in stack, we can use the value of previous iteration
;;
;; Lazy objects chain :
;;
;; +-------+  +---------+  +------+  +----------+  +------------+  +-----+  +------+
;; | inits |->| ctx-add |->| test |->| dispatch |->| test-exprs |->| end |->| succ |-> ...
;; +-------+  +---------+  +------+  +----------+  +------------+  +-----+  +------+
;;                            ^           |
;;                            |           V
;;                            |       +--------+
;;                            |       | bodies |
;;                            |       +--------+
;;                            |           |
;;                            |           V
;;                         +------+   +-------+
;;                         | bind |<--| steps |
;;                         +------+   +-------+
(define (mlc-do ast succ)

  ;; Get list of steps
  (define (get-steps variables)
    (if (null? variables)
      '()
      (let ((variable (car variables)))
        (if (null? (cddr variable))
          ;; No step, use variable
          (cons (car variable)   (get-steps (cdr variables)))
          ;; Step exists, add step
          (cons (caddr variable) (get-steps (cdr variables)))))))

  (let* (;; DO components
         (test       (car (caddr ast)))
         (test-exprs (if (null? (cdr (caddr ast)))
                         '(#f)
                         (cdr (caddr ast))))
         (variables  (map car (cadr ast)))
         (inits      (map cadr (cadr ast)))
         (steps      (get-steps (cadr ast)))
         (bodies     (cdddr ast))
         (mvars      (mutable-vars ast variables))
         ;; LAZY-END
         (lazy-end
           ;; Last object. Executed if test evaluates to true
           ;; Update ctx and rsp
           (make-lazy-code
             (lambda (cgc ctx)

               (let* ((nctx (ctx-move ctx 0 (- (+ (length test-exprs) (length variables)) 1)))
                      (mctx (ctx-pop-nb nctx (- (+ (length test-exprs) (length variables)) 1)))
                      (env (list-tail (ctx-env mctx) (length variables)))
                      (ctx (make-ctx (ctx-stack mctx) env (ctx-nb-args mctx))))

                (x86-pop cgc (x86-rax))
                (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (+ (length variables) (length test-exprs) -1))))
                (x86-push cgc (x86-rax))

                (jump-to-version cgc succ ctx)))))
         ;; LAZY-TEST
         (lazy-test #f) ;; do test
         ;; LAZY-BIND
         (lazy-bind
           (make-lazy-code
             (lambda (cgc ctx)
               ;; Update variables
               (do ((it   variables (cdr it))
                    (from (- (* 8 (length variables)) 8) (- from 8))
                    (to   (- (* 8 (+ (length variables) (length variables) (length bodies))) 8) (- to 8)))
                   ((null? it) #f)
                   (x86-mov cgc (x86-rax) (x86-mem from (x86-rsp)))
                   (if (member (car it) mvars)
                     (begin (x86-mov cgc (x86-rbx) (x86-mem to (x86-rsp))) ;; mvar box
                            (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)) (x86-rax)))
                     (x86-mov cgc (x86-mem to (x86-rsp)) (x86-rax))))

               ;; Update rsp
               (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (+ (length variables) (length bodies)))))

               ;; Update ctx
               (let* (;; Mov stack types
                    (lctx (ctx-mov-nb ctx
                                      (length variables)
                                      0
                                      (+ (length variables) (length bodies))))
                    ;; Keep same env
                    (env (ctx-env ctx))
                    ;; Remove pushed values
                    (mctx (ctx-pop-nb (make-ctx (ctx-stack lctx)
                                                env
                                                (ctx-nb-args lctx))
                                      (+ (length variables) (length bodies)))))

               (jump-to-version cgc lazy-test mctx)))))
         ;; LAZY-STEP
         (lazy-steps
           (gen-ast-l steps lazy-bind))
         ;; LAZY-BODY
         (lazy-body
           (if (null? bodies)
              lazy-steps ;; Jump directly to lazy-steps if no body
              (gen-ast-l bodies lazy-steps)))
         ;; LAZY-TEST-EXPRS
         (lazy-test-exprs (gen-ast-l test-exprs lazy-end))
         ;; LAZY-DISPATCH: Read result of test and jump to test-exors if #t or body if #f
         (lazy-dispatch (get-lazy-dispatch lazy-test-exprs lazy-body #t #f))
         ;; LAZY-ADD-CTX
         (lazy-add-ctx
           ;; Add variables to env and gen mutable vars
           (make-lazy-code
             (lambda (cgc ctx)
               (let* ((start (- (length (ctx-stack ctx)) (length variables) 1))
                      (env  (build-env mvars variables start (ctx-env ctx)))
                      (nctx (make-ctx (ctx-stack ctx) env (ctx-nb-args ctx)))
                      ;; Gen mutable vars
                      (mctx (gen-mutable cgc nctx mvars)))
                 ;; Jump to test with new ctx
                 (jump-to-version cgc lazy-test mctx))))))

    ;; Lazy-dispatch exists then generate test ast
    (set! lazy-test (gen-ast test lazy-dispatch))

    ;; Return first init lazy object
    (gen-ast-l inits lazy-add-ctx)))

;; Create a new lazy code object.
;; Takes the value from stack and cmp to #f
;; If == #f jump to lazy-fail
;; If != #f jump to lazy-success
(define (get-lazy-dispatch lazy-success lazy-fail from-stack? cmp-val)

    (make-lazy-code
       (lambda (cgc ctx)
         (let* ((ctx-out (if from-stack?
                            (ctx-pop ctx)
                            ctx))
                (label-jump (asm-make-label cgc (new-sym 'patchable_jump)))
                (stub-labels
                      (add-callback cgc 1
                        (let ((prev-action #f))

                          (lambda (ret-addr selector)
                            (let ((stub-addr (- ret-addr 5 2))
                                  (jump-addr (asm-label-pos label-jump)))

                              (if opt-verbose-jit
                                  (begin
                                    (println ">>> selector= " selector)
                                    (println ">>> prev-action= " prev-action)))

                              (if (not prev-action)

                                  (begin (set! prev-action 'no-swap)
                                         (if (= selector 1)

                                            ;; overwrite unconditional jump
                                            (gen-version (+ jump-addr 6) lazy-success ctx-out)

                                            (if (= (+ jump-addr 6 5) code-alloc)

                                              (begin (if opt-verbose-jit (println ">>> swapping-branches"))
                                                     (set! prev-action 'swap)
                                                     ;; invert jump direction
                                                     (put-u8 (+ jump-addr 1) (fxxor 1 (get-u8 (+ jump-addr 1))))
                                                     ;; make conditional jump to stub
                                                     (patch-jump jump-addr stub-addr)
                                                     ;; overwrite unconditional jump
                                                     (gen-version
                                                     (+ jump-addr 6)
                                                     lazy-fail
                                                     ctx-out))

                                              ;; make conditional jump to new version
                                              (gen-version jump-addr lazy-fail ctx-out))))

                                  (begin ;; one branch has already been patched
                                         ;; reclaim the stub
                                         (release-still-vector (get-scmobj ret-addr))
                                         (stub-reclaim stub-addr)
                                         (if (= selector 0)
                                            (gen-version (if (eq? prev-action 'swap) (+ jump-addr 6) jump-addr) lazy-fail ctx-out)
                                            (gen-version (if (eq? prev-action 'swap) jump-addr (+ jump-addr 6)) lazy-success ctx-out))))))))))

         (if from-stack?
          (x86-pop cgc (x86-rax)))
         (x86-cmp cgc (x86-rax) (x86-imm-int (obj-encoding cmp-val)))
         (x86-label cgc label-jump)
         (x86-je cgc (list-ref stub-labels 0))
         (x86-jmp cgc (list-ref stub-labels 1))))))

;;-----------------------------------------------------------------------------
;; APPLY & CALL

;;
;; Make lazy code from APPLY
;;
(define (mlc-apply ast succ)

  (let (;; Get closure and gen call sequence
        (lazy-call
          (make-lazy-code
            (lambda (cgc ctx)
              ;; GEN CALL SEQ
              (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
              (gen-call-sequence cgc #f #f)))))

    ;; First object of the chain, reserve a slot for the continuation
    (make-lazy-code
      (lambda (cgc ctx)
         (let* (;; Lazy code object to build continuation and move it to stack slot
                (lazy-build-continuation (get-lazy-continuation-builder (cadr ast) succ lazy-call '() ctx #t ast))
                ;; Remove apply op and lst, push args from lst and push closure
                (lazy-move-args
                  (make-lazy-code
                    (lambda (cgc ctx)
                      ;; Remove lst and op from stack
                      (x86-pop cgc (x86-rbx)) ;; lst
                      (x86-pop cgc (x86-rax)) ;; op
                      ;; Read and push all args from lst until we reach '()
                      (let ((label-end  (asm-make-label #f (new-sym 'apply-args-end)))
                            (label-loop (asm-make-label #f (new-sym 'apply-args-loop))))
                        ;; RDI contains the number of arguments
                        (x86-mov cgc (x86-rdi) (x86-imm-int 0))
                        (x86-label cgc label-loop)
                        ;; If current el is null, then jump to end
                        (x86-cmp cgc (x86-rbx) (x86-imm-int (obj-encoding '())))
                        (x86-je cgc label-end)
                          ;; Else, push arg and update RDI
                          (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)))           ;; Push car
                          (x86-mov cgc (x86-rbx) (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx))) ;; Get cdr for next iteration
                          (x86-inc cgc (x86-rdi))  ;; inc args number
                          (x86-jmp cgc label-loop) ;; next iteration
                        ;; All args are pushed
                        (x86-label cgc label-end))
                      ;; Encode nb args
                      (x86-shl cgc (x86-rdi) (x86-imm-int 2))
                      ;; Push closure
                      (x86-push cgc (x86-rax))
                      ;; Jump to lazy-build-continuation without ctx
                      ;; This works because lazy-build-continuation and its successor lazy-call do not use ctx.
                      (jump-to-version cgc lazy-build-continuation (make-ctx '() '() -1)))))
                  ;; Push args list of apply
                  (lazy-args-list (gen-ast (caddr ast) lazy-move-args)) ;; TODO: check that caddr is a pair ?
                  ;; Push function of apply
                  (lazy-fun (check-types (list CTX_CLO) (list (cadr ast)) lazy-args-list ast)))

              (x86-push cgc (x86-imm-int (obj-encoding #f))) ;; Reserve stack slot
              (jump-to-version cgc lazy-fun (ctx-push ctx CTX_RETAD)))))))

;;
;; Make lazy code from CALL EXPR
;;
(define (mlc-call ast succ)

  (let* (;; Tail call if successor's flags set contains 'ret flag
         (tail (member 'ret (lazy-code-flags succ)))
         ;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (get-lazy-error (ERR_TYPE_EXPECTED CTX_CLO)))
         ;; Lazy call
         (lazy-call (make-lazy-code (lambda (cgc ctx)

                                        ;; Call ctx in rdx
                                        (let* ((call-stack (if tail
                                                              (append (list-head (ctx-stack ctx) (+ 1 (length args))) (list CTX_RETAD))
                                                              (list-head (ctx-stack ctx) (+ (length args) 2))))
                                               (call-ctx (make-ctx call-stack '() -1)))

                                        (if tail
                                          (tail-shift cgc
                                                      ;; Nb slots to shift
                                                      (+ (length args) 1) ;; +1 closure
                                                      ;; Initial from slot
                                                      (length args)
                                                      ;; Initial to slot
                                                      (- (length (ctx-stack ctx)) 2)))

                                        ;; If count calls compiler opt
                                        (if (eq? (car ast) opt-count-calls)
                                           (gen-inc-slot cgc 'calls))

                                        (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))

                                        ;; Gen call sequence with closure in RAX
                                        (let ((nb-unk (count call-stack (lambda (n) (eq? n CTX_UNK)))))
                                          (if (and opt-entry-points (= nb-unk (length args)))
                                              (begin (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding (length args))))
                                                     (gen-call-sequence cgc #f #f))
                                              (gen-call-sequence cgc call-ctx (length (cdr ast)))))))))
         ;; Lazy code object to build the continuation
         (lazy-tail-operator (check-types (list CTX_CLO) (list (car ast)) lazy-call ast)))

    (if tail
        (if (> (length args) 0)
            ;; If args, then compile args
            (gen-ast-l args lazy-tail-operator)
            ;; Else, compile call
            lazy-tail-operator)
        ;; First object of the chain
        (make-lazy-code
          (lambda (cgc ctx)
            (let* (;; Lazy code object to build continuation and move it to stack slot
                   (lazy-build-continuation (get-lazy-continuation-builder (car ast) succ lazy-call args ctx #f ast))
                   ;; Lazy code object to push operator of the call
                   (lazy-operator (check-types (list CTX_CLO) (list (car ast)) lazy-build-continuation ast)))

              (x86-push cgc (x86-imm-int (obj-encoding #f))) ;; Reserve a slot for continuation
              (jump-to-version cgc
                               (gen-ast-l args lazy-operator) ;; Compile args and jump to operator
                               (ctx-push ctx CTX_RETAD))))))))

(define (get-lazy-continuation-builder op lazy-succ lazy-call args continuation-ctx from-apply? ast)

  (if opt-return-points
    (get-lazy-continuation-builder-cr  op lazy-succ lazy-call args continuation-ctx from-apply? ast)
    (get-lazy-continuation-builder-nor op lazy-succ lazy-call args continuation-ctx from-apply?)))

(define (get-lazy-continuation-builder-cr op lazy-succ lazy-call args continuation-ctx from-apply? ast)

  ;; Create stub and push ret addr
  (make-lazy-code-cont
     (lambda (cgc ctx)
       (let* (;; Lazy-continuation, push returned value
              (lazy-continuation
                (make-lazy-code
                  (lambda (cgc ctx)
                    (x86-push cgc (x86-rax))
                    (jump-to-version cgc lazy-succ ctx))))
              ;; Continuation stub
              (stub-labels (add-cont-callback cgc
                                              0
                                              (lambda (ret-addr selector type table)

                                                (let ((continuation-ctx (ctx-push continuation-ctx type)))
                                                  (gen-version-continuation-cr lazy-continuation
                                                                               continuation-ctx
                                                                               type
                                                                               table)))))
              ;; CRtable
              (crtable-key (get-crtable-key ast ctx))
              (stub-addr (vector-ref (list-ref stub-labels 0) 1))
              (crtable (get-crtable ast crtable-key stub-addr))
              (crtable-loc (- (obj-encoding crtable) 1)))

         (x86-mov cgc (x86-rax) (x86-imm-int crtable-loc))

         (if from-apply?
           (begin (x86-shl cgc (x86-rdi) (x86-imm-int 1)) ;; Rdi contains encoded number of args. Shiftl 1 to left to get nbargs*8
                  (x86-mov cgc (x86-mem 8 (x86-rsp) (x86-rdi)) (x86-rax)) ;; Mov to continuation stack slot [rsp+rdi+8] (rsp + nbArgs*8 + 8)
                  (x86-shr cgc (x86-rdi) (x86-imm-int 1))) ;; Restore encoded number of args
           (x86-mov cgc (x86-mem (* 8 (+ 1 (length args))) (x86-rsp)) (x86-rax))) ;; Move continuation value to the continuation stack slot
         (jump-to-version cgc lazy-call ctx)))))

;; Build continuation stub and load stub address to the continuation slot
(define (get-lazy-continuation-builder-nor op lazy-succ lazy-call args continuation-ctx from-apply?)
  ;; Create stub and push ret addr
  (make-lazy-code-cont
    (lambda (cgc ctx)
      (let* (;; Flag in stub : is the continuation already generated ?
             (gen-flag #f)
             ;; Label for return address loading
             (load-ret-label (asm-make-label cgc (new-sym 'load-ret-addr)))
             ;; Lazy continuation, push returned value
             (lazy-continuation
                (make-lazy-code
                   (lambda (cgc ctx)
                      (x86-push cgc (x86-rax))
                      (jump-to-version cgc lazy-succ (ctx-push ctx CTX_UNK)))))
             ;; Continuation stub
             (stub-labels (add-callback cgc
                                        0
                                        (lambda (ret-addr selector)
                                            (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag to continuation addr
                                               (set! gen-flag (gen-version-continuation load-ret-label
                                                                                        lazy-continuation
                                                                                        continuation-ctx))) ;; Remove operator, args, and continuation from stack
                                            gen-flag))))
        ;; Return address (continuation label)
        (x86-label cgc load-ret-label)
        (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1)))

        (if from-apply?
          (begin (x86-shl cgc (x86-rdi) (x86-imm-int 1)) ;; Rdi contains encoded number of args. Shiftl 1 to left to get nbargs*8
                 (x86-mov cgc (x86-mem 8 (x86-rsp) (x86-rdi)) (x86-rax)) ;; Mov to continuation stack slot [rsp+rdi+8] (rsp + nbArgs*8 + 8)
                 (x86-shr cgc (x86-rdi) (x86-imm-int 1))) ;; Restore encoded number of args
          (x86-mov cgc (x86-mem (* 8 (+ 1 (length args))) (x86-rsp)) (x86-rax))) ;; Move continuation value to the continuation stack slot
        (jump-to-version cgc lazy-call ctx)))))

;; Gen generic call sequence (call instructions) with call closure in RAX
(define (gen-generic-call-sequence cgc nb-args)
  ;; 1 - If nb-args given, put encoded nb in rdi
  ;; NOTE: only if nb-args is not #f to handle 'apply' (apply always writes nb-args in rdi, don't overwrite it!)
  (if nb-args
    (x86-mov cgc (x86-rdi) (x86-imm-int (* 4 nb-args))))
  ;; 2 - Get generic entry point
  (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
  (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rax))))

;; Gen call sequence (call instructions) with call closure in RAX
(define (gen-call-sequence cgc call-ctx nb-args) ;; Use multiple entry points?

    (if opt-entry-points

      (if (not nb-args)

          ;; It is a call from apply, then use generic entry point
          (gen-generic-call-sequence cgc #f)

          ;; If we use multiple entry points then:
          (let* ((idx (get-closure-index call-ctx)))
            (if idx ;; This condition is not on the previous 'if' because we don't want to create a table entry if we don't use multiple entry points

              (let ((cct-offset (* 8 (+ 2 idx))))
                  ;; 1 - Put ctx in r11
                  (x86-mov cgc (x86-r11) (x86-imm-int (ctx->still-ref call-ctx)))
                  ;; 2- Get cc-table
                  (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
                  ;; 3 - If opt-max-versions is not #f, a generic version could be called. So we need to give nb-args
                  ;; NOTE: only if nb-args is not #f to handle 'apply' (apply always writes nb-args in rdi, don't overwrite it!)
                  (if (and opt-max-versions nb-args)
                      (x86-mov cgc (x86-rdi) (x86-imm-int (* 4 nb-args))))
                  ;; 4 - Get entry point in cc-table
                  (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax))))

              ;; Table is full, gen generic call
              (gen-generic-call-sequence cgc nb-args))))

      ;; Do not use multiple entry points
      (if (not nb-args)
          (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
          (begin (x86-mov cgc (x86-rdi) (x86-imm-int (* 4 nb-args)))
                 (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))))

    ;; Jump to entry point
    (x86-jmp cgc (x86-rax)))

;;-----------------------------------------------------------------------------
;; Operators

;;
;; Make lazy code from BINARY OPERATOR
;;
(define (mlc-op-bin ast succ op)
  (let ((opnds (cdr ast)))
    (if (not (= (length opnds) 2))
      ;; != 2 operands, error
      (get-lazy-error ERR_WRONG_NUM_ARGS)
      ;; == 2 operands
      (let* (;; Overflow stub
             (overflow-labels (add-callback #f 0 (lambda (ret-addr selector)
                                                (error ERR_ARR_OVERFLOW))))

             (lazy-op
               (make-lazy-code
                 (lambda (cgc ctx)
                   (x86-pop cgc (x86-rbx)) ;; Pop right
                   (x86-pop cgc (x86-rax)) ;; Pop left
                   (x86-sar cgc (x86-rax) (x86-imm-int 2))
                   (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                   (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
                   (x86-je  cgc (get-label-error ERR_DIVIDE_ZERO)) ;; Check '/0'
                   (x86-cqo cgc)
                   (x86-idiv cgc (x86-rbx))
                   (cond ((eq? op 'quotient)
                           (x86-shl cgc (x86-rax) (x86-imm-int 2))
                           (x86-push cgc (x86-rax)))
                         ((eq? op 'remainder)
                           (x86-shl cgc (x86-rdx) (x86-imm-int 2))
                           (x86-push cgc (x86-rdx)))
                         ((eq? op 'modulo)
                           (x86-mov cgc (x86-rax) (x86-rdx)) ;; (a%b) in rax, b in rbx
                           (x86-add cgc (x86-rax) (x86-rbx)) ;; (a%b + b) in rax
                           (x86-cqo cgc)
                           (x86-idiv cgc (x86-rbx))
                           (x86-shl cgc (x86-rdx) (x86-imm-int 2))
                           (x86-push cgc (x86-rdx))))
                   (jump-to-version cgc
                                    succ
                                    (ctx-push (ctx-pop-nb ctx 2) CTX_NUM))))))
         ;; Check operands type
         (check-types (list CTX_NUM CTX_NUM)
                      (list (car opnds) (cadr opnds))
                      lazy-op
                      ast)))))

;;
;; Make lazy code from N-ARY OPERATOR
;;
;; TODO: tester les nombre de tests de types pour chaque combinaison
(define (mlc-op-n ast succ op)
  (if (member op '(< > <= >= =))
    (mlc-op-n-cmp ast succ op)
    (mlc-op-n-num ast succ op)))

;;
;; Make lazy code from N-ARY COMPARISON OPERATOR
;;
(define (mlc-op-n-cmp ast succ op)

   ;; Create final lazy object. This object clean stack and push 'res'
   (define (get-lazy-final res)
      (make-lazy-code
         (lambda (cgc ctx)
            (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- (length ast) 1))))
            (x86-push cgc (x86-imm-int (obj-encoding res)))
            (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx (- (length ast) 1))
                                                CTX_BOOL)))))

   ;; Gen false stub from jump-label & ctx and return stub label
   (define (get-stub-label label-jump ctx)
      (list-ref (add-callback #f 0 (lambda (ret-addr selector)
                                   (gen-version (asm-label-pos label-jump)
                                                (get-lazy-final #f)
                                                ctx)))
                0))

   ;; Build chain for all operands
   (define (build-chain lidx ridx)
      (if (or (< lidx 0) (< ridx 0))
        ;; All operands compared
        (get-lazy-final #t)
        ;; There is at least 1 comparison to perform
        (build-bincomp (build-chain (- lidx 1) (- ridx 1)) lidx ridx)))

   ;; Gen lazy code objects chain for binary comparison (x op y)
   ;; Build a lco for each node of the type checks tree (with int and float)
   (define (build-bincomp succ lidx ridx)
     (let* (;; Operations lco
           (lazy-ii (get-comp-ii succ lidx ridx))       ;; lco for int int operation
           (lazy-if (get-comp-ff succ lidx ridx #t #f)) ;; lco for int float operation
           (lazy-fi (get-comp-ff succ lidx ridx #f #t)) ;; lco for float int operation
           (lazy-ff (get-comp-ff succ lidx ridx #f #f)) ;; lco for float float operation
           ;; Right branch
           (lazy-yfloat2 (gen-fatal-type-test CTX_FLO ridx lazy-ff ast))
           (lazy-yint2   (gen-dyn-type-test CTX_NUM ridx lazy-fi lazy-yfloat2 ast))
           (lazy-xfloat  (gen-fatal-type-test CTX_FLO lidx lazy-yint2 ast))
           ;; Left branch
           (lazy-yfloat  (gen-fatal-type-test CTX_FLO ridx lazy-if ast))
           (lazy-yint    (gen-dyn-type-test CTX_NUM ridx lazy-ii lazy-yfloat ast))
           ;; Root node
           (lazy-xint    (gen-dyn-type-test CTX_NUM lidx lazy-yint lazy-xfloat ast)))
      lazy-xint))

    ;; Get lazy code object for comparison with int and int
   (define (get-comp-ii succ lidx ridx)
    (make-lazy-code
      (lambda (cgc ctx)
      (let ((label-jump (asm-make-label #f (new-sym 'label-jump)))
            (x86-op (cdr (assoc op `((< . ,x86-jge) (> . ,x86-jle) (<= . ,x86-jg) (>= . ,x86-jl) (= . ,x86-jne))))))
         (x86-mov cgc (x86-rax) (x86-mem (* 8 lidx) (x86-rsp)))
         (x86-cmp cgc (x86-rax) (x86-mem (* 8 ridx) (x86-rsp)))
         (x86-label cgc label-jump)
         (x86-op cgc (get-stub-label label-jump ctx))
         (jump-to-version cgc succ ctx)))))

   ;; Get lazy code object for comparison with float and float, float and int, and int and float
   ;; leftint?  to #t if left operand is an integer
   ;; rightint? to #t if right operand is an integer
   (define (get-comp-ff succ lidx ridx leftint? rightint?)
    (make-lazy-code
      (lambda (cgc ctx)
      (let ((label-jump (asm-make-label #f (new-sym 'label-jump)))
            ;; DO NOT USE jg* and jl* WITH FP VALUES !
            (x86-op (cdr (assoc op `((< . ,x86-jae) (> . ,x86-jbe) (<= . ,x86-ja) (>= . ,x86-jb) (= . ,x86-jne))))))
         (x86-mov cgc (x86-rax) (x86-mem (* 8 lidx) (x86-rsp)))
         (x86-mov cgc (x86-rbx) (x86-mem (* 8 ridx) (x86-rsp)))
         (if leftint?
            ;; Left is integer, the compiler converts it to double precision FP
            (begin (x86-sar cgc (x86-rax) (x86-imm-int 2))  ;; untag integer
                   (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax))) ;; convert to double
            ;; Left is double precision FP
            (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
         (if rightint?
            ;; Right is integer, the compiler converts it to double precision FP
            (begin (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                   (x86-cvtsi2sd cgc (x86-xmm1) (x86-rbx))
                   (x86-comisd cgc (x86-xmm0) (x86-xmm1)))
            ;; Right is double precision FP
            (x86-comisd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx))))
         (x86-label cgc label-jump)
         (x86-op cgc (get-stub-label label-jump ctx))
         (jump-to-version cgc succ ctx)))))

   ;; Push operands and start comparisons
   (gen-ast-l (cdr ast)
              (build-chain (- (length ast) 2)
                           (- (length ast) 3))))

;;
;; Make lazy code from N-ARY ARITHMETIC OPERATOR
;;
(define (mlc-op-n-num ast succ op)

  ;; Build chain for all operands
  (define (build-chain opnds)
    (if (null? opnds)
      succ
      (let* ((lazy-bin-op (build-binop (build-chain (cdr opnds))))
             (lazy-opnd (gen-ast (car opnds) lazy-bin-op)))
        lazy-opnd)))

  ;; Gen lazy code objects chain for binary operation (x op y)
  ;; Build a lco for each node of the type checks tree (with int and float)
  (define (build-binop succ)
    (let* (;; Operations lco
           (lazy-ii
             (if (eq? op '/)
                 (get-op-ff succ #t #t)   ;; If it is the / operator, fall back to float
                 (get-op-ii succ)))       ;; lco for int int operation
           (lazy-if (get-op-ff succ #t #f)) ;; lco for int float operation
           (lazy-fi (get-op-ff succ #f #t)) ;; lco for float int operation
           (lazy-ff (get-op-ff succ #f #f)) ;; lco for float float operation
           ;; Right branch
           (lazy-yfloat2 (gen-fatal-type-test CTX_FLO 0 lazy-ff ast))
           (lazy-yint2   (gen-dyn-type-test CTX_NUM 0 lazy-fi lazy-yfloat2 ast))
           (lazy-xfloat  (gen-fatal-type-test CTX_FLO 1 lazy-yint2 ast))
           ;; Left branch
           (lazy-yfloat  (gen-fatal-type-test CTX_FLO 0 lazy-if ast))
           (lazy-yint    (gen-dyn-type-test CTX_NUM 0 lazy-ii lazy-yfloat ast))
           ;; Root node
           (lazy-xint    (gen-dyn-type-test CTX_NUM 1 lazy-yint lazy-xfloat ast)))
      lazy-xint))

  ;; Get lazy code object for operation with int and int
  (define (get-op-ii succ)
    (let ((labels-overflow (add-callback #f 0 (lambda (ret-addr selector)
                                                (error ERR_ARR_OVERFLOW)))))
        (make-lazy-code
          (lambda (cgc ctx)
            (x86-pop cgc (x86-rax))
            (cond ((eq? op '+) (x86-add cgc (x86-mem 0 (x86-rsp)) (x86-rax)))
                  ((eq? op '-) (x86-sub cgc (x86-mem 0 (x86-rsp)) (x86-rax)))
                  ((eq? op '*) (x86-sar cgc (x86-rax) (x86-imm-int 2))
                               (x86-imul cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                               (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax)))
                  (else (error "NYI" op)))
            (x86-jo cgc (list-ref labels-overflow 0)) ;; NYI overflow
            (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_NUM))))))

  ;; Get lazy code object for operation with float and float, float and int, and int and float
  ;; leftint?  to #t if left operand is an integer
  ;; rightint? to #t if right operand is an integer
  (define (get-op-ff succ leftint? rightint?)
    (make-lazy-code
      (lambda (cgc ctx)
      ;; Alloc result flonum
      (gen-allocation cgc #f STAG_FLONUM 2)

      (let ((x86-op (cdr (assoc op `((+ . ,x86-addsd) (- . ,x86-subsd) (* . ,x86-mulsd) (/ . ,x86-divsd))))))
         (x86-pop cgc (x86-rbx)) ;; right in rbx
         (x86-pop cgc (x86-rax)) ;; left in rax
         (if leftint?
            ;; Left is integer, the compiler converts it to double precision FP
            (begin (x86-sar cgc (x86-rax) (x86-imm-int 2))  ;; untag integer
                   (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax))) ;; convert to double
            ;; Left is double precision FP
            (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
         (if rightint?
            ;; Right is integer, the compiler converts it to double precision FP
            (begin (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                   (x86-cvtsi2sd cgc (x86-xmm1) (x86-rbx))
                   (x86-op cgc (x86-xmm0) (x86-xmm1)))
            ;; Right is double precision FP
            (x86-op cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)))))
      ;; Write header
      (x86-mov cgc (x86-rax) (x86-imm-int (mem-header 2 STAG_FLONUM)))
      (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
      ;; Write number
      (x86-movsd cgc (x86-mem 8 alloc-ptr) (x86-xmm0))
      ;;
      (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
      (x86-push cgc (x86-rax))
      (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_FLO)))))

  (cond ((= (length ast) 1)
           (cond ((eq? op '+) (gen-ast 0 succ))
                 ((eq? op '-) (get-lazy-error ERR_WRONG_NUM_ARGS))
                 ((eq? op '*) (gen-ast 1 succ))
                 (else (error "Unknown operator " op))))
        ((and (= (length ast) 2) (eq? op '-))
           (gen-ast (list '* -1 (cadr ast)) succ))
        ((and (= (length ast) 2) (member op '(< > <= >= =)))
           (gen-ast #t succ))
        (else
           (gen-ast (cadr ast) (build-chain (cddr ast))))))

;;
;; Make lazy code from TYPE TEST
;;
(define (mlc-test ast succ)

  (let ((type (predicate-to-ctxtype (car ast)))
        (stack-idx 0)
        (lazy-success
          (make-lazy-code
            (lambda (cgc ctx)
                (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))
        (lazy-fail
          (make-lazy-code
            (lambda (cgc ctx)
              (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
              (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL))))))

    (gen-ast (cadr ast)
             (gen-dyn-type-test type stack-idx lazy-success lazy-fail ast))))

;;
;; Make lazy code to create pair
;; Create pair with the too values on top of the stack
;;
(define (mlc-pair succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (x86-codegen-pair cgc)
      (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_PAI)))))

;;-----------------------------------------------------------------------------

;; AST RELATED FUNCTIONS

;;-----------------------------------------------------------------------------

;;
;; CC TABLE
;;

;; CC Table (Closure Context Table) :
;; A closure contains a header, the CC Table addr, and all free vars
;; The CC Table contains multiple possible entry points (fixed number) for the procedure
;; Each slot contains initially the address of the procedure stub
;; As soon as a version is generated for a context, the slot is replaced by the generated address
;;
;; EX : closure at initial state
;; +----------------+---------+---------+---------+---------+---------+
;; |Header          |CC Table |Free var |Free var |   ...   |Free var |
;; |(Same as gambit)|addr     |    1    |    2    |         |    n    |
;; +----------------+----|----+---------+---------+---------+---------+
;;                       |
;;      +----------------+
;;      |
;;      v
;; +---------+---------+---------+---------+---------+
;; |Stub addr|Stub addr|Stub addr|   ...   |Stub addr|
;; |         |         |         |         |         |
;; +---------+---------+---------+---------+---------+
;;  index  0  index  1  index  2     ...    index  n
;;
;; EX closure with two existing versions
;; +----------------+---------+---------+---------+---------+---------+
;; |Header          |CC Table |Free var |Free var |   ...   |Free var |
;; |(Same as gambit)|addr     |    1    |    2    |         |    n    |
;; +----------------+----|----+---------+---------+---------+---------+
;;                       |
;;      +----------------+
;;      |
;;      v
;; +---------+---------+---------+---------+---------+
;; |Proc addr|Stub addr|Proc addr|   ...   |Stub addr|
;; |(ctx1)   |         |(ctx5)   |         |         |
;; +---------+---------+---------+---------+---------+
;;  index  0  index  1  index  2     ...    index  n

;;
;; VARIABLE SET
;;

;; Gen code to set a free/local
;; variable is the lookup result which contains id info
;;  ex: variable = '(n . identifier-obj)

;; Free var
(define (gen-set-freevar cgc ctx variable)
   (let ((mutable (identifier-mutable? (cdr variable))))
      (if mutable
        (begin (gen-get-freevar cgc ctx variable 'gen-reg)
               (x86-pop cgc (x86-rbx))
               (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) (x86-rbx))
               ctx)
        (error "Compiler error : set a non mutable free var"))))

;; Local var
(define (gen-set-localvar cgc ctx variable)

   (let ((mutable (identifier-mutable? (cdr variable))))
     (if mutable
        (begin (gen-get-localvar cgc ctx variable 'gen-reg)
               (x86-pop cgc (x86-rbx))
               (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) (x86-rbx))

               ;; Replace ctx type
               (let* ((fs (length (ctx-stack ctx)))
                      (idx (- fs 2 (identifier-offset (cdr variable)))))

                ;; move and reset pos
                (ctx-reset-pos (ctx-move ctx 0 idx #f) (car variable))))

        (error "Compiler error : set a non mutable local var"))))

;; Gen code to set a global var
(define (gen-set-globalvar cgc ctx variable)
   (x86-pop cgc (x86-rax))
   (x86-mov cgc (x86-mem (* 8 (cdr variable)) (x86-r10)) (x86-rax))
   ctx)

;;
;; VARIABLE GET
;;

;; Gen code to get a variable from closure/stack
;; variable is the lookup result which contains id info
;;  ex: variable = '(n . identifier-obj)
;; dest is the destination. possible values are :
;;  'stack : push value
;;  'gen-reg : general register (mov value to rax)
;; raw_value is #t if the value is copied directly from closure
;;              #f if the value is copied from memory (id variable is mutable)

;; Free variable
(define (gen-get-freevar cgc ctx variable dest #!optional (raw_value? #t))

   (let* ((offset (+ (- 16 TAG_MEMOBJ) (* 8 (identifier-offset (cdr variable)))))
          (clo-offset (* 8 (closure-pos ctx)))
          (mutable (identifier-mutable? (cdr variable))))

      ;; Get closure
      (x86-mov cgc (x86-rax) (x86-mem clo-offset (x86-rsp)))

      (if (or raw_value? (not mutable))
        ;; Raw value required (direct copy from closure)
        ;; OR variable is not mutable
        (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem offset (x86-rax))))
              ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax))))
              (else (error "Invalid destination")))
        ;; Real value required and variable is mutable
        (begin (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
               (cond ((eq? dest 'stack) (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                     ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                     (else (error "Invalid destination")))))

      (identifier-stype (cdr variable))))
      ;CTX_UNK))

;; Local variable
(define (gen-get-localvar cgc ctx variable dest #!optional (raw_value? #t))
   (let* ((fs (length (ctx-stack ctx)))
          (pos (- fs 2 (identifier-offset (cdr variable))))
          (mutable (identifier-mutable? (cdr variable))))

      (if (or raw_value? (not mutable))
        ;; Raw value required (direct copy from stack)
        ;; OR variable is not mutable
        (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem (* pos 8) (x86-rsp))))
              ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (* pos 8) (x86-rsp))))
              (else (error "Invalid destination")))
        ;; Real value required and variable is mutable
        (begin (x86-mov cgc (x86-rax) (x86-mem (* pos 8) (x86-rsp)))
               (cond ((eq? dest 'stack)
                        (begin
                               (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))))
                     ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                     (else (error "Invalid destination")))))
      (list-ref (ctx-stack ctx) pos)))

;; Gen code to get a global var
(define (gen-get-globalvar cgc ctx variable dest)

   (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem (* 8 (cdr variable)) (x86-r10))))
         ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (* 8 (cdr variable)) (x86-r10))))
         (else (error "Invalid destination")))

   ;; If this global is a non mutable global, return type else return unknown
   (let ((r (assoc (car variable) gids)))
     (if (and r (cdr r))
        (cdr r)
        CTX_UNK)))

;;
;; FREE VARS
;;

;; Extends env with 'fvars' free vars starting with offset
(define (build-fenv saved-stack saved-env mvars fvars offset)

  (if (null? fvars)
      '()
      (cons (cons (car fvars) (make-identifier 'free
                                               offset
                                               '()
                                               ;; Flags
                                               (let ((res (assoc (car fvars) saved-env)))
                                                  (if (identifier-mutable? (cdr res))
                                                    '(mutable)
                                                    '()))
                                               ;; SType
                                               (let* ((res (assoc (car fvars) saved-env)))
                                                 (if (eq? (identifier-type (cdr res)) 'local)
                                                    ;; From local var
                                                    (let ((idx (- (length saved-stack) 2 (identifier-offset (cdr res)))))
                                                      (list-ref saved-stack idx))
                                                    ;; From free var
                                                    (identifier-stype (cdr res))))))

            (build-fenv saved-stack saved-env mvars (cdr fvars) (+ offset 1)))))

;; Write free vars in closure
(define (gen-free-vars cgc vars ctx offset)
  (if (null? vars)
      '()
      (let* ((var (car vars))
             (res (assoc var (ctx-env ctx))))
         (if res
            (if (eq? (identifier-type (cdr res)) 'free)
               ;; Free var
               (gen-get-freevar  cgc ctx res 'gen-reg)
               ;; Local var
               (gen-get-localvar cgc ctx res 'gen-reg))
            (error "Can't find variable: " var))
         (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))
         (gen-free-vars cgc (cdr vars) ctx (+ offset 8)))))

;; Return all free vars used by the list of ast knowing env 'clo-env'
(define (free-vars-l lst clo-env ctx)
  (if (null? lst)
      '()
      (set-union (free-vars (car lst) clo-env ctx) (free-vars-l (cdr lst) clo-env ctx))))

;; Return all free vars used by ast knowing env 'clo-env'
(define (free-vars ast clo-env ctx)
  (cond ;; Symbol
        ((symbol? ast)
          (if (and (assoc ast (ctx-env ctx))
                   (not (member ast clo-env)))
              (list ast)
              '()))
        ;; Literal
        ((literal? ast) '())
        ;; Pair
        ((pair? ast)
          (let ((op (car ast)))
            (cond ;; If
                  ((eq? op 'if) (set-union (free-vars (cadr ast)   clo-env ctx)   ; cond
                                           (set-union (free-vars (caddr ast)  clo-env ctx)    ; then
                                                      (free-vars (cadddr ast) clo-env ctx)))) ; else
                  ;; Quote
                  ((eq? op 'quote) '())
                  ;; Lambda
                  ((eq? op 'lambda) (free-vars (caddr ast)
                                               (if (list? (cadr ast))
                                                  (append (cadr ast) clo-env)
                                                  (cons (cadr ast) clo-env))
                                               ctx))
                  ;; Call
                  (else (free-vars-l ast clo-env ctx)))))))

;;
;; MUTABLE VARS
;;

;; Return all mutable vars used by the list of ast
(define (mutable-vars-l lst params)
  (if (or (null? lst) (not (pair? lst)))
    '()
    (set-union (mutable-vars (car lst) params) (mutable-vars-l (cdr lst) params))))

;; Return all mutable vars used by ast
(define (mutable-vars ast params)
  (cond ;; Literal
        ((literal? ast) '())
        ;; Pair
        ((pair? ast)
           (let ((op (car ast)))
              (cond ((eq? op 'lambda) (mutable-vars (caddr ast) (set-sub params
                                                                         (if (list? (cadr ast))
                                                                           (cadr ast)
                                                                           (list (cadr ast)))
                                                                         '())))
                    ((eq? op 'set!)
                      (set-union (mutable-vars (caddr ast) params)
                                 (if (member (cadr ast) params)
                                   (list (cadr ast))
                                   '())))
                    (else (mutable-vars-l ast params)))))))

;;
;; OPTIMIZATIONS
;;

;; Shift stack slots for tail call optimization
;; 'nb': nb of slot to shift
;; 'init-from': position of first slot to move
;; 'init-to': destination of the first slot to move
(define (tail-shift cgc nb init-from init-to)
  (tail-shift-h cgc nb init-from init-to (- init-to init-from)))

(define (tail-shift-h cgc curr from to rsp-offset)
  (if (= curr 0)
      ;; All shifted, update RSP
      (x86-add cgc (x86-rsp) (x86-imm-int (* 8 rsp-offset)))
      ;; Shift next
      (begin (x86-mov cgc (x86-rax) (x86-mem (* from 8) (x86-rsp)))
             (x86-mov cgc (x86-mem (* to 8) (x86-rsp))  (x86-rax))
             (tail-shift-h cgc (- curr 1) (- from 1) (- to 1) rsp-offset))))

;;
;; UTILS
;;

;; Build new environment with ids starting from 'start'
;; Append this new environment to existing 'env'
;; ex : (build-env '(...) '(a b c) 8) -> ((a . 8) (b . 9) (c . 10))
;; mvars contains all mutable vars to tag created identifier objects
(define (build-env mvars ids start env)
  (if (null? ids)
    env
    (cons (cons (car ids) (make-identifier 'local
                                           start
                                           (list start)
                                           (if (member (car ids) mvars)
                                              '(mutable)
                                              '())
                                           '()))
          (build-env mvars (cdr ids) (+ start 1) env))))

;; Get position of current closure in stack
(define (closure-pos ctx)
  (- (length (ctx-stack ctx)) 2 (ctx-nb-args ctx))) ;; 2= 1length + 1retAddr

;; Get formal params from list of params
;; Ex: (formal-params '(a b c)  ) -> '(a b c)
;;     (formal-params '(a b . c)) -> '(a b)
(define (formal-params l)
  (if (not (pair? l))
     '()
     (cons (car l) (formal-params (cdr l)))))

;; Gen mutable variable
;; This code is a function prelude. It transforms variable from stack (args) tagged as "mutable"
;; into memory-allocated variables.
(define (gen-mutable cgc ctx mutable)

    (if (null? mutable)
       ctx ;; Return new ctx
       (let* ((res (assoc (car mutable) (ctx-env ctx)))
              (header-word (mem-header 2 STAG_MOBJECT))
              (fs (length (ctx-stack ctx)))
              (l-pos (- fs 2 (identifier-offset (cdr res))))
              (offset (* l-pos 8)))

        ;; Alloc
        (gen-allocation cgc ctx STAG_MOBJECT 2)

        ;; Create var in memory
        (gen-get-localvar cgc ctx res 'gen-reg) ;; There are only localvar here (no free vars)
        (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))

        ;; Replace local
        (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
        (x86-mov cgc (x86-mem offset (x86-rsp)) (x86-rax))

        (let* ((nstack (append (list-head (ctx-stack ctx) l-pos)
                              (cons CTX_MOBJ
                                    (list-tail (ctx-stack ctx) (+ l-pos 1)))))
               (nctx (make-ctx nstack (ctx-env ctx) (ctx-nb-args ctx))))
          ;; Gen next mutable vars
          (gen-mutable cgc nctx (cdr mutable))))))

;; Gen code to create rest list from stack in heap.
;; nb-pop: Number of values in rest list
;; sp-offset: offset from rsp of the first value in rest list
(define (gen-rest-lst cgc ctx nb-pop)
  ;; Push the last cdr
  (x86-push cgc (x86-imm-int (obj-encoding '())))
  ;; Buils rest list
  (gen-rest-lst-h cgc ctx nb-pop nb-pop 16)) ;; 24: '(), ctx, closure

;; Create a pair with top of stack in cdr
;; and argument slot (rsp + sp-offset) in car
;; then push this pair and create next.
(define (gen-rest-lst-h cgc ctx pos nb sp-offset)
  (if (= pos 0)
      ;; All pairs created, then change stack layout
      (begin ;; Mov rest list to stack
             (x86-pop cgc (x86-rax))
             (x86-mov cgc (x86-mem (* nb 8) (x86-rsp)) (x86-rax))
             ;; Update closure position
             (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
             (x86-mov cgc (x86-mem (- (* 8 nb) 8) (x86-rsp)) (x86-rax))
             ;; Update rsp
             (x86-add cgc (x86-rsp) (x86-imm-int (- (* 8 nb) 8))))
      ;; Create a pair and continue
      (begin ;; Alloc pair
             (gen-allocation cgc ctx STAG_PAIR 3)

             (let ((header (mem-header 3 STAG_PAIR)))
               ;; Write header in pair
               (x86-mov cgc (x86-rax) (x86-imm-int header))
               (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
               ;; Get car from stack (arg slot) and write in pair
               (x86-mov cgc (x86-rax) (x86-mem sp-offset (x86-rsp)))
               (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
               ;; Get cdr from stack (top of stack) and write in pair
               (x86-pop cgc (x86-rax))
               (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rax))
               ;; Tag & push
               (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
               (x86-push cgc (x86-rax)))
             ;; Create next pair
             (gen-rest-lst-h cgc ctx(- pos 1) nb (+ sp-offset 8)))))

;; Return label of a stub generating error with 'msg'
(define (get-label-error msg)
  (list-ref (add-callback #f
                          0
                          (lambda (ret-addr selector) (error msg)))
            0))
