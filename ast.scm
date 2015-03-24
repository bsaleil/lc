(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; TODO : RCX global ? (used by if/else stubs)

;;-----------------------------------------------------------------------------
;; Macros

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

(define-macro (assert c err)
   `(if (not ,c)
      (begin (pp ast)
             (println "!!! ERROR : " ,err)
             (exit 1))))

;;-----------------------------------------------------------------------------
;; Primitives

;; Primitives for type tests
(define type-predicates `(
  (output-port? ,CTX_OPORT)
  (input-port?  ,CTX_IPORT)
  (symbol?      ,CTX_SYM)
  (string?      ,CTX_STR)
  (char?        ,CTX_CHAR)
  (vector?      ,CTX_VECT)
  (number?      ,CTX_NUM)
  (procedure?   ,CTX_CLO)
  (pair?        ,CTX_PAI)
))

(define (type-from-predicate p)
  (cond ((eq? p 'pair?)        CTX_PAI)
        ((eq? p 'procedure?)   CTX_CLO)
        ((eq? p 'number?)      CTX_NUM)
        ((eq? p 'vector?)      CTX_VECT)
        ((eq? p 'char?)        CTX_CHAR)
        ((eq? p 'string?)      CTX_STR)
        ((eq? p 'symbol?)      CTX_SYM)
        ((eq? p 'input-port?)  CTX_IPORT)
        ((eq? p 'output-port?) CTX_OPORT)
        (else
          (error "NYI"))))

;; Primitives: name, nb args min, nb args max
(define primitives `(
   (car                 1  1  ,(prim-types 1 CTX_PAI))
   (cdr                 1  1  ,(prim-types 1 CTX_PAI))
   (eq?                 2  2  ,(prim-types 2 CTX_ALL CTX_ALL))
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

;;-----------------------------------------------------------------------------

(define (assert-p-nbargs ast)
  (assert (and (>= (length (cdr ast))
                   (cadr (assoc (car ast) primitives)))
               (<= (length (cdr ast))
                   (caddr (assoc (car ast) primitives))))
          ERR_WRONG_NUM_ARGS))

(define (assert-t-nbargs ast)
  (assert (= (length (cdr ast))
             1)
          ERR_WRONG_NUM_ARGS))

;;-----------------------------------------------------------------------------
;; AST DISPATCH

;; Gen lazy code from a list of exprs
(define (gen-ast-l lst succ)
  (if (null? lst)
     succ
     (gen-ast (car lst)
              (gen-ast-l (cdr lst) succ))))

;; Gen lazy code from ast
(define (gen-ast ast succ)
  (cond ;; String
        ((string? ast)  (mlc-string ast succ))
        ;; Symbol
        ((symbol? ast)  (mlc-identifier ast succ))
        ;; Literal
        ((literal? ast) (mlc-literal ast succ))
        ;; Pair
        ((pair? ast)
         (let ((op (car ast)))
           (cond ;; Special
                 ((member op '(breakpoint)) (mlc-special ast succ))
                 ;; Special without call
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
                 ((member op '(+ - * < > <= >= =))         (mlc-op-n ast succ op))
                 ((member op '(quotient modulo remainder)) (mlc-op-bin ast succ op))
                 ;; Tests
                 ((assoc op type-predicates) (mlc-test ast succ))
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
;; Make lazy code from num/bool/char/null literal
;;
(define (mlc-literal ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (if (and (number? ast) (>= ast 536870912)) ;; 2^(32-1-2) (32bits-sign-tags)
          (begin (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding ast)))
                 (x86-push cgc (x86-rax)))
          (x86-push cgc (x86-imm-int (obj-encoding ast))))
      (jump-to-version cgc
                       succ
                       (ctx-push ctx
                                 (cond ((number? ast)  CTX_NUM)
                                       ((boolean? ast) CTX_BOOL)
                                       ((char? ast)    CTX_CHAR)
                                       ((null? ast)    CTX_NULL)
                                       (else (error "NYI"))))))))
;;
;; Make lazy code from symbol literal
;;
(define (mlc-symbol ast succ)
  (make-lazy-code
    (lambda (cgc ctx) 
      (let ((qword (get-symbol-qword ast)))
        (x86-mov cgc (x86-rax) (x86-imm-int qword))
        (x86-push cgc (x86-rax))
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
        (x86-push cgc (x86-rbx)) ;; TODO + Remove if element is a literal ?
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
        (x86-mov cgc (x86-mem (- (* -8 (vector-length ast)) 16) alloc-ptr) (x86-rax))
        ;; Write length
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (vector-length ast))))
        (x86-mov cgc (x86-mem (- (* -8 (vector-length ast)) 8) alloc-ptr) (x86-rax))
        ;; Push vector
        (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ (* 8 (+ (vector-length ast) 2))) alloc-ptr))
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
      (let* ((len (string-length ast))
             (size (arithmetic-shift (bitwise-and (+ len 8) (bitwise-not 7)) -3))
             (header-word (mem-header (+ size 2) STAG_STRING)))
        
        (gen-allocation cgc ctx STAG_STRING (+ size 2))
        
        ;; Write header
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem (- (* -8 size) 16) alloc-ptr) (x86-rax))
        ;; Write length
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (string-length ast))))
        (x86-mov cgc (x86-mem (- (* -8 size) 8) alloc-ptr) (x86-rax)) 
        ;; Write chars
        (write-chars cgc ast 0 (* -8 size))
        ;; Push string
        (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ (* 8 (+ size 2))) alloc-ptr))
        (x86-push cgc (x86-rax))
        (jump-to-version cgc succ (ctx-push ctx CTX_STR))))))
      
;; Write chars of the literal string 'str':
;; Write str[pos] char to [alloc-ptr+offset], and write next chars
(define (write-chars cgc str pos offset)
   (if (< pos (string-length str))
             (let* ((int (char->integer (string-ref str pos)))
                    (encoded (if (> int 127)
                                 (* -1 (- 256 int))
                                 int)))
             (x86-mov cgc (x86-al) (x86-imm-int encoded))
             (x86-mov cgc (x86-mem offset alloc-ptr) (x86-al))
             (write-chars cgc str (+ pos 1) (+ offset 1)))))

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
                                  (error "Can't find variable: " ast)))))
                              
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
               (let ((r (assoc ast type-predicates)))
                 (if r
                    ;; Create and return function calling type predicate
                    (jump-to-version cgc
                                     (gen-ast `(lambda (a) (,ast a)) succ)
                                     ctx)
                    ;; Unknown id
                    (error "Can't find variable: " ast))))))))))

;;-----------------------------------------------------------------------------
;; INTERNAL

;;
;; Make lazy code from SET!
;;
(define (mlc-set! ast succ)
  
  (let* ((id (cadr ast))
         (lazy-set
            (make-lazy-code
               (lambda (cgc ctx)
                  (let ((glookup-res (assoc id globals)))
                    (let ((nctx 
                       (if glookup-res
                          ;; Global var
                          (gen-set-globalvar cgc ctx glookup-res)
                             (let ((res (assoc id (ctx-env ctx))))
                                (if res
                                   (if (eq? (identifier-type (cdr res)) 'free)
                                      (gen-set-freevar  cgc ctx res)  ;; Free var
                                      (gen-set-localvar cgc ctx res)) ;; Local var
                                   (error "Can't find variable: " id))))))
                  
                      (x86-push cgc (x86-imm-int ENCODING_VOID))
                      (jump-to-version cgc succ (ctx-push (ctx-pop nctx) CTX_VOID))))))))

     (gen-ast (caddr ast) lazy-set)))

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)
  
  (let* ((identifier (cadr ast))
         (lazy-bind (make-lazy-code (lambda (cgc ctx)
                                     (x86-pop cgc (x86-rax))
                                     (let* ((res (assoc identifier globals)) ;; Lookup in globals
                                            (pos (cdr res)))                 ;; Get global pos
                                                   
                                       (x86-mov cgc (x86-mem (* 8 pos) (x86-r10)) (x86-rax)))

                                     (x86-push cgc (x86-imm-int ENCODING_VOID))

                                     (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID)))))
         (lazy-val (gen-ast (caddr ast) lazy-bind)))

    (make-lazy-code (lambda (cgc ctx)
                      (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
                      (x86-mov cgc (x86-mem (* 8 (length globals)) (x86-r10)) (x86-rax))
                      (set! globals (cons (cons identifier (length globals)) globals))
                      (jump-to-version cgc lazy-val ctx)
                      ))))

;;
;; Make lazy code from LAMBDA
;;
(define (mlc-lambda ast succ lib-define)
  
  (let* (;; Lambda free vars
         (fvars #f)
         ;; Lambda mutable vars
         (mvars #f)
         ;; Rest param ?
         (rest-param (or (and (not (list? (cadr ast))) (not (pair? (cadr ast)))) ;; (foo . rest)
                         (and (pair? (cadr ast)) (not (list? (cadr ast)))))) ;; (foo a..z . rest)
         ;; Params list
         (params
           (if rest-param
              (formal-params (cadr ast))
              (cadr ast)))
         ;; Flatten list of param (include rest param)
         (all-params (flatten (cadr ast)))
         ;; Lazy lambda return
         (lazy-ret (make-lazy-code-ret ;; Lazy-code with 'ret flag
                     (lambda (cgc ctx)
                       ;; Here the stack is :
                       ;;         RSP
                       ;;     | ret-val | closure | arg n | ... | arg 1 | ret-addr |
                       ;; Or if rest :
                       ;;     | ret-val | closure |  rest | arg n | ... | arg 1 | ret-addr |
                       (let ((retval-offset
                                (if rest-param
                                   (* 8 (+ 2 (length params)))
                                   (* 8 (+ 1 (length params))))))
                         
                         ;; Pop return value
                         (x86-pop  cgc (x86-rax))
                         ;; Update SP to ret addr
                         (x86-add  cgc (x86-rsp) (x86-imm-int retval-offset))
                         ;; Jump to continuation
                         (x86-ret cgc)))))
         ;; Lazy lambda body
         (lazy-body (gen-ast (caddr ast) lazy-ret))
         ;; Lazy function prologue : creates rest param if any, transforms mutable vars, ...
         (lazy-prologue (make-lazy-code-entry
                           (lambda (cgc ctx)
                             
                              (let* ((actual-p (- (length (ctx-stack ctx)) 2)) ;; 1 for return address / closure
                                     (formal-p (ctx-nb-args ctx)))

                                (if (or (and (not rest-param) (not (= actual-p formal-p)))
                                        (and rest-param (< actual-p formal-p)))
                                   ;; Wrong number of arguments
                                   (begin
                                     (pp ast)
                                     (gen-error cgc ERR_WRONG_NUM_ARGS))
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
                                                  (set! nstack (cons CTX_CLO (cons CTX_PAI (cdr (ctx-stack ctx)))))
                                                  (set! nnbargs (+ (ctx-nb-args ctx) 1)))
                                                    ;(set! ctx cctx)))
                                               ((> actual-p formal-p)
                                                  ;; Build rest argument
                                                  (gen-rest-lst cgc ctx (- actual-p formal-p))
                                                  ;; Update ctx information
                                                  (set! nstack (cons CTX_CLO (cons CTX_PAI (list-tail (ctx-stack ctx) (- (length (ctx-stack ctx)) formal-p 1)))))
                                                  (set! nnbargs (+ (ctx-nb-args ctx) 1)))))
                                     (let* ((nctx (make-ctx nstack (ctx-env ctx) nnbargs))
                                            (mctx (gen-mutable cgc nctx mvars)))
                                       (jump-to-version cgc lazy-body mctx)))))))))

    ;; Lazy closure generation
    (make-lazy-code
      (lambda (cgc ctx)
        
        (let* (;; Lambda stub
               (stub-labels (add-fn-callback cgc
                                             0
                                             (lambda (sp sctx ret-addr selector closure)
                                               
                                               ;; Extends env with params and free vars
                                               (let* ((env (build-env mvars all-params 0 (build-fenv (ctx-stack ctx) (ctx-env ctx) mvars fvars 0)))
                                                      (nctx (make-ctx (ctx-stack sctx) env (length params))))
                                                 (gen-version-fn closure lazy-prologue nctx)))))
               (stub-addr (vector-ref (list-ref stub-labels 0) 1)))
          
          ;; COMPUTE FREE VARS
          (set! fvars (free-vars (caddr ast) all-params ctx))
          ;; COMPUTE MUTABLE VARS
          (set! mvars (mutable-vars (caddr ast) all-params))
          
          (let* ((closure-size (+ 2 (length fvars)))
                 (total-size (+ closure-size global-cc-table-maxsize 1)) ;; CCtable header -> +1
                 (header-word (mem-header closure-size STAG_PROCEDURE)))
            
            ;; ALLOC
            (gen-allocation cgc ctx STAG_PROCEDURE total-size)
    
            ;; 1 - WRITE OBJECT HEADER
            (x86-mov cgc (x86-rax) (x86-imm-int header-word))
            (x86-mov cgc (x86-mem (* -8 total-size) alloc-ptr) (x86-rax))

            ;; 2 - WRITE CC TABLE LOCATION
            (x86-lea cgc (x86-rax) (x86-mem (- (* -8 global-cc-table-maxsize) 8) alloc-ptr)) ;; CCtable header -> -8
            (x86-mov cgc (x86-mem (+ 8 (* -8 total-size)) alloc-ptr) (x86-rax))

            ;; 3 - WRITE FREE VARS
            (gen-free-vars cgc fvars ctx (+ 16 (* -8 total-size)))

            ;; 4 - WRITE CC TABLE
            (let ((cc-header (mem-header (+ 1 global-cc-table-maxsize) STAG_CCTABLE)))
              (x86-mov cgc (x86-rax) (x86-imm-int cc-header))
              (x86-mov cgc (x86-mem (+ (* 8 closure-size) (* -8 total-size)) alloc-ptr) (x86-rax)))
            (gen-cc-table cgc stub-addr (+ 8 (* 8 closure-size) (* -8 total-size)))
            
            ;; TAG AND PUSH CLOSURE
            (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ (* 8 total-size)) alloc-ptr))
            (x86-push cgc (x86-rax)))
            
          ;; Jump to next
          (jump-to-version cgc
                           succ
                           (ctx-push ctx
                                     CTX_CLO)))))))

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
                 (x86-push cgc (x86-imm-int ENCODING_VOID))
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
                          (x86-pop  cgc (x86-rax))
                          (x86-add  cgc (x86-rsp) (x86-imm-int (* 8 (- (length (cdr ast)) 1))))
                          (x86-push cgc (x86-rax))
                          (jump-to-version cgc succ mctx)))))))
             ;; LAZY BODIES
             (gen-ast-l (cdr ast) lazy-begin-out)))))-
    
;;-----------------------------------------------------------------------------
;; Bdingings (let, letrec, let*)

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
                          
                            (x86-pop cgc (x86-rax))
                            (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (+ (length ids) (length bodies) -1))))
                            (x86-push cgc (x86-rax))
                            (jump-to-version cgc succ ctx))))))
                   ;; LAZY BODIES
                   (lazy-bodies (gen-ast-l bodies lazy-let-out)))
            
            ;; Delegate with bodies as successor
            (cond ((eq? op 'let)    (mlc-let ast lazy-bodies))
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
                 (env (build-env ids ids start (ctx-env ctx))) ;; TODO: All ids are considered as mutable
                 (nctx (make-ctx stack env (ctx-nb-args ctx))))
             
             ;; Create values on stack (initial value is #f)
             (call-n (length ids) x86-push cgc (x86-imm-int (obj-encoding #f)))
             (let ((mctx (gen-mutable cgc nctx ids)))
              (jump-to-version cgc lazy-values mctx)))))))

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
        (x86-mov cgc (x86-rax) (x86-mem (* 8 from) (x86-rsp)))
        (x86-mov cgc (x86-rbx) (x86-mem (* 8 to) (x86-rsp)))
        (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)) (x86-rax))
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
;; Make lazy code from SPECIAL FORM
;;
(define (mlc-special ast succ)
  (cond ((eq? (car ast) 'breakpoint)
           (make-lazy-code
             (lambda (cgc ctx)
               (gen-breakpoint cgc)
               (x86-push cgc (x86-imm-int ENCODING_VOID))
               (jump-to-version cgc succ (ctx-push ctx CTX_VOID)))))))

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
  
  (let* ((special (car ast))
         (lazy-special
           (cond ;; EXIT
                 ((eq? special 'exit)
                    (make-lazy-code
                      (lambda (cgc ctx)
                         (gen-error cgc ""))))
                 ;; CONS
                 ((eq? special 'cons) (mlc-pair succ))
                 ;; NOT
                 ((eq? special 'not)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((label-done
                              (asm-make-label cgc (new-sym 'done))))
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                        (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                        (x86-je  cgc label-done)
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                        (x86-label cgc label-done)
                        (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL))))))
                 ;; NULL?
                 ((eq? special 'null?)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((label-done
                              (asm-make-label cgc (new-sym 'done))))
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                        (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                        (x86-je  cgc label-done)
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                        (x86-label cgc label-done)
                        (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL))))))
                 ;; EQ?
                 ((eq? special 'eq?)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((label-done
                              (asm-make-label cgc (new-sym 'done))))
                        (x86-pop cgc (x86-rax))
                        (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                        (x86-je  cgc label-done)
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                        (x86-label cgc label-done)
                        (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_BOOL))))))
                 ;; CAR & CDR
                 ((member special '(car cdr))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((offset
                              (if (eq? special 'car)
                                  (-  8 TAG_MEMOBJ)
                                  (- 16 TAG_MEMOBJ))))
                        (x86-pop cgc (x86-rax))
                        (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
                        (x86-push cgc (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_UNK))))))
                 ;; SET-CAR! & SET-CDR!
                 ((member special '(set-car! set-cdr!))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((offset
                              (if (eq? special 'set-car!)
                                  (-  8 TAG_MEMOBJ)
                                  (- 16 TAG_MEMOBJ))))
                        (x86-pop cgc (x86-rax)) ;; val
                        (x86-pop cgc (x86-rbx)) ;; pair
                        (x86-mov cgc (x86-mem offset (x86-rbx)) (x86-rax))
                        (x86-push cgc (x86-rbx))
                        (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_PAI))))))
                 ;; CURRENT-INPUT-PORT / CURRENT-OUTPUT-PORT
                 ((member special '(current-input-port current-output-port))
                   (make-lazy-code
                     (lambda (cgc ctx)
                       (let ((block-offset (if (eq? special 'current-output-port)
                                              8
                                              24)))
                         (x86-mov cgc (x86-rax) (x86-imm-int (+ TAG_MEMOBJ block-offset block-addr)))
                         (x86-push cgc (x86-rax))
                         (jump-to-version cgc succ (ctx-push ctx
                                                             (if (eq? special 'current-output-port)
                                                              CTX_OPORT
                                                              CTX_IPORT)))))))
                 ;; CLOSE-INPUT-PORT / CLOSE-OUTPUT-PORT
                 ((member special '(close-output-port close-input-port))
                   (make-lazy-code
                     (lambda (cgc ctx)
                       (gen-syscall-close cgc)
                       (x86-push cgc (x86-imm-int ENCODING_VOID))
                       (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID)))))
                 ;; OPEN-INPUT-FILE / OPEN-OUTPUT-FILE
                 ((member special '(open-output-file open-input-file))
                   (make-lazy-code
                     (lambda (cgc ctx)
                       (let* ((direction   (if (eq? special 'open-output-file) 'out 'in))
                              (stag        (if (eq? direction 'in) STAG_IPORT STAG_OPORT))
                              (header-word (mem-header 2 stag)))
                         ;; Gen 'open' syscall, file descriptor in rax
                         (gen-syscall-open cgc direction)
                         (x86-mov cgc (x86-rbx) (x86-rax))
                         ;; Allocate port object
                         (gen-allocation cgc ctx stag 2)
                         ;; Mov header
                         (x86-mov cgc (x86-rax) (x86-imm-int header-word))
                         (x86-mov cgc (x86-mem -16 alloc-ptr) (x86-rax))
                         ;; Mov descriptor
                         (x86-mov cgc (x86-mem  -8 alloc-ptr) (x86-rbx))
                         ;; Tag & push
                         (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))
                         (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                         ;; Jump to succ
                         (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (if (eq? direction 'in) CTX_IPORT CTX_OPORT)))))))
                 ;; EOF-OBJECT?
                 ((eq? special 'eof-object?)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((label-end (asm-make-label #f (new-sym 'label-end))))
                        (x86-pop cgc (x86-rax))
                        (x86-cmp cgc (x86-rax) (x86-imm-int ENCODING_EOF))
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                        (x86-jne cgc label-end)
                          (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                        (x86-label cgc label-end)
                        (x86-push cgc (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL))))))
                 ;; READ-CHAR
                 ((eq? special 'read-char)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      ;; Gen 'read' syscall (read 1 byte), encoded value (char or eof) in rax
                      (gen-syscall-read-char cgc)
                      ;; Push encoded result
                      (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                      ;; Jump to succ
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_CHAR)))))
                 ;; WRITE-CHAR
                 ((eq? special 'write-char)
                    (make-lazy-code
                      (lambda (cgc ctx)
                        ;; Gen 'read' syscall (read 1 byte), encoded value (char or eof) in rax
                        (gen-syscall-write-char cgc)
                        (x86-add cgc (x86-rsp) (x86-imm-int 16)) ;; NOTE: clean stack in gen-syscall-write-char?
                        ;; Push encoded result
                        (x86-push cgc (x86-imm-int ENCODING_VOID))
                        ;; Jump to succ
                        (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_VOID)))))
                 ;; CHAR<->INTEGER
                 ((member special '(char->integer integer->char))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (if (eq? special 'char->integer)
                        (x86-xor cgc (x86-mem 0 (x86-rsp)) (x86-imm-int TAG_SPECIAL) 8)
                        (x86-or  cgc (x86-mem 0 (x86-rsp)) (x86-imm-int TAG_SPECIAL) 8))
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx)
                                                          (if (eq? special 'char->integer)
                                                             CTX_NUM
                                                             CTX_CHAR))))))               
                 ;; MAKE-STRING
                 ((eq? special 'make-string)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let* ((header-word (mem-header 3 STAG_STRING)))
                        
                        ;; Pop encoded length
                        (if (= (length (cdr ast)) 1)
                          (x86-pop cgc (x86-rax))
                          (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp))))
                        
                        (x86-mov cgc (x86-rbx) (x86-rax))
                        
                        ;; Nb chars to byte size
                        (x86-shr cgc (x86-rax) (x86-imm-int 2))
                        (x86-and cgc (x86-rax) (x86-imm-int (bitwise-not 7)))
                        (x86-shr cgc (x86-rax) (x86-imm-int 1))
                        (x86-mov cgc (x86-rsi) (x86-rax))
                        
                        ;; Alloc
                        (gen-allocation cgc ctx STAG_STRING 3 #t)
                        
                        ;; Get string position in R15
                        (x86-mov cgc (x86-r15) (x86-rsi))
                        (x86-shl cgc (x86-r15) (x86-imm-int 1))
                        (x86-neg cgc (x86-r15))
                        (x86-add cgc (x86-r15) alloc-ptr)
                        (x86-sub cgc (x86-r15) (x86-imm-int 24))
                        
                        ;; Fill string
                        (x86-push cgc (x86-rbx))
                        
                        ;;
                        (x86-mov cgc (x86-rax) (x86-r15))
                        
                        (let ((label-loop (asm-make-label cgc (new-sym 'fill-vector-loop)))
                              (label-end  (asm-make-label cgc (new-sym 'fill-vector-end))))
                        
                        (if (= (length (cdr ast)) 2)
                          (begin (x86-mov cgc (x86-rdx) (x86-mem 8 (x86-rsp)))
                                 (x86-sar cgc (x86-rdx) (x86-imm-int 2))))
                        
                        ;; LOOP:
                        ;;   if (rbx == 0) jump END
                        (x86-label cgc label-loop)
                        (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
                        (x86-jle  cgc label-end)
                        
                          (if (= (length (cdr ast)) 1)
                            (begin ;; Init string slot
                                   (x86-mov cgc (x86-mem 16 (x86-rax)) (x86-imm-int 0) 64) ;; Write 0 in 8 chars
                                   ;; Update offset and remaining elements nb
                                   (x86-add cgc (x86-rax) (x86-imm-int 8))
                                   ;; Loop
                                   (x86-sub cgc (x86-rbx) (x86-imm-int 32))) ;; Remove 8 to encoded number (=8*4=32)
                            (begin ;;  TODO
                                   (x86-mov cgc (x86-mem 16 (x86-rax)) (x86-dl))
                                   (x86-add cgc (x86-rax) (x86-imm-int 1))
                                   (x86-sub cgc (x86-rbx) (x86-imm-int 4))))
                            
                        (x86-jmp cgc label-loop)
                        
                        ;; END:
                        (x86-label cgc label-end)
                        (x86-pop cgc (x86-rbx)))
                        
                        (if (= (length (cdr ast)) 2)
                           (x86-add cgc (x86-rsp) (x86-imm-int 16)))
                        
                        ;; Write encoded length
                        (x86-mov cgc (x86-mem 8 (x86-r15)) (x86-rbx))
                        
                        ;; Write header
                        (x86-shl cgc (x86-rsi) (x86-imm-int 6))
                        (x86-add cgc (x86-rsi) (x86-imm-int header-word))
                        (x86-mov cgc (x86-mem 0 (x86-r15)) (x86-rsi))
                        
                        ;; Push vector
                        (x86-add cgc (x86-r15) (x86-imm-int TAG_MEMOBJ))
                        (x86-push cgc (x86-r15))
                        
                        (jump-to-version cgc succ (ctx-push (if (= (length (cdr ast)) 1)
                                                              (ctx-pop ctx)
                                                              (ctx-pop-nb ctx 2))
                                                            CTX_STR))))))
                        
                 ;; MAKE-VECTOR
                 ((eq? special 'make-vector)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let* ((header-word (mem-header 2 STAG_VECTOR))
                             (header-reg (x86-rbx)))
                        
                        ;; Pop encoded length
                        (if (= (length (cdr ast)) 1)
                          (x86-pop cgc (x86-rax))
                          (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp))))
                        
                        (x86-mov cgc (x86-rbx) (x86-rax))
                        
                        ;; Alloc
                        (gen-allocation cgc ctx STAG_VECTOR 2 #t)
                        
                        ;; Get vector position in R15
                        (x86-mov cgc (x86-rax) (x86-rbx))
                        (x86-shl cgc (x86-rax) (x86-imm-int 1))
                        (x86-add cgc (x86-rax) (x86-imm-int 16))
                        (x86-mov cgc (x86-r15) alloc-ptr)
                        (x86-sub cgc (x86-r15) (x86-rax))
                        
                        ;; Fill vector
                        (x86-push cgc (x86-r15))
                        (x86-push cgc (x86-rbx))
                        
                          ;; Init value in RAX (0)
                          (if (= (length (cdr ast)) 1)
                            (x86-mov cgc (x86-rax) (x86-imm-int 0))
                            (x86-mov cgc (x86-rax) (x86-mem 16 (x86-rsp))))
                          
                          (let ((label-loop (asm-make-label cgc (new-sym 'fill-vector-loop)))
                                (label-end  (asm-make-label cgc (new-sym 'fill-vector-end))))
                          
                          ;; LOOP:
                          ;;    if (rbx == 0) jump END
                          (x86-label cgc label-loop)
                          (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
                          (x86-je  cgc label-end)
                          
                            ;; Init vector slot
                            (x86-mov cgc (x86-mem 16 (x86-r15)) (x86-rax))
                            ;; Update offset and remaining elements nb
                            (x86-add cgc (x86-r15) (x86-imm-int 8))
                            (x86-sub cgc (x86-rbx) (x86-imm-int 4))
                            ;; loop
                            (x86-jmp cgc label-loop)
                        
                        ;; END:  
                        (x86-label cgc label-end)
                        (x86-pop cgc (x86-rbx))
                        (x86-pop cgc (x86-r15)))
                        
                        (if (= (length (cdr ast)) 2)
                           (x86-add cgc (x86-rsp) (x86-imm-int 16)))
                        
                        ;; Write encoded length
                        (x86-mov cgc (x86-mem 8 (x86-r15)) (x86-rbx))
                        
                        ;; Write header
                        (x86-shl cgc (x86-rbx) (x86-imm-int 6))
                        (x86-add cgc (x86-rbx) (x86-imm-int header-word))
                        (x86-mov cgc (x86-mem 0 (x86-r15)) (x86-rbx))
                        
                        ;; Push vector
                        (x86-add cgc (x86-r15) (x86-imm-int TAG_MEMOBJ))
                        (x86-push cgc (x86-r15))
                        
                        ;;
                        (jump-to-version cgc succ (ctx-push (if (= (length (cdr ast)) 1)
                                                               (ctx-pop ctx)
                                                               (ctx-pop-nb ctx 2))
                                                            CTX_VECT))))))
                 
                 ;; STRING<->SYMBOL
                 ((eq? special 'string->symbol)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (gen-interned-symbol cgc)
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_SYM)))))
                 
                 ((eq? special 'symbol->string)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      
                      ;; Alloc
                      (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                      (x86-sub cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                      (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rax)))
                      (x86-shr cgc (x86-rax) (x86-imm-int 8))
                      (x86-shl cgc (x86-rax) (x86-imm-int 2))
                      (x86-mov cgc (x86-rbx) (x86-rax))
                      (gen-allocation cgc
                                      ctx
                                      STAG_STRING
                                      0
                                      #t)
                
                      ;; Symbol address in rbx
                      (x86-shl cgc (x86-rbx) (x86-imm-int 1))
                      (x86-neg cgc (x86-rbx))
                      (x86-add cgc (x86-rbx) alloc-ptr)
                      
                      ;; String address in rax
                      (x86-pop cgc (x86-rax))
                      (x86-sub cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                      
                      ;; Mov length in symbol
                      (x86-mov cgc (x86-r15) (x86-mem 8 (x86-rax)))
                      (x86-mov cgc (x86-mem 8 (x86-rbx)) (x86-r15))
                      
                      ;; Mov header in symbol
                      (x86-mov cgc (x86-r15) (x86-mem 0 (x86-rax)))
                      (x86-add cgc (x86-r15) (x86-imm-int (arithmetic-shift (- STAG_STRING STAG_SYMBOL) 3)))
                      (x86-mov cgc (x86-mem 0 (x86-rbx)) (x86-r15))
                      
                      ;; Encoded length in r15
                      (x86-shr cgc (x86-r15) (x86-imm-int 8))
                      (x86-shl cgc (x86-r15) (x86-imm-int 3))
                                            
                      ;; If encoded length == 16
                      ;;    jump label-fin
                      (let ((label-loop (asm-make-label cgc (new-sym 'label-loop)))
                            (label-fin  (asm-make-label cgc (new-sym 'label-fin))))
                        
                        (x86-label cgc label-loop)
                        (x86-cmp cgc (x86-r15) (x86-imm-int 16))
                        (x86-jle cgc label-fin)
                        
                          (x86-mov cgc (x86-rdx) (x86-mem -8 (x86-r15) (x86-rax)))
                          (x86-mov cgc (x86-mem -8 (x86-r15) (x86-rbx)) (x86-rdx))
                          (x86-sub cgc (x86-r15) (x86-imm-int 8))
                          (x86-jmp cgc label-loop)
                      
                        (x86-label cgc label-fin))
                        
                      (x86-add cgc (x86-rbx) (x86-imm-int TAG_MEMOBJ))
                      (x86-push cgc (x86-rbx))
                      
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_STR)))))
                      
                 ;; VECTOR-LENGTH & STRING-LENGTH
                 ((member special '(vector-length string-length))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (x86-pop cgc (x86-rax)) ;; Pop vector
                      (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_NUM)))))
                 
                 ;; VECTOR-REF & STRING-REF
                 ((member special '(vector-ref string-ref))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((vr? (eq? special 'vector-ref)))
                        (x86-pop cgc (x86-rax)) ;; Pop index
                        (x86-pop cgc (x86-rbx)) ;; Pop vector
                        (cond (vr?
                                (x86-shl cgc (x86-rax) (x86-imm-int 1))
                                (x86-add cgc (x86-rbx) (x86-rax))
                                (x86-push cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx)))
                                (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_UNK)))
                              ((not vr?)
                               (x86-shr cgc (x86-rax) (x86-imm-int 2)) ;; Decode position
                               (x86-mov cgc (x86-al) (x86-mem (- 16 TAG_MEMOBJ) (x86-rax) (x86-rbx))) ;; Get Char
                               (x86-and cgc (x86-rax) (x86-imm-int 255)) ;; Clear bits before al
                               (x86-shl cgc (x86-rax) (x86-imm-int 2)) ;; Encode char
                               (x86-add cgc (x86-rax) (x86-imm-int TAG_SPECIAL))
                               (x86-push cgc (x86-rax)) ;; Push char
                               (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2)
                                                                   (if (eq? special 'string-ref)
                                                                      CTX_CHAR
                                                                      CTX_UNK)))))))))                        
                        
                 
                 ;; VECTOR-SET! & STRING-SET!
                 ((member special '(vector-set! string-set!))
                  (make-lazy-code
                    (lambda (cgc ctx)
                        (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))  ;; Get index
                        (x86-mov cgc (x86-rbx) (x86-mem 16 (x86-rsp))) ;; Get vector
                        (x86-mov cgc (x86-rdx) (x86-mem 0 (x86-rsp)))  ;; Get new value
                        
                        (cond ((eq? special 'vector-set!)
                                (x86-shl cgc (x86-rax) (x86-imm-int 1))
                                (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx) (x86-rax)) (x86-rdx)))
                              
                              (else
                                (x86-shr cgc (x86-rdx) (x86-imm-int 2))
                                (x86-shr cgc (x86-rax) (x86-imm-int 2))
                                (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx) (x86-rax)) (x86-dl))))
                        
                        (x86-add cgc (x86-rsp) (x86-imm-int 24))
                        (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
                        (x86-push cgc (x86-rax))
                        
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
      (check-types types (cdr ast) lazy-special ast))))

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

                                (if verbose-jit
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

                                                (if verbose-jit (println ">>> swapping-branches"))

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
                            
                              (if verbose-jit
                                  (begin
                                    (println ">>> selector= " selector)
                                    (println ">>> prev-action= " prev-action)))
                            
                              (if (not prev-action)
                                  
                                  (begin (set! prev-action 'no-swap)
                                         (if (= selector 1)
                                          
                                            ;; overwrite unconditional jump
                                            (gen-version (+ jump-addr 6) lazy-success ctx-out)
                                          
                                            (if (= (+ jump-addr 6 5) code-alloc)

                                              (begin (if verbose-jit (println ">>> swapping-branches"))
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
;; TODO : factoriser avec mlc-call
(define (mlc-apply ast succ)

  (let* (;; LAZY CALL
         (lazy-call
          (make-lazy-code
            ;; Remove apply op and lst, push args from lst,
            ;; push closure, and call
            (lambda (cgc ctx)
              
              ;; Remove lst and op from stack
              (x86-pop cgc (x86-rax)) ;; lst
              (x86-pop cgc (x86-rbx)) ;; op

              ;; Read and push all args from lst until we reach '()
              (let ((label-end  (asm-make-label #f (new-sym 'apply-args-end)))
                    (label-loop (asm-make-label #f (new-sym 'apply-args-loop))))
                ;; RDI contains the number of arguments
                (x86-mov cgc (x86-rdi) (x86-imm-int 0))
                (x86-label cgc label-loop)
                ;; If current el is null, then jump to end
                (x86-cmp cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                (x86-je cgc label-end)
                  ;; Else, push arg and update RDI
                  (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))           ;; Push car
                  (x86-mov cgc (x86-rax) (x86-mem (- 16 TAG_MEMOBJ) (x86-rax))) ;; Get cdr for next iteration
                  (x86-inc cgc (x86-rdi))  ;; inc args number
                  (x86-jmp cgc label-loop) ;; next iteration
                ;; All args are pushed
                (x86-label cgc label-end))

              ;; Push closure
              (x86-push cgc (x86-rbx))

              (let* ((call-ctx (make-ctx fake-stack '() -1))
                     (cct-offset    (* 8 (+ 1 (get-closure-index call-ctx)))))
                ;; Put ctx with fake stack in r11 because we don't know the type/number of arguments
                (x86-mov cgc (x86-r11) (x86-imm-int (ctx->still-ref call-ctx)))

                ;; 1 - Get cc-table
                (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)))

                ;; 2 - Get entry point in cc-table
                (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax)))
                
                ;; 3 - Jump
                (x86-jmp cgc (x86-rax))))))
        
         ;; LAZY APPLY
         (lazy-apply
           ;; Push operator and args lst 
           (let ((lazy-right (gen-ast (caddr ast) lazy-call)))
             (gen-ast (cadr ast) lazy-right))))
    
    ;; Create stub and push ret addr
    (make-lazy-code
      (lambda (cgc ctx)
        (let* (;; Flag in stub : is the continuation already generated ?
               (gen-flag #f)
               ;; Label for return address loading
               (load-ret-label (asm-make-label cgc (new-sym 'load-ret-addr)))
               ;; Continuation stub
               (stub-labels (add-callback cgc
                                          0
                                          (lambda (ret-addr selector)
                                              (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag = continuation addr
                                                 (set! gen-flag (gen-version-continuation load-ret-label
                                                                                          (make-lazy-code ;; TODO : move 
                                                                                            (lambda (cgc ctx)
                                                                                              (x86-push cgc (x86-rax))
                                                                                              (jump-to-version cgc succ (ctx-push ctx CTX_UNK))))
                                                                                          ctx)))
                                              gen-flag))))
          ;; Return address (continuation label)
          (x86-label cgc load-ret-label)
          (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1)))
          (x86-push cgc (x86-rax))

          (jump-to-version cgc lazy-apply (ctx-push ctx CTX_RETAD)))))))

;;
;; Make lazy code from CALL EXPR
;;
(define (mlc-call ast succ)
  (let* (;; Tail call if successor's flags set contains 'ret flag
         (tail (member 'ret (lazy-code-flags succ)))
         ;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc (ERR_TYPE_EXPECTED CTX_CLO)))))
         ;; Lazy call
         (lazy-call (make-lazy-code (lambda (cgc ctx)
                                        ;; Call ctx in rdx                             
                                        (let* ((call-stack    (if tail
                                                                (append (list-head (ctx-stack ctx) (+ 1 (length args))) (list CTX_RETAD))
                                                                (list-head (ctx-stack ctx) (+ (length args) 2))))
                                               (call-ctx      (make-ctx call-stack '() -1))
                                               (cct-offset    (* 8 (+ 1 (get-closure-index call-ctx)))))                                        
                                        
                                        (if tail 
                                          (tail-shift cgc
                                                      ;; Nb slots to shift
                                                      (+ (length args) 1) ;; +1 closure
                                                      ;; Initial from slot
                                                      (length args)
                                                      ;; Initial to slot
                                                      (- (length (ctx-stack ctx)) 2)))
                                        
                                        ;; If count calls compiler opt
                                        (if (eq? (car ast) count-calls)
                                           (gen-inc-slot cgc 'calls))
                                        
                                        ;; 0 - R11 = still-box address containing call-ctx
                                        (x86-mov cgc (x86-r11) (x86-imm-int (ctx->still-ref call-ctx)))
                                      
                                        ;; 1 - Get cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))                ;; get closure
                                        (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))) ;; get cc-table
                                        
                                        ;; 2 - Get entry point in cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax)))
                                                                                
                                        ;; 3 - Jump to entry point                                        
                                        (x86-jmp cgc (x86-rax))))))
         ;; Lazy operator
         (lazy-operator (check-types (list CTX_CLO) (list (car ast)) lazy-call ast)))

    ;; Build first lazy code
    ;; This lazy code creates continuation stub and push return address
    (if tail
        (if (> (length args) 0)
          ;; If args, then compile args
          (gen-ast-l args lazy-operator)
          ;; Else, compile call
          lazy-operator)
        (make-lazy-code
           (lambda (cgc ctx)
              (let* (;; Flag in stub : is the continuation already generated ?
                     (gen-flag #f)
                     ;; Label for return address loading
                     (load-ret-label (asm-make-label cgc (new-sym 'load-ret-addr)))
                     ;; Continuation stub
                     (stub-labels (add-callback cgc
                                                0
                                                (lambda (ret-addr selector)
                                                    (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag = continuation addr
                                                       (set! gen-flag (gen-version-continuation load-ret-label
                                                                                                (make-lazy-code ;; TODO : move 
                                                                                                  (lambda (cgc ctx)
                                                                                                    (x86-push cgc (x86-rax))
                                                                                                    (jump-to-version cgc succ (ctx-push ctx CTX_UNK))))
                                                                                                ctx)))
                                                     gen-flag))))
                 ;; Return address (continuation label)
                 (x86-label cgc load-ret-label)
                 (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1)))
                 (x86-push cgc (x86-rax))
                  
                 (let ((succ (if (> (length args) 0)
                                ;; If args, then compile args
                                (gen-ast-l args lazy-operator)
                                ;; Else, compile call
                                lazy-operator)))
                            
                    (jump-to-version cgc succ (ctx-push ctx CTX_RETAD)))))))))

;;-----------------------------------------------------------------------------
;; Operators

;;
;; Make lazy code from BINARY OPERATOR
;;
(define (mlc-op-bin ast succ op)
  (let ((opnds (cdr ast)))
    (if (not (= (length opnds) 2))
      ;; != 2 operands, error
      (make-lazy-code
        (lambda (cgc succ)
          (gen-error cgc ERR_WRONG_NUM_ARGS)))
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
                   (x86-cqo cgc)
                   (x86-idiv cgc (x86-rbx))
                   (cond ((eq? op 'quotient) ;; TODO : check '/0'
                           (x86-shl cgc (x86-rax) (x86-imm-int 2))
                           ;(x86-jo cgc (list-ref stub-labels 0)) ;; TODO ? jo modulo
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
(define (mlc-op-n ast succ op)
  ;; TODO : Inlined primitive: check if redefined
  
  (let* (;; Operands number
         (nb-opnd (length (cdr ast)))
         ;; Overflow stub
         (overflow-labels (add-callback #f 0 (lambda (ret-addr selector)
                                            (error ERR_ARR_OVERFLOW))))
         ;; Lazy operation
         (lazy-op
           (make-lazy-code
             (lambda (cgc ctx)
              
               ;; NOTE: Optimization: if only 2 opnds we can write :
               ;;       pop rax
               ;;       sub [rsp+0], rax
               ;; -> less instructions, and no rsp update
               ;; Compute substraction with operands from stack
               (define (compute- offset nb total)
                 (if (= nb 0)
                     (x86-mov cgc (x86-mem (* 8 (- total 1)) (x86-rsp)) (x86-rax))
                     (begin (x86-sub cgc (x86-rax) (x86-mem offset (x86-rsp)))
                            (x86-jo cgc (list-ref overflow-labels 0))
                            (compute- (- offset 8) (- nb 1) total))))
              
               ;; Compute addition with operands from stack
               (define (compute+ offset nb)
                 (if (= nb 0)
                   (begin (x86-add cgc (x86-mem offset (x86-rsp)) (x86-rax))
                          (x86-jo cgc (list-ref overflow-labels 0)))
                   (begin (x86-add cgc (x86-rax) (x86-mem offset (x86-rsp)))
                          (x86-jo cgc (list-ref overflow-labels 0))
                          (compute+ (+ offset 8) (- nb 1)))))
               
               ;; Compute multiplication with operands from stack
               (define (compute* offset nb)
                 (if (= nb 0)
                   (begin (x86-sar cgc (x86-rax) (x86-imm-int 2))
                          (x86-imul cgc (x86-rax) (x86-mem offset (x86-rsp)))
                          (x86-jo cgc (list-ref overflow-labels 0))
                          (x86-mov cgc (x86-mem offset (x86-rsp)) (x86-rax)))                          
                   (begin (x86-sar cgc (x86-rax) (x86-imm-int 2))
                          (x86-imul cgc (x86-rax) (x86-mem offset (x86-rsp)))
                          (x86-jo cgc (list-ref overflow-labels 0))
                          (compute* (+ offset 8) (- nb 1)))))
               
               ;; Compute less with operands from stack
               (define (compute< x86op offset nb total label-end)
                 (if (= nb 0)
                    (begin (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                           (x86-label cgc label-end)
                           (x86-mov cgc (x86-mem (* 8 total) (x86-rsp)) (x86-rax)))
                    (begin (x86-cmp cgc (x86-rax) (x86-mem offset (x86-rsp)))
                           (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                           (x86op cgc label-end)
                           (if (> nb 1)
                            (x86-mov cgc (x86-rax) (x86-mem offset (x86-rsp))))
                           (compute< x86op (+ offset 8) (- nb 1) total label-end))))
               
               (cond ((eq? op '+)
                         (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; RAX = firsts
                         (compute+ 8 (- nb-opnd 2)))
                     ((eq? op '-)
                         (x86-mov cgc (x86-rax) (x86-mem (* 8 (- nb-opnd 1)) (x86-rsp))) ;; RAX = first opnd
                         (compute- (* 8 (- nb-opnd 2)) (- nb-opnd 1) nb-opnd))
                     ((eq? op '*)
                         (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                         (compute* 8 (- nb-opnd 2)))
                     ((member op '(< > <= >= =))
                         (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; RAX = last opnd
                         (compute< (cond ((eq? op '<) x86-jle) ;; n-th opnd must be > than (n-1)-th opnd then if false jle to end
                                         ((eq? op '>) x86-jge)
                                         ((eq? op '<=) x86-jl)
                                         ((eq? op '>=) x86-jg)
                                         ((eq? op '=) x86-jne))
                                   8
                                   (- nb-opnd 1)
                                   (- nb-opnd 1)
                                   (asm-make-label #f (new-sym 'label-<-end)))))
               
               ;; Update RSP
               (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- nb-opnd 1))))
               ;; Jump to succ
               (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx nb-opnd) (if (member op '(< > <= >= =))
                                                                               CTX_BOOL
                                                                               CTX_NUM)))))))
    
    (cond ;; No opnd, push 0 and jump to succ
          ((= (length (cdr ast)) 0)
             (cond ((eq? op '+) (gen-ast 0 succ))
                   ((eq? op '-) (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_WRONG_NUM_ARGS))))
                   ((eq? op '*) (gen-ast 1 succ))
                   ((member op '(< > <= >= =)) (gen-ast #t succ))))
          ;; 1 opnd,  push opnd, test type, and jump to succ
          ((= (length (cdr ast)) 1)
             (cond ((eq? op '+) (check-types (list CTX_NUM) (cdr ast) succ ast))
                   ((eq? op '-) (gen-ast (list '* -1 (cadr ast)) succ))
                   ((eq? op '*) (check-types (list CTX_NUM) (cdr ast) succ ast))
                   ((member op '(< > <= >= =)) (gen-ast #t succ))))
          ;; >1 opnd, build chain
          (else (check-types (make-list (length (cdr ast)) CTX_NUM)
                             (cdr ast)
                             lazy-op
                             ast)))))

;;
;; Make lazy code from TYPE TEST
;;
(define (mlc-test ast succ)
  
  (let ((lazy-test
          (make-lazy-code
            (lambda (cgc ctx)
                            
              (let* ((type (type-from-predicate (car ast)))
                     (known-type (car (ctx-stack ctx)))
                     (ctx-true  (ctx-push (ctx-pop (ctx-change-type ctx 0 type)) CTX_BOOL))
                     (ctx-false (ctx-push (ctx-pop ctx) CTX_BOOL)))
                
                ;; If 'all-tests' option enabled, then remove type information
                (if all-tests
                   (set! known-type CTX_UNK))
                
                (cond ;; known == expected
                      ((eq? type known-type)
                         (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                         (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                         (jump-to-version cgc succ ctx-true))
                      ;; known != expected && known != unknown
                      ((not (eq? known-type CTX_UNK))
                         (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                         (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                         (jump-to-version cgc succ ctx-false))
                      ;; known == unknown
                      (else
                        (let* ((stack-idx 0)
                               (laz-succ (make-lazy-code
                                           (lambda (cgc ctx)
                                             (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                             (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                             (jump-to-version cgc succ ctx-true))))
                               (laz-fail (make-lazy-code
                                           (lambda (cgc ctx)
                                             (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                             (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                             (jump-to-version cgc succ ctx-false)))))
                        
                            (jump-to-version cgc
                                             (gen-dyn-type-test type
                                                                stack-idx
                                                                ctx
                                                                laz-succ
                                                                ctx
                                                                laz-fail) ctx)))))))))
    (gen-ast (cadr ast) lazy-test)))
          
;;
;; Make lazy code to create pair
;; Create pair with the too values on top of the stack
;;
(define (mlc-pair succ)
  (make-lazy-code
    (lambda (cgc ctx)
       (let ((header-word (mem-header 3 STAG_PAIR)))
         
         ;; Alloc
         (gen-allocation cgc ctx STAG_PAIR 3)
         
         ;; Write object header
         (x86-mov cgc (x86-rax) (x86-imm-int header-word))
         (x86-mov cgc (x86-mem -24 alloc-ptr) (x86-rax))
         (x86-pop cgc (x86-rbx)) ;; pop CDR
         (x86-pop cgc (x86-rax)) ;; pop CAR
         ;; Write pair
         (x86-mov cgc (x86-mem -16 alloc-ptr)  (x86-rax))
         (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rbx))
         ;; Tag,Push closure and update alloc-ptr
         (x86-mov cgc (x86-rax) alloc-ptr)
         (x86-add cgc (x86-rax) (x86-imm-int (- TAG_MEMOBJ 24)))
         (x86-push cgc (x86-rax))
         (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_PAI))))))

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

;; Gen a new cc-table at 'alloc-ptr' and write 'stub-addr' in each slot
(define (gen-cc-table cgc stub-addr offset)
  (x86-mov cgc (x86-rax) (x86-imm-int stub-addr))
  (gen-cc-table-h cgc offset global-cc-table-maxsize))

;; Gen-cc-table helper
(define (gen-cc-table-h cgc offset nb-slots)
  (if (> nb-slots 0)
      (begin (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))
             (gen-cc-table-h cgc (+ offset 8) (- nb-slots 1)))))

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
     (if (cdr r)
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
                                               (let ((res (assoc (car fvars) saved-env)))
                                                  (if (identifier-mutable? (cdr res))
                                                    '(mutable)
                                                    '()))
                                               ;; TODO:
                                               (let* ((res (assoc (car fvars) saved-env)))
                                                 (if (eq? (identifier-type (cdr res)) 'local)
                                                    (let ((idx (- (length saved-stack) 2 (identifier-offset (cdr res)))))
                                                      (list-ref saved-stack idx))
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
         ;; TODO : free vars ctx information
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
  (if (null? lst)
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
  (if (< curr 0)
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

;; Set subtraction with lists
;; return lsta - lstb
;; res is accu
(define (set-sub lsta lstb res)
  (if (null? lsta)
    res
    (if (member (car lsta) lstb)
      (set-sub (cdr lsta) lstb res)
      (set-sub (cdr lsta) lstb (cons (car lsta) res)))))

;; Set union with lists
;; return lsta U lstb
(define (set-union lsta lstb)
  (if (null? lsta)
    lstb
    (if (member (car lsta) lstb)
      (set-union (cdr lsta) lstb)
      (set-union (cdr lsta) (cons (car lsta) lstb)))))

;; Flatten list x
(define (flatten x)
   (cond ((null? x) '())
         ((not (pair? x)) (list x))
         (else (append (flatten (car x))
                       (flatten (cdr x))))))

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
        (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rax))
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem -16 alloc-ptr) (x86-rax))

        ;; Replace local
        (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))
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
               (x86-mov cgc (x86-mem -24 alloc-ptr) (x86-rax))
               ;; Get car from stack (arg slot) and write in pair
               (x86-mov cgc (x86-rax) (x86-mem sp-offset (x86-rsp)))
               (x86-mov cgc (x86-mem -16 alloc-ptr) (x86-rax))
               ;; Get cdr from stack (top of stack) and write in pair
               (x86-pop cgc (x86-rax))
               (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rax))
               ;; Tag & push
               (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 24) alloc-ptr))
               (x86-push cgc (x86-rax)))
             ;; Create next pair
             (gen-rest-lst-h cgc ctx(- pos 1) nb (+ sp-offset 8)))))

;;-----------------------------------------------------------------------------
;; Utils

;; Is the v a literal ?
(define (literal? v)
   (or (char? v) (number? v) (symbol? v) (vector? v) (string? v) (boolean? v) (null? v)))

;; Call n times the function fn with given args
(define (call-n n fn . args)
  (if (> n 0)
    (begin (apply fn args)
           (apply call-n (append (list (- n 1) fn ) args)))))

;; Build a new list of length n and apply proc to each element
(define (build-list n proc)
  (define (build-list-h p proc)
    (if (= p 0)
      '()
      (cons (proc (- n p)) (build-list-h (- p 1) proc))))
  (build-list-h n proc))

;; Count assoc of el in lst
(define (assocount el lst)
  (if (null? lst)
    0
    (let ((c (car lst)))
      (if (equal? (car c) el)
        (+ 1 (assocount el (cdr lst)))
        (assocount el (cdr lst))))))