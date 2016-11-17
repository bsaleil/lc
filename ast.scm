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

(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm") ;; TODO regalloc remove when finished
(include "extern/copy-permanent.scm")

(define free-vars #f)

;;-----------------------------------------------------------------------------

(define perm-domain #f)
(define locat-table #f)

(define (init-frontend)
  (set! locat-table (make-table test: eq?))
  (set! perm-domain (make-perm-domain))
  (init-primitives))

;;-----------------------------------------------------------------------------
;; Macros

(define-macro (assert c err)
  `(if (not ,c)
       (begin
        (println "!!! ERROR : " ,err)
        (exit 1))))

;; Multiple bindings let
;; ex: (let ((a/b/c (foo))) (println a b c))
;;     -> (let ((#sym (foo))
;;              (a (car #sym))
;;              (b (cadr #sym))
;;              (c (caddr #sym)))
;;          (println a b c))
(define-macro (mlet bindings . body)

  (define (string-split str char)
    (define (list-split lst el r)
      (if (null? lst)
          (list (reverse r))
          (if (eq? (car lst) el)
              (cons (reverse r)
                      (list-split (cdr lst)
                                  el
                                  '()))
              (list-split (cdr lst)
                          el
                          (cons (car lst) r)))))
    (map list->string (list-split (string->list str) char '())))

  (define (count fn lst)
    (if (null? lst)
        0
        (if (fn (car lst))
            (+ 1 (count fn (cdr lst)))
            (count fn (cdr lst)))))

  (define (write-bindings bindings)
    (if (null? bindings)
        '()
        (let ((id (caar bindings)))
          (if (> (count (lambda (el) (char=? el #\/))
                        (string->list (symbol->string id)))
                 0)
              (let* ((r (string-split (symbol->string id) #\/))
                     (tmp (gensym)))
                (define (build-subbinding r i)
                  (if (null? r)
                      '()
                      (let ((accessor-sym
                              (string->symbol
                                (string-append
                                  "ca"
                                  (make-string i #\d)
                                  "r"))))
                      (cons `(,(string->symbol (car r)) (,accessor-sym ,tmp))
                            (build-subbinding (cdr r) (+ i 1))))))
                (append `((,tmp ,@(cdar bindings)) ,@(build-subbinding r 0))
                        (write-bindings (cdr bindings))))
              (cons (car bindings) (write-bindings (cdr bindings)))))))

  `(let* ,(write-bindings bindings) ,@body))

;;-----------------------------------------------------------------------------
;; Parsistent data structures

;; Associate an entry object to a function number
;; if opt-entry-points is #t, entry is the cc-table
;; if opt-entry-points is #f, entry is the 1-sized vector which contains ep
(define asc-globalfn-entry (make-table))
(define (asc-globalfn-entry-add fn-num entry)
  (table-set! asc-globalfn-entry fn-num entry))
(define (asc-globalfn-entry-get fn-num)
  (table-ref asc-globalfn-entry fn-num))

;; Keep each constant of the program in a still box
;; allowing the compiler to generate:
;; mov dest, [box]
;; to load the constant
(define cst-table (make-table test: equal?))
(define (cst-get cst)
  (let ((r (table-ref cst-table cst #f)))
    (if r
        (+ (obj-encoding r) (- 8 TAG_MEMOBJ))
        (let* ((box  (alloc-still-vector 1)))
          (vector-set! box 0 cst)
          (table-set! cst-table cst box)
          (+ (obj-encoding box) (- 8 TAG_MEMOBJ))))))

;;
;; entry-object -> stubs
;; Associate a pair generic,stub to an entry object
;; This structure is used to determine if an entry point is a stub
;; address or a version address
;;
(define asc-entry-stub (make-table test: eq?))
;; Add an entry to the table
(define (asc-entry-stub-add cctable generic-addr stub-addr)
  (assert (not (table-ref asc-entry-stub cctable #f))
          "Internal error")
  (table-set! asc-entry-stub cctable (cons generic-addr stub-addr)))
;; Read an entry from the table
(define (asc-entry-stub-get cctable)
  (table-ref asc-entry-stub cctable #f))

;;
;; (entry-obj . idx) -> label list
;; Associate a list of label to a pair entry-obj/idx
;; This structure is used to store all addresses where the compiler generated a
;; direct jump to a stub.
;; When the stub generate a version stored in this entry object
;; it patches all stored labels and clear the table entry
;;
;; idx is the ctx idx if using cctable, 0 otherwise
(define asc-entry-load
  (make-table
    test: (lambda (k1 k2)
            (and (eq? (car k1) (car k2))     ;; eq? on cctables
                 (=   (cdr k1) (cdr k2)))))) ;; = on idx
;; Add an entry to the table
(define (asc-entry-load-add entry-obj ctxidx label)
  (let ((r (table-ref asc-entry-load (cons entry-obj ctxidx) '())))
    (table-set! asc-entry-load (cons entry-obj ctxidx) (cons label r))))
;; Get all labels from entry object and ctxidx
(define (asc-entry-load-get entry-obj ctxidx)
  (table-ref asc-entry-load (cons entry-obj ctxidx) '()))
;; Clear the entry for the entry-object/ctxidx
(define (asc-entry-load-clear entry-obj ctxidx)
  (table-set! asc-entry-load (cons entry-obj ctxidx) '())) ;; TODO: remove table entry

;; Global variables information
(define nb-globals 0)
(define asc-globals (make-table))
(define (asc-globals-add id stype)
  (table-set! asc-globals id (cons nb-globals stype))
  (set! nb-globals (+ nb-globals 1)))
(define (asc-globals-get id)
  (table-ref asc-globals id #f))
(define (global-pos global) (car global))
(define (global-stype global) (cdr global))
(define (global-stype-set! global stype) (set-cdr! global stype))

;;
;; Associate a closure (encoding of scm object) to an entry-obj-loc
;; This asc keeps allocations of constant closures as permanent objects
(define asc-entryobj-globalclo (make-table))
(define (asc-entryobj-globalclo-get entry-obj-loc)
  (table-ref asc-entryobj-globalclo entry-obj-loc #f))
(define (asc-entryobj-globalclo-add entry-obj-loc clo-ptr)
  (assert (not (asc-entryobj-globalclo-get entry-obj-loc))
          "Internal error")
  (table-set! asc-entryobj-globalclo entry-obj-loc clo-ptr))

;;
;; Associate an ep entry object to an ast (eq? on ast)
(define asc-ast-epentry (make-table test: eq?))
(define (asc-ast-epentry-get ast)
  (table-ref asc-ast-epentry ast #f))
(define (asc-ast-epentry-add ast eo)
  (assert (not (asc-ast-epentry-get ast))
          "Internal error")
  (table-set! asc-ast-epentry ast eo))

;;-----------------------------------------------------------------------------
;; Type predicates

(define type-predicates `(
  (output-port? . ,ATX_OPO)
  (input-port?  . ,ATX_IPO)
  (symbol?      . ,ATX_SYM)
  (string?      . ,ATX_STR)
  (char?        . ,ATX_CHA)
  (vector?      . ,ATX_VEC)
  (fixnum?      . ,ATX_INT)
  (flonum?      . ,ATX_FLO)
  (procedure?   . ,ATX_CLO)
  (pair?        . ,ATX_PAI)
  (null?        . ,ATX_NUL)
))

(define (type-predicate? sym)
  (assq sym type-predicates))

;; WARNING: this function return constant instance ATX_* do not modify it!
(define (predicate->ctx-type predicate)
  (let ((r (assq predicate type-predicates)))
    (if r
      (cdr r)
      (error ERR_INTERNAL))))

;;-----------------------------------------------------------------------------
;; Primitives

(define (primitive-get sym)            (assoc sym primitives))
(define (primitive-sym prim)           (list-ref  prim 0))
(define (primitive-lco-cst prim)       (list-ref  prim 1))
(define (primitive-lcofun prim)        (list-ref  prim 2))
(define (primitive-codegen prim)       (list-ref  prim 3))
(define (primitive-rettype prim)       (list-ref  prim 4))
(define (primitive-nbargs prim)        (list-ref  prim 5))
(define (primitive-argtypes prim)      (list-tail prim 6))

;; TODO: remove or add ?/! from codegen-p functions
;; * Most primitives are "simple", they only need to call code generator.
;;   A "simple" primitive has only a codegen function and no lco getter
;; * More complex primitives need more complex work.
;;   A "complex" primitive has no codegen function but a lco getter that is
;;   used to guild lco chain and return first lco of the chain
;; TODO CST APPLY ?

(define primitives '())
(define (init-primitives)
  (set! primitives `(
    ;; Symbol            LCO cst all    LCO getter        Codegen function          Return-type Nb-args Args-type
    (car                 ,cst-car       #f                ,codegen-p-cxr            ,ATX_UNK 1 ,ATX_PAI                   )
    (cdr                 ,cst-cdr       #f                ,codegen-p-cxr            ,ATX_UNK 1 ,ATX_PAI                   )
    (cons                #f             #f                ,codegen-p-cons           ,ATX_PAI 2 ,ATX_ALL ,ATX_ALL          )
    (eq?                 ,cst-eq?       ,lco-p-eq?        #f                        ,ATX_BOO 2 ,ATX_ALL ,ATX_ALL          )
    (char=?              ,dummy-cst-all ,lco-p-char=?     #f                        ,ATX_BOO 2 ,ATX_CHA ,ATX_CHA          )
    (quotient            ,cst-binop     ,lco-p-binop      #f                        ,ATX_INT 2 ,ATX_INT ,ATX_INT          )
    (modulo              ,cst-binop     ,lco-p-binop      #f                        ,ATX_INT 2 ,ATX_INT ,ATX_INT          )
    (remainder           ,dummy-cst-all ,lco-p-binop      #f                        ,ATX_INT 2 ,ATX_INT ,ATX_INT          )
    (zero?               ,dummy-cst-all ,lco-p-zero?      #f                        ,ATX_BOO 1 ,ATX_NUM                   )
    (not                 ,cst-not       #f                ,codegen-p-not            ,ATX_BOO 1 ,ATX_ALL                   )
    (set-car!            #f             #f                ,codegen-p-set-cxr!       ,ATX_VOI 2 ,ATX_PAI ,ATX_ALL          )
    (set-cdr!            #f             #f                ,codegen-p-set-cxr!       ,ATX_VOI 2 ,ATX_PAI ,ATX_ALL          )
    (vector-length       ,dummy-cst-all #f                ,codegen-p-vector-length  ,ATX_INT 1 ,ATX_VEC                   )
    (vector-ref          ,dummy-cst-all #f                ,codegen-p-vector-ref     ,ATX_UNK 2 ,ATX_VEC ,ATX_INT          )
    (char->integer       ,dummy-cst-all #f                ,codegen-p-ch<->int       ,ATX_INT 1 ,ATX_CHA                   )
    (integer->char       ,cst-int->char #f                ,codegen-p-ch<->int       ,ATX_CHA 1 ,ATX_INT                   )
    (string-ref          ,dummy-cst-all #f                ,codegen-p-string-ref     ,ATX_CHA 2 ,ATX_STR ,ATX_INT          )
    (string-set!         #f             #f                ,codegen-p-string-set!    ,ATX_VOI 3 ,ATX_STR ,ATX_INT ,ATX_CHA )
    (vector-set!         #f             #f                ,codegen-p-vector-set!    ,ATX_VOI 3 ,ATX_VEC ,ATX_INT ,ATX_ALL )
    (string-length       ,dummy-cst-all #f                ,codegen-p-string-length  ,ATX_INT 1 ,ATX_STR                   )
    (exit                #f             #f                 #f                       ,ATX_VOI 0                            )
    (make-vector         #f             #f                ,codegen-p-make-vector    ,ATX_VEC 2 ,ATX_INT ,ATX_ALL          )
    (make-string         #f             #f                ,codegen-p-make-string    ,ATX_STR 2 ,ATX_INT ,ATX_CHA          )
    (eof-object?         ,dummy-cst-all #f                ,codegen-p-eof-object?    ,ATX_BOO 1 ,ATX_ALL                   )
    (symbol->string      ,dummy-cst-all #f                ,codegen-p-symbol->string ,ATX_STR 1 ,ATX_SYM                   )
    (current-output-port #f             ,lco-p-cur-x-port #f                        ,ATX_OPO 0                            )
    (current-input-port  #f             ,lco-p-cur-x-port #f                        ,ATX_IPO 0                            )
    (number?             ,cst-number?   ,lco-p-number?    #f                        ,ATX_BOO 1 ,ATX_ALL                   )
    (##apply             #f             ,lco-p-apply      #f                        ,ATX_UNK 2 ,ATX_CLO ,ATX_ALL          )
    (##box               #f             #f                ,codegen-p-box            ,ATX_BOX 1 ,ATX_ALL                   )
    (##unbox             #f             #f                ,codegen-p-unbox          ,ATX_UNK 1 ,ATX_ALL                   )
    (##set-box!          #f             #f                ,codegen-p-set-box        ,ATX_VOI 2 ,ATX_ALL ,ATX_ALL          )
    (##gettime-ns        #f             #f                ,codegen-p-gettime-ns     ,ATX_INT 0                            )
    (vector              #f             ,lco-p-vector     #f                        ,ATX_VEC #f                           )
    (list                #f             ,lco-p-list       #f                        ,ATX_PAI #f                           )
    ;; These primitives are inlined during expansion but still here to build lambda
    (real?               ,dummy-cst-all #f                #f                        ,ATX_BOO 1 ,ATX_ALL                   )
    (eqv?                ,dummy-cst-all #f                #f                        ,ATX_BOO 2 ,ATX_ALL ,ATX_ALL          ))))

(define (assert-p-nbargs sym ast)
  (let ((prim (primitive-get sym)))
    (assert (or (not (primitive-nbargs prim))
                (and (= (length (cdr ast))
                        (primitive-nbargs prim))))
            ERR_WRONG_NUM_ARGS)))

;;-----------------------------------------------------------------------------
;; AST SPECIAL NODES

(define (atom-node-make val)
  (list '$$atom val))

(define (atom-node? n)
  (and (pair? n)
       (eq? (car n) '$$atom)))

(define (atom-node-val n)
  (cadr n))

;;-----------------------------------------------------------------------------
;; AST DISPATCH

;; Gen lazy code from a list of exprs
(define (gen-ast-l lst succ)
  (foldr (lambda (el r) (gen-ast el r)) succ lst))

;; Gen lazy code from ast
(define (gen-ast ast succ)

  (assert (pair? ast) "Internal error")

  (let ((op (car ast)))
    (cond
      ;; Atom node
      ((atom-node? ast) (mlc-atom ast succ))
      ;; Special forms
      ((eq? op 'begin)  (mlc-begin ast succ))
      ((eq? op 'define) (mlc-define ast succ))
      ((eq? op 'if)     (mlc-if ast succ))
      ((eq? op 'lambda) (mlc-lambda-ast ast succ))
      ((eq? op 'let)    (mlc-let ast succ)) ;; Also handles let* (let* is a macro)
      ((eq? op 'letrec) (mlc-letrec ast succ))
      ((eq? op 'set!)   (mlc-set! ast succ))
      ;; Known operator
      ((atom-node? op)
         (let ((val (atom-node-val op)))
           (cond
             ;;
             ((primitive-get val)   (mlc-primitive val ast succ))
             ((type-predicate? val) (mlc-test val ast succ))
             ((gambit-call? op)     (mlc-gambit-call ast succ #f))
             ((member val '(+ - * < > <= >= = /)) (mlc-op-n ast succ val))
             ;; Call
             (else (mlc-call ast succ)))))
      ;; Call expr
      (else (mlc-call ast succ)))))

;;-----------------------------------------------------------------------------
;; ATOM

(define (mlc-atom ast succ)
  (let ((val (atom-node-val ast)))
    (cond ((string? val)          (mlc-string val ast succ))
          ((symbol? val)          (mlc-identifier val ast succ))
          ((compiler-flonum? val) (mlc-literal (exact->inexact val) ast succ))
          ((literal? val)         (mlc-literal val ast succ))
          ((and (pair? val)
                (eq? (car val) 'quote))
             (mlc-literal (cadr val) ast succ))
          (else (error "Internal error (mlc-atom)")))))

;;-----------------------------------------------------------------------------
;; LITERALS

(define (compiler-flonum? n)
 (and (number? n)
      (or (flonum? n)                            ;; 3.3
          (and (integer? n) (inexact? n))        ;; 3.
          (and (not (integer? n)) (exact? n))))) ;; (/ 10 3)

;;
;; Make lazy code from num/bool/char/null literal
;;
(define (mlc-literal lit ast succ)

  (if (and (integer? lit)
           (not (fixnum? lit)))
      ;; Bignum, fall back to flonum
      (set! lit (exact->inexact lit)))

  (make-lazy-code
    #f
    (lambda (cgc ctx)
      (let ((literal
              (if (and (##mem-allocated? lit)
                       (not (eq? (mem-allocated-kind lit) 'PERM)))
                  (copy-permanent lit #f perm-domain)
                  lit)))
        (let ((ctx (ctx-push ctx (literal->ctx-type literal) #f)))
          (jump-to-version cgc succ ctx))))))

;;
;; Make lazy code from string literal
;;
(define (mlc-string str ast succ)
  (make-lazy-code
    #f
    (lambda (cgc ctx)
      (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
        (apply-moves cgc ctx moves)
        (codegen-string cgc str reg)
        (jump-to-version cgc succ (ctx-push ctx (make-ctx-tstr) reg))))))

;;-----------------------------------------------------------------------------
;; VARIABLES GET

;;
;; Make lazy code from SYMBOL
;;
(define (mlc-identifier sym ast succ)

  (define next-is-cond (member 'cond (lazy-code-flags succ)))

  (define (inlined-cond? type-fn)
    (and next-is-cond
         (let ((type (type-fn)))
           (and
             type
             (not (ctx-tboo? type))     ;; If it's a bool, we must check the value
             (not (ctx-tunk? type)))))) ;; If it's a unk, we must check the type

  (define (lcl-inlined-cond? ctx identifier)
    (inlined-cond? (lambda () (ctx-identifier-type ctx identifier))))

  (define (gbl-inlined-cond? id)
    (inlined-cond? (lambda ()
                     (let ((r (asc-globals-get id)))
                       (and r (global-stype r))))))

  (make-lazy-code
    #f
    (lambda (cgc ctx)

      (let ((local  (assoc sym (ctx-env ctx)))
            (global (asc-globals-get sym)))

        ;;
        (cond ;; Identifier local or global and inlined condition
              ((or (and local  (lcl-inlined-cond? ctx (cdr local))) ;; TODO wip add bool cst
                   (and global (gbl-inlined-cond? sym)))
                (jump-to-version cgc (lazy-code-lco-true succ) ctx))
              ;; Identifier local and cst literal
              ((and local
                    (ctx-type-is-cst (ctx-identifier-type ctx (cdr local))))
                 ;; TODO use =>
                 (let* ((cst (ctx-type-cst (ctx-identifier-type ctx (cdr local))))
                        (ctx (ctx-push ctx (ctx-identifier-type ctx (cdr local)) #f sym)))
                   (jump-to-version cgc succ ctx)))
              ;; Identifier is a local variable
              (local
                (gen-get-localvar cgc ctx local succ))
              ;; Identifier is a global variable
              (global
                (gen-get-globalvar cgc ctx global succ))
              ;; Vector
              ((eq? sym 'vector)
                 (let* ((node-lv (atom-node-make 'list->vector))
                        (node-l  (atom-node-make 'l))
                        (lco     (gen-ast `(lambda l (,node-lv ,node-l)) succ)))
                   (jump-to-version cgc lco ctx)))
              ;; List
              ((eq? sym 'list)
                 (let* ((node (atom-node-make 'n))
                        (lco  (gen-ast `(lambda n ,node) succ)))
                   (jump-to-version cgc lco ctx)))
              ;; Primitive
              ((primitive-get sym) =>
                 (lambda (r)
                   (let ((ast
                           ;; primitive with fixed number of args
                           (let* ((args (build-list (primitive-nbargs r) (lambda (x) (string->symbol (string-append "arg" (number->string x))))))
                                  (args-nodes (map atom-node-make args)))
                             `(lambda ,args (,ast ,@args-nodes)))))
                     (jump-to-version cgc (gen-ast ast succ) ctx))))
              (else (gen-error cgc (ERR_UNKNOWN_VAR sym))))))))

(define (gen-closure-from-cst cgc ctx local succ)
  (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0))
         (stype (ctx-identifier-type ctx (cdr local)))
         (fn-num (ctx-type-cst stype))
         (entry-obj (asc-globalfn-entry-get fn-num)))
    (apply-moves cgc ctx moves)
    (gen-closure cgc reg #f entry-obj '())
    (jump-to-version cgc succ (ctx-push ctx (make-ctx-tclo) reg (car local)))))

(define (gen-get-localvar cgc ctx local succ)

  (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0))
         (loc (ctx-identifier-loc ctx (cdr local)))
         (type (ctx-identifier-type ctx (cdr local))))

    (apply-moves cgc ctx moves)

    (if (ctx-loc-is-freemem? loc)
        ;; It's a free var that is only in closure
        (let ((lclo (ctx-get-closure-loc ctx)))
          (codegen-get-free cgc (ctx-fs ctx) reg lclo loc))
        ;; The variable is in a register or in non closure memory
        (apply-moves cgc ctx (list (cons loc reg))))

    (if (and (ctx-type-is-cst type)
             reg)
        (error "NYI")) ;; TODO: Free a register only if type is not a cst
    (jump-to-version cgc succ (ctx-push ctx type reg (car local)))))

(define (gen-get-globalvar cgc ctx global succ)

  (let ((type (or (global-stype global) (make-ctx-tunk))))

    (if (ctx-type-is-cst type)
        ;; Type is cst, push cst to ctx
        (jump-to-version cgc succ (ctx-push ctx type #f))
        ;; Type is not a cst, free a register and use it
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
          (apply-moves cgc ctx moves)
          (codegen-get-global cgc (global-pos global) reg)
          (jump-to-version cgc succ (ctx-push ctx type reg))))))

;;-----------------------------------------------------------------------------
;; VARIABLES SET

;;
;; Make lazy code from SET!
;;
(define (mlc-set! ast succ)

  (let* ((id (cadr ast))
         (lazy-set!
           (make-lazy-code
             #f
             (lambda (cgc ctx)
               (let ((global (asc-globals-get id)))
                 (if global
                     (gen-set-globalvar cgc ctx global succ)
                     (error "Internal error"))))))
         (lazy-drop
           (make-lazy-code
             #f
             (lambda (cgc ctx)
               (let ((ctx (drop-cst-value cgc ctx 0)))
                 (jump-to-version cgc lazy-set! ctx))))))

    (gen-ast (caddr ast) lazy-drop)))

(define (gen-set-globalvar cgc ctx global succ)
  (mlet ((pos (global-pos global))
         (moves/reg/ctx (ctx-get-free-reg ctx succ 1))
         (tval (ctx-get-type ctx 0))
         (cst? (ctx-type-is-cst tval))
         (lval (if cst?
                   (ctx-type-cst tval)
                   (ctx-get-loc ctx 0))))
    (apply-moves cgc ctx moves)
    (codegen-set-global cgc (ctx-fs ctx) reg pos lval cst?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tvoi) reg))))

;;-----------------------------------------------------------------------------
;; INTERNAL FORMS

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)

  (let ((global (asc-globals-get (cadr ast))))
    (cond
          ((and global (ctx-tclo? (global-stype global)))
            ;; CONST FN TODO: remove when const versioning implemented!
            (let* ((identifier (cadr ast))
                   (fn-num (init-entry-cst (caddr ast) '() (ctx-init))))
              (ctx-type-cst-set! (global-stype global) fn-num)
              (make-lazy-code
                #f
                (lambda (cgc ctx)
                  (jump-to-version cgc succ (ctx-push ctx (global-stype global) #f))))))
          (else
            ;;
            (let* ((identifier (cadr ast))
                   (lazy-bind (make-lazy-code
                                #f
                                (lambda (cgc ctx)

                                  (mlet ((pos (global-pos (asc-globals-get identifier))) ;; Lookup in globals
                                         ;;
                                         (moves/reg/ctx (ctx-get-free-reg ctx succ 1))
                                         (type (ctx-get-type ctx 0))
                                         (cst? (ctx-type-is-cst type))
                                         (lvalue (if cst?
                                                     (ctx-type-cst type)
                                                     (ctx-get-loc ctx 0))))
                                    (apply-moves cgc ctx moves)
                                    (codegen-define-bind cgc (ctx-fs ctx) pos reg lvalue cst?)
                                    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tvoi) reg))))))
                   (lazy-drop
                     (make-lazy-code
                      #f
                       (lambda (cgc ctx)
                         (let ((type (ctx-get-type ctx 0)))
                           (if (and (ctx-type-is-cst type)
                                    (ctx-tclo? type))
                               (let ((ctx (drop-cst-value cgc ctx 0)))
                                 (jump-to-version cgc lazy-bind ctx))
                               (jump-to-version cgc lazy-bind ctx))))))
                   (lazy-val (gen-ast (caddr ast) lazy-drop)))

              (put-i64 (+ globals-addr (* 8 (global-pos (asc-globals-get (cadr ast))))) ENCODING_VOID)
              lazy-val)))))

;;
;; Make lazy code from LAMBDA
;;

;;
;; Create and return a generic prologue lco
(define (get-lazy-generic-prologue ast succ rest-param nb-formal)
  (make-lazy-code-entry
    #f
    (lambda (cgc ctx)
      (let ((nb-args (ctx-nb-args ctx))
            (label-next (asm-make-label #f (new-sym 'label-next))))
        ;;
        (if (not rest-param)
            (codegen-prologue-gen-nrest cgc nb-args)
            (codegen-prologue-gen-rest  cgc (ctx-fs ctx) nb-args))
        ;;
        (jump-to-version cgc succ ctx)))))

;;
;; Create and return a prologue lco
(define (get-lazy-prologue ast succ rest-param)
  (make-lazy-code-entry
    #f
    (lambda (cgc ctx)
      (let* ((nb-actual (ctx-nb-actual ctx))
             (nb-formal (ctx-nb-args ctx)))

        (cond ;; rest AND actual == formal
              ((and rest-param (= nb-actual (- nb-formal 1))) ;; -1 rest
               (set! ctx (ctx-push ctx (make-ctx-tnul #t '()) #f))
               (let ((reg
                       (if (<= nb-formal (length args-regs))
                           (list-ref args-regs (- nb-formal 1))
                           #f)))
                 (codegen-prologue-rest= cgc reg)
                 (jump-to-version cgc succ ctx)))
              ;; rest AND actual > formal
              ;; TODO merge > and == (?)
              ((and rest-param (> nb-actual (- nb-formal 1)))
               (let* ((nb-extra (- nb-actual (- nb-formal 1)))
                      (nctx (ctx-pop-n ctx (- nb-extra 1)))
                      (nctx (ctx-set-type nctx 0 (make-ctx-tpai) #f)))
                 (set! ctx nctx)
                 (let* ((nb-formal-stack
                          (if (> (- nb-formal 1) (length args-regs))
                              (- nb-formal 1 (length args-regs))
                              0))
                        (r
                          (if (<= nb-actual (length args-regs))
                              0
                              (- nb-actual (length args-regs))))
                        (nb-rest-stack (- r nb-formal-stack))
                        (rest-regs
                          (if (>= (- nb-formal 1) (length args-regs))
                              '()
                              (list-head
                                (list-tail args-regs (- nb-formal 1))
                                (- nb-actual nb-rest-stack nb-formal -1))))
                        (reg
                          (if (<= nb-formal (length args-regs))
                              (list-ref args-regs (- nb-formal 1))
                              #f)))

                   (codegen-prologue-rest>
                     cgc
                     (ctx-fs ctx)
                     nb-rest-stack
                     (reverse rest-regs)
                     reg))

                 (jump-to-version cgc succ ctx)))
              ;; (rest AND actual < formal) OR (!rest AND actual < formal) OR (!rest AND actual > formal)
              ((or (< nb-actual nb-formal) (> nb-actual nb-formal))
               (gen-error cgc ERR_WRONG_NUM_ARGS))
              ;; Else, nothing to do
              (else
                 (jump-to-version cgc succ ctx)))))))

;;
;; Create and return a function return lco
(define (get-lazy-return)

  (define lazy-ret
    (make-lazy-code-ret ;; Lazy-code with 'ret flag
      #f
      (lambda (cgc ctx)
        (let* ((fs (ctx-fs ctx))
               ;; Return value loc
               (type-ret (ctx-get-type ctx 0))
               (lret     (ctx-get-loc ctx 0))
               ;; Return address object loc
               (laddr (ctx-get-retobj-loc ctx)))

          (assert (not (ctx-type-is-cst type-ret)) "Internal error")

          ;; Gen return
          (if opt-return-points
              (let* ((crtable-offset (ctx-type->cridx type-ret)))
                (codegen-return-cr cgc fs fs laddr lret crtable-offset))
              (codegen-return-rp cgc fs fs laddr lret))))))
  ;; TODO: write a generic lazy-drop function (this function is used by multiple mlc-*)
  (make-lazy-code-ret
    #f
    (lambda (cgc ctx)
      (let ((ctx (drop-cst-value cgc ctx 0)))
        (jump-to-version cgc lazy-ret ctx)))))

;;
;; Create fn entry stub
(define (create-fn-stub ast fn-num fn-generator)

  ;; Function use rest param ?
  (define rest-param (or (and (not (list? (cadr ast))) (not (pair? (cadr ast)))) ;; (foo . rest)
                         (and (pair? (cadr ast)) (not (list? (cadr ast)))))) ;; (foo a..z . rest)
  ;; List of formal params
  (define params
    (if rest-param
        (formal-params (cadr ast))
        (cadr ast)))
  ;; Lazy lambda return
  (define lazy-ret (get-lazy-return))
  ;; Lazy lambda body
  (define lazy-body (gen-ast (caddr ast) lazy-ret))
  ;; Lazy function prologue
  (define lazy-prologue (get-lazy-prologue ast lazy-body rest-param))
  ;; Same as lazy-prologue but generate a generic prologue (no matter what the arguments are)
  (define lazy-prologue-gen (get-lazy-generic-prologue ast lazy-body rest-param (length params)))

  (add-fn-callback
    1
    fn-num
    (lambda (stack ret-addr selector closure)

      (cond ;; CASE 1 - Use entry point (no cctable)
            ((eq? opt-entry-points #f)
               (fn-generator closure lazy-prologue-gen #f #f))
            ;; CASE 2 - Function is called using generic entry point
            ((= selector 1)
               (fn-generator #f lazy-prologue-gen #f #t))
            ;; CASE 3 - Use multiple entry points AND use max-versions limit AND this limit is reached
            ((and opt-max-versions
                  (>= (lazy-code-nb-versions lazy-prologue) opt-max-versions))
               (fn-generator #f lazy-prologue-gen stack #t))
            ;; CASE 4 - Use multiple entry points AND limit is not reached or there is no limit
            (else
               (fn-generator #f lazy-prologue stack #f))))))

(define (get-entry-obj ast ctx fn-num fvars-imm fvars-late all-params bound-id)
  ;; Generator used to generate function code waiting for runtime data
  ;; First create function entry ctx
  ;; Then generate function prologue code
  (define (fn-generator closure prologue stack generic?)
    ;; In case the limit in the number of version is reached, we give #f to ctx-init-fn to get a generic ctx
    ;; but we still want to patch cctable at index corresponding to stack
    (let* ((ctx-stack (if generic? #f stack))
           (ctx (ctx-init-fn ctx-stack ctx all-params (append fvars-imm fvars-late) fvars-late fn-num bound-id)))
      (gen-version-fn ast closure entry-obj prologue ctx stack generic?)))

  ;; ---------------------------------------------------------------------------
  ;; Return 'entry-obj' (entry object)
  ;; An entry object is the object that contains entry-points-locs
  ;; In the case of -cc, entry object is the cctable
  (define (get-entry-obj-cc)
    (let* (;; Is the cctable new or existed before ?
           (cctable-new? #f)
           (cctable-key (get-cctable-key ast ctx fvars-imm fvars-late))
           (cctable     (let ((table (cctable-get cctable-key)))
                          (or table
                              (begin (set! cctable-new? #t) (cctable-make cctable-key))))))
      ;; The compiler needs to fill cctable if it is a new cctable
      (if cctable-new?
          (let* (;; Create stub
                 (stub-labels  (create-fn-stub ast fn-num fn-generator))
                 (stub-addr    (asm-label-pos (list-ref stub-labels 0)))
                 (generic-addr (asm-label-pos (list-ref stub-labels 1))))
            ;; Add cctable->stub-addrs assoc
            (asc-entry-stub-add cctable generic-addr stub-addr)
            (cctable-fill cctable stub-addr generic-addr)))
      cctable))
  ;; In the case of -ep, entry object is the still vector of size 1 that contain the single entry point
  (define (get-entry-obj-ep)
    (let ((existing (asc-ast-epentry-get ast)))
      (if existing
          ;; An entry points object already exists for this ast, use it!
          existing
          ;; TODO: we are supposed to use only one e.p. with -ep objects
          ;;       use a max-selector of 0 in create-fn-stub, and use only one -addr
          (let* (;; Create stub
                 (stub-labels  (create-fn-stub ast fn-num fn-generator))
                 (stub-addr    (asm-label-pos (list-ref stub-labels 0)))
                 (generic-addr (asm-label-pos (list-ref stub-labels 1)))
                 (entryvec (get-entry-points-loc ast stub-addr)))
            (asc-entry-stub-add entryvec generic-addr stub-addr)
            (asc-ast-epentry-add ast entryvec)
            entryvec))))

  (define entry-obj #f)

  (set! entry-obj
    (if opt-entry-points
        (get-entry-obj-cc)
        (get-entry-obj-ep)))

  entry-obj)


;;
;; Init constant lambda
(define (init-entry-cst ast free ctx)

  ;; Flatten list of param (include rest param)
  (define all-params (flatten (cadr ast)))

  (letrec (;; Closure unique number
           (fn-num (new-fn-num))
           (entry-obj (get-entry-obj ast ctx fn-num free '() all-params #f))
           (entry-obj-loc (- (obj-encoding entry-obj) 1)))

      ;; Add association fn-num -> entry point
      (asc-globalfn-entry-add fn-num entry-obj)

      ;; Return lambda identity
      fn-num))

;;
;; Init non constant lambda
(define (init-entry ast ctx fvars-imm fvars-late bound-id)

  ;; Flatten list of param (include rest param)
  (define all-params (flatten (cadr ast)))

  (letrec (;; Closure unique number
           (fn-num (new-fn-num))
           (entry-obj (get-entry-obj ast ctx fn-num fvars-imm fvars-late all-params bound-id))
           (entry-obj-loc (- (obj-encoding entry-obj) 1)))

      ;; Add association fn-num -> entry point
      (asc-globalfn-entry-add fn-num entry-obj)

      (list fn-num entry-obj)))

;; Compute free var sets for given lambda ast
;; return list (cst imm late) with:
;; cst:  list of cst free variables
;; imm:  immediate non cst free variables
;; late: late free variables
(define (get-free-infos ast bound-ids ctx)

  (let* ((body (caddr ast))
         (params (cadr ast))
         (enc-ids (append bound-ids (map car (ctx-env ctx))))
         (free-ids (free-vars body params enc-ids)))

    (define late (set-inter bound-ids free-ids))

    (let loop ((ids (set-sub free-ids late '()))
               (imm '())
               (cst '()))
      (if (null? ids)
          (list cst imm late)
          (let ((id (car ids)))
            (let* ((identifier (cdr (assoc id (ctx-env ctx))))
                   (type (ctx-identifier-type ctx identifier)))
              (if (ctx-type-is-cst type)
                  (loop (cdr ids) imm (cons id cst))
                  (loop (cdr ids) (cons id imm) cst))))))))

(define (mlc-lambda-ast ast succ)

  (make-lazy-code
    #f
    (lambda (cgc ctx)

      (define all-params (flatten (cadr ast)))

      (define fvars-imm
              (free-vars
                (caddr ast)
                all-params
                (map car (ctx-env ctx))))

      (define fvars-ncst (ctx-ids-keep-non-cst ctx fvars-imm))

      (mlet ((fn-num/entry-obj (init-entry ast ctx fvars-imm '() #f)))

        (if (null? fvars-ncst)

            ;; Cst function, add information to ctx
            (let* ((type (make-ctx-tclo #t fn-num))
                   (ctx  (ctx-push ctx type #f)))
              (jump-to-version cgc succ ctx))

            ;; Gen code to create closure
            (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
              (apply-moves cgc ctx moves)
              (gen-closure cgc reg ctx entry-obj fvars-ncst)
              ;;
              (jump-to-version cgc succ (ctx-push ctx (make-ctx-tclo) reg))))))))

;;
(define (gen-closure cgc reg ctx entry-obj fvars-ncst)

  (define entry-obj-loc (- (obj-encoding entry-obj) 1))

  ;; If 'stats' option, then inc closures slot
  (if opt-stats
    (gen-inc-slot cgc 'closures))

  (if (null? fvars-ncst)
      (gen-global-closure cgc reg ctx entry-obj entry-obj-loc)
      (gen-local-closure cgc reg ctx entry-obj-loc fvars-ncst)))

;; Create a global closure, and load it in dest register
(define (gen-global-closure cgc reg ctx entry-obj entry-obj-loc)

  ;; If a closure already is generated for a given entry-obj, return it
  ;; else, alloc new permanent closure, add it to asc and return it
  (define (get-closure-ptr)
    (let ((r (asc-entryobj-globalclo-get entry-obj-loc)))
      (or r
          (let* ((obj (alloc-perm-procedure))
                 (enc (obj-encoding obj)))
            (if opt-entry-points
                (put-i64 (+ (- enc TAG_MEMOBJ) OFFSET_BODY) entry-obj-loc)
                (put-i64 (+ (- enc TAG_MEMOBJ) OFFSET_BODY) (get-i64 (+ entry-obj-loc 8))))
            (asc-entryobj-globalclo-add entry-obj-loc enc)
            enc))))

  ;; The closure is constant so we only need to load closure ptr in dest register
  (let ((ptr (get-closure-ptr))
        (dest (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int ptr))))

;; Create a local closure, and load it in dest register
(define (gen-local-closure cgc reg ctx entry-obj-loc fvars-ncst)

  ;;
  (define close-length (length fvars-ncst))

  ;; Create closure
  ;; Closure size = length of free variables
  (codegen-closure-create cgc close-length)

  ;; Write entry point or cctable location
  (if opt-entry-points
      ;; If opt-entry-points generate a closure using cctable
      (codegen-closure-cc cgc entry-obj-loc close-length)
      ;; Else, generate a closure using a single entry point
      (codegen-closure-ep cgc entry-obj-loc close-length))

  ;; Write free variables
  (let* ((free-offset (* -1 (length fvars-ncst))))
    (gen-free-vars cgc fvars-ncst ctx free-offset alloc-ptr))

    ;; Put closure
    (codegen-closure-put cgc reg close-length))

;;
;; Make lazy code from BEGIN
;;
(define (mlc-begin ast succ)

  (define (build-chain exprs succ)
    (cond ((null? exprs)
           succ)
          ;; Last expr
          ((eq? (length exprs) 1)
           (gen-ast (car exprs) succ))
          ;; Not last expr, eval then free loc
          (else
             (let ((next (build-chain (cdr exprs) succ)))
               (gen-ast (car exprs)
                        (make-lazy-code
                          #f
                          (lambda (cgc ctx)
                            (jump-to-version cgc next (ctx-pop ctx)))))))))

  (cond ;; There is no body
        ((null? (cdr ast))
         (error ERR_BEGIN))
        ;; Only one body
        ((= (length (cdr ast)) 1)
         (gen-ast (cadr ast) succ))
        ;; >1 body
        (else (build-chain (cdr ast) succ))))

;;-----------------------------------------------------------------------------
;; LET Binding

(define (mlc-let ast succ)

  (define bindings (cadr ast))
  (define ids (map car bindings))
  (define values (map cadr bindings))
  (define body (caddr ast))

  (define (build-id-idx)
    (let loop ((ids ids)
               (idx (- (length ids) 1))
               (r '()))
      (if (null? ids)
          r
          (let ((id (car ids)))
            (loop (cdr ids) (- idx 1) (cons (cons id idx) r))))))

  (let* ((lazy-let-out (get-lazy-lets-out ast ids 0 succ))
         (lazy-body (gen-ast body lazy-let-out))
         (lazy-bind
           (make-lazy-code
             #f
             (lambda (cgc ctx)
               (let ((ctx (ctx-bind-locals ctx (build-id-idx))))
                 (jump-to-version cgc lazy-body ctx))))))

    (gen-ast-l values lazy-bind)))

;;-----------------------------------------------------------------------------
;; LETREC Binding

(define (mlc-letrec ast succ)

  ;; Letrec info
  (define ids (map car (cadr ast)))
  (define bindings (cadr ast))
  (define body (caddr ast))

  ;; ---------------------------------------------------------------------------
  ;; ANALYSES

  ;; Group bindings, return list (proc-vars const-proc-var other-vars) with:
  ;; proc-vars represents procedure bindings, with the form (id pos ast free-info)
  ;; const-proc-vars represents constant procedure bindings, with the form (id ast free-info)
  ;; other-vars represents non procedure bindings, with the forms (id ast)
  (define (group-bindings ctx)
    (mlet ((proc/other      (group-proc-others ctx ids bindings))
           (const-proc/proc (group-const-proc proc))
           (const-proc/proc (group-recursive-const-proc const-proc proc))
           (proc            (proc-vars-insert-pos proc (map car const-proc))))
        (list proc const-proc other)))

  ;; Take proc-vars objects.
  ;; For each proc-var object, compute pos field and insert it in object
  ;; (id ast free-info) -> (id pos ast free-info)
  (define (proc-vars-insert-pos proc-vars const-proc-vars-ids)
    (let loop ((l proc-vars) (res '()))
       (if (null? l)
           res
           (let* (;; Compute proc info object (id pos ast free-info)
                  ;; pos is the position of the closure in the big allocated object
                  (obj (car l))
                  (id (car obj))
                  (pos (get-closures-size res))
                  (ast (cadr obj))
                  (old-free-info (caddr obj))
                  (free-info
                    (list (append (car old-free-info)
                                  (set-inter const-proc-vars-ids
                                             (caddr old-free-info)))
                          (cadr old-free-info)
                          (set-sub (caddr old-free-info) const-proc-vars-ids '()))))
             (loop (cdr l)
                   (cons (list id pos ast free-info)
                         res))))))

  ;; Group bindings, return list (proc-vars other-vars)
  ;; proc-vars represent procedure bindings, with (id free-info val)
  ;; other-vars represent other bindings (id ast)
  (define (group-proc-others ctx bound-ids bindings)
    (let loop ((bindings bindings)
               (proc-vars '())
               (other-vars '()))
      (if (null? bindings)
          (list proc-vars other-vars)
          (let* ((binding (car bindings))
                 (id (car binding))
                 (val (cadr binding)))
            (if (and (pair? val)
                     (eq? (car val) 'lambda))
                (loop (cdr bindings)
                      (let ((free-info (get-free-infos val bound-ids ctx)))
                        (cons (list id val free-info) proc-vars))
                      other-vars)
                (loop (cdr bindings)
                      proc-vars
                      (cons binding other-vars)))))))

  ;; Group proc-vars, return list (const-proc-vars proc-vars)
  ;; const-proc-vars represent constant procedure bindings
  ;; proc-vars represent non constant procedure bindings
  (define (group-const-proc proc-vars)

    (let loop ((const-proc-vars '())
               (proc-vars proc-vars))

      (let loop2 ((l proc-vars)
                  (new-const-proc-vars const-proc-vars)
                  (new-proc-vars '()))
        (if (null? l)
            ;; Fixed point condition, if there is a change, continue else stop and return
            (if (= (length const-proc-vars)
                   (length new-const-proc-vars))
                (list new-const-proc-vars new-proc-vars)
                (loop new-const-proc-vars new-proc-vars))
            (let* ((proc-var (car l))
                   (free-info (caddr proc-var))
                   (all-free (append (cadr free-info) (caddr free-info))))
              (if (= (length (set-inter all-free
                                        (map car const-proc-vars)))
                     (length all-free))
                  (loop2 (cdr l) (cons proc-var new-const-proc-vars) new-proc-vars)
                  (loop2 (cdr l) new-const-proc-vars (cons proc-var new-proc-vars))))))))

  ;; Use and refine the result of group-const-proc
  ;; Fixed point to find recursively const procedures in bindigs refering others const bindings
  (define (group-recursive-const-proc const-proc-vars proc-vars)

    ;; Init fixed point set is all proc-vars with no imm free (only cst and late)
    (define const-init
      (keep (lambda (el)
              (null? (cadr (caddr el)))) ;; No imm free vars, only late
            proc-vars))

    (let loop ((const-set const-init))
      ;; Compute new set, only keep binding if all of its late vars
      ;; are members of const-set
      (let ((new-const-set
              (keep (lambda (el)
                      (let ((late-ids (caddr (caddr el))))
                        (= (length late-ids)
                           (length (set-inter late-ids (map car const-set))))))
                    const-set)))
        ;; Fixed point condition, if there is a change, continue else stop and return
        (if (= (length const-set)
               (length new-const-set))
            (list (append new-const-set
                          const-proc-vars)
                  (set-sub proc-vars new-const-set '()))
            (loop new-const-set)))))

  ;; ---------------------------------------------------------------------------
  ;; LAZY CODE OBJECTS

  ;; Eval each !fun binding
  (define (get-lazy-eval other-vars succ)

    (define (bind-last id succ)
      (make-lazy-code
        #f
        (lambda (cgc ctx)
          (let ((ctx (ctx-id-add-idx ctx id 0)))
            (jump-to-version cgc succ ctx)))))

    (let loop ((bindings other-vars))
      (if (null? bindings)
          succ
          (let ((binding (car bindings))
                (next (loop (cdr bindings))))
            (gen-ast (cadr binding)
                     (bind-last (car binding) next))))))

  ;; Alloc space for all non const closures
  ;; Init all closures by writing headers, entry points, free & late variables
  (define (get-lazy-make-closures succ proc-vars other-vars const-proc-vars)

    ;; This function takes a ctx and add const proc vars to it
    (define (cst-binder ctx)
      (bind-const-proc-vars ctx const-proc-vars))

    ;; Alloc all closures
    (define (get-lazy-alloc succ)
      (make-lazy-code
        #f
        (lambda (cgc ctx)
          (gen-allocation-imm cgc STAG_PROCEDURE (* 8 (- closures-size 1)))
          (jump-to-version cgc (get-lazy-init-closures succ) ctx))))

    ;; Init all closures. Write headers, entry points, and free & late variables
    (define (get-lazy-init-closures succ)
      (let loop ((lst (reverse proc-vars)))
        (if (null? lst)
            ;; The last thing to do is to bind const proc vars to new ctx, then jump to succ
            (make-lazy-code
              #f
              (lambda (cgc ctx)
                (jump-to-version cgc succ (cst-binder ctx))))
            (let* ((l (car lst))
                   (id (car l))
                   (offset (cadr l))
                   (ast (caddr l))
                   (free-info (cadddr l))
                   (fvars-cst  (car free-info))
                   (fvars-imm  (cadr free-info))
                   (fvars-late (caddr free-info))
                   (next (loop (cdr lst))))

              (get-lazy-init-closure id ast next offset fvars-cst fvars-imm fvars-late)))))

    ;; Init a closure. Write header, entry point, and free & late variables
    (define (get-lazy-init-closure id ast succ clo-offset free-cst free-imm free-late)

      (make-lazy-code
        #f
        (lambda (cgc ctx)

          (define clo-reg alloc-ptr)
          (define clo-life LIFE_MOVE)

          (define offset-start (* -8 (- closures-size clo-offset)))
          (define offset-header offset-start)
          (define offset-entry  (+ offset-header 8))
          (define offset-free   (+ offset-entry 8))
          (define offset-free-h (* -1 (- closures-size clo-offset 2)))
          (define offset-late   (+ offset-free (* 8 (length free-imm))))

          ;; TODO: use a gen-closure function (merge with existing gen-closure function)

          ;; Write header
          (let ((clo-size (* 8 (+ (length (append free-imm free-late)) 1))))
            (x86-mov cgc (x86-rax) (x86-imm-int (mem-header clo-size STAG_PROCEDURE clo-life)))
            (x86-mov cgc (x86-mem offset-header clo-reg) (x86-rax)))

          ;; Write entry point
          ;; ctx is computed each time using cst-binder in a tmp ctx to ensure that cst are not dropped.
          ;; if max-versions is reached, cst are dropped and we lost cst information.
          ;; This context is used by init-entry and gen-free-vars because they need this information
          (mlet ((ctx (cst-binder ctx))
                 (fn-num/entry-obj (init-entry ast ctx (append free-cst free-imm) free-late id))
                 (entry-obj-loc (- (obj-encoding entry-obj) 1)))
            (if opt-entry-points
                (x86-mov cgc (x86-rax) (x86-imm-int entry-obj-loc))
                (x86-mov cgc (x86-rax) (x86-mem (+ 8 entry-obj-loc))))
            (x86-mov cgc (x86-mem offset-entry clo-reg) (x86-rax))

            ;; Write free vars
            (gen-free-vars cgc free-imm ctx offset-free-h clo-reg))

          ;; Write late vars
          ;; TODO: late !fun ?
          (let loop ((lates free-late)
                     (offset offset-late))
            (if (not (null? lates))
                (let* ((late-inf (assoc (car lates) proc-vars))
                       (late-off (and late-inf (+ (* -8 (- closures-size (cadr late-inf))) TAG_MEMOBJ)))
                       (opnd
                         (if late-off
                             (x86-mem late-off clo-reg)
                             (begin
                               (assert (assoc (car lates) other-vars) "Internal error")
                               (x86-imm-int (obj-encoding #f))))))
                  (if (x86-mem? opnd)
                      (x86-lea cgc (x86-rax) opnd)
                      (x86-mov cgc (x86-rax) opnd))
                  (x86-mov cgc (x86-mem offset clo-reg) (x86-rax))
                  (loop (cdr lates) (+ offset 8)))))

          ;; Update ctx and load closure
          (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
            (apply-moves cgc ctx moves)
            (let ((dest (codegen-reg-to-x86reg reg)))
              (x86-lea cgc dest (x86-mem (+ offset-start TAG_MEMOBJ) clo-reg)))
            (let* ((ctx (ctx-push ctx (make-ctx-tclo) reg))
                   (ctx (ctx-id-add-idx ctx id 0)))
              (jump-to-version cgc succ ctx))))))

    ;; Total alloc size
    (define closures-size (get-closures-size proc-vars))

    ;; TODO: check each procedure does not require still object
    (if (not (mem-can-alloc-group? (* 8 closures-size)))
        (error "Letrec: NYI, alloc group > MSECTION_FUDGE"))

    ;; Return first LCO: closures alloc
    (if (= closures-size 0)
        ;; No closure to create, only bind cst proc vars
        (make-lazy-code
          #f
          (lambda (cgc ctx)
            (jump-to-version cgc succ (cst-binder ctx))))
        (get-lazy-alloc succ)))

  ;; ---------------------------------------------------------------------------

  ;; Add constant procedure bindings information to context
  (define (bind-const-proc-vars ctx const-proc-vars)

    (let* (;; We first add identifier to ctx with a fake fn-num value
           (cst-set
             (map (lambda (c) (cons (car c) -1))
                  const-proc-vars))
           ;; Bind const ids using fake cst-set
           (ctx (ctx-bind-consts ctx cst-set)))

      ;; Then, we init all entries and write fn-num values
      (let loop ((l const-proc-vars)
                 (ctx ctx))
        (if (null? l)
            ctx
            (let* ((free-inf (caddr (car l)))
                   (free-late (caddr free-inf))
                   (free-cst (car free-inf))
                   (fn-num (init-entry-cst (cadr (car l)) (append free-cst free-late) ctx)))
              (loop (cdr l)
                    (ctx-cst-fnnum-set! ctx (caar l) fn-num)))))))

  ;; Compute current closures total size
  ;; If proc-vars set contains no function, size is 0
  ;; If proc-vars set contains at least one function,
  ;; size is:
  ;; position of previously computed proc + size of previously computed proc
  ;; with size of previous proc is:
  ;; 2 (header & entry) + number of non const free vars + number of late vars
  (define (get-closures-size proc-set)
    (if (null? proc-set)
        0
        (let ((prev-finfo (cadddr (car proc-set))))
          (+ (cadar proc-set)
             2
             (length (cadr prev-finfo))
             (length (caddr prev-finfo))))))

  ;; Main lco
  (make-lazy-code
    #f
    (lambda (cgc ctx)

      (let* (;; Compute binding groups
             (r (group-bindings ctx))
             (proc-vars (car r))
             (const-proc-vars (cadr r))
             (other-vars (caddr r))
             ;; Build LCO chain
             (lazy-let-out (get-lazy-lets-out ast ids 0 succ))
             (lazy-body    (gen-ast body lazy-let-out))
             (lazy-fun     (get-lazy-make-closures lazy-body proc-vars other-vars const-proc-vars))
             (lazy-eval    (get-lazy-eval other-vars lazy-fun)))

        (let* (;; Bind others
               (id-idx (map (lambda (el) (cons (car el) #f))
                            (append proc-vars other-vars)))
               (ctx (ctx-bind-locals ctx id-idx #t)))

          ;;
          (jump-to-version cgc lazy-eval ctx))))))

;; Create and return out lazy code object of let/letrec
;; Unbind locals, unbox result, and update ctx
(define (get-lazy-lets-out ast ids nb-cst succ)

  (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                     make-lazy-code-ret
                     make-lazy-code)))
    (make-lc
      #f
      (lambda (cgc ctx)
        (let* ((type (ctx-get-type ctx 0))
               (loc  (ctx-get-loc ctx 0))
               (ctx  (ctx-unbind-locals ctx ids))
               (ctx  (ctx-pop-n ctx (+ (- (length ids) nb-cst) 1)))
               (ctx  (ctx-push ctx type loc)))
          (jump-to-version cgc succ ctx))))))

;;-----------------------------------------------------------------------------
;; SPECIAL

;;-----------------------------------------------------------------------------
;;

(define (gambit-call? op)
  (and (atom-node? op)
       (symbol? (atom-node-val op))
       (let* ((sym (atom-node-val op))
              (lstsym (string->list (symbol->string sym)))
              (lstprefix (string->list "gambit$$")))
         (and (> (length lstsym) (length lstprefix))
              (equal? (list-head lstsym (length lstprefix))
                      lstprefix)))))

;; generated-arg? is #t if call take 1 arg and this arg already is generated
(define (mlc-gambit-call ast succ generated-arg?)

  ;; lco dropping all cst arguments
  (define (get-lazy-drop-csts succ)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let loop ((i (- (length (cdr ast)) 1))
                   (ctx ctx))
          (if (< i 0)
              (jump-to-version cgc succ ctx)
              (loop (- i 1)
                    (drop-cst-value cgc ctx i)))))))

  (define (get-gambit-sym sym)
    (let* ((lstprefix (string->list "gambit$$"))
           (lstsym (string->list (symbol->string sym)))
           (lstres (list-tail lstsym (length lstprefix))))
      (string->symbol (list->string lstres))))

  (define (get-locs ctx nlocs)
    (if (= nlocs 0)
        '()
        (cons (ctx-get-loc ctx (- nlocs 1))
              (get-locs ctx (- nlocs 1)))))

  (let* ((lazy-call
           (make-lazy-code
             ast
             (lambda (cgc ctx)
               (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ (length (cdr ast))))
                      (gsym (get-gambit-sym (atom-node-val (car ast))))
                      (nargs (length (cdr ast))))
                 (apply-moves cgc ctx moves)
                 (let ((locs (get-locs ctx nargs)))
                   (let loop ((clocs locs)
                              (fs (ctx-fs ctx)))
                     (if (not (null? clocs))
                         (begin (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd fs (car clocs)))
                                (x86-upush cgc (x86-rax))
                                (loop (cdr clocs) (+ fs 1))))))
                 (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding nargs)))
                 (x86-upush cgc (x86-rax))
                 (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding gsym)))
                 (x86-upush cgc (x86-rax))
                 (x86-pcall cgc label-gambit-call-handler)
                 (x86-upop cgc (codegen-reg-to-x86reg reg))
                 (x86-add cgc (x86-usp) (x86-imm-int (* 8 (+ nargs 1))))
                 (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx nargs) (make-ctx-tunk) reg)))))))
    (if generated-arg?
        lazy-call
        (gen-ast-l
          (cdr ast)
          (get-lazy-drop-csts lazy-call)))))

;;-----------------------------------------------------------------------------
;; PRIMITIVES

;;
;;
(define (mlc-primitive prim ast succ)
  (cond ((and (= (length ast) 2)
              (member prim '(##fx-? ##fl-)))
           (error "NYI atom mlc-primitive 1"))
        ((eq? prim 'exit)
           (get-lazy-error ""))
        ;; TODO: this can be removed when = is implemented as a primitive (in primitives set)
        ;; TODO: zero? will then be handled by prim-p-zero?
        ((eq? prim 'zero?)
           (gen-ast (list (atom-node-make '=)
                          (cadr ast)
                          (atom-node-make 0))
                    succ))
        ((and (eq? prim 'make-vector)
              (eq? (length ast) 2))
           (mlc-primitive-d prim (append ast (list (atom-node-make 0))) succ))
        ((and (eq? prim 'make-string)
              (eq? (length ast) 2))
           (mlc-primitive-d prim (append ast (list (atom-node-make (integer->char 0)))) succ))
        (else
           (mlc-primitive-d prim ast succ))))

;;
;; Gen code for simple primitives (primitive that only call codegen)
(define (gen-primitive cgc ctx succ reg op)

  (define inlined-cond? (member 'cond (lazy-code-flags succ)))

  (let* ((prim (primitive-get op))
         (gen  (primitive-codegen prim))
         (fs (ctx-fs ctx))
         (nargs (primitive-nbargs prim))
         (r  (build-list
                  nargs
                  (lambda (n)
                    (let ((type (ctx-get-type ctx n)))
                      (if (ctx-type-is-cst type)
                          (cons #t (ctx-type-cst type))
                          (cons #f (ctx-get-loc ctx n)))))))
         (cst? (map car r))
         (locs (map cdr r))
         (args (append (list cgc fs op reg inlined-cond?)
                       (reverse locs)
                       (reverse cst?)))
         (rtype (primitive-rettype prim)))

    (apply gen args)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx nargs) rtype reg))))

;;
;; Special primitive 'number?'
(define (lco-p-number? ast op succ)
  (define (get-lazy-res r)
    (make-lazy-code
      ast
      (lambda (cgc ctx)
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 1)))
          (apply-moves cgc ctx moves)
          (codegen-set-bool cgc r reg)
          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg))))))

  (let* ((lazy-flo (gen-dyn-type-test ATX_FLO 0 (get-lazy-res #t) (get-lazy-res #f) #f))
         (lazy-fix (gen-dyn-type-test ATX_INT 0 (get-lazy-res #t) lazy-flo #f)))
    lazy-fix))

;;
;; Special primitive 'current-x-port'
(define (lco-p-cur-x-port ast op succ)
  (define lazy-out
    (make-lazy-code
      ast
      (lambda (cgc ctx)
        (let* ((type (if (eq? op 'current-input-port) (make-ctx-tipo) (make-ctx-topo)))
               (ctx (ctx-set-type ctx 0 type #f)))
          (jump-to-version cgc succ ctx)))))
  (let* ((sym (string->symbol (string-append "gambit$$" (symbol->string op))))
         (node (atom-node-make sym)))
    (gen-ast (list node) lazy-out)))

;;
;; Special primitive 'zero?'
(define (lco-p-zero? ast op succ)
  (let ((zero-node (atom-node-make 0))
        (=-node    (atom-node-make '=)))
  (gen-ast (list =-node (cadr ast) zero-node)
           succ)))

;;
;; Special primitive 'char=?'
(define (lco-p-char=? ast op succ)
  (let ((node (atom-node-make 'eq?)))
    (gen-ast (cons node (cdr ast))
             succ)))

;;
;; Special primitives 'quotient', 'modulo', 'remainder'
(define (lco-p-binop ast op succ)
  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (mlet ((label-div0 (get-label-error ERR_DIVIDE_ZERO))
             (moves/reg/ctx (ctx-get-free-reg ctx succ 2))
             (tleft  (ctx-get-type ctx 1))
             (tright (ctx-get-type ctx 0))
             (lcst?  (ctx-type-is-cst tleft))
             (rcst?  (ctx-type-is-cst tright))
             (lleft  (if lcst? (ctx-type-cst tleft)  (ctx-get-loc ctx 1)))
             (lright (if rcst? (ctx-type-cst tright) (ctx-get-loc ctx 0))))
        (apply-moves cgc ctx moves)
        (if (and lcst? rcst?)
            (error "NN"))
        (codegen-p-binop cgc (ctx-fs ctx) op label-div0 reg lleft lright lcst? rcst?)
        (jump-to-version cgc
                         succ
                         (ctx-push (ctx-pop-n ctx 2) (make-ctx-tint) reg))))))

;;
;; Special primitive 'eq?'
(define (lco-p-eq? ast op succ)

  ;; Inlined if cond eq?
  (define (gen-eq?-if cgc ctx typel typer lcst? rcst?)
    (let ((ll (if lcst? (ctx-type-cst typel)
                        (ctx-get-loc  ctx 1)))
          (lr (if rcst? (ctx-type-cst typer)
                        (ctx-get-loc  ctx 0)))
          (nctx (ctx-pop-n ctx 2)))
      (if (and (not (ctx-tunk? typel))
               (not (ctx-tunk? typer))
               (not (ctx-type-teq? typel typer)))
          ;; Types are known and !=
          (jump-to-version cgc (lazy-code-lco-false succ) nctx)
          ;;
          (begin
            (codegen-p-eq? cgc (ctx-fs ctx) 'eq? #f #t ll lr lcst? rcst?)
            ((lazy-code-generator succ) cgc nctx x86-jne)))))

  (define (gen-eq? cgc ctx typel typer lcst? rcst?)
    (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 2))
           (ll (if lcst? (ctx-type-cst typel)
                               (ctx-get-loc  ctx 1)))
           (lr (if rcst? (ctx-type-cst typer)
                         (ctx-get-loc  ctx 0)))
           (nctx (ctx-push (ctx-pop-n ctx 2) (make-ctx-tboo) reg)))
      (apply-moves cgc ctx moves)
      (if (and (not (ctx-tunk? typel))
               (not (ctx-tunk? typer))
               (not (ctx-type-teq? typel typer)))
          ;; Types are known and !=
          (codegen-set-bool cgc #f reg)
          ;;
          (codegen-p-eq? cgc (ctx-fs ctx) 'eq? reg #f ll lr lcst? rcst?))
      (jump-to-version cgc succ nctx)))

  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (let* ((typel (ctx-get-type ctx 1))
             (typer (ctx-get-type ctx 0))
             (lcst? (ctx-type-is-cst typel))
             (rcst? (ctx-type-is-cst typer))
             (if-cond? (member 'cond (lazy-code-flags succ))))
        (cond ((and lcst? rcst? if-cond?)
                 (error "NYI"))
              ((and lcst? rcst?)
                 (if (or (ctx-tclo? typel)
                         (ctx-tclo? typer))
                     (error "NYI, can't use literal->ctx-type with fn-num"))
                 (let ((ctx (ctx-pop-n ctx 2))
                       (r (eq? (ctx-type-cst typel) (ctx-type-cst typer))))
                   (jump-to-version cgc succ (ctx-push ctx (literal->ctx-type r) #f))))
              (if-cond?
                 (gen-eq?-if cgc ctx typel typer lcst? rcst?))
              (else
                 (gen-eq? cgc ctx typel typer lcst? rcst?)))))))

;;
;; Special primitive 'vector'
(define (lco-p-vector ast op succ)

  (define nbargs (length (cdr ast)))

  (let* ((lazy-set
           (make-lazy-code
             ast
             (lambda (cgc ctx)
               (let* ((vec-loc  (ctx-get-loc ctx 0))
                      (vec-opnd (codegen-loc-to-x86opnd (ctx-fs ctx) vec-loc)))

                 (let loop ((idx nbargs))
                   (if (= idx 0)
                       (let* ((ctx (ctx-pop-n ctx (+ nbargs 1)))
                              (ctx (ctx-push ctx (make-ctx-tvec) vec-loc)))
                         (jump-to-version cgc succ ctx))
                       (let* ((val-loc  (ctx-get-loc ctx idx))
                              (type (ctx-get-type ctx idx))
                              (cst? (ctx-type-is-cst type))
                              (val-opnd (if cst?
                                            (x86-imm-int (obj-encoding (ctx-type-cst type)))
                                            (codegen-loc-to-x86opnd (ctx-fs ctx) val-loc)))
                              (offset (- (* 8 (+ (- nbargs idx) 1)) TAG_MEMOBJ)))
                         (if (or (x86-imm? val-opnd)
                                 (x86-mem? val-opnd))
                             (begin (x86-mov cgc (x86-rax) val-opnd)
                                    (set! val-opnd (x86-rax))))
                         (x86-mov cgc (x86-mem offset vec-opnd) val-opnd)
                         (loop (- idx 1)))))))))

         (lazy-vector
           (gen-ast (list (atom-node-make 'make-vector)
                          (atom-node-make nbargs))
                    lazy-set)))

    lazy-vector))


;;
;; Special primitive 'list'
(define (lco-p-list ast op succ)

  (define len (length (cdr ast)))

  (define (build-chain n)
    (if (= n 0)
        (make-lazy-code
          #f
          (lambda (cgc ctx)
            (let* ((type (ctx-get-type ctx n))
                   (cst? (ctx-type-is-cst type))
                   (loc (if cst?
                            (ctx-type-cst type)
                            (ctx-get-loc ctx n))))
              (let ((pair-offset (* -3 8 n)))
                ;; Write header
                (x86-mov cgc (x86-mem (- pair-offset 24) alloc-ptr) (x86-imm-int (mem-header 16 STAG_PAIR)) 64)
                ;; Write null in cdr
                (x86-mov cgc (x86-mem (- pair-offset 16) alloc-ptr) (x86-imm-int (obj-encoding '())) 64)
                ;; Write value in car
                (let ((opnd
                        (if cst?
                            (x86-imm-int (obj-encoding loc))
                            (codegen-loc-to-x86opnd (ctx-fs ctx) loc))))
                  (if (or (x86-mem? opnd)
                          (x86-imm? opnd))
                      (begin
                        (x86-mov cgc (x86-rax) opnd)
                        (set! opnd (x86-rax))))
                  (x86-mov cgc (x86-mem (- pair-offset  8) alloc-ptr) opnd))
                (mlet ((moves/reg/ctx (ctx-get-free-reg (ctx-pop-n ctx len) succ 0)))
                   (apply-moves cgc ctx moves)
                   ;; Load first pair in dest register
                   (let ((dest (codegen-reg-to-x86reg reg)))
                     (let ((offset (+ (* len 3 -8) TAG_PAIR)))
                       (x86-lea cgc dest (x86-mem offset alloc-ptr))
                       (jump-to-version cgc succ (ctx-push ctx (make-ctx-tpai) reg)))))))))
        (make-lazy-code
          #f
          (lambda (cgc ctx)
            (let* ((type (ctx-get-type ctx n))
                   (cst? (ctx-type-is-cst type))
                   (loc (if cst?
                            (ctx-type-cst type)
                            (ctx-get-loc ctx n))))
              (let ((pair-offset (* -3 8 n)))
                ;; Write header
                (x86-mov cgc (x86-mem (- pair-offset 24) alloc-ptr) (x86-imm-int (mem-header 16 STAG_PAIR)) 64)
                ;; Write encoded next pair in cdr
                (x86-lea cgc (x86-rax) (x86-mem (+ pair-offset TAG_PAIR) alloc-ptr))
                (x86-mov cgc (x86-mem (- pair-offset 16) alloc-ptr) (x86-rax))
                ;; Write value in car
                (let ((opnd
                        (if cst?
                            (x86-imm-int (obj-encoding loc))
                            (codegen-loc-to-x86opnd (ctx-fs ctx) loc))))
                  (if (or (x86-imm? opnd)
                          (x86-mem? opnd))
                      (begin (x86-mov cgc (x86-rax) opnd)
                             (set! opnd (x86-rax))))
                  (x86-mov cgc (x86-mem (- pair-offset  8) alloc-ptr) opnd))
                ;; Continue
                (jump-to-version cgc (build-chain (- n 1)) ctx)))))))

  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (let ((size (- (* len 3 8) 8)))
        ;; One alloc for all pairs
        (gen-allocation-imm cgc STAG_PAIR size)
        (jump-to-version cgc (build-chain (- len 1)) ctx)))))

;;
;; Special primitive 'apply'
(define (lco-p-apply ast op succ)

  (make-lazy-code
    ast
    (lambda (cgc ctx)
      ;; Save used registers, generate and push continuation stub
      (set! ctx (call-save/cont cgc ctx ast succ #f 2 #t))
      ;; Push closure
      (call-get-closure cgc ctx 1)

      (let* ((label-end (asm-make-label #f (new-sym 'apply-end-args)))
             (llst (ctx-get-loc ctx 0))
             (oplst (codegen-loc-to-x86opnd (ctx-fs ctx) llst)))
        ;; r11, selector & r15 are used as tmp registers
        ;; It is safe because they are not used for parameters.
        ;; And if they are used after, they already are saved on the stack
        (x86-mov cgc (x86-rdx) oplst)
        (x86-mov cgc (x86-r11) (x86-imm-int 0))
        (let loop ((args-regs args-regs))
          (if (null? args-regs)
              (let ((label-loop (asm-make-label #f (new-sym 'apply-loop-args))))
                (x86-label cgc label-loop)
                (x86-cmp cgc (x86-rdx) (x86-imm-int (obj-encoding '())))
                (x86-je cgc label-end)
                  (x86-add cgc (x86-r11) (x86-imm-int 4))
                  (x86-mov cgc selector-reg (x86-mem (- OFFSET_PAIR_CAR TAG_PAIR) (x86-rdx)))
                  (x86-upush cgc selector-reg)
                  (x86-mov cgc (x86-rdx) (x86-mem (- OFFSET_PAIR_CDR TAG_PAIR) (x86-rdx)))
                  (x86-jmp cgc label-loop))
              (begin
                (x86-cmp cgc (x86-rdx) (x86-imm-int (obj-encoding '())))
                (x86-je cgc label-end)
                  (x86-add cgc (x86-r11) (x86-imm-int 4))
                  (x86-mov cgc (codegen-loc-to-x86opnd (ctx-fs ctx) (car args-regs)) (x86-mem (- OFFSET_PAIR_CAR TAG_PAIR) (x86-rdx)))
                  (x86-mov cgc (x86-rdx) (x86-mem (- OFFSET_PAIR_CDR TAG_PAIR) (x86-rdx)))
                (loop (cdr args-regs)))))
        (x86-label cgc label-end)
        ;; Reset selector used as tmp reg
        (x86-mov cgc selector-reg (x86-imm-int 0))

        (let ((fn-id-inf (call-get-eploc ctx (cadr ast))))
          (x86-mov cgc (x86-rdi) (x86-r11)) ;; Copy nb args in rdi
          (if (and (not opt-entry-points) fn-id-inf (car fn-id-inf))
              (x86-mov cgc (x86-rsi) (x86-imm-int (obj-encoding #f)))
              (x86-mov cgc (x86-rsi) (x86-rax))) ;; Move closure in closure reg
          (gen-call-sequence ast cgc #f #f (and fn-id-inf (cdr fn-id-inf))))))))

(define (mlc-primitive-d prim ast succ)

  ;; If all operands are cst, return list of ctx types
  ;; If one or more operands are not cst, return #f
  (define (get-all-cst-opnds ctx nopnds)
    (let loop ((idx (- nopnds 1))
               (r '()))
      (if (< idx 0)
          (reverse r)
          (let ((type (ctx-get-type ctx idx)))
            (if (ctx-type-is-cst type)
                (let ((cst (ctx-type-cst type)))
                  (assert (or (not (ctx-tclo? type))
                              (not (##mem-allocated? cst))
                              (permanent-object? cst))
                          "Internal error")
                  (loop (- idx 1) (cons type r)))
                #f)))))

  ;; This lco detect if all operands of the primitives are cst operands
  ;; If so, use cst primitive function associated to current primitive
  ;; else, jump to next
  (define (get-lazy-cst-check primitive lco-prim)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let* ((lco-alloc-cstfn (get-lazy-drop-cstfn lco-prim (length (cdr ast))))
               (lco-cst (primitive-lco-cst primitive))
               (opnds (and lco-cst (get-all-cst-opnds ctx (length (cdr ast))))))
          (if opnds
              (lco-cst cgc succ prim ctx opnds)
              (jump-to-version cgc lco-alloc-cstfn ctx))))))

  ;; Drop all const functions of primitive args
  (define (get-lazy-drop-cstfn succ nargs)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let loop ((i (- nargs 1))
                   (ctx ctx))
          (if (< i 0)
              (jump-to-version cgc succ ctx)
              (let ((type (ctx-get-type ctx i)))
                (if (and (ctx-type-is-cst type)
                         (ctx-tclo? type))
                    (loop (- i 1) (drop-cst-value cgc ctx i))
                    (loop (- i 1) ctx))))))))

  ;;
  (define (get-prim-lco primitive)
    (let ((lco-getter (primitive-lcofun primitive)))
      (if lco-getter
          (lco-getter ast prim succ)
          (make-lazy-code
            ast
            (lambda (cgc ctx)
              (define nb-opnds (length (cdr ast)))
              (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ nb-opnds)))
                (apply-moves cgc ctx moves)
                (gen-primitive cgc ctx succ reg prim)))))))

  ;; Assert primitive nb args
  (assert-p-nbargs prim ast)

  ;;
  (let* ((primitive (primitive-get prim))
         (lazy-primitive (get-prim-lco primitive))
         (lazy-cst-check (get-lazy-cst-check primitive lazy-primitive)))

    (if (primitive-nbargs primitive)
        ;; Build args lco chain with type checks
        (let ((types (primitive-argtypes primitive)))
          (assert (= (length types)
                     (length (cdr ast)))
                  "Primitive error")
          (check-types types (cdr ast) lazy-cst-check ast))
        ;; No need to check types
        (gen-ast-l (cdr ast) lazy-cst-check))))

;; Build lazy objects chain of 'args' list
;; and insert type check for corresponding 'types'
;; TODO: rename types -> type-tests-fn
(define (check-types types args succ ast)

  (define (check-cst-type type cst)
    (cond
      ((eq? type ATX_INT) (integer? cst))
      ((eq? type ATX_FLO) (flonum? cst))
      ((eq? type ATX_NUL) (null? cst))
      ((eq? type ATX_BOO) (boolean? cst))
      ((eq? type ATX_CHA) (char? cst))
      (else #f)))

  (define (check-types-h types args curr-pos)

    (if (null? types)
        succ
        (let* ((lazy-next (check-types-h (cdr types) (cdr args) (+ curr-pos 1))))
          (cond ;; special type all
                ((eq? (car types) ATX_ALL)
                   (gen-ast (car args) lazy-next))
                ;; special type number
                ((eq? (car types) ATX_NUM)
                   (let* ((lazy-flo (gen-fatal-type-test ATX_FLO 0 lazy-next ast))
                          (lazy-fix (gen-dyn-type-test   ATX_INT 0 lazy-next lazy-flo ast)))
                     (gen-ast (car args)
                              lazy-fix)))
                ;;
                (else
                   (gen-ast (car args)
                            (gen-fatal-type-test (car types) 0 lazy-next ast)))))))

  (check-types-h types args 0))

;;
;; Cst primitives functions

;; These functions are called when all arguments of a primitive are cst values
;; -> compute new cst value, update ctx, and jump to successor lco

;; NYI cst function
(define (dummy-cst-all cgc succ op ctx csts)
  (error "NYI"))

(define-macro (cst-prim-n pop-n compute-cst)
  `(lambda (cgc succ op ctx csts)
     (let* ((cst ,compute-cst)
            (type (literal->ctx-type cst))
            (ctx (ctx-push (ctx-pop-n ctx ,pop-n) type #f)))
       (jump-to-version cgc succ ctx))))

(define-macro (cst-prim-1 fn) `(cst-prim-n 1 (,fn (car csts))))
(define-macro (cst-prim-2 fn) `(cst-prim-n 2 (,fn (car csts) (cadr csts))))

;; 1 arg cst primitives
(define cst-car       (cst-prim-1 (lambda (cst) (car (ctx-type-cst cst)))))
(define cst-cdr       (cst-prim-1 (lambda (cst) (cdr (ctx-type-cst cst)))))
(define cst-not       (cst-prim-1 (lambda (cst) (not (ctx-type-cst cst)))))
(define cst-int->char (cst-prim-1 (lambda (cst) (integer->char (ctx-type-cst cst)))))
(define cst-number?   (cst-prim-1 (lambda (cst)
                                    (and (not     (ctx-tclo?    cst))
                                         (number? (ctx-type-cst cst))))))

;; 2 args cst primitives
(define cst-eq?
  (cst-prim-2 (lambda (cst1 cst2)
                (and (not (ctx-tclo? cst1))
                     (not (ctx-tclo? cst2))
                     (eq? (ctx-type-cst cst1)
                          (ctx-type-cst cst2))))))
(define cst-binop
  (cst-prim-2 (lambda (cst1 cst2)
                (eval (list op
                            (ctx-type-cst cst1)
                            (ctx-type-cst cst2))))))

;;-----------------------------------------------------------------------------
;; Branches

;;
;; Make lazy code from IF
;;
(define (mlc-if ast succ)

  (letrec ((condition (cadr ast))
           (lazy-code0
             (gen-ast (cadddr ast) succ))
           (lazy-code1
             (gen-ast (caddr ast) succ))
           (lazy-code-test
             (make-lazy-code-cond
               #f
               lazy-code1
               lazy-code0
               (lambda (cgc ctx #!optional x86-op)

                 (let* ((ctx0 (if x86-op ctx (ctx-pop ctx)))   ;; Pop condition result

                        (ctx1
                          ctx0)

                        (label-jump
                          (asm-make-label
                            cgc
                            (new-sym 'patchable_jump)))

                        (stub-first-label-addr #f)

                        (stub-labels
                          (add-callback
                            cgc
                            1
                            (let ((prev-action #f))
                              (lambda (ret-addr selector)
                                (let ((stub-addr
                                        stub-first-label-addr)
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

                   (let ((label-false (list-ref stub-labels 0))
                         (label-true  (list-ref stub-labels 1)))

                     (set! stub-first-label-addr
                           (min (asm-label-pos label-false)
                                (asm-label-pos label-true)))

                     (if x86-op
                         (codegen-inlined-if cgc label-jump label-false label-true x86-op)
                         (let* ((type (ctx-get-type ctx 0))
                                (cst? (ctx-type-is-cst type)))
                           ;; TODO WIP: do *not* create stub in the first two cases
                           (cond ((and cst? (ctx-type-cst type))
                                    (jump-to-version cgc lazy-code1 (ctx-pop ctx)))
                                 (cst?
                                    (jump-to-version cgc lazy-code0 (ctx-pop ctx)))
                                 (else
                                    (let ((lcond (ctx-get-loc ctx 0)))
                                      (codegen-if cgc (ctx-fs ctx) label-jump label-false label-true lcond))))))))))))

    (gen-ast
      (cadr ast)
      lazy-code-test)))

;;-----------------------------------------------------------------------------
;; APPLY & CALL

;; Return air (const? . fn-num)
;; const? is #t if fn-num is an fn-num of a const function
;; const? is #f if fn-num is an fn-num of a non const function
(define (call-get-eploc ctx op)

  (if (and (atom-node? op)
           (symbol? (atom-node-val op)))
      ;; Op is an atom node with a symbol
      (let ((sym (atom-node-val op)))

        (define global-opt
          (let ((global (asc-globals-get sym)))
            (if (and global
                     (not (assoc sym (ctx-env ctx)))
                     (ctx-tclo? (global-stype global))
                     (ctx-type-is-cst (global-stype global)))
                (cons #t (ctx-type-cst (global-stype global)))
                #f)))

        (or (ctx-get-eploc ctx sym)
            global-opt))
      #f))

;;
;; Call steps
;;

;; Save used registers and return updated ctx
(define (call-save/cont cgc ctx ast succ tail? idx-offset apply?)
  (if tail?
      ;; Tail call, no register to save and no continuation to generate
      ctx
      (mlet ((moves/nctx (ctx-save-call ctx idx-offset)))
        (define fctx (ctx-fs-inc nctx))
        ;; Save registers
        (set! moves (cons (cons 'fs 1) moves))
        (apply-moves cgc fctx moves)
        ;; Generate & push continuation
        ;; gen-continuation-* needs ctx without return address slot
        (if opt-return-points
            (gen-continuation-cr cgc ast succ nctx apply?)
            (gen-continuation-rp cgc ast succ nctx apply?))

        fctx)))

;; Push closure, put it in rax, and return updated ctx
(define (call-get-closure cgc ctx closure-idx)
  (let* ((fs (ctx-fs ctx))
         (loc  (ctx-get-loc     ctx closure-idx)))
    (codegen-load-closure cgc fs loc)))

;; Move args in regs or mem following calling convention
(define (call-prep-args cgc ctx ast nbargs const-fn)

  (let* ((cloloc (if const-fn #f (ctx-get-loc ctx nbargs)))
         (stackp/moves (ctx-get-call-args-moves ctx nbargs cloloc))
         (stackp (car stackp/moves))
         (moves (cdr stackp/moves)))

    (let loop ((fs (ctx-fs ctx))
               (locs stackp))
      (if (null? locs)
          (set! ctx (ctx-fs-update ctx fs))
          (begin
            (cond ((eq? (caar locs) 'constfn)
                     (let ((entry-obj (asc-globalfn-entry-get (cdar locs))))
                       (gen-closure cgc 'tmp #f entry-obj '())
                       (x86-upush cgc (codegen-reg-to-x86reg 'tmp))))
                  ((eq? (caar locs) 'const)
                     (x86-upush cgc (x86-imm-int (obj-encoding (cdar locs)))))
                  (else
                     (x86-upush cgc (codegen-loc-to-x86opnd fs (car locs)))))
            (loop (+ fs 1) (cdr locs)))))

    (let* ((used-regs
             (foldr (lambda (el r)
                      (define regs '())
                      (if (ctx-loc-is-register? (car el)) (set! regs (cons (car el) regs)))
                      (if (ctx-loc-is-register? (cdr el)) (set! regs (cons (cdr el) regs)))
                      (append regs r))
                    '()
                    moves))
             (unused-regs (set-sub (ctx-init-free-regs) used-regs '())))

      (if (null? unused-regs)
          (begin (apply-moves cgc ctx moves 'selector)
                 (x86-mov cgc selector-reg (x86-imm-int 0)))
          (apply-moves cgc ctx moves (car unused-regs)))

      ;; Force closure reg to contain #f for do_callback_fn if we call a const closure
      (if (and (not opt-entry-points)
               const-fn)
          (x86-mov cgc (x86-rsi) (x86-imm-int (obj-encoding #f))))

      ctx)))

;; Shift args and closure for tail call
(define (call-tail-shift cgc ctx ast tail? nbargs)

  ;; r11 is available because it's the ctx register
  (if tail?
      (let ((fs (ctx-fs ctx))
            (nshift
              (if (> (- nbargs (length args-regs)) 0)
                  (- nbargs (length args-regs))
                  0)))
        (let loop ((curr (- nshift 1)))
          (if (>= curr 0)
              (begin
                (x86-mov cgc (x86-r11) (x86-mem (* 8 curr) (x86-usp)))
                (x86-mov cgc (x86-mem (* 8 (+ (- fs nshift 1) curr)) (x86-usp)) (x86-r11))
                (loop (- curr 1)))))

        (if (not (= (- fs nshift 1) 0))
            (x86-add cgc (x86-usp) (x86-imm-int (* 8 (- fs nshift 1))))))))

;;
;; Make lazy code from CALL EXPR
;;
(define (mlc-call ast succ)

  (let* (;; fn-num. Computed when ctx is available
         ;; #f is called function is unknown at compile time
         ;; contains fn-num if called function is known at compile time
         (fn-id-inf #f) ;; (cst? . fn-num) or #f
         ;; Tail call if successor's flags set contains 'ret flag
         (tail? (member 'ret (lazy-code-flags succ)))
         ;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (get-lazy-error (ERR_TYPE_EXPECTED ATX_CLO)))
         ;; Lazy call
         (lazy-call
           (make-lazy-code
             #f
             (lambda (cgc ctx)




               ;; Handle const fn
               (let ((type (ctx-get-type ctx (length args))))
                 (if (ctx-type-is-cst type)
                     (set! fn-id-inf (cons #t (ctx-type-cst type)))))

               ;; Save used registers, generate and push continuation stub
               (set! ctx (call-save/cont cgc ctx ast succ tail? (+ (length args) 1) #f))

               ;; Move args to regs or stack following calling convention
               (set! ctx (call-prep-args cgc ctx ast (length args) (and fn-id-inf (car fn-id-inf))))

               ;; Shift args and closure for tail call
               (call-tail-shift cgc ctx ast tail? (length args))

               ;; Generate call sequence
               ;; Build call ctx
               (let ((call-ctx (ctx-init-call ctx (length (cdr ast)))))
                 (gen-call-sequence ast cgc call-ctx (length args) (and fn-id-inf (cdr fn-id-inf)))))))
         ;; Lazy code object to build the continuation
         (lazy-tail-operator (check-types (list ATX_CLO) (list (car ast)) lazy-call ast)))

    ;; Gen and check types of args
    (make-lazy-code
      ast
      (lambda (cgc ctx)

        ;; Check if the identity of called function is available
        (set! fn-id-inf (call-get-eploc ctx (car ast)))

        (if (and fn-id-inf (car fn-id-inf))
            (jump-to-version
              cgc
              (gen-ast-l (cdr ast) lazy-call)
              (ctx-push ctx (make-ctx-tclo #t (cdr fn-id-inf)) #f))
            (jump-to-version
              cgc
              (check-types (list ATX_CLO) (list (car ast)) (gen-ast-l (cdr ast) lazy-call) ast)
              ctx))))))

(define (gen-continuation-rp cgc ast succ ctx apply?)

  (let* ((lazy-continuation
           (make-lazy-code-cont
             #f
             (lambda (cgc ctx)
               (jump-to-version cgc succ ctx))))
         ;; Label for return address loading
         (load-ret-label (asm-make-label #f (new-sym 'load-ret-addr)))
         ;; Flag in stub : is the continuation already generated ?
         (gen-flag #f)
         ;; Continuation stub
         (stub-labels
           (add-callback cgc
                              0
                              (lambda (ret-addr selector)
                                (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag to continuation addr
                                    (mlet ((args (cdr ast))
                                           (ctx
                                             (if apply?
                                                 (ctx-pop-n ctx 2) ;; Pop operator and args
                                                 (ctx-pop-n ctx (+ (length args) 1)))))

                                      (set! gen-flag
                                            (gen-version-continuation
                                              load-ret-label
                                              lazy-continuation
                                              (ctx-push ctx (make-ctx-tunk) return-reg)))))
                                gen-flag))))
   ;; Generate code
   (codegen-load-cont-rp cgc load-ret-label (list-ref stub-labels 0))))

(define (gen-continuation-cr cgc ast succ ctx apply?)

  (let* ((lazy-continuation
           (make-lazy-code-cont
             #f
             (lambda (cgc ctx)
               (jump-to-version cgc succ ctx))))
         (stub-labels
           (add-cont-callback
             cgc
             0
             (lambda (ret-addr selector type table)

                   ;;
                   (let* ((args (cdr ast))
                          (ctx
                            (if apply?
                                (ctx-pop-n ctx 2) ;; Pop operator and args
                                (ctx-pop-n ctx (+ (length args) 1)))) ;; Remove closure and args from virtual stack
                          (generic?
                            (and opt-max-versions
                                 (>= (lazy-code-nb-versions lazy-continuation) opt-max-versions))))

                     (gen-version-continuation-cr
                       lazy-continuation
                       (ctx-push ctx (if generic? (make-ctx-tunk) type) return-reg)
                       type
                       generic?
                       table)))))
         ;; CRtable
         (crtable-key (get-crtable-key ast ctx))
         (stub-addr (vector-ref (list-ref stub-labels 0) 1))
         (crtable (get-crtable ast crtable-key stub-addr))
         (crtable-loc (- (obj-encoding crtable) 1)))

    ;; Generate code
    (codegen-load-cont-cr cgc crtable-loc)))

;; Gen call sequence (call instructions)
;; fn-num is fn identifier or #f
(define (gen-call-sequence ast cgc call-ctx nb-args fn-num)

  (define entry-obj (and fn-num (asc-globalfn-entry-get fn-num)))
  ;; TODO: eploc -> entry-obj-loc
  (define eploc (and entry-obj (- (obj-encoding entry-obj) 1)))

  (define (get-cc-direct cc-idx)
    (if (and cc-idx entry-obj)
        (get-xx-direct cc-idx)
        #f))

  (define (get-ep-direct)
    (if entry-obj
        (get-xx-direct 0)
        #f))

  (define (get-xx-direct idx)
    (let ((r (asc-entry-stub-get entry-obj))
          (ep (if opt-entry-points
                  (s64vector-ref entry-obj (+ idx 1))
                  (* 4 (vector-ref entry-obj 0)))))

      (cond ;; It's a call to an already generated entry point
            ((and (not (= ep (car r)))
                  (not (= ep (cdr r))))
               (list 'ep ep))
            ;; It's a call to a known stub
            ((or (= ep (car r))
                 (= ep (cdr r)))
               (let ((label (asm-make-label #f (new-sym 'stub_load_))))
                 (asc-entry-load-add entry-obj idx label)
                 (list 'stub ep label)))
            ;;
            (else
               #f))))


  (cond ((not opt-entry-points)
           (let ((direct (get-ep-direct)))
             (codegen-call-ep cgc nb-args eploc direct)))
        ((not nb-args) ;; apply
           (codegen-call-cc-gen cgc #f eploc))
        (else
           (let* ((idx (get-closure-index (list-head (ctx-stack call-ctx) nb-args)))
                  (direct (get-cc-direct idx)))
             (if idx
                 (codegen-call-cc-spe cgc idx nb-args eploc direct)
                 (codegen-call-cc-gen cgc nb-args eploc))))))

;;-----------------------------------------------------------------------------
;; Operators

;;
;; Make lazy code from N-ARY OPERATOR
;;
(define (mlc-op-n ast succ op) ;; '(+ - * < > <= >= = /)
  (assert (= (length ast) 3) "Internal error")
  (gen-ast-l (cdr ast)
             (get-lazy-n-binop ast op #f #f succ)))

(define (get-lazy-n-binop ast op lcst rcst succ)

  (define inlined-if-cond? (member 'cond (lazy-code-flags succ)))

  (define num-op? (member op '(+ - * /)))

  ;; TODO wip
  (define (get-stub-overflow-label cgc ctx reg lleft lcst? lright rcst?)
    (let ((labels
            (add-callback #f 0 (lambda (ret-addr selector)
                                 (let ((lco
                                        (make-lazy-code
                                          #f
                                          (lambda (cgc ctx)
                                            (let ((type (make-ctx-tflo)))
                                              (codegen-num-ff cgc (ctx-fs ctx) op reg lleft #t lright #t lcst? rcst? #t)
                                              (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))))))
                                   (gen-version-first lco ctx))))))
      (list-ref labels 0)))

  ;; Build chain to check type of two values (no cst)
  (define (type-check-two)
    (let* (;; Operations lco
           (lazy-op-ii
             (if (eq? op '/)
                 (get-op-ff #t #t)
                 (get-op-ii)))
           (lazy-op-if (get-op-ff #t #f))
           (lazy-op-fi (get-op-ff #f #t))
           (lazy-op-ff (get-op-ff #f #f))
           ;; Right branch
           (lazy-yfloat2 (gen-fatal-type-test ATX_FLO 0 lazy-op-ff ast))
           (lazy-yint2   (gen-dyn-type-test ATX_INT 0 lazy-op-fi lazy-yfloat2 ast))
           (lazy-xfloat  (gen-fatal-type-test ATX_FLO 1 lazy-yint2 ast))
           ;; Left branch
           (lazy-yfloat  (gen-fatal-type-test ATX_FLO 0 lazy-op-if ast))
           (lazy-yint    (gen-dyn-type-test ATX_INT 0 lazy-op-ii lazy-yfloat ast))
           ;; Root node
           (lazy-xint    (gen-dyn-type-test ATX_INT 1 lazy-yint lazy-xfloat ast)))
    lazy-xint))

  ;; TODO: Merge with get-op-ff
  (define (get-op-ii)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let* ((type  (if num-op? (make-ctx-tint) (make-ctx-tboo)))
               ;; If op is a num-op, we can't use an opnd register as dest in case of overflow
               (nopnd-regalloc (if num-op? 0 2))
               ;; right info
               (rtype (ctx-get-type ctx 0))
               (rcst? (ctx-type-is-cst rtype))
               (rloc  (if rcst? (ctx-type-cst rtype) (ctx-get-loc ctx 0)))
               ;; left info
               (ltype (ctx-get-type ctx 1))
               (lcst? (ctx-type-is-cst ltype))
               (lloc  (if lcst? (ctx-type-cst ltype) (ctx-get-loc ctx 1))))

          (cond ((and (not inlined-if-cond?) lcst? rcst?)
                  (let ((ctx (ctx-push (ctx-pop-n ctx 2)
                                       (literal->ctx-type (eval (list op lloc rloc)))
                                       #f)))
                    (jump-to-version cgc succ ctx)))
                ;; Inlined if condition with both cst
                ((and lcst? rcst?)
                  (let* ((r (eval (list op lloc rloc)))
                         (lco (if r (lazy-code-lco-true succ)
                                    (lazy-code-lco-false succ))))
                    (jump-to-version cgc lco (ctx-pop-n ctx 2))))
                ;; Inlined if condition
                (inlined-if-cond?
                  (let ((x86-op (codegen-cmp-ii cgc (ctx-fs ctx) op #f lloc rloc lcst? rcst? #t)))
                    ((lazy-code-generator succ) cgc (ctx-pop-n ctx 2) x86-op)))
                ;; In these cases, we need a free register
                ;; Then, get a free register, apply moves, and use it.
                (else
                  (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ nopnd-regalloc)))
                    (apply-moves cgc ctx moves)
                    (cond
                      (num-op?
                        (let ((overflow-label (get-stub-overflow-label cgc ctx reg lloc lcst? rloc rcst?)))
                          (codegen-num-ii cgc (ctx-fs ctx) op reg lloc rloc lcst? rcst? #t overflow-label))
                        (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))
                      (else
                          (codegen-cmp-ii cgc (ctx-fs ctx) op reg lloc rloc lcst? rcst? #f)
                          (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))))))))))

  ;;
  (define (get-op-ff leftint? rightint?)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let* ((type  (if num-op? (make-ctx-tflo) (make-ctx-tboo)))
               ;; right info
               (rtype (ctx-get-type ctx 0))
               (rcst? (ctx-type-is-cst rtype))
               (rloc  (if rcst? (ctx-type-cst rtype) (ctx-get-loc ctx 0)))
               ;; left info
               (ltype (ctx-get-type ctx 1))
               (lcst? (ctx-type-is-cst ltype))
               (lloc  (if lcst? (ctx-type-cst ltype) (ctx-get-loc ctx 1))))

          (cond ((and (not inlined-if-cond?) lcst? rcst?)
                  (error "NYI1"))
                ((and lcst? rcst?)
                  (error "NYI2"))
                (inlined-if-cond?
                  (let ((x86-op (codegen-cmp-ff cgc (ctx-fs ctx) op #f lloc leftint? rloc rightint? lcst? rcst? #t)))
                    ((lazy-code-generator succ) cgc (ctx-pop-n ctx 2) x86-op)))
                ;; In these cases, we need a free register
                ;; Then, get a free register, apply moves, and use it.
                (else
                  (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 2)))
                    (apply-moves cgc ctx moves)
                    (cond
                      (num-op?
                        (codegen-num-ff cgc (ctx-fs ctx) op reg lloc leftint? rloc rightint? lcst? rcst? #t)
                        (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))

                      (else
                        (codegen-cmp-ff cgc (ctx-fs ctx) op reg lloc leftint? rloc rightint? lcst? rcst? #f)
                        (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))))))))))

  (assert (not (and inlined-if-cond? (member op '(+ - * /))))
          "Internal compiler error")

  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (jump-to-version cgc (type-check-two) ctx))))

;;
;; Make lazy code from TYPE TEST
;;
(define (mlc-test sym ast succ)

  (define next-is-cond (member 'cond (lazy-code-flags succ)))

  (define (get-lazy-res bool)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 1)))
          (apply-moves cgc ctx moves)
          (codegen-set-bool cgc bool reg)
          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg))))))

  (define (get-lazy-inline bool)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let ((next (if bool (lazy-code-lco-true succ)
                             (lazy-code-lco-false succ))))
          (jump-to-version cgc next (ctx-pop ctx))))))

  (let ((type (predicate->ctx-type sym))
        (stack-idx 0)
        (lazy-fail
          (if next-is-cond
              (get-lazy-inline #f)
              (get-lazy-res #f)))
        (lazy-success
          (if next-is-cond
              (get-lazy-inline #t)
              (get-lazy-res #t))))

    (let ((check (gen-dyn-type-test type stack-idx lazy-success lazy-fail ast)))

      (if (and next-is-cond
               (atom-node? (cadr ast))
               (symbol? (atom-node-val (cadr ast))))
          (make-lazy-code
            #f
            (lambda (cgc ctx)
              (define sym (atom-node-val (cadr ast)))
              (define vartype (ctx-id-type ctx sym))
              (cond ((or (not vartype) (ctx-tunk? vartype))
                       (jump-to-version cgc (gen-ast (cadr ast) check) ctx))
                    ((ctx-type-teq? type vartype)
                       (jump-to-version cgc (lazy-code-lco-true succ) ctx))
                    (else
                       (jump-to-version cgc (lazy-code-lco-false succ) ctx)))))

          (gen-ast (cadr ast) check)))))

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

;; Store the cc table associated to each lambda (ast -> cctable)
;; cctable is a still vector
(define cctables (make-table test: (lambda (a b) (and (eq?    (car a) (car b))     ;; eq? on ast
                                                      (equal? (cdr a) (cdr b)))))) ;; equal? on ctx information

;; Store the cr table associated to each lambda (ast -> crtable)
;; crtable is a still vector
(define crtables (make-table test: (lambda (a b) (and (eq?    (car a) (car b))
                                                      (equal? (cdr a) (cdr b))))))

;; Create a new cr table with 'init' as stub value
(define (make-cr len init)
  (let ((v (alloc-still-vector len)))
    (let loop ((i 0))
      (if (< i (vector-length v))
        (begin (put-i64 (+ 8 (* 8 i) (- (obj-encoding v) 1)) init)
               (loop (+ i 1)))
        v))))

;; Return cctable associated to key or #f if not yet created
(define (cctable-get cctable-key)
  (table-ref cctables cctable-key #f))

;; Create a new cctable associated to key
;; !! cctable is not filled. Not even with dummy value (0)
(define (cctable-make cctable-key)
  (let* ((len     (+ 1 global-cc-table-maxsize))
         (cctable (make-s64vector len)))
    (table-set! cctables cctable-key cctable)
    cctable))

;; Fill cctable with stub and generic addresses
(define (cctable-fill cctable stub-addr generic-addr)
  ;; Fill cctable
  (put-i64 (+ 8 (- (obj-encoding cctable) 1)) generic-addr) ;; Write generic after header
  (let loop ((i 1))
    (if (< i (s64vector-length cctable))
      (begin (put-i64 (+ 8 (* 8 i) (- (obj-encoding cctable) 1)) stub-addr)
             (loop (+ i 1)))
      cctable)))

;-----------------------------------------------------------------------------

;; Return crtable from crtable-key
;; Return the existing table if already created or create one, add entry, and return it
(define (get-crtable ast crtable-key stub-addr)
  (let ((crtable (table-ref crtables crtable-key #f)))
    (if crtable
        crtable
        (let ((t (make-cr global-cr-table-maxsize stub-addr)))
          (table-set! crtables crtable-key t)
          t))))

;; This is the key used in hash table to find the cc-table for this closure.
;; The key is (ast . free-vars-inf) with ast the s-expression of the lambda
;; and free-vars-inf the type information of free vars ex. ((a . number) (b . char))
;; The hash function uses eq? on ast, and equal? on free-vars-inf.
;; This allows us to use different cctable if types of free vars are not the same.
;; (to properly handle type checks)
(define (get-cctable-key ast ctx fvars-imm fvars-late)
  (cons ast
        (append (map (lambda (n) (cons n 'closure)) fvars-late)
                (foldr (lambda (n r)
                         (if (member (car n) fvars-imm) ;; If this id is a free var of future lambda
                             (cons (cons (car n)
                                         (ctx-identifier-type ctx (cdr n)))
                                   r)
                             r))
                       '()
                       (ctx-env ctx)))))

;; TODO regalloc: Créer de nouvelles entrées dans la table (+ que le type de la valeur de retour)
;;                avec slot-loc free-regs
;; Return crtable key from ast and ctx
;; The key contains ast, stack types, and a list of known identifier with types
(define (get-crtable-key ast ctx)
  (cons ast
        (list (ctx-slot-loc ctx)
              (ctx-free-regs ctx)
              (ctx-free-mems ctx)
              (ctx-stack ctx)
              (ctx-env ctx))))

;; Store pairs associating cctable address to the code of the corresponding function
(define cctables-loc-code '())
(define entry-points-locs (make-table test: eq?))

(define (get-entry-points-loc ast stub-addr)
  (let ((r (table-ref entry-points-locs ast #f)))
    (if r
        r
        (let ((v (alloc-still-vector 1)))
          (vector-set! v 0 (quotient stub-addr 4)) ;; quotient 4 because vector-set write the encoded value (bug when using put-i64?)
          (table-set! entry-points-locs ast v)
          v))))

;;
;; FREE VARS
;;

;; free-offset is the current free variable offset position from alloc-ptr
;; clo-offset is the closure offset position from alloc-ptr
(define (gen-free-vars cgc ids ctx free-offset base-reg)
  (if (null? ids)
      #f
      (let* ((id (car ids))
             (identifier (cdr (assoc id (ctx-env ctx))))
             (loc (ctx-identifier-loc ctx identifier))
             (opn
               (cond ;; No loc, free variable which is only in closure
                     ((ctx-loc-is-freemem? loc)
                       (let* (;; Get closure loc
                              (closure-loc  (ctx-get-closure-loc ctx))
                              (closure-opnd (codegen-loc-to-x86opnd (ctx-fs ctx) closure-loc))
                              ;; Get free var offset
                              (fvar-pos (cdr loc))
                              (fvar-offset (+ 16 (* 8 fvar-pos)))) ;; 16:header,entrypoint -1: pos starts from 1 and not 0
                         (if (ctx-loc-is-memory? closure-loc)
                             (begin (x86-mov cgc (x86-rax) closure-opnd)
                                    (set! closure-opnd (x86-rax))))
                         (x86-mov cgc (x86-rax) (x86-mem (- fvar-offset TAG_MEMOBJ) closure-opnd))
                         (x86-rax)))
                     ;;
                     ((ctx-loc-is-memory? loc)
                       (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (ctx-fs ctx) loc))
                       (x86-rax))
                     ;;
                     (else
                       (codegen-reg-to-x86reg loc)))))
        (x86-mov cgc (x86-mem (* 8 free-offset) base-reg) opn)
        (gen-free-vars cgc (cdr ids) ctx (+ free-offset 1) base-reg))))

;; Return all free vars used by the list of ast knowing env 'clo-env'
(define (free-vars-l lst params enc-ids)
  (if (null? lst)
      '()
      (set-union (free-vars   (car lst) params enc-ids)
                 (free-vars-l (cdr lst) params enc-ids))))

;;
;; UTILS
;;

;; Check if type at index ctx-idx represents a constant
;; * If it is a constant, free a register, put it in a register,
;;   update ctx, return updated ctx
;; * Else return unmodified ctx
(define (drop-cst-value cgc ctx ctx-idx)

  (define (alloc-cstfn reg cst)
    (let ((entry-obj (asc-globalfn-entry-get cst)))
      (gen-closure cgc reg #f entry-obj '())))

  (define (alloc-cst reg cst)
    (let ((opnd (codegen-reg-to-x86reg reg)))
      (x86-mov cgc opnd (x86-imm-int (obj-encoding cst)))))

  (let* ((type (ctx-get-type ctx ctx-idx))
         (cst? (ctx-type-is-cst type)))
    (if cst?
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx #f 0))
               (cst (ctx-type-cst type)))
          (apply-moves cgc ctx moves)
          ;; Move object to free register
          (if (ctx-tclo? type)
              (alloc-cstfn reg cst)
              (alloc-cst   reg cst))
          ;; Update & return ctx
          (let* ((ntype ((ctx-type-ctor type)))
                 (ctx (ctx-set-type ctx ctx-idx ntype #f))
                 (ctx (ctx-set-loc ctx (stack-idx-to-slot ctx ctx-idx) reg)))
            ctx))
        ctx)))

;; Get formal params from list of params
;; Ex: (formal-params '(a b c)  ) -> '(a b c)
;;     (promal-params '(a b . c)) -> '(a b)
(define (formal-params l)
  (if (not (pair? l))
     '()
     (cons (car l) (formal-params (cdr l)))))

;; Return label of a stub generating error with 'msg'
(define (get-label-error msg) (list-ref (add-callback #f   0 (lambda (ret-addr selector) (error msg))) 0))
