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
(include "~~lib/_x86#.scm")
(include "config.scm")
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
;; (entry-obj . stack) -> label list
;; Associate a list of label to a pair entry-obj/stack
;; This structure is used to store all addresses where the compiler generated a
;; direct jump to a stub.
;; When the stub generates a version stored in this entry object
;; it patches all stored labels and clear the table entry
;;
;; cc-idx is the cctable index if using cctable, #f otherwise
(define asc-entry-load
  (make-table
    test: (lambda (k1 k2)
            (and (eq? (car k1) (car k2))     ;; eq? on cctables
                 (eq? (cdr k1) (cdr k2)))))) ;; eq? on cc-idx
;; Add an entry to the table
(define (asc-entry-load-add entry-obj cc-idx label)
  (let ((r (table-ref asc-entry-load (cons entry-obj cc-idx) '())))
    (table-set! asc-entry-load (cons entry-obj cc-idx) (cons label r))))
;; Get all labels from entry object and cc-idx
(define (asc-entry-load-get entry-obj cc-idx)
  (table-ref asc-entry-load (cons entry-obj cc-idx) '()))
;; Clear the entry for the entry-object/cc-idx
(define (asc-entry-load-clear entry-obj cc-idx)
  (table-set! asc-entry-load (cons entry-obj cc-idx) '())) ;; TODO: remove table entry

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
;; Associate an fn-num & ep entry object to an ast (eq? on ast)
(define asc-ast-epentry (make-table test: eq?))
(define (asc-ast-epentry-get ast)
  (table-ref asc-ast-epentry ast #f))
(define (asc-ast-epentry-add ast eo)
  (assert (not (asc-ast-epentry-get ast))
          "Internal error")
  (table-set! asc-ast-epentry ast eo))

(define asc-fnnum-ctx (make-table))
(define (asc-fnnum-ctx-get fn-num)
  (let ((r (table-ref asc-fnnum-ctx fn-num #f)))
    (assert r "Internal error")
    r))
(define (asc-fnnum-ctx-set fn-num ctx)
  (table-set! asc-fnnum-ctx fn-num ctx))

(define asc-cnnum-lco (make-table))
(define (asc-cnnum-lco-add cn-num lazy-code)
  (table-set! asc-cnnum-lco cn-num lazy-code))
(define (asc-cnnum-lco-get cn-num)
  (table-ref asc-cnnum-lco cn-num))

(define asc-cnnum-table (make-table))
(define (asc-cnnum-table-add cn-num crtable)
  (table-set! asc-cnnum-table cn-num crtable))
(define (asc-cnnum-table-get cn-num)
  (let ((r (table-ref asc-cnnum-table cn-num)))
    (if (procedure? r)
        (let ((table (r)))
          (table-set! asc-cnnum-table cn-num table)
          table)
        r)))

(define asc-cnnum-ctx (make-table))
(define (asc-cnnum-ctx-add cn-num ctx)
  (table-set! asc-cnnum-ctx cn-num ctx))
(define (asc-cnnum-ctx-get cn-num)
  (table-ref asc-cnnum-ctx cn-num))

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
(define (primitive-box-fl-args? prim)  (list-ref  prim 4))
(define (primitive-rettype prim)       (list-ref  prim 5))
(define (primitive-nbargs prim)        (list-ref  prim 6))
(define (primitive-argtypes prim)      (list-tail prim 7))

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
    ;; Symbol            LCO cst all    LCO getter        Codegen function          Box fl args? Return-type Nb-args Args-type
    (car                 ,cst-car       #f                ,codegen-p-cxr                #f       ,ATX_UNK 1 ,ATX_PAI                   )
    (cdr                 ,cst-cdr       #f                ,codegen-p-cxr                #f       ,ATX_UNK 1 ,ATX_PAI                   )
    (cons                #f             #f                ,codegen-p-cons               #t       ,ATX_PAI 2 ,ATX_ALL ,ATX_ALL          )
    (eq?                 ,cst-eq?       ,lco-p-eq?        #f                            #f       ,ATX_BOO 2 ,ATX_ALL ,ATX_ALL          )
    (char=?              ,cst-char=?    ,lco-p-eq?        #f                            #f       ,ATX_BOO 2 ,ATX_CHA ,ATX_CHA          )
    (quotient            ,cst-binop     ,lco-p-binop      #f                            #f       ,ATX_INT 2 ,ATX_INT ,ATX_INT          )
    (modulo              ,cst-binop     ,lco-p-binop      #f                            #f       ,ATX_INT 2 ,ATX_INT ,ATX_INT          )
    (remainder           ,cst-remainder ,lco-p-binop      #f                            #f       ,ATX_INT 2 ,ATX_INT ,ATX_INT          )
    (zero?               ,dummy-cst-all ,lco-p-zero?      #f                            #f       ,ATX_BOO 1 ,ATX_NUM                   )
    (not                 ,cst-not       ,lco-p-not        ,codegen-p-not                #f       ,ATX_BOO 1 ,ATX_ALL                   )
    (set-car!            #f             #f                ,codegen-p-set-cxr!           #t       ,ATX_VOI 2 ,ATX_PAI ,ATX_ALL          )
    (set-cdr!            #f             #f                ,codegen-p-set-cxr!           #t       ,ATX_VOI 2 ,ATX_PAI ,ATX_ALL          )
    (vector-length       ,cst-vec-len   #f                ,codegen-p-vector-length      #f       ,ATX_INT 1 ,ATX_VEC                   )
    (vector-ref          ,cst-vec-ref   #f                ,codegen-p-vector-ref         #f       ,ATX_UNK 2 ,ATX_VEC ,ATX_INT          )
    (char->integer       ,cst-char->int #f                ,codegen-p-ch<->int           #f       ,ATX_INT 1 ,ATX_CHA                   )
    (integer->char       ,cst-int->char #f                ,codegen-p-ch<->int           #f       ,ATX_CHA 1 ,ATX_INT                   )
    (string-ref          ,cst-str-ref   #f                ,codegen-p-string-ref         #f       ,ATX_CHA 2 ,ATX_STR ,ATX_INT          )
    (string-set!         #f             #f                ,codegen-p-string-set!        #t       ,ATX_VOI 3 ,ATX_STR ,ATX_INT ,ATX_CHA )
    (vector-set!         #f             #f                ,codegen-p-vector-set!        #t       ,ATX_VOI 3 ,ATX_VEC ,ATX_INT ,ATX_ALL )
    (string-length       ,cst-str-len   #f                ,codegen-p-string-length      #f       ,ATX_INT 1 ,ATX_STR                   )
    (exit                #f             #f                 #f                           #f       ,ATX_VOI 0                            )
    (make-vector         #f             #f                ,codegen-p-make-vector        #t       ,ATX_VEC 2 ,ATX_INT ,ATX_ALL          )
    (make-string         #f             #f                ,codegen-p-make-string        #f       ,ATX_STR 2 ,ATX_INT ,ATX_CHA          )
    (eof-object?         ,dummy-cst-all ,lco-p-eof-obj    ,codegen-p-eof-object?        #f       ,ATX_BOO 1 ,ATX_ALL                   )
    (symbol->string      ,cst-sym->str  #f                ,codegen-p-symbol->string     #f       ,ATX_STR 1 ,ATX_SYM                   )
    (current-output-port #f             ,lco-p-cur-x-port #f                            #f       ,ATX_OPO 0                            )
    (current-input-port  #f             ,lco-p-cur-x-port #f                            #f       ,ATX_IPO 0                            )
    (number?             ,cst-number?   ,lco-p-number?    #f                            #f       ,ATX_BOO 1 ,ATX_ALL                   )
    (##apply             #f             ,lco-p-apply      #f                            #f       ,ATX_UNK 2 ,ATX_CLO ,ATX_ALL          )
    (##box               #f             #f                ,codegen-p-box                #t       ,ATX_BOX 1 ,ATX_ALL                   )
    (##unbox             #f             #f                ,codegen-p-unbox              #t       ,ATX_UNK 1 ,ATX_ALL                   )
    (##set-box!          #f             #f                ,codegen-p-set-box            #t       ,ATX_VOI 2 ,ATX_ALL ,ATX_ALL          )
    (##gettime-ns        #f             #f                ,codegen-p-gettime-ns         #f       ,ATX_INT 0                            )
    (vector              #f             ,lco-p-vector     #f                            #t       ,ATX_VEC #f                           )
    (list                #f             ,lco-p-list       #f                            #t       ,ATX_PAI #f                           )
    ;; These primitives are inlined during expansion but still here to build lambda
    (real?               ,dummy-cst-all #f                #f                            #f       ,ATX_BOO 1 ,ATX_ALL                   )
    (eqv?                ,dummy-cst-all #f                #f                            #f       ,ATX_BOO 2 ,ATX_ALL ,ATX_ALL          ))))

(define (get-prim-lambda ast sym primitive)
  (let ((nbargs (primitive-nbargs primitive)))
    (if nbargs
        ;; Fixed nb args primitive
        (let* ((args (build-list (primitive-nbargs primitive) (lambda (x) (string->symbol (string-append "arg" (number->string x))))))
               (args-nodes (map atom-node-make args)))
          `(lambda ,args (,ast ,@args-nodes)))
        ;; Rest param primitive
        (cond ((eq? sym 'list)
                 (let ((node-l (atom-node-make 'l)))
                   `(lambda l ,node-l)))
              ((eq? sym 'vector)
                 (let ((node-l (atom-node-make 'l))
                       (node-lv (atom-node-make 'list->vector)))
                   (error "U")))
              (else (error "Internal error"))))))

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

(define (atom-node-val-set! node n)
  (set-car! (cdr node) n))

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
    (cond ((string? val)          (mlc-literal val ast succ))
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
      (let ((ctx (ctx-push ctx (literal->ctx-type lit) #f)))
        (jump-to-version cgc succ ctx)))))

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
           (cond ((and (ctx-type-boo? type)
                       (ctx-type-cst? type))
                    (if (ctx-type-cst type)
                        (lazy-code-lco-true succ)
                        (lazy-code-lco-false succ)))
                 ((and (not (ctx-type-boo? type))
                       (not (ctx-type-unk? type)))
                    (lazy-code-lco-true succ))
                 (else #f)))))

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
              ((or (and local  (lcl-inlined-cond? ctx (cdr local)))
                   (and global (gbl-inlined-cond? sym)))
               =>
               (lambda (lco)
                 (jump-to-version cgc lco ctx)))
              ;; Identifier local and cst literal
              ((and local
                    (ctx-type-cst? (ctx-identifier-type ctx (cdr local)))
                    (not (ctx-identifier-loc ctx (cdr local))))
                 ;; TODO use =>
                 (let* ((ctx (ctx-push ctx (ctx-identifier-type ctx (cdr local)) #f sym)))
                   (jump-to-version cgc succ ctx)))
              ;; Identifier is a local variable
              (local
                (gen-get-localvar cgc ast ctx local succ))
              ;; Identifier is a global variable
              (global
                (gen-get-globalvar cgc ast ctx global succ))
              ;; Primitive
              ((primitive-get sym) =>
                 (lambda (r)
                   (let ((ast (get-prim-lambda ast sym r)))
                     (jump-to-version cgc (gen-ast ast succ) ctx))))
              (else (gen-error cgc (ERR_UNKNOWN_VAR sym))))))))

(define (gen-get-localvar cgc ast ctx local succ)
  (mlet ((type (ctx-identifier-type ctx (cdr local)))
         (moves/reg/ctx
           (if (ctx-type-flo? type)
               (ctx-get-free-freg ast ctx succ 0)
               (ctx-get-free-reg ast ctx succ 0)))
         (loc (ctx-identifier-loc ctx (cdr local))))

    (apply-moves cgc ctx moves)

    (if (ctx-loc-is-freemem? loc)
        ;; It's a free var that is only in closure
        (let ((lclo (ctx-get-closure-loc ctx)))
          (codegen-get-free cgc (ctx-fs ctx) (ctx-ffs ctx) reg lclo loc))
        ;; The variable is in a register or in non closure memory
        (apply-moves cgc ctx (list (cons loc reg))))

    (jump-to-version cgc succ (ctx-push ctx type reg (car local)))))

(define (gen-get-globalvar cgc ast ctx global succ)

  (let ((type (or (global-stype global) (make-ctx-tunk))))

    (if (ctx-type-cst? type)
        ;; Type is cst, push cst to ctx
        (jump-to-version cgc succ (ctx-push ctx type #f))
        ;; Type is not a cst, free a register and use it
        (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 0)))
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
                     (gen-set-globalvar cgc ast ctx global succ)
                     (error "Internal error"))))))
         (lazy-drop
           (make-lazy-code
             #f
             (lambda (cgc ctx)
               (x86-label cgc (asm-make-label #f (new-sym 'LABEL_SET_)))
               (let ((ctx (drop-cst-value cgc ast ctx 0)))
                 (jump-to-version cgc lazy-set! ctx))))))

    (gen-ast (caddr ast) lazy-drop)))

(define (gen-set-globalvar cgc ast ctx global succ)
  (mlet ((pos (global-pos global))
         (moves/reg/ctx (ctx-get-free-reg ast ctx succ 1))
         (tval (ctx-get-type ctx 0))
         (cst? (and (ctx-type-cst? tval)
                    (not (ctx-get-loc ctx 0))))
         (lval (if cst?
                   (ctx-type-cst tval)
                   (ctx-get-loc ctx 0))))

    (apply-moves cgc ctx moves)
    (codegen-set-global cgc (ctx-fs ctx) (ctx-ffs ctx) reg pos lval cst?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tvoi) reg))))

;;-----------------------------------------------------------------------------
;; INTERNAL FORMS

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)

  (let ((global (asc-globals-get (cadr ast))))
    (cond
          ((and global (ctx-type-clo? (global-stype global)))
            ;; CONST FN TODO: remove when const versioning implemented!
            (let* ((identifier (cadr ast))
                   (fn-num (init-entry-cst (caddr ast) '() (ctx-init))))
              (ctx-type-cst-set! (global-stype global) fn-num)
              (make-lazy-code
                #f
                (lambda (cgc ctx)
                  (jump-to-version cgc succ (ctx-push ctx (global-stype global) #f))))))
          (else
            (let* ((val (and (atom-node? (caddr ast)) (atom-node-val (caddr ast))))
                   (primitive (and val (assoc val primitives))))
              (if primitive
                  ;; form (define sym primitive)
                  (begin (set! primitives
                           (cons (cons (cadr ast) (cdr primitive))
                                 primitives))
                         (make-lazy-code #f
                             (lambda (cgc ctx)
                               (jump-to-version cgc succ (ctx-push ctx (make-ctx-tbooc #f) #f)))))
                  ;; other
                  (let* ((identifier (cadr ast))
                         (lazy-bind (make-lazy-code
                                      #f
                                      (lambda (cgc ctx)
                                        (mlet ((pos (global-pos (asc-globals-get identifier))) ;; Lookup in globals
                                               ;;
                                               (moves/reg/ctx (ctx-get-free-reg ast ctx succ 1))
                                               (type (ctx-get-type ctx 0))
                                               (cst? (and (ctx-type-cst? type) (not (ctx-get-loc ctx 0))))
                                               (lvalue (if cst?
                                                           (ctx-type-cst type)
                                                           (ctx-get-loc ctx 0))))
                                          (apply-moves cgc ctx moves)
                                          (codegen-define-bind cgc (ctx-fs ctx) (ctx-ffs ctx) pos reg lvalue cst?)
                                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tvoi) reg))))))
                         (lazy-drop
                           (make-lazy-code
                            #f
                             (lambda (cgc ctx)
                               (let ((type (ctx-get-type ctx 0)))
                                 (if (and (ctx-type-cst? type)
                                          (ctx-type-clo? type))
                                     (let ((ctx (drop-cst-value cgc ast ctx 0)))
                                       (jump-to-version cgc lazy-bind ctx))
                                     (jump-to-version cgc lazy-bind ctx))))))
                         (lazy-val (gen-ast (caddr ast) lazy-drop)))

                    (put-i64 (+ globals-addr (* 8 (global-pos (asc-globals-get (cadr ast))))) ENCODING_VOID)
                    lazy-val)))))))

;;
;; Make lazy code from LAMBDA
;;

;;
;; Create and return a generic prologue lco
(define (get-lazy-generic-prologue ast succ rest-param nb-formal)
  (make-lazy-code-entry
    #f
    rest-param
    (lambda (cgc ctx)
      (let ((nb-args (ctx-nb-args ctx))
            (label-next (asm-make-label #f (new-sym 'label-next))))
        ;;
        (if (not rest-param)
            (codegen-prologue-gen-nrest cgc nb-args)
            (codegen-prologue-gen-rest  cgc (ctx-fs ctx) (ctx-ffs ctx) nb-args))
        ;;
        (jump-to-version cgc succ ctx)))))

;; TODO WIP MOVE
(define (gen-drop-float cgc ctx ast idx-from idx-to)
  (if (= idx-from idx-to)
      ctx
      (let* ((type (ctx-get-type ctx idx-from)))
        (if (and (ctx-type-flo? type)
                 (not (ctx-type-cst? type)))
            (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx #f 0))
                   (loc   (ctx-get-loc ctx idx-from))
                   (opnd  (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) loc))
                   (ropnd (codegen-reg-to-x86reg reg)))
              (apply-moves cgc ctx moves)
              (gen-allocation-imm cgc STAG_FLONUM 8)
              (if (ctx-loc-is-fregister? loc)
                  (x86-movsd cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) opnd)
                  (begin
                    (x86-mov cgc (x86-rax) opnd)
                    (x86-mov cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) (x86-rax))))
              (x86-lea cgc ropnd (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))
              (let* ((ident (ctx-ident-at ctx idx-from))
                     (ctx
                       ;; Remove slot-info if the slot belongs to an identifier with > 1 slots
                       (if (and ident (= (length (identifier-sslots (cdr ident))) 1))
                           ctx
                           (ctx-remove-slot-info ctx idx-from)))
                     (ctx (ctx-set-loc ctx (stack-idx-to-slot ctx idx-from) reg))
                     (ctx (ctx-set-type ctx idx-from (make-ctx-tunk) #f)))
                (gen-drop-float cgc ctx ast (+ idx-from 1) idx-to)))
            (gen-drop-float cgc ctx ast (+ idx-from 1) idx-to)))))

;;
;; Create and return a prologue lco
(define (get-lazy-prologue ast succ rest-param)
  (lazy-code-f-entry! succ)
  (if rest-param
      (lazy-code-f-rest! succ))
  succ)

;;
;; Create and return a function return lco
(define (get-lazy-return)

  (define (gen-return-rp cgc ctx)
    (set! ctx (drop-cst-value cgc #f ctx 0))
    (let* ((fs (ctx-fs ctx))
           (ffs (ctx-ffs ctx))
           ;; Return value loc
           (type-ret (ctx-get-type ctx 0))
           ;(type-arg (ctx-identifier-type ctx (cdr (ctx-ident ctx 'n0))))
           (lret     (ctx-get-loc ctx 0))
           ;; Return address object loc
           (laddr (ctx-get-retobj-loc ctx)))
      (codegen-return-rp cgc fs ffs laddr lret (ctx-type-flo? type-ret))))

  (define (gen-return-cr cgc ctx)

    (let ((cridx (crtable-get-idx (ctx-init-return ctx))))

        (assert (not (and (not cridx)
                          (ctx-type-flo? (ctx-get-type ctx 0))))
                "NYI case, cr overflow and ret value is a tflo")

        (if (or (not (const-versioned? (ctx-get-type ctx 0)))
                (not cridx))
            (set! ctx (drop-cst-value cgc #f ctx 0)))

        (let* ((fs (ctx-fs ctx))
               (ffs (ctx-ffs ctx))
               ;; Return value loc
               (type-ret (ctx-get-type ctx 0))
               ;(type-arg (ctx-identifier-type ctx (cdr (ctx-ident ctx 'n0))))
               (lret     (ctx-get-loc ctx 0))
               ;; Return address object loc
               (laddr (ctx-get-retobj-loc ctx))
               (taddr (ctx-get-type ctx (- (length (ctx-stack ctx)) 1)))
               ;;
               (cst? (not lret)))

          (assert (if (not lret) (ctx-type-cst? type-ret) #t) "Internal error")

          (if (ctx-type-cst? taddr)
              (let ((lco (asc-cnnum-lco-get (ctx-type-cst taddr)))
                    (opret (if (ctx-type-flo? type-ret)
                               (codegen-freg-to-x86reg return-freg)
                               (codegen-reg-to-x86reg return-reg)))
                    (opretval (and lret (codegen-loc-to-x86opnd fs ffs lret))))

                (if (and opretval
                         (not (eq? opret opretval)))
                    (if (ctx-type-flo? type-ret)
                        (x86-movsd cgc opret opretval)
                        (x86-mov cgc opret opretval)))

                (codegen-return-clean cgc fs ffs)
                (let* ((ctx (asc-cnnum-ctx-get (ctx-type-cst taddr)))
                       (ctx
                         (cond ((not lret)
                                 (ctx-push ctx type-ret #f))
                               ((ctx-type-flo? type-ret)
                                 (ctx-push ctx type-ret return-freg))
                               (else
                                 (ctx-push ctx type-ret return-reg)))))
                  (jump-to-version cgc lco ctx)))
              (codegen-return-cr cgc fs ffs laddr lret cridx (ctx-type-flo? type-ret) cst?)))))

      (make-lazy-code-ret ;; Lazy-code with 'ret flag
        #f
        (lambda (cgc ctx)
          (if opt-return-points
              (gen-return-cr cgc ctx)
              (gen-return-rp cgc ctx)))))

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

  (list
    lazy-prologue
    (add-fn-callback
      1
      fn-num
      (lambda (stack cc-idx cn-num ret-addr selector closure)

        (cond ;; CASE 1 - Use entry point (no cctable)
              ((eq? opt-entry-points #f)
                 (let ((lazy-prologue-gen (get-lazy-generic-prologue ast lazy-body rest-param (length params))))
                   (fn-generator closure lazy-prologue-gen #f cc-idx cn-num #f)))
              ;; CASE 2 - Function is called using generic entry point
              ((= selector 1)
                 (let ((lazy-prologue-gen (get-lazy-generic-prologue ast lazy-body rest-param (length params))))
                   (fn-generator #f lazy-prologue-gen #f cc-idx cn-num #t)))
              ;; CASE 3 - Use multiple entry points
              (else
                 (fn-generator #f lazy-prologue stack cc-idx cn-num #f)))))))

(define (get-entry-obj ast ctx fvars-imm fvars-late all-params bound-id)

  (define fn-num #f)

  ;; Generator used to generate function code waiting for runtime data
  ;; First create function entry ctx
  ;; Then generate function prologue code
  (define (fn-generator closure prologue stack cc-idx cn-num generic?)
    ;; In case the limit in the number of version is reached, we give #f to ctx-init-fn to get a generic ctx
    ;; but we still want to patch cctable at index corresponding to stack
    (let* ((ctxstack (if generic? #f stack))
           (ctx (ctx-init-fn cn-num ctxstack ctx all-params (append fvars-imm fvars-late) fvars-late fn-num bound-id)))
      (gen-version-fn ast closure entry-obj prologue ctx cc-idx generic?)))

  ;; ---------------------------------------------------------------------------
  ;; Return 'entry-obj' (entry object)
  ;; An entry object is the object that contains entry-points-locs
  ;; In the case of -cc, entry object is the cctable
  (define (get-entry-obj-cc)
    (let* ((r (get-cctable ast ctx fvars-imm fvars-late))
           (new? (car r))
           (cctable (cadr r)))
      (set! fn-num (cddr r))
      (if new?
          (mlet (;; Create stub only if cctable is new
                 (lco/stub-labels  (create-fn-stub ast fn-num fn-generator))
                 (stub-addr    (asm-label-pos (list-ref stub-labels 0)))
                 (generic-addr (asm-label-pos (list-ref stub-labels 1))))
            (asc-globalfn-entry-add fn-num (cons cctable (cons lco stub-addr)))
            (asc-entry-stub-add cctable generic-addr stub-addr)
            (cctable-fill cctable stub-addr generic-addr)))
      (values fn-num cctable)))

  ;; In the case of -ep, entry object is the still vector of size 1 that contain the single entry point
  (define (get-entry-obj-ep)
    (let ((existing (asc-ast-epentry-get ast)))
      (if existing
          ;; An entry points object already exists for this ast, use it!
          (values (car existing) (cdr existing))
          ;; TODO: we are supposed to use only one e.p. with -ep objects
          ;;       use a max-selector of 0 in create-fn-stub, and use only one -addr
          (mlet (;; Create stub
                 (fn-num       (new-fn-num))
                 (lco/stub-labels  (create-fn-stub ast fn-num fn-generator))
                 (stub-addr    (asm-label-pos (list-ref stub-labels 0)))
                 (generic-addr (asm-label-pos (list-ref stub-labels 1)))
                 (entryvec     (get-entry-points-loc ast stub-addr)))
            (asc-globalfn-entry-add fn-num (cons entryvec (cons lco stub-addr)))
            (asc-entry-stub-add entryvec generic-addr stub-addr)
            (asc-ast-epentry-add ast (cons fn-num entryvec))
            (values fn-num entryvec)))))

  (define entry-obj #f)

  (call-with-values
    (if opt-entry-points
        get-entry-obj-cc
        get-entry-obj-ep)
    (lambda (fn-num obj)
      (set! entry-obj obj)
      (asc-fnnum-ctx-set fn-num (list ctx all-params (append fvars-imm fvars-late) fvars-late fn-num bound-id))
      (values fn-num entry-obj))))


;;
;; Init constant lambda
(define (init-entry-cst ast free ctx)

  ;; Flatten list of param (include rest param)
  (define all-params (flatten (cadr ast)))

  (call-with-values
    (lambda () (get-entry-obj ast ctx free '() all-params #f))
    (lambda (fn-num entry-obj)
      fn-num)))

;;
;; Init non constant lambda
(define (init-entry ast ctx fvars-imm fvars-late bound-id)

  ;; Flatten list of param (include rest param)
  (define all-params (flatten (cadr ast)))

  (call-with-values
    (lambda () (get-entry-obj ast ctx fvars-imm fvars-late all-params bound-id))
    (lambda (fn-num entry-obj)
      (list fn-num entry-obj))))

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
               (imm '())    ;; imm free
               (cst '())    ;; cst free
               (cstid '())) ;; id-cst free (cst associated to cst identifier)
      (if (null? ids)
          (list cst cstid imm late)
          (let ((id (car ids)))
            (let* ((identifier (cdr (assoc id (ctx-env ctx))))
                   (type (ctx-identifier-type ctx identifier))
                   (loc  (ctx-identifier-loc ctx identifier)))
              (cond ((and (ctx-type-cst? type)
                          (identifier-cst identifier)
                          (not loc))
                       (loop (cdr ids) imm cst (cons id cstid)))
                    ((and (ctx-type-cst? type)
                          (not loc))
                       (loop (cdr ids) imm (cons id cst) cstid))
                    (else
                       (loop (cdr ids) (cons id imm) cst cstid)))))))))

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
            (let* ((type (make-ctx-tcloc fn-num))
                   (ctx  (ctx-push ctx type #f)))
              (jump-to-version cgc succ ctx))

            ;; Gen code to create closure
            (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 0)))
              (apply-moves cgc ctx moves)
              (gen-closure cgc reg ctx entry-obj fvars-ncst)
              ;;
              (jump-to-version cgc succ (ctx-push ctx (make-ctx-tcloi fn-num) reg))))))))

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

;; Return a pair (cst? . cst)
;; cst? is #f if the atom node is not a cst value
;; cst  is the cst value if node is a cst value (perm object if mem-allocated)
;; pair is either (#f . #f) or (#t . cst)
(define (cst-from-atom atom-node)
  (if (symbol? (atom-node-val atom-node))
      (cons #f #f)
      (let* ((val (atom-node-val atom-node))
             (cst (if (and (pair? val) (eq? (car val) 'quote))
                      (cadr val)
                      val)))
        (if (and (##mem-allocated? cst)
                 (not (eq? (mem-allocated-kind cst) 'PERM)))
            (set! cst (copy-permanent cst #f perm-domain)))
        (cons #t cst))))

(define (mlc-let ast succ)
  (define (cst-obj expr ctx) ;; TODO: detect fun cst
    (cond ;; Atom node, but not identifier
          ((atom-node? expr)
             (cst-from-atom expr))
          ;; lambda expr
          ((and (pair? expr)
                (eq? (car expr) 'lambda))
             (let ((r (get-free-infos expr '() ctx)))
               (if (and (null? (car r))
                        (null? (caddr r))
                        (null? (cadddr r)))
                   ;; if no free, or only cst-id free, it's a cst obj
                   (cons #t (init-entry-cst expr (flatten r) ctx))
                   ;; else, it's not a cst obj
                   (cons #f #f))))
          ;; Others
          (else
            (cons #f #f))))

  (define bindings (cadr ast))
  (define ids (map car bindings))
  (define values (map cadr bindings))
  (define body (caddr ast))

  (define (get-bindings-info ctx)
    (let loop ((cst '())
               (ncst '())
               (bindings bindings))
      (if (null? bindings)
          (list cst (reverse ncst))
          (let* ((binding (car bindings))
                 (id (car binding))
                 (v  (cadr binding))
                 (r  (cst-obj v ctx)))
            (cond ((and (car r) (pair? v) (eq? (car v) 'lambda))
                    ;; Binding is a fn cst, add (id cst #t) (#t -> function)
                    (loop (cons (list id (cdr r) #t) cst) ncst (cdr bindings)))
                  ((car r)
                    ;; Binding is a cst, add (id cst #f) (#f -> not a function)
                    (loop (cons (list id (cdr r) #f) cst) ncst (cdr bindings)))
                  (else
                    ;; Binding is not a cst, add (id . expr)
                    (loop cst (cons (cons id v) ncst) (cdr bindings))))))))

  ;; Bind all variables and jump to body
  (define (lazy-bind cst ncst)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let* (;; ncst id-idx set
               (id-idx
                 (let loop ((idx (- (length ncst) 1)) (ids (map car ncst)))
                   (if (null? ids)
                       '()
                       (cons (cons (car ids) idx)
                             (loop (- idx 1) (cdr ids))))))
               ;; Bind locals then consts
               (ctx (ctx-bind-locals ctx id-idx))
               (ctx (ctx-bind-consts ctx cst #t))
               ;;
               (lazy-let-out (get-lazy-lets-out ast ids 0 succ))
               (lazy-body (gen-ast body lazy-let-out)))
          (jump-to-version cgc lazy-body ctx)))))

  (make-lazy-code
    #f
    (lambda (cgc ctx)
      ;; Get cst/ncst info
      (mlet ((cst/ncst (get-bindings-info ctx)))
        (let ((lco-first
                ;; Gen all ncst and jump to bind
                (foldr (lambda (el r)
                         (gen-ast (cdr el) r))
                       (lazy-bind cst ncst)
                       ncst)))
          (jump-to-version cgc lco-first ctx))))))

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
    (mlet ((proc/other        (group-proc-others ctx ids bindings))
           (const-proc/proc   (group-const-proc proc))
           (const-proc/proc   (group-recursive-const-proc const-proc proc))
           (proc              (proc-vars-insert-pos proc (map car const-proc)))
           (const-nproc/other (group-cnproc-others other)))
        (list proc const-proc const-nproc other)))

  ;; Takes non proc bindings
  ;; TODO: not tested because for every LC tests, gambit frontend propagate and inline constants.
  ;; Group bindings, returns list (const-non-proc others) bindings
  ;; 'others' are bindings with !lambda & !constants values
  ;; 'const-non-proc' are bindings with !lambda & constants values.
  ;; const-non-proc has the form ((id1 cst1 fn?1) (id2 cst2 fn?2) ...) -> see ctx-bind-consts
  (define (group-cnproc-others bindings)
    (let loop ((bindings bindings)
               (cnprocs '())
               (others  '()))
        (if (null? bindings)
            (list cnprocs others)
            (let ((b (car bindings)))
              (if (atom-node? (cadr b))
                  (begin
                    ;(error "Needs to be tested.") ;; see TODO above
                    (let ((r (cst-from-atom (cadr b))))
                      (if (car r)
                          (loop (cdr bindings) (cons (list (car b) (cdr r) #f) cnprocs) others)
                          (loop (cdr bindings) cnprocs (cons b others)))))
                  (loop (cdr bindings) cnprocs (cons b others)))))))


  ;; Takes proc-vars objects.
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
                  ;; Compute new free-info set. Merge cst & cst-id vars of !cst procs
                  (free-info
                    (list (append (car old-free-info)
                                  (cadr old-free-info)
                                  (set-inter const-proc-vars-ids
                                             (cadddr old-free-info)))
                          (caddr old-free-info)
                          (set-sub (cadddr old-free-info) const-proc-vars-ids '()))))
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
                      (let* ((free-info (get-free-infos val bound-ids ctx)))
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
                   (all-free (append (caddr free-info) (cadddr free-info))))
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
              (null? (caddr (caddr el)))) ;; No imm free vars, only late
            proc-vars))

    (let loop ((const-set const-init))
      ;; Compute new set, only keep binding if all of its late vars
      ;; are members of const-set
      (let ((new-const-set
              (keep (lambda (el)
                      (let ((late-ids (cadddr (caddr el))))
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

          (define fn-num #f)

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
                 (fn-n/entry-obj (init-entry ast ctx (append free-cst free-imm) free-late id))
                 (entry-obj-loc (- (obj-encoding entry-obj) 1)))
            (set! fn-num fn-n) ;; update current function fn-num
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
          ;; We do not give ast to ctx-get-free-reg because it could free a register associated to an index
          ;; which is not yet associated to an identifier.
          ;; The association is done with ctx-id-add-idx.
          (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 0)))
            (apply-moves cgc ctx moves)
            (let ((dest (codegen-reg-to-x86reg reg)))
              (x86-lea cgc dest (x86-mem (+ offset-start TAG_MEMOBJ) clo-reg)))
            (let* ((ctx (ctx-push ctx (make-ctx-tcloi fn-num) reg))
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

    (mlet (;; Compute cst-id and cst sets
           ;; cst-id: cst bindings which are always cst
           ;; cst: cst bindings which are detected to be cst using versioning information
           (cst-id/cst
             (let loop ((c const-proc-vars)
                        (cst-id '())
                        (cst '()))
               (if (null? c)
                   (list cst-id cst)
                   (let* ((first (car c))
                          (free-inf (caddr first))
                          (l (list (car first) -1 #t)))
                     (if (null? (car free-inf))
                         (loop (cdr c) (cons l cst-id) cst)
                         (loop (cdr c) cst-id (cons l cst)))))))
           ;;
           (ctx (ctx-bind-consts ctx cst    #f))
           (ctx (ctx-bind-consts ctx cst-id #t)))

      ;; Then, we init all entries and write fn-num values
      (let loop ((l const-proc-vars)
                 (ctx ctx))
        (if (null? l)
            ctx
            (let* ((free-inf   (caddr (car l)))
                   (free-late  (cadddr free-inf))
                   (free-cst   (car free-inf))
                   (free-cstid (cadr free-inf))
                   (fn-num (init-entry-cst (cadr (car l)) (append free-cst free-cstid free-late) ctx)))
              (assert (null? (caddr free-inf)) "Internal error")
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
             (const-nproc-vars (caddr r))
             (other-vars (cadddr r))
             ;; Build LCO chain
             (lazy-let-out (get-lazy-lets-out ast ids 0 succ))
             (lazy-body    (gen-ast body lazy-let-out))
             (lazy-fun     (get-lazy-make-closures lazy-body proc-vars other-vars const-proc-vars))
             (lazy-eval    (get-lazy-eval other-vars lazy-fun)))

        (let* (;; Bind const nproc
               (ctx (ctx-bind-consts ctx const-nproc-vars #t))
               ;; Bind others
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
                    (drop-cst-value cgc ast ctx i)))))))

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
               (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ (length (cdr ast))))
                      (gsym (get-gambit-sym (atom-node-val (car ast))))
                      (nargs (length (cdr ast))))
                 (apply-moves cgc ctx moves)
                 (let loop ((idx (- nargs 1)))
                   (if (>= idx 0)
                       (let ((loc  (ctx-get-loc ctx idx))
                             (type (ctx-get-type ctx idx)))
                         (if (ctx-type-flo? type)
                             ;; Float, push a boxed float
                             (begin (gen-allocation-imm cgc STAG_FLONUM 8)
                                    (x86-movsd cgc (x86-mem -8 alloc-ptr) (codegen-freg-to-x86reg loc))
                                    (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))
                                    (x86-upush cgc (x86-rax))
                                    (loop (- idx 1)))
                             ;; !Float, push value
                             (begin (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) loc))
                                    (x86-upush cgc (x86-rax))
                                    (loop (- idx 1)))))))
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
         (ffs (ctx-ffs ctx))
         (nargs (primitive-nbargs prim))
         (r  (build-list
                  nargs
                  (lambda (n)
                    (let ((type (ctx-get-type ctx n)))
                      ;; assert that if we do not have a loc, type *must* be a cst
                      (assert (or (ctx-get-loc ctx n) (ctx-type-cst? type)) "Internal error")
                      (if (not (ctx-get-loc ctx n))
                          (cons #t (ctx-type-cst type))
                          (cons #f (ctx-get-loc ctx n)))))))
         (cst? (map car r))
         (locs (map cdr r))
         (args (append (list cgc fs ffs op reg inlined-cond?)
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
        (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 1)))
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
;; Special primitive 'not'
(define (lco-p-not ast op succ)
  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (let ((type (ctx-get-type ctx 0)))
        (if (or (ctx-type-boo? type)
                (ctx-type-unk? type))
            (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 1)))
              (apply-moves cgc ctx moves)
              (codegen-p-not cgc (ctx-fs ctx) (ctx-ffs ctx) op reg #f (ctx-get-loc ctx 0) #f)
              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg)))
            (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (literal->ctx-type #f) #f)))))))

;;
;; Special primitive 'eof-object?'
(define (lco-p-eof-obj ast op succ)
  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (let ((type (ctx-get-type ctx 0)))
        (if (ctx-type-unk? type)
            (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 1)))
              (apply-moves cgc ctx moves)
              (codegen-p-eof-object? cgc (ctx-fs ctx) (ctx-ffs ctx) op reg #f (ctx-get-loc ctx 0) #f)
              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg)))
            (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (literal->ctx-type #f) #f)))))))

;;
;; Special primitives 'quotient', 'modulo', 'remainder'
(define (lco-p-binop ast op succ)
  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (mlet ((label-div0 (get-label-error ERR_DIVIDE_ZERO))
             (moves/reg/ctx (ctx-get-free-reg ast ctx succ 2))
             (tleft  (ctx-get-type ctx 1))
             (tright (ctx-get-type ctx 0))
             (lcst?  (ctx-type-cst? tleft))
             (rcst?  (ctx-type-cst? tright))
             (lleft  (if lcst? (ctx-type-cst tleft)  (ctx-get-loc ctx 1)))
             (lright (if rcst? (ctx-type-cst tright) (ctx-get-loc ctx 0))))
        (apply-moves cgc ctx moves)
        (if (and lcst? rcst?)
            (error "NN"))
        (codegen-p-binop cgc (ctx-fs ctx) (ctx-ffs ctx) op label-div0 reg lleft lright lcst? rcst?)
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
      (if (or (and (not (ctx-type-unk? typel))
                   (not (ctx-type-unk? typer))
                   (not (ctx-type-is-a? typel typer))
                   (not (ctx-type-is-a? typer typel)))
              (and (ctx-type-flo? typel) (ctx-type-flo? typer)))
          ;; Types are known and !=
          (jump-to-version cgc (lazy-code-lco-false succ) nctx)
          ;;
          (begin
            (codegen-p-eq? cgc (ctx-fs ctx) (ctx-ffs ctx) 'eq? #f #t ll lr lcst? rcst?)
            ((lazy-code-generator succ) cgc nctx x86-jne)))))

  (define (gen-eq? cgc ctx typel typer lcst? rcst?)
    (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 2))
           (ll (if lcst? (ctx-type-cst typel)
                               (ctx-get-loc  ctx 1)))
           (lr (if rcst? (ctx-type-cst typer)
                         (ctx-get-loc  ctx 0)))
           (nctx (ctx-push (ctx-pop-n ctx 2) (make-ctx-tboo) reg)))
      (apply-moves cgc ctx moves)
      (if (or (and (not (ctx-type-unk? typel))
                   (not (ctx-type-unk? typer))
                   (not (ctx-type-is-a? typel typer))
                   (not (ctx-type-is-a? typer typel)))
              (and (ctx-type-flo? typel) (ctx-type-flo? typer)))
          ;; Types are known and !=
          (codegen-set-bool cgc #f reg)
          ;;
          (codegen-p-eq? cgc (ctx-fs ctx) (ctx-ffs ctx) 'eq? reg #f ll lr lcst? rcst?))
      (jump-to-version cgc succ nctx)))

  (make-lazy-code
    ast
    (lambda (cgc ctx)
      (let* ((typel (ctx-get-type ctx 1))
             (typer (ctx-get-type ctx 0))
             (lcst? (ctx-type-cst? typel))
             (rcst? (ctx-type-cst? typer))
             (if-cond? (member 'cond (lazy-code-flags succ))))
        (cond ((and lcst? rcst? if-cond?)
                 (error "NYI a"))
              ((and lcst? rcst?)
                 (if (or (ctx-type-clo? typel)
                         (ctx-type-clo? typer))
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
                      (vec-opnd (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) vec-loc)))

                 (let loop ((idx nbargs))
                   (if (= idx 0)
                       (let* ((ctx (ctx-pop-n ctx (+ nbargs 1)))
                              (ctx (ctx-push ctx (make-ctx-tvec) vec-loc)))
                         (jump-to-version cgc succ ctx))
                       (let* ((val-loc  (ctx-get-loc ctx idx))
                              (type (ctx-get-type ctx idx))
                              (cst? (and (ctx-type-cst? type) (not (ctx-type-id? type))))
                              (val-opnd (if cst?
                                            (x86-imm-int (obj-encoding (ctx-type-cst type)))
                                            (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) val-loc)))
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
                   (cst? (ctx-type-cst? type))
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
                            (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) loc))))
                  (if (or (x86-mem? opnd)
                          (x86-imm? opnd))
                      (begin
                        (x86-mov cgc (x86-rax) opnd)
                        (set! opnd (x86-rax))))
                  (x86-mov cgc (x86-mem (- pair-offset  8) alloc-ptr) opnd))
                (mlet ((moves/reg/ctx (ctx-get-free-reg ast (ctx-pop-n ctx len) succ 0)))
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
                   (cst? (ctx-type-cst? type))
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
                            (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) loc))))
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
      (define cn-num #f)
      ;; Save used registers, generate and push continuation stub
      (set! ctx (cdr (call-save/cont cgc ctx ast succ #f 2 #t #f)))
      ;; Push closure
      (call-get-closure cgc ctx 1)

      (let* ((label-end (asm-make-label #f (new-sym 'apply-end-args)))
             (lsttype (ctx-get-type ctx 0)))
        (cond
          ((and (ctx-type-cst? lsttype)
                (null? (ctx-type-cst lsttype)))
             ;; Only write the number of arguments (0)
             (x86-mov cgc (x86-r11) (x86-imm-int 0)))
          ((ctx-type-cst? lsttype)
             (error "NYI - apply (other cst)"))
          (else
             (let* ((llst  (ctx-get-loc ctx 0))
                    (oplst (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) llst)))
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
                         (x86-mov cgc (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) (car args-regs)) (x86-mem (- OFFSET_PAIR_CAR TAG_PAIR) (x86-rdx)))
                         (x86-mov cgc (x86-rdx) (x86-mem (- OFFSET_PAIR_CDR TAG_PAIR) (x86-rdx)))
                       (loop (cdr args-regs)))))
               (x86-label cgc label-end)
               ;; Reset selector used as tmp reg
               (x86-mov cgc selector-reg (x86-imm-int 0)))))

        (let ((fn-id-inf (call-get-eploc ctx (cadr ast))))
          (x86-mov cgc (x86-rdi) (x86-r11)) ;; Copy nb args in rdi
          (if (and (not opt-entry-points) fn-id-inf (car fn-id-inf))
              (x86-mov cgc (x86-rsi) (x86-imm-int (obj-encoding #f)))
              (x86-mov cgc (x86-rsi) (x86-rax))) ;; Move closure in closure reg
          (gen-call-sequence ast cgc #f #f #f (and fn-id-inf (cdr fn-id-inf)) #f #f))))))

(define (mlc-primitive-d prim ast succ)

  ;; If all operands are cst, return list of ctx types
  ;; If one or more operands are not cst, return #f
  (define (get-all-cst-opnds ctx nopnds)
    (let loop ((idx (- nopnds 1))
               (r '()))
      (if (< idx 0)
          (reverse r)
          (let ((type (ctx-get-type ctx idx)))
            (if (and (ctx-type-cst? type)
                     (not (ctx-type-id? type)))
                (let ((cst (ctx-type-cst type)))
                  (assert (or (not (ctx-type-clo? type))
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
                (if (and (ctx-type-cst? type)
                         (ctx-type-clo? type))
                    (loop (- i 1) (drop-cst-value cgc ast ctx i))
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
              (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ nb-opnds)))
                (apply-moves cgc ctx moves)
                (gen-primitive cgc ctx succ reg prim)))))))

  (define (get-box-fl-args-lco primitive next)
    (if (primitive-box-fl-args? primitive)
        (make-lazy-code #f
          (lambda (cgc ctx)
            (let* ((idx-to (or (primitive-nbargs primitive)
                               (length (cdr ast))))
                   (ctx (gen-drop-float cgc ctx ast 0 idx-to)))
              (jump-to-version cgc next ctx))))
        next))

  ;; Assert primitive nb args
  (assert-p-nbargs prim ast)

  ;;
  (let* ((primitive (primitive-get prim))
         (lazy-primitive   (get-prim-lco primitive))
         (lazy-box-fl-args (get-box-fl-args-lco primitive lazy-primitive))
         (lazy-cst-check   (get-lazy-cst-check primitive lazy-box-fl-args)))

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
  (pp op)
  (error "NYI dummy-cst-all"))

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
(define cst-vec-len   (cst-prim-1 (lambda (cst) (vector-length (ctx-type-cst cst)))))
(define cst-char->int (cst-prim-1 (lambda (cst) (char->integer (ctx-type-cst cst)))))
(define cst-int->char (cst-prim-1 (lambda (cst) (integer->char (ctx-type-cst cst)))))
(define cst-str-len   (cst-prim-1 (lambda (cst) (string-length (ctx-type-cst cst)))))
(define cst-sym->str  (cst-prim-1 (lambda (cst) (symbol->string (ctx-type-cst cst)))))
(define cst-number?   (cst-prim-1 (lambda (cst)
                                    (and (not     (ctx-type-clo?    cst))
                                         (number? (ctx-type-cst cst))))))

;; 2 args cst primitives
(define cst-eq?
  (cst-prim-2 (lambda (cst1 cst2)
                (and (not (ctx-type-clo? cst1))
                     (not (ctx-type-clo? cst2))
                     (eq? (ctx-type-cst cst1)
                          (ctx-type-cst cst2))))))
(define cst-char=?
  (cst-prim-2 (lambda (cst1 cst2)
                (char=? (ctx-type-cst cst1)
                        (ctx-type-cst cst2)))))
(define cst-binop
  (cst-prim-2 (lambda (cst1 cst2)
                (eval (list op
                            (ctx-type-cst cst1)
                            (ctx-type-cst cst2))))))

(define cst-remainder
  (cst-prim-2 (lambda (cst1 cst2)
                (eval (remainder (ctx-type-cst cst1)
                                 (ctx-type-cst cst2))))))

(define cst-vec-ref
  (cst-prim-2 (lambda (cst1 cst2)
                (vector-ref
                  (ctx-type-cst cst1)
                  (ctx-type-cst cst2)))))

(define cst-str-ref
  (cst-prim-2 (lambda (cst1 cst2)
                (string-ref
                  (ctx-type-cst cst1)
                  (ctx-type-cst cst2)))))

;;-----------------------------------------------------------------------------
;; Branches

;;
;; Make lazy code from IF
;;
(define (mlc-if ast succ)

  (letrec (;; If the successor lco is a return lco, duplicate it to have distinct return lco for each branch.
           ;; This avoids the jump in case the two branches go to the return lco with the same context.
           ;; (i.e. the version is inlined for a branch and a JMP is generated for the other branch,
           ;; we want to generate two inlined versions instead).
           (succ1 succ)
           (succ2
             (if (member 'ret (lazy-code-flags succ))
                 (get-lazy-return)
                 succ))
           ;;
           (condition (cadr ast))
           (lazy-code0 (gen-ast (cadddr ast) succ1))
           (lazy-code1 (gen-ast (caddr ast) succ2))
           (lazy-code-test
             (make-lazy-code-cond
               #f
               lazy-code1
               lazy-code0
               (lambda (cgc ctx #!optional x86-op)

                 (let* ((type (ctx-get-type ctx 0))
                        (cst? (ctx-type-cst? type)))
                   (cond ((and (not x86-op) cst? (ctx-type-cst type))
                            (jump-to-version cgc lazy-code1 (ctx-pop ctx)))
                         ((and (not x86-op) cst?)
                            (jump-to-version cgc lazy-code0 (ctx-pop ctx)))
                         (else
                            (let* ((ctx0 (if x86-op ctx (ctx-pop ctx)))   ;; Pop condition result
                                   (ctx1 ctx0)
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
                                    (let ((lcond (ctx-get-loc ctx 0)))
                                      (codegen-if cgc (ctx-fs ctx) (ctx-ffs ctx) label-jump label-false label-true lcond))))))))))))

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
                     (ctx-type-clo? (global-stype global))
                     (ctx-type-cst? (global-stype global)))
                (cons #t (ctx-type-cst (global-stype global)))
                #f)))

        (or (ctx-get-eploc ctx sym)
            global-opt))
      #f))

;;
;; Call steps
;;

;; Save used registers and return updated ctx
(define (call-save/cont cgc ctx ast succ tail? idx-offset apply? prop-cont?)
  (if tail?
      ;; Tail call, no register to save and no continuation to generate
      (if (ctx-const-continuation? ctx)
          (let ((cn-num (ctx-const-continuation ctx)))
            (cons cn-num ctx))
          (cons #f ctx))
      (mlet ((moves/nctx (ctx-save-call ast ctx idx-offset)))
        (define fctx nctx)
        (if (not prop-cont?)
            (begin
              (set! fctx (ctx-fs-inc fctx))
              (set! moves (cons (cons 'fs 1) moves))))
        ;; Save registers
        (apply-moves cgc fctx moves)
        ;; Generate & push continuation
        ;; gen-continuation-* needs ctx without return address slot
        (let* ((cn-num (new-cn-num))
               (lazy-cont
                 (if opt-return-points
                     (gen-continuation-cr cgc ast succ nctx cn-num apply? prop-cont?)
                     (gen-continuation-rp cgc ast succ nctx apply? prop-cont?))))

          (asc-cnnum-lco-add cn-num lazy-cont)
          (if prop-cont?
              (cons cn-num fctx)
              (cons #f fctx))))))

;; Push closure, put it in rax, and return updated ctx
(define (call-get-closure cgc ctx closure-idx)
  (let* ((fs (ctx-fs ctx))
         (ffs (ctx-ffs ctx))
         (loc  (ctx-get-loc     ctx closure-idx)))
    (codegen-load-closure cgc fs ffs loc)))

;; Move args in regs or mem following calling convention
(define (call-prep-args cgc ctx ast nbargs const-fn generic-entry? inlined-call? tail?)

  (let* ((cloloc  (if const-fn #f (ctx-get-loc ctx nbargs)))
         (contloc (ctx-get-loc ctx (- (length (ctx-stack ctx)) 1)))
         (stackp/moves (ctx-get-call-args-moves ast ctx nbargs cloloc contloc tail? generic-entry? inlined-call?))
         (stackp (car stackp/moves))
         (moves (cdr stackp/moves)))

    (let loop ((fs (ctx-fs ctx))
               (locs stackp))
      (if (null? locs)
          (set! ctx (ctx-fs-update ctx fs))
          (begin
            (cond ((eq? (caar locs) 'constfn)
                     (let ((entry-obj (car (asc-globalfn-entry-get (cdar locs)))))
                       (gen-closure cgc 'tmp #f entry-obj '())
                       (x86-upush cgc (codegen-reg-to-x86reg 'tmp))))
                  ((eq? (caar locs) 'const)
                     (x86-upush cgc (x86-imm-int (obj-encoding (cdar locs)))))
                  ((eq? (caar locs) 'flbox)
                     (let ((move (cons (car locs) 'rtmp)))
                       (apply-moves cgc ctx (list move))
                       (x86-upush cgc (codegen-reg-to-x86reg 'tmp))))
                  (else
                     (x86-upush cgc (codegen-loc-to-x86opnd fs (ctx-ffs ctx) (car locs)))))
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
;; nb-nfl-args is the number of non flonum arguments
;; nb-fl-args  is the number of flonum arguments
(define (call-tail-shift cgc ctx ast tail? nb-actual-args cn-num continuation-dropped?)

  ;; r11 is available because it's the ctx register
  (if tail?
      (let ((fs  (ctx-fs ctx))
            (ffs (ctx-ffs ctx))
            (nshift
              (if (> (- nb-actual-args (length args-regs)) 0)
                  (- nb-actual-args (length args-regs))
                  0)))

        ;; If the continuation is dropped, we need to shift one more element
        ;; and shift the elements one slot further
        (if continuation-dropped?
            (set! nshift (+ nshift 1)))

        (let ((sup (if (or cn-num continuation-dropped?) 0 1)))
          (if (not (= nshift
                      (- fs sup)))
              (let loop ((curr (- nshift 1)))
                (if (>= curr 0)
                    (begin
                      (x86-mov cgc (x86-r11) (x86-mem (* 8 curr) (x86-usp)))
                      (x86-mov cgc (x86-mem (* 8 (+ (- fs nshift sup) curr)) (x86-usp)) (x86-r11))
                      (loop (- curr 1))))))

          ;; Clean stacks
          (if (> ffs 0)
              (x86-add cgc (x86-rsp) (x86-imm-int (* 8 ffs)))) ;; TODO: NYI case if nfargs > number of fargs regs
          ;; TODO: merge two cases
          (if (not (= (- fs nshift sup) 0))
              (x86-add cgc (x86-usp) (x86-imm-int (* 8 (- fs nshift sup)))))))))

;;
;; Make lazy code from CALL EXPR
;;
(define (get-alt-idx types)

  (define (compute-type-dist t1 t2)
    (cond ((ctx-type-unk? t1)
             (if (ctx-type-unk? t2) 0 #f))
          ((ctx-type-cst? t1)
             (cond ;; t1 cst, t2 cst
                   ((ctx-type-cst? t2)
                      (if (equal? (ctx-type-cst t1) (ctx-type-cst t2))
                          0
                          #f))
                   ;;
                   ((ctx-type-teq? t1 t2) 1)
                   ;;
                   ((ctx-type-unk? t2) 2)
                   ;;
                   (else #f)))
          ((ctx-type-unk? t2) 1)    ;; t1 type, t2 unk
          ((ctx-type-cst? t2) #f)   ;; t1 type, t2 cst
          ((ctx-type-teq? t1 t2) 0) ;; t1 type, t2 type, teq
          (else #f)))               ;; t1 type, t2 type, !teq

  (define (compute-stack-dist types1 types2)
    (let loop ((types1 types1) (types2 types2)
               (dist 0))
      (cond ((null? types1) (if (null? types2) dist #f))
            ((null? types2) #f)
            (else
              (let ((d (compute-type-dist (car types1) (car types2))))
                (and d (loop (cdr types1) (cdr types2) (+ dist d))))))))


  (let loop ((entries (table->list global-cc-table))
             (r #f)
             (mindist #f))
    (if (null? entries)
        r
        (let* ((cn-num (caaar entries))
               (types  (cdaar entries))
               (dist (and #f ;; TODO: use cn-num to compute stack distance
                          (compute-stack-dist types types))))
          (if (and dist
                   (or (not mindist) (< dist mindist)))
              (loop (cdr entries) (car entries) dist)
              (loop (cdr entries) r mindist))))))

(define (get-ccidx-stack cn-num call-stack nb-args inlined-call?)

  (define idx #f)
  (define stack call-stack)
  (define force-generic? #f)
  (define need-merge? #f)

  (if inlined-call?
      (list idx stack need-merge?)

      (begin
        (if (and opt-entry-points
                 (or (not opt-call-max-len)
                     (<= nb-args opt-call-max-len)))
            (set! idx (cctable-get-idx (cons cn-num call-stack)))
            (set! force-generic? #t))

        (if (and opt-closest-cx-overflow
                 (not force-generic?)
                 (not idx))
            (let ((r (get-alt-idx call-stack)))
              (if r
                  (begin (set! need-merge? #t)
                         (set! stack (car r))
                         (set! idx   (cdr r))))))

        (list idx stack need-merge?))))

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

               (let* ((nb-args (length args))
                      (cn-num #f)
                      (prop-cont? opt-propagate-continuation)
                      (fn-loc/fn-num
                        (or fn-id-inf
                            (let ((type (ctx-get-type ctx (length args))))
                              (if (ctx-type-cst? type)
                                  (let ((loc (ctx-get-loc ctx (length args))))
                                    (cons (not loc) (ctx-type-cst type)))
                                  #f))))
                      (inlined-call?
                        (and opt-lazy-inlined-call
                             (let* ((fn-num    (and fn-loc/fn-num (cdr fn-loc/fn-num)))
                                    (obj       (and fn-num (asc-globalfn-entry-get fn-num)))
                                    (lazy-code (and obj (cadr obj))))
                               (and lazy-code (not (lazy-code-rest? lazy-code)))))))

               ;; Save used registers, generate and push continuation stub
               (let ((r (call-save/cont cgc ctx ast succ tail? (+ nb-args 1) #f prop-cont?)))
                 (set! cn-num (car r))
                 (set! ctx (cdr r)))

               (if #f;inlined-call?
                   (let* ((fn-num    (and fn-loc/fn-num (cdr fn-loc/fn-num)))
                          (obj       (and fn-num (asc-globalfn-entry-get fn-num)))
                          (lazy-code (and obj (cadr obj))))
                     ;;
                     (let* ((r (asc-fnnum-ctx-get fn-num))
                            (ctx (apply ctx-init-fn-inlined (cons cn-num (cons ctx (cons nb-args r))))))
                       ;; TODO patch table (?)
                       (x86-label cgc (asm-make-label #f (new-sym 'inlined_call_)))
                       (jump-to-version cgc lazy-code ctx)))

               (let* ((nb-args (length args))
                      (call-ctx (ctx-init-call ctx nb-args))
                      (call-stack (list-head (ctx-stack call-ctx) nb-args))
                      ;;
                      (r (get-ccidx-stack cn-num call-stack nb-args inlined-call?))
                      (cctable-idx (car   r))
                      (call-stack  (cadr  r))
                      (need-merge? (caddr r))
                      ;;
                      (continuation-dropped? #f)
                      ;;
                      (generic-entry? (and opt-entry-points (not inlined-call?) (not cctable-idx))))

                 ;; If the continuation is a cst and we need to call the generic e.p
                 ;; then we need to drop the cst continuation
                 (if (and (or generic-entry? (not prop-cont?))
                          cn-num)
                     (let ((table (asc-cnnum-table-get cn-num)))
                       (x86-mov cgc (x86-rax) (x86-imm-int (- (obj-encoding table) TAG_MEMOBJ)))
                       (x86-upush cgc (x86-rax))
                       (set! ctx (ctx-fs-inc ctx))
                       (set! continuation-dropped? #t)
                       (set! cn-num #f)))

                 (if need-merge?
                   (let loop ((nctx ctx) (stack-dest call-stack) (idx 0) (NB nb-args))
                     (if (= NB 0)
                         (set! ctx nctx)
                         (let ((type (list-ref (ctx-stack ctx) idx))
                               (type-dst (car stack-dest)))
                           (cond ((and (ctx-type-cst? type)
                                       (not (ctx-type-cst? type-dst)))
                                    (let* ((nctx (drop-cst-value cgc ast nctx idx))
                                           (nctx (if (ctx-type-flo? type)
                                                     (gen-drop-float cgc nctx ast idx (+ idx 1))
                                                     nctx)))
                                      (loop nctx (cdr stack-dest) (+ idx 1) (- NB 1))))
                                 ((and (ctx-type-flo? type)
                                       (not (ctx-type-flo? type-dst)))
                                    (let ((nctx (gen-drop-float cgc nctx ast idx (+ idx 1))))
                                      (loop nctx (cdr stack-dest) (+ idx 1) (- NB 1))))
                                 (else
                                (loop nctx (cdr stack-dest) (+ idx 1) (- NB 1))))))))

                 ;; pour chaque type de la pile du ctx qui fait partie du ctx (0 -> nb-args):
                    ;; si stack[i] est une cst et que la dest n'est pas cst
                        ;; on appelle drop cst
                    ;; sinon si stack[i] est un float et que la dest n'est pas un float
                        ;; on appelle drop float
                 ;; Move args to regs or stack following calling convention
                 (set! ctx (call-prep-args cgc ctx ast nb-args (and fn-loc/fn-num (car fn-loc/fn-num)) generic-entry? inlined-call? tail?))

                 ;; Shift args and closure for tail call
                 (mlet ((nfargs/ncstargs
                          (if (or (and (not opt-entry-points)
                                       (not inlined-call?))
                                  generic-entry?)
                              (list 0 0)
                              (let loop ((idx 0) (nf 0) (ncst 0))
                                (if (= idx nb-args)
                                    (list nf ncst)
                                    (let ((type (ctx-get-type ctx idx)))
                                      (cond ((and (const-versioned? type)
                                                  (not (ctx-type-id? type)))
                                               (loop (+ idx 1) nf (+ ncst 1)))
                                            ((ctx-type-flo? type)
                                               (loop (+ idx 1) (+ nf 1) ncst))
                                            (else
                                               (loop (+ idx 1) nf ncst)))))))))

                   (if (> nfargs (length regalloc-fregs))
                       (error "NYI c")) ;; Fl args that are on the pstack need to be shifted

                   (call-tail-shift cgc ctx ast tail? (- nb-args nfargs ncstargs) cn-num continuation-dropped?))

                 ;; Generate call sequence
                 (gen-call-sequence ast cgc call-stack cctable-idx nb-args (and fn-loc/fn-num (cdr fn-loc/fn-num)) inlined-call? cn-num))))))))

    ;; Gen and check types of args
    (make-lazy-code
      ast
      (lambda (cgc ctx)

        ;; Check if the identity of called function is available
        (set! fn-id-inf (call-get-eploc ctx (car ast)))

        ;; If the operator is an atom node, and it's a symbol
        ;; and the symbol is an identifier in ctx, and this identifier is a cst-id
        ;; Then fn-id-inf *must* be set for perfs
        (assert (not
                  (and (atom-node? (car ast))
                       (symbol? (atom-node-val (car ast)))
                       (let ((r (assoc (atom-node-val (car ast)) (ctx-env ctx))))
                         (and r
                              (identifier-cst (cdr r))
                              (not fn-id-inf)))))
                "Internal error")

        (if (and fn-id-inf (car fn-id-inf))
            (jump-to-version
              cgc
              (gen-ast-l (cdr ast) lazy-call)
              (ctx-push ctx (make-ctx-tcloc (cdr fn-id-inf)) #f (atom-node-val (car ast))))
            (jump-to-version
              cgc
              (check-types (list ATX_CLO) (list (car ast)) (gen-ast-l (cdr ast) lazy-call) ast)
              ctx))))))

(define (gen-continuation-rp cgc ast succ ctx apply? prop-cont?)

  ;; Add continuation flag to the lco
  (lazy-code-f-cont! succ)

  (if (not prop-cont?)
      (let* (;; Label for return address loading
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
                                                  succ
                                                  (ctx-push ctx (make-ctx-tunk) return-reg)))))
                                    gen-flag))))
      ;; Generate code
      (codegen-load-cont-rp cgc load-ret-label (list-ref stub-labels 0))))
  succ)

(define (gen-continuation-cr cgc ast succ ctx cn-num apply? prop-cont?)

  (define crtable #f)

  (define (gen-stub)
    (let* ((stub-labels
             (add-cont-callback
               cgc
               1
               (lambda (ret-addr selector type)

                     ;;
                     (let* ((generic? (not type))
                            (type (or type (make-ctx-tunk)))
                            (args (cdr ast))
                            (ctx
                              (if apply?
                                  (ctx-pop-n ctx 2) ;; Pop operator and args
                                  (ctx-pop-n ctx (+ (length args) 1)))) ;; Remove closure and args from virtual stack
                            (reg
                              (cond ((and (ctx-type-cst? type)
                                          (not (ctx-type-id? type)))
                                        #f)
                                    ((ctx-type-flo? type) return-freg)
                                    (else                 return-reg))))

                       (gen-version-continuation-cr
                         succ
                         (ctx-push ctx type reg)
                         type
                         crtable
                         generic?)))))
           ;; CRtable
           (stub-addr    (asm-label-pos (list-ref stub-labels 0)))
           (generic-addr (asm-label-pos (list-ref stub-labels 1))))
      (cons stub-addr generic-addr)))

  (define (gen-crtable)
    (let ((addrs (gen-stub)))
      (set! crtable (get-crtable ast ctx (car addrs) (cdr addrs)))
      crtable))

  ;; Add continuation flag to the lco
  (lazy-code-f-cont! succ)

  (asc-cnnum-ctx-add cn-num (ctx-pop-n ctx (+ (length (cdr ast)) 1)))

  (if (not prop-cont?)
      (begin
        (gen-crtable)
        (asc-cnnum-table-add cn-num crtable)
        (let ((crtable-loc (- (obj-encoding crtable) 1)))
          (codegen-load-cont-cr cgc crtable-loc)))
      (asc-cnnum-table-add cn-num (lambda () (gen-crtable))))

  succ)

;; Gen call sequence (call instructions)
;; fn-num is fn identifier or #f
(define (gen-call-sequence ast cgc call-stack cc-idx nb-args fn-num inlined-call? cn-num)

  (define obj (and fn-num (asc-globalfn-entry-get fn-num)))
  (define entry-obj (and obj (car obj)))
  (define lazy-code (and obj (cadr obj)))
  (define stub-addr (and obj (cddr obj)))
  ;; TODO: eploc -> entry-obj-loc
  (define eploc (and entry-obj (- (obj-encoding entry-obj) 1)))

  (define (get-cc-direct)
    (and lazy-code
         (let* ((stack call-stack)
                (version (table-ref (lazy-code-versions lazy-code)
                                    (append stack (list (make-ctx-tclo) (make-ctx-tret)))
                                    #f)))
           (if (and version
                    (not (lazy-code-rest? lazy-code)))
               (list 'ep (asm-label-pos (car version)))
               (let ((label (asm-make-label #f (new-sym 'stub_load_))))
                 (asc-entry-load-add entry-obj cc-idx label)
                 (list 'stub stub-addr label))))))

  (define (get-ep-direct)
    (and entry-obj
         (let ((ep (* 4 (vector-ref entry-obj 0))))
           (if (= ep stub-addr)
               (let ((label (asm-make-label #f (new-sym 'stub_load_))))
                 (asc-entry-load-add entry-obj -1 label)
                 (list 'stub stub-addr label))
               (list 'ep ep)))))

  (cond (inlined-call?
           (let* ((r (asc-fnnum-ctx-get fn-num))
                  (ctx (apply ctx-init-fn (cons cn-num (cons call-stack r)))))
             ;; TODO patch table (?)
             (x86-label cgc (asm-make-label #f (new-sym 'inlined_call_)))
             (jump-to-version cgc lazy-code ctx)))
        ((not opt-entry-points)
           (let ((direct (get-ep-direct)))
             (codegen-call-ep cgc nb-args eploc direct)))
        ((not cc-idx) ;; apply or cc-full
           (codegen-call-cc-gen cgc nb-args eploc))
        (else
           (let ((direct (get-cc-direct)))
             (codegen-call-cc-spe cgc cc-idx nb-args eploc direct)))))

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
                                              (codegen-num-ff cgc (ctx-fs ctx) (ctx-ffs ctx) op reg lleft #t lright #t lcst? rcst? #t)
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
               (rcst? (ctx-type-cst? rtype))
               (rloc  (if rcst? (ctx-type-cst rtype) (ctx-get-loc ctx 0)))
               ;; left info
               (ltype (ctx-get-type ctx 1))
               (lcst? (ctx-type-cst? ltype))
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
                  (let ((x86-op (codegen-cmp-ii cgc (ctx-fs ctx) (ctx-ffs ctx) op #f lloc rloc lcst? rcst? #t)))
                    ((lazy-code-generator succ) cgc (ctx-pop-n ctx 2) x86-op)))
                ;; In these cases, we need a free register
                ;; Then, get a free register, apply moves, and use it.
                (else
                  (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ nopnd-regalloc)))
                    (apply-moves cgc ctx moves)
                    (cond
                      (num-op?
                        (let ((overflow-label (get-stub-overflow-label cgc ctx reg lloc lcst? rloc rcst?)))
                          (codegen-num-ii cgc (ctx-fs ctx) (ctx-ffs ctx) op reg lloc rloc lcst? rcst? #t overflow-label))
                        (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))
                      (else
                          (codegen-cmp-ii cgc (ctx-fs ctx) (ctx-ffs ctx) op reg lloc rloc lcst? rcst? #f)
                          (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))))))))))

  ;;
  (define (get-op-ff leftint? rightint?)
    (make-lazy-code
      #f
      (lambda (cgc ctx)
        (let* ((type  (if num-op? (make-ctx-tflo) (make-ctx-tboo)))
               ;; right info
               (rtype (ctx-get-type ctx 0))
               (rcst? (ctx-type-cst? rtype))
               (rloc  (if rcst? (ctx-type-cst rtype) (ctx-get-loc ctx 0)))
               ;; left info
               (ltype (ctx-get-type ctx 1))
               (lcst? (ctx-type-cst? ltype))
               (lloc  (if lcst? (ctx-type-cst ltype) (ctx-get-loc ctx 1))))

          (cond ((and (not inlined-if-cond?) lcst? rcst?)
                  (let* ((r (eval (list op lloc rloc)))
                         (type (literal->ctx-type r))
                         (ctx (ctx-push (ctx-pop-n ctx 2) type #f)))
                    (jump-to-version cgc succ ctx)))
                ((and lcst? rcst?)
                  (let* ((r (eval (list op lloc rloc)))
                         (lco (if r (lazy-code-lco-true succ)
                                    (lazy-code-lco-false succ))))
                    (jump-to-version cgc lco (ctx-pop-n ctx 2))))
                (inlined-if-cond?
                  (let ((x86-op (codegen-cmp-ff cgc (ctx-fs ctx) (ctx-ffs ctx) op #f lloc leftint? rloc rightint? lcst? rcst? #t)))
                    ((lazy-code-generator succ) cgc (ctx-pop-n ctx 2) x86-op)))
                ;; In these cases, we need a free register
                ;; Then, get a free register, apply moves, and use it.
                (num-op?
                  (mlet ((moves/reg/ctx (ctx-get-free-freg ast ctx succ 2)))
                    (apply-moves cgc ctx moves)
                    (codegen-num-ff cgc (ctx-fs ctx) (ctx-ffs ctx) op reg lloc leftint? rloc rightint? lcst? rcst? #t)
                    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg))))
                (else
                  (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 2)))
                    (apply-moves cgc ctx moves)
                    (codegen-cmp-ff cgc (ctx-fs ctx) (ctx-ffs ctx) op reg lloc leftint? rloc rightint? lcst? rcst? #f)
                    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) type reg)))))))))

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
        (mlet ((moves/reg/ctx (ctx-get-free-reg ast ctx succ 1)))
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
              (cond ((or (not vartype) (ctx-type-unk? vartype))
                       (jump-to-version cgc (gen-ast (cadr ast) check) ctx))
                    ((ctx-type-is-a? vartype type)
                       (jump-to-version cgc (lazy-code-lco-true succ) ctx))
                    (else
                       (jump-to-version cgc (lazy-code-lco-false succ) ctx)))))

          (gen-ast (cadr ast) check)))))

;;-----------------------------------------------------------------------------

;; AST RELATED FUNCTIONS

;;-----------------------------------------------------------------------------

;; Return the total number of crtables
(define (cxtables-total all-tables)
  (lambda ()
    (foldr (lambda (el total)
             (let ((lst (table->list (cdr el))))
               (+ (length lst) total)))
           0
           (table->list all-tables))))

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

;; Create a new cr table with 'init' as stub value
(define (make-cc)
  (alloc-perm-vector-i64 (+ 1 global-cc-table-maxsize) 0))

;; This is the key used in hash table to find the cc-table for this closure.
;; The key represents captured values used to specialize tables
(define (get-cc-key ctx fvars-imm fvars-late)
  (foldr (lambda (n r)
           (if (member (car n) fvars-imm)
               (cons (ctx-identifier-type ctx (cdr n)) r)
               (cons #f r)))
         '()
         (ctx-env ctx)))

;; all-cctables associates an ast to a table ('equal?' table)
;; to get a cctable, we first use the eq? table to get cctables associated to this ast
;; then we use the equal? table to get cctable associated to the captured values
(define all-cctables (make-table test: eq?))
(define cctables-total (cxtables-total all-cctables))

;; Return cctable
;; Return the existing table if already created or create one, add entry, and return it
(define (get-cctable ast ctx fvars-imm fvars-late)
  ;; Use 'eq?' table to get prleiminary result
  (let ((cctables (table-ref all-cctables ast #f)))
    (if (not cctables)
        (let ((tables (make-table test: equal?)))
          (table-set! all-cctables ast tables)
          (set! cctables tables)))
    ;; Then use 'equal?' table to get cctable
    (let* ((key (get-cc-key ctx fvars-imm fvars-late))
           (cctable (table-ref cctables key #f)))

      (if cctable
          (cons #f cctable)
          (let ((cctable (make-cc))
                (fn-num (new-fn-num)))
            (table-set! cctables key (cons cctable fn-num))
            (cons #t (cons cctable fn-num)))))))

;; Fill cctable with stub and generic addresses
(define (cctable-fill cctable stub-addr generic-addr)
  ;; Fill cctable
  (##u64vector-fill! cctable stub-addr)
  (##u64vector-set!  cctable 0 generic-addr))

;-----------------------------------------------------------------------------
;; CR TABLES

;; Create a new cr table with 'init' as stub value
(define (make-cr stub-addr generic-addr)
  (let ((v (alloc-perm-vector-i64 (+ 1 global-cr-table-maxsize) stub-addr)))
    (##u64vector-set! v 0 generic-addr)
    v))

;; all-crtables associates an ast to a table ('equal?' table)
;; to get a crtable, we first use the eq? table to get crtables associated to this ast
;; then we use the equal? table to get crtable associated to the free values
(define all-crtables (make-table test: eq?))
(define crtables-total (cxtables-total all-crtables))

;; Return crtable from crtable-key
;; Return the existing table if already created or create one, add entry, and return it
(define (get-crtable ast ctx stub-addr generic-addr)
  ;; Use 'eq?' table to get prleiminary result
  (let ((crtables (table-ref all-crtables ast #f)))
    (if (not crtables)
        (let ((tables (make-table test: equal?)))
          (table-set! all-crtables ast tables)
          (set! crtables tables)))
    ;; Then use 'equal?' table to get crtable
    (let* ((key
            (list (ctx-slot-loc ctx)
                  (ctx-fs ctx)
                  (ctx-ffs ctx)
                  (ctx-stack ctx)
                  (ctx-env ctx)))
           (crtable (table-ref crtables key #f)))
      (or crtable
          (let ((crtable (make-cr stub-addr generic-addr)))
            (table-set! crtables key crtable)
            crtable)))))

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
                              (closure-opnd (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) closure-loc))
                              ;; Get free var offset
                              (fvar-pos (cdr loc))
                              (fvar-offset (+ 16 (* 8 fvar-pos)))) ;; 16:header,entrypoint -1: pos starts from 1 and not 0
                         (if (ctx-loc-is-memory? closure-loc)
                             (begin (x86-mov cgc (x86-rax) closure-opnd)
                                    (set! closure-opnd (x86-rax))))
                         (x86-mov cgc (x86-rax) (x86-mem (- fvar-offset TAG_MEMOBJ) closure-opnd))
                         (x86-rax)))
                     ;;
                     ((or (ctx-loc-is-memory? loc)
                          (ctx-loc-is-fmemory? loc))
                       (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) loc))
                       (x86-rax))
                     ;;
                     ((ctx-loc-is-fregister? loc)
                       (x86-movd/movq cgc (x86-rax) (codegen-freg-to-x86reg loc))
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
(define (drop-cst-value cgc ast ctx ctx-idx)

  (define (alloc-cst-flo reg cst)
    (let ((opnd (codegen-freg-to-x86reg reg)))
      (x86-mov cgc (x86-rax) (x86-imm-int (get-ieee754-imm64 cst)))
      (x86-movd/movq cgc opnd (x86-rax))))

  (define (alloc-cst-clo reg cst)
    (let ((entry-obj (car (asc-globalfn-entry-get cst))))
      (gen-closure cgc reg #f entry-obj '())))

  (define (alloc-cst reg cst)
    (let ((opnd (codegen-reg-to-x86reg reg)))
      (x86-mov cgc opnd (x86-imm-int (obj-encoding cst)))))

  (let* ((type (ctx-get-type ctx ctx-idx))
         (loc  (ctx-get-loc  ctx ctx-idx))
         (cst? (ctx-type-cst? type)))
    (if (and cst?
             (not loc))
        (mlet ((cst (ctx-type-cst type))
               (moves/reg/ctx
                 (if (ctx-type-flo? type)
                     (ctx-get-free-freg ast ctx #f 0)
                     (ctx-get-free-reg ast ctx #f 0))))
          (apply-moves cgc ctx moves)
          ;; Alloc cst
          (cond ((ctx-type-flo? type) (alloc-cst-flo reg cst))
                ((ctx-type-clo? type) (alloc-cst-clo reg cst))
                (else                 (alloc-cst reg cst)))
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
