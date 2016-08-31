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

(define expand #f)
;;-----------------------------------------------------------------------------
;; Macros

(define-macro (assert c err)
  `(if (not ,c)
       (begin
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

;; Primitives: name, nb args min, nb args max, args types, cst positions to check
(define primitives `(
                     (car                 1  1  ,(prim-types 1 ATX_PAI)                     ())
                     (cdr                 1  1  ,(prim-types 1 ATX_PAI)                     ())
                     (eq?                 2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (char=?              2  2  ,(prim-types 2 ATX_CHA ATX_CHA)        (0 1))
                     (zero?               1  1  ,(prim-types 1 ATX_INT)                     ())
                     (not                 1  1  ,(prim-types 1 ATX_ALL)                     ()) ;; + efficace cst TODO
                     (set-car!            2  2  ,(prim-types 2 ATX_PAI ATX_ALL)            (1))
                     (set-cdr!            2  2  ,(prim-types 2 ATX_PAI ATX_ALL)            (1))
                     (cons                2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (vector-length       1  1  ,(prim-types 1 ATX_VEC)                    ())
                     (vector-ref          2  2  ,(prim-types 2 ATX_VEC ATX_INT)           (1))
                     (char->integer       1  1  ,(prim-types 1 ATX_CHA)                   (0))
                     (integer->char       1  1  ,(prim-types 1 ATX_INT)                    (0))
                     (string-ref          2  2  ,(prim-types 2 ATX_STR ATX_INT)            (1))
                     (string-set!         3  3  ,(prim-types 3 ATX_STR ATX_INT ATX_CHA) (1 2))
                     (vector-set!         3  3  ,(prim-types 3 ATX_VEC ATX_INT ATX_ALL)    ()) ;; + efficace cst TODO
                     (string-length       1  1  ,(prim-types 1 ATX_STR)                     ())
                     (exit                0  0  ,(prim-types 0 )                            ())
                     (make-vector         1  2  ,(prim-types 1 ATX_INT 2 ATX_INT ATX_ALL)   ())
                     (make-string         1  2  ,(prim-types 1 ATX_INT 2 ATX_INT ATX_CHA)  ())
                     (eof-object?         1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (symbol->string      1  1  ,(prim-types 1 ATX_SYM)                     ())
                     (current-output-port 0  0  ,(prim-types 0 )                            ())
                     (current-input-port  0  0  ,(prim-types 0 )                            ())
                     ;; These primitives are inlined during expansion but still here to check args and/or build lambda
                     (number?             1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (real?               1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (eqv?                2  2  ,(prim-types 2 ATX_ALL ATX_ALL)             ())
                     ;;
                     (##fx+               2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fx-               2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fx*               2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fx+?              2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fx-?              2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fx*?              2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fl+               2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fl-               2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fl*               2  2  ,(prim-types 2 ATX_ALL ATX_ALL)          (0 1))
                     (##fixnum->flonum    1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (##mem-allocated?    1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (##subtyped?         1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (##box               1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (##unbox             1  1  ,(prim-types 1 ATX_ALL)                     ())
                     (##set-box!          2  2  ,(prim-types 2 ATX_ALL ATX_ALL)             ())))

(define (assert-p-nbargs prim ast)
  (let ((infos (cdr (assoc prim primitives))))
    (assert (or (not (car infos)) ;; nb args and types are not fixed
                (and (>= (length (cdr ast))
                         (cadr (assoc prim primitives)))
                     (<= (length (cdr ast))
                         (caddr (assoc prim primitives)))))
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

  (cond ;; Pair
        ((pair? ast)
         (let ((op (car ast)))
           (cond ;; Atom
                 ((atom-node? ast)
                    (mlc-atom ast succ))
                ;; Special
                ((atom-node? op)
                   (let ((val (atom-node-val op)))
                     (cond ;; Gambit call
                           ((gambit-call? op)
                              (mlc-gambit-call ast succ #f))
                           ;; Special
                           ((member val '(##subtype breakpoint $$sys-clock-gettime-ns)) (mlc-special val ast succ))
                           ;; TODO
                           ((and (eq? val 'write-char) (= (length ast) 2))
                             (let* ((cop-node (atom-node-make 'current-output-port))
                                    (ast (append ast (list (list cop-node)))))
                               (gen-ast ast succ)))
                           ;; Inlined primitive
                           ((assoc val primitives) (mlc-primitive val ast succ))
                           ;; List
                           ((eq? val 'list)
                              (mlc-list-p ast succ))
                           ;; Vector
                           ((eq? val 'vector) (mlc-vector-p ast succ))
                           ;; Apply
                           ((eq? val '$apply) (mlc-apply ast succ))
                           ;; Type predicate
                           ((type-predicate? val) (mlc-test val ast succ))
                           ;; Operator num
                           ((member val '(quotient modulo remainder)) (mlc-op-bin ast succ val)) ;; binary operator
                           ((member val '(+ - * < > <= >= = /))       (mlc-op-n ast succ val))   ;; nary operator
                           ;; Operator num
                           ((member val '(FLOAT+ FLOAT- FLOAT* FLOAT/ FLOAT< FLOAT> FLOAT<= FLOAT>= FLOAT=))
                              (let* ((generic-op (list->symbol (list-tail (symbol->list val) 5)))
                                     (opnode (atom-node-make generic-op)))
                                (gen-ast (cons opnode (cdr ast))
                                         succ)))
                           ;; Call
                           (else (mlc-call ast succ)))))
                 ; ;; TODO
                 ;; Quote
                 ((eq? 'quote (car ast)) (mlc-quote (cadr ast) ast succ))
                 ;; Set!
                 ((eq? 'set! (car ast)) (mlc-set! ast succ))
                 ;; Lambda
                 ((eq? op 'lambda) (mlc-lambda-ast ast succ #f))
                 ;; Begin
                 ((eq? op 'begin) (mlc-begin ast succ))
                 ;; Binding
                 ((eq? op 'let) (mlc-let ast succ)) ;; Also handles let* (let* is a macro)
                 ((eq? op 'letrec) (mlc-letrec ast succ))
                 ;; If
                 ((eq? op 'if) (mlc-if ast succ))
                 ;; Define
                 ((eq? op 'define) (mlc-define ast succ))
                 ;; Call expr
                 (else (mlc-call ast succ)))))
        ;; *unknown*
        (else
         (error "unknown ast" ast))))

;;-----------------------------------------------------------------------------
;; ATOM

(define (mlc-atom ast succ)
  (let ((val (atom-node-val ast)))
    (cond ((string? val) (mlc-string val ast succ))
          ((symbol? val) (mlc-identifier val ast succ))
          ((compiler-flonum? val) (mlc-flonum val ast succ))
          ((literal? val) (mlc-literal val ast succ))
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
  (if (and (number? lit)
           (or (>= lit (expt 2 61))
               (<  lit (* -1  (expt 2 60)))))
    (mlc-flonum lit ast succ)
    (make-lazy-code
      (lambda (cgc ctx)
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
          (apply-moves cgc ctx moves)
          (codegen-literal cgc lit reg)
          (jump-to-version cgc
                           succ
                           (ctx-push ctx
                                     (literal->ctx-type lit)
                                     reg)))))))

;;
;; Make lazy code from flonum literal
;;
(define (mlc-flonum flo ast succ)

  (make-lazy-code
      (lambda (cgc ctx)
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0))
               (immediate
                (if (< flo 0)
                    (let* ((ieee-rep (ieee754 (abs flo) 'double))
                           (64-mod   (bitwise-not (- ieee-rep 1)))
                           (64-modl  (bitwise-and (- (expt 2 63) 1) 64-mod)))
                      (* -1 64-modl))
                    (ieee754 flo 'double))))
          (apply-moves cgc ctx moves)
          (codegen-flonum cgc immediate reg)
          (jump-to-version cgc succ (ctx-push ctx (make-ctx-tflo) reg))))))

;;
;; Make lazy code from vector literal
;;
(define (mlc-vector ast succ)

  (define len (vector-length ast))

  (define (gen-set cgc ctx lidx)
    (let* ((lval (ctx-get-loc ctx lidx))
           (opval (codegen-loc-to-x86opnd (ctx-fs ctx) lval)))
      (if (ctx-loc-is-memory? lval)
          (begin (x86-mov cgc (x86-rax) opval)
                 (set! opval (x86-rax))))
      (x86-mov cgc (x86-mem (+ (* -8 len) (* 8 lidx)) alloc-ptr) opval)))

  (define lazy-vector
    (make-lazy-code
      (lambda (cgc ctx)
        (let ((len (vector-length ast)))
          (gen-allocation-imm cgc STAG_VECTOR (* 8 len))
          (let loop ((pos 0))
            (if (= pos len)
              (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ (vector-length ast))))
                (apply-moves cgc ctx moves)
                (x86-lea cgc (codegen-reg-to-x86reg reg) (x86-mem (+ (* -8 (+ len 1)) TAG_MEMOBJ) alloc-ptr))
                (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx len) (make-ctx-tvec) reg)))
              (begin
                (gen-set cgc ctx pos)
                (loop (+ pos 1)))))))))

  (gen-ast-l (reverse (vector->list ast)) lazy-vector))

;;
;; Make lazy code from string literal
;;
(define (mlc-string str ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
        (apply-moves cgc ctx moves)
        (codegen-string cgc str reg)
        (jump-to-version cgc succ (ctx-push ctx (make-ctx-tstr) reg))))))

;;
;; Make lazy code from QUOTE
;;

(define (mlc-quote val ast succ)

  (cond ((null? val)
          (mlc-literal '() ast succ))
        ((or (pair? val) (symbol? val) (vector? val))
          (make-lazy-code
            (lambda (cgc ctx)
              (define type (literal->ctx-type val))
              (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
                (apply-moves cgc ctx moves)
                (let ((dest (codegen-reg-to-x86reg reg)))
                  (if (permanent-object? val)
                      (x86-mov cgc dest (x86-imm-int (obj-encoding val)))
                      (x86-mov cgc dest (x86-mem (cst-get val)))))
                (jump-to-version cgc succ (ctx-push ctx type reg))))))
        (else (pp val) (error "Internal error mlc-quote"))))

;;-----------------------------------------------------------------------------
;; VARIABLES GET

;;
;; Make lazy code from SYMBOL
;;
;; TODO: réécrire: attention la recherche d'id met à jour le ctx
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
    (inlined-cond? (lambda () (table-ref gids id #f))))

  (make-lazy-code
    (lambda (cgc ctx)

      (let ((local  (assoc sym (ctx-env ctx)))
            (global (table-ref globals sym #f)))

        ;;
        (cond ;; Identifier local or global and inlined condition
              ((or (and local  (lcl-inlined-cond? ctx (cdr local)))
                   (and global (gbl-inlined-cond? sym)))
                (jump-to-version cgc (lazy-code-lco-true succ) ctx))
              ;; Identifier is a local const function
              ((and local
                    (member 'cst (identifier-flags (cdr local))))
                 (gen-closure-from-cst cgc ctx local succ))
              ;; Identifier is a free variable
              ((and local (eq? (identifier-kind (cdr local)) 'free))
                (gen-get-freevar cgc ctx local succ #f))
              ;; Identifier is a local variable
              (local
                (gen-get-localvar cgc ctx local succ #f))
              ;; Identifier is a global variable
              (global
                (gen-get-globalvar cgc ctx global succ))
              ;; Primitive
              ((assoc sym primitives) =>
                 (lambda (r)
                   (let ((ast
                           ;; primitive with fixed number of args
                           (let* ((args (build-list (cadr r) (lambda (x) (string->symbol (string-append "arg" (number->string x))))))
                                  (args-nodes (map atom-node-make args)))
                             `(lambda ,args (,ast ,@args-nodes)))))
                     (jump-to-version cgc (gen-ast ast succ) ctx))))
              ;; Vector
              ((eq? sym 'vector)
                 (let* ((node-lv (atom-node-make 'list->vector))
                        (node-l  (atom-node-make 'l))
                        (lco (gen-ast `(lambda l (,node-lv ,node-l)) succ)))
                   (jump-to-version cgc lco ctx)))
              ;; List
              ((eq? sym 'list)
                 (let ((node (atom-node-make 'n)))
                   (jump-to-version
                     cgc
                     (gen-ast `(lambda n ,node) succ)
                     ctx)))
              (else (gen-error cgc (ERR_UNKNOWN_VAR sym))))))))

(define (gen-closure-from-cst cgc ctx local succ)
  (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0))
         (stype (identifier-stype (cdr local)))
         (fn-num (ctx-tclo-fn-num stype))
         (entry-obj (asc-globalfn-entry-get fn-num)))
    (apply-moves cgc ctx moves)
    (gen-closure cgc reg #f #f entry-obj '())
    (jump-to-version cgc succ (ctx-push ctx (make-ctx-tclo) reg (car local)))))

;; TODO: merge with gen-get-localvar, it's now the same code!
(define (gen-get-freevar cgc ctx local succ for-set?)

  (let ((loc (ctx-identifier-loc ctx (cdr local)))
        (type (ctx-identifier-type ctx (cdr local))))

    (cond ((and for-set?
                (or (ctx-loc-is-register? loc)
                    (ctx-loc-is-memory? loc)))
             (codegen-load-loc cgc (ctx-fs ctx) loc))
          ((or (ctx-loc-is-register? loc)
               (ctx-loc-is-memory? loc))
             (mlet ((moves/reg/nctx (ctx-get-free-reg ctx succ 0)))
               (apply-moves cgc nctx moves)
               (apply-moves cgc nctx (list (cons loc reg)))
               (jump-to-version cgc succ (ctx-push nctx type reg (car local)))))
          ((ctx-loc-is-freemem? loc)
             (if for-set?
                 (let* ((fs (ctx-fs ctx))
                        (cloc (ctx-get-loc ctx (- (length (ctx-stack ctx)) 2)))
                        (copnd (codegen-loc-to-x86opnd fs cloc))
                        (coffset (- (* 8 (+ (cdr loc) 2)) TAG_MEMOBJ)))
                   (if (x86-reg? copnd)
                       (x86-mov cgc (x86-rax) (x86-mem coffset copnd))
                       (begin
                         (x86-mov cgc (x86-rax) copnd)
                         (x86-mov cgc (x86-rax) (x86-mem coffset (x86-rax))))))
                 (mlet ((moves/reg/nctx (ctx-get-free-reg ctx succ 0)))
                   (apply-moves cgc nctx moves)
                   (let* ((fs (ctx-fs nctx))
                          (cloc (ctx-get-loc ctx (- (length (ctx-stack ctx)) 2)))
                          (copnd (codegen-loc-to-x86opnd fs cloc))
                          (coffset (- (* 8 (+ (cdr loc) 2)) TAG_MEMOBJ))
                          (dest (codegen-reg-to-x86reg reg)))
                     (if (x86-reg? copnd)
                         (x86-mov cgc dest (x86-mem coffset copnd))
                         (begin
                           (x86-mov cgc (x86-rax) copnd) ;; Get closure
                           (x86-mov cgc dest (x86-mem coffset (x86-rax))))))
                   (jump-to-version cgc succ (ctx-push nctx type reg (car local)))))))))

;; TODO: + utiliser un appel récursif comme pour gen-get-freevar (??)
;; TODO coment: si mobject? est vrai, c'est qu'on veut le mobject dans le tmp reg (rax)
(define (gen-get-localvar cgc ctx local succ for-set?)

  (let ((loc (ctx-identifier-loc ctx (cdr local)))
        (type (ctx-identifier-type ctx (cdr local))))

    (if for-set?
        (codegen-load-loc cgc (ctx-fs ctx) loc)
        ;;
        (mlet ((moves/reg/nctx (ctx-get-free-reg ctx succ 0)))
          (apply-moves cgc nctx moves)
          (apply-moves cgc nctx (list (cons loc reg)))
          (jump-to-version cgc succ (ctx-push nctx type reg (car local)))))))

(define (gen-get-globalvar cgc ctx global succ)

  (mlet (;; Get variable type if known
         (r (table-ref gids (car global) #f))
         (type (or r (make-ctx-tunk)))
         ;; Get free register (dest)
         (moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
    (apply-moves cgc ctx moves)
    ;; Generate code to get global var from memory
    (codegen-get-global cgc (cdr global) reg)
    ;; Jump with updated ctx
    (jump-to-version cgc succ (ctx-push ctx type reg))))

;;-----------------------------------------------------------------------------
;; VARIABLES SET

;;
;; Make lazy code from SET!
;;
(define (mlc-set! ast succ)

  (let* ((id (cadr ast))
         (lazy-set!
           (make-lazy-code
             (lambda (cgc ctx)
               (let ((gres (table-ref globals id #f)))
                 (if gres
                     (gen-set-globalvar cgc ctx gres succ)
                     (let ((lres (assoc id (ctx-env ctx))))
                       (if lres
                           (if (eq? (identifier-kind (cdr lres)) 'free)
                               (gen-set-freevar cgc ctx lres succ)
                               (gen-set-localvar cgc ctx lres succ))
                           (error (ERR_UNKNOWN_VAR id))))))))))

    (gen-ast (caddr ast) lazy-set!)))

(define (get-non-global-setter get-function)
  (lambda (cgc ctx local succ)

    ;; Get mobject in tmp register
    (get-function cgc ctx local #f #t)

    (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 1))
           (lval (ctx-get-loc ctx 0))
           (type (ctx-get-type ctx 0)))
      (apply-moves cgc ctx moves)
      (codegen-set-non-global cgc reg lval (ctx-fs ctx))
      (let ((ctx (ctx-push (ctx-pop ctx) (make-ctx-tvoi) reg)))
        (jump-to-version cgc succ (ctx-set-type ctx local type))))))

(define gen-set-localvar (get-non-global-setter gen-get-localvar))
(define gen-set-freevar  (get-non-global-setter gen-get-freevar))

(define (gen-set-globalvar cgc ctx global succ)
  (mlet ((pos (cdr global))
         (moves/reg/ctx (ctx-get-free-reg ctx succ 1))
         (lval (ctx-get-loc ctx 0)))
    (apply-moves cgc ctx moves)
    (codegen-set-global cgc reg pos lval (ctx-fs ctx))
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tvoi) reg))))

;;-----------------------------------------------------------------------------
;; INTERNAL FORMS

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)

  (let* ((identifier (cadr ast))
         (lazy-bind (make-lazy-code
                      (lambda (cgc ctx)
                        (mlet ((res (table-ref globals identifier)) ;; Lookup in globals
                               (pos (cdr res))                  ;; Get global pos
                               ;;
                               (moves/reg/ctx (ctx-get-free-reg ctx succ 1))
                               (lvalue (ctx-get-loc ctx 0)))
                          (apply-moves cgc ctx moves)
                          (codegen-define-bind cgc (ctx-fs ctx) pos reg lvalue)
                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tvoi) reg))))))
         (lazy-val
           (if (ctx-tclo? (table-ref gids (cadr ast) #f))
               (mlc-lambda-ast (caddr ast) lazy-bind (cadr ast))
               (gen-ast (caddr ast) lazy-bind))))

    (table-set! globals identifier (cons identifier nb-globals))
    (put-i64 (+ globals-addr (* 8 nb-globals)) ENCODING_VOID)
    (set! nb-globals (+ nb-globals 1))
    lazy-val))

;;
;; Make lazy code from LAMBDA
;;

;;
;; Create and return a generic prologue lco
(define (get-lazy-generic-prologue ast succ rest-param nb-formal)
  (make-lazy-code-entry
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
    (lambda (cgc ctx)
      (let* ((nb-actual (- (length (ctx-stack ctx)) 2))
             (nb-formal (ctx-nb-args ctx)))
        (cond ;; rest AND actual == formal
              ((and rest-param (= nb-actual (- nb-formal 1))) ;; -1 rest
               (set! ctx (ctx-stack-push ctx (make-ctx-tnul)))
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
                      (nctx (ctx-stack-pop-n ctx (- nb-extra 1)))
                      (nctx (ctx-set-type nctx 0 (make-ctx-tpai))))
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
  (make-lazy-code-ret ;; Lazy-code with 'ret flag
    (lambda (cgc ctx)
      (let* ((clean-nb (ctx-fs ctx))
             ;; Retval loc
             (lres  (ctx-get-loc ctx 0))
             (opres (codegen-loc-to-x86opnd (ctx-fs ctx) lres))
             (opret (codegen-reg-to-x86reg return-reg))
             ;; Retaddr loc
             (laddr (ctx-get-loc ctx (- (length (ctx-stack ctx)) 1)))
             (opaddr (codegen-loc-to-x86opnd (ctx-fs ctx) laddr)))

        ;; Move retval to retreg
        (if (not (eq? opret opres))
            (x86-mov cgc opret opres))

        ;; Get return address or cctable in rdx
        (if (not (eq? opaddr (x86-rdx)))
            (x86-mov cgc (x86-rdx) opaddr))

        ;; Clean stack
        (if (> clean-nb 0)
            (x86-add cgc (x86-usp) (x86-imm-int (* 8 clean-nb 1))))

        ;; Gen return
        (if opt-return-points
            (let* ((ret-type (car (ctx-stack ctx)))
                   (crtable-offset (ctx-type->cridx ret-type)))
              (codegen-return-cr cgc crtable-offset))
              (codegen-return-rp cgc))))))

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
               (fn-generator closure lazy-prologue-gen stack #f))
            ;; CASE 2 - Use multiple entry points AND use max-versions limit AND this limit is reached
            ;;          OR use generic entry point
            ((or (= selector 1)
                 (and (= selector 0)
                      opt-max-versions
                      (>= (lazy-code-nb-versions lazy-prologue) opt-max-versions)))
               (fn-generator closure lazy-prologue-gen stack #t))
            ;; CASE 3 - Use multiple entry points AND limit is not reached or there is no limit
            (else
               (fn-generator closure lazy-prologue stack #f))))))

(define (get-entry-obj ast ctx fn-num fvars-imm fvars-late all-params global-opt)

  ;; Generator used to generate function code waiting for runtime data
  ;; First create function entry ctx
  ;; Then generate function prologue code
  (define (fn-generator closure prologue stack generic?)
    (let ((ctx (ctx-init-fn stack ctx all-params (append fvars-imm fvars-late) global-opt fvars-late)))
      (gen-version-fn ast closure entry-obj prologue ctx stack generic? global-opt)))
  ;;
  (define stub-labels  (create-fn-stub ast fn-num fn-generator))
  (define stub-addr    (asm-label-pos (list-ref stub-labels 0)))
  (define generic-addr (asm-label-pos (list-ref stub-labels 1)))

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
          (begin
            ;; Add cctable->stub-addrs assoc
            (asc-entry-stub-add cctable generic-addr stub-addr)
            (cctable-fill cctable stub-addr generic-addr)))
      cctable))
  ;; In the case of -ep, entry object is the still vector of size 1 that contain the single entry point
  (define (get-entry-obj-ep)
    (let ((entryvec (get-entry-points-loc ast stub-addr)))
      (asc-entry-stub-add entryvec generic-addr stub-addr)
      entryvec))

  (define entry-obj
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
(define (init-entry ast ctx fvars-imm fvars-late global-opt)

  ;; Flatten list of param (include rest param)
  (define all-params (flatten (cadr ast)))

  (letrec (;; Closure unique number
           (fn-num (new-fn-num))
           (entry-obj (get-entry-obj ast ctx fn-num fvars-imm fvars-late all-params global-opt))
           (entry-obj-loc (- (obj-encoding entry-obj) 1)))

      ;; Add compile time identity if known
      (if global-opt
          (ctime-entries-set global-opt fn-num))

      ;; Add association fn-num -> entry point
      (asc-globalfn-entry-add fn-num entry-obj)

      entry-obj))

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
            (let ((identifier (cdr (assoc id (ctx-env ctx)))))
              (if (member 'cst (identifier-flags identifier))
                  (loop (cdr ids) imm (cons id cst))
                  (loop (cdr ids) (cons id imm) cst))))))))

(define (mlc-lambda-ast ast succ global-opt)

  (make-lazy-code
    (lambda (cgc ctx)

      (define all-params (flatten (cadr ast)))

      (define fvars-imm
              (free-vars
                (caddr ast)
                all-params
                (map car (ctx-env ctx))))

      (define fvars-ncst
        (keep (lambda (el)
                (let ((identifier (cdr (assoc el (ctx-env ctx)))))
                  (not (member 'cst (identifier-flags identifier)))))
              fvars-imm))

      (let ((entry-obj (init-entry ast ctx fvars-imm '() global-opt)))

        ;; Gen code to create closure
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
          (apply-moves cgc ctx moves)
          (gen-closure cgc reg ctx ast entry-obj fvars-ncst)
          ;;
          (jump-to-version cgc succ (ctx-push ctx (make-ctx-tclo) reg)))))))

;;
(define (gen-closure cgc reg ctx ast entry-obj fvars-ncst)

  (define entry-obj-loc (- (obj-encoding entry-obj) 1))

  ;; If 'stats' option, then inc closures slot
  (if opt-stats
    (gen-inc-slot cgc 'closures))

  ;; TODO (refactoring): inline here
  (gen-local-closure cgc reg ctx entry-obj-loc fvars-ncst))

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
;; Bindings (let, letrec, let*)

;;
;; Make lazy code from LET
;;
(define (mlc-let ast succ)

  (define cst '())
  (define nor '())

  (define (build-id-idx ids l)
    (if (null? ids)
        '()
        (cons (cons (car ids) l)
              (build-id-idx (cdr ids) (- l 1)))))

  (define (reset-sets!)
    (set! cst '())
    (set! nor '()))

  (define (compute-sets! ctx)
    (let loop ((bindings (cadr ast)))
      (if (not (null? bindings))
          (let* ((binding (car bindings))
                 (id  (car binding))
                 (val (cadr binding)))
            (if (and (pair? val)
                     (eq? (car val) 'lambda))
                (let ((finfo (get-free-infos val '() ctx)))
                  (if (= (length (cadr finfo)) 0)
                      (let ((c (init-entry-cst val (car finfo) ctx)))
                        (set! cst (cons (cons id c) cst)))
                      (set! nor (cons binding nor))))
                (set! nor (cons binding nor)))
            (loop (cdr bindings))))))


  (make-lazy-code
    (lambda (cgc ctx)
      (reset-sets!)
      (compute-sets! ctx)
      (let* ((lazy-let-out  (get-lazy-lets-out ast (map car (cadr ast)) (length cst) succ))
             (lazy-body (gen-ast (caddr ast) lazy-let-out))
             (lazy-bind
               (make-lazy-code
                 (lambda (cgc ctx)
                   (let* ((id-idx (build-id-idx (map car nor)
                                                (- (length nor) 1)))
                          (ctx (ctx-bind-consts ctx cst))
                          (ctx (ctx-bind-locals ctx id-idx)))
                     (jump-to-version cgc lazy-body ctx)))))
             (lazy-exprs
               (gen-ast-l (map cadr nor) lazy-bind)))
        (jump-to-version cgc lazy-exprs ctx)))))

;;
;; TODO
(define (mlc-letrec ast succ)

  (define const-proc-vars '()) ;; (id free-info ast)
  (define proc-vars '()) ;; (id pos ast free-info)
  (define other-vars '()) ;; (id ast)

  (define (reset-sets!)
    (set! const-proc-vars '())
    (set! proc-vars '())
    (set! other-vars '()))

  (define (get-f-size fset)
    (if (null? fset)
        0
        (let ((prev-finfo (cadddr (car fset))))
          (+ (cadar fset) ;; pos previous
             2 ;; header, entry
             (length (cadr prev-finfo)) ;; nb imm !cst free
             (length (caddr prev-finfo)))))) ;; nb late free

  ;; Group bindings, return list (proc-vars, other-vars)
  ;; proc-vars represent procedure bindings, with (id free-info val)
  ;; other-vars represent other bindings
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
                        (cons (list id free-info val) proc-vars))
                      other-vars)
                (loop (cdr bindings)
                      proc-vars
                      (cons binding other-vars)))))))

  (define (find-cons-proc-vars proc-vars)

    (let loop ((const-proc-vars '())
               (proc-vars proc-vars))

      (let loop2 ((l proc-vars)
                  (new-const-proc-vars const-proc-vars)
                  (new-proc-vars '()))
        (if (null? l)
            (if (= (length const-proc-vars)
                   (length new-const-proc-vars))
                (list new-const-proc-vars new-proc-vars)
                (loop new-const-proc-vars new-proc-vars))
            (let* ((proc-var (car l))
                   (free-info (cadr proc-var))
                   (all-free (append (cadr free-info) (caddr free-info))))
              (if (= (length (set-inter all-free
                                        (map car const-proc-vars)))
                     (length all-free))
                  (loop2 (cdr l) (cons proc-var new-const-proc-vars) new-proc-vars)
                  (loop2 (cdr l) new-const-proc-vars (cons proc-var new-proc-vars))))))))

  (define (bind-const-proc-vars ctx const-proc-vars)
    (let* (;; Gen cst-set to bind constants with a cst type with a fake fn-num (-1)
           (cst-set
             (map (lambda (c)
                    (cons (car c) -1))
                  const-proc-vars))
           ;; Bind const ids using fake cst-set
           (ctx (ctx-bind-consts ctx cst-set)))
      ;; Init entry and update fn-num
      ;; of each new const variable
      (let loop ((l const-proc-vars)
                 (ctx ctx))
        (if (null? l)
            ctx
            (let* ((free-inf (cadr (car l)))
                   (free-late (caddr free-inf))
                   (fn-num (init-entry-cst (caddr (car l)) free-late ctx)))
              (loop (cdr l)
                    (ctx-cst-fnnum-set! ctx (caar l) fn-num)))))))


  (define (compute-sets! ctx)
    ;; 1 group proc-vars and other-vars
    (mlet ((proc/other (group-proc-others ctx (map car (cadr ast)) (cadr ast)))
           (const-proc/proc (find-cons-proc-vars proc)))
           ;;
           ;(ctx (bind-const-proc-vars ctx const-proc-vars)))

      ;; TODO: match api
      ;; TODO: Keed current layout and only compute pos + cst-late
      (let ((r '())
            (cst-ids (map car const-proc)))
        (let loop ((l proc))
          (if (null? l)
              '()
              (let* ((obj (car l))
                     (id (car obj))
                     (pos
                       (if (null? r)
                           0
                           (let* ((last (car r))
                                  (last-finfo (cadddr last)))
                             (+ 2
                                (cadr last)
                                (length (cadr last-finfo)) ;;
                                (length (caddr last-finfo)))))) ;;
                     (ast (caddr obj))
                     (old-free-info (cadr obj))
                     (free-info
                       (list (append (car old-free-info)
                                     (set-inter cst-ids
                                                (caddr old-free-info)))
                             (cadr old-free-info)
                             (set-sub (caddr old-free-info) cst-ids '()))))
                (set! r
                      (cons (list id pos ast free-info)
                            r))
                (loop (cdr l)))))
        (set! proc-vars r)
        (set! const-proc-vars const-proc)
        (set! other-vars other))))

  ;; Bind letrec ids to:
  ;; no location if !cst
  ;; cst type if cst
  (define (bind-ids cgc succ ctx)
    (let* ((ids (map car (cadr ast)))
           ;; Bind cst
           (ctx (bind-const-proc-vars ctx const-proc-vars))
           ;; Bind others
           (id-idx (map (lambda (el) (cons (car el) #f))
                        (append proc-vars other-vars)))
           (ctx (ctx-bind-locals ctx id-idx #t)))
      (jump-to-version cgc succ ctx)))

  ;; Eval & bind each !cst !fun bindings
  (define (eval-nf other-vars succ)

    (define (bind-last id succ)
      (make-lazy-code
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

  ;; Create !cst fun closures
  (define (create-fun succ)

    (define (mlc-lambda-letrec id ast succ closures-size clo-offset free-cst free-imm free-late)
      (make-lazy-code
        (lambda (cgc ctx)

          ;; TODO: supprimer les références à MSECTION_BIGGEST, utilie mem-still-required? à la place
          ;; TODO: restaurer rcx si il est utilisé (donc si still? est vrai)
          (define still? #f) ;; TODO
          (define clo-reg  (if still? (x86-rcx) alloc-ptr))
          (define clo-life (if still? LIFE_STILL LIFE_MOVE))

          (define offset-start (* -8 (- closures-size clo-offset)))
          (define offset-header offset-start)
          (define offset-entry  (+ offset-header 8))
          (define offset-free   (+ offset-entry 8))
          (define offset-free-h (* -1 (- closures-size clo-offset 2)))
          (define offset-late   (+ offset-free (* 8 (length free-imm))))
          ;; TODO: comment
          (if still?
              (x86-lea cgc (x86-rcx) (x86-mem (* 8 closures-size) (x86-rax))))
          ;; Write header
          (let ((clo-size (* 8 (+ (length (append free-imm free-late)) 1))))
            (x86-mov cgc (x86-rax) (x86-imm-int (mem-header clo-size STAG_PROCEDURE clo-life)))
            (x86-mov cgc (x86-mem offset-header clo-reg) (x86-rax)))
          ;; Write entry
          (let* ((entry-obj (init-entry ast ctx (append free-cst free-imm) free-late #f))
                 (entry-obj-loc (- (obj-encoding entry-obj) 1)))
            (x86-mov cgc (x86-rax) (x86-imm-int entry-obj-loc))
            (x86-mov cgc (x86-mem offset-entry clo-reg) (x86-rax)))
          ;; Write free vars
          (gen-free-vars cgc free-imm ctx offset-free-h clo-reg)
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

    ;; 1! alloc big closure
    (define closures-size
            (if (null? proc-vars)
                0
                (let* ((last (car proc-vars))
                       (last-finfo (cadddr last)))
                  (+ 2
                     (cadr last)
                     (length (cadr last-finfo)) ;; nb imm !cst free
                     (length (caddr last-finfo)))))) ;; nb late free

    (let ((lazy-write-closures
            (let loop ((lst (reverse proc-vars)))
              (if (null? lst)
                  succ
                  (let* ((l (car lst))
                         (id (car l))
                         (offset (cadr l))
                         (ast (caddr l))
                         (free-info (cadddr l))
                         (fvars-cst  (car free-info))
                         (fvars-imm  (cadr free-info))
                         (fvars-late (caddr free-info))
                         (next (loop (cdr lst))))
                    ;; closures-size clo-offset free-imm free-late
                    (mlc-lambda-letrec id ast next closures-size offset fvars-cst fvars-imm fvars-late))))))
      (make-lazy-code
        (lambda (cgc ctx)
          (gen-allocation-imm cgc STAG_PROCEDURE (* 8 (- closures-size 1)))
          (jump-to-version cgc lazy-write-closures ctx)))))

  (make-lazy-code
    (lambda (cgc ctx)
      (reset-sets!)
      (compute-sets! ctx)
      ;(if (member 'payoff-if-removed (map car (cadr ast)))
          ;(begin (println "###########################################################")
          ;       (println "################ CONST")
          ;       (println "###########################################################")
          ;       (pp const-proc-vars)
          ;       (println "###########################################################")
          ;       (println "################ OTHER")
          ;       (println "###########################################################")
          ;       (pp other-vars)
          ;       (println "###########################################################")
          ;       (println "################ FUN")
          ;       (println "###########################################################")
          ;       (pp proc-vars)
          ;       (error "oK"))
      (let* ((lazy-let-out (get-lazy-lets-out ast (map car (cadr ast)) (length const-proc-vars) succ))
             (lazy-body    (gen-ast (caddr ast) lazy-let-out))
             (lazy-fun  (create-fun lazy-body))
             (lazy-eval (eval-nf other-vars lazy-fun)))

        (bind-ids cgc lazy-eval ctx)))))

;; Create and return out lazy code object of let/letrec
;; Unbind locals, unbox result, and update ctx
(define (get-lazy-lets-out ast ids nb-cst succ)

  (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                     make-lazy-code-ret
                     make-lazy-code)))
    (make-lc
      (lambda (cgc ctx)
        (let* ((type (car (ctx-stack ctx)))
               (loc  (ctx-get-loc ctx 0))
               (ctx  (ctx-unbind-locals ctx ids))
               (ctx  (ctx-pop-n ctx (+ (- (length ids) nb-cst) 1)))
               (ctx  (ctx-push ctx type loc)))
          (jump-to-version cgc succ ctx))))))

;;-----------------------------------------------------------------------------
;; SPECIAL

;;
;; Make lazy code from SPECIAL FORM
;;
(define (mlc-special sym ast succ)
  (cond ((eq? sym 'breakpoint)
         (make-lazy-code
           (lambda (cgc ctx)
             (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
               (apply-moves cgc ctx moves)
               (x86-call cgc label-breakpoint-handler)
               (codegen-void cgc reg)
               (jump-to-version cgc succ (ctx-push ctx (make-ctx-tvoi) reg))))))
        ((eq? sym '$$sys-clock-gettime-ns)
         (make-lazy-code
           (lambda (cgc ctx)
             (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 0)))
               (apply-moves cgc ctx moves)
               (codegen-sys-clock-gettime-ns cgc reg)
               (jump-to-version cgc succ (ctx-push ctx (make-ctx-tint) reg))))))
        ((eq? sym '##subtype)
         (let* ((lazy-imm
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 1)))
                        (apply-moves cgc ctx moves)
                        (codegen-literal cgc STAG_PAIR reg)
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tint) reg))))))
                (lazy-subtype
                  (make-lazy-code
                    (lambda (cgc ctx)
                      ;; We know here that the value is not a pair
                      (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 1))
                             (type (ctx-get-type ctx 0)))
                        (apply-moves cgc ctx moves)
                        (if (ctx-tunk? type)
                            ;; type is unknown, get it from memory
                            (codegen-subtype cgc (ctx-fs ctx) reg (ctx-get-loc ctx 0))
                            ;; type is known, gen literal
                            (codegen-literal cgc (ctx-type->stag type) reg))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tint) reg))))))
                (lazy-pair-check
                         (gen-dyn-type-test ATX_PAI 0 lazy-imm lazy-subtype ast)))

           (gen-ast (cadr ast) lazy-pair-check)))))

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
        (gen-ast-l (cdr ast) lazy-call))))

;;-----------------------------------------------------------------------------
;; PRIMITIVES

;; primitive not
(define (prim-not cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0)))
    (codegen-not cgc (ctx-fs ctx) reg lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg))))

;; primitive eq?
(define (prim-eq? cgc ctx reg succ cst-infos)
;; NOTE: if inlined-if-cond? is #t, reg register is uselessly freed

  (define inlined-if-cond? (member 'cond (lazy-code-flags succ)))

  (let* ((lcst (assoc 0 cst-infos))
         (rcst (assoc 1 cst-infos))
         (tright (if rcst (literal->ctx-type (cdr rcst)) (ctx-get-type ctx 0)))
         (lright (if rcst (cdr rcst) (ctx-get-loc ctx 0)))
         (tleft
           (if lcst
               (literal->ctx-type (cdr lcst))
               (if rcst
                   (ctx-get-type ctx 0)
                   (ctx-get-type ctx 1))))
         (lleft
           (if lcst
               (cdr lcst)
               (if rcst
                   (ctx-get-loc ctx 0)
                   (ctx-get-loc ctx 1))))
         (n-pop (count (list lcst rcst) not)))

    (if (and (not (ctx-tunk? tleft))
             (not (ctx-tunk? tright))
             (not (ctx-type-teq? tleft tright)))
        ;; Both types are known and !=
        (if inlined-if-cond?
            ;; Then if it's an if cond, jump directly to false branch
            (let ((lco-false (lazy-code-lco-false succ)))
              (jump-to-version cgc lco-false (ctx-pop-n ctx n-pop)))
            ;; Then if it's not an if cond, result is #f
            (begin
              (x86-mov cgc (codegen-reg-to-x86reg reg) (x86-imm-int (obj-encoding #f)))
              (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tboo) reg))))
        ;; Both types are not known
        (begin
          (codegen-eq? cgc (ctx-fs ctx) reg lleft lright lcst rcst inlined-if-cond?)
          (let ((ctx
                  (if inlined-if-cond?
                      (ctx-pop-n ctx n-pop) ;; if it's an if inlined condition, no push required
                      (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tboo) reg))))
            (if inlined-if-cond?
                ((lazy-code-generator succ) cgc ctx x86-jne)
                (jump-to-version cgc succ ctx)))))))

;; primitive number?
(define (prim-number? cgc ctx reg succ cst-infos)

  (define (get-lazy-res r)
    (make-lazy-code
      (lambda (cgc ctx)
        (codegen-set-bool cgc r reg)
        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg)))))

  (let* ((lazy-flo (gen-dyn-type-test ATX_FLO 0 (get-lazy-res #t) (get-lazy-res #f) #f))
         (lazy-fix (gen-dyn-type-test ATX_INT 0 (get-lazy-res #t) lazy-flo #f)))
    (jump-to-version cgc lazy-fix ctx)))

;; primitives car & cdr
(define (prim-cxr cgc ctx reg succ cst-infos op)
  (let ((lval (ctx-get-loc ctx 0)))
    (codegen-car/cdr cgc (ctx-fs ctx) op reg lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tunk) reg))))

;; primitive eof-object?
(define (prim-eof-object? cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0)))
    (codegen-eof? cgc (ctx-fs ctx) reg lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg))))

;; primitive make-string
(define (prim-make-string cgc ctx reg succ cst-infos args)
  (let* ((init-value? (= (length args) 2))
         (llen (ctx-get-loc ctx (if init-value? 1 0)))
         (lval (if init-value? (ctx-get-loc ctx 0) #f)))
    (codegen-make-string cgc (ctx-fs ctx) reg llen lval)
    (jump-to-version cgc succ (ctx-push (if init-value?
                                            (ctx-pop-n ctx 2)
                                            (ctx-pop ctx))
                                        (make-ctx-tstr)
                                        reg))))

;; primitive make-vector
(define (prim-make-vector cgc ctx reg succ cst-infos args)
  (let* ((init-value? (= (length args) 2))
         (llen (ctx-get-loc ctx (if init-value? 1 0)))
         (lval (if init-value? (ctx-get-loc ctx 0) #f)))
    (if (and (fixnum? (car args)) (< (car args) MSECTION_BIGGEST))
        (codegen-make-vector-cst cgc (ctx-fs ctx) reg (car args) lval)
        (codegen-make-vector cgc (ctx-fs ctx) reg llen lval))
    (jump-to-version cgc succ (ctx-push (if init-value?
                                            (ctx-pop-n ctx 2)
                                            (ctx-pop ctx))
                                        (make-ctx-tvec)
                                        reg))))

;; primitive vector-ref
(define (prim-vector-ref cgc ctx reg succ cst-infos)

  (let* ((poscst (assoc 1 cst-infos))
         (lidx (if poscst (cdr poscst) (ctx-get-loc ctx 0)))
         (lvec
           (if poscst
               (ctx-get-loc ctx 0)
               (ctx-get-loc ctx 1)))
         (n-pop (if poscst 1 2)))

    (codegen-vector-ref cgc (ctx-fs ctx) reg lvec lidx poscst)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tunk) reg))))

;; primitive string-ref
(define (prim-string-ref cgc ctx reg succ cst-infos)

  (let* ((poscst (assoc 1 cst-infos))
         (lidx (if poscst (cdr poscst) (ctx-get-loc ctx 0)))
         (lstr
           (if poscst
               (ctx-get-loc ctx 0)
               (ctx-get-loc ctx 1)))
         (n-pop (if poscst 1 2)))
    (codegen-string-ref cgc (ctx-fs ctx) reg lstr lidx poscst)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tcha) reg))))

;; primitive vector-set!
(define (prim-vector-set! cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0))
        (lidx (ctx-get-loc ctx 1))
        (lvec (ctx-get-loc ctx 2)))
    (codegen-vector-set! cgc (ctx-fs ctx) reg lvec lidx lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 3) (make-ctx-tvoi) reg))))

;; primitive string-set!
(define (prim-string-set! cgc ctx reg succ cst-infos)

  (let* ((idx-cst (assoc 1 cst-infos))
         (chr-cst (assoc 2 cst-infos))
         (lchr (if chr-cst (cdr chr-cst) (ctx-get-loc ctx 0)))
         (lidx
           (if idx-cst
               (cdr idx-cst)
               (if chr-cst
                   (ctx-get-loc ctx 0)
                   (ctx-get-loc ctx 1))))
         (n-pop (+ (count (list idx-cst chr-cst) not) 1))
         (lstr (ctx-get-loc ctx (- n-pop 1))))
    (codegen-string-set! cgc (ctx-fs ctx) reg lstr lidx lchr idx-cst chr-cst)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tvoi) reg))))

;;
(define (prim-symbol->string cgc ctx reg succ cst-infos)
  (let* ((lsym  (ctx-get-loc ctx 0)))
    (codegen-symbol->string cgc (ctx-fs ctx) reg lsym)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tstr) reg))))

;;
(define (prim-mem-allocated? cgc ctx reg succ cst-infos)
  (let ((type (ctx-get-type ctx 0)))
    (cond ((ctx-tunk? type)
             (let ((lval (ctx-get-loc ctx 0)))
               (codegen-mem-allocated? cgc (ctx-fs ctx) reg lval)))
          ((ctx-type-mem-allocated? type)
             (codegen-set-bool cgc #t reg))
          (else
             (codegen-set-bool cgc #f reg))))
  (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg)))

;;
(define (prim-subtyped? cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0)))
    (codegen-subtyped? cgc (ctx-fs ctx) reg lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg))))

;;
(define (prim-fxl-op cgc ctx reg succ cst-infos op cgcfn fx?)
  (let* ((rcst  (assoc 1 cst-infos))
         (lcst  (assoc 0 cst-infos))
         (n-pop  (count (list lcst rcst) not))
         (lright (or (and rcst (cdr rcst)) (ctx-get-loc ctx 0)))
         (lleft  (or (and lcst (cdr lcst))
                     (if rcst
                         (ctx-get-loc ctx 0)
                         (ctx-get-loc ctx 1)))))
    (if (and lcst rcst)
        (codegen-literal cgc (+ (cdr lcst) (cdr rcst)) reg)
        (if (eq? cgcfn codegen-num-ii)
            (cgcfn cgc (ctx-fs ctx) op reg lleft lright lcst rcst #f)
            (cgcfn cgc (ctx-fs ctx) op reg lleft #f lright #f lcst rcst #f)))
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) (if fx? (make-ctx-tint) (make-ctx-tflo)) reg))))

(define (prim-fxl?-op cgc ctx reg succ cst-infos op cgcfn)

  (let* (;; Primitive
         (rcst  (assoc 1 cst-infos))
         (lcst  (assoc 0 cst-infos))
         (n-pop  (count (list lcst rcst) not))
         (lright (or (and rcst (cdr rcst)) (ctx-get-loc ctx 0)))
         (lleft  (or (and lcst (cdr lcst))
                     (if rcst
                         (ctx-get-loc ctx 0)
                         (ctx-get-loc ctx 1))))
         ;; Branch generator
         (generator
           (lambda (cgc)
             (codegen-num-ii cgc (ctx-fs ctx) op reg lleft lright lcst rcst #f)))
         ;; No overflow, next is succ
         (lazy-false succ)
         (ctx-false (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tint) reg))
         ;; Overflow, next is a new lco
         (lazy-true
           (make-lazy-code
             (lambda (cgc ctx)
               (x86-mov cgc (codegen-reg-to-x86reg reg) (x86-imm-int (obj-encoding #f)))
               (jump-to-version cgc succ ctx))))
         (ctx-true (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tboo) reg)))

  (assert (not (and lcst rcst)) "Internal error (prim-fxl-op?)")

  (jump-to-version
    cgc
    (mlc-branch x86-jo generator lazy-true lazy-false ctx-true ctx-false)
    ctx)))

;;
(define (prim-fixnum->flonum cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0)))
    (codegen-fixnum->flonum cgc (ctx-fs ctx) reg lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tflo) reg))))

;;
(define (prim-box cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0)))
    (codegen-box cgc (ctx-fs ctx) reg lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tbox) reg))))

;;
(define (prim-set-box! cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0))
        (lbox (ctx-get-loc ctx 1)))
    (codegen-set-box cgc (ctx-fs ctx) reg lbox lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) (make-ctx-tvoi) reg))))

;;
(define (prim-unbox cgc ctx reg succ cst-infos)
  (let ((lbox (ctx-get-loc ctx 0)))
    (codegen-unbox cgc (ctx-fs ctx) reg lbox)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tunk) reg))))

;; primitives set-car! & set-cdr!
(define (prim-set-cxr! cgc ctx reg succ cst-infos op)

  (let* ((valcst (assoc 1 cst-infos))
         (lval  (if valcst (cdr valcst) (ctx-get-loc ctx 0)))
         (lpair
           (if valcst
               (ctx-get-loc ctx 0)
               (ctx-get-loc ctx 1)))
         (n-pop (if valcst 1 2)))
    (codegen-scar/scdr cgc (ctx-fs ctx) op reg lpair lval valcst)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tvoi) reg))))

;; primitives current-input-port & current-output-port
(define (prim-current-x-port cgc ctx reg succ cst-infos op)
  (define lazy-out
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((stag (if (eq? op 'current-input-port) STAG_IPORT STAG_OPORT))
               (ctx (ctx-set-type ctx 0 stag)))
          (jump-to-version cgc succ ctx)))))
  (define lazy-current
    (let* ((sym (string->symbol (string-append "gambit$$" (symbol->string op))))
           (node (atom-node-make sym)))
      (gen-ast (list node) lazy-out)))
  (jump-to-version cgc lazy-current ctx))

;; primitives char->integer & integer->char
(define (prim-char<->int cgc ctx reg succ cst-infos op)
  (let* ((cst-arg (assoc 0 cst-infos))
         (lval
          (if cst-arg
              (cdr cst-arg)
              (ctx-get-loc ctx 0)))
         (n-pop (if cst-arg 0 1)))
    (codegen-ch<->int cgc (ctx-fs ctx) op reg lval cst-arg)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop)
                                        (if (eq? op 'char->integer)
                                            (make-ctx-tint)
                                            (make-ctx-tcha))
                                        reg))))

;; primitives vector-length & string-length
(define (prim-x-length cgc ctx reg succ cst-infos op)
  (let ((lval (ctx-get-loc ctx 0))
        (codegen-fn
          (if (eq? op 'vector-length)
              codegen-vector-length
              codegen-string-length)))
    (codegen-fn cgc (ctx-fs ctx) reg lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tint) reg))))

;;

(define (mlc-primitive prim ast succ)
  (cond ((and (= (length ast) 2)
              (member prim '(##fx-? ##fl-)))
           (error "NYI atom mlc-primitive 1"))
          ; (mlc-primitive-d (list op 0 (cadr ast)) succ))
        ((and (= (length ast) 2)
              (eq? prim 'zero?))
           (let ((ast (list (atom-node-make '=)
                            (cadr ast)
                            (atom-node-make 0))))
             (gen-ast ast succ)))
        (else
           (mlc-primitive-d prim ast succ))))

(define (mlc-primitive-d prim ast succ)

  ;; Assert primitive nb args
  (assert-p-nbargs prim ast)

  ;;
  (let* ((cst-infos (get-prim-cst-infos prim ast))
         (lazy-primitive
           (cond
             ((eq? prim 'exit) (get-lazy-error ""))
             ((eq? prim 'cons) (mlc-pair succ cst-infos))
             (else
               (make-lazy-code
                 (lambda (cgc ctx)
                   (define nb-opnds (- (length (cdr ast)) (length cst-infos)))
                   (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ nb-opnds)))
                     (apply-moves cgc ctx moves)
                     ;; TODO: add function in 'primitives' set
                     (case prim
                       ((not)               (prim-not            cgc ctx reg succ cst-infos))
                       ((eq?)               (prim-eq?            cgc ctx reg succ cst-infos))
                       ((char=?)            (prim-eq?            cgc ctx reg succ cst-infos))
                       ((number?)           (prim-number?        cgc ctx reg succ cst-infos))
                       ((car cdr)           (prim-cxr            cgc ctx reg succ cst-infos prim))
                       ((eof-object?)       (prim-eof-object?    cgc ctx reg succ cst-infos))
                       ((make-string)       (prim-make-string    cgc ctx reg succ cst-infos (cdr ast)))
                       ((make-vector)       (prim-make-vector    cgc ctx reg succ cst-infos (cdr ast)))
                       ((vector-ref)        (prim-vector-ref     cgc ctx reg succ cst-infos))
                       ((string-ref)        (prim-string-ref     cgc ctx reg succ cst-infos))
                       ((vector-set!)       (prim-vector-set!    cgc ctx reg succ cst-infos))
                       ((string-set!)       (prim-string-set!    cgc ctx reg succ cst-infos))
                       ((symbol->string)    (prim-symbol->string cgc ctx reg succ cst-infos))
                       ((##fx+)             (prim-fxl-op         cgc ctx reg succ cst-infos '+ codegen-num-ii #t))
                       ((##fx-)             (prim-fxl-op         cgc ctx reg succ cst-infos '- codegen-num-ii #t))
                       ((##fx*)             (prim-fxl-op         cgc ctx reg succ cst-infos '* codegen-num-ii #t))
                       ((##fx+?)            (prim-fxl?-op        cgc ctx reg succ cst-infos '+ codegen-num-ii))
                       ((##fx-?)            (prim-fxl?-op        cgc ctx reg succ cst-infos '- codegen-num-ii))
                       ((##fx*?)            (prim-fxl?-op        cgc ctx reg succ cst-infos '* codegen-num-ii))
                       ((##fl+)             (prim-fxl-op         cgc ctx reg succ cst-infos '+ codegen-num-ff #f))
                       ((##fl-)             (prim-fxl-op         cgc ctx reg succ cst-infos '- codegen-num-ff #f))
                       ((##fl*)             (prim-fxl-op         cgc ctx reg succ cst-infos '* codegen-num-ff #f))
                       ((##fixnum->flonum)  (prim-fixnum->flonum cgc ctx reg succ cst-infos))
                       ((##mem-allocated?)  (prim-mem-allocated? cgc ctx reg succ cst-infos))
                       ((##subtyped?)       (prim-subtyped?      cgc ctx reg succ cst-infos))
                       ((##box)             (prim-box            cgc ctx reg succ cst-infos))
                       ((##unbox)           (prim-unbox          cgc ctx reg succ cst-infos))
                       ((##set-box!)        (prim-set-box!       cgc ctx reg succ cst-infos))
                       ((set-car! set-cdr!)                      (prim-set-cxr!       cgc ctx reg succ cst-infos prim))
                       ((current-input-port current-output-port) (prim-current-x-port cgc ctx reg succ cst-infos prim))
                       ((char->integer integer->char)            (prim-char<->int     cgc ctx reg succ cst-infos prim))
                       ((vector-length string-length)            (prim-x-length       cgc ctx reg succ cst-infos prim))
                       (else (error "Unknown primitive"))))))))))

    (let* ((primitive (assoc prim primitives))
           ;; Get list of types required by this primitive
           (types (if (cadr primitive)
                      (cdr (assoc (length (cdr ast))
                                  (cadddr primitive)))
                      (build-list (length (cdr ast)) (lambda (el) ctx-tall?)))))

      (assert (= (length types)
                 (length (cdr ast)))
              "Primitive error")

      ;; Build args lco chain with type checks
      (check-types types (cdr ast) lazy-primitive ast cst-infos))))

;; TODO WIP
(define (get-prim-cst-infos prim ast)

  (define (get-prim-cst-infos-h args cst-positions curr-pos)
    (if (or (null? args)
            (null? cst-positions))
        '()
        (if (eq? curr-pos (car cst-positions))
            (if (and (atom-node? (car args))
                     (let ((v (atom-node-val (car args))))
                       (or (integer? v)
                           (boolean? v)
                           (char?    v)
                           (null?    v))))
              (cons (cons curr-pos (atom-node-val (car args)))
                    (get-prim-cst-infos-h (cdr args) (cdr cst-positions) (+ curr-pos 1)))
              (get-prim-cst-infos-h (cdr args) (cdr cst-positions) (+ curr-pos 1)))
            (get-prim-cst-infos-h (cdr args) cst-positions (+ curr-pos 1)))))

  (let ((primitive (assoc prim primitives)))
    (get-prim-cst-infos-h
      (cdr ast)
      (cadddr (cdr primitive))
      0)))

;; Build lazy objects chain of 'args' list
;; and insert type check for corresponding 'types'
;; TODO: rename types -> type-tests-fn
(define (check-types types args succ ast #!optional (cst-infos '()))

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
        (let* ((lazy-next (check-types-h (cdr types) (cdr args) (+ curr-pos 1)))
               (r (assoc curr-pos cst-infos)))
          (cond ;;
                ((or (and r (eq? (car types) ATX_ALL))
                     (and r (check-cst-type (car types) (cdr r))))
                   lazy-next)
                ;;
                (r
                   (get-lazy-error "NYI ERROR WRONG TYPE"))
                ;;
                ((eq? (car types) ATX_ALL)
                   (gen-ast (car args) lazy-next))
                ;;
                (else
                   (gen-ast (car args)
                            (gen-fatal-type-test (car types) 0 lazy-next ast)))))))

  (check-types-h types args 0))

;;
;; Vector primitive (not in primitives because we need >1 LCO)
(define (mlc-vector-p ast succ)

  (define (get-chain i exprs)
    (if (null? exprs)
        succ
        (let ((lco-set
                (make-lazy-code
                  (lambda (cgc ctx)
                    (let ((succ (get-chain (+ i 1) (cdr exprs)))
                          (lval (ctx-get-loc ctx 0))
                          (lvec (ctx-get-loc ctx 1)))

                      (let ((opval (codegen-loc-to-x86opnd (ctx-fs ctx) lval))
                            (opvec (codegen-loc-to-x86opnd (ctx-fs ctx) lvec)))
                        (if (x86-mem? opvec)
                            (begin (x86-mov cgc (x86-rax) opvec)
                                   (x86-mov cgc (x86-mem (- (+ 8 (* 8 i)) TAG_MEMOBJ) (x86-rax)) opval))
                            (x86-mov cgc (x86-mem (- (+ 8 (* 8 i)) TAG_MEMOBJ) opvec) opval)))
                      (jump-to-version cgc succ (ctx-pop ctx)))))))
          (gen-ast (car exprs) lco-set))))

  (gen-ast
    (list (atom-node-make 'make-vector)
          (atom-node-make (length (cdr ast))))
    (get-chain 0 (cdr ast))))

;;
;; List primitive (not in primitives because we need >1 LCO)
(define (mlc-list-p ast succ)

  (define len (length (cdr ast)))

  (define (build-chain n)
    (if (= n 0)
        (make-lazy-code
          (lambda (cgc ctx)
            (let ((loc (ctx-get-loc ctx n)))
              (let ((pair-offset (* -3 8 n)))
                ;; Write header
                (x86-mov cgc (x86-mem (- pair-offset 24) alloc-ptr) (x86-imm-int (mem-header 16 STAG_PAIR)) 64)
                ;; Write null in cdr
                (x86-mov cgc (x86-mem (- pair-offset 16) alloc-ptr) (x86-imm-int (obj-encoding '())) 64)
                ;; Write value in car
                (x86-mov cgc (x86-mem (- pair-offset  8) alloc-ptr) (codegen-loc-to-x86opnd (ctx-fs ctx) loc))
                (mlet ((moves/reg/ctx (ctx-get-free-reg (ctx-pop-n ctx len) succ 0)))
                   (apply-moves cgc ctx moves)
                   ;; Load first pair in dest register
                   (let ((dest (codegen-reg-to-x86reg reg)))
                     (let ((offset (+ (* len 3 -8) TAG_PAIR)))
                       (x86-lea cgc dest (x86-mem offset alloc-ptr))
                       (jump-to-version cgc succ (ctx-push ctx (make-ctx-tpai) reg)))))))))
        (make-lazy-code
          (lambda (cgc ctx)
            (let ((loc (ctx-get-loc ctx n)))
              (let ((pair-offset (* -3 8 n)))
                ;; Write header
                (x86-mov cgc (x86-mem (- pair-offset 24) alloc-ptr) (x86-imm-int (mem-header 16 STAG_PAIR)) 64)
                ;; Write encoded next pair in cdr
                (x86-lea cgc (x86-rax) (x86-mem (+ pair-offset TAG_PAIR) alloc-ptr))
                (x86-mov cgc (x86-mem (- pair-offset 16) alloc-ptr) (x86-rax))
                ;; Write value in car
                (let ((opnd (codegen-loc-to-x86opnd (ctx-fs ctx) loc)))
                  (if (x86-mem? opnd)
                      (begin (x86-mov cgc (x86-rax) opnd)
                             (set! opnd (x86-rax))))
                  (x86-mov cgc (x86-mem (- pair-offset  8) alloc-ptr) opnd))
                ;; Continue
                (jump-to-version cgc (build-chain (- n 1)) ctx)))))))

  (cond ;; (list)
        ((= len 0)
          (gen-ast (atom-node-make '()) succ))
        ((> (* len 3 8) MSECTION_BIGGEST)
          (gen-ast
            (let* ((sym (gensym))
                   (lnode (atom-node-make 'list))
                   (snode (atom-node-make sym)))
              `(let ((,sym ,lnode))
                 (,snode ,@(cdr ast))))
            succ))
        ;; (list ..)
        (else
        (let* ((lazy-list
                 (make-lazy-code
                   (lambda (cgc ctx)
                     (let ((size (- (* len 3 8) 8)))
                       ;; One alloc for all pairs
                       (gen-allocation-imm cgc STAG_PAIR size)
                       (jump-to-version cgc (build-chain (- len 1)) ctx))))))
          (gen-ast-l (cdr ast) lazy-list)))))

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

                     ;; Si on reconnait le pattern (if SYM SYM X) et que succ est cond

                     (if x86-op
                         (codegen-inlined-if cgc label-jump label-false label-true x86-op)
                         (let* ((lcond (ctx-get-loc ctx 0)))
                           (codegen-if cgc (ctx-fs ctx) label-jump label-false label-true lcond)))))))))

    (gen-ast
      (cadr ast)
      lazy-code-test)))

(define (mlc-branch x86-jop generator lazy-true lazy-false ctx-true ctx-false)

  (make-lazy-code
    (lambda (cgc ctx)

      (let* ((label-jump (asm-make-label cgc (new-sym 'patchable_jump)))
             (stub-first-label-addr #f)
             (stub-labels
               (add-callback cgc 1
                 (let ((prev-action #f))
                   (lambda (ret-addr selector)
                     (let ((stub-addr stub-first-label-addr)
                           (jump-addr (asm-label-pos label-jump)))

                       (if (not prev-action)
                           (begin (set! prev-action 'no-swap)
                                  (if (= selector 1)
                                      ;; overwrite unconditional jump
                                      (gen-version (+ jump-addr 6) lazy-false ctx-false)
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
                                                              lazy-true
                                                              ctx-true))

                                          ;; make conditional jump to new version
                                          (gen-version jump-addr lazy-true ctx-true))))

                           (begin ;; one branch has already been patched
                                  ;; reclaim the stub
                                  (release-still-vector (get-scmobj ret-addr))
                                  (stub-reclaim stub-addr)
                                  (if (= selector 0)
                                     (gen-version (if (eq? prev-action 'swap) (+ jump-addr 6) jump-addr) lazy-true ctx-true)
                                     (gen-version (if (eq? prev-action 'swap) jump-addr (+ jump-addr 6)) lazy-false ctx-false))))))))))

        (set! stub-first-label-addr
              (min (asm-label-pos (list-ref stub-labels 0))
                   (asm-label-pos (list-ref stub-labels 1))))

        (generator cgc)

        (x86-label cgc label-jump)
        (x86-jop cgc (list-ref stub-labels 0))
        (x86-jmp cgc (list-ref stub-labels 1))))))


;;-----------------------------------------------------------------------------
;; APPLY & CALL

(define (call-get-eploc ctx op)

  (if (and (atom-node? op)
           (symbol? (atom-node-val op)))
      ;; Op is an atom node with a symbol
      (let ((sym (atom-node-val op)))

        (define global-opt?
          (and (not (assoc sym (ctx-env ctx)))
               (ctx-tclo? (table-ref gids sym #f))))

        (if global-opt?
            (let ((fn-num (ctime-entries-get sym)))
              (if (not fn-num) (error "Internal error (call-get-eploc)"))
              fn-num)
            (ctx-get-eploc ctx sym)))
      #f))

;;
;; Make lazy code from APPLY
;;
(define (mlc-apply ast succ)

  (let* ((lazy-call
          (make-lazy-code
            (lambda (cgc ctx)
              (let ((fn-num (call-get-eploc ctx (cadr ast))))
                (x86-mov cgc (x86-rdi) (x86-r11)) ;; Copy nb args in rdi
                (x86-mov cgc (x86-rsi) (x86-rax)) ;; Move closure in closure reg
                (gen-call-sequence ast cgc #f #f fn-num)))))
        (lazy-args
          (make-lazy-code
            (lambda (cgc ctx)
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
                (jump-to-version cgc lazy-call ctx)))))
        (lazy-pre
          (make-lazy-code
            (lambda (cgc ctx)

              ;; Save used registers, generate and push continuation stub
              (set! ctx (call-save/cont cgc ctx ast succ #f 2 #t))

              ;; Push closure
              (call-get-closure cgc ctx 1)
              (jump-to-version cgc lazy-args ctx)))))

    (let ((lazy-lst (gen-ast (caddr ast) lazy-pre)))
      (gen-ast (cadr ast) lazy-lst))))

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
            (gen-continuation-cr cgc ast succ nctx '() apply?) ;; TODO: remove '() arg
            (gen-continuation-rp cgc ast succ nctx '() apply?))

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
          (let ((opnd (codegen-loc-to-x86opnd fs (car locs))))
            (x86-upush cgc opnd)
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
         (fn-num #f)
         ;; Tail call if successor's flags set contains 'ret flag
         (tail? (member 'ret (lazy-code-flags succ)))
         ;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (get-lazy-error (ERR_TYPE_EXPECTED ATX_CLO)))
         ;; Lazy call
         (lazy-call
           (make-lazy-code
             (lambda (cgc ctx)

               ;; Save used registers, generate and push continuation stub
               (set! ctx (call-save/cont cgc ctx ast succ tail? (+ (length args) 1) #f))

               ;; Move args to regs or stack following calling convention
               (set! ctx (call-prep-args cgc ctx ast (length args) fn-num))

               ;; Shift args and closure for tail call
               (call-tail-shift cgc ctx ast tail? (length args))

               ;; Generate call sequence
               ;; Build call ctx
               (let ((call-ctx
                       (ctx-copy
                         (ctx-init)
                         (append (list-head (ctx-stack ctx) (length (cdr ast)))
                                 (list (make-ctx-tclo) (make-ctx-tret))))))
                 (gen-call-sequence ast cgc call-ctx (length args) fn-num)))))
         ;; Lazy code object to build the continuation
         (lazy-tail-operator (check-types (list ATX_CLO) (list (car ast)) lazy-call ast)))

    ;; Gen and check types of args
    (make-lazy-code
      (lambda (cgc ctx)

        ;; Check if the identity of called function is available
        (set! fn-num (call-get-eploc ctx (car ast)))

        (if fn-num
            (jump-to-version
              cgc
              (gen-ast-l (cdr ast) lazy-call)
              (ctx-push ctx (make-ctx-tclo) #f))
            (jump-to-version
              cgc
              (check-types (list ATX_CLO) (list (car ast)) (gen-ast-l (cdr ast) lazy-call) ast)
              ctx))))))

;; TODO regalloc: merge -rp and -cr + comments
(define (gen-continuation-rp cgc ast succ ctx saved-regs apply?)

  (let* ((lazy-continuation
           (make-lazy-code-cont
             (lambda (cgc ctx)

               ;; Restore registers
               (for-each
                 (lambda (i)
                   (let ((opnd (codegen-reg-to-x86reg i)))
                     (x86-upop cgc opnd)))
                 (reverse saved-regs))

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

;; TODO regalloc: merge -rp and -cr + comments
(define (gen-continuation-cr cgc ast succ ctx saved-regs apply?)

  (let* ((lazy-continuation
           (make-lazy-code-cont
             (lambda (cgc ctx)

               ;; Restore registers
               (for-each
                 (lambda (i)
                   (let ((opnd (codegen-reg-to-x86reg i)))
                     (x86-upop cgc opnd)))
                 (reverse saved-regs))

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
           (let* ((idx (get-closure-index (ctx-stack call-ctx)))
                  (direct (get-cc-direct idx)))
             (if idx
                 (codegen-call-cc-spe cgc idx nb-args eploc direct)
                 (codegen-call-cc-gen cgc nb-args eploc))))))

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
      (let* ((lazy-op
               (make-lazy-code
                 (lambda (cgc ctx)
                   (mlet ((label-div0 (get-label-error ERR_DIVIDE_ZERO))
                          (moves/reg/ctx (ctx-get-free-reg ctx succ 2))
                          (lleft (ctx-get-loc ctx 1))
                          (lright (ctx-get-loc ctx 0)))
                     (apply-moves cgc ctx moves)
                     (codegen-binop cgc (ctx-fs ctx) op label-div0 reg lleft lright)
                     (jump-to-version cgc
                                      succ
                                      (ctx-push (ctx-pop-n ctx 2) (make-ctx-tint) reg)))))))
         ;; Check operands type
         (check-types (list ATX_INT ATX_INT)
                      (list (car opnds) (cadr opnds))
                      lazy-op
                      ast)))))

;;
;; Make lazy code from N-ARY OPERATOR
;;
(define (mlc-op-n ast succ op) ;; '(+ - * < > <= >= = /)

  (define (int-val-cst n)
    (and (atom-node? n)
         (integer? (atom-node-val n))
         (atom-node-val n)))

  ;; Ast if 0 opnd
  (define (ast0 op)
    (case op
      ((+) (atom-node-make 0))
      ((*) (atom-node-make 1))
      ((< <= > >= =)  (atom-node-make #t))))

  ;; Ast if 1 opnd
  (define (ast1 op opnd)
    (case op
      ((+ *) opnd)
      ((-)   (list (atom-node-make '-)
                   (atom-node-make 0)
                   opnd))
      ((/)   (list (atom-node-make '/)
                   (atom-node-make 1)
                   opnd))
      ((< <= > >= =) (atom-node-make #t))))

  ;; Transform numeric operator
  ;; (+ 1 2 3) -> (+ (+ 1 2) 3)
  (define (trans-num-op ast)
    `(,(car ast)
       ,(list (car ast) (cadr ast) (caddr ast))
       ,@(cdddr ast)))

  (cond
    ((= (length ast) 1) (gen-ast (ast0 op) succ))
    ((= (length ast) 2) (gen-ast (ast1 op (cadr ast)) succ))
    ((> (length ast) 3)
       (if (member op '(+ - * /))
           (gen-ast (trans-num-op ast) succ)
           (error "Internal error (mlc-op-n)"))) ;; comparisons are handled by macro expander
    (else
      (let ((lcst (int-val-cst (cadr ast)))
            (rcst (int-val-cst (caddr ast))))

        (cond
          ((and lcst rcst)
             (if (not (member op '(+ - * /)))
                 (error "NYI mlc-op-n"))
             (let ((r (eval (list op lcst rcst))))
               (gen-ast (atom-node-make r)
                        succ)))
          (lcst
             (gen-ast (caddr ast)
                      (get-lazy-n-binop ast op lcst #f succ)))
          (rcst
             (gen-ast (cadr ast)
                      (get-lazy-n-binop ast op #f rcst succ)))
          (else
            (gen-ast (cadr ast)
                     (gen-ast (caddr ast)
                              (get-lazy-n-binop ast op #f #f succ)))))))))

(define (get-lazy-n-binop ast op lcst rcst succ)

  (define inlined-if-cond? (member 'cond (lazy-code-flags succ)))

  (define num-op? (member op '(+ - * /)))

  ;; Build chain to check type of one value (if one of them is a cst)
  (define (type-check-one)
    (let* (;; Op
           (lazy-op-i
             (if (eq? op '/)
                 (get-op-ff #t #t)
                 (get-op-ii)))
           (lazy-op-f (get-op-ff (integer? lcst) (integer? rcst)))
           ;; Checks
           (lazy-float (gen-fatal-type-test ATX_FLO 0 lazy-op-f ast))
           (lazy-int (gen-dyn-type-test ATX_INT 0 lazy-op-i lazy-float ast)))
      lazy-int))

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
      (lambda (cgc ctx)
        (let* ((type (if num-op? (make-ctx-tint) (make-ctx-tboo)))
               (n-pop (count (list lcst rcst) not))
               (res (if inlined-if-cond? #f (ctx-get-free-reg ctx succ n-pop)))
               (moves (if res (car res) '()))
               (reg   (if res (cadr res) #f))
               (ctx   (if res (caddr res) ctx))
               ;; We need to get locs AFTER ctx-get-free-reg call
               (lright (or rcst (ctx-get-loc ctx 0)))
               (lleft
                 (if rcst
                     (or lcst (ctx-get-loc ctx 0))
                     (or lcst (ctx-get-loc ctx 1)))))
          (apply-moves cgc ctx moves)
          (cond (num-op?
                  (codegen-num-ii cgc (ctx-fs ctx) op reg lleft lright lcst rcst #t)
                  (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) type reg)))
                (inlined-if-cond?
                  (let ((x86-op (codegen-cmp-ii cgc (ctx-fs ctx) op reg lleft lright lcst rcst #t)))
                    ((lazy-code-generator succ) cgc (ctx-pop-n ctx n-pop) x86-op)))
                (else
                    (codegen-cmp-ii cgc (ctx-fs ctx) op reg lleft lright lcst rcst #f)
                    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) type reg))))))))


  ;;
  (define (get-op-ff leftint? rightint?)
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((type (if num-op? (make-ctx-tflo) (make-ctx-tboo)))
               (n-pop (count (list lcst rcst) not))
               (res (if inlined-if-cond? #f (ctx-get-free-reg ctx succ n-pop)))
               (moves (if res (car res) '()))
               (reg (if res (cadr res) #f))
               (ctx (if res (caddr res) ctx))
               ;; We need to get locs AFTER ctx-get-free-reg call
               (lright (or rcst (ctx-get-loc ctx 0)))
               (lleft
                 (if rcst
                     (or lcst (ctx-get-loc ctx 0))
                     (or lcst (ctx-get-loc ctx 1)))))
          (apply-moves cgc ctx moves)
          (cond (num-op?
                  (codegen-num-ff cgc (ctx-fs ctx) op reg lleft leftint? lright rightint? lcst rcst #t)
                  (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) type reg)))
                (inlined-if-cond?
                  (let ((x86-op (codegen-cmp-ff cgc (ctx-fs ctx) op reg lleft leftint? lright rightint? lcst rcst #t)))
                    ((lazy-code-generator succ) cgc (ctx-pop-n ctx n-pop) x86-op)))
                (else
                  (codegen-cmp-ff cgc (ctx-fs ctx) op reg lleft leftint? lright rightint? lcst rcst #f)
                  (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) type reg))))))))

  (assert (not (and inlined-if-cond? (member op '(+ - * /))))
          "Internal compiler error")

  (if (or lcst rcst)
      (type-check-one)
      (type-check-two)))

;;
;; Make lazy code from TYPE TEST
;;
;; TODO: explain if-cond-true
(define (mlc-test sym ast succ)

  (define next-is-cond (member 'cond (lazy-code-flags succ)))

  (define (get-lazy-res bool)
    (make-lazy-code
      (lambda (cgc ctx)
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx succ 1)))
          (apply-moves cgc ctx moves)
          (codegen-set-bool cgc bool reg)
          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (make-ctx-tboo) reg))))))

  (define (get-lazy-inline bool)
    (make-lazy-code
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

;;
;; Make lazy code to create pair
;; Create pair with the too values on top of the stack
;;
;; TODO: move in primitives
(define (mlc-pair succ #!optional (cst-infos '()))
  (make-lazy-code
    (lambda (cgc ctx)
      (mlet ((car-cst (assoc 0 cst-infos))
             (cdr-cst (assoc 1 cst-infos))
             (n-pop (count (list car-cst cdr-cst) not))
             (moves/reg/ctx (ctx-get-free-reg ctx succ n-pop))
             (lcdr (if cdr-cst (cdr cdr-cst) (ctx-get-loc ctx 0)))
             (car-idx (if cdr-cst 0 1))
             (lcar
               (if car-cst
                   (cdr car-cst)
                   (ctx-get-loc ctx car-idx))))
      (apply-moves cgc ctx moves)
      (codegen-pair cgc (ctx-fs ctx) reg lcar lcdr car-cst cdr-cst)
      (jump-to-version cgc
                       succ
                       (ctx-push (ctx-pop-n ctx n-pop) (make-ctx-tpai) reg))))))

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

;;-----------------------------------------------------------------------------
;; Compile time lookup optimization

;; This table associates an entry point to each symbol representing a global
;; immutable function.
;; The entry point is the stub address or the machine code address if opt-entry-points is #f
;; The entry point is the cc-table if opt-entry-points is #t
(define ctime-entries (make-table))
;; Set the entry point for given id
(define (ctime-entries-set id fn-num)
  (table-set! ctime-entries id fn-num))
;; Get currently known entry point from given id
(define (ctime-entries-get id)
  (table-ref ctime-entries id #f))

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
                                         (if (and (eq? (identifier-kind (cdr n)) 'local)
                                                  (not (member 'cst (identifier-flags (cdr n)))))
                                             ;; If local, get type from stack
                                             (let ((type (ctx-identifier-type ctx (cdr n))))
                                               type)
                                             ;; If free, get type from env
                                             (identifier-stype (cdr n))))
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
                              (closure-lidx (- (length (ctx-stack ctx)) 2))
                              (closure-loc  (ctx-get-loc ctx closure-lidx))
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

;; Return all free vars used by ast knowing env 'clo-env'
(define (free-vars body params enc-ids)
  ;; TODO: memoize result with given input to avoid multiple calls (analyses.scm & ast.scm)
  (cond ;; Keyword
        ((symbol? body) '())
        ;; Atom node
        ((atom-node? body)
           (let ((val (atom-node-val body)))
             (if (and (symbol? val)
                      (member val enc-ids)
                      (not (member val params)))
                 (list val)
                 '())))
        ;; Pair
        ((pair? body)
          (let ((op (car body)))
            (cond ;; If
                  ((eq? op 'if) (set-union (free-vars (cadr body) params enc-ids)   ; cond
                                           (set-union (free-vars  (caddr body) params enc-ids)    ; then
                                                      (free-vars (cadddr body) params enc-ids)))) ; else
                  ;; Quote
                  ((eq? op 'quote) '())
                  ;; Lambda
                  ((eq? op 'lambda) (free-vars (caddr body)
                                               (if (list? (cadr body))
                                                  (append (cadr body) params)
                                                  (cons (cadr body) params))
                                               enc-ids))
                  ;; Call
                  (else (free-vars-l body params enc-ids)))))
        ;;
        (else (error "Unexpected expr (free-vars)"))))

;;
;; UTILS
;;

;; Get position of current closure in stack
(define (closure-pos ctx)
  (- (length (ctx-stack ctx)) 2 (ctx-nb-args ctx))) ;; 2= 1length + 1retAddr

;; Get formal params from list of params
;; Ex: (formal-params '(a b c)  ) -> '(a b c)
;;     (promal-params '(a b . c)) -> '(a b)
(define (formal-params l)
  (if (not (pair? l))
     '()
     (cons (car l) (formal-params (cdr l)))))

;; Return label of a stub generating error with 'msg'
(define (get-label-error msg) (list-ref (add-callback #f   0 (lambda (ret-addr selector) (error msg))) 0))
