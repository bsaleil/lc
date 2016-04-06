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
;; Type predicates

(define type-predicates `(
  (output-port? . ,CTX_OPORT)
  (input-port?  . ,CTX_IPORT)
  (symbol?      . ,CTX_SYM)
  (string?      . ,CTX_STR)
  (char?        . ,CTX_CHAR)
  (vector?      . ,CTX_VECT)
  (fixnum?      . ,CTX_INT)
  (flonum?      . ,CTX_FLO)
  (procedure?   . ,CTX_CLO)
  (pair?        . ,CTX_PAI)
  (null?        . ,CTX_NULL)
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

;; Primitives: name, nb args min, nb args max, args types, cst positions to check
(define primitives `(
                     (car                 1  1  ,(prim-types 1 CTX_PAI)                     ())
                     (cdr                 1  1  ,(prim-types 1 CTX_PAI)                     ())
                     (eq?                 2  2  ,(prim-types 2 CTX_ALL CTX_ALL)          (0 1))
                     (char=?              2  2  ,(prim-types 2 CTX_CHAR CTX_CHAR)        (0 1))
                     (not                 1  1  ,(prim-types 1 CTX_ALL)                     ()) ;; + efficace cst TODO
                     (set-car!            2  2  ,(prim-types 2 CTX_PAI CTX_ALL)            (1))
                     (set-cdr!            2  2  ,(prim-types 2 CTX_PAI CTX_ALL)            (1))
                     (cons                2  2  ,(prim-types 2 CTX_ALL CTX_ALL)          (0 1))
                     (vector-length       1  1  ,(prim-types 1 CTX_VECT)                    ())
                     (vector-ref          2  2  ,(prim-types 2 CTX_VECT CTX_INT)           (1))
                     (char->integer       1  1  ,(prim-types 1 CTX_CHAR)                   (0))
                     (integer->char       1  1  ,(prim-types 1 CTX_INT)                    (0))
                     (string-ref          2  2  ,(prim-types 2 CTX_STR CTX_INT)            (1))
                     (string->symbol      1  1  ,(prim-types 1 CTX_STR)                     ())
                     (symbol->string      1  1  ,(prim-types 1 CTX_SYM)                     ())
                     (close-output-port   1  1  ,(prim-types 1 CTX_OPORT)                   ())
                     (close-input-port    1  1  ,(prim-types 1 CTX_IPORT)                   ())
                     (open-output-file    1  1  ,(prim-types 1 CTX_STR)                     ())
                     (open-input-file     1  1  ,(prim-types 1 CTX_STR)                     ())
                     (string-set!         3  3  ,(prim-types 3 CTX_STR CTX_INT CTX_CHAR) (1 2))
                     (vector-set!         3  3  ,(prim-types 3 CTX_VECT CTX_INT CTX_ALL)    ()) ;; + efficace cst TODO
                     (string-length       1  1  ,(prim-types 1 CTX_STR)                     ())
                     (read-char           1  1  ,(prim-types 1 CTX_IPORT)                   ())
                     (exit                0  0  ,(prim-types 0 )                            ())
                     (make-vector         1  2  ,(prim-types 1 CTX_INT 2 CTX_INT CTX_ALL)   ())
                     (make-string         1  2  ,(prim-types 1 CTX_INT 2 CTX_INT CTX_CHAR)  ())
                     (eof-object?         1  1  ,(prim-types 1 CTX_ALL)                     ())
                     (write-char          2  2  ,(prim-types 2 CTX_CHAR CTX_OPORT)          ())
                     (current-output-port 0  0  ,(prim-types 0 )                            ())
                     (current-input-port  0  0  ,(prim-types 0 )                            ())))


(define (assert-p-nbargs ast)
  (let ((infos (cdr (assoc (car ast) primitives))))
    (assert (or (not (car infos)) ;; nb args and types are not fixed
                (and (>= (length (cdr ast))
                         (cadr (assoc (car ast) primitives)))
                     (<= (length (cdr ast))
                         (caddr (assoc (car ast) primitives)))))
            ERR_WRONG_NUM_ARGS)))

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
                 ((member op '(breakpoint)) (mlc-special ast succ))
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
                 ;; Binding
                 ((eq? op 'let) (mlc-let ast succ)) ;; Also handles let* (let* is a macro)
                 ((eq? op 'letrec) (mlc-letrec ast succ))
                 ;; Operator num
                 ((member op '(FLOAT+ FLOAT- FLOAT* FLOAT/ FLOAT< FLOAT> FLOAT<= FLOAT>= FLOAT=))
                  (let ((generic-op (list->symbol (list-tail (symbol->list op) 5))))
                    (gen-ast (cons generic-op (cdr ast))
                             succ)))
                 ((member op '(+ - * < > <= >= = /))         (mlc-op-n ast succ op)) ;; nary operator
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
;; Make lazy code from num/bool/char/null literal
;;
(define (mlc-literal ast succ)
  (if (and (number? ast)
           (or (>= ast (expt 2 61))
               (<  ast (* -1  (expt 2 60)))))
    (mlc-flonum ast succ)
    (make-lazy-code
      (lambda (cgc ctx)
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
          (apply-moves cgc ctx moves)
          (codegen-literal cgc ast reg)
          (jump-to-version cgc
                           succ
                           (ctx-push ctx
                                     (cond ((integer? ast) CTX_INT)
                                           ((boolean? ast) CTX_BOOL)
                                           ((char? ast)    CTX_CHAR)
                                           ((null? ast)    CTX_NULL)
                                           (else (error ERR_INTERNAL)))
                                     reg)))))))

;;
;; Make lazy code from flonum literal
;;
(define (mlc-flonum ast succ)
  (make-lazy-code
      (lambda (cgc ctx)
        (mlet ((immediate
                (if (< ast 0)
                    (let* ((ieee-rep (ieee754 (abs ast) 'double))
                           (64-mod   (bitwise-not (- ieee-rep 1)))
                           (64-modl  (bitwise-and (- (expt 2 63) 1) 64-mod)))
                      (* -1 64-modl))
                    (ieee754 ast 'double)))
               (moves/reg/ctx (ctx-get-free-reg ctx)))
          (apply-moves cgc ctx moves)
          (codegen-flonum cgc immediate reg)
          (jump-to-version cgc succ (ctx-push ctx CTX_FLO reg))))))

;;
;; Make lazy code from symbol literal
;;
(define (mlc-symbol ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
        (apply-moves cgc ctx moves)
        (codegen-symbol cgc ast reg)
        (jump-to-version cgc succ (ctx-push ctx CTX_SYM reg))))))

;;
;; Make lazy code from vector literal
;;
;; TODO regalloc: comment
(define (mlc-vector ast succ)

  (define (gen-set cgc ctx lidx)
    (let* ((lval (ctx-get-loc ctx lidx))
           (opval (codegen-loc-to-x86opnd (ctx-fs ctx) lval)))
      (if (ctx-loc-is-memory? lval)
          (begin (x86-mov cgc (x86-rax) opval)
                 (set! opval (x86-rax))))
      (x86-mov cgc (x86-mem (+ (* lidx 8) 16) alloc-ptr) opval)))

  (define lazy-vector
    (make-lazy-code
      (lambda (cgc ctx)
        (let loop ((pos 0))
          (if (= pos (vector-length ast))
              (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
                (apply-moves cgc ctx moves)
                (x86-lea cgc (codegen-reg-to-x86reg reg) (x86-mem TAG_MEMOBJ alloc-ptr))
                (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx (vector-length ast)) CTX_VECT reg)))
              (begin
                (gen-set cgc ctx pos)
                (loop (+ pos 1))))))))


  (define lazy-alloc
    (make-lazy-code
      (lambda (cgc ctx)
        (let ((header-word (mem-header (+ 2 (vector-length ast)) STAG_VECTOR)))
          ;; Allocate array in alloc-ptr
          (gen-allocation cgc #f STAG_VECTOR (+ (vector-length ast) 2))
          ;; Write header
          (x86-mov cgc (x86-rax) (x86-imm-int header-word))
          (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
          ;; Write length
          (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (vector-length ast))))
          (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
          (jump-to-version cgc lazy-vector ctx)))))


  (gen-ast-l (reverse (vector->list ast)) lazy-alloc))

;;
;; Make lazy code from string literal
;;
(define (mlc-string ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
        (apply-moves cgc ctx moves)
        (codegen-string cgc ast reg)
        (jump-to-version cgc succ (ctx-push ctx CTX_STR reg))))))

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

;;-----------------------------------------------------------------------------
;; VARIABLES GET

;;
;; Make lazy code from SYMBOL
;;
;; TODO: réécrire: attention la recherche d'id met à jour le ctx
(define (mlc-identifier ast succ)

  (make-lazy-code
    (lambda (cgc ctx)

      (let ((local  (assoc ast (ctx-env ctx)))
            (global (table-ref globals ast #f)))

        ;;
        (cond ;; Identifier is a free variable
              ((and local (eq? (identifier-kind (cdr local)) 'free))
                (gen-get-freevar cgc ctx local succ #f))
              ;; Identifier is a local variable
              (local
                (gen-get-localvar cgc ctx local succ #f))
              ;; Identifier is a global variable
              (global
                (gen-get-globalvar cgc ctx global succ))
              ;; Primitive
              ((assoc ast primitives) =>
                 (lambda (r)
                   (let ((args (build-list (cadr r) (lambda (x) (string->symbol (string-append "arg" (number->string x)))))))
                     (jump-to-version
                       cgc
                       (gen-ast `(lambda ,args (,ast ,@args)) succ)
                       ctx))))
              (else (gen-error cgc (ERR_UNKNOWN_VAR ast))))))))

;; TODO: merge with gen-get-localvar, it's now the same code!
(define (gen-get-freevar cgc ctx local succ for-set?)

  (let ((loc (ctx-identifier-loc ctx (cdr local)))
        (type (ctx-identifier-type ctx (cdr local))))

    (cond ((ctx-loc-is-register? loc)
             (if for-set?
                 (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (ctx-fs ctx) loc))
                 (jump-to-version cgc succ (ctx-push ctx type loc (car local)))))
          ((ctx-loc-is-memory? loc)
             (if for-set?
                 (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd loc))
                 (mlet ((moves/reg/nctx (ctx-get-free-reg ctx)))
                   (apply-moves cgc nctx moves)
                   (apply-moves cgc nctx (list (cons loc reg)))
                   (jump-to-version cgc succ (ctx-push nctx type reg (car local))))))
          ((ctx-loc-is-freemem? loc)
             (if for-set?
                 (let ((fs (ctx-fs ctx)))
                   (x86-mov cgc (x86-rax) (x86-mem (* 8 (- fs 2)) (x86-rsp)))
                   (x86-mov cgc (x86-rax) (x86-mem (- (* 8 (+ (cdr loc) 2)) TAG_MEMOBJ) (x86-rax))))
                 (mlet ((moves/reg/nctx (ctx-get-free-reg ctx)))
                   (apply-moves cgc nctx moves)
                   (let ((fs (ctx-fs nctx)))
                     (x86-mov cgc (x86-rax) (x86-mem (* 8 (- fs 2)) (x86-rsp))) ;; Get closure
                     (x86-mov
                       cgc
                       (codegen-reg-to-x86reg reg)
                       (x86-mem (- (* 8 (+ (cdr loc) 2)) TAG_MEMOBJ) (x86-rax))))
                   (jump-to-version cgc succ (ctx-push nctx type reg (car local)))))))))

;; TODO: + utiliser un appel récursif comme pour gen-get-freevar (??)
;; TODO coment: si mobject? est vrai, c'est qu'on veut le mobject dans le tmp reg (rax)
(define (gen-get-localvar cgc ctx local succ for-set?)

  (let ((loc (ctx-identifier-loc ctx (cdr local)))
        (type (if (ctx-identifier-mutable? ctx (cdr local))
                  CTX_UNK
                  (ctx-identifier-type ctx (cdr local)))))

    (if for-set?
        (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (ctx-fs ctx) loc))
        (if (ctx-loc-is-register? loc)
            ;;
            (jump-to-version cgc succ (ctx-push ctx type loc (car local)))
            ;;
            (mlet ((moves/reg/nctx (ctx-get-free-reg ctx)))
              (apply-moves cgc nctx moves)
              (apply-moves cgc nctx (list (cons loc reg)))
              (jump-to-version cgc succ (ctx-push nctx type reg (car local))))))))

(define (gen-get-globalvar cgc ctx global succ)

  (mlet (;; Get variable type if known
         (r (table-ref gids (car global) #f))
         (type (or r CTX_UNK))
         ;; Get free register (dest)
         (moves/reg/ctx (ctx-get-free-reg ctx)))
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

(define (gen-set-localvar cgc ctx local succ)

  ;; Get mobject in tmp register
  (gen-get-localvar cgc ctx local #f #t)

  ;;
  (mlet ((moves/reg/ctx (ctx-get-free-reg ctx))
         (lval (ctx-get-loc ctx 0))
         (type (ctx-get-type ctx 0)))
    (apply-moves cgc ctx moves)
    (let ((dest (codegen-reg-to-x86reg reg))
          (opval (codegen-loc-to-x86opnd (ctx-fs ctx) lval)))
      (if (ctx-loc-is-memory? lval)
          (begin (x86-mov cgc dest opval)
                 (set! opval dest)))
      (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) opval)
      (x86-mov cgc dest (x86-imm-int ENCODING_VOID))
      (let* ((ctx (ctx-push (ctx-pop ctx) CTX_VOID reg))
             (ctx (ctx-set-type ctx local type))) ;; TODO regalloc unk
        (jump-to-version cgc succ ctx)))))

;; TODO Merge with gen-set-localvar
(define (gen-set-freevar cgc ctx local succ)

  ;; Get mobject in tmp register
  (gen-get-freevar cgc ctx local #f #t)

  ;;
  (mlet ((moves/reg/ctx (ctx-get-free-reg ctx))
         (lval (ctx-get-loc ctx 0))
         (type (ctx-get-type ctx 0)))
    (apply-moves cgc ctx moves)
    (let ((dest (codegen-reg-to-x86reg reg))
          (opval (codegen-loc-to-x86opnd (ctx-fs ctx) lval)))
      (if (ctx-loc-is-memory? lval)
          (begin (x86-mov cgc dest opval)
                 (set! opval dest)))
      (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) opval)
      (x86-mov cgc dest (x86-imm-int ENCODING_VOID))
      (let* ((ctx (ctx-push (ctx-pop ctx) CTX_VOID reg))
             (ctx (ctx-set-type ctx local type))) ;; TODO regalloc unk
        (jump-to-version cgc succ ctx)))))

(define (gen-set-globalvar cgc ctx global succ)
  (mlet ((pos (cdr global))
         (moves/reg/ctx (ctx-get-free-reg ctx))
         (lval (ctx-get-loc ctx 0)))
    (apply-moves cgc ctx moves)
    (codegen-set-global cgc reg pos lval (ctx-fs ctx))
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID reg))))

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
                               (moves/reg/ctx (ctx-get-free-reg ctx))
                               (lvalue (ctx-get-loc ctx 0)))
                          (apply-moves cgc ctx moves)
                          (codegen-define-bind cgc (ctx-fs ctx) pos reg lvalue)
                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID reg))))))
         (lazy-val (gen-ast (caddr ast) lazy-bind)))

    (make-lazy-code
      (lambda (cgc ctx)
        ;; TODO regalloc: this codegen is useless ? Just update globals set?
        (codegen-define-id cgc)
        (table-set! globals identifier (cons identifier nb-globals))
        (set! nb-globals (+ nb-globals 1))
        (jump-to-version cgc lazy-val ctx)))))

;;
;; Make lazy code from LAMBDA
;;

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

                       ;; 1 - Get clean stack size (higher mx in ctx)
                       (let* ((clean-nb (- (ctx-fs ctx) 1))
                              (lres (ctx-get-loc ctx 0))
                              (opres (codegen-loc-to-x86opnd (ctx-fs ctx) lres))
                              (mutable? (ctx-is-mutable? ctx 0)))

                         ;; 2 - Move res in rax
                         ;; TODO regalloc2
                         (if mutable?
                             (if (ctx-loc-is-memory? lres)
                                 (begin (x86-mov cgc (x86-rax) opres)
                                        (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                                 (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opres)))
                             (x86-mov cgc (x86-rax) opres))

                         ;; 3 - Clean stack
                         (x86-add cgc (x86-rsp) (x86-imm-int (* 8 clean-nb)))

                         (if opt-return-points
                             (let* ((ret-type (car (ctx-stack ctx)))
                                    (crtable-offset (type-to-cridx ret-type)))
                               (codegen-return-cr cgc crtable-offset))
                             (codegen-return-rp cgc)))
                       ;; 4 - ret seq
                       )))
         ;; Lazy lambda body
         (lazy-body (gen-ast (caddr ast) lazy-ret))
         ;; Lazy function prologue : creates rest param if any, transforms mutable vars, ...
         (lazy-prologue (get-lazy-prologue ast lazy-body rest-param mvars))
         ;; Same as lazy-prologue but generate a generic prologue (no matter what the arguments are)
         (lazy-prologue-gen (get-lazy-generic-prologue ast lazy-body rest-param mvars (length params))))

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
                                                       (error "NYI1")
                                                       (let* ((cctx (ctx-init-fn sctx ctx all-params fvars mvars))
                                                              (stack
                                                                (append (make-list (length all-params) CTX_UNK)
                                                                        (list CTX_CLO CTX_RETAD)))
                                                              (gctx (ctx-copy cctx stack))) ;; To handle rest param
                                                         (gen-version-fn ast closure lazy-prologue-gen gctx cctx #f)))

                                                    ;; CASE 2 - Do not use multiple entry points
                                                    ((= selector 1)
                                                        (let ((ctx (ctx-init-fn sctx ctx all-params fvars mvars)))
                                                          (gen-version-fn ast closure lazy-prologue-gen ctx ctx #t)))

                                                    ;; CASE 3 - Use multiple entry points AND limit is not reached or there is no limit
                                                    (else
                                                       (let ((ctx (ctx-init-fn sctx ctx all-params fvars mvars)))
                                                         (gen-version-fn ast closure lazy-prologue ctx ctx #f)))))))

               (stub-addr (vector-ref (list-ref stub-labels 0) 1))
               (generic-addr (vector-ref (list-ref stub-labels 1) 1)))

          ;; Get free vars from ast
          (set! fvars (free-vars
                        (caddr ast)
                        all-params
                        (map car (ctx-env ctx))))

          ;; If 'stats' option, then inc closures slot
          (if opt-stats
            (gen-inc-slot cgc 'closures))

          ;; Create closure
          (codegen-closure-create cgc (length fvars))

          ;; Write entry point or cctable location
          (if opt-entry-points
              ;; If opt-entry-points generate a closure using cctable
              (let* ((cctable-key (get-cctable-key ast ctx fvars))
                     (cctable     (get-cctable ast cctable-key stub-addr generic-addr))
                     (cctable-loc (- (obj-encoding cctable) 1)))
                (codegen-closure-cc cgc cctable-loc))
              ;; Else, generate a closure using a single entry point
              (let ((ep-loc (get-entry-points-loc ast stub-addr)))
                (codegen-closure-ep cgc ep-loc)))

          ;; Write free variables
          (gen-free-vars cgc fvars ctx 0)

          (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
            (apply-moves cgc ctx moves)

            ;; Put closure
            (codegen-closure-put cgc reg)

            ;; Trigger the next object
            (if opt-propagate-functionid
                (error "NYI - mlc-lambda: Use (CTX_CLOi with cctable or closure id)")
                (jump-to-version cgc succ (ctx-push ctx CTX_CLO reg)))))))))

;; TODO CLEAN raglloc args regs
(define (get-lazy-generic-prologue ast succ rest-param mvars nb-formal)
  (make-lazy-code-entry
    (lambda (cgc ctx)
      (let ((nb-args (ctx-nb-args ctx))
            (label-next (asm-make-label #f (new-sym 'label-next)))
            (label-end (asm-make-label #f (new-sym 'label-end))))
        (if (not rest-param)
            ;; If not rest, only check args number
            (begin
              (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args)))
              (x86-je cgc label-end)
              (gen-error cgc "TODO ERR1")
              (x86-label cgc label-end))
            ;; If rest, check args number then build rest list from regs and stack
            (let ((nb-args-regs (length args-regs))
                  (label-rest       (asm-make-label #f (new-sym 'prologue-rest)))
                  (label-rest-loop  (asm-make-label #f (new-sym 'prologue-rest-loop)))
                  (label-rest-end   (asm-make-label #f (new-sym 'prologue-rest-end)))
                  (label-next-arg   (asm-make-label #f (new-sym 'prologue-next-arg)))
                  (label-from-stack (asm-make-label #f (new-sym 'prologue-from-stack)))
                  (label-next-arg-end (asm-make-label #f (new-sym 'prologue-next-arg-end))))
              (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (- nb-args 1))))
              (x86-jge cgc label-rest)
                (gen-error cgc "TODO ERR2")
              ;; GET NEXT ARG PART
              (x86-label cgc label-next-arg)
                (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (length args-regs))))
                (x86-jg cgc label-from-stack)
                (let loop ((i (length args-regs))
                           (regs (reverse args-regs)))
                  (if (> i 0)
                      (begin
                          (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding i)))
                          (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (ctx-fs ctx) (car regs)))
                          (x86-je cgc label-next-arg-end)
                          (loop (- i 1) (cdr regs)))))
                (x86-jmp cgc label-next-arg-end)
                (x86-label cgc label-from-stack)
                (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))
                (x86-mov cgc (x86-r13) (x86-mem 0 (x86-rsp)))
                (x86-add cgc (x86-rsp) (x86-imm-int 16))
                (x86-sub cgc (x86-rdi) (x86-imm-int (obj-encoding 1)))
                (x86-jmp cgc (x86-r13))
                (x86-label cgc label-next-arg-end)
                (x86-sub cgc (x86-rdi) (x86-imm-int (obj-encoding 1)))
                (x86-ret cgc)
              ;; END GET NEXT ARG PART

              (x86-label cgc label-rest)
              ;; cdr (rax) = '()
              (x86-mov cgc (x86-r14) (x86-imm-int (obj-encoding '())))
              (x86-label cgc label-rest-loop)
              (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (- nb-args 1))))
              (x86-je cgc label-rest-end)
              (let ((header-word (mem-header 3 STAG_PAIR)))
                ;; Alloc
                (gen-allocation cgc #f STAG_PAIR 3)
                (x86-mov cgc (x86-rax) (x86-imm-int header-word))
                (x86-mov cgc (x86-mem  0 alloc-ptr) (x86-rax))
                (x86-call cgc label-next-arg)
                (x86-mov cgc (x86-mem  8 alloc-ptr) (x86-rax))
                (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-r14))
                (x86-lea cgc (x86-r14) (x86-mem TAG_MEMOBJ alloc-ptr))
                (x86-jmp cgc label-rest-loop))
              ;
              (x86-label cgc label-rest-end)
              (if (< nb-args (length args-regs))
                  (let ((reg (list-ref args-regs (- nb-args 1))))
                    (x86-mov cgc (codegen-reg-to-x86reg reg) (x86-r14)))
                  (x86-push cgc (x86-r14)))))

        (gen-mutable cgc ctx mvars)
        (jump-to-version cgc succ ctx)))))

;; Create and return a lazy prologue
(define (get-lazy-prologue ast succ rest-param mvars)
  (make-lazy-code-entry
    (lambda (cgc ctx)
      (let* ((nb-actual (- (length (ctx-stack ctx)) 2))
             (nb-formal (ctx-nb-args ctx)))
        (cond ;; rest AND actual == formal
              ((and rest-param (= nb-actual (- nb-formal 1))) ;; -1 rest
               (set! ctx (ctx-stack-push ctx CTX_NULL))
               (let ((reg
                       (if (<= nb-formal (length args-regs))
                           (list-ref args-regs (- nb-formal 1))
                           #f)))
                 (codegen-prologue-rest= cgc reg)
                 (gen-mutable cgc ctx mvars)
                 (jump-to-version cgc succ ctx)))
              ;; rest AND actual > formal
              ;; TODO merge > and == (?)
              ((and rest-param (> nb-actual (- nb-formal 1)))
               (let* ((nb-extra (- nb-actual (- nb-formal 1)))
                      (nctx (ctx-stack-pop-n ctx (- nb-extra 1)))
                      (nctx (ctx-set-type nctx 0 CTX_PAI)))
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

                 (gen-mutable cgc ctx mvars)
                 (jump-to-version cgc succ ctx)))
              ;; (rest AND actual < formal) OR (!rest AND actual < formal) OR (!rest AND actual > formal)
              ((or (< nb-actual nb-formal) (> nb-actual nb-formal))
               (gen-error cgc ERR_WRONG_NUM_ARGS))
              ;; Else, nothing to do
              (else
                 (gen-mutable cgc ctx mvars)
                 (jump-to-version cgc succ ctx)))))))

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

;; TODO regalloc: merge with mlc-letrec (body lazy-out)
;; TODO + use mlc-binding function for mlc-let and mlc-letrec ?
(define (mlc-let ast succ)

  (define (build-id-idx ids l)
    (if (null? ids)
        '()
        (cons (cons (car ids) l)
              (build-id-idx (cdr ids) (- l 1)))))

  (let* ((ids (map car (cadr ast)))
         (values (map cadr (cadr ast)))
         (body   (cddr ast))
         (lazy-out
           (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                           make-lazy-code-ret
                           make-lazy-code)))
             (make-lc
               (lambda (cgc ctx)
                 (let* ((type (car (ctx-stack ctx)))
                        (loc  (ctx-get-loc ctx 0))
                        (mutable? (ctx-is-mutable? ctx 0))
                        (ctx  (ctx-unbind-locals ctx ids))
                        (ctx  (ctx-pop-n ctx (+ (length ids) 1)))
                        (ctx  (ctx-push ctx type loc)))
                   ;; NOTE see lazy-out comment in mlc-letrec
                   ;; Merge all code returning a boxed value
                   (if mutable?
                       (let ((opnd (codegen-loc-to-x86opnd (ctx-fs ctx) loc)))
                        (if (ctx-loc-is-memory? loc)
                            (begin (x86-mov cgc (x86-rax) opnd)
                                   (x86-mov cgc opnd (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                            (x86-mov cgc opnd (x86-mem (- 8 TAG_MEMOBJ) opnd)))))
                   (jump-to-version cgc succ ctx))))))
         (lazy-body
           (gen-ast (cons 'begin body) lazy-out))
         (lazy-binds
           (make-lazy-code
             (lambda (cgc ctx)
               (unbox-mutable cgc ctx 0 (length ids))
               (let* ((id-idx (build-id-idx ids (- (length ids) 1)))
                      (mvars (mutable-vars (cddr ast) ids)) ;; Get mutable vars
                      (ctx (ctx-bind-locals ctx id-idx mvars)))
                 (gen-mutable cgc ctx mvars)
                 (jump-to-version cgc lazy-body ctx))))))
   (gen-ast-l values lazy-binds)))

;; TODO
(define (unbox-mutable cgc ctx idx-start idx-lim)
  (if (< idx-start idx-lim)
      (if (ctx-is-mutable? ctx idx-start)
          (let* ((loc (ctx-get-loc ctx idx-start))
                 (opnd (codegen-loc-to-x86opnd (ctx-fs ctx) loc)))
            (if (ctx-loc-is-memory? loc)
                (begin (x86-mov cgc (x86-rax) opnd)
                       (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
                       (x86-mov cgc opnd (x86-rax)))
                (x86-mov cgc opnd (x86-mem (- 8 TAG_MEMOBJ) opnd))))
          (unbox-mutable cgc ctx (+ idx-start 1) idx-lim))))
;; TODO

(define (mlc-letrec ast succ)

  (define (alloc cgc ids ctx stack-types)
    (if (null? ids)
        ctx
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
          (apply-moves cgc ctx moves)
          (let ((header-word (mem-header 2 STAG_MOBJECT))
                (dest (codegen-reg-to-x86reg reg)))
            ;; Alloc code
            (gen-allocation cgc #f STAG_MOBJECT 2)
            ;; Write variable
            (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
            (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
            ;; Write header
            (x86-mov cgc (x86-rax) (x86-imm-int header-word))
            (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
            ;; Replace local
            (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))
            (alloc cgc (cdr ids) (ctx-push ctx (car stack-types) reg) (cdr stack-types))))))

  (if (and (not (contains (map cadr (cadr ast)) 'lambda))
           (not (contains (map cadr (cadr ast)) 'set!)))
      ;; If letrec bind values do not contain 'lambda symbol, it's a let
      (gen-ast (cons 'let (cdr ast)) succ)
      (let* ((ids (map car (cadr ast)))
             (body (cddr ast))
             (lazy-out
               (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                               make-lazy-code-ret
                               make-lazy-code)))
                 (make-lc
                   (lambda (cgc ctx)
                     (let* ((type (car (ctx-stack ctx)))
                            (loc  (ctx-get-loc ctx 0))
                            (mutable? (ctx-is-mutable? ctx 0))
                            (ctx  (ctx-unbind-locals ctx ids))
                            (ctx  (ctx-pop-n ctx (+ (length ids) 1)))
                            (ctx  (ctx-push ctx type loc)))
                       ;; NOTE: if we now that res value is a variable, we lost this information
                       ;; because we return non mutable object
                       ;; TODO: lost information only if mutable?
                       (if mutable?
                           (let ((opnd (codegen-loc-to-x86opnd (ctx-fs ctx) loc)))
                            (if (ctx-loc-is-memory? loc)
                                (begin (x86-mov cgc (x86-rax) opnd)
                                       (x86-mov cgc opnd (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                                (x86-mov cgc opnd (x86-mem (- 8 TAG_MEMOBJ) opnd)))))
                       (jump-to-version cgc succ ctx))))))
             (lazy-set
               (make-lazy-code
                 (lambda (cgc ctx)
                   (let loop ((i 0)
                              (ctx ctx))
                     (if (= i (length ids))
                         (let ((ctx (ctx-pop-n ctx (length ids))))
                           (jump-to-version cgc (gen-ast-l body lazy-out) ctx))
                         (let ((lfrom (ctx-get-loc ctx i))
                               (mut-from? (ctx-is-mutable? ctx i))
                               (lto   (ctx-get-loc ctx (+ i (length ids))))
                               (ctx   (ctx-stack-move ctx i (+ i (length ids)))))

                           (codegen-letrec-set! cgc (ctx-fs ctx) lto lfrom mut-from?)

                           (loop (+ i 1) ctx)))))))
             (lazy-pre
               (make-lazy-code
                 (lambda (cgc ctx)
                   (let* ((mvars (mutable-vars (cdr ast) ids))
                          (stack-types
                            (foldr (lambda (el r)
                                     (if (and (pair? (cadr el))
                                              (eq? (caadr el) 'lambda)
                                              (not (member (car el) mvars)))
                                         (cons CTX_CLO r)    ;; Non mutable and lambda, keep the type
                                         (cons CTX_UNK r)))
                                   '()
                                   (cadr ast)))
                          (ctx (alloc cgc ids ctx stack-types))
                          (rids (reverse ids))
                          (bind-lst (build-list (length ids) (lambda (n) (cons (list-ref rids n) n)))))

                     (jump-to-version
                       cgc
                       (gen-ast-l (map cadr (cadr ast)) lazy-set)
                       (ctx-bind-locals ctx bind-lst mvars #t)))))))
       lazy-pre)))

;;-----------------------------------------------------------------------------
;; SPECIAL

;;
;; Make lazy code from special id $$print-flonum
;;
(define (mlc-printflonum ast succ)
  (let ((spec
          (make-lazy-code
            (lambda (cgc ctx)
              (mlet ((moves/reg/ctx (ctx-get-free-reg ctx))
                     (loc  (ctx-get-loc ctx 0))
                     (mut? (ctx-is-mutable? ctx 0)))
                (if mut?
                    (error "NYI mutable print-flonum"))
                (apply-moves cgc ctx moves)
                (codegen-print-flonum cgc (ctx-fs ctx) reg loc)
                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID reg)))))))
    (gen-ast (cadr ast) spec)))

;;
;; Make lazy code from SPECIAL FORM
;;
(define (mlc-special ast succ)
  (cond ((eq? (car ast) 'breakpoint)
         (make-lazy-code
           (lambda (cgc ctx)
             (gen-breakpoint cgc)
             (codegen-void cgc)
             (jump-to-version cgc succ (ctx-push ctx CTX_VOID)))))))

;;-----------------------------------------------------------------------------
;; PRIMITIVES

;; primitive not
(define (prim-not cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0))
        (mut-val? (ctx-is-mutable? ctx 0)))
    (codegen-not cgc (ctx-fs ctx) reg lval mut-val?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL reg))))

;; primitive eq?
(define (prim-eq? cgc ctx reg succ cst-infos)

  (let* ((lcst (assoc 0 cst-infos))
         (rcst (assoc 1 cst-infos))
         (lright (if rcst (cdr rcst) (ctx-get-loc ctx 0)))
         (lleft
           (if lcst
               (cdr lcst)
               (if rcst
                   (ctx-get-loc ctx 0)
                   (ctx-get-loc ctx 1))))
         (mutl? (and (not lcst) (ctx-is-mutable? ctx (if rcst 0 1))))
         (mutr? (and (not rcst) (ctx-is-mutable? ctx 0)))
         (n-pop (count (list lcst rcst) not)))
    (codegen-eq? cgc (ctx-fs ctx) reg lleft lright lcst rcst mutl? mutr?)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) CTX_BOOL reg))))

;; primitives car & cdr
(define (prim-cxr cgc ctx reg succ cst-infos op)
  (let ((lval (ctx-get-loc ctx 0))
        (mut-val? (ctx-is-mutable? ctx 0)))
    (codegen-car/cdr cgc (ctx-fs ctx) op reg lval mut-val?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_UNK reg))))

;; primitive eof-object?
(define (prim-eof-object? cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0))
        (mut-val? (ctx-is-mutable? ctx 0)))
    (codegen-eof? cgc (ctx-fs ctx) reg lval mut-val?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL reg))))

;; primitive read-char
(define (prim-read-char cgc ctx reg succ cst-infos)
  (let* ((lport (ctx-get-loc ctx 0))
         (mut-port? (ctx-is-mutable? ctx 0)))
    (codegen-read-char cgc (ctx-fs ctx) reg lport mut-port?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_CHAR reg))))

;; primitive write-char
(define (prim-write-char cgc ctx reg succ cst-infos)
  (let ((lchar (ctx-get-loc ctx 1))
        (lport (ctx-get-loc ctx 0))
        (mut-char? (ctx-is-mutable? ctx 1))
        (mut-port? (ctx-is-mutable? ctx 0)))
    (codegen-write-char cgc (ctx-fs ctx) reg lchar lport mut-char? mut-port?)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) CTX_VOID reg))))

;; primitive make-string
(define (prim-make-string cgc ctx reg succ cst-infos args)
  (let* ((init-value? (= (length args) 2))
         (llen (ctx-get-loc ctx (if init-value? 1 0)))
         (lval (if init-value? (ctx-get-loc ctx 0) #f))
         (mut-len? (ctx-is-mutable? ctx (if init-value? 1 0)))
         (mut-val? (and init-value? (ctx-is-mutable? ctx 0))))
    (codegen-make-string cgc (ctx-fs ctx) reg llen lval mut-len? mut-val?)
    (jump-to-version cgc succ (ctx-push (if init-value?
                                            (ctx-pop-n ctx 2)
                                            (ctx-pop ctx))
                                        CTX_STR
                                        reg))))

;; primitive make-vector
(define (prim-make-vector cgc ctx reg succ cst-infos args)
  (let* ((init-value? (= (length args) 2))
         (llen (ctx-get-loc ctx (if init-value? 1 0)))
         (lval (if init-value? (ctx-get-loc ctx 0) #f))
         (mut-len? (ctx-is-mutable? ctx (if init-value? 1 0)))
         (mut-val? (and init-value? (ctx-is-mutable? ctx 0))))
    (codegen-make-vector cgc (ctx-fs ctx) reg llen lval mut-len? mut-val?)
    (jump-to-version cgc succ (ctx-push (if init-value?
                                            (ctx-pop-n ctx 2)
                                            (ctx-pop ctx))
                                        CTX_VECT
                                        reg))))

;; primitive symbol->string
(define (prim-symbol->string cgc ctx reg succ cst-infos)
  (let ((lsym (ctx-get-loc ctx 0))
        (mut-sym? (ctx-is-mutable? ctx 0)))
    (codegen-sym->str cgc (ctx-fs ctx) reg lsym mut-sym?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_STR reg))))

;; primitive string->symbol
(define (prim-string->symbol cgc ctx reg succ cst-infos)
  (let ((lstr (ctx-get-loc ctx 0))
        (mut-str? (ctx-is-mutable? ctx 0)))
    (codegen-str->sym cgc (ctx-fs ctx) reg lstr mut-str?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_SYM reg))))

;; primitive vector-ref
(define (prim-vector-ref cgc ctx reg succ cst-infos)

  (let* ((poscst (assoc 1 cst-infos))
         (lidx (if poscst (cdr poscst) (ctx-get-loc ctx 0)))
         (lvec
           (if poscst
               (ctx-get-loc ctx 0)
               (ctx-get-loc ctx 1)))
         (idx-mut? (and (not poscst) (ctx-is-mutable? ctx 0)))
         (vec-mut? (ctx-is-mutable? ctx (if poscst 0 1)))
         (n-pop (if poscst 1 2)))

    (codegen-vector-ref cgc (ctx-fs ctx) reg lvec lidx poscst idx-mut? vec-mut?)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) CTX_UNK reg))))

;; primitive string-ref
(define (prim-string-ref cgc ctx reg succ cst-infos)

  (let* ((poscst (assoc 1 cst-infos))
         (lidx (if poscst (cdr poscst) (ctx-get-loc ctx 0)))
         (lstr
           (if poscst
               (ctx-get-loc ctx 0)
               (ctx-get-loc ctx 1)))
         (idx-mut? (and (not poscst) (ctx-is-mutable? ctx 0)))
         (str-mut? (ctx-is-mutable? ctx (if poscst 0 1)))
         (n-pop (if poscst 1 2)))
    (codegen-string-ref cgc (ctx-fs ctx) reg lstr lidx poscst idx-mut? str-mut?)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) CTX_CHAR reg))))

;; primitive vector-set!
(define (prim-vector-set! cgc ctx reg succ cst-infos)
  (let ((lval (ctx-get-loc ctx 0))
        (lidx (ctx-get-loc ctx 1))
        (lvec (ctx-get-loc ctx 2)))
    (if (or (ctx-is-mutable? ctx 0)
            (ctx-is-mutable? ctx 1)
            (ctx-is-mutable? ctx 2))
        (error "NYI"))
    (codegen-vector-set! cgc (ctx-fs ctx) reg lvec lidx lval)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 3) CTX_VOID reg))))

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
         (mut-chr? (and (not chr-cst) (ctx-is-mutable? ctx 0)))
         (mut-idx? (and (not idx-cst) (ctx-is-mutable? ctx (if chr-cst 0 1))))
         (n-pop (+ (count (list idx-cst chr-cst) not) 1))
         (lstr (ctx-get-loc ctx (- n-pop 1)))
         (mut-str? (ctx-is-mutable? ctx (- n-pop 1))))
    (codegen-string-set! cgc (ctx-fs ctx) reg lstr lidx lchr idx-cst chr-cst mut-str? mut-idx? mut-chr?)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) CTX_VOID reg))))

;; primitives set-car! & set-cdr!
(define (prim-set-cxr! cgc ctx reg succ cst-infos op)

  (let* ((valcst (assoc 1 cst-infos))
         (lval  (if valcst (cdr valcst) (ctx-get-loc ctx 0)))
         (lpair
           (if valcst
               (ctx-get-loc ctx 0)
               (ctx-get-loc ctx 1)))
         (mut-val?  (and (not valcst) (ctx-is-mutable? ctx 0)))
         (mut-pair? (ctx-is-mutable? ctx (if valcst 0 1)))
         (n-pop (if valcst 1 2)))
    (codegen-scar/scdr cgc (ctx-fs ctx) op reg lpair lval valcst mut-val? mut-pair?)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) CTX_VOID reg))))

;; primitives current-input-port & current-output-port
(define (prim-current-x-port cgc ctx reg succ cst-infos op)
  (codegen-current-io-port cgc op reg)
  (jump-to-version cgc succ (ctx-push ctx
                                      (if (eq? op 'current-output-port)
                                          CTX_OPORT
                                          CTX_IPORT)
                                      reg)))

;; primitive close-input-port & close-output-port
(define (prim-close-x-port cgc ctx reg succ cst-infos op)
  (let ((lport (ctx-get-loc ctx 0))
        (mut-port? (ctx-is-mutable? ctx 0)))
    (codegen-close-io-port cgc (ctx-fs ctx) reg lport mut-port?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID reg))))

;; primitives open-input-file & open-output-file
(define (prim-open-x-file cgc ctx reg succ cst-infos op)
  (let ((lstr (ctx-get-loc ctx 0))
        (mut-str? (ctx-is-mutable? ctx 0)))
    (codegen-open-io-file cgc (ctx-fs ctx) op reg lstr mut-str?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx)
                                        (if (eq? op 'open-input-file)
                                            CTX_IPORT
                                            CTX_OPORT)
                                        reg))))

;; primitives char->integer & integer->char
(define (prim-char<->int cgc ctx reg succ cst-infos op)
  (let* ((cst-arg (assoc 0 cst-infos))
         (lval
          (if cst-arg
              (cdr cst-arg)
              (ctx-get-loc ctx 0)))
         (mut-val? (if cst-arg #f (ctx-is-mutable? ctx 0)))
         (n-pop (if cst-arg 0 1)))
    (codegen-ch<->int cgc (ctx-fs ctx) op reg lval cst-arg mut-val?)
    (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop)
                                        (if (eq? op 'char->integer)
                                            CTX_INT
                                            CTX_CHAR)
                                        reg))))

;; primitives vector-length & string-length
(define (prim-x-length cgc ctx reg succ cst-infos op)
  (let ((lval (ctx-get-loc ctx 0))
        (mut-val? (ctx-is-mutable? ctx 0)))
    (codegen-vec/str-length cgc (ctx-fs ctx) reg lval mut-val?)
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_INT reg))))

;;
(define (mlc-primitive ast succ)

  ;; Adjust args for some primitives
  (if (and (eq? (car ast) 'write-char)
           (= (length ast) 2))
      (set! ast (append ast '((current-output-port)))))

  ;; Assert primitive nb args
  (assert-p-nbargs ast)

  ;;
  (let* ((cst-infos (get-prim-cst-infos ast))
         (lazy-primitive
           (cond
             ((eq? (car ast) 'exit) (get-lazy-error ""))
             ((eq? (car ast) 'cons) (mlc-pair succ cst-infos))
             (else
               (make-lazy-code
                 (lambda (cgc ctx)
                   (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
                     (apply-moves cgc ctx moves)
                     (case (car ast)
                       ((not)            (prim-not            cgc ctx reg succ cst-infos))
                       ((eq?)            (prim-eq?            cgc ctx reg succ cst-infos))
                       ((char=?)         (prim-eq?            cgc ctx reg succ cst-infos))
                       ((car cdr)        (prim-cxr            cgc ctx reg succ cst-infos (car ast)))
                       ((eof-object?)    (prim-eof-object?    cgc ctx reg succ cst-infos))
                       ((read-char)      (prim-read-char      cgc ctx reg succ cst-infos))
                       ((write-char)     (prim-write-char     cgc ctx reg succ cst-infos))
                       ((make-string)    (prim-make-string    cgc ctx reg succ cst-infos (cdr ast)))
                       ((make-vector)    (prim-make-vector    cgc ctx reg succ cst-infos (cdr ast)))
                       ((string->symbol) (prim-string->symbol cgc ctx reg succ cst-infos))
                       ((symbol->string) (prim-symbol->string cgc ctx reg succ cst-infos))
                       ((vector-ref)     (prim-vector-ref     cgc ctx reg succ cst-infos))
                       ((string-ref)     (prim-string-ref     cgc ctx reg succ cst-infos))
                       ((vector-set!)    (prim-vector-set!    cgc ctx reg succ cst-infos))
                       ((string-set!)    (prim-string-set!    cgc ctx reg succ cst-infos))
                       ((set-car! set-cdr!)                      (prim-set-cxr!       cgc ctx reg succ cst-infos (car ast)))
                       ((current-input-port current-output-port) (prim-current-x-port cgc ctx reg succ cst-infos (car ast)))
                       ((close-input-port close-output-port)     (prim-close-x-port   cgc ctx reg succ cst-infos (car ast)))
                       ((open-input-file open-output-file)       (prim-open-x-file    cgc ctx reg succ cst-infos (car ast)))
                       ((char->integer integer->char)            (prim-char<->int     cgc ctx reg succ cst-infos (car ast)))
                       ((vector-length string-length)            (prim-x-length       cgc ctx reg succ cst-infos (car ast)))
                       (else (error "Unknown primitive"))))))))))

    (let* ((primitive (assoc (car ast) primitives))
           ;; Get list of types required by this primitive
           (types (if (cadr primitive)
                      (cdr (assoc (length (cdr ast))
                                  (cadddr primitive)))
                      (build-list (length (cdr ast)) (lambda (el) CTX_ALL)))))

      (assert (= (length types)
                 (length (cdr ast)))
              "Primitive error")

      ;; Build args lco chain with type checks
      (check-types types (cdr ast) lazy-primitive ast cst-infos))))

;; TODO WIP
(define (get-prim-cst-infos ast)

  (define (get-prim-cst-infos-h args cst-positions curr-pos)
    (if (or (null? args)
            (null? cst-positions))
        '()
        (if (eq? curr-pos (car cst-positions))
            (if (or (integer? (car args))
                    (boolean? (car args))
                    (char?    (car args))
                    (null?    (car args)))
                (cons (cons curr-pos (car args))
                      (get-prim-cst-infos-h (cdr args) (cdr cst-positions) (+ curr-pos 1)))
                (get-prim-cst-infos-h (cdr args) (cdr cst-positions) (+ curr-pos 1)))
            (get-prim-cst-infos-h (cdr args) cst-positions (+ curr-pos 1)))))

  (let ((primitive (assoc (car ast) primitives)))
    (get-prim-cst-infos-h
      (cdr ast)
      (cadddr (cdr primitive))
      0)))

;; Build lazy objects chain of 'args' list
;; and insert type check for corresponding 'types'
(define (check-types types args succ ast #!optional (cst-infos '()))

  (define (check-cst-type type cst)
    (cond
      ((eq? type CTX_INT) (integer? cst))
      ((eq? type CTX_FLO) (flonum? cst))
      ((eq? type CTX_NULL) (null? cst))
      ((eq? type CTX_BOOL) (boolean? cst))
      ((eq? type CTX_CHAR) (char? cst))
      (else #f)))

  (define (check-types-h types args curr-pos)

    (if (null? types)
        succ
        (let* ((lazy-next (check-types-h (cdr types) (cdr args) (+ curr-pos 1)))
               (r (assoc curr-pos cst-infos)))
          (cond ;;
                ((or (and r (eq? (car types) CTX_ALL))
                     (and r (check-cst-type (car types) (cdr r))))
                   lazy-next)
                ;;
                (r
                   (get-lazy-error "NYI ERROR WRONG TYPE"))
                ;;
                ((eq? (car types) CTX_ALL)
                   (gen-ast (car args) lazy-next))
                ;;
                (else
                   (gen-ast (car args)
                            (gen-fatal-type-test (car types) 0 lazy-next ast)))))))

  (check-types-h types args 0))

;;-----------------------------------------------------------------------------
;; Conditionals

;;
;; Make lazy code from IF
;;
(define (mlc-if ast succ)

  (let* ((condition (cadr ast))
         (cleft  (and (pair? condition) (>= (length condition) 3) (cadr condition)))
         (cright (and (pair? condition) (>= (length condition) 3) (caddr condition)))
         (inline-condition?
           (and (pair? condition)
                (member (caadr ast) '(< > <= >= =))))
         (lazy-code0
           (gen-ast (cadddr ast) succ))
         (lazy-code1
           (gen-ast (caddr ast) succ))
         (lazy-code-test
           (make-lazy-code
             (lambda (cgc ctx)
               (let* ((n-pop (count (list cleft cright) (lambda (n) (not (integer? n)))))
                      (ctx0
                        (if inline-condition?
                          (ctx-pop-n ctx n-pop) ;; Pop both condition operands
                          (ctx-pop ctx)))   ;; Pop condition result

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

                 (let ((label-false (list-ref stub-labels 0))
                       (label-true  (list-ref stub-labels 1)))

                   (if inline-condition?
                       (let* ((lazy-cmp
                                (get-lazy-n-binop
                                  condition
                                  (caadr ast)
                                  (and (integer? cleft) cleft)
                                  (and (integer? cright) cright)
                                  succ
                                  (list label-jump label-true label-false))))
                         (jump-to-version cgc lazy-cmp ctx))
                       (let* ((lcond (ctx-get-loc ctx 0))
                              (mut-cond? (ctx-is-mutable? ctx 0)))
                         (codegen-if cgc (ctx-fs ctx) label-jump label-false label-true lcond mut-cond?)))))))))

    (if inline-condition?
        (cond ((< (length (cadr ast)) 3)
                 ;; if (op) or (op opnd) then inline the true branch
                 (gen-ast (caddr ast) succ))
              ((and inline-condition? (number? cleft) (number? cright))
                 ;; operands are constants, inline corresponding branch
                 (if (eval condition)
                     (gen-ast (caddr ast)  succ)
                     (gen-ast (cadddr ast) succ)))
              ((and inline-condition? (integer? cleft))
                 (gen-ast cright lazy-code-test))
              ((and inline-condition? (integer? cright))
                 (gen-ast cleft lazy-code-test))
              (else
                 (gen-ast-l (cdr condition) lazy-code-test)))
        (gen-ast
          (cadr ast)
          lazy-code-test))))

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

         (codegen-dispatch-imm cgc label-jump (list-ref stub-labels 0) (list-ref stub-labels 1) from-stack? cmp-val)))))

;;-----------------------------------------------------------------------------
;; APPLY & CALL

;;
;; Make lazy code from APPLY
;;
(define (mlc-apply ast succ)

  (let* ((lazy-call
          (make-lazy-code
            (lambda (cgc ctx)
              (x86-mov cgc (x86-rdi) (x86-r11)) ;; Copy nb args in rdi
              (gen-call-sequence ast cgc #f #f))))
        (lazy-args
          (make-lazy-code
            (lambda (cgc ctx)
              (let* ((label-end (asm-make-label #f (new-sym 'apply-end-args)))
                     (llst (ctx-get-loc ctx 0))
                     (oplst (codegen-loc-to-x86opnd (ctx-fs ctx) llst)))
                ;; r11, r14 & r15 are used as tmp registers
                ;; It is safe because they are not used for parameters.
                ;; And if they are used after, they already are saved on the stack
                (x86-mov cgc (x86-r15) oplst)
                (x86-mov cgc (x86-r11) (x86-imm-int 0))
                (let loop ((args-regs args-regs))
                  (if (null? args-regs)
                      (let ((label-loop (asm-make-label #f (new-sym 'apply-loop-args))))
                        (x86-label cgc label-loop)
                        (x86-cmp cgc (x86-r15) (x86-imm-int (obj-encoding '())))
                        (x86-je cgc label-end)
                          (x86-add cgc (x86-r11) (x86-imm-int 4))
                          (x86-mov cgc (x86-r14) (x86-mem (- 8 TAG_MEMOBJ) (x86-r15)))
                          (x86-push cgc (x86-r14))
                          (x86-mov cgc (x86-r15) (x86-mem (- 16 TAG_MEMOBJ) (x86-r15)))
                          (x86-jmp cgc label-loop))
                      (begin
                        (x86-cmp cgc (x86-r15) (x86-imm-int (obj-encoding '())))
                        (x86-je cgc label-end)
                          (x86-add cgc (x86-r11) (x86-imm-int 4))
                          (x86-mov cgc (codegen-loc-to-x86opnd (ctx-fs ctx) (car args-regs)) (x86-mem (- 8 TAG_MEMOBJ) (x86-r15)))
                          (x86-mov cgc (x86-r15) (x86-mem (- 16 TAG_MEMOBJ) (x86-r15)))
                        (loop (cdr args-regs)))))
                (x86-label cgc label-end)
                (jump-to-version cgc lazy-call ctx)))))
        (lazy-pre
          (make-lazy-code
            (lambda (cgc ctx)
              ;; Save used registers and update ctx
              (set! ctx (call-save-registers cgc ctx #f 2))

              ;; Generate continuation stub and push return address
              (set! ctx (call-gen-continuation cgc ctx #f ast succ #t))

              ;; Push closure
              (set! ctx (call-get-closure cgc ctx 1))
              (jump-to-version cgc lazy-args ctx)))))

    (let ((lazy-lst (gen-ast (caddr ast) lazy-pre)))
      (gen-ast (cadr ast) lazy-lst))))

;;
;; Call steps
;;

;; Save used registers and return updated ctx
(define (call-save-registers cgc ctx tail? idx-offset)
  (if tail?
      ctx
      (mlet ((moves/nctx (ctx-save-call ctx idx-offset)))
        (apply-moves cgc nctx moves)
        nctx)))

;; Gen continuation stub, push return address, and return updated ctx
(define (call-gen-continuation cgc ctx tail? ast succ apply?)
  (if tail?
      ctx
      (begin
        (if opt-return-points
            (gen-continuation-cr cgc ast succ ctx '() apply?) ;; TODO: remove '() arg
            (gen-continuation-rp cgc ast succ ctx '() apply?))
        (ctx-fs-inc ctx))))

;; Push closure, put it in rax, and return updated ctx
(define (call-get-closure cgc ctx closure-idx)
  (let* ((fs (ctx-fs ctx))
         (lclo (ctx-get-loc ctx closure-idx))
         (mut-clo? (ctx-is-mutable? ctx closure-idx))
         (opclo (codegen-loc-to-x86opnd fs lclo)))
    (if mut-clo?
        (begin
          (if (ctx-loc-is-memory? lclo)
              (begin (x86-mov cgc (x86-rax) opclo)
                     (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
              (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opclo)))
          (set! opclo (x86-rax))))
    (x86-mov cgc (x86-rax) opclo) ;; closure need to be in rax for do-callback-fn (TODO: get closure from stack in do-callback-fn and remove this)
    (x86-push cgc (x86-rax)))
  (ctx-fs-inc ctx))

;; Move args in regs or mem following calling convention
(define (call-prep-args cgc ctx ast nbargs)

  (let* ((stackp/moves (ctx-get-call-args-moves ctx nbargs))
         (stackp (car stackp/moves))
         (moves (cdr stackp/moves)))

    (let loop ((fs (ctx-fs ctx))
               (locs stackp))
      (if (null? locs)
          (set! ctx (ctx-fs-update ctx fs))
          (let ((opnd (codegen-loc-to-x86opnd fs (car locs))))
            (x86-push cgc opnd)
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
      (assert (not (null? unused-regs)) "Internal error")
      (apply-moves cgc ctx moves (car unused-regs))

      ctx)))

;; Unbox mutable args
(define (call-unbox-args cgc ctx nbargs)
  (let loop ((narg 0)
             (stack-idx (- nbargs 1)))
    (if (< narg nbargs)
        (begin
          (if (ctx-is-mutable? ctx stack-idx)
              (if (< narg (length args-regs))
                  (let* ((rloc (list-ref args-regs narg))
                         (opnd (codegen-loc-to-x86opnd (ctx-fs ctx) rloc)))
                    (x86-mov cgc opnd (x86-mem (- 8 TAG_MEMOBJ) opnd)))
                  (let ((memopnd
                          (x86-mem (* stack-idx 8) (x86-rsp))))
                    (x86-mov cgc (x86-r14) memopnd)
                    (x86-mov cgc (x86-r14) (x86-mem (- 8 TAG_MEMOBJ) (x86-r14)))
                    (x86-mov cgc memopnd (x86-r14)))))
          (loop (+ narg 1) (- stack-idx 1))))))

;; Shift args and closure for tail call
(define (call-tail-shift cgc ctx ast tail? nbargs)

  ;; Use r14 because it is not an args reg
  (if tail?
      (let ((fs (ctx-fs ctx))
            (nshift
              (if (> (- nbargs (length args-regs)) 0)
                  (+ (- nbargs (length args-regs)) 1)
                  1)))
        (let loop ((curr (- nshift 1)))
          (if (>= curr 0)
              (begin
                (x86-mov cgc (x86-r14) (x86-mem (* 8 curr) (x86-rsp)))
                (x86-mov cgc (x86-mem (* 8 (+ (- fs nshift 1) curr)) (x86-rsp)) (x86-r14))
                (loop (- curr 1)))))

        (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- fs nshift 1)))))))

;;
;; Make lazy code from CALL EXPR
;;
(define (mlc-call ast succ)

  (let* (;; Tail call if successor's flags set contains 'ret flag
         (tail? (member 'ret (lazy-code-flags succ)))
         ;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (get-lazy-error (ERR_TYPE_EXPECTED CTX_CLO)))
         ;; Lazy call
         (lazy-call
           (make-lazy-code
             (lambda (cgc ctx)

               ;; Save used registers and update ctx
               (set! ctx (call-save-registers cgc ctx tail? (+ (length args) 1)))

               ;; Generate continuation stub and push return address
               (set! ctx (call-gen-continuation cgc ctx tail? ast succ #f))

               ;; Push closure
               (set! ctx (call-get-closure cgc ctx (length args)))

               ;; Move args to regs or stack following calling convention
               (set! ctx (call-prep-args cgc ctx ast (length args)))

               ;; Unbox mutable args
               (call-unbox-args cgc ctx (length args))

               ;; Shift args and closure for tail call
               (call-tail-shift cgc ctx ast tail? (length args))

               ;; Generate call sequence
               ;; Build call ctx
               (let ((call-ctx
                       (ctx-copy
                         (ctx-init)
                           (append (list-head (ctx-stack ctx) (length (cdr ast)))
                                   (list CTX_CLO CTX_RETAD)))))
                 (gen-call-sequence ast cgc call-ctx (length args))))))
         ;; Lazy code object to build the continuation
         (lazy-tail-operator (check-types (list CTX_CLO) (list (car ast)) lazy-call ast)))

    ;; Gen and check types of args
    (check-types
      (list CTX_CLO)
      (list (car ast))
      (gen-ast-l (cdr ast) lazy-call)
      ast)))

;; TODO regalloc: merge -rp and -cr + comments
(define (gen-continuation-rp cgc ast succ ctx saved-regs apply?)

  (let* ((lazy-continuation
           (make-lazy-code-cont
             (lambda (cgc ctx)

               ;; Restore registers
               (for-each
                 (lambda (i)
                   (let ((opnd (codegen-reg-to-x86reg i)))
                     (x86-pop cgc opnd)))
                 (reverse saved-regs))

               ;; Move result to location
               (let* ((lres   (ctx-get-loc ctx 0))
                      (opres  (codegen-loc-to-x86opnd (ctx-fs ctx) lres)))
                 ;; Result is in rax
                 (x86-mov cgc opres (x86-rax)))
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
                                                 (ctx-pop-n ctx (+ (length args) 1))))
                                           (moves/reg/ctx (ctx-get-free-reg ctx)))

                                      (apply-moves cgc ctx moves)
                                      (set! gen-flag
                                            (gen-version-continuation
                                              load-ret-label
                                              lazy-continuation
                                              (ctx-push ctx CTX_UNK reg)))))
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
                     (x86-pop cgc opnd)))
                 (reverse saved-regs))

               ;; Move result to location
               (let* ((lres   (ctx-get-loc ctx 0))
                      (fs (- (ctx-fs ctx) (length saved-regs)))
                      (opres  (codegen-loc-to-x86opnd fs lres)))
                 ;; Result is in rax
                 (x86-mov cgc opres (x86-rax)))
               (jump-to-version cgc succ ctx))))
         (stub-labels
           (add-cont-callback cgc
                              0
                              (lambda (ret-addr selector type table)
                                     (mlet ((args (cdr ast))
                                            (ctx
                                              (if apply?
                                                  (ctx-pop-n ctx 2) ;; Pop operator and args
                                                  (ctx-pop-n ctx (+ (length args) 1)))) ;; Remove closure and args from virtual stack
                                            (moves/reg/ctx (ctx-get-free-reg ctx)))
                                         (apply-moves cgc ctx moves)
                                         (gen-version-continuation-cr lazy-continuation
                                                                      (ctx-push ctx type reg)
                                                                      type
                                                                      table)))))
         ;; CRtable
         (crtable-key (get-crtable-key ast ctx))
         (stub-addr (vector-ref (list-ref stub-labels 0) 1))
         (crtable (get-crtable ast crtable-key stub-addr))
         (crtable-loc (- (obj-encoding crtable) 1)))

    ;; Generate code
    (codegen-load-cont-cr cgc crtable-loc)))

;; Gen call sequence (call instructions)
(define (gen-call-sequence ast cgc call-ctx nb-args)
    (cond ((and opt-entry-points nb-args)
             (let* ((idx (get-closure-index call-ctx)))
               (if idx
                   (codegen-call-cc-spe cgc idx (ctx->still-ref call-ctx) nb-args)
                   (codegen-call-cc-gen cgc))))
          ((and opt-entry-points (not nb-args))
             (codegen-call-cc-gen cgc))
          (else
             (codegen-call-ep cgc nb-args))))

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
                          (moves/reg/ctx (ctx-get-free-reg ctx))
                          (lleft (ctx-get-loc ctx 1))
                          (lright (ctx-get-loc ctx 0))
                          (mut-left? (ctx-is-mutable? ctx 1))
                          (mut-right? (ctx-is-mutable? ctx 0)))
                     (if (or mut-left? mut-right?)
                         (error "NYI"))
                     (apply-moves cgc ctx moves)
                     (codegen-binop cgc (ctx-fs ctx) op label-div0 reg lleft lright)
                     (jump-to-version cgc
                                      succ
                                      (ctx-push (ctx-pop-n ctx 2) CTX_INT reg)))))))
         ;; Check operands type
         (check-types (list CTX_INT CTX_INT)
                      (list (car opnds) (cadr opnds))
                      lazy-op
                      ast)))))

;;
;; Make lazy code from N-ARY OPERATOR
;;
(define (mlc-op-n ast succ op) ;; '(+ - * < > <= >= = /)

  ;; Ast if 0 opnd
  (define (ast0 op)
    (case op
      ((+) 0)
      ((*) 1)
      ((< <= > >= =) #t)))

  ;; Ast if 1 opnd
  (define (ast1 op opnd)
    (case op
      ((+ *) opnd)
      ((-)   `(- 0 ,opnd))
      ((/)   `(/ 1 ,opnd))
      ((< <= > >= =) #t)))

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
           (error "Internal error"))) ;; comparisons are handled by macro expander
    (else
      (let ((lcst (integer? (cadr ast)))
            (rcst (integer? (caddr ast))))
        (cond
          ((and lcst rcst)
             (gen-ast (eval ast) succ))
          (lcst
             (gen-ast (caddr ast)
                      (get-lazy-n-binop ast op (cadr ast) #f succ)))
          (rcst
             (gen-ast (cadr ast)
                      (get-lazy-n-binop ast op #f (caddr ast) succ)))
          (else
            (gen-ast (cadr ast)
                     (gen-ast (caddr ast)
                              (get-lazy-n-binop ast op #f #f succ)))))))))

(define (get-lazy-n-binop ast op lcst rcst succ #!optional (inline-if-labels #f))

  (define inlined-if-cond? inline-if-labels)
  (define num-op? (member op '(+ - * /)))

  ;; Build chain to check type of one value (if one of them is a cst)
  (define (type-check-one)
    (let* (;; Op
           (lazy-op-i (get-op-ii))
           (lazy-op-f (get-op-ff (integer? lcst) (integer? rcst)))
           ;; Checks
           (lazy-float (gen-fatal-type-test CTX_FLO 0 lazy-op-f ast))
           (lazy-int (gen-dyn-type-test CTX_INT 0 lazy-op-i lazy-float ast)))
      lazy-int))

  ;; Build chain to check type of two values (no cst)
  (define (type-check-two)
    (let* (;; Operations lco
           (lazy-op-ii (get-op-ii))
           (lazy-op-if (get-op-ff #t #f))
           (lazy-op-fi (get-op-ff #f #t))
           (lazy-op-ff (get-op-ff #f #f))
           ;; Right branch
           (lazy-yfloat2 (gen-fatal-type-test CTX_FLO 0 lazy-op-ff ast))
           (lazy-yint2   (gen-dyn-type-test CTX_INT 0 lazy-op-fi lazy-yfloat2 ast))
           (lazy-xfloat  (gen-fatal-type-test CTX_FLO 1 lazy-yint2 ast))
           ;; Left branch
           (lazy-yfloat  (gen-fatal-type-test CTX_FLO 0 lazy-op-if ast))
           (lazy-yint    (gen-dyn-type-test CTX_INT 0 lazy-op-ii lazy-yfloat ast))
           ;; Root node
           (lazy-xint    (gen-dyn-type-test CTX_INT 1 lazy-yint lazy-xfloat ast)))
    lazy-xint))

  ;; TODO: Merge with get-op-ff
  (define (get-op-ii)
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((type (if num-op? CTX_INT CTX_BOOL))
               (res (if inlined-if-cond? #f (ctx-get-free-reg ctx)))
               (moves (if res (car res) '()))
               (reg   (if res (cadr res) #f))
               (ctx   (if res (caddr res) ctx))
               ;; We need to get locs AFTER ctx-get-free-reg call
               (lright (or rcst (ctx-get-loc ctx 0)))
               (lleft
                 (if rcst
                     (or lcst (ctx-get-loc ctx 0))
                     (or lcst (ctx-get-loc ctx 1))))
               (mut-right? (and (not rcst) (ctx-is-mutable? ctx 0)))
               (mut-left?  (and (not lcst) (ctx-is-mutable? ctx (if rcst 0 1))))
               (n-pop (count (list lcst rcst) not)))
          (apply-moves cgc ctx moves)

          (if num-op?
              (codegen-num-ii cgc (ctx-fs ctx) op reg lleft lright lcst rcst mut-left? mut-right?)
              (codegen-cmp-ii cgc (ctx-fs ctx) op reg lleft lright lcst rcst mut-left? mut-right? inline-if-labels))

          (if (not inlined-if-cond?)
              (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) type reg)))))))

  ;;
  (define (get-op-ff leftint? rightint?)
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((type (if num-op? CTX_FLO CTX_BOOL))
               (res (if inlined-if-cond? #f (ctx-get-free-reg ctx)))
               (moves (if res (car res) '()))
               (reg (if res (cadr res) #f))
               (ctx (if res (caddr res) ctx))
               ;; We need to get locs AFTER ctx-get-free-reg call
               (lright (or rcst (ctx-get-loc ctx 0)))
               (lleft
                 (if rcst
                     (or lcst (ctx-get-loc ctx 0))
                     (or lcst (ctx-get-loc ctx 1))))
               (mut-right? (and (not rcst) (ctx-is-mutable? ctx 0)))
               (mut-left?  (and (not lcst) (ctx-is-mutable? ctx (if rcst 0 1))))
               (n-pop (count (list lcst rcst) not)))
          (apply-moves cgc ctx moves)
          (if (or mut-left? mut-right?)
              (error "NYI mutable float op"))
          (if num-op?
              (codegen-num-ff cgc (ctx-fs ctx) op reg lleft leftint? lright rightint? lcst rcst)
              (codegen-cmp-ff cgc (ctx-fs ctx) op reg lleft leftint? lright rightint? lcst rcst inline-if-labels))

          (if (not inlined-if-cond?)
              (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx n-pop) type reg)))))))

  (assert (not (and inlined-if-cond? (member op '(+ - * /))))
          "Internal compiler error")

  (if (or lcst rcst)
      (type-check-one)
      (type-check-two)))

;;
;; Make lazy code from TYPE TEST
;;
(define (mlc-test ast succ)

  (define (get-lazy-res bool)
    (make-lazy-code
      (lambda (cgc ctx)
        (mlet ((moves/reg/ctx (ctx-get-free-reg ctx)))
          (apply-moves cgc ctx moves)
          (codegen-set-bool cgc bool reg)
          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL reg))))))

  (let ((type (predicate-to-ctxtype (car ast)))
        (stack-idx 0)
        (lazy-success (get-lazy-res #t))
        (lazy-fail    (get-lazy-res #f)))

    (gen-ast (cadr ast)
             (gen-dyn-type-test type stack-idx lazy-success lazy-fail ast))))

;;
;; Make lazy code to create pair
;; Create pair with the too values on top of the stack
;;
(define (mlc-pair succ #!optional (cst-infos '()))
  (make-lazy-code
    (lambda (cgc ctx)
      (mlet ((moves/reg/ctx (ctx-get-free-reg ctx))
             (car-cst (assoc 0 cst-infos))
             (cdr-cst (assoc 1 cst-infos))
             (lcdr (if cdr-cst (cdr cdr-cst) (ctx-get-loc ctx 0)))
             (mut-cdr? (and (not cdr-cst) (ctx-is-mutable? ctx 0)))
             (car-idx (if cdr-cst 0 1))
             (lcar
               (if car-cst
                   (cdr car-cst)
                   (ctx-get-loc ctx car-idx)))
             (mut-car? (and (not car-cst) (ctx-is-mutable? ctx car-idx)))
             (n-pop (count (list car-cst cdr-cst) not)))
      (apply-moves cgc ctx moves)
      (codegen-pair cgc (ctx-fs ctx) reg lcar lcdr car-cst cdr-cst mut-car? mut-cdr?)
      (jump-to-version cgc
                       succ
                       (ctx-push (ctx-pop-n ctx n-pop) CTX_PAI reg))))))

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
                                 (if (eq? (identifier-kind (cdr n)) 'local)
                                     ;; If local, get type from stack
                                     (let ((type (ctx-identifier-type ctx (cdr n))))
                                       type)
                                     ;; If free, get type from env
                                     (identifier-stype (cdr n))))
                           r)
                     r))
               '()
               (ctx-env ctx))))

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

(define (gen-free-vars cgc ids ctx offset)
  (if (null? ids)
      '()
      (let* ((identifier (cdr (assoc (car ids) (ctx-env ctx))))
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
        (x86-mov cgc (x86-mem (+ 16 (* offset 8)) alloc-ptr) opn)
        (gen-free-vars cgc (cdr ids) ctx (+ offset 1)))))

;; Return all free vars used by the list of ast knowing env 'clo-env'
(define (free-vars-l lst params enc-ids)
  (if (null? lst)
      '()
      (set-union (free-vars   (car lst) params enc-ids)
                 (free-vars-l (cdr lst) params enc-ids))))

;; Return all free vars used by ast knowing env 'clo-env'
(define (free-vars body params enc-ids)
  (cond ;; Symbol
        ((symbol? body)
          (if (and (member body enc-ids)
                   (not (member body params)))
              (list body)
              '()))
        ;; Literal
        ((literal? body) '())
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
                  (else (free-vars-l body params enc-ids)))))))

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
                    ((eq? op 'let)
                      ;; Search for mutable only in bindings, and body if ids not redefined
                      (let* ((mvars-bindings (mutable-vars-l (cadr ast) params))
                             (let-ids (map car (cadr ast)))
                             (p (set-sub params let-ids '()))
                             (mvars-body (mutable-vars-l (cddr ast) p)))
                        (set-union mvars-bindings mvars-body)))
                    ((eq? op 'letrec)
                      ;; Search for mutable in ast if ids not redefined
                      (let* ((letrec-ids (map car (cadr ast)))
                             (p (set-sub params letrec-ids '())))
                        (set-union (mutable-vars-l (cadr ast) p)
                                   (mutable-vars-l (cddr ast) p))))
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
      (codegen-clean-stack cgc rsp-offset)
      ;; Shift next
      (begin (codegen-tco-move-arg cgc from to)
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
;;     (promal-params '(a b . c)) -> '(a b)
(define (formal-params l)
  (if (not (pair? l))
     '()
     (cons (car l) (formal-params (cdr l)))))


;; TODO regalloc
(define (gen-mutable cgc ctx mvars)
  (if (null? mvars)
      ctx
      (let* ((mid (car mvars))
             (resid (assoc mid (ctx-env ctx)))
             (identifier (cdr resid))
             (loc (ctx-identifier-loc ctx identifier)))

        (codegen-mutable cgc (ctx-fs ctx) loc)
        (gen-mutable cgc ctx (cdr mvars)))))

;; Return label of a stub generating error with 'msg'
(define (get-label-error msg) (list-ref (add-callback #f   0 (lambda (ret-addr selector) (error msg))) 0))
