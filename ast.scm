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

;; Primitives: name, nb args min, nb args max
(define primitives `(
   (car                 1  1  ,(prim-types 1 CTX_PAI))
   (cdr                 1  1  ,(prim-types 1 CTX_PAI))
   (eq?                 2  2  ,(prim-types 2 CTX_ALL CTX_ALL))
   (char=?              2  2  ,(prim-types 2 CTX_CHAR CTX_CHAR))
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
   (list                #f) ;; nb-args and types are not fixed
))

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
                ; ;; Do
                ; ((eq? op 'do) (mlc-do ast succ))
                 ;; Binding
                 ((eq? op 'let) (mlc-let ast succ)) ;; Also handles let* (let* is a macro)
                 ((eq? op 'letrec) (mlc-letrec ast succ))
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
        (let* ((immediate
                (if (< ast 0)
                    (let* ((ieee-rep (ieee754 (abs ast) 'double))
                           (64-mod   (bitwise-not (- ieee-rep 1)))
                           (64-modl  (bitwise-and (- (expt 2 63) 1) 64-mod)))
                      (* -1 64-modl))
                    (ieee754 ast 'double)))
               (res (ctx-get-free-reg cgc ctx))
               (reg (car res))
               (ctx (cdr res)))
          (codegen-flonum cgc immediate reg)
          (jump-to-version cgc succ (ctx-push ctx CTX_FLO reg))))))

;;
;; Make lazy code from symbol literal
;;
(define (mlc-symbol ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (let* ((res (ctx-get-free-reg cgc ctx))
             (reg (car res))
             (ctx (cdr res)))
        (codegen-symbol cgc ast reg)
        (jump-to-version cgc succ (ctx-push ctx CTX_SYM reg))))))

;;
;; Make lazy code from vector literal
;;
;; TODO regalloc, need to handle make-vector
;; Stratégie:
;; Lazy-put : 'push' le vecteur?? et saute au succ
;; Lazy-chain : chaine v[i] -> mov-in-vector -> v[i+1] -> mov-in-vector ...
;; Lazy-alloc : alloue un vector de taille n rempli de 0 (important pour le GC) -> donc dépend de make-vector
;;              puis saute vers Lazy-chain
;; TODO regalloc: comment
(define (mlc-vector ast succ)

  (define (gen-set cgc ctx lidx)
    (let* ((res (ctx-get-free-reg cgc ctx))
           (reg (car res))
           (ctx (cdr res))
           (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx lidx)))
           (opval (codegen-loc-to-x86opnd lval)))

      (if (ctx-loc-is-memory? lval)
          (begin (x86-mov cgc (x86-rax) opval)
                 (set! opval (x86-rax))))

      (x86-mov cgc (x86-mem (+ (* lidx 8) 16) alloc-ptr) opval)
      ctx))

  (define lazy-vector
    (make-lazy-code
      (lambda (cgc ctx)
        (let loop ((pos 0) (ctx ctx))
          (if (= pos (vector-length ast))
              (let* ((res (ctx-get-free-reg cgc ctx))
                     (reg (car res))
                     (ctx (cdr res)))
                (x86-lea cgc (codegen-reg-to-x86reg reg) (x86-mem TAG_MEMOBJ alloc-ptr))
                (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx (vector-length ast)) CTX_VECT reg)))
              (let ((ctx (gen-set cgc ctx pos)))
                (loop (+ pos 1) ctx)))))))


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
      (let* ((res (ctx-get-free-reg cgc ctx))
             (reg (car res))
             (ctx (cdr res)))
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

;;
;; Make lazy code from SYMBOL
;;
;; TODO: réécrire: attention la recherche d'id met à jour le ctx
(define (mlc-identifier ast succ)

  (make-lazy-code
    (lambda (cgc ctx)

      (let ((local  (assoc ast (ctx-env ctx)))
            (global (assoc ast globals)))
        ;;
        (cond ;; Identifier is a free variable
              ((and local (eq? (identifier-kind (cdr local)) 'free))
                (gen-get-freevar cgc ctx local succ))
              ;; Identifier is a local variable
              (local
                (gen-get-localvar cgc ctx local succ))
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

;; TODO: comment on recursive call in not raw?
;; TODO: commenter le fonctionnement de orig-loc
(define (gen-get-freevar cgc ctx local succ #!optional (raw? #f))

  (let ((mutable? (member 'mutable (identifier-flags (cdr local)))))

    (if raw?
        (let ((loc (ctx-identifier-loc ctx (cdr local) #t)))
          ;; We want mobject, then write in rax
          (let* ((fpos (ctx-floc-to-fpos loc))
                 (closure-lidx (- (length (ctx-stack ctx)) 2))
                 (closure-loc (ctx-get-loc ctx (ctx-lidx-to-slot ctx closure-lidx)))
                 (closure-opnd (codegen-loc-to-x86opnd closure-loc)))

            (if (ctx-loc-is-memory? closure-loc)
                (begin (x86-mov cgc (x86-rax) closure-opnd)
                       (set! closure-opnd (x86-rax))))
            (x86-mov cgc (x86-rax) (x86-mem (- (+ (* fpos 8) 16) TAG_MEMOBJ) closure-opnd))))
        ;; We don't want raw value
        (let* ((loc (ctx-identifier-loc ctx (cdr local)))
               (res (ctx-get-free-reg cgc ctx (list loc)))
               (reg (car res))
               (ctx (cdr res))
               (orig? (ctx-loc-is-orig-loc ctx (cdr local) loc))
               (type (ctx-identifier-type ctx (cdr local))))
          (cond ;; orig loc and mutable or not
                (orig?
                   (let ((dest (codegen-loc-to-x86opnd reg)))
                     ;; Get mobj in rax
                     (gen-get-freevar cgc ctx local succ #t)
                     (if mutable?
                         (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
                         (x86-mov cgc dest (x86-rax)))))
                ;; not orig loc and mutable or not
                (else
                  (let ((dest (codegen-loc-to-x86opnd reg))
                        (opvar (codegen-loc-to-x86opnd loc)))
                    (x86-mov cgc dest opvar))))
          (jump-to-version cgc succ (ctx-push ctx type reg (car local))))))) ;; TODO type

          ;; cas1: mutable & boxed (donc mutable & fn)
          ;;    mov dest, closure
          ;;    mov dest, [closure+offset]
          ;;    mov dest, [dest+offset]
          ;; cas2: mutable & !boxed (donc reg ou mem)
          ;;    mov dest, loc
          ;; cas3: !mutable
          ;;    cas 3.1 floc:
          ;;        mov dest, closure
          ;;        mov dest, [closure+offset]
          ;;    cas 3.2 !floc:
          ;;        mov dest,loc

(define (ctx-loc-is-orig-loc ctx identifier loc)
  (if (eq? (identifier-kind identifier) 'free)
      (eq? loc (identifier-cloc identifier))
      (let ((orig-slot (list-ref (identifier-sslots identifier)
                                 (- (length (identifier-sslots identifier)) 1))))
        (eq? loc (ctx-get-loc ctx orig-slot)))))

;; TODO: + utiliser un appel récursif comme pour gen-get-freevar (??)
;; TODO coment: si mobject? est vrai, c'est qu'on veut le mobject dans le tmp reg (rax)
(define (gen-get-localvar cgc ctx local succ #!optional (raw? #f))

  (let ((mutable? (member 'mutable (identifier-flags (cdr local)))))

    (if raw?
        ;; We want mobject, then write in rax
        (let* ((loc (ctx-identifier-loc ctx (cdr local) #t))
               (opnd (codegen-loc-to-x86opnd loc)))
          (x86-mov cgc (x86-rax) opnd))
        ;; We don't want mobject
        (let* ((loc (ctx-identifier-loc ctx (cdr local)))
               (res (ctx-get-free-reg cgc ctx (list loc)))
               (reg (car res))
               (ctx (cdr res))
               (boxed? (ctx-loc-is-orig-loc ctx (cdr local) loc))
               (type
                 (if mutable?
                     CTX_UNK
                     (ctx-identifier-type ctx (cdr local)))))
          (cond ;; Mutable and in memory
                ((and (ctx-loc-is-memory? loc) mutable?)
                  (let ((dest (codegen-reg-to-x86reg reg))
                        (opnd (codegen-loc-to-x86opnd loc)))
                    (x86-mov cgc (x86-rax) opnd)
                    (if boxed?
                        (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
                        (x86-mov cgc dest (x86-rax))))) ;; TODO
                ;; Mutable and in register
                (mutable?
                  (let ((dest (codegen-reg-to-x86reg reg))
                        (opnd (codegen-loc-to-x86opnd loc)))
                    (if boxed?
                        (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) opnd))
                        (x86-mov cgc dest opnd)))) ;; TODO
                ;; Not mutable and in reg or mem
                (else
                  (let ((dest (codegen-reg-to-x86reg reg))
                        (opnd (codegen-loc-to-x86opnd loc)))
                    (x86-mov cgc dest opnd))))
          (jump-to-version cgc succ (ctx-push ctx type reg (car local)))))))

(define (gen-get-globalvar cgc ctx global succ)

  (let* (;; Get variable type if known
         (r (assoc (car global) gids))
         (type (if (and r (cdr r))
                   (cdr r)
                   CTX_UNK))
         ;; Get free register (dest)
         (res (ctx-get-free-reg cgc ctx))
         (reg (car res))
         (ctx (cdr res)))
    ;; Generate code to get global var from memory
    (codegen-get-global cgc (cdr global) reg)
    ;; Jump with updated ctx
    (jump-to-version cgc succ (ctx-push ctx type reg))))

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
    (let* ((res (ctx-get-free-reg cgc ctx))
               (reg (car res))
               (ctx (cdr res)))
          (codegen-literal cgc ast reg)
          (jump-to-version cgc
                           succ
                           (ctx-push ctx
                                     (cond ((integer? ast) CTX_NUM)
                                           ((boolean? ast) CTX_BOOL)
                                           ((char? ast)    CTX_CHAR)
                                           ((null? ast)    CTX_NULL)
                                           (else (error ERR_INTERNAL)))
                                     reg)))))))

;;-----------------------------------------------------------------------------
;; INTERNAL FORMS

;;
;; Make lazy code from SET!
;;
(define (mlc-set! ast succ)

  (let* ((id (cadr ast))
         (lazy-set!
           (make-lazy-code
             (lambda (cgc ctx)
               (let ((gres (assoc id globals)))
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
  (let* ((res (ctx-get-free-reg cgc ctx))
         (reg (car res))
         (ctx (cdr res))
         (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
    (let ((dest (codegen-reg-to-x86reg reg))
          (opval (codegen-loc-to-x86opnd lval)))
      (if (ctx-loc-is-memory? lval)
          (begin (x86-mov cgc dest opval)
                 (set! opval dest)))
      (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) opval)
      (x86-mov cgc dest (x86-imm-int ENCODING_VOID))
      (let* ((ctx (ctx-push (ctx-pop ctx) CTX_VOID reg))
             (ctx (ctx-identifier-change-type ctx (cdr local) CTX_UNK))) ;; TODO regalloc unk
        (jump-to-version cgc succ ctx)))))

(define (gen-set-freevar cgc ctx local succ)
  ;; Get mobject in tmp register
  (gen-get-freevar cgc ctx local #f #t)
  ;;
  (let* ((res (ctx-get-free-reg cgc ctx))
         (reg (car res))
         (ctx (cdr res))
         (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
    (let ((dest (codegen-reg-to-x86reg reg))
          (opval (codegen-loc-to-x86opnd lval)))
      (if (ctx-loc-is-memory? lval)
          (begin (x86-mov cgc dest opval)
                 (set! opval dest)))
      (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) opval)
      (x86-mov cgc dest (x86-imm-int ENCODING_VOID))
      (let* ((ctx (ctx-push (ctx-pop ctx) CTX_VOID reg))
             (ctx (ctx-identifier-change-type ctx (cdr local) CTX_UNK))) ;; TODO unk
        (jump-to-version cgc succ ctx)))))

(define (gen-set-globalvar cgc ctx global succ)
  (let* ((pos (cdr global))
         (res (ctx-get-free-reg cgc ctx))
         (reg (car res))
         (ctx (cdr res))
         (dest (codegen-reg-to-x86reg reg))
         (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
         (opval (codegen-loc-to-x86opnd lval)))
    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))
    (x86-mov cgc (x86-mem (* 8 pos) global-ptr) opval)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))
    (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID reg))))

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)

  (let* ((identifier (cadr ast))
         (lazy-bind (make-lazy-code
                      (lambda (cgc ctx)
                        (let* ((res (assoc identifier globals)) ;; Lookup in globals
                               (pos (cdr res))                  ;; Get global pos
                               ;;
                               (res (ctx-get-free-reg cgc ctx))     ;; Return reg,ctx
                               (reg (car res))
                               (ctx (cdr res))
                               (lvalue (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                          (codegen-define-bind cgc pos reg lvalue)
                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID reg))))))
         (lazy-val (gen-ast (caddr ast) lazy-bind)))

    (make-lazy-code
      (lambda (cgc ctx)
        ;; TODO regalloc: this codegen is useless ? Just update globals set?
        (codegen-define-id cgc)
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

;; TODO regalloc: Créer de nouvelles entrées dans la table (+ que le type de la valeur de retour)
;;                avec slot-loc free-regs
;; Return crtable key from ast and ctx
;; The key contains ast, stack types, and a list of known identifier with types
(define (get-crtable-key ast ctx)
  (cons ast
        (list (ctx-slot-loc ctx)
              (ctx-free-regs ctx)
              (ctx-stack ctx)
              (ctx-env ctx))))
              ;; TODO regalloc: need to store sslots of identifiers
            ;  (map (lambda (el)
            ;         (cons (car el)
            ;           (identifier-stype (cdr el))))
            ;       (ctx-env ctx)))))

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
                       ;; TODO regalloc
                       ;; 1 - Get clean stack size (higher mx in ctx)
                       (let* ((clean-nb (- (ctx-fs ctx) 1))
                              (lres (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
                              (opres (codegen-loc-to-x86opnd lres)))
                         ;; 2 - Move res in rax
                         (x86-mov cgc (x86-rax) opres)

                         ;; 3 - Clean stack
                         (x86-add cgc (x86-rsp) (x86-imm-int (* 8 clean-nb)))
                         ;;
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
         (lazy-prologue-gen (get-lazy-generic-prologue ast lazy-body rest-param mvars)))

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

                                                       (error "NYI fn callback mlc-lambda"))

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
          (set! fvars (free-vars (caddr ast) all-params ctx))

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

          (let* ((res (ctx-get-free-reg cgc ctx))
                 (reg (car res))
                 (ctx (cdr res)))

            ;; Put closure
            (codegen-closure-put cgc reg)

            ;; Trigger the next object
            (if opt-propagate-functionid
                (error "NYI - mlc-lambda: Use (CTX_CLOi with cctable or closure id)")
                (jump-to-version cgc succ (ctx-push ctx CTX_CLO reg)))))))))

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

;; TODO CLEAN raglloc args regs
(define (get-lazy-generic-prologue ast succ rest-param mvars)
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
                          (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (car regs)))
                          (x86-je cgc label-next-arg-end)
                          (loop (- i 1) (cdr regs)))))
                ;(x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (car (reverse args-regs))))
                ;
                ;(let loop ((i (- (length args-regs) 1))
                ;           (regs (cdr (reverse args-regs))))
                ;  (if (not (= i 0))
                ;      (begin
                ;        ;  (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding i)))
                ;        ;  (x86-je cgc label-next-arg-end)
                ;        ;  (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd (car regs)))
                ;          (loop (- i 1) (cdr regs)))))
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
                    (x86-mov cgc (codegen-loc-to-x86opnd reg) (x86-r14)))
                  (x86-push cgc (x86-r14)))))

        (jump-to-version cgc succ ctx)))))
        ;
        ;(if rest-param
        ;    ;; Function using rest param
        ;    (let ((label-case2 (asm-make-label #f (new-sym 'generic-prologue-case2-)))
        ;          (label-end   (asm-make-label #f (new-sym 'generic-prologue-end))))
        ;      ;; If rdi < nb-args - 1, error
        ;      (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (- nb-args 1))))
        ;      (x86-jge cgc label-case2)
        ;        (gen-error cgc ERR_WRONG_NUM_ARGS)
        ;      ;; If rdi >= nb-args - 1, gen rest list from arguments
        ;      (x86-label cgc label-case2)
        ;        (let ((label-lend (asm-make-label #f (new-sym 'label-lend)))
        ;              (label-lloop (asm-make-label #f (new-sym 'label-lloop)))
        ;              (header-word (mem-header 3 STAG_PAIR)))
        ;          ;; pos (rsi) = 0
        ;          (x86-mov cgc (x86-rsi) (x86-imm-int 0))
        ;          ;; cdr (rbx) = '()
        ;          (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding '())))
        ;          ;; lim (rdi) = rdi - nb-args - 1
        ;          (x86-sub cgc (x86-rdi) (x86-imm-int (obj-encoding (- nb-args 1))))
        ;          (x86-shl cgc (x86-rdi) (x86-imm-int 1))
        ;          ;; if po == lim then goto end
        ;          (x86-label cgc label-lloop)
        ;          (x86-cmp cgc (x86-rsi) (x86-rdi))
        ;          (x86-je cgc label-lend)
        ;            ;;; p = pair
        ;            (gen-allocation cgc #f STAG_PAIR 3)
        ;            ;; p.header = header-word
        ;            (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        ;            (x86-mov cgc (x86-mem  0 alloc-ptr) (x86-rax))
        ;            ;; p.cdr = cdr
        ;            (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rbx))
        ;            ;; p.car = stack[pos]
        ;            (x86-mov cgc (x86-rbx) (x86-mem 0 (x86-rsp) (x86-rsi)))
        ;            (x86-mov cgc (x86-mem  8 alloc-ptr) (x86-rbx))
        ;            ;; cdr = p
        ;            (x86-lea cgc (x86-rbx) (x86-mem TAG_MEMOBJ alloc-ptr))
        ;            ;; pos++
        ;            (x86-add cgc (x86-rsi) (x86-imm-int 8))
        ;            (x86-jmp cgc label-lloop)
        ;          (x86-label cgc label-lend)
        ;          (x86-add cgc (x86-rsp) (x86-rdi))
        ;          (x86-push cgc (x86-rbx)))
        ;      (x86-label cgc label-end))
        ;
        ;    ;; Not rest param, then if formal != actual, error.
        ;    (begin
        ;      (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args)))
        ;      (x86-je cgc label-next)
        ;        (gen-error cgc ERR_WRONG_NUM_ARGS)
        ;      (x86-label cgc label-next)))
        ;(gen-mutable cgc ctx mvars)
        ;(jump-to-version cgc succ ctx)))))
        ;
;; Create and return a lazy prologue
(define (get-lazy-prologue ast succ rest-param mvars)
  (make-lazy-code-entry
    (lambda (cgc ctx)
      (let* ((nb-actual (- (length (ctx-stack ctx)) 2))
             (nb-formal (ctx-nb-args ctx))
             (nstack  (ctx-stack ctx))    ;; New stack  (change if rest-param)
             (nnbargs (ctx-nb-args ctx))) ;; New nbargs (change if rest-param)

        (cond ;; rest AND actual == formal
              ((and rest-param (= nb-actual (- nb-formal 1))) ;; -1 rest
                 (set! ctx (ctx-stack-push ctx CTX_NULL))
                 (codegen-prologue-rest= cgc))
              ;; rest AND actual > formal
              ((and rest-param (> nb-actual (- nb-formal 1)))
                 (let* ((nb-extra (- nb-actual (- nb-formal 1)))
                        (nctx (ctx-pop-n ctx (- nb-extra 1)))
                        (nctx (ctx-change-type nctx 0 CTX_PAI)))
                   (set! ctx nctx)
                   (codegen-prologue-rest> cgc nb-extra)))
              ;; (rest AND actual < formal) OR (!rest AND actual < formal) OR (!rest AND actual > formal)
              ((or (< nb-actual nb-formal) (> nb-actual nb-formal))
                 (gen-error cgc ERR_WRONG_NUM_ARGS))
              ;; Else, nothing to do
              (else #f))

        (gen-mutable cgc ctx mvars)
        (jump-to-version cgc succ ctx)))))

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

;; TODO

;; TODO regalloc: factoriser avec mlc-letrec (body lazy-out)
;; TODO regalloc kind, flags, stype (voir ctx-bind)
;; TODO + utiliser mlc-binding quand mlc-let* mlc-letrec ajoutés
;; TODO ajouter un lc après 'bind' pour mettre les variables mutables en mémoire
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
                 (let* ((ctx (ctx-unbind ctx ids)) ;; update env
                        (ctx (ctx-move-lidx ctx 0 (length ids))) ;; mov result slot
                        (ctx (ctx-pop-n ctx (length ids)))) ;; pop from virtual stack
                   (jump-to-version cgc succ ctx))))))
         (lazy-body
           (gen-ast (cons 'begin body) lazy-out))
         (lazy-binds
           (make-lazy-code
             (lambda (cgc ctx)
               (let* ((id-idx (build-id-idx ids (- (length ids) 1)))
                      (mvars (mutable-vars (cddr ast) ids)) ;; Get mutable vars
                      (ctx (ctx-bind ctx id-idx mvars)))
                 (gen-mutable cgc ctx mvars)
                 (jump-to-version cgc lazy-body ctx))))))
    (gen-ast-l values lazy-binds)))

(define (mlc-letrec ast succ)

  (define (alloc cgc ids ctx stack-types)
    (if (null? ids)
        ctx
        (let* ((res (ctx-get-free-reg cgc ctx))
               (reg (car res))
               (ctx (cdr res)))

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

  (let* ((ids (map car (cadr ast)))
         (body (cddr ast))
         (lazy-out
           (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                          make-lazy-code-ret
                          make-lazy-code)))
             (make-lc
               (lambda (cgc ctx)
                 (let* ((ctx (ctx-unbind ctx ids)) ;; update env
                        (ctx (ctx-move-lidx ctx 0 (length ids))) ;; mov result slot
                        (ctx (ctx-pop-n ctx (length ids)))) ;; pop from virtual stack
                   (jump-to-version cgc succ ctx))))))
         (lazy-set
           (make-lazy-code
             (lambda (cgc ctx)
               (let loop ((i 0)
                          (ctx ctx))
                 (if (= i (length ids))
                     (let ((ctx (ctx-pop-n ctx (length ids))))
                       (jump-to-version cgc (gen-ast-l body lazy-out) ctx))
                     (let ((lfrom (ctx-get-loc ctx (ctx-lidx-to-slot ctx i)))
                           (lto   (ctx-get-loc ctx (ctx-lidx-to-slot ctx (+ i (length ids)))))
                           (ctx   (ctx-move-type ctx i (+ i (length ids)))))
                       (let ((opfrom (codegen-loc-to-x86opnd lfrom))
                             (opto   (codegen-loc-to-x86opnd lto))
                             (regtopop #f))
                         (cond ((and (ctx-loc-is-memory? lfrom)
                                     (ctx-loc-is-memory? lto))
                                  (let* ((regs (list (x86-rbx) (x86-rcx) (x86-rdx)))
                                         (pr (foldr
                                               (lambda (el r)
                                                 (if (and (not (eq? el opfrom))
                                                          (not (eq? el opto)))
                                                     el
                                                     r))
                                               #f
                                               regs)))
                                    (x86-push cgc pr)
                                    (x86-mov cgc (x86-rax) opfrom)
                                    (x86-mov cgc pr opto)
                                    (set! opfrom (x86-rax))
                                    (set! opto pr)
                                    (set! regtopop pr)))
                               ((ctx-loc-is-memory? lfrom)
                                  (x86-mov cgc (x86-rax) opfrom)
                                  (set! opfrom (x86-rax)))
                               ((ctx-loc-is-memory? lto)
                                  (x86-mov cgc (x86-rax) opto)
                                  (set! opto (x86-rax))))

                         (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opto) opfrom)
                         (if regtopop
                             (x86-pop cgc regtopop))
                         (loop (+ i 1) ctx))))))))
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
                   (ctx-bind ctx bind-lst ids)))))))
    lazy-pre))

  ;(define (alloc cgc ids ctx)
  ;  (if (null? ids)
  ;      ctx
  ;      (let* ((res (ctx-get-free-reg ctx)) ;; Return reg,ctx
  ;             (reg (car res))
  ;             (ctx (cdr res)))
  ;        (x86-mov cgc (codegen-reg-to-x86reg reg) (x86-imm-int ENCODING_VOID))
  ;        (alloc
  ;          cgc
  ;          (cdr ids)
  ;          (ctx-push ctx CTX_UNK reg))))) ;; TODO: unknown, because ids are mutable
  ;
  ;(let* ((ids (map car (cadr ast)))
  ;       (body (cddr ast))
  ;       (lazy-out
  ;           ;; TODO
  ;           (let ((make-lc (if (member 'ret (lazy-code-flags succ))
  ;                          make-lazy-code-ret
  ;                          make-lazy-code)))
  ;             (make-lc
  ;               (lambda (cgc ctx)
  ;                 (let* ((ctx (ctx-unbind ctx ids)) ;; update env
  ;                        (ctx (ctx-move-lidx ctx 0 (length ids))) ;; mov result slot
  ;                        (ctx (ctx-pop-n ctx (length ids)))) ;; pop from virtual stack
  ;                   (jump-to-version cgc succ ctx))))))
  ;       (lazy-body
  ;         (gen-ast (cons 'begin body) lazy-out))
  ;       (lazy-set
  ;         (make-lazy-code
  ;           (lambda (cgc ctx)
  ;             (pp "AVANT SET")
  ;             (pp ctx)
  ;             ;; TODO regalloc comment this
  ;             ;; for i=0, i<nbBindings, i++
  ;             ;;   ;; rax = valeur de virtual-stack[i+nbBindings]
  ;             ;;   ;; op = operand(virtual-stack[i])
  ;             ;;   ;; mov [op+7], rax (reg intermediaire si en mémoire)
  ;             (let loop ((i 0)
  ;                        (ctx ctx)
  ;                        (ids (reverse ids)))
  ;                  (if (= i (length (map car (cadr ast)))) ;; TODO: use ids, but not the one in loop
  ;                      (begin (pp "APRES SET") (pp ctx)
  ;                      (jump-to-version cgc
  ;                                       lazy-body
  ;                                       (ctx-pop-n ctx (length (map car (cadr ast)))))) ;; TODO meme que partout
  ;                      (let* ((lfrom  (ctx-get-loc ctx (ctx-lidx-to-slot ctx i)))
  ;                             (lto    (ctx-get-loc ctx (ctx-lidx-to-slot ctx (+ i (length (map car (cadr ast))))))) ;; TODO: meme que dessous
  ;                             (ctx    (ctx-move-lidx ctx i (+ i (length (map car (cadr ast)))))) ;; TODO: use ids, but not the one in loop
  ;                             (AA (begin (pp "APRES MOVE") (pp ctx)))
  ;                             (ctx    (ctx-set-id-slot ctx (car ids) (ctx-lidx-to-slot ctx (+ i (length (map car (cadr ast))))))) ;; TODO: use ids, but not the one in loop
  ;                             (BB (begin (pp "APRES SET SLOT") (pp ctx)))
  ;                             (opfrom (codegen-loc-to-x86opnd lfrom))
  ;                             (opto   (codegen-loc-to-x86opnd lto)))
  ;
  ;                        ;; If mobject is in memory, move to rax
  ;                        (cond ((and (ctx-loc-is-memory? lfrom)
  ;                                    (ctx-loc-is-memory? lto))
  ;                                 (error "NYI mlc-letrec"))
  ;                              ((ctx-loc-is-memory? lfrom)
  ;                                 (x86-mov cgc (x86-rax) opfrom)
  ;                                 (set! opfrom (x86-rax)))
  ;                              ((ctx-loc-is-memory? lto)
  ;                                 (x86-mov cgc (x86-rax) opto)
  ;                                 (set! opto (x86-rax))))
  ;
  ;                        (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opto) opfrom)
  ;                        (loop (+ i 1)
  ;                              ctx
  ;                              (cdr ids))))))))
  ;       (lazy-pre
  ;         (make-lazy-code
  ;           (lambda (cgc ctx)
  ;             (let* ((ctx (alloc cgc ids ctx)) ;; Init ids slots with VOID
  ;                    (bind-list (map (lambda (l) (cons (list-ref ids l) l))
  ;                                    (build-list (length ids) (lambda (l) l))))
  ;                    (ctx (ctx-bind ctx bind-list ids))) ;; Bind identifiers to virtual stack slots ;; TODO: mutable vars
  ;               (pp "AVANT LA LECTURE DE LA VARIABLE (OK)")
  ;               (pp ctx)
  ;               (jump-to-version
  ;                 cgc
  ;                 (gen-ast-l (map cadr (cadr ast)) lazy-set)
  ;                 ctx))))))
  ;  lazy-pre))

;; TODO: remove build-env

;; TODO regalloc remove
;(define (mlc-let ast succ)
;  (let* ((ids (map car (cadr ast)))
;         (values (map cadr (cadr ast)))
;         ;; 2 - Update env, ctx, gen mutable and jump to bodies
;         (lazy-let-in
;            (make-lazy-code
;               (lambda (cgc ctx)
;                  (let* ((mvars (mutable-vars ast ids))
;                         (start (- (length (ctx-stack ctx)) (length ids) 1))
;                         (env (build-env mvars ids start (ctx-env ctx)))
;                         (nctx (make-ctx (ctx-stack ctx) env (ctx-nb-args ctx)))
;                         ;; Gen mutable vars
;                         (mctx (gen-mutable cgc nctx mvars)))
;                     ;; Jump to first body
;                     (jump-to-version cgc succ mctx))))))
;      ;; 1 - Gen all bound values
;      (gen-ast-l values lazy-let-in)))

;;-----------------------------------------------------------------------------
;; SPECIAL & PRIMITIVES

;;
;; Make lazy code from special id $$print-flonum
;;
(define (mlc-printflonum ast succ)
  (let ((spec
          (make-lazy-code
            (lambda (cgc ctx)
              (codegen-print-flonum cgc)
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
               (codegen-void cgc)
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

  ;; Manage fake implementation. NOTE: remove when all implemented
  ;; TODO: implement 'char=?'
  (cond ((eq? (car ast) 'char=?)
            (gen-ast (cons 'eq? (cdr ast)) succ))
        (else
          (let* ((special (car ast))
                 (lazy-special
                   (cond ;; EXIT
                         ;; TODO regalloc ok
                         ((eq? special 'exit)
                            (get-lazy-error ""))
                         ;; CONS
                         ;; TODO ragalloc ok
                         ((eq? special 'cons) (mlc-pair succ))
                         ;; NOT
                         ;; TODO regalloc ok
                         ((eq? special 'not)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx))
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-not cgc reg lval)
                                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL reg))))))
                         ;; EQ?
                         ;; TODO regalloc ok
                         ((eq? special 'eq?)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lleft  (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                                     (lright (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-eq? cgc reg lleft lright)
                                (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) CTX_BOOL reg))))))
                         ;; CAR & CDR
                         ;; TODO regalloc ok
                         ((member special '(car cdr))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-car/cdr cgc special reg lval)
                                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_UNK reg))))))
                         ;; SET-CAR! & SET-CDR!
                         ;; TODO regalloc ok
                         ((member special '(set-car! set-cdr!))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
                                     (lpair (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1))))
                               (codegen-scar/scdr cgc special reg lpair lval)
                               (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) CTX_VOID reg))))))
                         ;; CURRENT-INPUT-PORT / CURRENT-OUTPUT-PORT
                         ;; TODO regalloc ok
                         ((member special '(current-input-port current-output-port))
                           (make-lazy-code
                             (lambda (cgc ctx)
                               (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                      (reg (car res))
                                      (ctx (cdr res)))
                                 (codegen-current-io-port cgc special reg)
                                 (jump-to-version cgc succ (ctx-push ctx
                                                                     (if (eq? special 'current-output-port)
                                                                         CTX_OPORT
                                                                         CTX_IPORT)
                                                                     reg))))))
                         ;; CLOSE-INPUT-PORT / CLOSE-OUTPUT-PORT
                         ;; TODO regalloc ok
                         ((member special '(close-output-port close-input-port))
                           (make-lazy-code
                             (lambda (cgc ctx)
                               (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                      (reg (car res))
                                      (ctx (cdr res))
                                      (lport (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                 (codegen-close-io-port cgc reg lport)
                                 (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID reg))))))
                         ;; OPEN-INPUT-FILE / OPEN-OUTPUT-FILE
                         ;; TODO regalloc ok
                         ((member special '(open-output-file open-input-file))
                           (make-lazy-code
                             (lambda (cgc ctx)
                               (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                      (reg (car res))
                                      (ctx (cdr res))
                                      (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                 (codegen-open-io-file cgc special reg lval)
                                 (jump-to-version cgc succ (ctx-push (ctx-pop ctx)
                                                                     (if (eq? special 'open-input-file)
                                                                         CTX_IPORT
                                                                         CTX_OPORT)
                                                                     reg))))))
                         ;; EOF-OBJECT?
                         ;; TODO regalloc ok
                         ((eq? special 'eof-object?)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-eof? cgc reg lval)
                                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL reg))))))
                         ;; READ-CHAR
                         ;; TODO regalloc ok
                         ((eq? special 'read-char)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lport (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-read-char cgc reg lport)
                                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_CHAR reg))))))
                         ;; WRITE-CHAR
                         ;; TODO regalloc ok
                         ((eq? special 'write-char)
                            (make-lazy-code
                              (lambda (cgc ctx)
                                (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                       (reg (car res))
                                       (ctx (cdr res))
                                       (lchar (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                                       (lport (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                  (codegen-write-char cgc reg lchar lport)
                                  (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) CTX_VOID reg))))))
                         ;; CHAR<->INTEGER
                         ;; TODO regalloc ok
                         ((member special '(char->integer integer->char))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-ch<->int cgc special reg lval)
                                (jump-to-version cgc succ (ctx-push (ctx-pop ctx)
                                                                    (if (eq? special 'char->integer)
                                                                        CTX_NUM
                                                                        CTX_CHAR)
                                                                    reg))))))
                         ;; MAKE-STRING
                         ;; TODO regalloc ok
                         ((eq? special 'make-string)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((init-value? (= (length (cdr ast)) 2))
                                     (res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (llen (ctx-get-loc ctx (ctx-lidx-to-slot ctx (if init-value? 1 0))))
                                     (lval (if init-value? (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)) #f)))
                                (codegen-make-string cgc reg llen lval)
                                (jump-to-version cgc succ (ctx-push (if init-value?
                                                                        (ctx-pop-n ctx 2)
                                                                        (ctx-pop ctx))
                                                                    CTX_STR
                                                                    reg))))))
                         ;; MAKE-VECTOR
                         ;; TODO regalloc ok
                         ((eq? special 'make-vector)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((init-value? (= (length (cdr ast)) 2))
                                     (res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (llen (ctx-get-loc ctx (ctx-lidx-to-slot ctx (if init-value? 1 0))))
                                     (lval (if init-value? (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)) #f)))
                                (codegen-make-vector cgc reg llen lval)
                                (jump-to-version cgc succ (ctx-push (if init-value?
                                                                       (ctx-pop-n ctx 2)
                                                                       (ctx-pop ctx))
                                                                    CTX_VECT
                                                                    reg))))))
                         ;; STRING->SYMBOL
                         ;; TODO regalloc ok
                         ((eq? special 'string->symbol)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx))
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lstr (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-str->sym cgc reg lstr)
                                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_SYM reg))))))
                         ;; SYMBOL->STRING
                         ;; TODO regalloc ok
                         ((eq? special 'symbol->string)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx))
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lsym (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                              (codegen-sym->str cgc reg lsym)
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_STR reg))))))
                         ;; VECTOR-LENGTH & STRING-LENGTH
                         ;; TODO regalloc ok
                         ((member special '(vector-length string-length))
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-vec/str-length cgc reg lval)
                                (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_NUM reg))))))
                         ;; VECTOR-REF
                         ;; TODO regalloc ok
                         ((eq? special 'vector-ref)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lvec (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                                     (lidx (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                                (codegen-vector-ref cgc reg lvec lidx)
                                (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) CTX_UNK reg))))))
                         ;; STRING-REF
                         ;; TODO regalloc ok
                         ((eq? special 'string-ref)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lstr (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                                     (lidx (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                              (codegen-string-ref cgc reg lstr lidx)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 2) CTX_CHAR reg))))))
                         ;; VECTOR-SET!
                         ;; TODO regalloc ok
                         ((eq? special 'vector-set!)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
                                     (lidx (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                                     (lvec (ctx-get-loc ctx (ctx-lidx-to-slot ctx 2))))
                                (codegen-vector-set! cgc reg lvec lidx lval)
                                (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 3) CTX_VOID reg))))))
                         ;; STRING-SET!
                         ;; TODO regalloc ok
                         ((eq? special 'string-set!)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
                                     (reg (car res))
                                     (ctx (cdr res))
                                     (lchr (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
                                     (lidx (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                                     (lstr (ctx-get-loc ctx (ctx-lidx-to-slot ctx 2))))
                              (codegen-string-set! cgc reg lstr lidx lchr)
                              (jump-to-version cgc succ (ctx-push (ctx-pop-n ctx 3) CTX_VOID reg))))))
                         ;; LIST
                         ((eq? special 'list)
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (let ((len (length (cdr ast))))
                                (codegen-list cgc len)
                                (jump-to-version cgc
                                                 succ
                                                 (ctx-push (ctx-pop ctx len)
                                                           (if (null? (cdr ast))
                                                               CTX_NULL
                                                               CTX_PAI)))))))
                         ;; OTHERS
                         (else (error "NYI")))))

            (let* ((primitive (assoc (car ast) primitives))
                   ;; Get list of types required by this primitive
                   (types (if (cadr primitive)
                              (cdr (assoc (length (cdr ast))
                                          (cadddr primitive)))
                              (build-list (length (cdr ast)) (lambda (el) CTX_ALL)))))
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

                 (let ((label-false (list-ref stub-labels 0))
                       (label-true  (list-ref stub-labels 1)))
                   ;; Gen code
                   (let* ((lcond (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                     (codegen-if cgc label-jump label-false label-true lcond))))))))
    (gen-ast
      (cadr ast)
      lazy-code-test)))

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
              (let ((label-g (asm-make-label #f (new-sym 'label-apply-nargs-g)))
                    (label-e (asm-make-label #f (new-sym 'label-apply-nargs-e))))
                (x86-mov cgc (x86-rdi) (x86-r11)) ;; Copy nb args in rdi
                (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (length args-regs))))
                (x86-jg cgc label-g)
                  (x86-lea cgc base-ptr (x86-mem 16 (x86-rsp)))
                  (x86-jmp cgc label-e)
                (x86-label cgc label-g)
                  (x86-lea cgc base-ptr (x86-mem (- 16 (* 8 (length args-regs)))
                                                 (x86-rsp)
                                                 (x86-rdi)
                                                 1))
                (x86-label cgc label-e)
                (gen-call-sequence ast cgc #f #f)))))
        (lazy-args
          (make-lazy-code
            (lambda (cgc ctx)
              ;; rax = lst
              (let* ((label-end (asm-make-label #f (new-sym 'apply-end-args)))
                     (llst (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
                     (oplst (codegen-loc-to-x86opnd llst)))
                (x86-mov cgc (x86-rbp) oplst)
                (x86-mov cgc (x86-r11) (x86-imm-int 0))
                (let loop ((args-regs args-regs))
                  (if (null? args-regs)
                      (let ((label-loop (asm-make-label #f (new-sym 'apply-loop-args))))
                        (x86-label cgc label-loop)
                        (x86-cmp cgc (x86-rbp) (x86-imm-int (obj-encoding '())))
                        (x86-je cgc label-end)
                          (x86-add cgc (x86-r11) (x86-imm-int 4))
                          (x86-mov cgc (x86-r14) (x86-mem (- 8 TAG_MEMOBJ) (x86-rbp)))
                          (x86-push cgc (x86-r14))
                          (x86-mov cgc (x86-rbp) (x86-mem (- 16 TAG_MEMOBJ) (x86-rbp)))
                          (x86-jmp cgc label-loop))
                      (begin
                        (x86-cmp cgc (x86-rbp) (x86-imm-int (obj-encoding '())))
                        (x86-je cgc label-end)
                          (x86-add cgc (x86-r11) (x86-imm-int 4))
                          (x86-mov cgc (codegen-loc-to-x86opnd (car args-regs)) (x86-mem (- 8 TAG_MEMOBJ) (x86-rbp)))
                          (x86-mov cgc (x86-rbp) (x86-mem (- 16 TAG_MEMOBJ) (x86-rbp)))
                        (loop (cdr args-regs)))))
                (x86-label cgc label-end)
                (jump-to-version cgc lazy-call ctx)))))
        (lazy-closure
          (make-lazy-code
            (lambda (cgc ctx)
              (let* ((lclo (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                     (opclo (codegen-loc-to-x86opnd lclo)))
                (x86-mov cgc (x86-rax) opclo) ;; closure need to be in rax for do-callback-fn (TODO: get closure from stack in do-callback-fn and remove this)
                (x86-push cgc (x86-rax))
                (jump-to-version cgc lazy-args ctx)))))
        (lazy-retaddr
          (make-lazy-code
            (lambda (cgc ctx)

              (let* ((free (ctx-free-regs ctx))
                     (all  (build-list (length regalloc-regs) (lambda (i)
                                                         (string->symbol
                                                           (string-append "r" (number->string i))))))
                     (save (set-sub all free '())))
                ;; Save used regs
                (for-each
                  (lambda (i)
                    (let ((opnd (codegen-reg-to-x86reg i)))
                      (x86-push cgc opnd)))
                  save)
                (x86-push cgc base-ptr)

                (if opt-return-points
                    (gen-continuation-cr cgc ast succ ctx save)
                    (gen-continuation-rp cgc ast succ ctx save #t))
                (jump-to-version cgc lazy-closure ctx))))))

    (let ((lazy-lst (gen-ast (caddr ast) lazy-retaddr)))
      (gen-ast (cadr ast) lazy-lst))))
  ;(make-lazy-code
  ;  (lambda (cgc ctx)
  ;    ;; 2 - push all regs
  ;    (if (not tail)
  ;        (let* ((free (ctx-free-regs ctx))
  ;               (all  (build-list (length regalloc-regs) (lambda (i)
  ;                                                          (string->symbol
  ;                                                            (string-append "r" (number->string i))))))
  ;               (save (set-sub all free '())))
  ;          ;; Save used regs
  ;          (for-each
  ;            (lambda (i)
  ;              (let ((opnd (codegen-reg-to-x86reg i)))
  ;                (x86-push cgc opnd)))
  ;            save)
  ;          (x86-push cgc base-ptr)
  ;          ;; Gen continuation
  ;          (if opt-return-points
  ;              (gen-continuation-cr cgc ast succ ctx save)
  ;              (gen-continuation-rp cgc ast succ ctx save)))))))

;;
;; Make lazy code from CALL EXPR
;;
(define (mlc-call ast succ)

  (let* (;; Tail call if successor's flags set contains 'ret flag
         (tail #f) ;(member 'ret (lazy-code-flags succ))) ;; TODO TAIL CALL WITH ARGS REGS
         ;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (get-lazy-error (ERR_TYPE_EXPECTED CTX_CLO)))
         ;; Lazy call
         (lazy-call (make-lazy-code (lambda (cgc ctx)

                                        ;; 1 - Build call ctx
                                        (let ((call-ctx
                                                (ctx-init-with-stack
                                                  (append (list-head (ctx-stack ctx) (length (cdr ast)))
                                                          (list CTX_CLO CTX_RETAD)))))

                                          ;; 2 - push all regs
                                          (if (not tail)
                                              (let* ((free (ctx-free-regs ctx))
                                                     (all  (build-list (length regalloc-regs) (lambda (i)
                                                                                         (string->symbol
                                                                                           (string-append "r" (number->string i))))))
                                                     (save (set-sub all free '())))
                                                ;; Save used regs
                                                (for-each
                                                  (lambda (i)
                                                    (let ((opnd (codegen-reg-to-x86reg i)))
                                                      (x86-push cgc opnd)))
                                                  save)
                                                (x86-push cgc base-ptr)
                                                ;; Gen continuation
                                                (if opt-return-points
                                                    (gen-continuation-cr cgc ast succ ctx save)
                                                    (gen-continuation-rp cgc ast succ ctx save #f))))

                                          ;; 4 TODO closure slot
                                          (let* ((lclo (ctx-get-loc ctx (ctx-lidx-to-slot ctx (length (cdr ast)))))
                                                 (opclo (codegen-loc-to-x86opnd lclo)))
                                            (x86-mov cgc (x86-rax) opclo) ;; closure need to be in rax for do-callback-fn (TODO: get closure from stack in do-callback-fn and remove this)
                                            (x86-push cgc (x86-rax)))

                                          ;; 5 - Move args to regs or stack following calling convention
                                          (let* ((stackp/moves (ctx-get-call-args-moves ctx (length args)))
                                                 (stackp (car stackp/moves))
                                                 (moves (cdr stackp/moves)))
                                            (if (not (null? stackp))
                                                (for-each
                                                   (lambda (el)
                                                     (let ((opnd (codegen-loc-to-x86opnd el)))
                                                       (x86-push cgc opnd)))
                                                   stackp))
                                            (for-each
                                                (lambda (el)
                                                  (let ((opnddst (codegen-loc-to-x86opnd (car el)))
                                                        (opndsrc (codegen-loc-to-x86opnd (cdr el))))
                                                    (x86-mov cgc opnddst opndsrc)))
                                                moves) (x86-label cgc (asm-make-label #f (new-sym 'LAB_AP))))

                                          ;; Shift args and closure for tail call
                                          ;; 0 -> (length args) COMPARISON
                                          ;; Use rbx because rax holds closure
                                          (if tail
                                              (let ((nb-args (length args))
                                                    (fs (ctx-fs ctx)))
                                                (let loop ((n (length args)))
                                                  (if (>= n 0)
                                                      (begin
                                                        (x86-mov cgc (x86-rax) (x86-mem (* 8 n) (x86-rsp)))
                                                        (x86-mov cgc (x86-mem (* 8 (+ (- fs 1) n)) (x86-rsp)) (x86-rax))
                                                        (loop (- n 1)))))))

                                          ;; TODO update rsp
                                          (if tail
                                              (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- (ctx-fs ctx) 1)))))

                                          ;; TODO update rbp for callee
                                          (let ((args-fs
                                                   (let ((r (- (length args) (length args-regs))))
                                                     (if (< r 0) 0 r))))
                                            (x86-lea cgc base-ptr (x86-mem (* (+ args-fs 2) 8) (x86-rsp))))

                                          ;; 6 - Gen call sequence
                                          (gen-call-sequence ast cgc call-ctx (length (cdr ast)))))))
         ;; Lazy code object to build the continuation
         (lazy-tail-operator (check-types (list CTX_CLO) (list (car ast)) lazy-call ast)))

    (if #f ;; TODO regalloc
        (if (> (length args) 0)
            ;; If args, then compile args
            (gen-ast-l args lazy-tail-operator)
            ;; Else, compile call
            lazy-tail-operator)
        ;; First object of the chain
        (let ((lazy-operator (check-types (list CTX_CLO) (list (car ast)) (gen-ast-l (cdr ast) lazy-call) ast)))
          lazy-operator))))

;; TODO regalloc: merge -rp and -cr + comments
(define (gen-continuation-rp cgc ast succ ctx saved-regs apply?)

  (let* ((lazy-continuation
           (make-lazy-code
             (lambda (cgc ctx)

               ;; Restore registers
               (x86-pop cgc base-ptr)
               (for-each
                 (lambda (i)
                   (let ((opnd (codegen-reg-to-x86reg i)))
                     (x86-pop cgc opnd)))
                 (reverse saved-regs))

               ;; Move result to location
               (let* ((lres   (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
                      (opres  (codegen-loc-to-x86opnd lres)))
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
                                    (let* ((args (cdr ast))
                                           (ctx
                                             (if apply?
                                                 (ctx-pop-n ctx 2) ;; Pop operator and args
                                                 (ctx-pop-n ctx (+ (length args) 1))))
                                           (res (ctx-get-free-reg cgc ctx))
                                           (reg (car res)))

                                      (set! gen-flag
                                            (gen-version-continuation
                                              load-ret-label
                                              lazy-continuation
                                              (ctx-push ctx CTX_UNK reg)))))
                                gen-flag))))
   ;; Generate code
   (codegen-load-cont-rp cgc load-ret-label (list-ref stub-labels 0))))

;; TODO regalloc: merge -rp and -cr + comments
(define (gen-continuation-cr cgc ast succ ctx saved-regs)

  (let* ((lazy-continuation
           (make-lazy-code
             (lambda (cgc ctx)

               ;; Restore registers
               (x86-pop cgc base-ptr)
               (for-each
                 (lambda (i)
                   (let ((opnd (codegen-reg-to-x86reg i)))
                     (x86-pop cgc opnd)))
                 (reverse saved-regs))

               ;; Move result to location
               (let* ((lres   (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
                      (opres  (codegen-loc-to-x86opnd lres)))
                 ;; Result is in rax
                 (x86-mov cgc opres (x86-rax)))
               (jump-to-version cgc succ ctx))))
         (stub-labels
           (add-cont-callback cgc
                              0
                              (lambda (ret-addr selector type table)
                                     (let* ((args (cdr ast))
                                            (ctx (ctx-pop-n ctx (+ (length args) 1))) ;; Remove closure and args from virtual stack
                                            (res (ctx-get-free-reg cgc ctx))
                                            (reg (car res)))
                                        ; (print "Gen version cont de ")
                                        ; (pp ast)
                                        ; (pp ctx)
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
                   (let* ((label-div0 (get-label-error ERR_DIVIDE_ZERO))
                          (res (ctx-get-free-reg cgc ctx))
                          (reg (car res))
                          (ctx (cdr res))
                          (lleft (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
                          (lright (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
                     (codegen-binop cgc op label-div0 reg lleft lright)
                     (jump-to-version cgc
                                      succ
                                      (ctx-push (ctx-pop (ctx-pop ctx)) CTX_NUM reg)))))))
         ;; Check operands type
         (check-types (list CTX_NUM CTX_NUM)
                      (list (car opnds) (cadr opnds))
                      lazy-op
                      ast)))))

;;
;; Make lazy code from N-ARY OPERATOR
;;
(define (mlc-op-n ast succ op)
  (if (member op '(< > <= >= =))
    (mlc-op-n-cmp ast succ op)
    (mlc-op-n-num ast succ op)))

;;
;; Make lazy code from N-ARY COMPARISON OPERATOR
;;
;; TODO regalloc:
;; (< 1 2 3) ->
;; (let ((a 1) (b 2) (c 3))
;;   (and (< a b) (< b c)))
;; PAREIL POUR OP-N-NUM ?
(define (mlc-op-n-cmp ast succ op)

  ;; Gen lazy code objects chain for binary operation (x op y)
  ;; Build a lco for each node of the type checks tree (with int and float)
  (define (build-binop succ)
    (let* (;; Operations lco
           (lazy-ii (get-op-ii succ))
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
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
               (reg (car res))
               (ctx (cdr res))
               (lright (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
               (lleft  (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1))))
          (codegen-cmp-ii cgc op reg lleft lright)
          (jump-to-version cgc succ (ctx-push (ctx-pop (ctx-pop ctx)) CTX_BOOL reg))))))

  ;; TODO regalloc comment
  (define (get-op-ff succ leftint? rightint?)
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((res (ctx-get-free-reg cgc ctx))
               (reg (car res))
               (ctx (cdr res))
               (lright (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
               (lleft  (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1))))
          (codegen-cmp-ff cgc op reg lleft leftint? lright rightint?)
          (jump-to-version cgc succ (ctx-push (ctx-pop (ctx-pop ctx)) CTX_BOOL reg))))))

  (cond ((<= (length (cdr ast)) 1)
           (gen-ast #t succ))
        ((=  (length (cdr ast)) 2)
           (gen-ast-l (cdr ast) (build-binop succ)))
        (else (error "Internal error (mlc-op-n-cmp)"))))

;(define (mlc-op-n-cmp ast succ op)

  ;;; Create final lazy object. This object clean stack and push 'res'
  ;(define (get-lazy-final res)
  ;  (make-lazy-code
  ;    (lambda (cgc ctx)
  ;      (codegen-cmp-end cgc (- (length ast) 1) res)
  ;      (jump-to-version cgc succ (ctx-push (ctx-pop ctx (- (length ast) 1))
  ;                                          CTX_BOOL)))))
  ;
  ;;; Gen false stub from jump-label & ctx and return stub label
  ;(define (get-stub-label label-jump ctx)
  ;  (list-ref (add-callback #f 0 (lambda (ret-addr selector)
  ;                                 (gen-version (asm-label-pos label-jump)
  ;                                              (get-lazy-final #f)
  ;                                              ctx)))
  ;            0))
  ;
  ;;; Build chain for all operands
  ;(define (build-chain lidx ridx)
  ;  (if (or (< lidx 0) (< ridx 0))
  ;      ;; All operands compared
  ;      (get-lazy-final #t)
  ;      ;; There is at least 1 comparison to perform
  ;      (build-bincomp (build-chain (- lidx 1) (- ridx 1)) lidx ridx)))
  ;
  ;;; Gen lazy code objects chain for binary comparison (x op y)
  ;;; Build a lco for each node of the type checks tree (with int and float)
  ;(define (build-bincomp succ lidx ridx)
  ;  (let* (;; Operations lco
  ;         (lazy-ii (get-comp-ii succ lidx ridx))       ;; lco for int int operation
  ;         (lazy-if (get-comp-ff succ lidx ridx #t #f)) ;; lco for int float operation
  ;         (lazy-fi (get-comp-ff succ lidx ridx #f #t)) ;; lco for float int operation
  ;         (lazy-ff (get-comp-ff succ lidx ridx #f #f)) ;; lco for float float operation
  ;         ;; Right branch
  ;         (lazy-yfloat2 (gen-fatal-type-test CTX_FLO ridx lazy-ff ast))
  ;         (lazy-yint2   (gen-dyn-type-test CTX_NUM ridx lazy-fi lazy-yfloat2 ast))
  ;         (lazy-xfloat  (gen-fatal-type-test CTX_FLO lidx lazy-yint2 ast))
  ;         ;; Left branch
  ;         (lazy-yfloat  (gen-fatal-type-test CTX_FLO ridx lazy-if ast))
  ;         (lazy-yint    (gen-dyn-type-test CTX_NUM ridx lazy-ii lazy-yfloat ast))
  ;         ;; Root node
  ;         (lazy-xint    (gen-dyn-type-test CTX_NUM lidx lazy-yint lazy-xfloat ast)))
  ;    lazy-xint))
  ;
  ;;; Get lazy code object for comparison with int and int
  ;(define (get-comp-ii succ lidx ridx)
  ;  (make-lazy-code
  ;    (lambda (cgc ctx)
  ;      (codegen-cmp-ii cgc ctx op lidx ridx get-stub-label)
  ;      (jump-to-version cgc succ ctx))))
  ;
  ;;; Get lazy code object for comparison with float and float, float and int, and int and float
  ;;; leftint?  to #t if left operand is an integer
  ;;; rightint? to #t if right operand is an integer
  ;(define (get-comp-ff succ lidx ridx leftint? rightint?)
  ;  (make-lazy-code
  ;    (lambda (cgc ctx)
  ;      (codegen-cmp-ff cgc ctx op lidx ridx get-stub-label leftint? rightint?)
  ;      (jump-to-version cgc succ ctx))))
  ;
  ;;; Push operands and start comparisons
  ;(gen-ast-l (cdr ast)
  ;           (build-chain (- (length ast) 2)
  ;                        (- (length ast) 3))))

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
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
               (reg (car res))
               (ctx (cdr res))
               (lright (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
               (lleft  (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1))))
        (codegen-num-ii cgc op reg lleft lright)
        (jump-to-version cgc succ (ctx-push (ctx-pop (ctx-pop ctx)) CTX_NUM reg))))))

  ;; Get lazy code object for operation with float and float, float and int, and int and float
  ;; leftint?  to #t if left operand is an integer
  ;; rightint? to #t if right operand is an integer
  (define (get-op-ff succ leftint? rightint?)
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
               (reg (car res))
               (ctx (cdr res))
               (lright (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0)))
               (lleft  (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1))))
          (codegen-num-ff cgc op reg lleft leftint? lright rightint?)
          (jump-to-version cgc succ (ctx-push (ctx-pop (ctx-pop ctx)) CTX_FLO reg))))))

  (cond ((= (length ast) 1)
           (cond ((eq? op '+) (gen-ast 0 succ))
                 ((eq? op '-) (get-lazy-error ERR_WRONG_NUM_ARGS))
                 ((eq? op '*) (gen-ast 1 succ))
                 (else (error "Unknown operator " op))))
        ((and (= (length ast) 2) (eq? op '-))
           (gen-ast (list '* -1 (cadr ast)) succ))
        ((and (= (length ast) 2) (member op '(< > <= >= =))) ;; TODO: useless ?
           (gen-ast #t succ))
        (else
           (let ((dummy
                   (make-lazy-code
                       (lambda (cgc ctx)
                         (jump-to-version cgc (gen-ast (cadr ast) (build-chain (cddr ast))) ctx)))))
            dummy))))


;;
;; Make lazy code from TYPE TEST
;;
(define (mlc-test ast succ)

  (define (get-lazy-res bool)
    (make-lazy-code
      (lambda (cgc ctx)
        (let* ((res (ctx-get-free-reg cgc ctx))
               (reg (car res))
               (ctx (cdr res)))
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
(define (mlc-pair succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (let* ((res (ctx-get-free-reg cgc ctx)) ;; Return reg,ctx
             (reg (car res))
             (ctx (cdr res))
             (lcar (ctx-get-loc ctx (ctx-lidx-to-slot ctx 1)))
             (lcdr (ctx-get-loc ctx (ctx-lidx-to-slot ctx 0))))
      (codegen-pair cgc reg lcar lcdr)
      (jump-to-version cgc
                       succ
                       (ctx-push (ctx-pop (ctx-pop ctx)) CTX_PAI reg))))))

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

;;
;; VARIABLE GET
;;

;;
;; FREE VARS
;;

(define (gen-free-vars cgc ids ctx offset)
  (if (null? ids)
      '()
      (let* ((identifier (cdr (assoc (car ids) (ctx-env ctx))))
             (loc (ctx-identifier-loc ctx identifier #t))
             (opn
               (cond ;; No loc, free variable which is only in closure
                     ((ctx-loc-is-floc? loc)
                       (let* (;; Get closure loc
                              (closure-lidx (- (length (ctx-stack ctx)) 2))
                              (closure-loc  (ctx-get-loc ctx (ctx-lidx-to-slot ctx closure-lidx)))
                              (closure-opnd (codegen-loc-to-x86opnd closure-loc))
                              ;; Get free var offset
                              (fvar-pos (ctx-floc-to-fpos loc))
                              (fvar-offset (+ 16 (* 8 fvar-pos)))) ;; 16:header,entrypoint -1: pos starts from 1 and not 0
                         (if (ctx-loc-is-memory? closure-loc)
                             (begin (x86-mov cgc (x86-rax) closure-opnd)
                                    (set! closure-opnd (x86-rax))))
                         (x86-mov cgc (x86-rax) (x86-mem (- fvar-offset TAG_MEMOBJ) closure-opnd))
                         (x86-rax)))
                     ;;
                     ((ctx-loc-is-memory? loc)
                       (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd loc))
                       (x86-rax))
                     ;;
                     (else
                       (codegen-reg-to-x86reg loc)))))
        (x86-mov cgc (x86-mem (+ 16 (* offset 8)) alloc-ptr) opn)
        (gen-free-vars cgc (cdr ids) ctx (+ offset 1)))))

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

        (codegen-mutable cgc loc)
        (gen-mutable cgc ctx (cdr mvars)))))

;; Return label of a stub generating error with 'msg'
(define (get-label-error msg) (list-ref (add-callback #f   0 (lambda (ret-addr selector) (error msg))) 0))
