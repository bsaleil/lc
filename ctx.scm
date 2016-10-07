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

(define mem-header #f)
(define regalloc-regs #f)
(define lazy-code-flags #f)
(define mem-allocated-kind #f)

;;-----------------------------------------------------------------------------
;; Ctx

;; Compilation context
(define-type ctx
  stack     ;; virtual stack of types
  slot-loc  ;; alist which associates a virtual stack slot to a location
  free-regs ;; list of current free virtual registers
  free-mems ;; list of current free memory slots
  env       ;; alist which associates a variable symbol to an identifier object
  nb-actual ;;
  nb-args   ;; number of arguments of function of the current stack frame
  fs        ;; current frame size
  fn-num    ;; fn-num of current function
)

(define (ctx-copy ctx #!optional stack slot-loc free-regs free-mems env nb-actual nb-args fs fn-num)
  (make-ctx
    (or stack      (ctx-stack ctx))
    (or slot-loc   (ctx-slot-loc ctx))
    (or free-regs  (ctx-free-regs ctx))
    (or free-mems  (ctx-free-mems ctx))
    (or env        (ctx-env ctx))
    (or nb-actual  (ctx-nb-actual ctx))
    (or nb-args    (ctx-nb-args ctx))
    (or fs         (ctx-fs ctx))
    (or fn-num     (ctx-fn-num ctx))))

;; Return ctx that only contains regalloc information
(define (ctx-rm-regalloc ctx)
  (ctx-copy ctx #f 0 0 0 #f #f #f 0))

;; Generate initial free regs list
(define (ctx-init-free-regs)
  (build-list (length regalloc-regs) (lambda (i) (cons 'r i))))

;; Create an empty context
(define (ctx-init)
  (make-ctx '()
            '()
            (ctx-init-free-regs)
            '()
            '()
            #f
            -1
            0
            #f))

;;-----------------------------------------------------------------------------
;; Ctx types

;; Represent all ctx-types
;; sym must be different for each type because it is used to test if two types
;; represents the same type (symbol, integer, etc...)
(define-type ctx-type
  extender: define-ctx-type
  sym
  mem-allocated?
  is-cst
  cst)

;; Define a new ctx type based on ctx-type
;; (def-ctx-type closure #f ident) expand to:
;;   (define-ctx-type ctx-tclo constructor: ctx-tclo* ident)
;;   (define (make-ctx-tclo #!optional ident) (make-ctx-tclo* 'closure #f ident))
(define-macro (def-ctx-type sym mem-allocated? . fields)
  (let* ((short (substring (symbol->string sym) 0 3))
         (typename  (string->symbol (string-append "ctx-t" short)))
         (typector  (string->symbol (string-append "make-ctx-t" short)))
         (typector* (string->symbol (string-append "make-ctx-t" short "*")))
         (typepred  (string->symbol (string-append "ctx-t" short "?"))))
  `(begin (define-ctx-type ,typename constructor: ,typector* ,@fields)
          (define (,typector #!optional is-cst cst ,@fields)
            (,typector* (quote ,sym) ,mem-allocated? is-cst cst ,@fields))
          (set! ctx-type-ctors
                (cons (cons ,typepred ,typector) ctx-type-ctors)))))

;; associate ctx type predicate to ctx type constructor
;; (filled by def-ctx-type macro)
(define ctx-type-ctors `())

;; Define all used ctx-types
(def-ctx-type all     #f) ;; Special type used for primitives
(def-ctx-type number  #f) ;; Special type used for primitives
(def-ctx-type unknown #f)
(def-ctx-type char    #f)
(def-ctx-type void    #f)
(def-ctx-type null    #f)
(def-ctx-type retaddr #f)
(def-ctx-type integer #f)
(def-ctx-type boolean #f)
(def-ctx-type box     #t)
(def-ctx-type pair    #t)
(def-ctx-type vector  #t)
(def-ctx-type string  #t)
(def-ctx-type symbol  #t)
(def-ctx-type iport   #t)
(def-ctx-type float   #t)
(def-ctx-type oport   #t)
(def-ctx-type closure #t)

(define (ctx-type-ctor t)
  (let loop ((l ctx-type-ctors))
    (let ((pred (caar l)))
      (if (pred t)
          (cdar l)
          (loop (cdr l))))))

;; Check if two ctx-type objects represent the same type
(define (ctx-type-teq? t1 t2)
  (eq? (ctx-type-sym t1)
       (ctx-type-sym t2)))

;; Return a new type instance without any constant information
(define (ctx-type-nocst t)
  (if (ctx-type-is-cst t)
      (let ((ctor (ctx-type-ctor t)))
        (ctor))
      t))

;; Build and return a ctx type from a literal
(define (literal->ctx-type l)
  (cond
    ((char?    l) (make-ctx-tcha #t l))
    ((null?    l) (make-ctx-tnul #t l))
    ((fixnum?  l) (make-ctx-tint #t l))
    ((boolean? l) (make-ctx-tboo #t l))
    ((pair?    l) (make-ctx-tpai #t l))
    ((vector?  l) (make-ctx-tvec #t l))
    ((string?  l) (make-ctx-tstr #t l))
    ((symbol?  l) (make-ctx-tsym #t l))
    ((flonum?  l) (make-ctx-tflo #t l))
    (else (pp l) (error "Internal error (literal->ctx-type)"))))

;; CTX IDENTIFIER LOC
;; Return best loc for identifier. (Register if available, memory otherwise)
(define (ctx-identifier-loc ctx identifier)

  (define (get-best-loc slot-loc sslots mloc)
    (if (null? slot-loc)
        (and (assert mloc "Internal error (ctx-identifier-loc)") mloc)
        (let ((sl (car slot-loc)))
          (if (member (car sl) sslots)
              (if (ctx-loc-is-register? (cdr sl))
                  (cdr sl)
                  (get-best-loc (cdr slot-loc) sslots (cdr sl)))
              (get-best-loc (cdr slot-loc) sslots mloc)))))

  (let ((sslots (identifier-sslots identifier)))
    (if (null? sslots)
        ;; Free var
        (begin
          (assert (eq? (identifier-kind identifier) 'free) "Internal error (ctx-identifier-loc)")
          (identifier-cloc identifier))
        (get-best-loc (ctx-slot-loc ctx) sslots #f))))

;;
(define (ctx-fs-inc ctx)
  (ctx-copy ctx #f #f #f #f #f #f #f (+ (ctx-fs ctx) 1)))

(define (ctx-fs-update ctx fs)
  (ctx-copy ctx #f #f #f #f #f #f #f fs))

;;
;; CTX INIT CALL
(define (ctx-init-call ctx nb-args)

  (define (get-stack)
    (let loop ((head (list-head (ctx-stack ctx) nb-args)))
      (if (null? head)
          (list (make-ctx-tclo) (make-ctx-tret))
          (let ((first (car head)))
            (cons (ctx-type-nocst first)
                  (loop (cdr head)))))))

  (ctx-copy
    (ctx-init)
    (get-stack)))

;;
;; GENERIC
;; TODO wip
(define (ctx-generic ctx)

  (define slot-loc (ctx-slot-loc ctx))
  (define free-regs (ctx-free-regs ctx))
  (define free-mems (ctx-free-mems ctx))
  (define fs (ctx-fs ctx))
  (define stack #f)

  (define (change-loc slot-loc slot loc)
    (if (null? slot-loc)
        (error "Internal error")
        (let ((sl (car slot-loc)))
          (if (eq? (car sl) slot)
              (cons (cons slot loc) (cdr slot-loc))
              (cons sl (change-loc (cdr slot-loc) slot loc))))))

  (define (set-new-loc! slot)
    (cond ((not (null? free-regs))
             (set! slot-loc (change-loc slot-loc slot (car free-regs)))
             (set! free-regs (cdr free-regs)))
          (else
             (error "NYI1"))))

  (define (compute-stack stack slot)
    (if (null? stack)
        '()
        (let ((first (car stack)))
          (if (ctx-type-is-cst first)
              (set-new-loc! slot))
          (cons (make-ctx-tunk) (compute-stack (cdr stack) (- slot 1))))))

  (define (compute-env env)
    (if (null? env)
        '()
        (let ((first (car env)))
          (if (identifier-stype (cdr first))
              ;; there is a stype in identifier
              (let ((new-stype (make-ctx-tunk)))
                (if (null? (identifier-sslots (cdr first)))
                    ;; identifier-sslots is null, it's a constant
                    (let ((slot (length slot-loc)))
                      (assert (or (ctx-type-is-cst (identifier-stype (cdr first)))
                                  (eq? (identifier-kind (cdr first)) 'free))
                              "Internal error r")
                      (set! stack (cons (make-ctx-tunk) stack))
                      (set! slot-loc (cons (cons slot #f) slot-loc))
                      (set-new-loc! slot)
                      (cons (cons (car first)
                                  (identifier-copy (cdr first) #f (list slot) #f new-stype))
                            (compute-env (cdr env))))
                    ;; identifier-sslots is not null, only remove type information
                    (cons (cons (car first)
                                (identifier-copy (cdr first) #f #f #f new-stype))
                          (compute-env (cdr env)))))
              ;; no stype in identifier
              (cons first (compute-env (cdr env)))))))

  (assert (not (findDuplicates (ctx-slot-loc ctx)
                               (lambda (a b) (and (cdr a) (cdr b) (eq? (cdr a) (cdr b))))))
          "NYI3")


  (set! stack (compute-stack (ctx-stack ctx) (- (length (ctx-stack ctx)) 1)))

  (let ((env   (compute-env   (ctx-env ctx))))
    (ctx-copy ctx stack slot-loc free-regs free-mems env)))

;;
;; CTX INIT FN
(define (ctx-init-fn stack enclosing-ctx args free-vars late-fbinds fn-num bound-id)

  ;; Separate constant and non constant free vars
  ;; Return a pair with const and nconst sets
  ;; const contains id and type of all constant free vars
  ;; nconst contains id and type of all non constant free vars
  (define (find-const-free ids)
    (let loop ((ids ids)
               (const '())
               (nconst '()))
      (if (null? ids)
          (cons const (reverse nconst)) ;; Order is important for non cst free variables!
          (let* ((id (car ids))
                 (late? (member id late-fbinds))
                 (enc-identifier (and (not late?) (cdr (ctx-ident enclosing-ctx id))))
                 (enc-type       (and (not late?) (ctx-identifier-type enclosing-ctx enc-identifier)))
                 (cst?           (and (not late?) (ctx-type-is-cst enc-type))))
            (if cst?
                (loop (cdr ids) (cons (cons id enc-type) const) nconst)
                (loop (cdr ids) const (cons (cons id enc-type) nconst)))))))

  ;;
  ;; STACK
  (define (init-stack stack)
    (let loop ((i (length args))
               (stack stack))
      (if (= i 0)
          stack
          (cons (ctx-type-nocst (car stack))
                (loop (- i 1) (cdr stack))))))

  ;;
  ;; FREE REGS
  (define (init-free-regs)
    (let* ((all (ctx-init-free-regs))
           (used-args
             (if (<= (length args) (length args-regs))
                 (list-head args-regs (length args))
                 args-regs))
           (used (cons '(r . 2) used-args)))
      (set-sub all used '())))

  ;;
  ;; ENV
  (define (init-env)
    (let* ((r (find-const-free free-vars))
           (free-const (car r))
           (free-nconst (cdr r)))
      (append (init-env-free free-nconst)
              (init-env-free-const free-const)
              (init-env-local))))

  (define (init-env-local)
    (define (init-env-local-h ids slot)
      (if (null? ids)
          '()
          (let* ((id (car ids))
                 (identifier
                   (make-identifier
                     'local (list slot) '() #f #f #f)))
            (cons (cons id identifier)
                  (init-env-local-h (cdr ids) (+ slot 1))))))
    (init-env-local-h args 2))

  (define (init-env-free-const free-const)
    (if (null? free-const)
        '()
        (let ((first (car free-const)))
          (cons (cons (car first)
                      (make-identifier 'local '() '(cst) (cdr first) #f (eq? (car first) bound-id)))
                (init-env-free-const (cdr free-const))))))

  (define (init-env-free free-vars)
    (define (init-env-free-h ids nvar)
      (if (null? ids)
          '()
          (let* ((id         (caar ids))
                 (enc-type   (or (cdar ids) (make-ctx-tclo))) ;; enclosing type, or #f if late
                 (late?      (member id late-fbinds))
                 (identifier (make-identifier 'free '() '() enc-type (cons 'f nvar) (eq? id bound-id))))
            (cons (cons id identifier)
                  (init-env-free-h (cdr ids) (+ nvar 1))))))
    (init-env-free-h free-vars 0))

  ;;
  ;; SLOT-LOC
  (define (init-slot-loc)
    (append
      (reverse (init-slot-loc-local 1 args-regs 2 0)) ;; Reverse for best display for debug purposes
      (init-slot-loc-base)))

  (define (init-slot-loc-local mem avail-regs slot nvar)
    (if (= nvar (length args))
        '()
        (if (null? avail-regs)
            (let ((loc (cons 'm mem)))
              (cons (cons slot loc)
                    (init-slot-loc-local (+ mem 1) '() (+ slot 1) (+ nvar 1))))
            (let ((loc (car avail-regs)))
              (cons (cons slot loc)
                    (init-slot-loc-local mem (cdr avail-regs) (+ slot 1) (+ nvar 1)))))))

  (define (init-slot-loc-base)
    '((1 r . 2) (0 m . 0)))

  ;;
  ;; FS
  (define (init-fs nb-args)
    (if (<= nb-args (length args-regs))
        1
        (+ (- nb-args (length args-regs)) 1)))

  ;;
  (make-ctx
    (or (and stack (init-stack stack))
        (append (make-list (length args) (make-ctx-tunk)) (list (make-ctx-tclo) (make-ctx-tret))))
    (init-slot-loc)
    (init-free-regs)
    '()
    (init-env)
    (and stack (- (length stack) 2))
    (length args)
    (init-fs (length args))
    fn-num))

;; TODO WIP MOVE
(define (ctx-loc-used ctx loc . excluded-idx)
  (let loop ((sls (ctx-slot-loc ctx)))
    (cond ((null? sls)
             #f)
          ((and (eq? (cdar sls) loc)
                (not (member (slot-to-stack-idx ctx (car sls))
                             excluded-idx)))
             #f)
          (else
             (loop (cdr sls))))))

;;
;; GET FREE REG
(define (ctx-get-free-reg ctx succ nb-opnds)

  ;; TODO: prefer 'preferred' to 'deep-opnd-reg' ?

  ;; Preferred register is used if it's member of free registers
  ;; 'return-reg' register is preferred if the successor lco is a return lco
  ;; TODO: also use preferred register if a register need to be spilled
  (define preferred
    (if (and succ
             (member 'ret (lazy-code-flags succ)))
        return-reg
        #f))

  (define deep-opnd-reg
    (let loop ((idx (- nb-opnds 1)))
      (if (< idx 0)
          #f
          (let ((r (ctx-get-loc ctx idx)))
            ;; We keep the the loc associated to this opnd if
            ;; (i) it's a register and (ii) this register is not used elsewhere
            (if (and (ctx-loc-is-register? r)
                     (not (ctx-loc-used ctx idx)))
                r
                (loop (- idx 1)))))))

  (define (get-spilled-reg)
    (let ((sl
            (foldr (lambda (el r)
                     (if (and (or (not r) (< (car el) (car r)))
                              (ctx-loc-is-register? (cdr el)))
                         el
                         r))
                   #f
                   (ctx-slot-loc ctx))))
      (assert sl "Internal error (ctx-get-free-reg)")
      (cdr sl)))

  (if deep-opnd-reg
      ;; TODO WIP comment
      (list '() deep-opnd-reg ctx)
      ;; TODO WIP comment
      (let ((free-regs (ctx-free-regs ctx)))
        (if (null? free-regs)
            (let* ((moves/mloc/ctx (ctx-get-free-mem ctx))
                   (moves (car moves/mloc/ctx))
                   (mloc  (cadr moves/mloc/ctx))
                   (ctx   (caddr moves/mloc/ctx))
                   (spill-reg (get-spilled-reg))
                   (reg-slots (ctx-get-slots ctx spill-reg)))
              ;; 1: changer tous les slots pour r -> m
              (let ((ctx (ctx-set-loc-n ctx reg-slots mloc))
                    (moves (append moves
                                   (list (cons spill-reg mloc)))))

                (list moves spill-reg ctx)))
            (let* ((r (and preferred (member preferred free-regs)))
                   (reg (if r (car r) (car free-regs)))
                   (free (set-sub free-regs (list reg) '())))
              (list '()
                    reg
                    (ctx-copy ctx #f #f free)))))))

;;
;;
(define (ctx-get-eploc ctx id)

  (let ((r (assoc id (ctx-env ctx))))
    (or
      ;; Constant closure
      (and r
           (ctx-tclo? (identifier-stype (cdr r)))
           (ctx-type-is-cst (identifier-stype (cdr r)))
           (cons #t (ctx-type-cst (identifier-stype (cdr r)))))
      ;; This id
      (and r
           (identifier-thisid (cdr r))
           (let ((r (ctx-fn-num ctx)))
             (if r
                 (cons #f r)
                 #f))))))

;;
;; TODO: change to bind-top
(define (ctx-id-add-idx ctx id stack-idx)
  (define slot (stack-idx-to-slot ctx stack-idx))
  (define (build-env env)
    (if (null? env)
        (error "Internal error")
        (if (eq? (caar env) id)
            (cons (cons id
                        (identifier-copy (cdar env) #f (cons slot (identifier-sslots (cdar env)))))
                  (cdr env))
            (cons (car env)
                  (build-env (cdr env))))))

  (let ((env (build-env (ctx-env ctx))))

    (ctx-copy ctx #f #f #f #f env)))

;;
;; BIND CONSTANTS
(define (ctx-bind-consts ctx cst-set)

  (define (build-env cst-set env)
    (if (null? cst-set)
        env
        (let ((id  (caar cst-set))
              (cst (cdar cst-set)))
          (build-env (cdr cst-set)
                     (cons (cons id
                                 (make-identifier
                                   'local
                                   '()
                                   '(cst)
                                   (make-ctx-tclo #t cst)
                                   #f
                                   #f))
                           env)))))

  (let ((env (build-env cst-set (ctx-env ctx))))
   (ctx-copy ctx #f #f #f #f env)))

;;
;; BIND LOCALS
(define (ctx-bind-locals ctx id-idx #!optional letrec-bind?)

  (define (clean-env env bound-slots)
    (if (null? env)
        '()
        (let* ((ident (car env))
               (idslots (identifier-sslots (cdr ident))))
          (cons (cons (car ident)
                      (identifier-copy (cdr ident) #f (set-sub idslots bound-slots '())))
                (clean-env (cdr env) bound-slots)))))

  (define (gen-env env id-idx)
    (if (null? id-idx)
        env
        (let ((first (car id-idx)))
          (cons (cons (car first)
                      (make-identifier
                        'local   ;; symbol 'free or 'local
                        (if letrec-bind?
                            '()
                            (list (stack-idx-to-slot ctx (cdr first))))
                        '()
                        #f
                        #f
                        #f))
                (gen-env env (cdr id-idx))))))

  (let* ((env
           (if letrec-bind?
               (ctx-env ctx)
               (clean-env (ctx-env ctx)
                          (map (lambda (el) (stack-idx-to-slot ctx el))
                               (map cdr id-idx)))))
         (env
           (gen-env env id-idx)))

    (ctx-copy ctx #f #f #f #f env)))

;; This is one of the few ctx function with side effect!
;; The side effect is used to update letrec constant bindings
(define (ctx-cst-fnnum-set! ctx id fn-num)
  (let* ((r (assoc id (ctx-env ctx))))
    (assert (and r
                 (ctx-type-is-cst (identifier-stype (cdr r)))
                 (ctx-tclo?       (identifier-stype (cdr r))))
            "Internal error (ctx-cst-fnnum-set!)")
    (let ((stype (identifier-stype (cdr r))))
      (ctx-type-cst-set! stype fn-num)
      ctx)))

;;
;; UNBIND LOCALS
(define (ctx-unbind-locals ctx ids)

  (define (gen-env env ids)
    (if (null? env)
        (begin (assert (null? ids) "Internal error (ctx-unbind-locals)")
               '())
        (let ((ident (car env)))
          (if (member (car ident) ids)
              (gen-env (cdr env) (set-sub ids (list (car ident)) '()))
              (cons ident
                    (gen-env (cdr env) ids))))))

  (ctx-copy ctx #f #f #f #f (gen-env (ctx-env ctx) ids)))

;;
;; IDENTIFIER TYPE
(define (ctx-identifier-type ctx identifier)

  (let ((stype (identifier-stype identifier)))
    (if stype
        stype
        (let* ((sslots (identifier-sslots identifier))
               (sidx (slot-to-stack-idx ctx (car sslots))))
          (list-ref (ctx-stack ctx) sidx)))))

;;
;; SAVE CALL
;; Called when compiling a call site.
;; Compute moves required to save registers.
;; Returns:
;;  moves: list of moves
;;  ctx: updated ctx
(define (ctx-save-call octx idx-start)

  ;; pour chaque slot sur la vstack:
    ;; Si le slot appartient à une variable, et que cette variable à déjà un emplacement mémoire:
      ;; on remplace simplement la loc dans le slot loc, aucun mouvement à générer
    ;; Sinon:
      ;; on récupère un emplacement mémoire vide
      ;; on remplace la loc dans slot-loc
      ;; on retourne le mouvement reg->mem

  (define (save-one curr-idx ctx)
    (let ((loc (ctx-get-loc ctx curr-idx)))
      (if (or (not loc)
              (ctx-loc-is-memory? loc))
          ;; If loc associated to current index is a memory loc, nothing to do
          (cons '() ctx)
          ;; Loc is a register, we need to save it
          (let* ((ident (ctx-ident-at ctx curr-idx))
                 (mloc  (and ident (ctx-ident-mloc ctx ident))))
            (if (and ident mloc)
                ;; Is this slot is associated to a variable, and this variable already have a memory location
                ;; Then, simply update slot-loc set
                (cons '()
                      (ctx-set-loc ctx (stack-idx-to-slot ctx curr-idx) mloc))
                ;; Else, we need a new memory slot
                (let* ((r (ctx-get-free-mem ctx))
                       (moves (car r))
                       (mem (cadr r))
                       (ctx (caddr r))
                       (ctx (ctx-set-loc ctx (stack-idx-to-slot ctx curr-idx) mem)))

                  ;; Remove all 'fs moves
                  (cons (append (set-sub moves (list (assoc 'fs moves)) '())
                                (list (cons loc mem)))
                        ctx)))))))


  (define (save-all curr-idx moves ctx)
    (if (= curr-idx (length (ctx-stack ctx)))
        (let ((nb-new-slots (- (ctx-fs ctx) (ctx-fs octx))))
          (list (cons (cons 'fs nb-new-slots) moves)
                ctx))
        (let ((r (save-one curr-idx ctx)))
          (save-all
            (+ curr-idx 1)
            (append moves (car r))
            (cdr r)))))

  (save-all idx-start '() octx))

;;
;; PUSH
(define (ctx-push ctx type loc #!optional id)

  (define (get-env env id slot)
    (if (null? env)
        '()
        (let ((ident (car env)))
          (if (eq? (car ident) id)
              (cons (cons id
                          (identifier-copy
                            (cdr ident)
                            #f
                            (cons slot (identifier-sslots (cdr ident)))))
                    (cdr env))
              (cons ident
                    (get-env (cdr env) id slot))))))

  ;; If loc is #f, type must be a constant
  (assert (if (not loc)
              (ctx-type-is-cst type)
              #t)
          "Internal error")

  ;; We do *NOT* want non permanent constant in ctx
  (assert (not (and (ctx-type-is-cst type)
                    (##mem-allocated? (ctx-type-cst type))
                    (not (eq? (mem-allocated-kind (ctx-type-cst type)) 'PERM))))
          "Internal error")

  (let* ((slot (length (ctx-stack ctx))))

   (ctx-copy
     ctx
     (cons type (ctx-stack ctx))
     (cons (cons slot loc)
           (ctx-slot-loc ctx))
     (set-sub (ctx-free-regs ctx) (list loc) '())
     (set-sub (ctx-free-mems ctx) (list loc) '())
     (if id
         (get-env (ctx-env ctx) id slot)
         #f)
     #f
     #f
     #f)))

;; TODO: move
;; Is loc 'loc' used in slot-loc set ?
(define (loc-used? loc slot-loc)
  (if (null? slot-loc)
      #f
      (or (equal? (cdar slot-loc) loc)
          (loc-used? loc (cdr slot-loc)))))

;;
;; POP-N
(define (ctx-pop-n ctx n)
  (if (= n 0)
      ctx
      (ctx-pop-n
        (ctx-pop ctx)
        (- n 1))))

;;
;; POP
(define (ctx-pop ctx)

  ;; If one of the positions of an identifier is given slot, remove this slot.
  ;; If this slot is the only position, remove the identifier
  (define (env-remove-slot env slot)
    (if (null? env)
        '()
        (let ((ident (car env)))
          (if (member slot (identifier-sslots (cdr ident)))
              (cons (cons (car ident)
                          (identifier-copy
                            (cdr ident)
                            #f
                            (set-sub (identifier-sslots (cdr ident)) (list slot) '())))
                    (env-remove-slot (cdr env) slot))
              (cons ident (env-remove-slot (cdr env) slot))))))

  ;;
  (let* ((slot (- (length (ctx-stack ctx)) 1))
         (r (assoc-remove slot (ctx-slot-loc ctx)))
         (loc (and (car r) (cdar r)))
         (slot-loc (cdr r)))

    (ctx-copy
      ctx
      (cdr (ctx-stack ctx))                        ;; stack: remove top
      slot-loc                                     ;; slot-loc: remove popped slot
      (if (and loc
               (not (loc-used? loc slot-loc))      ;; free-regs: add popped loc if it's an unused reg
               (ctx-loc-is-register? loc))
          (cons loc (ctx-free-regs ctx))
          #f)
      (if (and loc
               (not (loc-used? loc slot-loc))      ;; free-mems: add popped loc if it's an unused mem
               (ctx-loc-is-memory? loc))
          (cons loc (ctx-free-mems ctx))
          #f)
      (env-remove-slot (ctx-env ctx) slot))))      ;; env: remove popped slot from env

;;
;; GET LOC
(define (ctx-get-loc ctx stack-idx)
  (let* ((slot (stack-idx-to-slot ctx stack-idx))
         (r (assoc slot (ctx-slot-loc ctx))))
    (assert r "Internal error (ctx-get-loc)")
    (cdr r)))

;;
;; Get closure loc
(define (ctx-get-closure-loc ctx)
  (ctx-get-loc ctx (- (length (ctx-stack ctx)) 2)))

;;
;; Get retobj loc
(define (ctx-get-retobj-loc ctx)
  (ctx-get-loc ctx (- (length (ctx-stack ctx)) 1)))

;; Is register?
(define (ctx-loc-is-register? loc)
  (and (pair? loc)
       (eq? (car loc) 'r)))

;; Is memory ?
(define (ctx-loc-is-memory? loc)
  (and (pair? loc)
       (eq? (car loc) 'm)))

;; Is free variable loc ?
(define (ctx-loc-is-freemem? loc)
  (and (pair? loc)
       (eq? (car loc) 'f)))

;;
;; GET TYPE
(define (ctx-get-type ctx stack-idx)
  (list-ref (ctx-stack ctx) stack-idx))

;; GET TYPE FROM ID
;; Return #f if not found, type if found
(define (ctx-id-type ctx id)
  (let ((ident (assoc id (ctx-env ctx))))
    (if ident
        (ctx-identifier-type ctx (cdr ident))
        #f)))

;;
;; SET TYPE
;; Set type of data to 'type'
;; data could be a stack index
;; or an ident (id . identifier) object
(define (ctx-set-type ctx data type change-id-type)

  ;; We do *NOT* want non permanent constant in ctx
  (assert (not (and (ctx-type-is-cst type)
                    (##mem-allocated? (ctx-type-cst type))
                    (not (eq? (mem-allocated-kind (ctx-type-cst type)) 'PERM))))
          "Internal error")

  (let ((ident (or (and (pair? data) (symbol? (car data)) (identifier? (cdr data)) data)
                   (ctx-ident-at ctx data))))
    (if (and change-id-type ident)
        ;; Change for each identifier slot
        (set-ident-type ctx ident type)
        ;; Change only this slot
        (ctx-copy ctx (stack-change-type (ctx-stack ctx) data type)))))

;;
;; GET CALL ARGS MOVES
;; TODO nettoyer
;;; TODO: uniformiser et placer
;; TODO: not 3 & 5 because rdi and R11 are used for ctx, nb-args
;; cloloc is the location of the closure.
;; If cloloc is #f, no need to move the closure to the closure reg.
;; If cloloc is not #f, we add an extra move to required moves set which is closure -> closure reg
(define args-regs '((r . 0) (r . 1) (r . 4) (r . 6) (r . 7) (r . 8))) ;; TODO move
;; TODO: move
;; TODO: we need to use only loc notation to use regs. Here we do not use r9 because r9 is rdx
;;       and rdx already is used in return code sequence. But we SHOULD use r9 instead of rdx directly in lazy-ret
;; Return reg is one of the last registers to increase the chances it is chosen
;; if it is preferred register in ctx-get-free-reg (which is the case each time succ lco is a 'ret lco)
(define return-reg '(r . 8)) ;; TODO move

;;
;;
;;
;;
;; TODO PRIVATE module

;;
;; Return all slots associated to loc
(define (ctx-get-slots ctx loc)
  (foldr (lambda (sl r)
           (if (equal? (cdr sl) loc)
               (cons (car sl) r)
               r))
         '()
         (ctx-slot-loc ctx)))

;;
;; Return ident object from id
(define (ctx-ident ctx id)
 (let ((env (ctx-env ctx)))
   (assoc id env)))

;;
(define (ctx-set-loc-n ctx slots loc)
  (foldr (lambda (slot ctx)
           (ctx-set-loc ctx slot loc))
         ctx
         slots))

;; TODO: use stack idx to match public api
;; Change loc associated to given slot
(define (ctx-set-loc ctx slot loc)

  (define (get-slot-loc slot-loc)
    (if (null? slot-loc)
        '()
        (let ((sl (car slot-loc)))
          (if (eq? (car sl) slot)
              (cons (cons slot loc) (cdr slot-loc))
              (cons sl (get-slot-loc (cdr slot-loc)))))))

  (define (get-free-* slot-loc curr-free old-loc loc check-loc-type)
    (let* ((r (loc-used? old-loc slot-loc))
           (free-set
             (if (or (not (check-loc-type old-loc))
                     r)
                 curr-free
                 (cons old-loc curr-free))))
    (set-sub free-set (list loc) '())))

  (define (get-free-regs slot-loc curr-free old-loc loc)
    (get-free-* slot-loc curr-free old-loc loc ctx-loc-is-register?))
  (define (get-free-mems slot-loc curr-free old-loc loc)
    (get-free-* slot-loc curr-free old-loc loc ctx-loc-is-memory?))

  (let* ((old-loc (cdr (assoc slot (ctx-slot-loc ctx))))
         (slot-loc (get-slot-loc (ctx-slot-loc ctx)))
         (free-regs (get-free-regs slot-loc (ctx-free-regs ctx) old-loc loc))
         (free-mems (get-free-mems slot-loc (ctx-free-mems ctx) old-loc loc)))
    (ctx-copy ctx #f slot-loc free-regs free-mems)))

;; Return a free memory slot
;; Return moves, mloc and updated ctx
;(moves/mem/ctx (ctx-get-free-mem ctx))
(define (ctx-get-free-mem ctx)
  (if (not (null? (ctx-free-mems ctx)))
      ;; An existing memory slot is empty, use it
      (list '() (car (ctx-free-mems ctx)) ctx)
      ;; Alloc a new memory slot
      (let ((mloc (cons 'm (ctx-fs ctx))))
        (list (list (cons 'fs 1))
              mloc
              (ctx-copy ctx #f #f #f (cons mloc (ctx-free-mems ctx)) #f #f #f (+ (ctx-fs ctx) 1))))))


  ;; Si un emplacement mémoire est libre, on le retourne sans rien modifier
  ;; sinon on en alloue un

;; Return memory location associated to ident or #f if no memory slot
(define (ctx-ident-mloc ctx ident)

  (define (get-mloc slot-loc sslots)
    (if (null? sslots)
        #f
        (let ((r (assoc (car sslots) slot-loc)))
          (or (and r (ctx-loc-is-memory? (cdr r)) (cdr r))
              (get-mloc slot-loc (cdr sslots))))))

  (let ((sslots (identifier-sslots (cdr ident))))
    (get-mloc (ctx-slot-loc ctx) sslots)))

(define (stack-change-type stack stack-idx type)
  (append (list-head stack stack-idx) (cons type (list-tail stack (+ stack-idx 1)))))

(define (stack-idx-to-slot ctx stack-idx)
  (- (length (ctx-stack ctx)) stack-idx 1))

(define (slot-to-stack-idx ctx slot)
  (- (length (ctx-stack ctx)) slot 1))

;; Change type for each slots of identifier
(define (set-ident-type ctx ident type)

  (define (change-stack stack sslots)
    (if (null? sslots)
        stack
        (change-stack
          (stack-change-type stack (slot-to-stack-idx ctx (car sslots)) type)
          (cdr sslots))))

  (if (eq? (identifier-stype (cdr ident)) 'free)
      (error "NYI set-ident-type"))

  (let ((sslots (identifier-sslots (cdr ident))))
    (ctx-copy
      ctx
      (change-stack (ctx-stack ctx) sslots))))

(define (ctx-ident-at ctx stack-idx)
  (define (ident-at-slot env slot)
    (if (null? env)
        #f
        (let ((ident (car env)))
          (if (member slot (identifier-sslots (cdr ident)))
              ident
              (ident-at-slot (cdr env) slot)))))
  (ident-at-slot
    (ctx-env ctx)
    (stack-idx-to-slot ctx stack-idx)))

;;
(define (ctx-ids-keep-non-cst ctx ids)
  (keep (lambda (el)
          (let* ((identifier (cdr (assoc el (ctx-env ctx))))
                 (type (ctx-identifier-type ctx identifier)))
            (not (ctx-type-is-cst type))))
        ids))

;;-----------------------------------------------------------------------------
;; Ctx

;; Identifier object
(define-type identifier
  kind   ;; symbol 'free or 'local
  sslots ;; list of stack slots where the variable is
  flags  ;; list of variable
  stype  ;; ctx type (copied to virtual stack)
  cloc   ;; closure slot if free variable
  thisid ;;
)

;; TODO USE IT ! remove all make-ctx which are only copies and use ctx-copy
(define (identifier-copy identifier #!optional kind sslots flags stype cloc thisid)
  (make-identifier
    (or kind   (identifier-kind identifier))
    (or sslots (identifier-sslots identifier))
    (or flags  (identifier-flags identifier))
    (or stype  (identifier-stype identifier))
    (or cloc   (identifier-cloc identifier))
    (or thisid (identifier-thisid identifier))))


;;-----------------------------------------------------------------------------
;; Merge code
;;-----------------------------------------------------------------------------

;; Compute and returns moves needed to merge reg alloc from src-ctx to dst-ctx
(define (ctx-regalloc-merge-moves src-ctx dst-ctx)

  (define (get-req-moves)
    (define sl-dst (ctx-slot-loc dst-ctx))
    (let loop ((sl (ctx-slot-loc src-ctx)))
      (if (null? sl)
          '()
          (let ((first (car sl)))
            (if (cdr first)
                ;; Loc is not #f
                (let ((src (cdr first))
                      (dst (cdr (assoc (car first) sl-dst))))
                  (if (equal? src dst)
                      (loop (cdr sl))
                      (cons (cons src dst) (loop (cdr sl)))))
                ;; Loc is #f
                (let* ((slot (car first))
                       (type (list-ref (ctx-stack src-ctx) (slot-to-stack-idx src-ctx slot))))
                  (assert (ctx-type-is-cst type) "Internal error2")
                  (let* ((dst
                           (cdr (assoc slot (ctx-slot-loc dst-ctx))))
                         (src
                           (if (ctx-tclo? type)
                               (cons 'constfn (ctx-type-cst type))
                               (cons 'const (ctx-type-cst type)))))
                    (cons (cons src dst) (loop (cdr sl))))))))))

  (let* ((req-moves (get-req-moves))
         (moves (steps req-moves))
         (fs-move (cons 'fs (- (ctx-fs dst-ctx)
                               (ctx-fs src-ctx)))))
    (cons fs-move moves)))

(define (ctx-get-call-args-moves ctx nb-args cloloc)

  (define clomove (and cloloc (cons cloloc '(r . 2))))

  (define (get-req-moves curr-idx rem-regs moves pushed)
    (if (< curr-idx 0)
        (cons (reverse pushed) moves)
        (let* ((type (ctx-get-type ctx curr-idx))
               (from
                 (let ((loc (ctx-get-loc ctx curr-idx)))
                   (or loc
                       (cond ((and (ctx-type-is-cst type)
                                   (ctx-tclo? type))
                              (cons 'constfn (ctx-type-cst type)))
                             ((ctx-type-is-cst type)
                                (cons 'const (ctx-type-cst type)))
                             (else
                                (error "Internal error")))))))
          (if (null? rem-regs)
              (get-req-moves (- curr-idx 1) '() moves (cons from pushed))
              (get-req-moves
                (- curr-idx 1)
                (cdr rem-regs)
                (cons (cons from (car rem-regs))
                      moves)
                pushed)))))

  (let ((pushed/moves (get-req-moves (- nb-args 1) args-regs '() '())))

    (cons (car pushed/moves)
          (if clomove
              (steps (append (cdr pushed/moves) (list clomove)))
              (steps (cdr pushed/moves))))))

;; TODO rename
(define (steps required-moves)

  (let loop ((real-moves '())
             (req-moves required-moves))
    (if (null? req-moves)
        real-moves
        (let ((r (step '() req-moves '())))
          (loop (append real-moves (car r)) (cdr r))))))

;; This function takes all required moves (without considering values overwriting)
;; and returns a list of real moves ex. ((r0 . r4) (r4 . r5)) for one step,
;; and the list of moves that remain to be processed.

;; visited-locs: list of visited nodes (source node of a move) during the step
;; moves: list of moves required to merge ctx
;; pending-moves: list of currently computed real moves
;; TODO rename
(define (step visited req-moves pending-moves #!optional src-sym)

  ;; A loc is available if it is not a source of a move in required moves
  ;; We can then directly overwrite its content
  (define (loc-available loc)
    (not (assoc loc req-moves)))

  ;; Update list of required moves, replace each src by its new position
  (define (update-req-moves next-req-moves step-real-moves)
    (if (null? next-req-moves)
        '()
        (let* ((move (car next-req-moves))
               (src (car move))
               (r (assoc src step-real-moves))
               (updated-move
                 (cond ((and r (eq? (cdr r) 'rtmp))
                          (let ((r (assoc 'rtmp step-real-moves)))
                            (cons (cdr r) (cdr move))))
                       (r
                          (cons (cdr r) (cdr move)))
                       (else
                          move))))
            (cons updated-move
                  (update-req-moves (cdr next-req-moves) step-real-moves)))))

  (let* ((move
           (cond ((null? req-moves) #f)
                 (src-sym           (assoc src-sym req-moves))
                 (else              (car req-moves))))
         (src (and move (car move)))
         (dst (and move (cdr move))))

    (cond ;; Case 0: No more move, validate pending moves
          ((not move)
             (cons pending-moves
                   '()))
          ;; Case 2: dst has already been visited, it's a cycle.
          ;;         validate pending moves using temporary register
          ((member dst visited)
             (let ((real-moves
                     (append (cons (cons src 'rtmp)
                                   pending-moves)
                             (list (cons 'rtmp dst))))
                   (req-moves (set-sub req-moves (list move) '())))
               (cons real-moves
                     (update-req-moves req-moves real-moves))))
          ;; Case 1: Destination is free, validate pending moves
          ((loc-available dst)
             (let ((real-moves (cons move
                                     pending-moves))
                   (req-moves  (set-sub req-moves (list move) '())))
               (cons real-moves
                     (update-req-moves req-moves real-moves))))
          ;; Case X: src is dst
          ((equal? src dst)
             (step (cons src visited)
                   (set-sub req-moves (list move) '())
                   (cons move pending-moves)))
          ;; Case 3: dst is an src of an other move,
          ;;         add current move to pending moves and continue with next move
          (else
             (step (cons src visited)
                   (set-sub req-moves (list move) '())
                   (cons move pending-moves)
                   dst)))))
