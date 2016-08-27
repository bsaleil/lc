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

;;-----------------------------------------------------------------------------
;; Ctx

;; Compilation context
(define-type ctx
  stack     ;; virtual stack of types
  slot-loc  ;; alist which associates a virtual stack slot to a location
  free-regs ;; list of current free virtual registers
  free-mems ;; list of current free memory slots
  env       ;; alist which associates a variable symbol to an identifier object
  nb-args   ;; number of arguments of function of the current stack frame
  fs        ;; current frame size
  eploc     ;;
)

;(define (make-regalloc-ctx slot-loc free-regs fs)
;  (make-ctx #f slot-loc free-regs #f #f fs))

;; TODO USE IT ! remove all make-ctx which are only copies and use ctx-copy
(define (ctx-copy ctx #!optional stack slot-loc free-regs free-mems env nb-args fs eploc)
  (make-ctx
    (or stack     (ctx-stack ctx))
    (or slot-loc  (ctx-slot-loc ctx))
    (or free-regs (ctx-free-regs ctx))
    (or free-mems  (ctx-free-mems ctx))
    (or env       (ctx-env ctx))
    (or nb-args   (ctx-nb-args ctx))
    (or fs        (ctx-fs ctx))
    (or eploc     (ctx-eploc ctx))))

;; Return ctx that only contains regalloc information
(define (ctx-rm-regalloc ctx)
  (ctx-copy ctx #f 0 0 0 #f #f 0))

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
  mem-allocated?)

;; Define a new ctx type based on ctx-type
;; (def-ctx-type closure #f ident) expand to:
;;   (define-ctx-type ctx-tclo constructor: ctx-tclo* ident)
;;   (define (make-ctx-tclo ident) (make-ctx-tclo* 'closure #f ident))
(define-macro (def-ctx-type sym mem-allocated? . fields)
  (let* ((short (substring (symbol->string sym) 0 3))
         (typename  (string->symbol (string-append "ctx-t" short)))
         (typector  (string->symbol (string-append "make-ctx-t" short)))
         (typector* (string->symbol (string-append "make-ctx-t" short "*")))
         (typepred  (string->symbol (string-append "ctx-t" short "?"))))
  `(begin (define-ctx-type ,typename constructor: ,typector* ,@fields)
          (define (,typector ,@fields) (,typector* (quote ,sym) ,mem-allocated? ,@fields))
          (set! ctx-type-ctors
                (cons (cons ,typepred ,typector) ctx-type-ctors)))))

;; associate ctx type predicate to ctx type constructor
;; (filled by def-ctx-type macro)
(define ctx-type-ctors `())

;; Define all used ctx-types
(def-ctx-type all     #f)
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

;; Build and return a ctx type from a literal
(define (literal->ctx-type l)
  (cond
    ((char?    l) (make-ctx-tcha))
    ((null?    l) (make-ctx-tnul))
    ((fixnum?  l) (make-ctx-tint))
    ((boolean? l) (make-ctx-tboo))
    ((pair?    l) (make-ctx-tpai))
    ((vector?  l) (make-ctx-tvec))
    ((string?  l) (make-ctx-tstr))
    ((symbol?  l) (make-ctx-tsym))
    ((flonum?  l) (make-ctx-tflo))
    (else (error "Internal error (literal->ctx-type)"))))

;;-----------------------------------------------------------------------------

;;; TODO public api

(define (ctx-generic ctx)
  (error "NYI"))

;; Compute and returns moves needed to merge reg alloc from src-ctx to dst-ctx
(define (ctx-regalloc-merge-moves src-ctx dst-ctx)
  (define sl-dst (ctx-slot-loc dst-ctx))
  (define (get-req-moves)
    (let loop ((sl (ctx-slot-loc src-ctx)))
      (if (null? sl)
          '()
          (let ((first (car sl)))
            (if (cdr first)
                (cons
                  (cons (cdr first)
                        (cdr (assoc (car first) sl-dst)))
                  (loop (cdr sl)))
                (loop (cdr sl)))))))
  (let* ((req (get-req-moves))
         (req-clean ;; remove wrong moves (e.g. '(r1 . r1))
           (foldr (lambda (el r)
                    (if (equal? (car el) (cdr el))
                        r
                        (cons el r)))
                  '()
                  req))
         (moves (steps req-clean))
         (fs-move
           (cons 'fs (- (ctx-fs dst-ctx)
                        (ctx-fs src-ctx)))))
    (cons fs-move moves)))




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
  (ctx-copy ctx #f #f #f #f #f #f (+ (ctx-fs ctx) 1)))

(define (ctx-fs-update ctx fs)
  (ctx-copy ctx #f #f #f #f #f #f fs))

;;
(define (ctx-add-mem-slots ctx nslots)
  (let* ((fs (ctx-fs ctx))
         (new-free (build-list nslots (lambda (n) (cons 'm (+ fs n)))))
         (free-mem (append new-free (ctx-free-mems ctx))))
    (ctx-copy ctx #f #f #f free-mem #f #f (+ fs nslots) #f)))

;;
;; CTX INIT FN
(define (ctx-init-fn stack enclosing-ctx args free-vars global-opt? late-fbinds eploc bound-id)

  ;;
  ;; FREE REGS
  (define (init-free-regs)
    (let* ((all (ctx-init-free-regs))
           (used-args
             (if (<= (length args) (length args-regs))
                 (list-head args-regs (length args))
                 args-regs))
           (used
             (if global-opt?
                 used-args
                 (cons '(r . 2) used-args))))
      (set-sub all used '())))

  ;;
  ;; ENV
  (define (init-env)
    (append (init-env-free)
            (init-env-local)))

  (define (init-env-* ids slot nvar fn-make)
    (if (null? ids)
        '()
        (let* ((id (car ids))
               (identifier (fn-make id slot nvar)))
          (cons (cons id identifier)
                (init-env-* (cdr ids) (+ slot 1) (+ nvar 1) fn-make)))))

  (define (init-env-free)
    ;; Create environment entries for free variables
    (init-env-*
      free-vars
      2
      0
      (lambda (id slot nvar)
        (make-identifier
          'free
          '()
          '()
          (if (member id late-fbinds)
              ;; If id is a late-fbind, type it's a function
              (make-ctx-tclo)
              ;; Else, get type from enclosing ctx
              (let ((ident (ctx-ident enclosing-ctx id)))
                (ctx-identifier-type enclosing-ctx (cdr ident))))
          (cons 'f nvar)
          (eq? id bound-id)))))

  (define (init-env-local)
    (init-env-*
      args
      2
      0
      (lambda (id slot nvar)
        (make-identifier
          'local
          (list slot)
          '()
          #f
          #f
          #f))))

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
    (if global-opt?
        ;; If global optimized call, closure is still on vstack but loc is #f
        '((1 . #f) (0 m . 0))
        '((1 r . 2) (0 m . 0))))

  ;;
  ;; FS
  (define (init-fs nb-args)
    (if (<= nb-args (length args-regs))
        1
        (+ (- nb-args (length args-regs)) 1)))

  ;;
  (make-ctx
    (or stack
        (append (make-list (length args) (make-ctx-tunk)) (list (make-ctx-tclo) (make-ctx-tret))))
    (init-slot-loc)
    (init-free-regs)
    '()
    (init-env)
    (length args)
    (init-fs (length args))
    eploc))

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
    (if (member 'ret (lazy-code-flags succ))
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
  (define (get env)
    (if (null? env)
        #f
        (cond ;; Identifier represents current function
              ((and (eq? (caar env) id)
                    (identifier-thisid (cdar env)))
                 (ctx-eploc ctx))
              ;; Identifier does not represent current function, stop
              ((eq? (caar env) id)
                 #f)
              ;; Else continue
              (else
                 (get (cdr env))))))
  (get (ctx-env ctx)))

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
                        (list (stack-idx-to-slot ctx (cdr first)))
                        '()
                        #f
                        #f
                        #f))
                (gen-env env (cdr id-idx))))))

  (let* ((env
           (clean-env (ctx-env ctx)
                      (map (lambda (el) (stack-idx-to-slot ctx el))
                           (map cdr id-idx))))
         (env
           (gen-env env id-idx)))

    (ctx-copy ctx #f #f #f #f env)))

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
  (if (eq? (identifier-kind identifier) 'free)
      (identifier-stype identifier)
      (let* ((sslots (identifier-sslots identifier))
             (sidx (slot-to-stack-idx ctx (car sslots))))
        (list-ref (ctx-stack ctx) sidx))))


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
;; STACK PUSH
;; Add a type on top of stack but do not change other components of ctx
;; TODO: use ctx-push with reg set to #f (?)
(define (ctx-stack-push ctx type)
  (ctx-copy ctx (cons type (ctx-stack ctx))))
;;
;; STACK POP
(define (ctx-stack-pop ctx)
  (ctx-copy ctx (cdr (ctx-stack ctx))))
(define (ctx-stack-pop-n ctx n)
  (if (= n 0)
      ctx
      (ctx-stack-pop-n
        (ctx-stack-pop ctx)
        (- n 1))))
;;
;; STACK MOVE
(define (ctx-stack-move ctx idx-from idx-to)
  (ctx-copy
    ctx
    (let ((old (ctx-stack ctx)))
      (append (list-head old idx-to) (cons (list-ref old idx-from) (list-tail old (+ idx-to 1)))))))

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
              (if (and (= (length (identifier-sslots (cdr ident))) 1)
                       (not (eq? (identifier-kind (cdr ident)) 'free)))
                  ;; It is the only position of the identifier, then remove identifier from env
                  (env-remove-slot (cdr env) slot)
                  ;; Else, just remove this slot
                  (cons (cons (car ident)
                              (identifier-copy
                                (cdr ident)
                                #f
                                (set-sub (identifier-sslots (cdr ident)) (list slot) '())))
                        (env-remove-slot (cdr env) slot)))
              (cons ident (env-remove-slot (cdr env) slot))))))

  ;;
  (let* ((slot (- (length (ctx-stack ctx)) 1))
         (r (assoc-remove slot (ctx-slot-loc ctx)))
         (loc (cdar r))
         (slot-loc (cdr r)))

    (ctx-copy
      ctx
      (cdr (ctx-stack ctx))                        ;; stack: remove top
      (cdr (assoc-remove slot (ctx-slot-loc ctx))) ;; slot-loc: remove popped slot
      (if (and (not (loc-used? loc slot-loc))      ;; free-regs: add popped loc if it's an unused reg
               (ctx-loc-is-register? loc))
          (cons loc (ctx-free-regs ctx))
          #f)
      (if (and (not (loc-used? loc slot-loc))      ;; free-mems: add popped loc if it's an unused mem
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
(define (ctx-set-type ctx data type)

  (let ((ident (or (and (pair? data) (symbol? (car data)) (identifier? (cdr data)) data)
                   (ctx-ident-at ctx data))))
    (if ident
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

(define (ctx-get-call-args-moves ctx nb-args cloloc)

  (define clomove (and cloloc (cons cloloc '(r . 2))))

  (define (get-req-moves curr-idx rem-regs moves pushed)
    (if (< curr-idx 0)
        (cons (reverse pushed) moves)
        (let ((loc (ctx-get-loc ctx curr-idx)))
          (if (null? rem-regs)
              (get-req-moves (- curr-idx 1) '() moves (cons loc pushed))
              (get-req-moves
                (- curr-idx 1)
                (cdr rem-regs)
                (cons (cons loc (car rem-regs))
                      moves)
                pushed)))))

  (let ((pushed/moves (get-req-moves (- nb-args 1) args-regs '() '())))

    (cons (car pushed/moves)
          (if clomove
              (steps (append (cdr pushed/moves) (list clomove)))
              (steps (cdr pushed/moves))))))


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
              (ctx-copy ctx #f #f #f (cons mloc (ctx-free-mems ctx)) #f #f (+ (ctx-fs ctx) 1))))))


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
