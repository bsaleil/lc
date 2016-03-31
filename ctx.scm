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
(define CTX_UNK #f)
(define CTX_CLO #f)
(define CTX_RETAD #f)
(define regalloc-regs #f)

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
)

;(define (make-regalloc-ctx slot-loc free-regs fs)
;  (make-ctx #f slot-loc free-regs #f #f fs))

;; TODO USE IT ! remove all make-ctx which are only copies and use ctx-copy
(define (ctx-copy ctx #!optional stack slot-loc free-regs free-mems env nb-args fs)
  (make-ctx
    (or stack     (ctx-stack ctx))
    (or slot-loc  (ctx-slot-loc ctx))
    (or free-regs (ctx-free-regs ctx))
    (or free-mems  (ctx-free-mems ctx))
    (or env       (ctx-env ctx))
    (or nb-args   (ctx-nb-args ctx))
    (or fs        (ctx-fs ctx))))


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
            0))


;;; TODO public api
;; L'api ne doit fonctionner qu'avec un index de pile. Les slots sont propre à l'implantation.
;;

;; CTX IDENTIFIER LOC
;; Return best loc for identifier. (Register if available, memory otherwise)
(define (ctx-identifier-loc ctx identifier)

  (define (get-best-loc slot-loc sslots mloc)
    (if (null? slot-loc)
        (and (assert mloc "Internal error") mloc)
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
          (assert (eq? (identifier-kind identifier) 'free) "Internal error")
          (identifier-cloc identifier))
        (get-best-loc (ctx-slot-loc ctx) sslots #f))))

;;
(define (ctx-fs-inc ctx)
  (ctx-copy ctx #f #f #f #f #f #f (+ (ctx-fs ctx) 1)))

(define (ctx-fs-update ctx fs)
  (ctx-copy ctx #f #f #f #f #f #f fs))

;;
;; IS MUTABLE ?
;; is this stack-idx mutable?
(define (ctx-is-mutable? ctx stack-idx)
  (let ((ident (ctx-ident-at ctx stack-idx)))
    (and
      ident
      (member 'mutable (identifier-flags (cdr ident))))))

;;
;; CTX INIT FN
(define (ctx-init-fn call-ctx enclosing-ctx args free-vars mutable-vars)

  ;;
  ;; FREE REGS
  (define (init-free-regs)
    (if (<= (length args) (length args-regs))
        (set-sub (ctx-init-free-regs)
                 (list-head args-regs (length args))
                 '())
        (set-sub (ctx-init-free-regs)
                 args-regs
                 '())))

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
    (init-env-*
      free-vars
      2
      0
      (lambda (id slot nvar)
        (make-identifier
          'free
          '()
          (let ((ident (ctx-ident enclosing-ctx id)))
            (identifier-flags (cdr ident)))
          CTX_UNK
          (cons 'f nvar)))))

  (define (init-env-local)
    (init-env-*
      args
      2
      0
      (lambda (id slot nvar)
        (make-identifier
          'local
          (list slot)
          (if (member id mutable-vars) '(mutable) '())
          #f
          #f))))

  ;;
  ;; SLOT-LOC
  (define (init-slot-loc)
    (append
      (reverse (init-slot-loc-local 2 args-regs 2 0)) ;; Reverse for best display for debug purposes
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
    '((1 m . 1) (0 m . 0)))

  ;;
  ;; FS
  (define (init-fs nb-args)
    (if (<= nb-args (length args-regs))
        2
        (+ (- nb-args (length args-regs)) 2)))

  ;;
  (make-ctx
    (if call-ctx
                (ctx-stack call-ctx)
                (append (make-list (length args) CTX_UNK) (list CTX_CLO CTX_RETAD)))
    (init-slot-loc)
    (init-free-regs)
    '()
    (init-env)
    (length args)
    (init-fs (length args))))

;;
;; GET FREE REG
(define (ctx-get-free-reg ctx)

  (let ((free-regs (ctx-free-regs ctx)))
    (if (null? free-regs)
        (let* ((moves/mloc/ctx (ctx-get-free-mem ctx))
               (moves (car moves/mloc/ctx))
               (mloc  (cadr moves/mloc/ctx))
               (ctx   (caddr moves/mloc/ctx))
               (spill-reg (cons 'r 0)) ;; TODO: better strat
               (reg-slots (ctx-get-slots ctx spill-reg)))
          ;; 1: changer tous les slots pour r -> m
          (let ((ctx (ctx-set-loc-n ctx reg-slots mloc))
                (moves (append moves
                               (list (cons spill-reg mloc)))))
            (list moves spill-reg ctx)))
        (list '()
              (car free-regs)
              (ctx-copy ctx #f #f (cdr free-regs))))))

;;
;; BIND LOCALS
(define (ctx-bind-locals ctx id-idx mvars #!optional letrec-bind?)

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
                        (cond ((and letrec-bind?
                                    (not (member (car first) mvars)))
                                 '(mutable letrec-nm))
                              ((member (car first) mvars)
                                 '(mutable))
                              (else
                                 '()))
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
        (begin (assert (null? ids) "Internal error")
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
      (if (ctx-loc-is-memory? loc)
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

  ;; Is loc 'loc' used in slot-loc set ?
  (define (loc-used? slot-loc loc)
    (if (null? slot-loc)
        #f
        (let ((sl (car slot-loc)))
          (or (eq? (cdr sl) loc)
              (loc-used? (cdr slot-loc) loc)))))

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
      (if (and (not (loc-used? slot-loc loc))      ;; free-regs: add popped loc if it's an unused reg
               (ctx-loc-is-register? loc))
          (cons loc (ctx-free-regs ctx))
          #f)
      (if (and (not (loc-used? slot-loc loc))      ;; free-mems: add popped loc if it's an unused mem
               (ctx-loc-is-memory? loc))
          (cons loc (ctx-free-mems ctx))
          #f)
      (env-remove-slot (ctx-env ctx) slot))))      ;; env: remove popped slot from env

;;
;; GET LOC
(define (ctx-get-loc ctx stack-idx)
  (let* ((slot (stack-idx-to-slot ctx stack-idx))
         (r (assoc slot (ctx-slot-loc ctx))))
    (assert r "Internal error")
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
(define args-regs '((r . 0) (r . 1) (r . 2) (r . 4) (r . 6))) ;; TODO
(define (ctx-get-call-args-moves ctx nb-args)

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
          (steps (cdr pushed/moves)))))

  ; (define slot-loc (ctx-slot-loc ctx))
  ; (define len-stack (length (ctx-stack ctx)))
  ; (define all-reg
  ;   (build-list (length regalloc-regs) (lambda (i) (cons 'r i))))
   ;
  ; ;; Check if value in location 'loc' is used for
  ; ;; a next argument (if so, we can't overwrite it)
  ; (define (is-used-after? loc curr-slot lim-slot)
  ;   (foldr (lambda (el r)
  ;            (or (and (equal? (cdr el) loc)
  ;                     (> (car el) curr-slot)
  ;                     (<= (car el) lim-slot))
  ;                r))
  ;          #f
  ;          slot-loc))
   ;
  ; ;; Return the ordered locations
  ; (define (pushed-locs)
  ;   (define (pushed-locs-h lidx)
  ;     (if (< lidx 0)
  ;         '()
  ;         (let ((r (pushed-locs-h (- lidx 1))))
  ;           (cons
  ;             (ctx-get-loc ctx lidx)
  ;             r))))
  ;   (pushed-locs-h (- nb-args (length args-regs) 1)))
   ;
  ; ;; Return the registers unused for the function call
  ; ;; A register is unused if it is free or associated to
  ; ;; a value which is not an argument
  ; (define (get-unused-regs)
  ;   (let* ((slot-start (- len-stack nb-args))
  ;          (slot-end   (+ slot-start (length args-regs) -1)))
  ;     (foldr (lambda (el r)
  ;              (if (or (not (car el))
  ;                      (and (>= (car el) slot-start)
  ;                           (<= (car el) slot-end)))
  ;                  r
  ;                  (if (ctx-loc-is-register? (cdr el))
  ;                      (cons (cdr el) r)
  ;                      r)))
  ;            '()
  ;            slot-loc)))
   ;
  ; ;; Compute and return all moves needed to put the arguments in the
  ; ;; calling convention registers
  ; ;; Return a pair with:
  ; ;; moves in car
  ; ;; moves needed to retrieve saved values in unused registers
  ; (define (move-regs unregs)
  ;   (define (rec-loop rem lidx args-regs unregs)
  ;     (if (= rem 0)
  ;         (cons '() '())
  ;         (let* ((src (ctx-get-loc ctx lidx))
  ;                (dst (car args-regs)))
  ;           (cond ((eq? src dst)
  ;                    (let* ((moves/save (rec-loop (- rem 1) (- lidx 1) (cdr args-regs) unregs))
  ;                           (moves (car moves/save))
  ;                           (save  (cdr moves/save)))
  ;                      moves/save))
  ;                 ((is-used-after? dst (stack-idx-to-slot ctx lidx) (stack-idx-to-slot ctx (- lidx rem -1)))
  ;                    (let* ((moves/save (rec-loop (- rem 1) (- lidx 1) (cdr args-regs) (cdr unregs)))
  ;                           (moves (car moves/save))
  ;                           (save  (cdr moves/save)))
  ;                      (cons
  ;                        (cons (cons (car unregs) src) moves)
  ;                        (cons (cons dst (car unregs)) save))))
  ;                 (else
  ;                      (let* ((moves/save (rec-loop (- rem 1) (- lidx 1) (cdr args-regs) unregs))
  ;                             (moves (car moves/save))
  ;                             (save  (cdr moves/save)))
  ;                        (cons
  ;                          (cons (cons dst src) moves)
  ;                          save)))))))
   ;
  ;   (let ((moves/save
  ;           (rec-loop
  ;             (min nb-args (length args-regs))
  ;             (- nb-args 1)
  ;             args-regs
  ;             unregs)))
  ;     (append (car moves/save) (cdr moves/save))))

 ;;;; GO
 ;(let* (;; 1 - Get unused registers
 ;       (unregs
 ;         (set-sub (append (ctx-free-regs ctx)
 ;                          (get-unused-regs))
 ;                  args-regs
 ;                  '()))
 ;       (rrr (begin (print "UNUSED ARE = ") (pp unregs)))
 ;       ;; 2 - Get needed moves
 ;       (moves  (move-regs unregs))
 ;       ;; 3 - Get locs to push
 ;       (locs   (pushed-locs)))
 ;
 ;(cons locs moves)))


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
    (let* ((r (assoc old-loc slot-loc))
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

;;; Clear versioning information
;(define (ctx-clear ctx)
;
;  (define (env-clear env)
;    (if (null? env)
;        '()
;        (let ((ident (car env)))
;          (cons (cons (car ident)
;                      (make-identifier
;                        (identifier-kind (cdr ident))
;                        (if (eq? (identifier-kind (cdr ident)) 'free)
;                            '()
;                            (list-tail
;                              (identifier-sslots (cdr ident))
;                              (- (length (identifier-sslots (cdr ident))) 1)))
;                        (identifier-flags (cdr ident))
;                        (if (identifier-stype (cdr ident))
;                            CTX_UNK
;                            #f)
;                        (identifier-cloc (cdr ident))))
;                (env-clear (cdr env))))))
;
;  (let ((stack (make-list (length (ctx-stack ctx)) CTX_UNK))
;        (env (env-clear (ctx-env ctx))))
;    (ctx-copy ctx stack #f #f env)))
;
;;; Convert stack list index to slot
;(define (ctx-lidx-to-slot ctx lidx)
;  (- (length (ctx-stack ctx)) lidx 1))
;
;;; Convert stack slot to list index
;(define (ctx-slot-to-lidx ctx slot)
;  (- (length (ctx-stack ctx)) slot 1))
;
;;; Return type associated to identifier
;(define (ctx-identifier-type ctx identifier)
;  (if (eq? 'free (identifier-kind identifier))
;      (identifier-stype identifier)
;      (let ((slot (car (identifier-sslots identifier))))
;        (list-ref (ctx-stack ctx) (ctx-slot-to-lidx ctx slot)))))
;
;;; Return location associated to stack slot
;(define (ctx-get-loc ctx slot)
;  (let ((r (assoc slot (ctx-slot-loc ctx))))
;    (if r
;        (cdr r)
;        (error "NYI error"))))
;
;;; Return location associated to identifier
;(define (ctx-identifier-loc ctx identifier #!optional (orig-loc? #f))
;
;  ;; Return 'best' location
;  ;; Return register location if identifier is available in a register only or in register and memory
;  ;; Return memory location instead
;  (define (get-loc slots ret)
;    (if (null? slots)
;        ret
;        (let* ((r (assoc (car slots) (ctx-slot-loc ctx)))
;               (loc (cdr r)))
;          (if (ctx-loc-is-register? loc)
;              loc
;              (get-loc (cdr slots) loc)))))
;
;  (if (eq? (identifier-kind identifier) 'free)
;      (let ((slots (identifier-sslots identifier)))
;        (if (or orig-loc? (null? slots))
;            (identifier-cloc identifier)
;            (get-loc slots #f)))
;      (let ((slots (identifier-sslots identifier)))
;        (if orig-loc?
;            (ctx-get-loc ctx (list-ref slots (- (length slots) 1)))
;            (get-loc slots #f)))))
;
;;; loc is a register if loc is of the form (rx . value)
;;; loc is a free variable location if loc is of the form (fx . value)
;;; loc is a memory location if loc if of the form (integer . value)
;;; Return #t if location is a register, else return #f
;(define (ctx-loc-is-register? loc)
;  (and (symbol? loc)
;       (char=? (string-ref (symbol->string loc) 0) #\r)))
;
;;; Return #t if location is a memory location, else return #f
;(define (ctx-loc-is-memory? loc)
;  (integer? loc))
;
;;; Return #t if location is a free variable location, else return #f
;(define (ctx-loc-is-floc? loc)
;  (and (symbol? loc)
;       (char=? (string-ref (symbol->string loc) 0) #\f)))
;
;;; Is loc the original location of identifier ?
;;; (floc for free identifier, or first mloc for local identifier)
;(define (ctx-loc-is-orig-loc ctx identifier loc)
;  (if (eq? (identifier-kind identifier) 'free)
;      (eq? loc (identifier-cloc identifier))
;      (let ((orig-slot (list-ref (identifier-sslots identifier)
;                                 (- (length (identifier-sslots identifier)) 1))))
;        (eq? loc (ctx-get-loc ctx orig-slot)))))
;
;;; Get free variable position in closure from free location
;(define (ctx-floc-to-fpos floc)
;  (- (string->number
;       (list->string
;         (cdr (string->list
;                (symbol->string floc)))))
;     1))
;
;;; Return ident pair (id . identifier) which is on slot 'slot'
;(define (ctx-ident-at ctx slot)
;  (define (env-ident-at env)
;    (if (null? env)
;        #f
;        (let ((identifier (cdar env)))
;          (if (member slot (identifier-sslots identifier))
;              (car env)
;              (env-ident-at (cdr env))))))
;  (env-ident-at (ctx-env ctx)))

;;-----------------------------------------------------------------------------
;; Register allocation

;;; Return moves needed to merge two register allocation context
;;; ex. ((r0 . r5) (r5 . r3))
;(define (reg-alloc-merge slot-loc-src slot-loc-dst)
;
;  ;; Get needed moves without considering values overwriting
;  (define (get-required-moves slot-loc-src slot-loc-dst)
;    (cond ((null? slot-loc-src)
;             '())
;          ((not (caar slot-loc-src))
;             (get-required-moves
;               (cdr slot-loc-src)
;               slot-loc-dst))
;          (else
;            (let* ((slot    (caar slot-loc-src))
;                   (loc-src (cdar slot-loc-src))
;                   (loc-dst
;                     (let ((r (assoc slot slot-loc-dst)))
;                       (if r
;                           (cdr r)
;                           (error "Internal error regalloc merge")))))
;              (if (equal? loc-src loc-dst)
;                  ;; For this slot, loc are the same in src and dst ctx, nothing to do.
;                  (get-required-moves (cdr slot-loc-src) slot-loc-dst)
;                  ;; For this slot, loc are different in src and dst ctx,
;                  ;; add a move src -> dst to the list of moves
;                  (cons (cons loc-src loc-dst)
;                        (get-required-moves (cdr slot-loc-src) slot-loc-dst)))))))
;
;  ;; Compute real moves (considering values overwriting) from required moves
;  ;; Compute a step with 'step' function until required moves set is empty
;  (let loop ((real-moves '())
;             (req-moves
;               (get-required-moves
;                 slot-loc-src
;                 slot-loc-dst)))
;    (if (null? req-moves)
;        real-moves
;        (let ((r (step '() req-moves '())))
;          (loop (append (car r) real-moves) (cdr r))))))
;

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
;; TODO
(define (step visited req-moves pending-moves #!optional src-sym)

  ;; A loc is available if it is not a source of a move in required moves
  ;; We can then directly overwrite its content
  (define (loc-available loc)
    (not (assoc loc req-moves)))

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
                             (list (cons 'rtmp dst)))))
               (cons real-moves
                     (set-sub req-moves (list move) '()))))
          ;; Case 1: Destination is free, validate pending moves
          ((loc-available dst)
             (let ((real-moves (cons move
                                     pending-moves)))
               (cons real-moves
                     (set-sub req-moves (list move) '()))))
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

;;;-----------------------------------------------------------------------------
;;; Call register save
;
;;; TODO move
;;; Return a memory loc in which identifier is, or #f id identifier is not in memory
;(define (ctx-identifier-mloc ctx identifier)
;
;  (define (slots-mloc slots)
;    (if (null? slots)
;        #f
;       (let ((r (assoc (car slots) (ctx-slot-loc ctx))))
;         (if (ctx-loc-is-memory? (cdr r))
;             (cdr r)
;             (slots-mloc (cdr slots))))))
;
;  (let* ((slots (identifier-sslots identifier)))
;    (slots-mloc slots)))
;
;;; Return an avaiable memory slot, or allocate a new one
;;; Return loc and updated ctx
;(define (ctx-get-free-mem ctx)
;  (define (get-existing-free)
;    (foldr (lambda (el r)
;             (if (not (car el))
;                 (begin (assert (ctx-loc-is-memory? (cdr el)) "Internal error")
;                        (cdr el))
;                 r))
;           #f
;           (ctx-slot-loc ctx)))
;
;  (define (alloc-slot)
;    (let ((mloc (ctx-fs ctx)))
;      (cons mloc
;            (ctx-copy
;              ctx
;              #f
;              (cons (cons #f mloc) (ctx-slot-loc ctx))
;              #f
;              #f
;              #f
;              (+ (ctx-fs ctx) 1)))))
;
;  (let ((mloc (get-existing-free)))
;    (if mloc
;        (cons mloc ctx)
;        (alloc-slot))))
;
;;;
;(define (ctx-change-loc ctx slot loc)
;  (error "NYI"))
;
;  ;(define (loc-used? slot-loc loc)
;  ;  (if (null? slot-loc)
;  ;      #f
;  ;      (let ((sl (car slot-loc)))
;  ;        (or (eq? (cdr sl) loc)
;  ;            (loc-used? (cdr slot-loc) loc)))))
;  ;
;  ;(let* ((r (assoc-remove slot (ctx-slot-loc ctx)))
;  ;       (sl (car r))
;  ;       (slot-loc-rm (cdr r))
;  ;       (slot-loc
;  ;         (if (ctx-loc-is-register? (cdr sl))
;  ;             (cons (cons slot loc)
;  ;                   slot-loc-rm)
;  ;             (append (list (cons slot loc)
;  ;                           (cons #f   (cdr sl)))
;  ;                     slot-loc-rm)))
;  ;       (free-regs
;  ;         (if (and (ctx-loc-is-register? (cdr sl))
;  ;                  (not (loc-used? slot-loc (cdr sl))))
;  ;             (cons (cdr sl) (ctx-free-regs ctx))
;  ;             (ctx-free-regs ctx)))
;  ;       (free-regs
;  ;         (if (ctx-loc-is-register? loc)
;  ;             (set-sub free-regs (list loc) '())
;  ;             free-regs)))
;  ;
;  ;  (ctx-copy ctx  #f slot-loc free-regs)))
;
;;; END TODO MOVE
;
;;; Return moves/ctx
;(define (ctx-save-slots ctx slots)
;  (define (save-h ctx moves slots)
;    (if (null? slots)
;        (list moves ctx)
;        (let ((nmoves/ctx (ctx-save-slot ctx (car slots))))
;          (save-h
;            (cadr nmoves/ctx)
;            (append (car nmoves/ctx) moves) (cdr slots)))))
;  (save-h ctx '() slots))
;
;;; Return moves/ctx
;;; moves: moves used to save this slot
;;; ctx: updated ctx
;(define (ctx-save-slot ctx slot)
;  (let ((sl (assoc slot (ctx-slot-loc ctx))))
;    (if (ctx-loc-is-memory? (cdr sl))
;        ;; This slot is associated to a memory location, nothing to do
;        (cons '() ctx)
;        ;; This slot is associated to a register, we need to save it
;        (let* ((ident (ctx-ident-at ctx slot))
;               (mloc (and ident (ctx-identifier-mloc ctx (cdr ident)))))
;          (if (and mloc)
;              ;; An id is in this register and this id already has a memory location
;              ;; No moves, and we only change loc associated to slot
;              (list '()
;                    (ctx-change-loc ctx (car sl) mloc))
;              ;; We need a new memory location
;              (let* ((r (ctx-get-free-mem ctx))
;                     (mloc (car r))
;                     (ctx (cdr r)))
;                (pp "FREE MEM")
;                (pp ctx)
;                (pp (car sl))
;                (pp mloc)
;                (list (list (cons (cdr sl) mloc))
;                      (ctx-change-loc ctx (car sl) mloc))))))))


;;-----------------------------------------------------------------------------
;; Ctx

;; Identifier object
(define-type identifier
  kind   ;; symbol 'free or 'local
  sslots ;; list of stack slots where the variable is
  flags  ;; list of variable
  stype  ;; ctx type (copied to virtual stack)
  cloc   ;; closure slot if free variable
)

(define (identifier-mutable? identifier)
  (member 'mutable (identifier-flags identifier)))

;; TODO USE IT ! remove all make-ctx which are only copies and use ctx-copy
(define (identifier-copy identifier #!optional kind sslots flags stype cloc)
  (make-identifier
    (or kind   (identifier-kind identifier))
    (or sslots (identifier-sslots identifier))
    (or flags  (identifier-flags identifier))
    (or stype  (identifier-stype identifier))
    (or cloc   (identifier-cloc identifier))))
