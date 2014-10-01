;;-----------------------------------------------------------------------------

;; AST FUNCTIONS

;;-----------------------------------------------------------------------------

;; Global ids
;; Contains a list of global ids with id,position,type-information
;; ex. '((foo 1 number) (bar 2 bool) (fun 3 closure))
(define globals '())

;;-----------------------------------------------------------------------------

;; Gen lazy code from ast
(define (gen-ast ast succ)
  (cond ;; Literal
        ((or (number? ast) (boolean? ast) (null? ast)) (mlc-literal ast succ))
        ;; Symbol
        ((symbol? ast) (mlc-symbol ast succ))
        ;; Pair
        ((pair? ast)
         (let ((op (car ast)))
           (cond ;; Special with call
                 ((member op '($$putchar)) (mlc-special-c ast succ))
                 ;; Special without call
                 ((member op '($cons $car $cdr)) (mlc-special-nc ast succ))
                 ;; Quote
                 ((eq? 'quote (car ast)) (mlc-quote (cadr ast) succ))
                 ;; Set!
                 ((eq? 'set! (car ast)) (mlc-set! ast succ))
                 ;; Lambda
                 ((eq? op 'lambda) (mlc-lambda ast succ))
                 ;; Operator num
                 ((member op '($+ $- $* $quotient $modulo $< $> $=)) (mlc-op-num ast succ op))
                 ;; Operator gen
                 ((member op '($eq?)) (mlc-op-gen ast succ op))
                 ;; Tests
                 ((member op '($number? $procedure? $pair?)) (mlc-test ast succ))
                 ;; If
                 ((eq? op 'if) (mlc-if ast succ))
                 ;; Define
                 ((eq? op 'define) (mlc-define ast succ))
                 ;; Call expr
                 (else (mlc-call ast succ)))))
        ;; *unknown*
        (else
         (error "unknown ast" ast))))

;;
;; Make lazy code from LITERAL
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
                                       ((null? ast)    CTX_NULL)))))))

;;
;; Make lazy code from QUOTE
;;
(define (mlc-quote ast succ)
  (cond ((pair? ast)
         (let* ((lazy-pair (mlc-pair succ))
                (lazy-cdr  (mlc-quote (cdr ast) lazy-pair)))
           (mlc-quote (car ast) lazy-cdr)))
        ((symbol? ast) (begin (pp ast) (error "NYI quoted symbol")))
        (else (gen-ast ast succ))))

;;
;; Make lazy code from SET!
;;
(define (mlc-set! ast succ)
  (let* ((variable (cadr ast))
        (lazy-set (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((glookup-res (assoc variable globals)))
                        (if glookup-res
                            (begin (x86-pop cgc (x86-rax))
                                   (x86-mov cgc (x86-mem (* 8 (cdr glookup-res)) (x86-r10)) (x86-rax))
                                   (jump-to-version cgc succ (ctx-pop ctx)))
                            (begin (x86-pop cgc (x86-rax))
                                   (x86-push cgc (x86-imm-int ENCODING_VOID))
                                   (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID)))))))))
                            ;(error "NYI set!")))))))
    (gen-ast (caddr ast) lazy-set)))

;;
;; Make lazy code from SYMBOL
;;
(define (mlc-symbol ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      ;; Lookup in local env
      (let* ((res (assoc ast (ctx-env ctx)))
             (ctx-type (if res
                          (if (eq? (identifier-type (cdr res)) 'free)
                             ;; Free var
                             (gen-get-freevar  cgc ctx res 'stack)
                             ;; Local var
                             (gen-get-localvar cgc ctx res 'stack))
                          (let ((res (assoc ast globals)))
                             (if res
                                ;; Global var
                                (gen-get-globalvar cgc ctx res 'stack)
                                ;; Unknown
                                (error "Can't find variable: " ast))))))
           (jump-to-version cgc succ (ctx-push ctx ctx-type))))))

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)
  (let* ((identifier (cadr ast))
         (lazy-bind (make-lazy-code (lambda (cgc ctx)
                                     (x86-pop cgc (x86-rax))
                                     (let* ((res (assoc identifier globals)) ;; Lookup in globals
                                           (pos (cadr res))                  ;; Get global pos
                                           (ctx-type (car (ctx-stack ctx)))) ;; Get type from top of stack
                                       
                                       ;; Set global type in globals
                                       (set-cdr! (cdr res) ctx-type)
                                                                              
                                       (x86-mov cgc (x86-mem (* 8 pos) (x86-r10)) (x86-rax)))

                                     (x86-push cgc (x86-imm-int ENCODING_VOID))

                                     (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID)))))
         (lazy-val (gen-ast (caddr ast) lazy-bind)))

    (make-lazy-code (lambda (cgc ctx)
                      (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
                      (x86-mov cgc (x86-mem (* 8 (length globals)) (x86-r10)) (x86-rax))
                      (set! globals (cons (cons identifier (cons (length globals) CTX_VOID)) globals))
                      (jump-to-version cgc lazy-val ctx)
                      ))))

;;
;; Make lazy code from SPECIAL FORM (called specials)
;;
(define (mlc-special-c ast succ)
  (let* ((name (car ast))
         (label (cond ((eq? name '$$putchar) label-$$putchar)
                      (else "NYI special")))
         (lazy-special (make-lazy-code
                         (lambda (cgc ctx)
                           (x86-call cgc label)
                           (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID))))))
    (if (> (length (cdr ast)) 0)
        (gen-ast-l (cdr ast) lazy-special)
        lazy-special)))

;;
;; Make lazy code from SPECIAL FORM (inlined specials)
;;
(define (mlc-special-nc ast succ)
  (let* ((special (car ast))
         (lazy-special
           (cond ;; CONS
                 ((eq? special '$cons) (mlc-pair succ))
                 ;; CAR & CDR
                 ((member special '($car $cdr))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((offset
                              (if (eq? special '$car)
                                  (- 8 TAG_MEMOBJ)
                                  (- 16 TAG_MEMOBJ))))
                        (x86-pop cgc (x86-rax))
                        (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
                        (x86-push cgc (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_UNK))))))
                 ;; OTHERS
                 (else (error "NYI")))))

    (cond ;; $CONS
          ((eq? special '$cons)
           (let ((lazy-right (gen-ast (caddr ast) lazy-special)))
             (gen-ast (cadr ast) lazy-right)))
          ;; $CAR & $CDR
          ((member special '($car $cdr))
           (let* ((lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_PAIR_EXPECTED))))
                  (lazy-main (make-lazy-code
                               (lambda (cgc ctx)
                                 (let ((type (car (ctx-stack ctx))))
                                   (cond ((eq? type CTX_PAI) (jump-to-version cgc lazy-special ctx))
                                         ((eq? type CTX_UNK) (jump-to-version cgc
                                                                              (gen-dyn-type-test CTX_PAI
                                                                                                 0
                                                                                                 (ctx-push (ctx-pop ctx) CTX_PAI)
                                                                                                 lazy-special
                                                                                                 ctx
                                                                                                 lazy-fail)
                                                                              ctx))
                                         (else (gen-error cgc ERR_PAIR_EXPECTED))))))))
             (gen-ast (cadr ast) lazy-main))))))

;; TODO
;; TODO
(define (gen-mutated cgc ctx mutated)
  '())

;;
;; Make lazy code from LAMBDA
;;
(define (mlc-lambda ast succ)
  (let* (;; Lambda parameters
         (params (cadr ast))
         ;; Lambda free vars
         (fvars #f)
         ;; TODO
         (mutated #f)
         ;; Lazy lambda return
         (lazy-ret (make-lazy-code
                     (lambda (cgc ctx)
                       ;; Here the stack is :
                       ;;         RSP
                       ;;     | ret-val |  ctx  | ret-addr | closure | arg n | ... | arg 1 |
                       ;; Pop return value
                       (x86-pop cgc (x86-rax))
                       ;; Mov return value at bottom of the frame
                       (x86-mov cgc (x86-mem (* 8 (+ 2 (length params))) (x86-rsp)) (x86-rax))
                       ;; Mov ret-addr in rax
                       (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))
                       ;; RSP now point to return value
                       (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (+ 2 (length params)))))
                       ;; Jump to continuation
                       (x86-jmp cgc (x86-rax)))))
         ;; Lazy lambda body
         (lazy-body (gen-ast (caddr ast) lazy-ret))
         ;; Lazy mutated : Move mutated vars in memory
         (lazy-mutated (make-lazy-code
                           (lambda (cgc ctx)
                              ;;(gen-mutated cgc ctx mutated)
                              (jump-to-version cgc lazy-body ctx)))))

    ;; Lazy closure generation
    (make-lazy-code
      (lambda (cgc ctx)
        (let* (;; Lambda stub
               (stub-labels (add-fn-callback cgc
                                             0
                                             (lambda (sp ctx ret-addr selector closure)
                                               ;; Extends env with params and free vars
                                               (let* ((env (append (build-env params 0) (build-fenv fvars 0)))
                                                      (ctx (make-ctx (ctx-stack ctx) env)))
                                                 (gen-version-fn closure lazy-mutated ctx)))))
               (stub-addr (vector-ref (list-ref stub-labels 0) 1)))

          ;; 0 - COMPUTE FREE VARS
          (set! fvars (free-vars (caddr ast) params ctx))

          ;; 0b - variables mutées TODO
          (set! mutated (mutated-vars (caddr ast) params ctx))

          ;; 1 - WRITE OBJECT HEADER
          (let ((header-word (+ (arithmetic-shift (+ 2 (length fvars)) 8) (arithmetic-shift STAG_PROCEDURE 3) 6))) ;; 2 + nbFreeVars | STAG_PROCEDURE | 110 => Length | Procedure | Permanent
            (x86-mov cgc (x86-rax) (x86-imm-int header-word))
            (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax)))

          ;; 2 - WRITE CC TABLE LOCATION
          (x86-mov cgc (x86-rax) (x86-imm-int (+ 16 (* 8 (length fvars))))) ;; 16 = 8(header) + 8(location)
          (x86-add cgc (x86-rax) alloc-ptr)
          (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))

          ;; 3 - WRITE FREE VARS
          (gen-free-vars cgc fvars ctx 16) ;; 16 = 8(header) + 8(location)

          ;; 4 - WRITE CC TABLE
          (gen-cc-table cgc stub-addr (+ 16 (* 8 (length fvars))))

          ;; 5 - TAG AND PUSH CLOSURE
          (x86-mov cgc (x86-rax) alloc-ptr)
          (x86-add cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
          (x86-push cgc (x86-rax))

          ;; 6 - UPDATE ALLOC PTR
          (x86-add cgc alloc-ptr (x86-imm-int (* 8 (+ 2 (length fvars) global-cc-table-maxsize))))

          ;; Jump to next
          (jump-to-version cgc
                           succ
                           (ctx-push ctx
                                     CTX_CLO)))))))

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

                                (if dev-log
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

                                                (if dev-log (println ">>> swapping-branches"))

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

;;
;; Make lazy code from CALL EXPRESSION
;;
(define (mlc-call ast succ)
  (let* (;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_PRO_EXPECTED))))
         ;; Lazy call
         (lazy-call (make-lazy-code (lambda (cgc ctx)
                                      (let* (;; Flag in stub : is the continuation already generated ?
                                             (gen-flag #f)
                                             ;; Label for return address loading
                                             (load-ret-label (asm-make-label cgc (new-sym 'load-ret-addr)))
                                             ;; Continuation stub
                                             (stub-labels (add-callback cgc
                                                                        0
                                                                        (lambda (ret-addr selector)
                                                                          ;; Remove lambda and args from ctx, and add retval (unknown)
                                                                          (let ((ctx-continuation (ctx-push (ctx-pop (ctx-pop-nb ctx (length args))) CTX_UNK)))
                                                                            (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag = continuation addr
                                                                                (set! gen-flag (gen-version-continuation load-ret-label
                                                                                                                         succ
                                                                                                                         ctx-continuation)))
                                                                            gen-flag)))))

                                        ;; Return address (continuation label)
                                        (x86-label cgc load-ret-label)
                                        (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1)))
                                        (x86-push cgc (x86-rax))

                                        ;; Call ctx in rdx
                                        (let* ((call-stack    (cons CTX_CTXID (cons CTX_RETAD (list-head (ctx-stack ctx) (+ 1 (length args))))))
                                               (call-ctx      (make-ctx call-stack '()))
                                               (ctx-id        (length ctx_ids))
                                               (cct-offset    (* 8 (get-closure-index call-ctx))))

                                          (set! ctx_ids (cons (cons ctx-id call-ctx) ctx_ids))
                                          (x86-mov cgc (x86-rax) (x86-imm-int ctx-id))
                                          (x86-push cgc (x86-rax))

                                        ;; 1 - Get cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem 16 (x86-rsp)))               ;; get closure
                                        (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))) ;; get cc-table

                                        ;; 2 - Get entry point in cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax)))

                                        ;; 3 - Jump to entry point
                                        (x86-jmp cgc (x86-rax)))))))
         ;; Lazy main
         (lazy-main (make-lazy-code (lambda (cgc ctx)
                                      (let ((op-type (car (ctx-stack ctx))))
                                        (cond ((eq? op-type CTX_CLO) (jump-to-version cgc lazy-call ctx))
                                              ((eq? op-type CTX_UNK) (jump-to-version cgc
                                                                                      (gen-dyn-type-test CTX_CLO
                                                                                                         0
                                                                                                         (ctx-push (ctx-pop ctx) CTX_CLO)
                                                                                                         lazy-call
                                                                                                         ctx
                                                                                                         lazy-fail)
                                                                                      ctx))
                                              (else (gen-error cgc ERR_PRO_EXPECTED)))))))
         ;; Lazy callee
         (lazy-operator (gen-ast (car ast) lazy-main)))


    (if (> (length args) 0)
        (gen-ast-l args lazy-operator)
        lazy-operator)))

;;
;; Make lazy code from NUMBER OPERATOR
;;
(define (mlc-op-num ast succ op)
  (letrec (   ;; Lazy code used if type test fail
              (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_NUM_EXPECTED))))
              ;; Lazy code of left operand
              (lazy-ast-left  (gen-ast (cadr ast)  lazy-ast-right))
              ;; Lazy code of right operand
              (lazy-ast-right (gen-ast (caddr ast) lazy-main))
              ;; Gen operation code (assumes both operands are num)
              (lazy-code-op (make-lazy-code
                              (lambda (cgc ctx)

                                ;; Arithmetic overflow
                                (let* ((ctx-overflow (ctx-pop ctx)) ;; First operand pop
                                       (lazy-overflow (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_ARR_OVERFLOW))))
                                       (label-jo (asm-make-label #f (new-sym 'jump-overflow)))
                                       ;; TODO : new kind of stub (add-callback without ret-addr and without selector?)
                                       ;; TODO : overflow stub could be global
                                       (stub-labels (add-callback cgc
                                                                  0
                                                                  (lambda (ret-addr selector)
                                                                    (gen-version (vector-ref label-jo 1) lazy-overflow ctx-overflow)))))

                                (x86-pop cgc (x86-rbx))
                                (case op
                                  (($+)
                                   (begin
                                     (x86-add cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  (($-)
                                   (begin
                                     (x86-sub cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  (($*)
                                   (begin (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                     (x86-imul cgc (x86-rbx) (x86-mem 0 (x86-rsp)))
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  (($quotient) ;; TODO : check '/0'
                                   (begin
                                          (x86-pop cgc (x86-rax))
                                          (x86-sar cgc (x86-rax) (x86-imm-int 2))
                                          (x86-cqo cgc)
                                          (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                          (x86-idiv cgc (x86-rbx))
                                          (x86-shl cgc (x86-rax) (x86-imm-int 2))
                                          (x86-push cgc (x86-rax))
                                          (x86-label cgc label-jo)
                                          (x86-jo cgc (list-ref stub-labels 0))))
                                  (($modulo) ;; TODO : check '/0'
                                   (begin
                                          (x86-pop cgc (x86-rax))
                                          (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                          (x86-sar cgc (x86-rax) (x86-imm-int 2))
                                          (x86-cqo cgc)
                                          (x86-idiv cgc (x86-rbx))
                                          (x86-mov cgc (x86-rax) (x86-rdx)) ;; (a%b) in rax, b in rbx
                                          (x86-add cgc (x86-rax) (x86-rbx)) ;; (a%b + b) in rax
                                          (x86-cqo cgc)
                                          (x86-idiv cgc (x86-rbx))
                                          (x86-shl cgc (x86-rdx) (x86-imm-int 2))
                                          (x86-push cgc (x86-rdx))))
                                  (($<)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-jl  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  (($>)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-jg  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  (($=)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-je  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  (else
                                    (error "unknown op" op)))
                                (jump-to-version cgc
                                                 succ
                                                 (ctx-push
                                                   (ctx-pop (ctx-pop ctx))
                                                   (cond ((member op '($+ $- $* $modulo $quotient)) CTX_NUM)
                                                         ((member op '($< $> $=)) CTX_BOOL))))))))
              ;; Lazy code, tests types from ctx to jump to the correct lazy-code
              (lazy-main (make-lazy-code
                           (lambda (cgc ctx)

                             (let ((left-type  (cadr (ctx-stack ctx)))
                                   (right-type (car (ctx-stack ctx)))
                                   ;; ctx with num for right operand
                                   (rctx  (ctx-push (ctx-pop ctx) CTX_NUM))
                                   ;; ctx with num for left operand
                                   (lctx  (make-ctx (cons (car (ctx-stack ctx)) (cons CTX_NUM (cddr (ctx-stack ctx)))) (ctx-env ctx)))
                                   ;; ctx with num for left AND right operand
                                   (lrctx (make-ctx (cons CTX_NUM (cons CTX_NUM (cddr (ctx-stack ctx)))) (ctx-env ctx))))

                               (cond ((eq? left-type CTX_NUM)
                                      (cond ((eq? right-type CTX_NUM)     (jump-to-version cgc lazy-code-op ctx))
                                            ((eq? right-type CTX_UNK) (jump-to-version cgc (gen-dyn-type-test CTX_NUM 0 rctx lazy-code-op ctx lazy-fail) ctx))
                                            (else                      (gen-error cgc ERR_NUM_EXPECTED))))

                                     ((eq? left-type CTX_UNK)
                                      (cond ((eq? right-type CTX_NUM)     (jump-to-version cgc (gen-dyn-type-test CTX_NUM 1 lctx lazy-code-op ctx lazy-fail) ctx))
                                            ((eq? right-type CTX_UNK) (let* ((right-test (gen-dyn-type-test CTX_NUM 1 lrctx lazy-code-op ctx lazy-fail))
                                                                              (left-test  (gen-dyn-type-test CTX_NUM 0 lctx right-test ctx lazy-fail)))
                                                                         (jump-to-version cgc left-test ctx)))
                                            (else                      (gen-error cgc ERR_NUM_EXPECTED))))
                                     (else (gen-error cgc ERR_NUM_EXPECTED))))))))
    ;; Return left operand lazy-code
    lazy-ast-left))

;;
;; Make lazy code from GENERIC OPERATOR
;;
(define (mlc-op-gen ast succ op)
  (let ((lazy-code-op (make-lazy-code
                        (lambda (cgc ctx)
                          (x86-pop cgc (x86-rbx))
                          (case op
                            (($eq?)
                             (let ((label-done
                                     (asm-make-label cgc (new-sym 'done))))
                               (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                               (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                               (x86-je  cgc label-done)
                               (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                               (x86-label cgc label-done)
                               (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                            (else
                              (error "unknown op" op)))
                          (jump-to-version cgc
                                           succ
                                           (ctx-push
                                             (ctx-pop (ctx-pop ctx))
                                             (cond ((member op '($eq?)) CTX_BOOL))))))))
    (gen-ast-l (cdr ast) lazy-code-op)))

;;
;; Make lazy code from TYPE TEST
;;
(define (mlc-test ast succ)
  (let* ((op (car ast))
         (lazy-test (make-lazy-code
                      (lambda (cgc ctx)
                        (let ((label-done (asm-make-label cgc (new-sym 'label_done))))
                          (x86-pop   cgc (x86-rax))
                          (if (eq? op '$number?)
                              ;; $number?
                              (begin (x86-and   cgc (x86-rax) (x86-imm-int 3))
                                     (x86-mov   cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                     (x86-je    cgc label-done)
                                     (x86-mov   cgc (x86-rax) (x86-imm-int (obj-encoding #f))))
                              ;; Others
                              (begin (x86-mov cgc (x86-rbx) (x86-rax))
                                     (x86-and cgc (x86-rax) (x86-imm-int 3))
                                     (x86-cmp cgc (x86-rax) (x86-imm-int 1)) ;; CMP avec tag memory allocated obj
                                     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                     (x86-jne cgc label-done)
                                     ;; It's a memory allocated obj
                                     (x86-mov cgc (x86-rbx) (x86-mem -1 (x86-rbx)))
                                     (x86-and cgc (x86-rbx) (x86-imm-int 248))
                                     (cond ((eq? op '$procedure?) (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_PROCEDURE)))) ;; STAG_PROCEDURE << 3
                                           ((eq? op '$pair?)      (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_PAIR))))      ;; STAG_PAIR << 3
                                           (else (error "NYI")))
                                     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                     (x86-jne cgc label-done)
                                     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))))
                          ;;
                          (x86-label cgc label-done)
                          (x86-push  cgc (x86-rax))
                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))))
    (gen-ast (cadr ast) lazy-test)))

;;
;; Make lazy code to create pair
;; Create pair with the too values on top of the stack
;;
(define (mlc-pair succ)
  (make-lazy-code
    (lambda (cgc ctx)
       (let ((header-word (+ (arithmetic-shift 3 8) (arithmetic-shift 1 3) 6))) ;; 000...011 | STAG_PAIR | 110 => Length | Pair | Permanent
         ;; TODO : mov directly to memory
         ;; Write object header
         (x86-mov cgc (x86-rax) (x86-imm-int header-word))
         (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
         (x86-pop cgc (x86-rbx)) ;; pop CDR
         (x86-pop cgc (x86-rax)) ;; pop CAR
         ;; Write pair
         (x86-mov cgc (x86-mem 8 alloc-ptr)  (x86-rax))
         (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rbx))
         ;; Tag,Push closure and update alloc-ptr
         (x86-mov cgc (x86-rax) alloc-ptr)
         (x86-add cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
         (x86-push cgc (x86-rax))
         (x86-add cgc alloc-ptr (x86-imm-int 24))
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

;; Global closure context table
(define global-cc-table '())
(define global-cc-table-maxsize 20)

;; Gen a new cc-table at 'alloc-ptr' and write 'stub-addr' in each slot
(define (gen-cc-table cgc stub-addr offset)
  (x86-mov cgc (x86-rax) (x86-imm-int stub-addr))
  (gen-cc-table-h cgc offset global-cc-table-maxsize))

;; Gen-cc-table helper
(define (gen-cc-table-h cgc offset nb-slots)
  (if (> nb-slots 0)
      (begin (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))
             (gen-cc-table-h cgc (+ offset 8) (- nb-slots 1)))))

;; Get cc-table index for 'ctx'. Associates a new index if ctx is a new one
(define (get-closure-index ctx)
  (let ((r (assoc (ctx-stack ctx) global-cc-table)))
    (if r
        (cdr r)
        (let ((idx (length global-cc-table)))
          (if (= idx global-cc-table-maxsize)
              (error "CC Table is full")
              (begin (set! global-cc-table (cons (cons (ctx-stack ctx) idx) global-cc-table))
                     idx))))))

;;
;; SYMBOL READ/WRITE
;;

;; Gen code to get a free var from closure
;; info is the lookup result which contains id info
;;  ex: info = '(n free . 0) for free var 'n' at index '0'
;; dest is the destination of free var. possible values are :
;;  'stack : push value on top of stack
;;  'gen-reg : general register (mov value into rax)
(define (gen-get-freevar cgc ctx variable dest)
   (let* ((offset (+ 15 (* 8 (identifier-offset (cdr variable)))))
          (clo-offset (* 8 (closure-pos (ctx-stack ctx)))))
      ;; Get closure
      (x86-mov cgc (x86-rax) (x86-mem clo-offset (x86-rsp)))
      ;; Get value & push
      (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
      (cond  ((eq? dest 'stack)   (x86-push cgc (x86-rax)))
             ((eq? dest 'gen-reg) #f) ;; Already in rax
             (else (error "Invalid destination")))
      CTX_UNK)) ;; TODO return free var info when implemented

;; Gen code to get a local var from stack
;; info is the lookup result which contains id info
;;  ex: info = '(n . 0) for local var 'n' at index '0'
;; dest is the destination of local var. possible values are :
;;  'stack : push value on top of stack
;;  'gen-reg : general register (mov value into rax)
(define (gen-get-localvar cgc ctx variable dest)
   (let* ((fs (length (ctx-stack ctx)))
          (pos (- fs 1 (identifier-offset (cdr variable)))))
      (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem (* pos 8) (x86-rsp))))
            ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (* pos 8) (x86-rsp))))
            (error "Invalid destination"))
      (list-ref (ctx-stack ctx) pos)))

;; Gen code to get a global var from memory
;; info is the lookup result which contains id info
;;  ex: info = '(n 0 . number) for local var 'n' at global index 0 with type info 'number'
;; dest is the destination of global var. possible values are :
;;  'stack : push value on top of stack
(define (gen-get-globalvar cgc ctx info dest)
   (x86-mov cgc (x86-rax) (x86-mem (* 8 (cadr info)) (x86-r10)))
   (if (eq? dest 'stack)
      (x86-push cgc (x86-rax))
      (error "Invalid destination"))
   (cddr info))

;;
;; FREE VARS
;;

;; Extends env with 'fvars' free vars starting with offset
(define (build-fenv fvars offset)
  (if (null? fvars)
      '()
      (cons (cons (car fvars) (make-identifier 'free offset '()))
            (build-fenv (cdr fvars) (+ offset 1)))))

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

;; TODO
(define (ensub lsta lstb res)
  (if (null? lsta)
    res
    (if (member (car lsta) lstb)
      (ensub (cdr lsta) lstb res)
      (ensub (cdr lsta) lstb (cons (car lsta) res)))))


;; TODO 
(define (mutated-vars-l lst params ctx) ;; TODO ctx ?
  (if (null? lst)
    '()
    (append (mutated-vars (car lst) params ctx) (mutated-vars-l (cdr lst) params ctx))))

;; TODO
(define (mutated-vars ast params ctx) ;; TODO ctx ?
  (cond ;; Literal & Symbol 
        ((or (null? ast) (number? ast) (boolean? ast) (symbol? ast)) '())
        ;; Pair
        ((pair? ast)
           (let ((op (car ast)))
              (cond ((eq? op 'lambda) (mutated-vars (caddr ast) (ensub params (cadr ast) '()) ctx))
                    ((eq? op 'set!)
                      (if (member (cadr ast) params)
                        (list (cadr ast))
                        '()))
                    (else (mutated-vars-l ast params ctx)))))))

;; Return all free vars used by the list of ast knowing env 'clo-env'
(define (free-vars-l lst clo-env ctx)
  (if (null? lst)
      '()
      (append (free-vars (car lst) clo-env ctx) (free-vars-l (cdr lst) clo-env ctx))))

;; Return all free vars used by ast knowing env 'clo-env'
(define (free-vars ast clo-env ctx)
  (cond ;; Literal
        ((or (number? ast) (boolean? ast)) '())
        ;; Symbol
        ((symbol? ast)
          (cond ((member ast clo-env) '())
                ((and (assoc ast globals)
                      (not (assoc ast (ctx-env ctx))))
                        '())
                (else (list ast))))
        ;; Pair
        ((pair? ast)
          (let ((op (car ast)))
            (cond ;; If
                  ((eq? op 'if) (append (free-vars (cadr ast)   clo-env ctx)   ; cond
                                        (free-vars (caddr ast)  clo-env ctx)   ; then
                                        (free-vars (cadddr ast) clo-env ctx))) ; else
                  ;; Quote
                  ((eq? op 'quote) '())
                  ;; Lambda
                  ((eq? op 'lambda) (free-vars (caddr ast) (append (cadr ast) clo-env) ctx))
                  ;; Special
                  ((member op '(set! $cons $car $cdr $$putchar $+ $- $* $quotient $modulo $< $> $= $eq? $number? $procedure? $pair?)) (free-vars-l (cdr ast) clo-env ctx))
                  ;; Call
                  (else (free-vars-l ast clo-env ctx)))))))

;;
;; UTILS
;;

;; Build new environment with ids dtarting from 'start'
;; ex : (buile-env '(a b c) 8) -> ((a . 8) (b . 9) (c . 10))
(define (build-env ids start)
  (if (null? ids)
    '()
    (cons (cons (car ids) (make-identifier 'local start '()))
          (build-env (cdr ids) (+ start 1)))))

;; Get position of first occurrence of CTX_CLO in ctx
(define (closure-pos stack)
  (if (null? stack)
      (error "Can't find " CTX_CLO)
      (if (eq? CTX_CLO (car stack))
          0
          (+ 1 (closure-pos (cdr stack))))))

;; Return label associated to function name
(define (lookup-fn name)
  (let ((r (assoc name functions)))
    (if r
      (cdr r)
      (cond ((eq? name '$$putchar)  label-$$putchar)
            (else (error "NYI"))))))

;; TODO : ctx_ids to solve segfault on ctx read
(define ctx_ids '())
