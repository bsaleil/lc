;;-----------------------------------------------------------------------------

;; AST FUNCTIONS

;;-----------------------------------------------------------------------------

;; Make lazy code from literal
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
                                 (cond ((number? ast) 'num)
                                       ((boolean? ast) 'bool)))))))

;; Make lazy code from symbol
(define (mlc-symbol ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (let* ((fs (length (ctx-stack ctx)))
             (lookup-res (assoc ast (ctx-env ctx))))
        (if lookup-res ;; Id exists
            (let ((pos (- fs 1 (cdr lookup-res))))
              (x86-push cgc (x86-mem (* pos 8) (x86-rsp)))
              (jump-to-version cgc
                               succ
                               (ctx-push ctx
                                         (list-ref (ctx-stack ctx) pos))))
            ;; Else, lookup in globals
            (let ((glookup-res (assoc ast globals)))
              (if glookup-res
                  (begin (x86-mov cgc (x86-rax) (x86-mem (* -8 (cdr glookup-res)) (x86-r10)))
                         (x86-push cgc (x86-rax))
                         (jump-to-version cgc succ (ctx-push ctx 'unknown))) ;; TODO, get ctx info of global
                  (error "Can't find variable: " ast))))))))

;; TODO
;; TODO : enlever l'utilisation de r9 ?
(define globals '())

;; Make lazy code from 'define
(define (mlc-define ast succ)
  (let ((lazy-bind (make-lazy-code (lambda (cgc ctx)
                                     (pp ctx)
                                     (x86-pop cgc (x86-rax))
                                     ;; TODO
                                     (x86-mov cgc (x86-mem (* -8 (length globals)) (x86-r10)) (x86-rax))
                                     (set! globals (cons (cons (cadr ast) (length globals)) globals))
                                     (x86-push cgc (x86-imm-int -18)) ;; -18 = #!void
                                     (jump-to-version cgc succ (ctx-push (ctx-pop ctx) 'void))))))
    (gen-ast (caddr ast) lazy-bind)))

;; TODO
(define (mlc-begin ast succ)
  (let* ((lazy-d (gen-ast (caddr ast) succ))
        (lazy-inter (make-lazy-code
                      (lambda (cgc ctx)
                        (x86-pop cgc (x86-rax))
                        (jump-to-version cgc lazy-d (ctx-pop ctx)))))
        (lazy-u (gen-ast (cadr ast) lazy-inter)))
    lazy-u))

;; TODO
(define (mlc-special ast succ)
  (let* ((name (car ast))
         (label (cond ((eq? name '$$putchar) label-$$putchar)
                      (else "NYI special")))
         (lazy-special (make-lazy-code 
                         (lambda (cgc ctx)
                           (x86-call cgc label)
                           (jump-to-version cgc succ (ctx-push (ctx-pop ctx) 'void)))))) ;; TODO : ctx changes with other special
    (if (> (length (cdr ast)) 0)
        (gen-ast-l (cdr ast) lazy-special)
        lazy-special)))

;; Make lazy code from 'lambda
(define (mlc-lambda ast succ)
  (let* (;; Lambda parameters
         (params (cadr ast))
         ;; Lazy lambda return
         (lazy-ret (make-lazy-code (lambda (cgc ctx)
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
         (lazy-body (gen-ast (caddr ast) lazy-ret)))
    
    ;; Lazy closure generation
    (make-lazy-code
      (lambda (cgc ctx)
        (let* (;; Create lambda stub
               (stub-labels (add-fn-callback cgc
                                             0
                                             (lambda (sp ctx ret-addr selector closure)
                                               ;; Extends env with params
                                               (let ((ctx (make-ctx (ctx-stack ctx) (build-env params 0))))
                                                 (gen-version-fn closure lazy-body ctx)))))
               (stub-addr (vector-ref (list-ref stub-labels 0) 1)))
          
          ;; Write object header
          (x86-mov cgc (x86-rax) (x86-imm-int 2678)) ;; 000..1010 | 01110 | 110 => Length=table.length | Procedure | Permanent
          (x86-mov cgc (x86-mem 0  (x86-r9)) (x86-rax))
          (x86-add cgc (x86-r9) (x86-imm-int 8))
          ;; Write closure context table
          (gen-cc-table cgc stub-addr)
          ;; Tag and push closure
          (x86-mov cgc (x86-rax) (x86-r9))
          (x86-sub cgc (x86-rax) (x86-imm-int (+ -1 8))) ;; 1 = tag, -8 = head word
          (x86-push cgc (x86-rax))
          ;; Update alloc ptr
          (x86-add cgc alloc-ptr (x86-imm-int (* 8 global-cc-table-maxsize)))
          ;; Jump to next
          (jump-to-version cgc
                           succ
                           (ctx-push ctx
                                     'lambda)))))))

;; Make lazy code from 'if
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

;; Return label associated to function name
(define (lookup-fn name)
  (let ((r (assoc name functions)))
    (if r
      (cdr r)
      (cond ((eq? name '$$putchar)  label-$$putchar)
            (else (error "NYI"))))))

;;
;; CC Table (Closure Context Table) :
;; A closure contains multiple possible entry points (fixed number) for the procedure
;; Each slot contains initially the address of the procedure stub
;; As soon as a version is generated for a context, the slot is replaced by the generated address
;;
;; EX : closure at initial state
;; +----------------+---------+---------+---------+---------+---------+
;; |Header          |Stub addr|Stub addr|Stub addr|   ...   |Stub addr|
;; |(Same as gambit)|         |         |         |         |         |
;; +----------------+---------+---------+---------+---------+---------+
;;                   index  0  index  1  index  2     ...    index  n
;;
;; EX closure with two existing versions
;; +----------------+---------+---------+---------+---------+---------+
;; |Header          |Proc addr|Stub addr|Proc addr|   ...   |Stub addr|
;; |(Same as gambit)|(ctx1)   |         |(ctx5)   |         |         |
;; +----------------+---------+---------+---------+---------+---------+
;;                   index  0  index  1  index  2     ...    index  n
;;

;; Global closure context table
(define global-cc-table '())
(define global-cc-table-maxsize 10)

;; Get closure index for 'ctx' associates a new index if ctx is a new one
(define (get-closure-index ctx)
  (let ((r (assoc (ctx-stack ctx) global-cc-table)))
    (if r
        (cdr r)
        (let ((idx (length global-cc-table)))
          (if (= idx global-cc-table-maxsize)
              (error "CC Table is full")
              (begin (set! global-cc-table (cons (cons (ctx-stack ctx) idx) global-cc-table))
                     idx))))))
              
;; Gen a new cc-table at 'alloc-ptr' and write 'stub-addr'
(define (gen-cc-table cgc stub-addr)
  (x86-mov cgc (x86-rax) (x86-imm-int stub-addr))
  (gen-cc-table-h cgc 0 global-cc-table-maxsize))           

(define (gen-cc-table-h cgc offset nb-slots)
  (if (> nb-slots 0)
      (begin (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))
             (gen-cc-table-h cgc (+ offset 8) (- nb-slots 1)))))

;; Make lazy code from call expr
(define (mlc-call ast succ)
  (let* (;; Call arguments
         (args (cdr ast))
         ;; Label for return address loading
         (load-ret-label (asm-make-label #f (new-sym 'load-ret-addr)))
         ;; Lazy call
         (lazy-call (make-lazy-code (lambda (cgc ctx)
                                      (let* (;; Flag in stub : is the continuation already generated ?
                                             (gen-flag #f)
                                             ;; Continuation stub
                                             (stub-labels (add-callback cgc
                                                                        0
                                                                        (lambda (ret-addr selector)
                                                                          ;; Remove lambda and args from ctx, and add retval (unknown)
                                                                          (let ((ctx-continuation (ctx-push (ctx-pop (ctx-pop-nb ctx (length args))) 'unknown)))
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
                                        (let* ((call-stack (cons 'ctx (cons 'retAddr (list-head (ctx-stack ctx) (+ 1 (length args))))))
                                               (call-ctx   (make-ctx call-stack '())))
                                          
                                          (let ((ctxid (length LES_CTX)))
                                            (set! LES_CTX (cons (cons ctxid call-ctx) LES_CTX))
                                            (x86-mov cgc (x86-rax) (x86-imm-int ctxid))
                                            (x86-push cgc (x86-rax)))
                                        
                                          ;; Get cc table offset for this ctx
                                          (let* ((closure-index (get-closure-index call-ctx))
                                                 (offset (- (+ (* closure-index 8) 8) 1))) ;; -1 procdure tag, +8 header word
                                            ;(gen-dump-stack cgc)
                                            
                                            
                                            (x86-mov cgc (x86-rax) (x86-mem 16 (x86-rsp)))
                                            (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax))))
                                          
                                          
                                          (x86-jmp cgc (x86-rax)))))))
         ;; Lazy callee
         (lazy-callee (gen-ast (car ast) lazy-call)))
    
    (if (> (length args) 0)
        (gen-ast-l args lazy-callee)
        lazy-callee)))

(define LES_CTX '())

;; Make lazy code from special form $$msg
(define (mlc-$$msg ast succ)
  (make-lazy-code (lambda (cgc ctx)
                    (gen-error cgc ctx (cadr ast) #f)
                    (jump-to-version cgc succ ctx))))

;; Make lazy code from operator
(define (mlc-op ast succ op)
  (letrec (   ;; Lazy code used if type test fail
              (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc ctx ERR_NUM_EXPECTED))))
              ;; Lazy code of left operand
              (lazy-ast-left  (gen-ast (cadr ast)  lazy-ast-right))
              ;; Lazy code of right operand
              (lazy-ast-right (gen-ast (caddr ast) lazy-main))
              ;; Gen operation code (assumes both operands are 'num)
              (lazy-code-op (make-lazy-code
                              (lambda (cgc ctx)
                                
                                ;; Arithmetic overflow
                                (let* ((ctx-overflow (ctx-pop ctx)) ;; First operand pop
                                       (lazy-overflow (make-lazy-code (lambda (cgc ctx) (gen-error cgc ctx ERR_ARR_OVERFLOW))))
                                       (label-jo (asm-make-label #f (new-sym 'jump-overflow)))
                                       ;; TODO : new kind of stub (add-callback without ret-addr and without selector?)
                                       ;; TODO : overflow stub could be global
                                       (stub-labels (add-callback cgc
                                                                  0
                                                                  (lambda (ret-addr selector)
                                                                    (gen-version (vector-ref label-jo 1) lazy-overflow ctx-overflow)))))
                                
                                (x86-pop cgc (x86-rbx))
                                (case op
                                  ((+)
                                   (begin
                                     (x86-add cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  ((-)
                                   (begin
                                     (x86-sub cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  ((*)
                                   (begin (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                     (x86-imul cgc (x86-rbx) (x86-mem 0 (x86-rsp)))
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  ((quotient) ;; TODO : check '/0'
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
                                  ((modulo) ;; TODO : check '/0'
                                   (begin
                                          (x86-pop cgc (x86-rax))
                                          (x86-sar cgc (x86-rax) (x86-imm-int 2))
                                          (x86-cqo cgc)
                                          (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                          (x86-idiv cgc (x86-rbx))
                                          (x86-shl cgc (x86-rdx) (x86-imm-int 2))
                                          (x86-push cgc (x86-rdx))
                                          (x86-label cgc label-jo)
                                          (x86-jo cgc (list-ref stub-labels 0))))
                                  ((<)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-jl  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  ((>)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-jg  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  ((=)
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
                                                   (cond ((member op '(+ - * modulo quotient)) 'num)
                                                         ((member op '(< > =)) 'bool))))))))
              ;; Lazy code, tests types from ctx to jump to the correct lazy-code  
              (lazy-main (make-lazy-code
                           (lambda (cgc ctx)
                             
                             (let ((left-type  (cadr (ctx-stack ctx)))
                                   (right-type (car (ctx-stack ctx)))
                                   ;; ctx with 'num for right operand
                                   (rctx  (ctx-push (ctx-pop ctx) 'num))
                                   ;; ctx with 'num for left operand
                                   (lctx  (make-ctx (cons (car (ctx-stack ctx)) (cons 'num (cddr (ctx-stack ctx)))) (ctx-env ctx)))
                                   ;; ctx with 'num for left AND right operand
                                   (lrctx (make-ctx (cons 'num (cons 'num (cddr (ctx-stack ctx)))) (ctx-env ctx))))
                                                           
                               (cond ((eq? left-type 'num)
                                      (cond ((eq? right-type 'num)     (jump-to-version cgc lazy-code-op ctx))
                                            ((eq? right-type 'unknown) (jump-to-version cgc (gen-dyn-type-test 0 rctx lazy-code-op ctx lazy-fail) ctx))
                                            (else                      (gen-error cgc ctx ERR_NUM_EXPECTED))))
                                     
                                     ((eq? left-type 'unknown)
                                      (cond ((eq? right-type 'num)     (jump-to-version cgc (gen-dyn-type-test 1 lctx lazy-code-op ctx lazy-fail) ctx))
                                            ((eq? right-type 'unknown) (let* ((right-test (gen-dyn-type-test 1 lrctx lazy-code-op ctx lazy-fail))
                                                                              (left-test  (gen-dyn-type-test 0 lctx right-test ctx lazy-fail)))
                                                                         (jump-to-version cgc left-test ctx)))
                                            (else                      (gen-error cgc ctx ERR_NUM_EXPECTED))))
                                     (else (gen-error cgc ctx ERR_NUM_EXPECTED))))))))
    ;; Return left operand lazy-code
    lazy-ast-left))