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
            (error "Can't find variable: " ast))))))

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
                                
                                (println ">>> selector= " selector)
                                (println ">>> prev-action= " prev-action)
                                
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
                                                
                                                (println ">>> swapping-branches")
                                                
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

;; Make lazy code from 'define
(define (mlc-define ast succ)
  (let* ((params (cadr (caddr ast)))
         (lazy-ret (make-lazy-code (lambda (cgc ctx)
                                     (x86-pop cgc (x86-rax)) ;; Ret val
                                     (x86-pop cgc (x86-rbx)) ;; Ret addr
                                     (x86-add cgc (x86-rsp) (x86-imm-int (* (length params) 8)))
                                     (x86-push cgc (x86-rax))
                                     (x86-jmp cgc (x86-rbx)))))
         (lazy-body (gen-ast (caddr (caddr ast)) lazy-ret))
         (function-name (cadr ast)))
    (make-lazy-code (lambda (cgc ctx)
                      (let* ((stub-labels (add-fn-callback cgc
                                                           0
                                                           (lambda (ret-addr selector call-site-addr ctx)
                                                             ;; Add params to env
                                                             (let ((ctx (make-ctx (ctx-stack ctx) (build-env params 0))))
                                                               (gen-version-fn call-site-addr lazy-body ctx)))))
                             (function-label (list-ref stub-labels 0)))
                        (set! functions (cons (cons function-name function-label) functions))
                        (jump-to-version cgc succ ctx))))))

;; Return label associated to function name
(define (lookup-fn name)
  (let ((r (assoc name functions)))
    (if r
      (cdr r)
      (cond ((eq? name '$$putchar) label-$$putchar)
            (else (error "NYI"))))))

;; Make lazy code from call expr
(define (mlc-call ast succ)
  (let* ((opid (car ast))
         (args (cdr ast))
         (lazy-call (make-lazy-code (lambda (cgc ctx)
                                      (let* ((fun-label (lookup-fn opid))
                                             (load-ret-label (asm-make-label cgc (new-sym 'load-ret-addr)))
                                             ;; Flag in stub : is the continuation already generated ?
                                             (gen-flag #f)
                                             ;; Create continuation stubs 
                                             (stub-labels (add-callback cgc
                                                                        0
                                                                        (lambda (ret-addr selector)
                                                                          (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag = continuation addr
                                                                              (set! gen-flag (gen-version-continuation load-ret-label
                                                                                                                       succ
                                                                                                                       (ctx-push (ctx-pop-nb ctx (length args)) 'unknown)))) ;; remove args and add ret val
                                                                          gen-flag))))
                                        ;; Return address
                                        (x86-label cgc load-ret-label)
                                        (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1))) ;; Push continuation label addr
                                        (x86-push cgc (x86-rax))
                                        
                                        ;; Call ctx in rdx
                                        (let* ((call-stack (cons 'retAddr (list-head (ctx-stack ctx) (length args))))
                                               (call-ctx   (make-ctx call-stack '())))
                                          (x86-mov cgc (x86-rdx) (x86-imm-int (obj-encoding call-ctx))))

                                        ;; Call function stub to keep address of this call site
                                        (x86-call cgc fun-label))))))
    
    (if (> (length args) 0)
        (gen-ast-l args lazy-call) ;; Gen args and give lazy-call as successor
        lazy-call)))

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
                                
                                (x86-pop cgc (x86-rax))
                                (case op
                                  ((+)
                                   (begin
                                     (x86-add cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  ((-)
                                   (begin
                                     (x86-sub cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  ((*)
                                   (begin (x86-shr cgc (x86-rax) (x86-imm-int 2))
                                     (x86-imul cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  ((<)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                     (x86-jl  cgc label-done)
                                     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))))
                                  ((=)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                     (x86-je  cgc label-done)
                                     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))))
                                  (else
                                    (error "unknown op" op)))
                                (jump-to-version cgc
                                                 succ
                                                 (ctx-push
                                                   (ctx-pop (ctx-pop ctx))
                                                   (cond ((member op '(+ - *)) 'num)
                                                         ((member op '(< =)) 'bool))))))))
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
