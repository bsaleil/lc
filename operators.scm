;;
;; Make lazy code from N-ARY COMPARISON OPERATOR
;;
(define (mlc-op-n-cmp ast succ op)
   
   ;; False lazy code object
   (define lazy-false (make-lazy-code
                         (lambda (cgc ctx)
                            (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- (length ast) 1))))
                            (x86-push cgc (x86-imm-int (obj-encoding #f)))
                            (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx (- (length ast) 1))
                                                                CTX_BOOL)))))

   ;; Gen false stub from label & ctx
   (define (gen-false-stub label ctx)
      (add-callback #f 0 (lambda (ret-addr selector)
                            (gen-version (asm-label-pos label)
                                         lazy-false
                                         ctx))))

   ;; Build lazy code objects chain
   (define (build-chain lidx ridx)
      (if (or (< lidx 0) (< ridx 0))
        ;; All operands compared, clean stack and push #t
        (make-lazy-code
          (lambda (cgc ctx)
            (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- (length ast) 1))))
            (x86-push cgc (x86-imm-int (obj-encoding #t)))
            (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx (- (length ast) 1))
                                                CTX_BOOL))))
        ;; There is at least 1 comparison to perform
        (make-lazy-code
          (lambda (cgc ctx)
            (let ((label-jump (asm-make-label #f (new-sym 'label-jump)))
                  (x86-op (cdr (assoc op `((< . ,x86-jge) (> . ,x86-jle) (<= . ,x86-jg) (>= . ,x86-jl) (= . x86-jne))))))
              (x86-mov cgc (x86-rax) (x86-mem (* 8 lidx) (x86-rsp)))
              (x86-cmp cgc (x86-rax) (x86-mem (* 8 ridx) (x86-rsp)))
              (x86-label cgc label-jump)
              (x86-op cgc (list-ref (gen-false-stub label-jump ctx) 0))
              (jump-to-version cgc (build-chain (- lidx 1) (- ridx 1)) ctx))))))

   ;; Push operands and start comparisons
   (gen-ast-l (cdr ast)
              (build-chain (- (length ast) 2)
                           (- (length ast) 3))))