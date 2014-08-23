
(define label-$$putchar  (asm-make-label #f '$$putchar))

;; PUTCHAR

(define (gen-$$putchar cgc)
  ;; label
  (x86-label cgc label-$$putchar)
  ;; save used regs
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdx))
  (x86-push cgc (x86-rdi))
  ;; linux syscall code
  (x86-mov cgc (x86-rax) (x86-imm-int 1)) ;; LINUX
  ;; untag char
  (x86-mov cgc (x86-rsi) (x86-mem 40 (x86-rsp)))
  (x86-shr cgc (x86-rsi) (x86-imm-int 2))
  (x86-mov cgc (x86-mem 40 (x86-rsp)) (x86-rsi))
  ;; putchar (char is in [rsp+40])
  (x86-mov cgc (x86-rsi) (x86-rsp))
  (x86-add cgc (x86-rsi) (x86-imm-int 40))
  (x86-mov cgc (x86-rdx) (x86-imm-int 1))
  (x86-mov cgc (x86-rdi) (x86-imm-int 1)) 
  (x86-syscall cgc)
  ;; restore used regs
  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-rdx))
  (x86-pop cgc (x86-rsi))
  ;; remove call site addr
  (x86-add cgc (x86-rsp) (x86-imm-int 8))
  ;; replace char with return value (#!void)
  (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-imm-int -18) 64) ;; (-18) is #!void
  (x86-ret cgc))