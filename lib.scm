
(define label-$$putchar  (asm-make-label #f '$$putchar))

;; PUTCHAR

(define (gen-$$putchar cgc)  
  ;; label
  (x86-label cgc label-$$putchar)
  ;; SAVE destroyed regs TODO
  (x86-push cgc (x86-rcx))
  (x86-push cgc (x86-r11))
  ;; save used regs
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdx))
  (x86-push cgc (x86-rdi))
  ;; linux syscall code
  (x86-mov cgc (x86-rax) (x86-imm-int 1)) ;; LINUX
  ;; untag char
  (x86-mov cgc (x86-rsi) (x86-mem 48 (x86-rsp)))
  (x86-shr cgc (x86-rsi) (x86-imm-int 2))
  (x86-mov cgc (x86-mem 48 (x86-rsp)) (x86-rsi))
  ;; putchar (char is in [rsp+40])
  (x86-mov cgc (x86-rsi) (x86-rsp)) ;; TODO lea ?
  (x86-add cgc (x86-rsi) (x86-imm-int 48))
  (x86-mov cgc (x86-rdx) (x86-imm-int 1))
  (x86-mov cgc (x86-rdi) (x86-imm-int 1)) 
  (x86-syscall cgc)
  ;; restore used regs
  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-rdx))
  (x86-pop cgc (x86-rsi))
  ;; RESTORE destroyed regs TODO
  (x86-pop cgc (x86-rcx))
  (x86-pop cgc (x86-r11))
  ;; remove call site addr
  (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-imm-int -18) 64) ;; (-18) = #!void
  (x86-ret cgc))