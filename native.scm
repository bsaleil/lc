
(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

(define LINUX_SYSCALL
  '((close . 3)
    (open  . 2)
    (write . 1)
    (read  . 0)))

(define (gen-native cgc)
  (gen-$$putchar cgc))

(define label-$$putchar  (asm-make-label #f '$$putchar))

;;-----------------------------------------------------------------------------
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
  (x86-pop cgc (x86-r11))
  (x86-pop cgc (x86-rcx))
  ;; remove call site addr
  (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-imm-int -18) 64) ;; (-18) = #!void
  (x86-ret cgc))

;;-----------------------------------------------------------------------------
;; OPEN

(define (gen-syscall-open cgc direction)
  
  (define flags (if (eq? direction 'in)
                  0    ;;  0 is O_RDONLY
                  577)) ;; 65 is O_WRONLY | O_CREAT | O_TRUNC
  
  (define permissions 420) ;; S_IRUSR |  S_IWUSR | S_IRGRP | S_IROTH
  
  (define err (if (eq? direction 'in)
                ERR_OPEN_INPUT_FILE
                ERR_OPEN_OUTPUT_FILE))
  
  ;; Save destroyed regs
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel 
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel 
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))
  
  ;; c-string argument (rdi)
  (x86-mov cgc (x86-rdi) (x86-mem 32 (x86-rsp))) ;; Str in rdi
  (x86-mov cgc (x86-rbx) (x86-mem (- 8 TAG_MEMOBJ) (x86-rdi)))  ;; Str length in rbx
  (x86-shr cgc (x86-rbx) (x86-imm-int 2))        ;; Decode str length
  (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx) (x86-rdi)) (x86-imm-int 0) 8) ;; Add null char (c string)
  (x86-add cgc (x86-rdi) (x86-imm-int (- 16 TAG_MEMOBJ))) ;; Rdi is now a c string
  ;; flags argument (rsi)
  (x86-mov cgc (x86-rsi) (x86-imm-int flags))
  ;; mode argument (rdx)
  ;; mode arg is ignored if 'in because O_CREAT is not specified.
  (if (eq? direction 'out)
      (x86-mov cgc (x86-rdx) (x86-imm-int permissions)))
  ;; syscall number (rax)
  (x86-mov cgc (x86-rax) (x86-imm-int (cdr (assoc 'open LINUX_SYSCALL))))

  ;; perform syscall
  (x86-syscall cgc)

  ;; Returned value
  (let ((label-syscall-ok (asm-make-label #f (new-sym 'label-syscall-ok))))
    (x86-cmp cgc (x86-rax) (x86-imm-int 0))
    ;; Result is >= then syscall is ok
    (x86-jge cgc label-syscall-ok)
      ;; Else syscall failed
      (gen-error cgc err)
    (x86-label cgc label-syscall-ok))
  ;; Restore destroyed regs
  (x86-pop cgc (x86-rsi))
  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-r11))
  (x86-pop cgc (x86-rcx)))

;;-----------------------------------------------------------------------------
;; CLOSE

(define (gen-syscall-close cgc)
  
  (x86-pop cgc (x86-rax)) ;; Port in rax
  
  ;; Save destroyed regs
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel 
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel 
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))
  
  ;; File descriptor argument (rdi)
  (x86-mov cgc (x86-rdi) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
  ;; syscall number (rax)
  (x86-mov cgc (x86-rax) (x86-imm-int (cdr (assoc 'close LINUX_SYSCALL))))
  
  ;; perform syscall
  (x86-syscall cgc)
  
  ;; No effect if the file has already been closed

  ;; Restore destroyed regs
  (x86-pop cgc (x86-rsi))
  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-r11))
  (x86-pop cgc (x86-rcx)))

;;-----------------------------------------------------------------------------
;; READ-CHAR

(define (gen-syscall-read-char cgc)

  ;; Save destroyed regs
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel 
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))

  ;; file descriptor (rdi)
  (x86-mov cgc (x86-rdi) (x86-mem 32 (x86-rsp))) ;; Port object in rdi
  (x86-mov cgc (x86-rdi) (x86-mem (- 8 TAG_MEMOBJ) (x86-rdi)))
  ;; syscall number (rax)
  (x86-mov cgc (x86-rax) (x86-imm-int (cdr (assoc 'read LINUX_SYSCALL))))
  ;; buffer (rsi)
  (x86-push cgc (x86-rax)) ;; buffer is top of stack
  (x86-mov cgc (x86-rsi) (x86-rsp))
  ;; count (rdx)
  (x86-mov cgc (x86-rdx) (x86-imm-int 1)) ;; Read only one byte

  ;; perform syscall
  (x86-syscall cgc)

  ;; Returned value
  (let ((label-read-eof (asm-make-label #f (new-sym 'label-read-eof)))
        (label-read-ch  (asm-make-label #f (new-sym 'label-read-ch))))
  (x86-cmp cgc (x86-rax) (x86-imm-int 0))
  (x86-pop cgc (x86-rax)) ;; Pop buffer
  ;; if res = 0, EOF is read
  (x86-je cgc label-read-eof) ;; = 0, EOF
  ;; if res > 0, a CHAR is read
  (x86-jg cgc label-read-ch)  ;; > 0, CHAR
  ;; else, it's an error
  (gen-error cgc ERR_READ_CHAR)

  ;; EOF : mov non encoded EOF value
  (x86-label cgc label-read-eof)
  (x86-mov cgc (x86-rax) (x86-imm-int NENCODING_EOF))
  ;; Encode special (char or eof)
  (x86-label cgc label-read-ch)
  (x86-shl cgc (x86-rax) (x86-imm-int 2))
  (x86-add cgc (x86-rax) (x86-imm-int TAG_SPECIAL)))
  ;; Restore destroyed regs
  (x86-pop cgc (x86-rsi))
  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-r11))
  (x86-pop cgc (x86-rcx)))

;;-----------------------------------------------------------------------------
;; WRITE-CHAR

(define (gen-syscall-write-char cgc)

  ;; Save destroyed regs
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel 
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))

  ;; file descriptor (rdi)
  (x86-mov cgc (x86-rdi) (x86-mem 32 (x86-rsp))) ;; Port object in rdi
  (x86-mov cgc (x86-rdi) (x86-mem (- 8 TAG_MEMOBJ) (x86-rdi)))
  ;; buffer (rsi)
  (x86-mov cgc (x86-rax) (x86-mem 40 (x86-rsp)))
  (x86-shr cgc (x86-rax) (x86-imm-int 2))
  (x86-mov cgc (x86-mem 40 (x86-rsp)) (x86-rax))
  (x86-lea cgc (x86-rsi) (x86-mem 40 (x86-rsp)))
  ;; count (rdx)
  (x86-mov cgc (x86-rdx) (x86-imm-int 1)) ;; Read only one byte
  ;; syscall number (rax)
  (x86-mov cgc (x86-rax) (x86-imm-int (cdr (assoc 'write LINUX_SYSCALL))))

  ;; perform syscall
  (x86-syscall cgc)

  ;; Returned value
  (let ((label-syscall-ok (asm-make-label #f (new-sym 'label-syscall-ok))))
    (x86-cmp cgc (x86-rax) (x86-imm-int 1))
    ;; Result is >= then syscall is ok
    (x86-je cgc label-syscall-ok)
      ;; Else syscall failed
      (gen-error cgc ERR_WRITE_CHAR)
    (x86-label cgc label-syscall-ok))
  
  ;; Restore destroyed regs
  (x86-pop cgc (x86-rsi))
  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-r11))
  (x86-pop cgc (x86-rcx)))