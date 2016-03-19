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

(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

(define LINUX_SYSCALL
  '((close . 3)
    (open  . 2)
    (write . 1)
    (read  . 0)))

;;-----------------------------------------------------------------------------
;; OPEN

(define (gen-syscall-open cgc direction)

  (define flags (if (eq? direction 'in)
                  0     ;; O_RDONLY
                  577)) ;; O_WRONLY | O_CREAT | O_TRUNC

  (define permissions 420) ;; S_IRUSR |  S_IWUSR | S_IRGRP | S_IROTH

  (define err (if (eq? direction 'in)
                ERR_OPEN_INPUT_FILE
                ERR_OPEN_OUTPUT_FILE))

  ;; Save destroyed regs
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))

  ;; c-string argument (rdi)
  (x86-mov cgc (x86-rdi) (x86-rax)) ;; Str in rdi (Str is in rax)
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

  ;; Port is in rax

  ;; Save destroyed regs
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))

  ;; File descriptor argument (rdi)
  (x86-mov cgc (x86-rdi) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
  ;; syscall number (rax)
  (x86-mov cgc (x86-rax) (x86-imm-int (cdr (assoc 'close LINUX_SYSCALL))))

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
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))

  ;; file descriptor (rdi)
  (x86-mov cgc (x86-rdi) (x86-rax)) ;; Port object in rdi (port is in rax)
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
;; TODO: stdout ??
(define (gen-syscall-write-char cgc)

  ;; Save destroyed regs
  (x86-push cgc (x86-rcx)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-r11)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-push cgc (x86-rdi))
  (x86-push cgc (x86-rsi))
  (x86-push cgc (x86-rdx))

  ;; file descriptor (rdi)
  (x86-mov cgc (x86-rdi) (x86-rax)) ;; Port object in rdi (port is in rax)
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
  (x86-pop cgc (x86-rdx))
  (x86-pop cgc (x86-rsi))
  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-r11))
  (x86-pop cgc (x86-rcx)))
