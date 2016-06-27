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

(define C_CLOCK_MONOTONIC 1)
(define C_SIZEOF_TIMESPEC 16)
(define C_TIMESPEC_SEC_OFFSET 0)
(define C_TIMESPEC_NSEC_OFFSET 8)

(define LINUX_SYSCALL
  '((clock_gettime . 228)))

;;-----------------------------------------------------------------------------
;; WRITE-CHAR
(define (gen-syscall-clock-gettime cgc)

  (x86-label cgc (asm-make-label #f (new-sym 'syscall_clock-gettime_)))

  ;; Save destroyed regs
  (x86-upush cgc (x86-rcx)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-upush cgc (x86-r11)) ;; Destroyed by kernel (System V Application Binary Interface AMD64 Architecture Processor Supplement section A.2)
  (x86-upush cgc (x86-rdi))
  (x86-upush cgc (x86-rsi))

  ;; Clock in rdi
  (x86-mov cgc (x86-rdi) (x86-imm-int C_CLOCK_MONOTONIC))

  ;; timespect struct address in rsi
  ;; alloc struct in pstack!
  (x86-sub cgc (x86-rsp) (x86-imm-int C_SIZEOF_TIMESPEC))
  (x86-mov cgc (x86-rsi) (x86-rsp))

  ;; syscall number (rax)
  (x86-mov cgc (x86-rax) (x86-imm-int (cdr (assoc 'clock_gettime LINUX_SYSCALL))))

  (x86-syscall cgc)

  ;;
  (x86-mov cgc (x86-rax) (x86-mem C_TIMESPEC_SEC_OFFSET (x86-rsp)))
  (x86-mov cgc (x86-rcx) (x86-imm-int (expt 10 9)))
  (x86-imul cgc (x86-rax) (x86-rcx))
  (x86-add cgc (x86-rax) (x86-mem C_TIMESPEC_NSEC_OFFSET (x86-rsp)))

  (x86-add cgc (x86-rsp) (x86-imm-int C_SIZEOF_TIMESPEC))

  (x86-upop cgc (x86-rsi))
  (x86-upop cgc (x86-rdi))
  (x86-upop cgc (x86-r11))
  (x86-upop cgc (x86-rcx)))
