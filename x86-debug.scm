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

(define label-breakpoint #f)

;;-----------------------------------------------------------------------------
;; BREAKPOINT

;; Gen call to breakpoint label
(define (gen-breakpoint cgc)

  (push-pop-regs
     cgc
     all-regs
     (lambda (cgc)
       (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
       (x86-and  cgc (x86-rsp) (x86-imm-int -16))
       (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
       (x86-push cgc (x86-rdi))
       (x86-call cgc label-breakpoint) ;; call C function
       (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
       )))

;; Give a cmd to user to print registers or stack
(c-define (break_point sp) (long) void "break_point" ""

  (let ((reg-names '("R15" "R14" "R13" "R12" "R11" "R10" " R9" " R8"
                     "RDI" "RSI" "RBP" "RDX" "RCX" "RBX" "RAX" "RSP")))

  (define (get-regs regs offset)
    (if (null? regs)
      '()
      (cons (cons (car regs) (get-i64 (+ sp offset)))
            (get-regs (cdr regs) (+ offset 8)))))

  (define (println-slot slot)
    (let ((str (number->string slot)))
      (print slot (make-string (- 19 (string-length str)) #\space)))
    (print "|  ")
    (let ((tag (bitwise-and slot 3)))
      (cond ((= tag 0) (print (quotient slot 4)))
            ((= tag 1) (print "Mem object"))
            ((= slot ENCODING_VOID) (print "void"))
            ((= slot ENCODING_EOF)  (print "eof"))
            ((= tag 2) (print (encoding-obj slot)))))
    (newline))

  (define (print-stack n regs)
    (cond ((= n 0))
          ((= n 1)
             (println "       +--------------------+--------------------+")
             (print   "RSP -> | ") (println-slot (get-i64 (+ (* 8 (- n 1)) (cdr (assoc "RSP" regs)))))
             (println "       +--------------------+--------------------+"))
          (else
             (print-stack (- n 1) regs)
             (print   "       | ") (println-slot (get-i64 (+ (* 8 (- n 1)) (cdr (assoc "RSP" regs)))))
             (println "       +--------------------+--------------------+"))))

  (define (print-regs regs)
    (if (not (null? regs))
      (begin (print-regs (cdr regs))
             (print-reg  (car regs)))))

  (define (print-reg reg)
    (println (car reg) " = " (cdr reg)))

  (let ((regs (get-regs reg-names 0)))

    (define (run)
      (print ">")
      (let ((r (read-line)))
        (set! r (string-upcase r))
        (let ((reg (assoc r regs)))
          (cond ;; Print help
                ((string=? r "HELP")
                   (println "Breakpoint commands:")
                   (println "   r*      | print register (rax, rbx, ...)")
                   (println "   regs    | print registers")
                   (println "   stack   | print 5 stack slots")
                   (println "   stack n | print n stack slots")
                   (println "   next    | continue"))
                ;; Print one reg
                ((assoc r regs) => (lambda (r) (print-reg r)))
                ;; Print all regs
                ((string=? r "REGS")  (print-regs regs))
                ;; Print stack (5 slots)
                ((string=? r "STACK") (print-stack 5 regs))
                ;; Print stack (n slots)
                ((and (> (string-length r) 6)
                      (string=? (substring r 0 5) "STACK")
                      (char=? (string-ref r 5) #\space)
                      (string->number (substring r 6 (string-length r))))
                  (let ((num (string->number (substring r 6 (string-length r)))))
                    (if (and (integer? num)
                             (> num 0))
                        (print-stack num regs)
                        (println "Unknown command"))))
                ;; Continue
                ((string=? r "NEXT"))
                ;; Unknown
                (else (println "Unknown command")))
          (if (not (string=? r "NEXT"))
            (run)))))

    (println "\033[1;31m⚫ Breakpoint\033[0m")
    (run))))

;;-----------------------------------------------------------------------------
;; SLOT DEBUG

(define (get-slot slot)
  (get-i64 (+ block-addr (* 8 (cdr (assoc slot debug-slots))))))

(define (gen-set-slot cgc slot imm)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-push cgc (x86-rax))
    (x86-push cgc (x86-rbx))
    (x86-mov  cgc (x86-rax) (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov  cgc (x86-rbx) (x86-imm-int imm))
    (x86-mov  cgc (x86-mem 0 (x86-rax)) (x86-rbx))
    (x86-pop cgc (x86-rbx))
    (x86-pop cgc (x86-rax))))

(define (gen-get-slot cgc slot r)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-mov cgc r (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov cgc r (x86-mem 0 r))))

(define (gen-inc-slot cgc slot)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-push cgc (x86-rax))
    (x86-push cgc (x86-rbx))
    (x86-mov cgc (x86-rax) (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov cgc (x86-rbx) (x86-mem 0 (x86-rax)))
    (x86-inc cgc (x86-rbx))
    (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-rbx))
    (x86-pop cgc (x86-rbx))
    (x86-pop cgc (x86-rax))))
