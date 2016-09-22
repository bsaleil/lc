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

;;-----------------------------------------------------------------------------
;; x86-push/pop redef

(define x86-ppush #f)
(define x86-ppop  #f)
(define x86-upush #f)
(define x86-upush #f)

(define (x86-upush-l cgc opnds)
  (x86-sub cgc (x86-usp) (x86-imm-int (* 8 (length opnds))))
  (let loop ((offset (* 8 (- (length opnds) 1)))
             (l opnds))
    (if (not (null? l))
        (begin (x86-mov cgc (x86-mem offset (x86-usp)) (car l))
               (loop (- offset 8) (cdr l))))))

(define (x86-upop-l cgc opnds)
  (let loop ((offset 0)
             (l opnds))
    (if (not (null? l))
        (begin (x86-mov cgc (car l) (x86-mem offset (x86-usp)))
               (loop (+ offset 8) (cdr l)))))
  (x86-add cgc (x86-usp) (x86-imm-int (* 8 (length opnds)))))

(let ((gpush x86-push)
      (gpop  x86-pop)
      (push/pop-error
        (lambda n
          (error "Internal error, do *NOT* directly use x86-push/pop functions."))))
  (set! x86-ppush gpush)
  (set! x86-ppop  gpop)
  (set! x86-upush
        (lambda (cgc op)
          (cond ((x86-imm? op)
                  (x86-sub cgc (x86-usp) (x86-imm-int 8))
                  (x86-mov cgc (x86-mem 0 (x86-usp)) op 64))
                ((x86-mem? op)
                  ;; TODO
                  (x86-ppush cgc (x86-rax))
                  (x86-mov cgc (x86-rax) op)
                  (x86-sub cgc (x86-usp) (x86-imm-int 8))
                  (x86-mov cgc (x86-mem 0 (x86-usp)) (x86-rax))
                  (x86-ppop cgc (x86-rax)))
                (else
                  (x86-sub cgc (x86-usp) (x86-imm-int 8))
                  (x86-mov cgc (x86-mem 0 (x86-usp)) op)))))
  (set! x86-upop
        (lambda (cgc op)
          (x86-mov cgc op (x86-mem 0 (x86-usp)))
          (x86-add cgc (x86-usp) (x86-imm-int 8))))
  (set! x86-push push/pop-error)
  (set! x86-pop  push/pop-error))

;;-----------------------------------------------------------------------------
;; x86-call redef

;; x86-call function produce an error.
;; x86-call could generate a call with a non aligned return address
;; which may cause trouble to the gc
;; Use specialized *x86-call-label-unaligned-ret* and *x86-call-label-aligned-ret* instead
;; x86-call-label-aligned-ret is a ucall: store ret addr in ustack and align return address
;; x86-call-label-unaligned-ret is a pcall: store ret addr in pstack and does not change return address

;; Generate a call to a label with a return address not necessarily aligned to 4
(define x86-call-label-unaligned-ret #f)

(define (gen-x86-error-call)
  (lambda (cgc opnd)
    (error "Internal error, do *NOT* directly use x86-call function.")))

;; Generate a call to a label with a return address aligned to 4
;; (end with 00 which is the integer tag)
(define (gen-x86-aligned-call call-fn)
  (lambda (cgc label)

    (define align-mult 4) ;; tag 00 (int tag)
    (define call-size 5)  ;; Call to a label is a *CALL rel32* which is a 5 bytes instruction on x86_64
    (define opnop #x90)   ;; NOP opcode

    (define nop-needed 0) ;; Number of NOP needed to align return address

    (asm-at-assembly

     cgc

     (lambda (cb self)
       (let ((ex (modulo (+ self call-size) align-mult)))
         (if (> ex 0)
             (set! nop-needed (- align-mult ex))
             (set! nop-needed 0))
         nop-needed))

     (lambda (cb self)
       (let loop ((i nop-needed))
         (if (> i 0)
           (begin (asm-8 cb opnop)
                  (loop (- i 1)))))))

    (call-fn cgc label)))

;; Redefine calls
(set! x86-pcall x86-call)
(let ((gambit-call x86-call))
  (set! x86-call (gen-x86-error-call)))
  ;(set! x86-call-label-unaligned-ret gambit-call)
  ;(set! x86-call-label-aligned-ret (gen-x86-aligned-call gambit-call)))

;;-----------------------------------------------------------------------------
;; x86 Registers

;; x86 registers map associate virtual register to x86-register
(define codegen-regmap
  (foldr (lambda (el r)
           (cons (cons (cons 'r el)
                       (list-ref regalloc-regs el))
                 r))
         '()
         (build-list (length regalloc-regs) (lambda (l) l))))

(define alloc-ptr  (x86-r9))
(define global-ptr (x86-r8))
(define selector-reg (x86-rcx))
(define selector-reg-32 (x86-ecx))
(define (x86-usp) (x86-rbp)) ;; user stack pointer is rbp

(define (x86-r64->r32 r64)
  (assert (x86-reg64? r64) "Internal error")
  (cond ((eq? r64 (x86-rax)) (x86-eax))
        ((eq? r64 (x86-rbx)) (x86-ebx))
        ((eq? r64 (x86-rcx)) (x86-ecx))
        ((eq? r64 (x86-rdx)) (x86-edx))
        ((eq? r64 (x86-rsi)) (x86-esi))
        ((eq? r64 (x86-rdi)) (x86-edi))
        ((eq? r64 (x86-rbp)) (x86-ebp))
        ((eq? r64 (x86-rsp)) (x86-esp))
        ((eq? r64 (x86-r8 )) (x86-r8d))
        ((eq? r64 (x86-r9 )) (x86-r9d))
        ((eq? r64 (x86-r10)) (x86-r10d))
        ((eq? r64 (x86-r11)) (x86-r11d))
        ((eq? r64 (x86-r12)) (x86-r12d))
        ((eq? r64 (x86-r13)) (x86-r13d))
        ((eq? r64 (x86-r14)) (x86-r14d))
        ((eq? r64 (x86-r15)) (x86-r15d))))

;; NOTE: temporary register is always rax
;; NOTE: selector is always rcx

;; Offsets
(define OFFSET_PAIR_CAR 16)
(define OFFSET_PAIR_CDR  8)
(define OFFSET_PROC_EP 8)
(define OFFSET_BOX 8)
(define OFFSET_FLONUM 8)
(define (OFFSET_PROC_FREE i) (+ 16 (* 8 i)))

;;-----------------------------------------------------------------------------
;; x86 Codegen utils

(define-macro (neq? l r)
  `(not (eq? ,l ,r)))

(define (int32? n)
  (and (integer? n)
       (>= n (expt -2 31))
       (<  n (expt 2 31))))

(define (codegen-void cgc reg)
  (let ((opnd (codegen-reg-to-x86reg reg)))
    (x86-mov cgc opnd (x86-imm-int ENCODING_VOID))))

(define (codegen-set-bool cgc b reg)
  (let ((dest (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (obj-encoding b)))))

(define (codegen-load-loc cgc fs loc)
  (let ((opnd (codegen-loc-to-x86opnd fs loc)))
    (x86-mov cgc (x86-rax) opnd)))

(define (pick-reg used-regs)
  (define (pick-reg-h regs used)
    (if (null? regs)
        (error "Internal error")
        (if (not (member (car regs) used))
            (car regs)
            (pick-reg-h (cdr regs) used))))
  (pick-reg-h regalloc-regs used-regs))


(define (codegen-loc-to-x86opnd fs loc)
  (cond ((eq? loc 'selector)
         selector-reg)
        ((ctx-loc-is-register? loc)
         (codegen-reg-to-x86reg loc))
        ((ctx-loc-is-memory? loc)
         (codegen-mem-to-x86mem fs loc))
        (else (error "Internal error"))))

(define (codegen-mem-to-x86mem fs mem)
  (x86-mem (* 8 (- fs (cdr mem) 1)) (x86-usp)))

(define (codegen-reg-to-x86reg reg)
  (cdr (assoc reg codegen-regmap)))

(define (codegen-is-imm-64? imm)
  (or (< imm -2147483648)
      (> imm 2147483647)))

;;-----------------------------------------------------------------------------
;; TODO

;; !! USE FS VARIABLE FROM CODEGEN FUNCTIONS
(define-macro (begin-with-cg-macro . exprs)
  ;;
  `(let ()
    (define ##registers-saved## '())
    ,@exprs
    (restore-saved)))

;; Move a value from src memory to dst register
;; update operand
(define-macro (unmem! dst src)
  `(begin (x86-mov cgc ,dst ,src)
          (set! ,(car src) ,(car dst))))

;;
(define-macro (chk-unmem! dst src)
  `(if (x86-mem? ,src)
       (unmem! ,dst ,src)))

;; Find an unused register, save it, unmem from src to this register
;; update saved set
(define-macro (pick-unmem! src used-regs)
  (let ((sym (gensym)))
    `(let ((,sym (pick-reg ,used-regs)))
       (x86-upush cgc ,sym)
       (set! fs (+ fs 1))
       (set! ##registers-saved## (cons ,sym ##registers-saved##))
       (unmem! ((lambda () ,sym)) ,src))))

;; Check if src is in memory. If so, pick-unmem (unmem in an available reg)
(define-macro (chk-pick-unmem! src used-regs)
  `(begin
    (if (x86-mem? ,src)
        (pick-unmem! ,src (append ,used-regs ##registers-saved##)))))
;;
(define-macro (restore-saved)
  `(for-each (lambda (el) (x86-upop cgc el)) ##registers-saved##))

;;-----------------------------------------------------------------------------
;; Define
;;-----------------------------------------------------------------------------

(define (codegen-define-bind cgc fs pos reg lvalue cst?)

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (if cst?
                   (x86-imm-int (obj-encoding lvalue))
                   (codegen-loc-to-x86opnd fs lvalue))))
    (cond ((x86-reg? opval)
             (x86-mov cgc (x86-mem (* 8 pos) global-ptr) opval))
          (else (x86-mov cgc (x86-rax) opval)
                (x86-mov cgc (x86-mem (* 8 pos) global-ptr) (x86-rax))))
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; If
;;-----------------------------------------------------------------------------

(define (codegen-if cgc fs label-jump label-false label-true lcond)
  (let ((opcond (codegen-loc-to-x86opnd fs lcond)))

    (x86-cmp cgc opcond (x86-imm-int (obj-encoding #f)))
    (x86-label cgc label-jump)
    (x86-je  cgc label-false)
    (x86-jmp cgc label-true)))

(define (codegen-inlined-if cgc label-jump label-false label-true x86-op)
  (x86-label cgc label-jump)
  (x86-op cgc label-false)
  (x86-jmp cgc label-true))

;;-----------------------------------------------------------------------------
;; Variables
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; get
(define (codegen-get-global cgc pos reg)
  (let ((dest  (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-mem (* 8 pos) global-ptr))))

;; get free
;; Get free variable directly from closure.
(define (codegen-get-free cgc fs reg lclo lvar)

  (let ((copnd (codegen-loc-to-x86opnd fs lclo))
        (coffset (- (* 8 (+ (cdr lvar) 2)) TAG_MEMOBJ))
        (dest (codegen-reg-to-x86reg reg)))

  (if (x86-reg? copnd)
      (x86-mov cgc dest (x86-mem coffset copnd))
      (begin
        (x86-mov cgc (x86-rax) copnd) ;; Get closure
        (x86-mov cgc dest (x86-mem coffset (x86-rax)))))))

;;-----------------------------------------------------------------------------
;; set
(define (codegen-set-global cgc fs reg pos lval cst?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (if cst?
                   (x86-imm-int (obj-encoding lval))
                   (codegen-loc-to-x86opnd fs lval))))

    (if (or (x86-imm? opval)
            (x86-mem? opval))
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (x86-mov cgc (x86-mem (* 8 pos) global-ptr) opval)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

(define (codegen-set-non-global cgc reg lval fs)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))

    (if (x86-mem? opval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) opval)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; Values
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Literal
(define (codegen-literal cgc lit reg)
  (let ((dest (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (obj-encoding lit)))))

;;-----------------------------------------------------------------------------
;; Flonum
(define (codegen-flonum cgc immediate reg)

  (let ((dest (codegen-reg-to-x86reg reg)))

    (gen-allocation-imm cgc STAG_FLONUM 8)

    ;; Write number
    (x86-mov cgc (x86-rax) (x86-imm-int immediate))
    (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rax))

    ;; Put flonum
    (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))))

;;-----------------------------------------------------------------------------
;; Symbol
(define (codegen-symbol cgc sym reg)
  (let ((qword (obj-encoding sym))
        (dest  (codegen-reg-to-x86reg reg)))
    ;; Check symbol is a PERM gambit object
    (assert (= (bitwise-and (get-i64 (- qword TAG_MEMOBJ)) 7) 6) "Internal error")
    (x86-mov cgc dest (x86-imm-int qword))))

;;-----------------------------------------------------------------------------
;; String
(define (codegen-string cgc str reg)

  (let ((dest (codegen-reg-to-x86reg reg)))

    (x86-mov cgc (x86-rax) (x86-imm-int (* (string-length str) 4)))
    (gen-allocation-rt cgc STAG_STRING (x86-rax))

    ;; Write chars
    (write-chars cgc str 0 (- 8 TAG_MEMOBJ))

    ;; Put string
    (x86-mov cgc dest (x86-rax))))

(define (write-chars cgc str idx offset)
  (if (< idx (string-length str))
      (let* ((int (char->integer (string-ref str idx))))
        (x86-mov cgc (x86-mem offset (x86-rax)) (x86-imm-int int) 32)
        (write-chars cgc str (+ idx 1) (+ offset 4)))))

;;-----------------------------------------------------------------------------
;; Functions
;;-----------------------------------------------------------------------------

;; Generate a generic function prologue without rest param
(define (codegen-prologue-gen-nrest cgc fn-nb-args)
  (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding fn-nb-args)))
  (x86-jne cgc label-err-wrong-num-args))

;; Generate a generic function prologue with rest param
(define (codegen-prologue-gen-rest cgc fs fn-nb-args)

  (let ((nb-args-regs       (length args-regs))
        (label-rest         (asm-make-label #f (new-sym 'prologue-rest)))
        (label-rest-loop    (asm-make-label #f (new-sym 'prologue-rest-loop)))
        (label-rest-end     (asm-make-label #f (new-sym 'prologue-rest-end)))
        (label-next-arg     (asm-make-label #f (new-sym 'prologue-next-arg)))
        (label-from-stack   (asm-make-label #f (new-sym 'prologue-from-stack)))
        (label-next-arg-end (asm-make-label #f (new-sym 'prologue-next-arg-end))))

    (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (- fn-nb-args 1))))
    (x86-jge cgc label-rest)
      (gen-error cgc ERR_WRONG_NUM_ARGS)

    ;; Get next arg
    (x86-label cgc label-next-arg)
      (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (length args-regs))))
      (x86-jg cgc label-from-stack)
      (let loop ((i (length args-regs))
                 (regs (reverse args-regs)))
        (if (> i 0)
            (begin
                (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding i)))
                (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd fs (car regs)))
                (x86-je cgc label-next-arg-end)
                (loop (- i 1) (cdr regs)))))
      (x86-jmp cgc label-next-arg-end)
      (x86-label cgc label-from-stack)
      (x86-upop cgc (x86-rax))
      (x86-label cgc label-next-arg-end)
      (x86-sub cgc (x86-rdi) (x86-imm-int (obj-encoding 1)))
      (x86-ret cgc)
    ;; END get next arg

    (x86-label cgc label-rest)
    ;; cdr (rax) = '()
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding '())))
    (x86-label cgc label-rest-loop)
    (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (- fn-nb-args 1))))
    (x86-je cgc label-rest-end)

      ;; Alloc
      (gen-allocation-imm cgc STAG_PAIR 16)
      (x86-pcall cgc label-next-arg)
      (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax))
      (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) selector-reg)
      (x86-lea cgc selector-reg (x86-mem (+ -24 TAG_PAIR) alloc-ptr))
      (x86-jmp cgc label-rest-loop)
    ;;
    (x86-label cgc label-rest-end)
    (if (<= fn-nb-args (length args-regs))
        (let ((reg (list-ref args-regs (- fn-nb-args 1))))
          (x86-mov cgc (codegen-reg-to-x86reg reg) selector-reg))
        (x86-upush cgc selector-reg))
    ;; Reset selector used as temporary
    (x86-mov cgc selector-reg (x86-imm-int 0))))

;; Generate specialized function prologue with rest param and actual == formal
(define (codegen-prologue-rest= cgc destreg)
  (let ((dest
          (and destreg (codegen-reg-to-x86reg destreg))))
    (if dest
        (x86-mov cgc dest (x86-imm-int (obj-encoding '())))
        (x86-upush cgc (x86-imm-int (obj-encoding '()))))))

;; Generate specialized function prologue with rest param and actual > formal
(define (codegen-prologue-rest> cgc fs nb-rest-stack rest-regs destreg)

  (let ((regs
          (map (lambda (el) (codegen-loc-to-x86opnd fs el))
               rest-regs))
        (dest
          (and destreg (codegen-loc-to-x86opnd fs destreg)))
        (label-loop-end (asm-make-label #f (new-sym 'prologue-loop-end)))
        (label-loop     (asm-make-label #f (new-sym 'prologue-loop))))

    ;; TODO: Only one alloc
    ;; r11 is available because it's the ctx reg
    (x86-mov cgc (x86-r11) (x86-imm-int (obj-encoding '())))
    ;; Stack
    (x86-mov cgc (x86-rdx) (x86-imm-int (obj-encoding nb-rest-stack)))
    (x86-label cgc label-loop)
    (x86-cmp cgc (x86-rdx) (x86-imm-int 0))
    (x86-je cgc label-loop-end)
    (gen-allocation-imm cgc STAG_PAIR 16)
    (x86-upop cgc (x86-rax))
    (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax))
    (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-r11))
    (x86-lea cgc (x86-r11) (x86-mem (+ -24 TAG_PAIR) alloc-ptr))
    (x86-sub cgc (x86-rdx) (x86-imm-int (obj-encoding 1)))
    (x86-jmp cgc label-loop)
    (x86-label cgc label-loop-end)
    ;; Regs
    (for-each
      (lambda (src)
        (gen-allocation-imm cgc STAG_PAIR 16)
        (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) src)
        (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-r11))
        (x86-lea cgc (x86-r11) (x86-mem (+ -24 TAG_PAIR) alloc-ptr)))
      regs)
    ;; Dest
    (if dest
        (x86-mov cgc dest (x86-r11))
        (x86-upush cgc (x86-r11)))))

;; Alloc closure and write header
(define (codegen-closure-create cgc nb-free)
  (let* ((closure-size  (+ 1 nb-free))) ;; entry point & free vars
    ;; Alloc closure
    (gen-allocation-imm cgc STAG_PROCEDURE (* 8 closure-size))))

;; Write entry point in closure (do not use cctable)
(define (codegen-closure-ep cgc entryvec-loc nb-free)
  (let ((offset (+ OFFSET_PROC_EP (* -8 (+ nb-free 2)))))
    (x86-mov cgc (x86-rax) (x86-mem (+ 8 entryvec-loc)))
    (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))))

;; Write cctable ptr in closure (use multiple entry points)
(define (codegen-closure-cc cgc cctable-loc nb-free)
  (let ((offset (+ OFFSET_PROC_EP (* -8 (+ nb-free 2)))))
    (x86-mov cgc (x86-rax) (x86-imm-int cctable-loc))
    (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))))

;; Load closure in tmp register
(define (codegen-load-closure cgc fs loc)
  (let ((opnd (codegen-loc-to-x86opnd fs loc)))
    (x86-mov cgc (x86-rax) opnd)))

;;; Push closure
(define (codegen-closure-put cgc reg nb-free)
  (let ((dest   (codegen-reg-to-x86reg reg))
        (offset (+ (* -8 (+ nb-free 2)) TAG_MEMOBJ)))
    (x86-lea cgc dest (x86-mem offset alloc-ptr))))

;;
(define (codegen-return-common cgc fs clean-nb lretobj lretval cst-val?)
  (let ((opretobj (codegen-loc-to-x86opnd fs lretobj))
        (opretval (if cst-val?
                      (x86-imm-int (obj-encoding lretval))
                      (codegen-loc-to-x86opnd fs lretval)))
        (opret    (codegen-reg-to-x86reg return-reg)))
    ;; Move return value to return register
    (if (not (eq? opret opretval))
        (x86-mov cgc opret opretval))
    ;; Move return address (or cctable address) in rdx
    (if (not (eq? opretobj (x86-rdx)))
        (x86-mov cgc (x86-rdx) opretobj))
    ;; Clean stack
    (if (> clean-nb 0)
        (x86-add cgc (x86-usp) (x86-imm-int (* 8 clean-nb 1))))))

;; Generate function return using a return address
;; Retaddr (or cctable) is in rdx
(define (codegen-return-rp cgc fs clean-nb lretobj lretval cst-val?)
    (codegen-return-common cgc fs clean-nb lretobj lretval cst-val?)
    (x86-jmp cgc (x86-rdx)))

;; Generate function return using a crtable
;; Retaddr (or cctable) is in rdx
(define (codegen-return-cr cgc fs clean-nb lretobj lretval cst-val? crtable-offset)
    (codegen-return-common cgc fs clean-nb lretobj lretval cst-val?)
    (x86-mov cgc (x86-rax) (x86-mem crtable-offset (x86-rdx)))
    (x86-mov cgc (x86-r11) (x86-imm-int (obj-encoding crtable-offset)))
    (x86-jmp cgc (x86-rax)))

;; Generate function call using a single entry point
;; eploc is the cctable or entry points if it's known
(define (codegen-call-ep cgc nb-args eploc direct-eploc)
  ;; TODO: use call/ret if opt-entry-points opt-return-points are #f
  (if nb-args ;; If nb-args given, move encoded in rdi, else nb-args is already encoded in rdi (apply)
      (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args))))

  (cond (direct-eploc
          ;; If it's a direct call to a not yet generated entry point, add stub_load label
          (if (eq? (car direct-eploc) 'stub)
              (let ((load-label (caddr direct-eploc)))
                (x86-label cgc load-label)))
          (let ((label (asm-make-label #f (new-sym 'known_dest_) (cadr direct-eploc))))
            (x86-jmp cgc label)))
        (eploc
          (error "NYI codegen-call-ep"))
          ;(x86-jmp cgc (x86-mem (+ eploc 8) #f)))
        (else
          (x86-mov cgc (x86-rdx) (x86-mem (- 8 TAG_MEMOBJ) (x86-rsi)))
          (x86-jmp cgc (x86-rdx)))))

;; Generate function call using a cctable and generic entry point
;; eploc is the cctable or entry points if it's known
(define (codegen-call-cc-gen cgc nb-args eploc)
  (if nb-args
      (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args))))

  (if eploc
      (x86-mov cgc (x86-rdx) (x86-imm-int eploc))
      (x86-mov cgc (x86-rdx) (x86-mem (- 8 TAG_MEMOBJ) (x86-rsi)))) ;; Get table

  (x86-jmp cgc (x86-mem 8 (x86-rdx)))) ;; Jump to generic entry point

;; Generate function call using a cctable and specialized entry point
;; eploc is the cctable or entry points if it's known
(define (codegen-call-cc-spe cgc idx nb-args eploc direct-eploc)

    ;; Closure is in rax
    (let ((cct-offset (* 8 (+ 2 idx))))
      ;; 1 - Put ctx in r11
      (if (or (not direct-eploc)                  ;; ctx needed if it's not a direct call
              (not (eq? (car direct-eploc) 'ep))) ;; ctx needed if it's a direct call to a stub
          (x86-mov cgc (x86-r11) (x86-imm-int (obj-encoding idx))))
      ;; 2 - Put nbargs in rdi if needed
      (if opt-max-versions
           (x86-mov cgc (x86-rdi) (x86-imm-int (* 4 nb-args))))
      ;; 3- Get cc-table
      (cond (direct-eploc
              ;; If it's a direct call to a not yet generated entry point, add stub_load label
              (if (eq? (car direct-eploc) 'stub)
                  (let ((load-label (caddr direct-eploc)))
                    (x86-label cgc load-label)))
              (let ((label (asm-make-label #f (new-sym 'known_dest_) (cadr direct-eploc))))
                (x86-jmp cgc label)))
            (eploc
              (x86-mov cgc (x86-rdx) (x86-imm-int eploc))
              (x86-jmp cgc (x86-mem cct-offset (x86-rdx))))
            (else
              (x86-mov cgc (x86-rdx) (x86-mem (- 8 TAG_MEMOBJ) (x86-rsi)))
              (x86-jmp cgc (x86-mem cct-offset (x86-rdx)))))))

;; Load continuation using specialized return points
(define (codegen-load-cont-cr cgc crtable-loc)
  (x86-mov cgc (x86-rax) (x86-imm-int crtable-loc))
  (assert (= (modulo crtable-loc 4) 0) "Internal error")
  (x86-mov cgc (x86-mem 0 (x86-usp)) (x86-rax)))

(define (codegen-load-cont-rp cgc label-load-ret label-cont-stub)
  (x86-label cgc label-load-ret)
  (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref label-cont-stub 1)))
  (assert (= (modulo (vector-ref label-cont-stub 1) 4) 0) "Internal error")
  (x86-mov cgc (x86-mem 0 (x86-usp)) (x86-rax)))

;;-----------------------------------------------------------------------------
;; Operators
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; N-ary arithmetic operators

;; Gen code for arithmetic operation on int/int
(define (codegen-num-ii cgc fs op reg lleft lright lcst? rcst? overflow?)

  (assert (not (and lcst? rcst?)) "Internal codegen error")

  (let ((labels-overflow (add-callback #f 0 (lambda (ret-addr selector)
                                              (error ERR_ARR_OVERFLOW))))
        (dest    (codegen-reg-to-x86reg reg))
        (opleft  (and (not lcst?) (codegen-loc-to-x86opnd fs lleft)))
        (opright (and (not rcst?) (codegen-loc-to-x86opnd fs lright))))

   ;; Handle cases like 1. 2. etc...
   (if (and lcst? (flonum? lleft))
       (set! lleft (##flonum->fixnum lleft)))
   (if (and rcst? (flonum? lright))
       (set! lright (##flonum->fixnum lright)))

   (cond
     (lcst?
       (cond ((eq? op '+) (if (not (eq? dest opright))
                              (x86-mov cgc dest opright))
                          (x86-add cgc dest (x86-imm-int (obj-encoding lleft))))
             ((eq? op '-) (if (eq? dest opright)
                              (begin (x86-mov cgc (x86-rax) dest)
                                     (set! opright (x86-rax))))
                          (x86-mov cgc dest (x86-imm-int (obj-encoding lleft)))
                          (x86-sub cgc dest opright))
             ((eq? op '*) (x86-imul cgc dest opright (x86-imm-int lleft)))))
     (rcst?
       (cond ((eq? op '+) (if (not (eq? dest opleft))
                              (x86-mov cgc dest opleft))
                          (x86-add cgc dest (x86-imm-int (obj-encoding lright))))
             ((eq? op '-) (if (not (eq? dest opleft))
                              (x86-mov cgc dest opleft))
                          (x86-sub cgc dest (x86-imm-int (obj-encoding lright))))
             ((eq? op '*) (x86-imul cgc dest opleft (x86-imm-int lright)))))
     (else
       (cond ((eq? op '+)
                (cond ((eq? dest opleft)
                         (x86-add cgc dest opright))
                      ((eq? dest opright)
                         (x86-add cgc dest opleft))
                      (else
                         (x86-mov cgc dest opleft)
                         (x86-add cgc dest opright))))
             ((eq? op '-)
                (cond ((eq? dest opleft)
                         (x86-sub cgc dest opright))
                      ((eq? dest opright)
                         (x86-mov cgc (x86-rax) opright)
                         (x86-mov cgc dest opleft)
                         (x86-sub cgc dest (x86-rax)))
                      (else
                         (x86-mov cgc dest opleft)
                         (x86-sub cgc dest opright))))
             ((eq? op '*)
                (cond ((eq? dest opleft)
                         (x86-sar cgc dest (x86-imm-int 2))
                         (x86-imul cgc dest opright))
                      ((eq? dest opright)
                         (x86-sar cgc dest (x86-imm-int 2))
                         (x86-imul cgc dest opleft))
                      (else
                         (x86-mov cgc dest opleft)
                         (x86-sar cgc dest (x86-imm-int 2))
                         (x86-imul cgc dest opright)))))))

   (if overflow?
       (x86-jo cgc (list-ref labels-overflow 0)))))

;; Gen code for arithmetic operation on float/float (also handles int/float and float/int)
(define (codegen-num-ff cgc fs op reg lleft leftint? lright rightint? lcst? rcst? overflow?)

  (assert (not (and lcst? rcst?)) "Internal codegen error")

  (let ((dest    (codegen-reg-to-x86reg reg))
        (opleft  (and (not lcst?) (codegen-loc-to-x86opnd fs lleft)))
        (opright (and (not rcst?) (codegen-loc-to-x86opnd fs lright))))

    ;; Handle cases like 1. 2. etc...
    (if (and lcst? (flonum? lleft))
        (set! lleft (##flonum->fixnum lleft)))
    (if (and rcst? (flonum? lright))
        (set! lright (##flonum->fixnum lright)))

    ;; Alloc result flonum
    (gen-allocation-imm cgc STAG_FLONUM 8)

    (let ((x86-op (cdr (assoc op `((+ . ,x86-addsd) (- . ,x86-subsd) (* . ,x86-mulsd) (/ . ,x86-divsd))))))

    ;; Right operand
     (if rightint?
        ;; Right is register or mem or cst and integer
         (begin
           (if rcst?
               (x86-mov cgc (x86-rax) (x86-imm-int lright))
               (begin (x86-mov cgc (x86-rax) opright)
                      (x86-sar cgc (x86-rax) (x86-imm-int 2)))) ;; untag integer
           (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax))) ;; convert to double
         (if (ctx-loc-is-register? lright)
            ;; Right is register and not integer, then get float value in xmm0
             (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) opright))
            ;; Right is memory and not integer, then get float value in xmm0
             (begin (x86-mov cgc (x86-rax) opright)
                    (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))))
     (set! opright (x86-xmm0))

    ;; Left operand
     (if leftint?
        ;; Left is register or mem or cst and integer
         (begin
           (if lcst?
               (x86-mov cgc (x86-rax) (x86-imm-int lleft))
               (begin (x86-mov cgc (x86-rax) opleft)
                      (x86-sar cgc (x86-rax) (x86-imm-int 2)))) ;; untag integer
           (x86-cvtsi2sd cgc (x86-xmm1) (x86-rax))) ;; convert to double
         (if (ctx-loc-is-register? lleft)
            ;; Left is register and not integer, then get float value in xmm1
             (x86-movsd cgc (x86-xmm1) (x86-mem (- 8 TAG_MEMOBJ) opleft))
            ;; Left is memory and not integer, then get float value in xmm1
             (begin (x86-mov cgc (x86-rax) opleft)
                    (x86-movsd cgc (x86-xmm1) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))))
     (set! opleft (x86-xmm1))

    ;; Operator, result in opleft
    (x86-op cgc opleft opright)

    ;; Write number
    (x86-movsd cgc (x86-mem -8 alloc-ptr) opleft)

    ;; Put
    (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr)))))

;;-----------------------------------------------------------------------------
;; N-ary comparison operators

(define (codegen-cmp-ii cgc fs op reg lleft lright lcst? rcst? inline-if-cond?)

  (define-macro (if-inline expr)
    `(if inline-if-cond? #f ,expr))

  (assert (not (and lcst? rcst?)) "Internal codegen error")

  (let* ((x86-op  (cdr (assoc op `((< . ,x86-jl) (> . ,x86-jg) (<= . ,x86-jle) (>= . ,x86-jge) (= . ,x86-je)))))
         (x86-iop (cdr (assoc op `((< . ,x86-jg) (> . ,x86-jl) (<= . ,x86-jge) (>= . ,x86-jle) (= . ,x86-je)))))
         (x86-inline-op  (cdr (assoc op `((< . ,x86-jge) (> . ,x86-jle) (<= . ,x86-jg) (>= . ,x86-jl) (= . ,x86-jne)))))
         (x86-inline-iop (cdr (assoc op `((< . ,x86-jle) (> . ,x86-jge) (<= . ,x86-jl) (>= . ,x86-jg) (= . ,x86-jne)))))
         (dest      (if-inline (codegen-reg-to-x86reg reg)))
         (label-end (if-inline (asm-make-label #f (new-sym 'label-end))))
         (opl (and (not lcst?) (codegen-loc-to-x86opnd fs lleft)))
         (opr (and (not rcst?) (codegen-loc-to-x86opnd fs lright)))
         (selop x86-op)
         (selinop x86-inline-op))

    ;; Handle cases like 1. 2. etc...
    (if (and lcst? (flonum? lleft))
        (set! lleft (##flonum->fixnum lleft)))
    (if (and rcst? (flonum? lright))
        (set! lright (##flonum->fixnum lright)))

    (if lcst?
        (if (not (int32? (obj-encoding lleft)))
            (begin
              (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lleft)))
              (set! opl (x86-rax)))
            (begin
              (set! opl opr)
              (set! opr (x86-imm-int (obj-encoding lleft)))
              (set! selop x86-iop)
              (set! selinop x86-inline-iop))))

    (if rcst?
        (if (not (int32? (obj-encoding lright)))
            (begin
              (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lright)))
              (set! opr (x86-rax)))
            (set! opr (x86-imm-int (obj-encoding lright)))))

    (if (and (x86-mem? opl)
             (x86-mem? opr))
        (begin
          (x86-mov cgc (x86-rax) opl)
          (x86-cmp cgc (x86-rax) opr 64))
        (x86-cmp cgc opl opr 64))

    (if inline-if-cond?
        selinop ;; Return x86-op
        (begin (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
               (selop cgc label-end)
               (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
               (x86-label cgc label-end)))))

(define (codegen-cmp-ff cgc fs op reg lleft leftint? lright rightint? lcst? rcst? inline-if-cond?)

  (define-macro (if-inline expr)
    `(if inline-if-cond? #f ,expr))

  (assert (not (and lcst? rcst?)) "Internal codegen error")

  (if (and lcst? (flonum? lleft))
      (set! lleft (##flonum->fixnum lleft)))
  (if (and rcst? (flonum? lright))
      (set! lright (##flonum->fixnum lright)))

  (let ((dest (if-inline (codegen-reg-to-x86reg reg)))
        (label-end (if-inline (asm-make-label #f (new-sym 'label-end))))
        (opleft  (if lcst? lleft  (codegen-loc-to-x86opnd fs lleft)))
        (opright (if rcst? lright (codegen-loc-to-x86opnd fs lright)))
        (x86-op (cdr (assoc op `((< . ,x86-jae) (> . ,x86-jbe) (<= . ,x86-ja) (>= . ,x86-jb) (= . ,x86-jne))))))

    ;; Left operand
    (cond
      (lcst?
         (x86-mov cgc (x86-rax) (x86-imm-int opleft))
         (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax)))
      (leftint?
         (x86-mov cgc (x86-rax) opleft)
         (x86-sar cgc (x86-rax) (x86-imm-int 2))
         (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax)))
      ((ctx-loc-is-memory? lleft)
       (x86-mov cgc (x86-rax) opleft)
       (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
      (else
         (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) opleft))))

    ;; Right operand
    (cond
      (rcst?
        (x86-mov cgc (x86-rax) (x86-imm-int opright))
        (x86-cvtsi2sd cgc (x86-xmm1) (x86-rax))
        (x86-comisd cgc (x86-xmm0) (x86-xmm1)))
      (rightint?
        (x86-mov cgc (x86-rax) opright)
        (x86-sar cgc (x86-rax) (x86-imm-int 2))
        (x86-cvtsi2sd cgc (x86-xmm1) (x86-rax))
        (x86-comisd cgc (x86-xmm0) (x86-xmm1)))
      ((ctx-loc-is-memory? lright)
       (x86-mov cgc (x86-rax) opright)
       (x86-comisd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
      (else
        (x86-comisd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) opright))))

    ;; NOTE: check that mlc-if patch is able to patch ieee jcc instructions (ja, jb, etc...)
    (if inline-if-cond?
        x86-op ;; return x86 op
        (begin (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
               (x86-op cgc label-end)
               (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
               (x86-label cgc label-end)))))

;;-----------------------------------------------------------------------------
;; PRIMITIVES
;;-----------------------------------------------------------------------------

;;
;; mem-allocated?
(define (codegen-p-mem-allocated? cgc fs op reg inlined-cond? lval)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval))
        (label-next (asm-make-label #f (new-sym 'next_))))
    (x86-mov cgc (x86-rax) opval)
    (x86-and cgc (x86-rax) (x86-imm-int 1))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (x86-je cgc label-next)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-label cgc label-next)))

;;
;; subtyped?
(define (codegen-p-subtyped? cgc fs op reg inlined-cond? lval)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval))
        (label-end (asm-make-label #f (new-sym 'subtyped_end_))))
    (x86-mov cgc (x86-rax) opval)
    (x86-and cgc (x86-rax) (x86-imm-int 3))
    (x86-cmp cgc (x86-rax) (x86-imm-int 1))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-je  cgc label-end)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (x86-label cgc label-end)))

;;
;; box
(define (codegen-p-box cgc fs op reg inlined-cond? lval cst?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (if cst?
                   (x86-imm-int (obj-encoding lval))
                   (codegen-loc-to-x86opnd fs lval))))
    (gen-allocation-imm cgc STAG_MOBJECT 8)
    (if (or (x86-mem? opval)
            (x86-imm? opval))
        (begin
          (x86-mov cgc (x86-rax) opval)
          (set! opval (x86-rax))))
    (x86-mov cgc (x86-mem (+ -16 OFFSET_BOX) alloc-ptr) opval)
    (x86-lea cgc dest (x86-mem (+ -16 TAG_MEMOBJ) alloc-ptr))))

;;
;; unbox
(define (codegen-p-unbox cgc fs op reg inlined-cond? lbox cst?)
  (assert (not cst?) "Internal error, box can't be cst")
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opbox (codegen-loc-to-x86opnd fs lbox)))
    (if (x86-mem? opbox)
        (begin (x86-mov cgc (x86-rax) opbox)
               (set! opbox (x86-rax))))
    (x86-mov cgc dest (x86-mem (- OFFSET_BOX TAG_MEMOBJ) opbox))))

;;
;; box-set!
(define (codegen-p-set-box cgc fs op reg inlined-cond? lbox lval cst-box? cst-val?)
  (assert (not cst-box?) "Internal error, unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opbox (codegen-loc-to-x86opnd fs lbox))
        (opval (if cst-val?
                   (x86-imm-int (obj-encoding lval))
                   (codegen-loc-to-x86opnd fs lval)))
        (use-selector? #f))

    (if (x86-mem? opbox)
        (begin (x86-mov cgc (x86-rax) opbox)
               (set! opbox (x86-rax))))

    (if (or (x86-imm? opval)
            (x86-mem? opval))
        (if (eq? opbox (x86-rax))
            (begin (x86-mov cgc selector-reg opval)
                   (set! opval selector-reg)
                   (set! use-selector? #t))
            (begin (x86-mov cgc (x86-rax) opval)
                   (set! opval (x86-rax)))))

    (x86-mov cgc (x86-mem (- OFFSET_BOX TAG_MEMOBJ) opbox) opval)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))

    (if use-selector?
        (x86-mov cgc selector-reg (x86-imm-int 0)))))

;;
;; cons
(define (codegen-p-cons cgc fs op reg inlined-cond? lcar lcdr car-cst? cdr-cst?)

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opcar (and (not car-cst?) (codegen-loc-to-x86opnd fs lcar)))
        (opcdr (and (not cdr-cst?) (codegen-loc-to-x86opnd fs lcdr))))
    (gen-allocation-imm cgc STAG_PAIR 16)
    (cond (car-cst?
            (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lcar)))
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax)))
          ((x86-mem? opcar)
            (x86-mov cgc (x86-rax) opcar)
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax)))
          (else
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) opcar)))
    (cond (cdr-cst?
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-imm-int (obj-encoding lcdr)) 64))
          ((x86-mem? opcdr)
            (x86-mov cgc (x86-rax) opcdr)
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-rax)))
          (else
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) opcdr)))

    (x86-lea cgc dest (x86-mem (+ -24 TAG_PAIR) alloc-ptr))))

;;
;; quotient/modulo/remainder
(define (codegen-p-binop cgc fs op label-div0 reg lleft lright lcst? rcst?)

  (assert (not (and lcst? rcst?)) "Internal error")

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (lopnd (if lcst?
                    (x86-imm-int (obj-encoding lleft))
                    (codegen-loc-to-x86opnd fs lleft)))
         (ropnd (if rcst?
                    (x86-imm-int (obj-encoding lright))
                    (codegen-loc-to-x86opnd fs lright)))
         (save-rdx? (neq? dest (x86-rdx)))
         (restore-fn #f)
         (selector-used #f))

    ;; If ropnd is imm or rdx, use selector
    (if (or (x86-imm? ropnd)
            (eq? ropnd (x86-rdx)))
        (begin (x86-mov cgc selector-reg ropnd)
               (set! ropnd selector-reg)
               (set! selector-used #t)))

    ;; If rdx must be saved
    (if save-rdx?
        (if (eq? ropnd selector-reg)
            ;; If selector used, use pstack
            (begin (x86-ppush cgc (x86-rdx))
                   (set! restore-fn (lambda (cgc) (x86-ppop cgc (x86-rdx)))))
            ;; Else, use selector
            (begin (x86-mov cgc selector-reg (x86-rdx))
                   (set! restore-fn (lambda (cgc) (x86-mov cgc (x86-rdx) selector-reg)))
                   (set! selector-used #t))))

    (x86-cmp cgc ropnd (x86-imm-int 0))
    (x86-je cgc label-div0)

    (x86-mov cgc (x86-rax) lopnd)
    (x86-sar cgc (x86-rax) (x86-imm-int 2))
    (x86-cqo cgc)

    (x86-sar cgc ropnd (x86-imm-int 2))
    (x86-idiv cgc ropnd)

    (cond ((eq? op 'quotient)
            (x86-shl cgc (x86-rax) (x86-imm-int 2))
            (x86-mov cgc dest (x86-rax)))
          ((eq? op 'remainder)
            (x86-shl cgc (x86-rdx) (x86-imm-int 2))
            (x86-mov cgc dest (x86-rdx)))
          ((eq? op 'modulo)
            (x86-mov cgc (x86-rax) (x86-rdx))
            (x86-add cgc (x86-rax) ropnd)
            (x86-cqo cgc)
            (x86-idiv cgc ropnd)
            (x86-shl cgc (x86-rdx) (x86-imm-int 2))
            (x86-mov cgc dest (x86-rdx))))

    ;; Restore ropnd
    (if (and (not (= dest ropnd))
             (not (= selector-reg ropnd)))
        (x86-shl cgc ropnd (x86-imm-int 2)))

    ;; Restore rdx
    (restore-fn cgc)
    ;; Restore selector
    (if selector-used
        (x86-xor cgc selector-reg selector-reg))))

;;
;; not
(define (codegen-p-not cgc fs op reg inlined-cond? lval val-cst?)

  (define dest (codegen-reg-to-x86reg reg))

  (cond
    ((and val-cst? lval)
      (x86-mov cgc dest (x86-imm-int (obj-encoding #f))))
    (val-cst?
      (x86-mov cgc dest (x86-imm-int (obj-encoding #t))))
    (else
      (let ((label-done
              (asm-make-label cgc (new-sym 'done)))
            (dest (codegen-reg-to-x86reg reg))
            (opval (codegen-loc-to-x86opnd fs lval)))

        (if (eq? dest opval)
            (begin
              (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
              (x86-cmp cgc opval (x86-rax))
              (x86-mov cgc dest (x86-imm-int (obj-encoding #f))))
            (begin
              (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
              (x86-cmp cgc opval dest)))

        (x86-jne cgc label-done)
        (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
        (x86-label cgc label-done)))))

;;
;; eq?
(define (codegen-p-eq? cgc fs op reg inlined-cond? lleft lright lcst? rcst?)

  ;; (eq? cst1 cst2) is handled by code expansion
  (assert (not (and lcst? rcst?)) "Internal error (codegen-eq?)")

  (let ((dest (and reg (codegen-reg-to-x86reg reg)))
        (label-done (asm-make-label #f (new-sym 'eq?_end_)))
        (lopnd (and (not lcst?) (codegen-loc-to-x86opnd fs lleft)))
        (ropnd (and (not rcst?) (codegen-loc-to-x86opnd fs lright))))

   (cond (lcst?
          ;; Check for imm64
          (if (codegen-is-imm-64? (obj-encoding lleft))
              (begin (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lleft)))
                     (x86-cmp cgc (x86-rax) ropnd))
              (x86-cmp cgc ropnd (x86-imm-int (obj-encoding lleft)))))
         (rcst?
          (if (codegen-is-imm-64? (obj-encoding lright))
              (begin (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lright)))
                     (x86-cmp cgc (x86-rax) lopnd))
              (x86-cmp cgc lopnd (x86-imm-int (obj-encoding lright)))))
         (else
          (if (and (x86-mem? lopnd)
                   (x86-mem? ropnd))
              (begin (x86-mov cgc (x86-rax) lopnd)
                     (set! lopnd (x86-rax))))
          (x86-cmp cgc lopnd ropnd)))

   (if (not inlined-cond?)
       (begin
         (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
         (x86-je  cgc label-done)
         (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
         (x86-label cgc label-done)))))

;;
;; car/cdr
(define (codegen-p-cxr cgc fs op reg inlined-cond? lval cst?)

  (assert (not cst?) "Internal error")

  (let ((offset
          (if (eq? op 'car)
              (- OFFSET_PAIR_CAR TAG_PAIR)
              (- OFFSET_PAIR_CDR TAG_PAIR)))
        (dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))

    (if (x86-mem? opval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (x86-mov cgc dest (x86-mem offset opval))))

;;
;; symbol->string
(define (codegen-p-symbol->string cgc fs op reg inlined-cond? lsym sym-cst?)

  (assert (not sym-cst?) "Internal error. Unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opsym (codegen-loc-to-x86opnd fs lsym)))

    (if (x86-mem? opsym)
        (begin (x86-mov cgc (x86-rax) opsym)
               (set! opsym (x86-rax))))

    ;; Get string scheme object from symbol representation
    (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) opsym))))

;;
;; set-car!/set-cdr!
(define (codegen-p-set-cxr! cgc fs op reg inlined-cond? lpair lval pair-cst? val-cst?)
  (assert (not pair-cst?) "Internal error, unexpected cst operand")
  (let ((offset
          (if (eq? op 'set-car!)
              (- OFFSET_PAIR_CAR TAG_PAIR)
              (- OFFSET_PAIR_CDR TAG_PAIR)))
        (dest (codegen-reg-to-x86reg reg))
        (oppair (codegen-loc-to-x86opnd fs lpair))
        (opval (and (not val-cst?) (codegen-loc-to-x86opnd fs lval))))

    (if (x86-mem? oppair)
        (begin (x86-mov cgc (x86-rax) oppair)
               (set! oppair (x86-rax))))

    (cond
      (val-cst?
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lval)))
        (x86-mov cgc (x86-mem offset oppair) (x86-rax)))
      ((ctx-loc-is-memory? lval)
       (x86-mov cgc dest opval)
       (x86-mov cgc (x86-mem offset oppair) dest))
      (else
        (x86-mov cgc (x86-mem offset oppair) opval)))

    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;
;; eof-object?
(define (codegen-p-eof-object? cgc fs op reg inlined-cond? lval cst?)

  (define dest (codegen-reg-to-x86reg reg))

  (cond
    ((and cst? (eof-object? lval))
       (x86-mov cgc dest (x86-imm-int (obj-encoding #t))))
    (cst?
       (x86-mov cgc dest (x86-imm-int (obj-encoding #f))))
    (else
       (let ((label-end (asm-make-label #f (new-sym 'label-end)))
             (opval (codegen-loc-to-x86opnd fs lval)))

         ;; ENCODING_EOF is a a imm64 and cmp r/m64, imm32 is not possible
         ;; then use a r64
         (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_EOF))

         (x86-cmp cgc opval (x86-rax))
         (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
         (x86-jne cgc label-end)
         (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
         (x86-label cgc label-end)))))

;;
;; char->integer/integer->char
(define (codegen-p-ch<->int cgc fs op reg inlined-cond? lval #!optional cst?)

  (let ((dest (codegen-reg-to-x86reg reg)))

   (cond
     ((and cst? (eq? op 'integer->char))
      (x86-mov cgc dest (x86-imm-int (obj-encoding (integer->char lval)))))
     ((and cst? (eq? op 'char->integer))
      (x86-mov cgc dest (x86-imm-int (obj-encoding (char->integer lval)))))
     (else
        (let ((opval (codegen-loc-to-x86opnd fs lval)))

          (if (neq? dest opval)
              (x86-mov cgc dest opval))

          (if (eq? op 'char->integer)
              (x86-xor cgc dest (x86-imm-int TAG_SPECIAL))
              (x86-or  cgc dest (x86-imm-int TAG_SPECIAL))))))))

;;
;; make-string
(define (codegen-p-make-string cgc fs op reg inlined-cond? llen lval len-cst? val-cst?)

  (if len-cst?
      (codegen-p-make-string-imm cgc fs reg llen lval val-cst?)
      (codegen-p-make-string-opn cgc fs reg llen lval val-cst?)))

;;
;; make-vector with cst len
(define (codegen-p-make-string-imm cgc fs reg llen lval val-cst?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval))
                    (codegen-loc-to-x86opnd fs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-string-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-string-end))))

    (gen-allocation-imm cgc STAG_STRING (* 4 llen))

    (x86-mov cgc (x86-rax) (x86-imm-int (* -4 llen)))
    (if val-cst?
        (x86-mov cgc selector-reg (x86-imm-int (char->integer lval)))
        (begin
          (x86-mov cgc selector-reg opval)
          (x86-shr cgc selector-reg (x86-imm-int 2))))
    (x86-label cgc label-loop)
    (x86-cmp cgc (x86-rax) (x86-imm-int 0))
    (x86-je cgc label-end)

      (let ((memop (x86-mem 0 alloc-ptr (x86-rax))))
        (x86-mov cgc memop selector-reg-32)
        (x86-add cgc (x86-rax) (x86-imm-int 4))
        (x86-jmp cgc label-loop))

    (x86-label cgc label-end)
    (x86-lea cgc dest (x86-mem (+ (- (* -4 llen) 8) TAG_MEMOBJ) 0 alloc-ptr))
    (x86-mov cgc selector-reg (x86-imm-int 0))))

;;
;; make-vector with !cst len
(define (codegen-p-make-string-opn cgc fs reg llen lval val-cst?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd fs llen))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval))
                    (codegen-loc-to-x86opnd fs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-string-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-string-end))))

    (x86-mov cgc (x86-rax) oplen)
    (gen-allocation-rt cgc STAG_STRING (x86-rax))

    (x86-mov cgc selector-reg oplen)
    (if val-cst?
        (x86-mov cgc dest (x86-imm-int (char->integer lval)))
        (begin
          (if (not (eq? dest opval)) (x86-mov cgc dest opval))
          (x86-shr cgc dest (x86-imm-int 2))))
    (x86-label cgc label-loop)
    (x86-cmp cgc selector-reg (x86-imm-int 0))
    (x86-je cgc label-end)

      (let ((memop (x86-mem (- 4 TAG_MEMOBJ) (x86-rax) selector-reg)))
        (x86-mov cgc memop (x86-r64->r32 dest))
        (x86-sub cgc selector-reg (x86-imm-int 4))
        (x86-jmp cgc label-loop))

    (x86-label cgc label-end)
    (x86-mov cgc dest (x86-rax))
    (x86-mov cgc selector-reg (x86-imm-int 0))))

;;
;; make-vector
(define (codegen-p-make-vector cgc fs op reg inlined-cond? llen lval cst-len? cst-val?)

  (cond ((and cst-len?
              (mem-still-required? (* 8 llen)))
           (error "NYI cgc"))
        (cst-len?
           (codegen-p-make-vector-imm cgc fs reg llen lval cst-val?))
        (else
           (codegen-p-make-vector-opn cgc fs reg llen lval cst-val?))))

;; make-vector with cst len
(define (codegen-p-make-vector-imm cgc fs reg llen lval cst-val?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (x86-imm-int (obj-encoding llen)))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval))
                    (codegen-loc-to-x86opnd fs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    ;; Alloc vector
    (gen-allocation-imm cgc STAG_VECTOR (* 8 llen))
    ;; Loop counter
    (x86-mov cgc selector-reg (x86-imm-int (* -8 llen)))
    ;; Loop
    (if cst-val?
        (x86-mov cgc dest (x86-imm-int (obj-encoding lval))))
    (x86-label cgc label-loop)
    (x86-cmp cgc selector-reg (x86-imm-int 0))
    (x86-je cgc label-end)

      (let ((memop (x86-mem 0 alloc-ptr selector-reg)))
        (if cst-val?
            (x86-mov cgc memop dest)
            (x86-mov cgc memop opval))
        (x86-add cgc selector-reg (x86-imm-int 8))
        (x86-jmp cgc label-loop))

    (x86-label cgc label-end)
    (x86-lea cgc dest (x86-mem (+ (* (+ llen 1) -8) TAG_MEMOBJ) alloc-ptr))
    (x86-mov cgc selector-reg (x86-imm-int 0))))

;; make-vector with !cst len
(define (codegen-p-make-vector-opn cgc fs reg llen lval cst-val?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd fs llen))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval))
                    (codegen-loc-to-x86opnd fs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    ;; Alloc
    (x86-mov cgc (x86-rax) oplen)
    (x86-shl cgc (x86-rax) (x86-imm-int 1))
    (gen-allocation-rt cgc STAG_VECTOR (x86-rax))

    ;; Loop
    (x86-mov cgc selector-reg oplen)
    (x86-shl cgc selector-reg (x86-imm-int 1))
    (if cst-val?
        (x86-mov cgc dest (x86-imm-int (obj-encoding lval))))
    (x86-label cgc label-loop)
    (x86-cmp cgc selector-reg (x86-imm-int 0))
    (x86-je cgc label-end)

      (let ((memop (x86-mem (- TAG_MEMOBJ) (x86-rax) selector-reg)))

        (if cst-val?
            (x86-mov cgc memop dest)
            (x86-mov cgc memop opval))

        (x86-sub cgc selector-reg (x86-imm-int 8))
        (x86-jmp cgc label-loop))

    ;;
    (x86-label cgc label-end)
    (x86-mov cgc dest (x86-rax))
    (x86-mov cgc selector-reg (x86-imm-int 0))))

;;
;; vector-length
(define (codegen-p-vector-length cgc fs op reg inlined-cond? lvec cst-vec?)

  (assert (not cst-vec?) "Internal error")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opvec (codegen-loc-to-x86opnd fs lvec)))

    (if (ctx-loc-is-memory? lvec)
        (begin (x86-mov cgc (x86-rax) opvec)
               (set! opvec (x86-rax))))

    (x86-mov cgc dest (x86-mem (- TAG_MEMOBJ) opvec))
    (x86-shr cgc dest (x86-imm-int 9))))

;;
;; string-length
(define (codegen-p-string-length cgc fs op reg inlined-cond? lstr str-cst?)

  (assert (not str-cst?) "Internal error. Unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd fs lstr)))

    (if (ctx-loc-is-memory? lstr)
        (begin (x86-mov cgc (x86-rax) opstr)
               (set! opstr (x86-rax))))

    (x86-mov cgc dest (x86-mem (- TAG_MEMOBJ) opstr))
    (x86-shr cgc dest (x86-imm-int 8))))

;;
;; vector-ref
;; TODO val-cst? -> idx-cst?
(define (codegen-p-vector-ref cgc fs op reg inlined-cond? lvec lidx vec-cst? val-cst?)

  (assert (not vec-cst?) "Internal error")

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (opvec (codegen-loc-to-x86opnd fs lvec))
         (opidx (and (not val-cst?) (codegen-loc-to-x86opnd fs lidx)))
         (use-selector #f))

    (if (x86-mem? opvec)
        (begin (x86-mov cgc (x86-rax) opvec)
               (set! opvec  (x86-rax))))

    (if (and opidx
             (x86-mem? opidx))
        (if (eq? opvec (x86-rax))
            (begin (x86-mov cgc selector-reg opidx)
                   (set! opidx selector-reg)
                   (set! use-selector #t))
            (begin (x86-mov cgc (x86-rax) opidx)
                   (set! opidx (x86-rax)))))

    (if val-cst?
        (x86-mov cgc dest (x86-mem (+ (- 8 TAG_MEMOBJ) (* 8 lidx)) opvec #f 1))
        (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) opvec opidx 1)))

    (if use-selector
        (x86-xor cgc selector-reg selector-reg))))

;;
;; string-ref
(define (codegen-p-string-ref cgc fs op reg inlined-cond? lstr lidx str-cst? idx-cst?)

  (assert (not str-cst?) "Internal error, unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd fs lstr))
        (opidx (and (not idx-cst?) (codegen-loc-to-x86opnd fs lidx)))
        (str-mem? (ctx-loc-is-memory? lstr))
        (idx-mem? (ctx-loc-is-memory? lidx))
        (use-selector #f))

    (if (x86-mem? opstr)
        (begin (x86-mov cgc (x86-rax) opstr)
               (set! opstr (x86-rax))))
    (if (and opidx
             (x86-mem? opidx))
        (if (eq? opstr (x86-rax))
            (begin (x86-mov cgc selector-reg opidx)
                   (set! opidx selector-reg)
                   (set! use-selector #t))
            (begin (x86-mov cgc (x86-rax) opidx)
                   (set! opstr (x86-rax)))))

    (if idx-cst?
        (x86-mov cgc (x86-eax) (x86-mem (+ (- 8 TAG_MEMOBJ) (* 4 lidx)) opstr))
        (x86-mov cgc (x86-eax) (x86-mem (- 8 TAG_MEMOBJ) opidx opstr)))

    (x86-shl cgc (x86-rax) (x86-imm-int 2))
    (x86-add cgc (x86-rax) (x86-imm-int TAG_SPECIAL))
    (x86-mov cgc dest (x86-rax))))

;;
;; vector-set!
(define (codegen-p-vector-set! cgc fs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)

  (assert (not vec-cst?) "Internal error")
  (assert (or (not val-cst?)
              (not (##mem-allocated? lval)))
          "Internal error")

  (let* ((dest (codegen-reg-to-x86reg reg))
         (opvec (codegen-loc-to-x86opnd fs lvec))
         (opidx (if idx-cst?
                    (x86-imm-int (obj-encoding lidx))
                    (codegen-loc-to-x86opnd fs lidx)))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval))
                    (codegen-loc-to-x86opnd fs lval))))

    (if (x86-mem? opvec)
        (begin (x86-mov cgc (x86-rax) opvec)
               (set! opvec (x86-rax))))

    (cond ;; cst/cst
          ;; cst/mem
          ((or (and idx-cst? val-cst?)
               (and idx-cst? (x86-mem? opval)))
             (x86-mov cgc selector-reg opval)
             (x86-mov cgc (x86-mem (+ (* 8 lidx) (- 8 TAG_MEMOBJ)) opvec) selector-reg)
             (x86-mov cgc selector-reg (x86-imm-int 0)))
          ;; cst/reg
          (idx-cst?
             (x86-mov cgc (x86-mem (+ (* 8 lidx) (- 8 TAG_MEMOBJ)) opvec) opval))
          ;; mem/cst
          ;; mem/mem
          ;; reg/cst
          ;; reg/mem
          ((or (and (x86-mem? opidx) val-cst?)
               (and (x86-mem? opidx) (x86-mem? opval))
               (and (x86-reg? opidx) val-cst?)
               (and (x86-reg? opidx) (x86-mem? opval)))
             (if (eq? opvec (x86-rax))
                 (error "NYI"))
             (x86-mov cgc selector-reg opval)
             (x86-mov cgc (x86-rax) opidx)
             (x86-shl cgc (x86-rax) (x86-imm-int 1))
             (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opvec (x86-rax)) selector-reg)
             (x86-mov cgc selector-reg (x86-imm-int 0)))
          ;; reg/reg
          ;; mem/reg
          (else
             (x86-mov cgc selector-reg opidx)
             (x86-shl cgc selector-reg (x86-imm-int 1))
             (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opvec selector-reg) opval)
             (x86-mov cgc selector-reg (x86-imm-int 0))))

    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;
;; string-set!
(define (codegen-p-string-set! cgc fs op reg inlined-cond? lstr lidx lchr str-cst? idx-cst? chr-cst?)

  (assert (not str-cst?) "Internal error. Unexpected cst operand")

  (let ((dest (codegen-reg-to-x86reg reg))
        (opstr (lambda () (codegen-loc-to-x86opnd fs lstr)))
        (opidx (lambda () (and (not idx-cst?) (codegen-loc-to-x86opnd fs lidx))))
        (opchr (lambda () (and (not chr-cst?) (codegen-loc-to-x86opnd fs lchr))))
        (oprax (lambda () (x86-rax))))

    (begin-with-cg-macro

      ;;
      ;; Unmem
      (if (not chr-cst?)
          (unmem! (oprax) (opchr))) ;; If not a cst, we want char in rax


      (if (not idx-cst?)
          (if (eq? (opchr) (x86-rax))
              (pick-unmem! (opidx) (list (opstr) (opidx) (opchr)))
              (unmem! (oprax) (opidx))))

      (if (or (eq? (opidx) (x86-rax))
              (eq? (opchr) (x86-rax)))
          (chk-pick-unmem! (opstr) (list (opstr) (opidx) (opchr)))
          (chk-unmem! (oprax) (opstr)))

      ;; TODO: no need to have idx in a new reg (idx reg is not modified)

      ;;
      ;; Primitive code
      (if (not chr-cst?)
          (x86-shr cgc (opchr) (x86-imm-int 2)))

      (cond ((and idx-cst? chr-cst?)
             (x86-mov cgc
                      (x86-mem (+ (- 8 TAG_MEMOBJ) (* 4 lidx)) (opstr))
                      (x86-imm-int (char->integer lchr))
                      32))
            (idx-cst?
               (x86-mov cgc
                        (x86-mem (+ (- 8 TAG_MEMOBJ) (* 4 lidx)) (opstr))
                        (x86-eax)))
            (chr-cst?
              (x86-mov cgc
                       (x86-mem (- 8 TAG_MEMOBJ) (opstr) (opidx))
                       (x86-imm-int (char->integer lchr))
                       32))
            (else
              (x86-mov cgc
                       (x86-mem (- 8 TAG_MEMOBJ) (opstr) (opidx))
                       (x86-eax)))) ;; If char is not a cst, it is in rax

      (x86-mov cgc dest (x86-imm-int ENCODING_VOID)))))

(define (codegen-p-gettime-ns cgc fs op reg inlined-cond?)
  (let ((opnd (codegen-reg-to-x86reg reg)))
    ;; Get monotonic time in rax
    (gen-syscall-clock-gettime cgc)
    (x86-mov cgc opnd (x86-rax))
    (x86-shl cgc opnd (x86-imm-int 2))))

;;-----------------------------------------------------------------------------
;; Others
;;-----------------------------------------------------------------------------

(define (codegen-subtype cgc fs reg lval)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))

    (if (x86-mem? opval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    ;; Get header
    (x86-mov cgc dest (x86-mem (- TAG_MEMOBJ) opval))
    ;; Get stype
    (x86-and cgc dest (x86-imm-int 248))
    (x86-shr cgc dest (x86-imm-int 1))))

(define (codegen-fixnum->flonum cgc fs reg lval)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (lval  (codegen-loc-to-x86opnd fs lval)))

    (gen-allocation-imm cgc STAG_FLONUM 8)

    (x86-cvtsi2sd cgc (x86-xmm0) lval)
    (x86-movsd cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) (x86-xmm0))
    (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))))


(define (codegen-make-vector-cst cgc fs reg len lval)

  (let ((loop     (asm-make-label #f (new-sym 'make-vector-loop)))
        (loop-end (asm-make-label #f (new-sym 'make-vector-end)))
        (opval (if lval
                   (codegen-loc-to-x86opnd fs lval)
                   (x86-imm-int 0)))
        (dest  (codegen-reg-to-x86reg reg)))

    ;; Primitive code
    (gen-allocation-imm cgc STAG_VECTOR (* len 8))

    ;; dest contains encoded vector
    (x86-lea cgc dest (x86-mem (- (* len -8) (- 8 TAG_MEMOBJ)) alloc-ptr))
    (x86-mov cgc (x86-rax) (x86-imm-int (* len 8)))
    (x86-label cgc loop)
    (x86-cmp cgc (x86-rax) (x86-imm-int 0))
    (x86-je cgc loop-end)
      (x86-mov cgc (x86-mem (* -1 TAG_MEMOBJ) (x86-rax) dest) opval 64)
      (x86-sub cgc (x86-rax) (x86-imm-int 8))
      (x86-jmp cgc loop)
    (x86-label cgc loop-end)))
