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

(include "config.scm")

(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

(define-macro (on-dynamic-mode expr)
  `(if (not opt-static-mode)
       ,expr))

(define-macro (l32 loc)
  `(if (x86-reg? ,loc)
       (x86-r64->r32 ,loc)
       ,loc))

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
  (set! x86-ppop  gpop)
  (set! x86-ppush gpush)
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
(define tmp-reg (x86-rax))
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
(define OFFSET_BODY 8)
(define (OFFSET_PROC_FREE i) (+ 16 (* 8 i)))

;;-----------------------------------------------------------------------------
;; x86 Codegen utils

(define-macro (neq? l r)
  `(not (eq? ,l ,r)))

(define (know-dest-symbol dest-addr)
  (let ((str (string-append "dest_"
                            (number->string dest-addr 16)
                            "_")))
    (new-sym (string->symbol str))))

(define (int32? n)
  (and (integer? n)
       (>= n (expt -2 31))
       (<  n (expt 2 31))))

(define (codegen-void cgc reg)
  (let ((opnd (codegen-reg-to-x86reg reg)))
    (x86-mov cgc opnd (x86-imm-int (obj-encoding #!void)))))

(define (codegen-set-bool cgc b reg)
  (let ((dest (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (obj-encoding b 1)))))

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


(define (codegen-loc-to-x86opnd fs ffs loc)
  (cond ((eq? loc 'selector)
         selector-reg)
        ((eq? loc 'tmp)
         tmp-reg)
        ((ctx-loc-is-register? loc)
         (codegen-reg-to-x86reg loc))
        ((ctx-loc-is-memory? loc)
         (codegen-mem-to-x86mem fs loc))
        ((ctx-loc-is-fregister? loc)
         (codegen-freg-to-x86reg loc))
        ((ctx-loc-is-fmemory? loc)
         (codegen-fmem-to-x86mem ffs loc))
        (else (error "Internal error"))))

(define (codegen-mem-to-x86mem fs mem)
  (x86-mem (* 8 (- fs (cdr mem) 1)) (x86-usp)))

(define (codegen-reg-to-x86reg reg)
  (cond ((eq? reg 'selector) selector-reg)
        ((eq? reg 'tmp) tmp-reg)
        (else
          (cdr (assoc reg codegen-regmap)))))

(define (codegen-freg-to-x86reg freg)
  (list-ref regalloc-fregs (cdr freg)))

(define (codegen-fmem-to-x86mem ffs fmem)
  (x86-mem (* 8 (- ffs (cdr fmem) 1)) (x86-rsp)))

(define (codegen-is-imm-64? imm)
  (or (< imm -2147483648)
      (> imm 2147483647)))

(define (immediate-to-xmm cgc xmm flo)
  (if (= flo 0.0)
      (x86-pxor cgc xmm xmm)
      (let* ((rep (flonum->ieee754 flo 'double))
             (rl (and (not (= rep 0)) (xmm_imm_shift rep)))) ;; right left
        (if rl
            ;; We can use psrl/psll
            (begin (x86-pcmpeqb cgc xmm xmm)
                   (and (> (car rl) 0) (x86-psrlw cgc xmm (x86-imm-int (car rl))))
                   (and (> (cdr rl) 0) (x86-psllw cgc xmm (x86-imm-int (cdr rl)))))
            (begin (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value (flonum->ieee754 flo 'double))))
                   (x86-movd/movq cgc xmm (x86-rax)))))))

;;-----------------------------------------------------------------------------
;; Stacks
;;-----------------------------------------------------------------------------

(define (codegen-clean-stacks cgc ustack pstack)
  (if (not (= ustack 0))
      (x86-add cgc (x86-usp) (x86-imm-int (* 8 ustack))))
  (if (not (= pstack 0))
      (x86-add cgc (x86-rsp) (x86-imm-int (* 8 pstack)))))

;;-----------------------------------------------------------------------------
;; Define
;;-----------------------------------------------------------------------------

(define (codegen-define-bind cgc fs ffs pos reg lvalue cst?)

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (if cst?
                   (x86-imm-int (obj-encoding lvalue 2))
                   (codegen-loc-to-x86opnd fs ffs lvalue))))
    (cond ((x86-reg? opval)
             (x86-mov cgc (x86-mem (* 8 pos) global-ptr) opval))
          (else (x86-mov cgc (x86-rax) opval)
                (x86-mov cgc (x86-mem (* 8 pos) global-ptr) (x86-rax))))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

;;-----------------------------------------------------------------------------
;; If
;;-----------------------------------------------------------------------------

(define (codegen-if cgc fs ffs label-jump label-false label-true lcond)
  (let ((opcond (codegen-loc-to-x86opnd fs ffs lcond))
        (immf (obj-encoding #f 3)))

    (if (codegen-is-imm-64? immf)
        (begin (x86-mov cgc (x86-rax) (x86-imm-int immf))
               (x86-cmp cgc opcond (x86-rax)))
        (x86-cmp cgc opcond (x86-imm-int immf)))
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
(define (codegen-get-free cgc fs ffs reg lclo lvar)
  (let ((copnd (codegen-loc-to-x86opnd fs ffs lclo))
        (coffset (- (* 8 (+ (cdr lvar) 2)) TAG_MEMOBJ))
        (dest (codegen-loc-to-x86opnd fs ffs reg))
        (x86-op (if (ctx-loc-is-fregister? reg) x86-movsd x86-mov)))

  (if opt-nan-boxing
      (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
             (x86-and cgc (x86-rax) copnd)
             (x86-op cgc dest (x86-mem (* 8 (+ (cdr lvar) 2)) (x86-rax))))
      (if (x86-reg? copnd)
          (x86-op cgc dest (x86-mem coffset copnd))
          (begin
            (x86-mov cgc (x86-rax) copnd) ;; Get closure
            (x86-op cgc dest (x86-mem coffset (x86-rax))))))))

(define (codegen-write-free-variable cgc fs ffs src-loc dst-pos closure-loc base-reg)
  (if opt-nan-boxing
      (codegen-write-free-variable-nan cgc fs ffs src-loc dst-pos closure-loc base-reg)
      (codegen-write-free-variable-tag cgc fs ffs src-loc dst-pos closure-loc base-reg)))

(define (codegen-write-free-variable-nan cgc fs ffs src-loc dst-pos closure-loc base-reg)
  (cond ;;
        ((ctx-loc-is-freemem? src-loc)
           (let* ((closure-opnd (codegen-loc-to-x86opnd fs ffs closure-loc))
                  (fvar-pos (cdr src-loc))
                  (fvar-offset (+ 16 (* 8 fvar-pos)))
                  (treg (x86-rax)))
             (if (ctx-loc-is-memory? closure-loc)
                 (begin (x86-mov cgc (x86-rax) closure-opnd)
                        (set! closure-opnd (x86-rax))))
             (if (eq? closure-opnd (x86-rax))
                 (begin (set! treg selector-reg)))
             (x86-mov cgc treg (x86-imm-int NB_MASK_VALUE_48))
             (x86-and cgc treg closure-opnd)
             (x86-mov cgc treg (x86-mem fvar-offset treg))
             (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) treg)
             (eq? treg selector-reg)))
        ;;
        ((or (ctx-loc-is-memory? src-loc)
             (ctx-loc-is-fmemory? src-loc))
          (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd fs ffs src-loc))
          (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) (x86-rax))
          #f)
        ;;
        ((ctx-loc-is-fregister? src-loc)
          (x86-movd/movq cgc (x86-rax) (codegen-freg-to-x86reg src-loc))
          (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) (x86-rax))
          #f)
        ;;
        (else
          (let ((opn (codegen-reg-to-x86reg src-loc)))
            (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) opn)
            #f))))

(define (codegen-write-free-variable-tag cgc fs ffs src-loc dst-pos closure-loc base-reg)
  (cond ;;
        ((ctx-loc-is-freemem? src-loc)
           (let* ((closure-opnd (codegen-loc-to-x86opnd fs ffs closure-loc))
                  (fvar-pos (cdr src-loc))
                  (fvar-offset (+ 16 (* 8 fvar-pos))))
             (if (ctx-loc-is-memory? closure-loc)
                 (begin (x86-mov cgc (x86-rax) closure-opnd)
                        (set! closure-opnd (x86-rax))))
             (x86-mov cgc (x86-rax) (x86-mem (- fvar-offset TAG_MEMOBJ) closure-opnd))
             (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) (x86-rax))))
        ;;
        ((or (ctx-loc-is-memory? src-loc)
             (ctx-loc-is-fmemory? src-loc))
          (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd fs ffs src-loc))
          (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) (x86-rax)))
        ;;
        ((ctx-loc-is-fregister? src-loc)
          (assert (not (eq? base-reg (x86-rax))) "Internal error in codegen, unimplemented case")
          (x86-movd/movq cgc (x86-rax) (codegen-freg-to-x86reg src-loc))
          (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) (x86-rax)))
        ;;
        (else
          (let ((opn (codegen-reg-to-x86reg src-loc)))
            (x86-mov cgc (x86-mem (* 8 dst-pos) base-reg) opn))))
  #f)

;;-----------------------------------------------------------------------------
;; set
(define (codegen-set-global cgc fs ffs reg pos lval cst?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (if cst?
                   (x86-imm-int (obj-encoding lval 4))
                   (codegen-loc-to-x86opnd fs ffs lval))))

    (if (or (x86-imm? opval)
            (x86-mem? opval))
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (x86-mov cgc (x86-mem (* 8 pos) global-ptr) opval)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

(define (codegen-set-non-global cgc reg lval fs)
  (if opt-nan-boxing (error "NYI codegen nan"))
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))

    (if (x86-mem? opval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) opval)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

;;-----------------------------------------------------------------------------
;; Values
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Literal
(define (codegen-literal cgc lit reg)
  (let ((dest (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (obj-encoding lit 5)))))

;;-----------------------------------------------------------------------------
;; Symbol
(define (codegen-symbol cgc sym reg)
  (if opt-nan-boxing (error "NYI codegen nan"))
  (let ((qword (obj-encoding sym 6))
        (dest  (codegen-reg-to-x86reg reg)))
    ;; Check symbol is a PERM gambit object
    (assert (= (bitwise-and (get-i64 (- qword TAG_MEMOBJ)) 7) 6) "Internal error")
    (x86-mov cgc dest (x86-imm-int qword))))

;;-----------------------------------------------------------------------------
;; String
(define (codegen-string cgc str reg)
  (if opt-nan-boxing (error "NYI codegen nan"))

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
;; Boxing
;;-----------------------------------------------------------------------------

(define (codegen-box-float-nan cgc src dst)
  (if (x86-mem? src)
      (x86-mov cgc dst src)
      (x86-movd/movq cgc dst src)))

(define (codegen-box-float-tag cgc float-loc src dst)
  (gen-allocation-imm cgc STAG_FLONUM 8)
  (if (ctx-loc-is-fregister? float-loc)
      (x86-movsd cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) src)
      (begin
        (x86-mov cgc (x86-rax) src)
        (x86-mov cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) (x86-rax))))
  (x86-lea cgc dst (x86-mem (- TAG_MEMOBJ 16) alloc-ptr)))

(define (codegen-box-float cgc fs ffs float-loc dest-loc)
  (define opnd (codegen-loc-to-x86opnd fs ffs float-loc))
  (define dest (codegen-reg-to-x86reg dest-loc))
  (assert (ctx-loc-is-register? dest-loc) "Internal codegen error")
  (if opt-nan-boxing
      (codegen-box-float-nan cgc opnd dest)
      (codegen-box-float-tag cgc float-loc opnd dest)))

;;-----------------------------------------------------------------------------
;; Functions
;;-----------------------------------------------------------------------------

;; Generate a generic function prologue without rest param
(define (codegen-prologue-gen-nrest cgc fn-nb-args)
  (let ((imm (obj-encoding fn-nb-args)))
    (if (codegen-is-imm-64? imm)
        (begin (x86-mov cgc (x86-rax) (x86-imm-int imm))
               (x86-cmp cgc (x86-rdi) (x86-rax)))
        (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding fn-nb-args 7)))))
  (x86-jne cgc label-err-wrong-num-args))

;; Generate a generic function prologue with rest param
(define (codegen-prologue-gen-rest cgc fs ffs fn-nb-args)
  (if opt-nan-boxing
      (codegen-prologue-gen-rest-nan cgc fs ffs fn-nb-args)
      (codegen-prologue-gen-rest-tag cgc fs ffs fn-nb-args)))

(define (codegen-prologue-gen-rest-nan cgc fs ffs fn-nb-args)

  (let ((nb-args-regs       (length args-regs))
        (label-rest         (asm-make-label #f (new-sym 'prologue-rest)))
        (label-rest-loop    (asm-make-label #f (new-sym 'prologue-rest-loop)))
        (label-rest-end     (asm-make-label #f (new-sym 'prologue-rest-end)))
        (label-next-arg     (asm-make-label #f (new-sym 'prologue-next-arg)))
        (label-from-stack   (asm-make-label #f (new-sym 'prologue-from-stack)))
        (label-next-arg-end (asm-make-label #f (new-sym 'prologue-next-arg-end))))

    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (- fn-nb-args 1))))
    (x86-cmp cgc (x86-rdi) (x86-rax))
    (x86-jge cgc label-rest)
      (gen-error cgc ERR_WRONG_NUM_ARGS)

    ;; Get next arg
    (x86-label cgc label-next-arg)
      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (length args-regs))))
      (x86-cmp cgc (x86-rdi) (x86-rax))
      (x86-jg cgc label-from-stack)
      (let loop ((i (length  args-regs))
                 (regs (reverse args-regs)))
        (if (> i 0)
            (begin
                (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding i)))
                (x86-cmp cgc (x86-rdi) (x86-rax))
                (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd fs ffs (car regs)))
                (x86-je cgc label-next-arg-end)
                (loop (- i 1) (cdr regs)))))
      (x86-jmp cgc label-next-arg-end)
      (x86-label cgc label-from-stack)
      (x86-upop cgc (x86-rax))
      (x86-label cgc label-next-arg-end)
      (x86-sub cgc (x86-rdi) (x86-imm-int 1))
      (x86-ret cgc)
    ;; END get next arg

    (x86-label cgc label-rest)
    ;; cdr (rax) = '()
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding '() 12)))
    (x86-label cgc label-rest-loop)
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (- fn-nb-args 1))))
    (x86-cmp cgc (x86-rdi) (x86-rax))
    (x86-je cgc label-rest-end)

      ;; Alloc
      (gen-allocation-imm cgc STAG_PAIR 16)
      (x86-pcall cgc label-next-arg)
      (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax))
      (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) selector-reg)

      ;;
      (x86-mov cgc selector-reg (x86-imm-int (to-64-value NB_MASK_MEM)))
      (x86-lea cgc selector-reg (x86-mem -24 alloc-ptr selector-reg))
      (x86-jmp cgc label-rest-loop)
    ;;
    (x86-label cgc label-rest-end)
    (if (<= fn-nb-args (length args-regs))
        (let ((reg (list-ref args-regs (- fn-nb-args 1))))
          (x86-mov cgc (codegen-reg-to-x86reg reg) selector-reg))
        (x86-upush cgc selector-reg))
    ;; Reset selector used as temporary
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 14)))))


(define (codegen-prologue-gen-rest-tag cgc fs ffs fn-nb-args)

  (let ((nb-args-regs       (length args-regs))
        (label-rest         (asm-make-label #f (new-sym 'prologue-rest)))
        (label-rest-loop    (asm-make-label #f (new-sym 'prologue-rest-loop)))
        (label-rest-end     (asm-make-label #f (new-sym 'prologue-rest-end)))
        (label-next-arg     (asm-make-label #f (new-sym 'prologue-next-arg)))
        (label-from-stack   (asm-make-label #f (new-sym 'prologue-from-stack)))
        (label-next-arg-end (asm-make-label #f (new-sym 'prologue-next-arg-end))))

    (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (- fn-nb-args 1) 8)))
    (x86-jge cgc label-rest)
      (gen-error cgc ERR_WRONG_NUM_ARGS)

    ;; Get next arg
    (x86-label cgc label-next-arg)
      (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (length args-regs) 9)))
      (x86-jg cgc label-from-stack)
      (let loop ((i (length  args-regs))
                 (regs (reverse args-regs)))
        (if (> i 0)
            (begin
                (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding i 10)))
                (x86-mov cgc (x86-rax) (codegen-loc-to-x86opnd fs ffs (car regs)))
                (x86-je cgc label-next-arg-end)
                (loop (- i 1) (cdr regs)))))
      (x86-jmp cgc label-next-arg-end)
      (x86-label cgc label-from-stack)
      (x86-upop cgc (x86-rax))
      (x86-label cgc label-next-arg-end)
      (x86-sub cgc (x86-rdi) (x86-imm-int (obj-encoding 1 11)))
      (x86-ret cgc)
    ;; END get next arg

    (x86-label cgc label-rest)
    ;; cdr (rax) = '()
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding '() 12)))
    (x86-label cgc label-rest-loop)
    (x86-cmp cgc (x86-rdi) (x86-imm-int (obj-encoding (- fn-nb-args 1) 13)))
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
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 14)))))

;; Generate specialized function prologue with rest param and actual == formal
(define (codegen-prologue-rest= cgc destreg)
  (let ((dest
          (and destreg (codegen-reg-to-x86reg destreg))))
    (if dest
        (x86-mov cgc dest (x86-imm-int (obj-encoding '() 15)))
        (x86-upush cgc (x86-imm-int (obj-encoding '() 16))))))

;; Generate specialized function prologue with rest param and actual > formal
(define (codegen-prologue-rest> cgc fs ffs locs rloc)
  ;; TODO: one allocation for list
  ;; TODO: use x86 loop for mem locs

  (let ((dest (codegen-loc-to-x86opnd fs ffs rloc)))

    (x86-mov cgc (x86-rcx) (x86-imm-int (obj-encoding '() 17)))

    (let loop ((locs locs))
      (if (not (null? locs))
          (let* ((loc (car locs))
                 (cdr-pushed? #f)
                 (opnd
                   (cond ((eq? (car loc) 'c)
                            (x86-imm-int (obj-encoding (cdr loc) 18)))
                         ((eq? (car loc) 'cf)
                            (let ((entry-obj (car (asc-globalfn-entry-get (cdr loc)))))
                              (x86-upush cgc (x86-rcx))
                              (gen-closure cgc 'selector #f entry-obj '())
                              (set! cdr-pushed? #t)
                              (x86-rcx)))
                         (else
                            (codegen-loc-to-x86opnd fs ffs loc)))))
            (gen-allocation-imm cgc STAG_PAIR 16)
            (if (or (x86-mem? opnd)
                    (x86-imm-int? opnd))
                (begin (x86-mov cgc (x86-rax) opnd)
                       (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax)))
                (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) opnd))
            (if cdr-pushed?
                (x86-upop cgc (x86-rcx)))
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-rcx))
            (if opt-nan-boxing
                (begin (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
                       (x86-lea cgc (x86-rcx) (x86-mem -24 alloc-ptr (x86-rax))))
                (x86-lea cgc (x86-rcx) (x86-mem (+ -24 TAG_PAIR) alloc-ptr)))
            (loop (cdr locs)))))

    (x86-mov cgc dest (x86-rcx))
    (x86-mov cgc (x86-rcx) (x86-imm-int (obj-encoding 0 19)))))

;; Alloc closure and write header
(define (codegen-closure-create cgc nb-free)
  (let* ((closure-size  (+ 1 nb-free))) ;; entry point & free vars
    ;; Alloc closure
    (gen-allocation-imm cgc STAG_PROCEDURE (* 8 closure-size))))

;; Write entry point in closure (do not use cctable)
(define (codegen-closure-ep cgc entryvec-loc nb-free)
  (let ((offset (+ OFFSET_PROC_EP (* -8 (+ nb-free 2)))))
    (if (int32? (+ 8 entryvec-loc))
        (x86-mov cgc (x86-rax) (x86-mem (+ 8 entryvec-loc)))
        (begin (x86-mov cgc (x86-rax) (x86-imm-int (+ 8 entryvec-loc)))
               (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rax)))))
    (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))))

;; Write cctable ptr in closure (use multiple entry points)
(define (codegen-closure-cc cgc cctable-loc nb-free)
  (let ((offset (+ OFFSET_PROC_EP (* -8 (+ nb-free 2)))))
    (x86-mov cgc (x86-rax) (x86-imm-int cctable-loc))
    (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))))

;; Load closure in tmp register
(define (codegen-load-closure cgc fs ffs loc)
  (let ((opnd (codegen-loc-to-x86opnd fs ffs loc)))
    (x86-mov cgc (x86-rax) opnd)))

;;; Push closure
(define (codegen-closure-put cgc reg nb-free)
  (if opt-nan-boxing
      (let ((dest   (codegen-reg-to-x86reg reg))
            (offset (* -8 (+ nb-free 2))))
        (x86-mov cgc dest (x86-imm-int (to-64-value NB_MASK_MEM)))
        (x86-lea cgc dest (x86-mem offset alloc-ptr dest)))
      (let ((dest   (codegen-reg-to-x86reg reg))
            (offset (+ (* -8 (+ nb-free 2)) TAG_MEMOBJ)))
        (x86-lea cgc dest (x86-mem offset alloc-ptr)))))

;;
(define (codegen-return-retobj cgc fs ffs lretobj)
  (let ((opretobj (codegen-loc-to-x86opnd fs ffs lretobj)))
    (if (not (eq? opretobj (x86-rdx)))
        (x86-mov cgc (x86-rdx) opretobj))))

;;
(define (codegen-return-clean cgc fs ffs)
  (if (> fs 0)
      (x86-add cgc (x86-usp) (x86-imm-int (* 8 fs))))
  (if (> ffs 0)
      (x86-add cgc (x86-rsp) (x86-imm-int (* 8 ffs)))))

;; Generate function return using a return address
;; Retaddr (or cctable) is in rdx
(define (codegen-return-rp cgc fs ffs lretobj lretval float?)

    (let ((opret    (codegen-reg-to-x86reg return-reg))
          (opretval (codegen-loc-to-x86opnd fs ffs lretval)))

      (if (and (eq? lretobj return-reg)
               (not float?))
          (error "NYI, see codegen-return-cr"))

      ;; Move return value to return register
      (if float?
          ;; Box float
          (begin
            (gen-allocation-imm cgc STAG_FLONUM 8)
            (if (x86-mem? opretval)
                (error "NYI")
                (x86-movsd cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) opretval))
            (x86-lea cgc opret (x86-mem (- TAG_MEMOBJ 16) alloc-ptr)))
          ;;
          (if (not (eq? opret opretval))
              (x86-mov cgc opret opretval))))

    (codegen-return-retobj cgc fs ffs lretobj)
    (codegen-return-clean cgc fs ffs)
    (x86-jmp cgc (x86-rdx)))

;; Generate function return using a crtable
;; Retaddr (or cctable) is in rdx
(define (codegen-return-cr cgc fs ffs lretobj lretval cridx float? cst?)

      (define cont-saved? #f)

      (define (move-retval)
        (let ((opret (if float?
                         (codegen-freg-to-x86reg return-freg)
                         (codegen-reg-to-x86reg return-reg)))
              (opretval (and (not cst?) (codegen-loc-to-x86opnd fs ffs lretval))))
          (if (and opretval
                   (not (eq? opret opretval)))
              (if float?
                  (x86-movsd cgc opret opretval)
                  (x86-mov cgc opret opretval)))))

      (assert (not (and cst? (not opt-const-vers)))
              "Internal error")

    ;; If the continuation is in the return register,
    ;; save it to rax
    (if (and (eq? lretobj return-reg)
             (not float?))
        (begin
          (x86-mov cgc (x86-rax) (codegen-reg-to-x86reg return-reg))
          (set! cont-saved? #t)))

    (move-retval)
    (if (not cont-saved?)
        (codegen-return-retobj cgc fs ffs lretobj))
    (codegen-return-clean cgc fs ffs)
    (let ((table-reg (if cont-saved? (x86-rax) (x86-rdx))))
      (if cridx
          (x86-mov cgc (x86-rax) (x86-mem (+ 16 (* 8 cridx)) table-reg))
          (x86-mov cgc (x86-rax) (x86-mem 8 table-reg)))
      (x86-mov cgc (x86-r11) (x86-imm-int (obj-encoding cridx 20)))
      (x86-jmp cgc (x86-rax))))

;; Generate function call using a single entry point
;; eploc is the cctable or entry points if it's known
(define (codegen-call-ep cgc nb-args eploc direct-eploc)
  (if opt-nan-boxing (error "NYI codegen nan"))
  (if nb-args ;; If nb-args given, move encoded in rdi, else nb-args is already encoded in rdi (apply)
      (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args 21))))

  (cond (direct-eploc
          ;; If it's a direct call to a not yet generated entry point, add stub_load label
          (if (eq? (car direct-eploc) 'stub)
              (let ((load-label (caddr direct-eploc)))
                (x86-label cgc load-label)))
          (let ((label (asm-make-label #f (know-dest-symbol (cadr direct-eploc)) (cadr direct-eploc))))
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
  (if opt-nan-boxing
      (codegen-call-cc-gen-nan cgc nb-args eploc)
      (codegen-call-cc-gen-tag cgc nb-args eploc)))

(define (codegen-call-cc-gen-nan cgc nb-args eploc)
  (if nb-args
      (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args 22))))

  (if eploc
      (x86-mov cgc (x86-rdx) (x86-imm-int eploc))
      (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
             (x86-and cgc (x86-rax) (x86-rsi))
             (x86-mov cgc (x86-rdx) (x86-mem 8 (x86-rax))))) ;; get table

  (x86-jmp cgc (x86-mem 8 (x86-rdx)))) ;; Jump to generic entry point

(define (codegen-call-cc-gen-tag cgc nb-args eploc)
  (if nb-args
      (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args 22))))

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
          (x86-mov cgc (x86-r11) (x86-imm-int (obj-encoding idx 23))))
      ;; 2 - Put nbargs in rdi if needed
      (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args)))
      ;; 3- Get cc-table
      (cond (direct-eploc
              ;; If it's a direct call to a not yet generated entry point, add stub_load label
              (if (eq? (car direct-eploc) 'stub)
                  (let ((load-label (caddr direct-eploc)))
                    (x86-label cgc load-label)))
              (let ((label (asm-make-label #f (know-dest-symbol (cadr direct-eploc)) (cadr direct-eploc))))
                (x86-jmp cgc label)))
            (eploc
              (x86-mov cgc (x86-rdx) (x86-imm-int eploc))
              (x86-jmp cgc (x86-mem cct-offset (x86-rdx))))
            (else
              (if opt-nan-boxing
                  (begin (x86-mov cgc (x86-rdx) (x86-imm-int NB_MASK_VALUE_48))
                         (x86-and cgc (x86-rdx) (x86-rsi))
                         (x86-mov cgc (x86-rdx) (x86-mem 8 (x86-rdx))))
                  (x86-mov cgc (x86-rdx) (x86-mem (- 8 TAG_MEMOBJ) (x86-rsi))))
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

(define (codegen-apply-xxx cgc fs ffs lst-loc)
  (if opt-nan-boxing
      (codegen-apply-xxx-nan cgc fs ffs lst-loc)
      (codegen-apply-xxx-tag cgc fs ffs lst-loc)))

(define (codegen-apply-xxx-nan cgc fs ffs lst-loc)
  ;; r11, selector & r15 are used as tmp registers
  ;; It is safe because they are not used for parameters.
  ;; And if they are used after, they already are saved on the stack
  (define label-end (asm-make-label #f (new-sym 'apply-end-args)))
  (define lst-op (codegen-loc-to-x86opnd fs ffs lst-loc))
  (x86-mov cgc (x86-rdx) lst-op)
  (x86-mov cgc (x86-r11) (x86-imm-int 0))
  (let loop ((args-regs args-regs))
    (if (null? args-regs)
        (let ((label-loop (asm-make-label #f (new-sym 'apply-loop-args))))
          (x86-label cgc label-loop)
          (x86-mov cgc selector-reg (x86-imm-int (obj-encoding '())))
          (x86-cmp cgc (x86-rdx) selector-reg)
          (x86-je cgc label-end)
            (x86-inc cgc (x86-r11))
            (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_48))
            (x86-and cgc selector-reg (x86-rdx))
            (x86-mov cgc (x86-rdx) (x86-mem OFFSET_PAIR_CAR selector-reg))
            (x86-upush cgc (x86-rdx))
            (x86-mov cgc (x86-rdx) (x86-mem OFFSET_PAIR_CDR selector-reg))
            (x86-jmp cgc label-loop))
        (begin
          (x86-mov cgc selector-reg (x86-imm-int (obj-encoding '())))
          (x86-cmp cgc (x86-rdx) selector-reg)
          (x86-je cgc label-end)
            (x86-inc cgc (x86-r11))
            (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_48))
            (x86-and cgc selector-reg (x86-rdx))
            (x86-mov cgc (codegen-loc-to-x86opnd fs ffs (car args-regs)) (x86-mem OFFSET_PAIR_CAR selector-reg))
            (x86-mov cgc (x86-rdx) (x86-mem OFFSET_PAIR_CDR selector-reg))
          (loop (cdr args-regs)))))
  (x86-label cgc label-end)
  (x86-mov cgc selector-reg (x86-imm-int (to-64-value NB_MASK_FIX)))
  (x86-or cgc (x86-r11) selector-reg)
  ;; Reset selector used as tmp reg
  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 26))))

(define (codegen-apply-xxx-tag cgc fs ffs lst-loc)
  ;; r11, selector & r15 are used as tmp registers
  ;; It is safe because they are not used for parameters.
  ;; And if they are used after, they already are saved on the stack
  (define label-end (asm-make-label #f (new-sym 'apply-end-args)))
  (define lst-op (codegen-loc-to-x86opnd fs ffs lst-loc))
  (x86-mov cgc (x86-rdx) lst-op)
  (x86-mov cgc (x86-r11) (x86-imm-int 0))
  (let loop ((args-regs args-regs))
    (if (null? args-regs)
        (let ((label-loop (asm-make-label #f (new-sym 'apply-loop-args))))
          (x86-label cgc label-loop)
          (x86-cmp cgc (x86-rdx) (x86-imm-int (obj-encoding '() 24)))
          (x86-je cgc label-end)
            (x86-add cgc (x86-r11) (x86-imm-int 4))
            (x86-mov cgc selector-reg (x86-mem (- OFFSET_PAIR_CAR TAG_PAIR) (x86-rdx)))
            (x86-upush cgc selector-reg)
            (x86-mov cgc (x86-rdx) (x86-mem (- OFFSET_PAIR_CDR TAG_PAIR) (x86-rdx)))
            (x86-jmp cgc label-loop))
        (begin
          (x86-cmp cgc (x86-rdx) (x86-imm-int (obj-encoding '() 25)))
          (x86-je cgc label-end)
            (x86-add cgc (x86-r11) (x86-imm-int 4))
            (x86-mov cgc (codegen-loc-to-x86opnd fs ffs (car args-regs)) (x86-mem (- OFFSET_PAIR_CAR TAG_PAIR) (x86-rdx)))
            (x86-mov cgc (x86-rdx) (x86-mem (- OFFSET_PAIR_CDR TAG_PAIR) (x86-rdx)))
          (loop (cdr args-regs)))))
  (x86-label cgc label-end)
  ;; Reset selector used as tmp reg
  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 26))))

(define (codegen-gambit-call cgc op-symbol nb-args dest-loc)
  (assert (ctx-loc-is-register? dest-loc) "Internal error")

  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding nb-args 27)))
  (x86-upush cgc (x86-rax))
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding op-symbol 28)))
  (x86-upush cgc (x86-rax))
  (x86-pcall cgc label-gambit-call-handler)
  (x86-upop cgc (codegen-reg-to-x86reg dest-loc))
  (x86-add cgc (x86-usp) (x86-imm-int (* 8 (+ nb-args 1)))))

;; Shift arguments on stack for tail calls
;; starts with [usp+offset-from] -> [usp+offset-to]
(define (codegen-tail-shift cgc nb-shift offset-from offset-to)
  (if (>= nb-shift 0)
      (begin
        (x86-mov cgc (x86-r11) (x86-mem offset-from (x86-usp)))
        (x86-mov cgc (x86-mem offset-to (x86-usp)) (x86-r11))
        (codegen-tail-shift cgc (- nb-shift 1) (- offset-from 8) (- offset-to 8)))))

;;-----------------------------------------------------------------------------
;; Operators
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; N-ary arithmetic operators

(define (codegen-num-ii cgc fs ffs op reg lleft lright lcst? rcst?)

  (assert (not (and lcst? rcst?)) "Internal error")

  (let ((dest    (codegen-reg-to-x86reg reg))
        (opleft  (and (not lcst?) (codegen-loc-to-x86opnd fs ffs lleft)))
        (opright (and (not rcst?) (codegen-loc-to-x86opnd fs ffs lright))))

   ;; Handle cases like 1. 2. etc...
   (if (and lcst? (flonum? lleft))
       (set! lleft (##flonum->fixnum lleft)))
   (if (and rcst? (flonum? lright))
       (set! lright (##flonum->fixnum lright)))

   (if opt-nan-boxing
      (codegen-num-ii-nb  cgc op dest opleft opright lleft lright lcst? rcst?)
      (codegen-num-ii-tag cgc op dest opleft opright lleft lright lcst? rcst?))))

(define (codegen-num-ii-nb cgc op dest opleft opright lleft lright lcst? rcst?)

   (define (box reg)
     (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_FIX)))
     (x86-lea cgc reg (x86-mem 0 reg (x86-rax))))

   (define dest32 (x86-r64->r32 dest))

   (cond
     (lcst?
       (cond ((eq? op '+) (if (not (eq? dest opright))
                              (x86-mov cgc dest opright))
                          (x86-add cgc dest32 (x86-imm-int lleft))
                          (box dest))
             ((eq? op '-) (if (eq? dest opright)
                              (begin (x86-mov cgc (x86-rax) dest)
                                     (set! opright (x86-rax))))
                          (x86-mov cgc dest (x86-imm-int lleft))
                          (x86-sub cgc dest32 (l32 opright))
                          (box dest))
             ((eq? op '*) (x86-imul cgc dest32 (l32 opright) (x86-imm-int lleft))
                          (box dest))))
     (rcst?
       (cond ((eq? op '+)
                (if (not (eq? dest opleft))
                    (x86-mov cgc dest opleft))
                (x86-add cgc dest32 (x86-imm-int lright))
                (box dest))
             ((eq? op '-)
                (if (not (eq? dest opleft))
                    (x86-mov cgc dest opleft))
                (x86-sub cgc dest32 (x86-imm-int lright))
                (box dest))
             ((eq? op '*)
                (x86-imul cgc dest32 (l32 opleft) (x86-imm-int lright))
                (box dest))))
     (else
       (cond ((eq? op '+)
                (cond ((eq? dest opleft)
                         (x86-add cgc dest32 (l32 opright)))
                      ((eq? dest opright)
                         (error "nb ii 6.2"))
                      (else
                         (x86-mov cgc dest opleft)
                         (x86-add cgc dest32 (l32 opright))
                         (box dest))))
             ((eq? op '-)
                (cond ((eq? dest opleft)
                         (error "nb ii 7.1"))
                      ((eq? dest opright)
                         (error "nb ii 7.2"))
                      (else
                         (x86-mov cgc dest opleft)
                         (x86-sub cgc dest32 (l32 opright))
                         (box dest))))
             ((eq? op '*)
                (cond ((eq? dest opleft)
                         (error "nb ii 8.1"))
                      ((eq? dest opright)
                         (error "nb ii 8.2"))
                      (else
                         (x86-mov cgc dest opleft)
                         (x86-imul cgc dest32 (l32 opright))
                         (box dest))))))))

;; Gen code for arithmetic operation on int/int
(define (codegen-num-ii-tag cgc op dest opleft opright lleft lright lcst? rcst?)

   (cond
     (lcst?
       (cond ((eq? op '+) (if (not (eq? dest opright))
                              (x86-mov cgc dest opright))
                          (x86-add cgc dest (x86-imm-int (obj-encoding lleft 29))))
             ((eq? op '-) (if (eq? dest opright)
                              (begin (x86-mov cgc (x86-rax) dest)
                                     (set! opright (x86-rax))))
                          (x86-mov cgc dest (x86-imm-int (obj-encoding lleft 30)))
                          (x86-sub cgc dest opright))
             ((eq? op '*) (x86-imul cgc dest opright (x86-imm-int lleft)))))
     (rcst?
       (cond ((eq? op '+) (if (not (eq? dest opleft))
                              (x86-mov cgc dest opleft))
                          (x86-add cgc dest (x86-imm-int (obj-encoding lright 31))))
             ((eq? op '-) (if (not (eq? dest opleft))
                              (x86-mov cgc dest opleft))
                          (x86-sub cgc dest (x86-imm-int (obj-encoding lright 32))))
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
                         (x86-imul cgc dest opright))))))))

(define (codegen-num-ff cgc fs ffs op reg lleft leftint? lright rightint? lcst? rcst?)
  (if opt-float-unboxing
      (codegen-num-ff-nobox cgc fs ffs op reg lleft leftint? lright rightint? lcst? rcst?)
      (codegen-num-ff-box cgc fs ffs op reg lleft leftint? lright rightint? lcst? rcst?)))

;; Gen code for arithmetic operation on float/float (also handles int/float and float/int)
(define (codegen-num-ff-nobox cgc fs ffs op reg lleft leftint? lright rightint? lcst? rcst?)

  (assert (not (and lcst? rcst?)) "Internal error")

  (let ((sdest #f)
        (dest (codegen-freg-to-x86reg reg))
        (opleft (and (not lcst?) (codegen-loc-to-x86opnd fs ffs lleft)))
        (opright (and (not rcst?) (codegen-loc-to-x86opnd fs ffs lright))))

    ;;
    (let ((x86-op (cdr (assoc op `((+ . ,x86-addsd) (- . ,x86-subsd) (* . ,x86-mulsd) (/ . ,x86-divsd))))))

      (if (or (eq? dest opleft)
              (eq? dest opright))
          (begin (set! sdest dest)
                 (set! dest (x86-xmm1))))

      ;; Left operand
      (cond ((and leftint? lcst?)
               (immediate-to-xmm cgc dest (fixnum->flonum lleft)))
            (leftint?
               (if opt-nan-boxing
                   (begin
                     (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_32))
                     (x86-and cgc (x86-rax) opleft))
                   (begin
                     (x86-mov cgc (x86-rax) opleft)
                     (x86-sar cgc (x86-rax) (x86-imm-int 2))))
               (x86-cvtsi2sd cgc dest (x86-rax)))
            (lcst?
               (immediate-to-xmm cgc dest lleft))
            ((x86-mem? opleft)
               (x86-movsd cgc dest opleft))
            ;; Nothing to do, left operand is in a xmm register
            ((x86-xmm? opleft)
               (if (not (equal? dest opleft))
                   (x86-movsd cgc dest opleft)))
            (else ;; x86-reg
               (x86-movsd cgc dest (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opleft))))

      ;; Right operand
      (cond ((and rightint? rcst?)
               (immediate-to-xmm cgc (x86-xmm0) (fixnum->flonum lright))
               (x86-op cgc dest (x86-xmm0)))
            (rightint?
              (if opt-nan-boxing
                  (begin
                    (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_32))
                    (x86-and cgc (x86-rax) opright))
                  (begin
                    (x86-mov cgc (x86-rax) opright)
                    (x86-sar cgc (x86-rax) (x86-imm-int 2))))
              (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax))
              (x86-op cgc dest (x86-xmm0)))
            (rcst?
               (immediate-to-xmm cgc (x86-xmm0) lright)
               (x86-op cgc dest (x86-xmm0)))
            ((x86-mem? opright)
               (error "N3"))
            ;; Right operand is in a xmm register
            ((x86-xmm? opright)
               (x86-op cgc dest opright))
            (else ;; x86-reg
               (x86-op cgc dest (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opright)))))

    (if (and sdest
             (not (eq? dest sdest)))
        (x86-movsd cgc sdest dest))))

;; Gen code for arithmetic operation on float/float (also handles int/float and float/int)
(define (codegen-num-ff-box cgc fs ffs op reg lleft leftint? lright rightint? lcst? rcst?)

  (assert (not (and lcst? rcst?)) "Internal error")

  (let ((dest (codegen-reg-to-x86reg reg))
        (opleft (and (not lcst?) (codegen-loc-to-x86opnd fs ffs lleft)))
        (opright (and (not rcst?) (codegen-loc-to-x86opnd fs ffs lright))))

    ;; Alloc result flonum
    (if (not opt-nan-boxing)
        (gen-allocation-imm cgc STAG_FLONUM 8))

    ;;
    (let ((x86-op (cdr (assoc op `((+ . ,x86-addsd) (- . ,x86-subsd) (* . ,x86-mulsd) (/ . ,x86-divsd))))))

      ;; Left operand
      (cond ((and leftint? lcst?)
               (immediate-to-xmm cgc (x86-xmm0) (fixnum->flonum lleft)))
            (leftint?
               (if opt-nan-boxing
                   (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_32))
                          (x86-and cgc (x86-rax) opleft))
                   (begin (x86-mov cgc (x86-rax) opleft)
                          (x86-sar cgc (x86-rax) (x86-imm-int 2))))
               (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax)))
            (lcst?
               (immediate-to-xmm cgc (x86-xmm0) lleft))
            (else
               (if opt-nan-boxing
                   ;; nan boxing
                   (if (x86-mem? opleft)
                       (x86-movsd cgc (x86-xmm0) opleft)
                       (x86-movd/movq cgc (x86-xmm0) opleft))
                   ;; tagging
                   (begin
                     (if (and opt-stats (not opt-float-unboxing))
                         (gen-inc-slot cgc 'flunbox))
                     (if (x86-mem? opleft)
                         (begin
                           (x86-mov cgc (x86-rax) opleft)
                           (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                         (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) opleft)))))))

      ;; Right operand
      (cond ((and rightint? rcst?)
               (immediate-to-xmm cgc (x86-xmm1) (fixnum->flonum lright))
               (x86-op cgc (x86-xmm0) (x86-xmm1)))
            (rightint?
              (if opt-nan-boxing
                  (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_32))
                         (x86-and cgc (x86-rax) opright))
                  (begin (x86-mov cgc (x86-rax) opright)
                         (x86-sar cgc (x86-rax) (x86-imm-int 2))))
              (x86-cvtsi2sd cgc (x86-xmm1) (x86-rax))
              (x86-op cgc (x86-xmm0) (x86-xmm1)))
            (rcst?
               (immediate-to-xmm cgc (x86-xmm1) lright)
               (x86-op cgc (x86-xmm0) (x86-xmm1)))
            (else
               (if opt-nan-boxing
                   ;; nan boxing
                   (if (x86-mem? opright)
                       (x86-op cgc (x86-xmm0) opright)
                       (begin (x86-movd/movq cgc (x86-xmm1) opright)
                              (x86-op cgc (x86-xmm0) (x86-xmm1))))
                   ;; tagging
                   (begin
                     (if (and opt-stats (not opt-float-unboxing))
                         (gen-inc-slot cgc 'flunbox))
                     (if (x86-mem? opright)
                         (begin (x86-mov cgc (x86-rax) opright)
                                (x86-op cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                         (x86-op cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) opright)))))))

      (if opt-nan-boxing
          (x86-movd/movq cgc dest (x86-xmm0))
          (begin ;; Write number
                 (x86-movsd cgc (x86-mem -8 alloc-ptr) (x86-xmm0))
                 ;; Put
                 (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr)))))))

;;-----------------------------------------------------------------------------
;; N-ary comparison operators

(define (codegen-cmp-ii cgc fs ffs op reg lleft lright lcst? rcst? inline-if-cond?)

  (define-macro (if-inline expr)
    `(if inline-if-cond? #f ,expr))

  (assert (not (and lcst? rcst?)) "Internal error")

  (let* ((x86-op  (cdr (assoc op `((< . ,x86-jl) (> . ,x86-jg) (<= . ,x86-jle) (>= . ,x86-jge) (= . ,x86-je)))))
         (x86-iop (cdr (assoc op `((< . ,x86-jg) (> . ,x86-jl) (<= . ,x86-jge) (>= . ,x86-jle) (= . ,x86-je)))))
         (x86-inline-op  (cdr (assoc op `((< . ,x86-jge) (> . ,x86-jle) (<= . ,x86-jg) (>= . ,x86-jl) (= . ,x86-jne)))))
         (x86-inline-iop (cdr (assoc op `((< . ,x86-jle) (> . ,x86-jge) (<= . ,x86-jl) (>= . ,x86-jg) (= . ,x86-jne)))))
         (dest      (if-inline (codegen-reg-to-x86reg reg)))
         (label-end (if-inline (asm-make-label #f (new-sym 'label-end))))
         (opl (and (not lcst?) (codegen-loc-to-x86opnd fs ffs lleft)))
         (opr (and (not rcst?) (codegen-loc-to-x86opnd fs ffs lright)))
         (selop x86-op)
         (selinop x86-inline-op))

    ;; Handle cases like 1. 2. etc...
    (if (and lcst? (flonum? lleft))
        (set! lleft (##flonum->fixnum lleft)))
    (if (and rcst? (flonum? lright))
        (set! lright (##flonum->fixnum lright)))

    (let* ((invert
             (if opt-nan-boxing
                (codegen-cmp-ii-nb  cgc opl lcst? lleft opr rcst? lright)
                (codegen-cmp-ii-tag cgc opl lcst? lleft opr rcst? lright)))
           (selop   (if invert x86-iop selop))
           (selinop (if invert x86-inline-iop selinop)))

      (if inline-if-cond?
          selinop ;; Return x86-op
          (begin (x86-mov cgc dest (x86-imm-int (obj-encoding #t 33)))
                 (selop cgc label-end)
                 (x86-mov cgc dest (x86-imm-int (obj-encoding #f 34)))
                 (x86-label cgc label-end))))))


(define (codegen-cmp-ii-nb cgc opl lcst? lleft opr rcst? lright)

  (cond (lcst?
          (let ((opl (l32 opr))
                (opr (x86-imm-int lleft)))
            (x86-cmp cgc opl opr)
            #t))
        ;; cmp loc, imm
        (rcst?
          (cond ((x86-reg? opl)
                   (let ((r32 (x86-r64->r32 opl)))
                     (x86-cmp cgc r32 (x86-imm-int lright))
                     #f))
                (else
                   (error "NYI cmp-ii 2")))) ;; mov rax, imm; cmp rax, left
        (else
          (if (and (x86-mem? opl)
                   (x86-mem? opr))
            (begin
              (x86-mov cgc (x86-rax) opl)
              (x86-cmp cgc (x86-eax) (x86-r64->r32 opr)))
            (let ((l (l32 opl))
                  (r (l32 opr)))
              (x86-cmp cgc l r 32)))
          #f)))

(define (codegen-cmp-ii-tag cgc opl lcst? lleft opr rcst? lright)

    (define invert #f)

    (if lcst?
        (if (not (int32? (obj-encoding lleft 35)))
            (begin
              (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lleft 36)))
              (set! opl (x86-rax)))
            (begin
              (set! opl opr)
              (set! opr (x86-imm-int (obj-encoding lleft 37)))
              (set! invert #t))))

    (if rcst?
        (if (not (int32? (obj-encoding lright 38)))
            (begin
              (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lright 39)))
              (set! opr (x86-rax)))
            (set! opr (x86-imm-int (obj-encoding lright 40)))))

    (if (and (x86-mem? opl)
             (x86-mem? opr))
        (begin
          (x86-mov cgc (x86-rax) opl)
          (x86-cmp cgc (x86-rax) opr 64))
        (x86-cmp cgc opl opr 64))

    invert)

(define (codegen-cmp-ff cgc fs ffs op reg lleft leftint? lright rightint? lcst? rcst? inline-if-cond?)

  (define-macro (if-inline expr)
    `(if inline-if-cond? #f ,expr))

  (assert (not (and lcst? rcst?)) "Internal error")

  (let ((dest (if-inline (codegen-reg-to-x86reg reg)))
        (label-end (if-inline (asm-make-label #f (new-sym 'label-end))))
        (opleft  (and (not lcst?) (codegen-loc-to-x86opnd fs ffs lleft)))
        (opright (and (not rcst?) (codegen-loc-to-x86opnd fs ffs lright)))
        (x86-op (cdr (assoc op `((< . ,x86-jae) (> . ,x86-jbe) (<= . ,x86-ja) (>= . ,x86-jb) (= . ,x86-jne))))))

    (on-dynamic-mode
      ;; Left operand
      (cond ((and leftint? lcst?)
               (immediate-to-xmm cgc (x86-xmm0) (fixnum->flonum lleft))
               (set! opleft (x86-xmm0)))
            (leftint?
               (if opt-nan-boxing
                   (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_32))
                          (x86-and cgc (x86-rax) opleft))
                   (begin (x86-mov cgc (x86-rax) opleft)
                          (x86-shr cgc (x86-rax) (x86-imm-int 2))))
               (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax))
               (set! opleft (x86-xmm0)))
            (lcst?
               (immediate-to-xmm cgc (x86-xmm0) lleft)
               (set! opleft (x86-xmm0)))
            ((x86-mem? opleft)
               (error "N1"))
            ;; Nothing to do, left operand is in a xmm register
            ((x86-xmm? opleft)
               #f)
            (else ;; x86-reg
               (if opt-nan-boxing
                   (x86-movd/movq cgc (x86-xmm0) opleft)
                   (begin (if (and opt-stats (not opt-float-unboxing))
                              (gen-inc-slot cgc 'flunbox))
                          (x86-movsd cgc (x86-xmm0) (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opleft))))
               (set! opleft (x86-xmm0)))))

    (on-dynamic-mode
      ;; Right operand
      (cond ((and rightint? rcst?)
               (assert (not leftint?) "Internal error")
               (immediate-to-xmm cgc (x86-xmm1) (fixnum->flonum lright))
               (x86-comisd cgc opleft (x86-xmm1)))
            (rightint?
               (if opt-nan-boxing
                   (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_32))
                          (x86-and cgc (x86-rax) opright))
                   (begin (x86-mov cgc (x86-rax) opright)
                          (x86-shr cgc (x86-rax) (x86-imm-int 2))))
               (x86-cvtsi2sd cgc (x86-xmm1) (x86-rax))
               (x86-comisd cgc opleft (x86-xmm1)))
            (rcst?
               (immediate-to-xmm cgc (x86-xmm1) lright)
               (x86-comisd cgc opleft (x86-xmm1)))
            ((x86-mem? opright)
               (error "NYI a"))
            ((x86-xmm? opright)
               (x86-comisd cgc opleft opright))
            (else ;; x86-reg
               (if opt-nan-boxing
                   (begin (x86-movd/movq cgc (x86-xmm1) opright)
                          (x86-comisd cgc opleft (x86-xmm1)))
                   (begin (if (and opt-stats (not opt-float-unboxing))
                              (gen-inc-slot cgc 'flunbox))
                          (x86-comisd cgc opleft (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opright)))))))

    ;; NOTE: check that mlc-if patch is able to patch ieee jcc instructions (ja, jb, etc...)
    (if inline-if-cond?
        x86-op ;; return x86 op
        (on-dynamic-mode
          (begin (x86-mov cgc dest (x86-imm-int (obj-encoding #f 41)))
                 (x86-op cgc label-end)
                 (x86-mov cgc dest (x86-imm-int (obj-encoding #t 42)))
                 (x86-label cgc label-end))))))

;;-----------------------------------------------------------------------------
;; PRIMITIVES
;;-----------------------------------------------------------------------------

;;
;; mem-allocated?
(define (codegen-p-mem-allocated? cgc fs ffs op reg inlined-cond? lval)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs ffs lval))
        (label-next (asm-make-label #f (new-sym 'next_))))
    (x86-mov cgc (x86-rax) opval)
    (x86-and cgc (x86-rax) (x86-imm-int 1))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f 43)))
    (x86-je cgc label-next)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t 44)))
    (x86-label cgc label-next)))

;;
;; subtyped?
(define (codegen-p-subtyped? cgc fs ffs op reg inlined-cond? lval)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs ffs lval))
        (label-end (asm-make-label #f (new-sym 'subtyped_end_))))
    (x86-mov cgc (x86-rax) opval)
    (x86-and cgc (x86-rax) (x86-imm-int 3))
    (x86-cmp cgc (x86-rax) (x86-imm-int 1))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t 45)))
    (x86-je  cgc label-end)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f 46)))
    (x86-label cgc label-end)))

;; fixnum->flonum

(define (codegen-p-fixnum->flonum cgc fs ffs op reg inlined-cond? lval cst?)
  (assert (not cst?) "Internal error, unexpected cst operand")
  (if opt-float-unboxing
      (codegen-p-fixnum->flonum-nobox cgc fs ffs op reg inlined-cond? lval cst?)
      (codegen-p-fixnum->flonum-box cgc fs ffs op reg inlined-cond? lval cst?)))

(define (codegen-p-fixnum->flonum-nobox cgc fs ffs op reg inlined-cond? lval cst?)
  (define dest (codegen-freg-to-x86reg reg))
  (define opnd (codegen-loc-to-x86opnd fs ffs lval))

  (cond ((not opt-nan-boxing)
           (x86-mov cgc (x86-rax) opnd)
           (x86-sar cgc (x86-rax) (x86-imm-int 2)))
        ((x86-mem? opnd)
           (x86-mov cgc (x86-rax) opnd)
           (x86-movsx cgc (x86-rax) (x86-eax)))
        (else
           (x86-movsx cgc (x86-rax) (x86-r64->r32 opnd))))

  (x86-cvtsi2sd cgc dest (x86-rax)))

(define (codegen-p-fixnum->flonum-box cgc fs ffs op reg inlined-cond? lval cst?)
  (define dest (codegen-reg-to-x86reg reg))
  (define opnd (codegen-loc-to-x86opnd fs ffs lval))

  ;; Alloc result flonum
  (if (not opt-nan-boxing)
      (gen-allocation-imm cgc STAG_FLONUM 8))

  (cond ((not opt-nan-boxing)
           (x86-mov cgc (x86-rax) opnd)
           (x86-sar cgc (x86-rax) (x86-imm-int 2)))
        ((x86-mem? opnd)
           (x86-mov cgc (x86-rax) opnd)
           (x86-movsx cgc (x86-rax) (x86-eax)))
        (else
           (x86-movsx cgc (x86-rax) (x86-r64->r32 opnd))))

  (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax))

  (if opt-nan-boxing
      (x86-movd/movq cgc dest (x86-xmm0))
      (begin ;; Write number
             (x86-movsd cgc (x86-mem -8 alloc-ptr) (x86-xmm0))
             ;; Put
             (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr)))))
;;
;; box
(define (codegen-p-box cgc fs ffs op reg inlined-cond? lval cst?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (if cst?
                   (x86-imm-int (obj-encoding lval 47))
                   (codegen-loc-to-x86opnd fs ffs lval))))
    (gen-allocation-imm cgc STAG_MOBJECT 8)
    (if (or (x86-mem? opval)
            (x86-imm? opval))
        (begin
          (x86-mov cgc (x86-rax) opval)
          (set! opval (x86-rax))))
    (x86-mov cgc (x86-mem (+ -16 OFFSET_BOX) alloc-ptr) opval)
    (if opt-nan-boxing
        (begin (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
               (x86-lea cgc dest (x86-mem -16 alloc-ptr (x86-rax))))
        (x86-lea cgc dest (x86-mem (+ -16 TAG_MEMOBJ) alloc-ptr)))))

;;
;; unbox
(define (codegen-p-unbox cgc fs ffs op reg inlined-cond? lbox cst?)
  (assert (not cst?) "Internal error, box can't be cst")
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opbox (codegen-loc-to-x86opnd fs ffs lbox)))
    (if (x86-mem? opbox)
        (begin (x86-mov cgc (x86-rax) opbox)
               (set! opbox (x86-rax))))

    (if opt-nan-boxing
        (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
               (x86-and cgc (x86-rax) opbox)
               (x86-mov cgc dest (x86-mem OFFSET_BOX (x86-rax))))
        (x86-mov cgc dest (x86-mem (- OFFSET_BOX TAG_MEMOBJ) opbox)))))

;;
;; box-set!

(define (codegen-p-set-box cgc fs ffs op reg inlined-cond? lbox lval cst-box? cst-val?)
  (if opt-nan-boxing
      (codegen-p-set-box-nan cgc fs ffs op reg inlined-cond? lbox lval cst-box? cst-val?)
      (codegen-p-set-box-tag cgc fs ffs op reg inlined-cond? lbox lval cst-box? cst-val?)))

(define (codegen-p-set-box-nan cgc fs ffs op reg inlined-cond? lbox lval cst-box? cst-val?)

  (assert (not cst-box?) "Internal error, unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opbox (codegen-loc-to-x86opnd fs ffs lbox))
        (opval (and (not cst-val?) (codegen-loc-to-x86opnd fs ffs lval))))

    (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
    (x86-and cgc (x86-rax) opbox)

    (cond (cst-val?
            (x86-mov cgc dest (x86-imm-int (obj-encoding lval 48)))
            (set! opval dest))
          ((x86-mem? opval)
            (x86-mov cgc dest opval)
            (set! opval dest)))

    (x86-mov cgc (x86-mem 8 (x86-rax)) opval)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

(define (codegen-p-set-box-tag cgc fs ffs op reg inlined-cond? lbox lval cst-box? cst-val?)

  (assert (not cst-box?) "Internal error, unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opbox (codegen-loc-to-x86opnd fs ffs lbox))
        (opval (if cst-val?
                   (x86-imm-int (obj-encoding lval 49))
                   (codegen-loc-to-x86opnd fs ffs lval)))
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
    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))

    (if use-selector?
        (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 50))))))

;;
;; cons
(define (codegen-p-cons cgc fs ffs op reg inlined-cond? lcar lcdr car-cst? cdr-cst?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opcar (and (not car-cst?) (codegen-loc-to-x86opnd fs ffs lcar)))
        (opcdr (and (not cdr-cst?) (codegen-loc-to-x86opnd fs ffs lcdr))))
    (gen-allocation-imm cgc STAG_PAIR 16)
    (cond (car-cst?
            (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lcar 51)))
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax)))
          ((x86-mem? opcar)
            (x86-mov cgc (x86-rax) opcar)
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) (x86-rax)))
          (else
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CAR) alloc-ptr) opcar)))
    (cond (cdr-cst?
            (let ((encoding (obj-encoding lcdr 52)))
              (if (codegen-is-imm-64? encoding)
                  (begin (x86-mov cgc (x86-rax) (x86-imm-int encoding))
                         (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-rax)))
                  (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-imm-int (obj-encoding lcdr 53)) 64))))
          ((x86-mem? opcdr)
            (x86-mov cgc (x86-rax) opcdr)
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) (x86-rax)))
          (else
            (x86-mov cgc (x86-mem (+ -24 OFFSET_PAIR_CDR) alloc-ptr) opcdr)))

    ;; mov dest, MEM_MASK
    ;; lea dest, [alloc-ptr+dest]

    ;; Tag pair
    (if opt-nan-boxing
        (begin
          (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
          (x86-lea cgc dest (x86-mem -24 alloc-ptr (x86-rax))))
        (x86-lea cgc dest (x86-mem (+ -24 TAG_PAIR) alloc-ptr)))))

;;
;; list
;; csts?/locs is a list of pair (cst? . loc) for each 'list' argument
(define (codegen-p-list cgc fs ffs rloc csts?/locs)

  (define nb-els (length csts?/locs))
  (assert (ctx-loc-is-register? rloc) "Internal error")
  (assert (not (null? csts?/locs))    "Internal error")

  (gen-allocation-imm cgc STAG_PAIR (- (* nb-els 3 8) 8))

  (let loop ((n (- (length csts?/locs) 1))
             (csts?/locs csts?/locs))
    (if (null? (cdr csts?/locs))
        ;; Last element
        (let* ((cst? (caar csts?/locs))
               (loc  (cdar csts?/locs))
               (pair-offset (* -3 8 n)))
          ;; Write header
          (let ((imm (mem-header 16 STAG_PAIR)))
            (if (codegen-is-imm-64? imm)
                (begin (x86-mov cgc (x86-rax) (x86-imm-int imm))
                       (x86-mov cgc (x86-mem (- pair-offset 24) alloc-ptr) (x86-rax)))
                (x86-mov cgc (x86-mem (- pair-offset 24) alloc-ptr) (x86-imm-int imm) 64)))
          ;; Write null in cdr
          (let ((imm (obj-encoding '() 54)))
            (if (codegen-is-imm-64? imm)
                (begin (x86-mov cgc (x86-rax) (x86-imm-int imm))
                       (x86-mov cgc (x86-mem (- pair-offset 16) alloc-ptr) (x86-rax)))
                (x86-mov cgc (x86-mem (- pair-offset 16) alloc-ptr) (x86-imm-int imm) 64)))
          ;; Write value in car
          (let ((opnd
                  (if cst?
                      (x86-imm-int (obj-encoding loc 55))
                      (codegen-loc-to-x86opnd fs ffs loc))))
            (if (or (x86-mem? opnd)
                    (x86-imm? opnd))
                (begin
                  (x86-mov cgc (x86-rax) opnd)
                  (set! opnd (x86-rax))))
            (x86-mov cgc (x86-mem (- pair-offset  8) alloc-ptr) opnd))

          ;; Load first pair in dest
          (if opt-nan-boxing
              (let ((dest (codegen-reg-to-x86reg rloc))
                    (offset (* nb-els 3 -8)))
                (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
                (x86-lea cgc dest (x86-mem offset alloc-ptr (x86-rax))))
              (let ((dest (codegen-reg-to-x86reg rloc))
                    (offset (+ (* nb-els 3 -8) TAG_PAIR)))
                (x86-lea cgc dest (x86-mem offset alloc-ptr)))))

        ;; Not last element
        (let* ((cst? (caar csts?/locs))
               (loc  (cdar csts?/locs))
               (pair-offset (* -3 8 n)))
          ;; Write header
          (x86-mov cgc (x86-mem (- pair-offset 24) alloc-ptr) (x86-imm-int (mem-header 16 STAG_PAIR)) 64)
          ;; Write encoded next pair in cdr
          (if opt-nan-boxing
              (begin (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
                     (x86-lea cgc (x86-rax) (x86-mem pair-offset alloc-ptr (x86-rax))))
              (x86-lea cgc (x86-rax) (x86-mem (+ pair-offset TAG_PAIR) alloc-ptr)))
          (x86-mov cgc (x86-mem (- pair-offset 16) alloc-ptr) (x86-rax))
          ;; Write value in car
          (let ((opnd
                  (if cst?
                      (x86-imm-int (obj-encoding loc 56))
                      (codegen-loc-to-x86opnd fs ffs loc))))
            (if (or (x86-imm? opnd)
                    (x86-mem? opnd))
                (begin (x86-mov cgc (x86-rax) opnd)
                       (set! opnd (x86-rax))))
            (x86-mov cgc (x86-mem (- pair-offset  8) alloc-ptr) opnd))
          ;; Continue
          (loop (- n 1) (cdr csts?/locs))))))


;;
;; quotient/modulo/remainder
(define (codegen-p-binop cgc fs ffs op label-div0 reg lleft lright lcst? rcst?)

  (assert (not (and lcst? rcst?)) "Internal error")

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (lopnd (if lcst?
                    (x86-imm-int (obj-encoding lleft 57))
                    (codegen-loc-to-x86opnd fs ffs lleft)))
         (ropnd (if rcst?
                    (x86-imm-int (obj-encoding lright 58))
                    (codegen-loc-to-x86opnd fs ffs lright)))
         (save-rdx? (neq? dest (x86-rdx)))
         (restore-fn #f)
         (selector-used #f))

    ;; If ropnd is imm or rdx, use selector
    (if (or (x86-imm? ropnd)
            (x86-mem? ropnd)
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

    ;;
    (if opt-nan-boxing
        (codegen-p-binop-nan cgc op dest lcst? lleft lopnd ropnd label-div0)
        (codegen-p-binop-tag cgc op dest lcst? lleft lopnd ropnd label-div0))

    ;; Restore rdx
    (if restore-fn
        (restore-fn cgc))

    ;; Restore selector
    (if selector-used
        (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 59))))))


(define (codegen-p-binop-nan cgc op dest lcst? lleft lopnd ropnd label-div0)

    (x86-cmp cgc (x86-r64->r32 ropnd) (x86-imm-int 0))
    (x86-je cgc label-div0)

    (if lcst?
        (x86-mov cgc (x86-eax) (x86-imm-int lleft))
        (x86-mov cgc (x86-eax) (l32 lopnd)))

    (x86-cdq cgc) ;; eax -> edx:eax
    (x86-idiv cgc (x86-r64->r32 ropnd))

    (cond ((eq? op 'quotient)
            (x86-mov cgc dest (x86-imm-int (to-64-value NB_MASK_FIX)))
            (x86-or  cgc dest (x86-rax)))
          ((eq? op 'remainder)
            (x86-mov  cgc dest (x86-imm-int (to-64-value NB_MASK_FIX)))
            (x86-or   cgc dest (x86-rdx)))
          ((eq? op 'modulo)
            (x86-mov  cgc (x86-rax) (x86-rdx))
            (x86-add  cgc (x86-eax) (x86-r64->r32 ropnd))
            (x86-cdqe cgc)
            (x86-cqo  cgc)
            (x86-idiv cgc (x86-r64->r32 ropnd))
            (x86-mov  cgc dest (x86-imm-int (to-64-value NB_MASK_FIX)))
            (x86-or   cgc dest (x86-rdx)))))

(define (codegen-p-binop-tag cgc op dest lcst? lleft lopnd ropnd label-div0)

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
        (x86-shl cgc ropnd (x86-imm-int 2))))

;;
;; odd? & even?
(define (codegen-p-odd?-even? cgc fs ffs op reg inlined-cond? lval val-cst?)

  (define dest (codegen-reg-to-x86reg reg))
  (define opnd (codegen-loc-to-x86opnd fs ffs lval))
  (define label-end (asm-make-label #f (new-sym 'odd-even)))
  (define bool (eq? op 'even?))

  (assert (not val-cst?) "Internal error. Unexpected constant")

  (x86-mov cgc (x86-rax) opnd)
  (if opt-nan-boxing
      (x86-and cgc (x86-rax) (x86-imm-int 1))  ;;b1
      (x86-and cgc (x86-rax) (x86-imm-int 4))) ;; b100
  (x86-mov cgc dest (x86-imm-int (obj-encoding bool)))
  (x86-je cgc label-end)
  (x86-mov cgc dest (x86-imm-int (obj-encoding (not bool))))
  (x86-label cgc label-end))

;;
;; sin
(define (codegen-p-native-fl cgc fs ffs op reg inlined-cond? lval val-cst?)
  (assert (not val-cst?) "Internal error. Unexpected constant")
  (let* ((addr (cond ((eq? op 'sin)  (get-sin-addr))
                     ((eq? op 'cos)  (get-cos-addr))
                     ((eq? op 'atan) (get-atan-addr))
                     (else (error "NYI case"))))
         (label (asm-make-label #f (new-sym op) addr)))
    (if opt-float-unboxing
        (codegen-p-native-fl-nobox cgc fs ffs reg lval label)
        (codegen-p-native-fl-box   cgc fs ffs reg lval label))))

(define (codegen-p-native-fl-nobox cgc fs ffs reg lval label)
  (let ((dest (codegen-freg-to-x86reg reg))
        (opnd (codegen-loc-to-x86opnd fs ffs lval)))
    (ppush-pop-xmm
      cgc
      (set-sub regalloc-fregs (list dest))
      (lambda (cgc)
        (ppush-pop-regs
          cgc
          c-caller-save-regs
          (lambda (cgc)
            (cond ((x86-mem? opnd) (error "NYI case primitive sin"))
                  ((x86-xmm? opnd) (x86-movsd cgc (x86-xmm0) opnd))
                  (else (error "NYI case primitive sin")))
            ;;
            (x86-mov  cgc (x86-rax) (x86-rsp)) ;; align stack-pointer for C call
            (x86-and  cgc (x86-rsp) (x86-imm-int -16))
            (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
            (x86-ppush cgc (x86-rax))
            (x86-pcall cgc label)
            (x86-ppop cgc (x86-rsp))
            ;;
            (x86-movsd cgc dest (x86-xmm0))))))))

(define (codegen-p-native-fl-box cgc fs ffs reg lval label)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opnd (codegen-loc-to-x86opnd fs ffs lval)))

    ;; Alloc result flonum
    (if (not opt-nan-boxing)
        (gen-allocation-imm cgc STAG_FLONUM 8))

        (ppush-pop-regs
          cgc
          (set-sub c-caller-save-regs (list dest))
          (lambda (cgc)
            (cond ((x86-mem? opnd) (error "NYI case primitive sin"))
                  ((x86-xmm? opnd) (error "NYI case primitive sin"))
                  (else
                    (if opt-nan-boxing
                        (x86-movd/movq cgc (x86-xmm0) opnd)
                        (begin (if (and opt-stats (not opt-float-unboxing))
                                   (gen-inc-slot cgc 'flunbox))
                               (x86-movsd cgc (x86-xmm0) (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opnd))))))
            ;;
            (x86-mov  cgc (x86-rax) (x86-rsp)) ;; align stack-pointer for C call
            (x86-and  cgc (x86-rsp) (x86-imm-int -16))
            (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
            (x86-ppush cgc (x86-rax))
            (x86-pcall cgc label)
            (x86-ppop cgc (x86-rsp))
            ;;
            (if opt-nan-boxing
                (x86-movd/movq cgc dest (x86-xmm0))
                (begin ;; Write number
                       (x86-movsd cgc (x86-mem -8 alloc-ptr) (x86-xmm0))
                       ;; Put
                       (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))))))))

;;
;; sqrt
(define (codegen-p-sqrt cgc fs ffs op reg inlined-cond? lval val-cst?)
  (assert (not val-cst?) "Internal error, unexpected cst operand")
  (if opt-float-unboxing
      (codegen-p-sqrt-nobox cgc fs ffs reg lval)
      (codegen-p-sqrt-box   cgc fs ffs reg lval)))

(define (codegen-p-sqrt-nobox cgc fs ffs reg lval)
  (define dest (codegen-freg-to-x86reg reg))
  (define opnd (codegen-loc-to-x86opnd fs ffs lval))

  (cond ((and (x86-reg? opnd)
              (x86-xmm? opnd))
           (x86-sqrtsd cgc dest opnd))
        (else
           (error "NYI case sqrt"))))

(define (codegen-p-sqrt-box cgc fs ffs reg lval)
  (define dest (codegen-reg-to-x86reg reg))
  (define opnd (codegen-loc-to-x86opnd fs ffs lval))

  ;; Alloc result flonum
  (if (not opt-nan-boxing)
      (gen-allocation-imm cgc STAG_FLONUM 8))

  (cond ((x86-reg? opnd) ;; general register
           (if opt-nan-boxing
               (begin (x86-movd/movq cgc (x86-xmm1) opnd)
                      (x86-sqrtsd cgc (x86-xmm0) (x86-xmm1)))
               (begin (if (and opt-stats (not opt-float-unboxing))
                          (gen-inc-slot cgc 'flunbox))
                      (x86-sqrtsd cgc (x86-xmm0) (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opnd)))))
        (else
           (error "NYI case sqrt")))

  (if opt-nan-boxing
      (x86-movd/movq cgc dest (x86-xmm0))
      (begin ;; Write number
             (x86-movsd cgc (x86-mem -8 alloc-ptr) (x86-xmm0))
             ;; Put
             (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr)))))

;;
;; not
(define (codegen-p-not cgc fs ffs op reg inlined-cond? lval val-cst?)

  (define dest (codegen-reg-to-x86reg reg))

  (cond
    ((and val-cst? lval)
      (x86-mov cgc dest (x86-imm-int (obj-encoding #f 60))))
    (val-cst?
      (x86-mov cgc dest (x86-imm-int (obj-encoding #t 61))))
    (else
      (let ((label-done
              (asm-make-label cgc (new-sym 'done)))
            (dest (codegen-reg-to-x86reg reg))
            (opval (codegen-loc-to-x86opnd fs ffs lval)))

        (if (eq? dest opval)
            (begin
              (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f 62)))
              (x86-cmp cgc opval (x86-rax))
              (x86-mov cgc dest (x86-imm-int (obj-encoding #f 63))))
            (begin
              (x86-mov cgc dest (x86-imm-int (obj-encoding #f 64)))
              (x86-cmp cgc opval dest)))

        (x86-jne cgc label-done)
        (x86-mov cgc dest (x86-imm-int (obj-encoding #t 65)))
        (x86-label cgc label-done)))))

;;
;; eq?
(define (codegen-p-eq? cgc fs ffs op reg inlined-cond? lleft lright lcst? rcst?)

  ;; (eq? cst1 cst2) is handled by code expansion
  (assert (not (and lcst? rcst?)) "Internal error (codegen-eq?)")

  (let ((dest (and reg (codegen-reg-to-x86reg reg)))
        (label-done (asm-make-label #f (new-sym 'eq?_end_)))
        (lopnd (and (not lcst?) (codegen-loc-to-x86opnd fs ffs lleft)))
        (ropnd (and (not rcst?) (codegen-loc-to-x86opnd fs ffs lright))))

   (cond (lcst?
          ;; Check for imm64
          (if (codegen-is-imm-64? (obj-encoding lleft 66))
              (begin (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lleft 67)))
                     (x86-cmp cgc (x86-rax) ropnd))
              (x86-cmp cgc ropnd (x86-imm-int (obj-encoding lleft 68)))))
         (rcst?
          (if (codegen-is-imm-64? (obj-encoding lright 69))
              (begin (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lright 70)))
                     (x86-cmp cgc (x86-rax) lopnd))
              (x86-cmp cgc lopnd (x86-imm-int (obj-encoding lright 71)))))
         (else
          (if (and (x86-mem? lopnd)
                   (x86-mem? ropnd))
              (begin (x86-mov cgc (x86-rax) lopnd)
                     (set! lopnd (x86-rax))))
          (x86-cmp cgc lopnd ropnd)))

   (if (not inlined-cond?)
       (begin
         (x86-mov cgc dest (x86-imm-int (obj-encoding #t 72)))
         (x86-je  cgc label-done)
         (x86-mov cgc dest (x86-imm-int (obj-encoding #f 73)))
         (x86-label cgc label-done)))))

;;
;; car/cdr
(define (codegen-p-cxr cgc fs ffs op reg inlined-cond? lval cst?)

  (assert (not cst?) "Internal error")

  (let ((offset
          (if (eq? op 'car)
              (- OFFSET_PAIR_CAR TAG_PAIR)
              (- OFFSET_PAIR_CDR TAG_PAIR)))
        (dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs ffs lval)))


    (if opt-nan-boxing
        (begin
          (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_VALUE_48)))
          (x86-and cgc (x86-rax) opval)
          (x86-mov cgc dest (x86-mem (+ offset TAG_PAIR) (x86-rax))))
        (begin
          (if (x86-mem? opval)
              (begin (x86-mov cgc (x86-rax) opval)
                     (set! opval (x86-rax))))
          (x86-mov cgc dest (x86-mem offset opval))))))

;;
;; symbol->string
(define (codegen-p-symbol->string cgc fs ffs op reg inlined-con? lsym sym-cst?)
  (if opt-nan-boxing
      (codegen-p-symbol->string-nan cgc fs ffs op reg inlined-con? lsym sym-cst?)
      (codegen-p-symbol->string-tag cgc fs ffs op reg inlined-con? lsym sym-cst?)))

(define (codegen-p-symbol->string-nan cgc fs ffs op reg inlined-cond? lsym sym-cst?)

  (assert (not sym-cst?) "Internal error. Unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opsym (codegen-loc-to-x86opnd fs ffs lsym)))

    ;; Get string scheme object from symbol representation
    (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
    (x86-and cgc (x86-rax) opsym)
    (x86-mov cgc dest (x86-mem 8 (x86-rax)))
    (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
    (x86-lea cgc dest (x86-mem -1 dest (x86-rax)))))

(define (codegen-p-symbol->string-tag cgc fs ffs op reg inlined-cond? lsym sym-cst?)

  (assert (not sym-cst?) "Internal error. Unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opsym (codegen-loc-to-x86opnd fs ffs lsym)))

    (if (x86-mem? opsym)
        (begin (x86-mov cgc (x86-rax) opsym)
               (set! opsym (x86-rax))))

    ;; Get string scheme object from symbol representation
    (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) opsym))))

;;
;; string->symbol
(define (codegen-p-string->symbol cgc fs ffs op reg inlined-con? lstr sym-cst?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd fs ffs lstr)))
    (x86-upush cgc opstr)
    (x86-pcall cgc label-gambit-str-to-sym-handler)
    (x86-upop cgc dest)
    (if opt-nan-boxing
        (begin (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
               (x86-lea cgc dest (x86-mem (- TAG_MEMOBJ) dest (x86-rax)))))))


;;
;; set-car!/set-cdr!
(define (codegen-p-set-cxr! cgc fs ffs op reg inlined-cond? lpair lval pair-cst? val-cst?)
  (assert (not pair-cst?) "Internal error, unexpected cst operand")
  (let ((dest (codegen-reg-to-x86reg reg))
        (oppair (codegen-loc-to-x86opnd fs ffs lpair))
        (opval (and (not val-cst?) (codegen-loc-to-x86opnd fs ffs lval))))

    (if opt-nan-boxing
        (codegen-p-set-cxr!-nan cgc op dest oppair opval lval val-cst?)
        (codegen-p-set-cxr!-tag cgc op dest oppair opval lval val-cst?))

    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

(define (codegen-p-set-cxr!-nan cgc op dest oppair opval lval val-cst?)
  (define offset (if (eq? op 'set-car!) OFFSET_PAIR_CAR OFFSET_PAIR_CDR))
  (define saved-dest #f)

  (if (and (eq? dest opval)
           (not val-cst?))
      (begin (set! saved-dest dest)
             (set! dest selector-reg)))

  (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
  (x86-mov cgc dest oppair)
  (x86-and cgc dest (x86-rax))

  (cond
    (val-cst?
      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lval 74)))
      (x86-mov cgc (x86-mem offset dest) (x86-rax)))
    ((ctx-loc-is-memory? lval)
     (x86-mov cgc (x86-rax) opval)
     (x86-mov cgc (x86-mem offset dest) (x86-rax)))
    (else
     (x86-mov cgc (x86-mem offset dest) opval)))

  (if saved-dest
      (begin (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0)))
             (set! dest saved-dest)))
  (x86-mov cgc dest (x86-imm-int (obj-encoding #!void))))



(define (codegen-p-set-cxr!-tag cgc op dest oppair opval lval val-cst?)
  (define offset (if (eq? op 'set-car!)
                     (- OFFSET_PAIR_CAR TAG_PAIR)
                     (- OFFSET_PAIR_CDR TAG_PAIR)))

  (if (x86-mem? oppair)
      (begin (x86-mov cgc (x86-rax) oppair)
             (set! oppair (x86-rax))))

  (cond
    (val-cst?
      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lval 75)))
      (x86-mov cgc (x86-mem offset oppair) (x86-rax)))
    ((ctx-loc-is-memory? lval)
     (x86-mov cgc dest opval)
     (x86-mov cgc (x86-mem offset oppair) dest))
    (else
      (x86-mov cgc (x86-mem offset oppair) opval)))

  (x86-mov cgc dest (x86-imm-int (obj-encoding #!void))))

;;
;; eof-object?
(define (codegen-p-eof-object? cgc fs ffs op reg inlined-cond? lval cst?)

  (define dest (codegen-reg-to-x86reg reg))

  (assert (not cst?) "Internal error")

  (let ((label-end (asm-make-label #f (new-sym 'label-end)))
        (opval (codegen-loc-to-x86opnd fs ffs lval)))
    ;; ENCODING_EOF is a a imm64 and cmp r/m64, imm32 is not possible
    ;; then use a r64
    (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value (if opt-nan-boxing NB_ENCODED_EOF ENCODING_EOF))))
    (x86-cmp cgc opval (x86-rax))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f 78)))
    (x86-jne cgc label-end)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t 79)))
    (x86-label cgc label-end)))

;;
;; char->integer/integer->char
(define (codegen-p-ch<->int cgc fs ffs op reg inlined-cond? lval #!optional cst?)

  (let ((dest (codegen-reg-to-x86reg reg)))

   (cond
     ((and cst? (eq? op 'integer->char))
      (x86-mov cgc dest (x86-imm-int (obj-encoding (integer->char lval) 80))))
     ((and cst? (eq? op 'char->integer))
      (x86-mov cgc dest (x86-imm-int (obj-encoding (char->integer lval) 81))))
     (else
        (let ((opval (codegen-loc-to-x86opnd fs ffs lval)))

          (if (neq? dest opval)
              (x86-mov cgc dest opval))

          (if opt-nan-boxing
              (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_BIT_CHA))
                     (if (eq? op 'char->integer)
                         (x86-xor cgc dest (x86-rax))
                         (x86-or  cgc dest (x86-rax))))
              (if (eq? op 'char->integer)
                  (x86-xor cgc dest (x86-imm-int TAG_SPECIAL))
                  (x86-or  cgc dest (x86-imm-int TAG_SPECIAL)))))))))

;;
;; make-string
(define (codegen-p-make-string cgc fs ffs op reg inlined-cond? llen lval len-cst? val-cst?)

  (cond ((and opt-nan-boxing len-cst?)
          (codegen-p-make-string-imm-nan cgc fs ffs reg llen lval val-cst?))
        (opt-nan-boxing
          (codegen-p-make-string-opn-nan cgc fs ffs reg llen lval val-cst?))
        (len-cst?
          (codegen-p-make-string-imm-tag cgc fs ffs reg llen lval val-cst?))
        (else
          (codegen-p-make-string-opn-tag cgc fs ffs reg llen lval val-cst?))))

;;
;; string with cst len
(define (codegen-p-make-string-imm-nan cgc fs ffs reg llen lval val-cst?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval 82))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-string-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-string-end))))

    (gen-allocation-imm cgc STAG_STRING (* 4 llen))

    (x86-mov cgc (x86-rax) (x86-imm-int (* -4 llen)))
    (if val-cst?
        (x86-mov cgc selector-reg (x86-imm-int (char->integer lval)))
        (x86-mov cgc selector-reg opval))
    (x86-label cgc label-loop)
    (x86-cmp cgc (x86-rax) (x86-imm-int 0))
    (x86-je cgc label-end)

      (let ((memop (x86-mem 0 alloc-ptr (x86-rax))))
        (x86-mov cgc memop selector-reg-32)
        (x86-add cgc (x86-rax) (x86-imm-int 4))
        (x86-jmp cgc label-loop))

    (x86-label cgc label-end)
    (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
    (x86-lea cgc dest (x86-mem (- (* -4 llen) 8) alloc-ptr (x86-rax)))
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 83)))))

;;
;; string with cst len
(define (codegen-p-make-string-imm-tag cgc fs ffs reg llen lval val-cst?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval 82))
                    (codegen-loc-to-x86opnd fs ffs lval)))
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
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 83)))))

;;
;; string with !cst len
(define (codegen-p-make-string-opn-nan cgc fs ffs reg llen lval val-cst?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd fs ffs llen))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval 84))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-string-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-string-end))))

    (x86-mov cgc (x86-eax) (l32 oplen))
    (x86-shl cgc (x86-rax) (x86-imm-int 2))
    (gen-allocation-rt cgc STAG_STRING (x86-rax))

    ;; str addr in rax
    (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_48))
    (x86-and cgc (x86-rax) selector-reg)

    ;; len in selector
    (x86-mov cgc selector-reg oplen)
    (x86-mov cgc selector-reg-32 selector-reg-32)

    ;; init val in dest
    (if val-cst?
        (x86-mov cgc dest (x86-imm-int (char->integer lval)))
        (x86-mov cgc dest opval))
    (x86-label cgc label-loop)
    (x86-cmp cgc selector-reg (x86-imm-int 0))
    (x86-je cgc label-end)

      (let ((memop (x86-mem 4 (x86-rax) selector-reg 2)))
        (x86-mov cgc memop (l32 dest))
        (x86-sub cgc selector-reg (x86-imm-int 1))
        (x86-jmp cgc label-loop))

    (x86-label cgc label-end)
    (x86-mov cgc dest (x86-imm-int (to-64-value NB_MASK_MEM)))
    (x86-or  cgc dest (x86-rax))
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 85)))))


;;
;; string with !cst len
(define (codegen-p-make-string-opn-tag cgc fs ffs reg llen lval val-cst?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd fs ffs llen))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval 84))
                    (codegen-loc-to-x86opnd fs ffs lval)))
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
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 85)))))

;;
;; make-vector
(define (codegen-p-make-vector cgc fs ffs op reg inlined-cond? llen lval cst-len? cst-val?)
  (cond ((and cst-len?
              (mem-still-required? (* 8 llen)))
           (codegen-p-make-vector-imm-sti cgc fs ffs reg llen lval cst-val?))
        (cst-len?
           (codegen-p-make-vector-imm cgc fs ffs reg llen lval cst-val?))
        (else
           (codegen-p-make-vector-opn cgc fs ffs reg llen lval cst-val?))))

;; make-vector with cst len
(define (codegen-p-make-vector-imm cgc fs ffs reg llen lval cst-val?)
  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (x86-imm-int (obj-encoding llen 86)))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval 87))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    ;; Alloc vector
    (gen-allocation-imm cgc STAG_VECTOR (* 8 llen))
    ;; Loop counter
    (x86-mov cgc selector-reg (x86-imm-int (* -8 llen)))
    ;; Loop
    (if cst-val?
        (x86-mov cgc dest (x86-imm-int (obj-encoding lval 88))))
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
    (if opt-nan-boxing
        (begin (x86-lea cgc dest (x86-mem (* (+ llen 1) -8) alloc-ptr))
               (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
               (x86-or cgc dest (x86-rax)))
        (x86-lea cgc dest (x86-mem (+ (* (+ llen 1) -8) TAG_MEMOBJ) alloc-ptr)))
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 89)))))

;; make-vector with cst len (still)
(define (codegen-p-make-vector-imm-sti cgc fs ffs reg llen lval cst-val?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (x86-imm-int (obj-encoding llen 90)))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval 91))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

  ;; Alloc vector
  (gen-allocation-imm-sti cgc STAG_VECTOR (* 8 llen))
  ;; Loop counter
  (x86-mov cgc selector-reg (x86-imm-int (* 8 llen)))
  ;; Loop
  (if cst-val?
     (x86-mov cgc dest (x86-imm-int (obj-encoding lval 92))))
  (x86-label cgc label-loop)
  (x86-cmp cgc selector-reg (x86-imm-int 0))
  (x86-je cgc label-end)

    (let ((memop (x86-mem (- TAG_MEMOBJ) (x86-rax) selector-reg)))
      (if cst-val?
          (x86-mov cgc memop dest)
          (x86-mov cgc memop opval))
      (x86-sub cgc selector-reg (x86-imm-int 8))
      (x86-jmp cgc label-loop))

  (x86-label cgc label-end)
  (if opt-nan-boxing
      (begin (x86-mov cgc dest (x86-imm-int (to-64-value (- NB_MASK_MEM TAG_MEMOBJ))))
             (x86-lea cgc dest (x86-mem 0 (x86-rax) dest)))
      (x86-mov cgc dest (x86-rax)))
  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 93)))))

;; make-vector with !cst len
(define (codegen-p-make-vector-opn cgc fs ffs reg llen lval cst-val?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd fs ffs llen))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval 94))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    (define save #f)
    (define (get-saved-reg)
      alloc-ptr)

    (if opt-nan-boxing
        (begin

            (x86-mov cgc (x86-eax) (l32 oplen))
            (x86-shl cgc (x86-rax) (x86-imm-int 3))
            (gen-allocation-rt cgc STAG_VECTOR (x86-rax))

            ;; if val is cst, mem, or eq dest
            ;; we need another free register
            (if (or cst-val?
                    (x86-mem? opval)
                    (eq? dest opval))
                (begin
                  (set! save (get-saved-reg))
                  (x86-ppush cgc save)
                  (x86-mov cgc save opval)
                  (set! opval save)))

            ;;
            (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_48))
            (x86-and cgc selector-reg (x86-rax)) ;; vector addr in selector

            (x86-mov cgc (l32 dest) (l32 oplen)) ;; vector len in dest reg
            (x86-label cgc label-loop)
            (x86-cmp cgc dest (x86-imm-int 0))
            (x86-je cgc label-end)

              (x86-mov cgc (x86-mem 0 selector-reg dest 3) opval)
              (x86-sub cgc dest (x86-imm-int 1))
              (x86-jmp cgc label-loop)

            (x86-label cgc label-end)
            (if save
                (x86-ppop cgc save))
            (x86-mov cgc dest (x86-rax))
            (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 95))))

        (begin
            (x86-mov cgc (x86-rax) oplen)
            (x86-shl cgc (x86-rax) (x86-imm-int 1))
            (gen-allocation-rt cgc STAG_VECTOR (x86-rax))

            ;; Loop
            (x86-mov cgc selector-reg oplen)
            (x86-shl cgc selector-reg (x86-imm-int 1))
            (if cst-val?
                (x86-mov cgc dest (x86-imm-int (obj-encoding lval 96))))
            (x86-label cgc label-loop)
            (x86-cmp cgc selector-reg (x86-imm-int 0))
            (x86-je cgc label-end)

              (let ((memop (x86-mem (- TAG_MEMOBJ) (x86-rax) selector-reg)))

                (cond (cst-val?
                        (x86-mov cgc memop dest))
                      ((x86-mem? opval)
                        (x86-mov cgc dest opval)
                        (x86-mov cgc memop dest))
                      (else
                        (x86-mov cgc memop opval)))

                (x86-sub cgc selector-reg (x86-imm-int 8))
                (x86-jmp cgc label-loop))

            ;;
            (x86-label cgc label-end)
            (x86-mov cgc dest (x86-rax))
            (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 97)))))))

;;
;; make-f64vector
(define (codegen-p-make-f64vector cgc fs ffs op reg inlined-cond? llen lval cst-len? cst-val?)
  (cond ((and cst-len?
              (mem-still-required? (* 8 llen)))
           (codegen-p-make-f64vector-imm-sti cgc fs ffs reg llen lval cst-val?))
        (cst-len?
           (codegen-p-make-f64vector-imm cgc fs ffs reg llen lval cst-val?))
        (else
           (codegen-p-make-f64vector-opn cgc fs ffs reg llen lval cst-val?))))

(define (codegen-p-make-f64vector-imm cgc fs ffs reg llen lval cst-val?)
  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (x86-imm-int (obj-encoding llen 86)))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval 87))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    ;; Alloc vector
    (gen-allocation-imm cgc STAG_F64VECTOR (* 8 llen))
    ;; Loop counter
    (x86-mov cgc selector-reg (x86-imm-int (* -8 llen)))
    ;; Loop
    (cond (cst-val?
            (x86-mov cgc dest (x86-imm-int (flonum->ieee754 lval 'double))))
          ((and (not opt-nan-boxing)
                (not opt-float-unboxing))
            (if (and opt-stats (not opt-float-unboxing))
                       (gen-inc-slot cgc 'flunbox))
            (if (x86-reg? opval)
                (x86-mov cgc dest (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opval))
                (error "NYI case make-f64vector"))))
    (x86-label cgc label-loop)
    (x86-cmp cgc selector-reg (x86-imm-int 0))
    (x86-je cgc label-end)

      (let ((memop (x86-mem 0 alloc-ptr selector-reg)))
        (cond ((or cst-val?
                   (not opt-float-unboxing))
                (x86-mov cgc memop dest))
              ((x86-xmm? opval)
                (x86-movsd cgc memop opval))
              (else
                (x86-mov cgc memop opval)))
        (x86-add cgc selector-reg (x86-imm-int 8))
        (x86-jmp cgc label-loop))

    (x86-label cgc label-end)
    (if opt-nan-boxing
        (begin (x86-lea cgc dest (x86-mem (* (+ llen 1) -8) alloc-ptr))
               (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
               (x86-or cgc dest (x86-rax)))
        (x86-lea cgc dest (x86-mem (+ (* (+ llen 1) -8) TAG_MEMOBJ) alloc-ptr)))
    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 89)))))

;; make-f64vector with cst len (still)
(define (codegen-p-make-f64vector-imm-sti cgc fs ffs reg llen lval cst-val?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (x86-imm-int (obj-encoding llen 90)))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval 91))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

  ;; Alloc vector
  (gen-allocation-imm-sti cgc STAG_F64VECTOR (* 8 llen))
  ;; Loop counter
  (x86-mov cgc selector-reg (x86-imm-int (* 8 llen)))
  ;; Loop
  (cond (cst-val?
          (x86-mov cgc dest (x86-imm-int (flonum->ieee754 lval 'double))))
        ((and (not opt-nan-boxing)
              (not opt-float-unboxing))
          (if (and opt-stats (not opt-float-unboxing))
                     (gen-inc-slot cgc 'flunbox))
          (if (x86-reg? opval)
              (x86-mov cgc dest (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opval))
              (error "NYI case make-f64vector"))))
  (x86-label cgc label-loop)
  (x86-cmp cgc selector-reg (x86-imm-int 0))
  (x86-je cgc label-end)

    (let ((memop (x86-mem (- TAG_MEMOBJ) (x86-rax) selector-reg)))
      (cond ((or cst-val?
                 (not opt-float-unboxing))
              (x86-mov cgc memop dest))
            ((x86-xmm? opval)
              (x86-movsd cgc memop opval))
            (else
              (x86-mov cgc memop opval)))
      (x86-sub cgc selector-reg (x86-imm-int 8))
      (x86-jmp cgc label-loop))

  (x86-label cgc label-end)
  (if opt-nan-boxing
      (begin (x86-mov cgc dest (x86-imm-int (to-64-value (- NB_MASK_MEM TAG_MEMOBJ))))
             (x86-lea cgc dest (x86-mem 0 (x86-rax) dest)))
      (x86-mov cgc dest (x86-rax)))
  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 93)))))

;; make-f64vector with !cst len
(define (codegen-p-make-f64vector-opn cgc fs ffs reg llen lval cst-val?)

  (let* ((dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd fs ffs llen))
         (opval (if cst-val?
                    (x86-imm-int (obj-encoding lval 94))
                    (codegen-loc-to-x86opnd fs ffs lval)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    (define save #f)
    (define (get-saved-reg)
      alloc-ptr)

    (if opt-nan-boxing
        (begin
            (x86-mov cgc (x86-eax) (l32 oplen))
            (x86-shl cgc (x86-rax) (x86-imm-int 3))
            (gen-allocation-rt cgc STAG_F64VECTOR (x86-rax))

            ;; if val is cst, mem, or eq dest
            ;; we need another free register
            (if (x86-mem? opval)
                (error "NYI case make-f64vector"))
            (if (or cst-val?
                    (eq? dest opval))
                (begin
                  (set! save (get-saved-reg))
                  (x86-ppush cgc save)
                  (x86-mov cgc save opval)
                  (set! opval save)))
            ;;
            (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_48))
            (x86-and cgc selector-reg (x86-rax)) ;; vector addr in selector

            (x86-mov cgc (l32 dest) (l32 oplen)) ;; vector len in dest reg
            (x86-label cgc label-loop)
            (x86-cmp cgc dest (x86-imm-int 0))
            (x86-je cgc label-end)
              (if (x86-xmm? opval)
                  (x86-movsd cgc (x86-mem 0 selector-reg dest 3) opval)
                  (x86-mov cgc (x86-mem 0 selector-reg dest 3) opval))
              (x86-sub cgc dest (x86-imm-int 1))
              (x86-jmp cgc label-loop)

            (x86-label cgc label-end)
            (if save
                (x86-ppop cgc save))
            (x86-mov cgc dest (x86-rax))
            (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 95))))

        (begin
            (x86-mov cgc (x86-rax) oplen)
            (x86-shl cgc (x86-rax) (x86-imm-int 1))
            (gen-allocation-rt cgc STAG_F64VECTOR (x86-rax))

            ;; Loop
            (x86-mov cgc selector-reg oplen)
            (x86-shl cgc selector-reg (x86-imm-int 1))
            (cond (cst-val?
                    (x86-mov cgc dest (x86-imm-int (flonum->ieee754 lval 'double))))
                  ((not opt-float-unboxing)
                    (if (and opt-stats (not opt-float-unboxing))
                               (gen-inc-slot cgc 'flunbox))
                    (if (x86-reg? opval)
                        (x86-mov cgc dest (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opval))
                        (error "NYI case make-f64vector"))))
            (x86-label cgc label-loop)
            (x86-cmp cgc selector-reg (x86-imm-int 0))
            (x86-je cgc label-end)

              (let ((memop (x86-mem (- TAG_MEMOBJ) (x86-rax) selector-reg)))

                (cond ((or cst-val?
                           (not opt-float-unboxing))
                        (x86-mov cgc memop dest))
                      ((x86-xmm? opval)
                        (x86-movsd cgc memop opval))
                      ((x86-mem? opval)
                        (x86-mov cgc dest opval)
                        (x86-mov cgc memop dest))
                      (else
                        (x86-mov cgc memop opval)))

                (x86-sub cgc selector-reg (x86-imm-int 8))
                (x86-jmp cgc label-loop))

            ;;
            (x86-label cgc label-end)
            (x86-mov cgc dest (x86-rax))
            (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 97)))))))

;;
;; *vector-length
(define (codegen-p-*vector-length cgc fs ffs op reg inlined-cond? lvec cst-vec?)

  (assert (not cst-vec?) "Internal error")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opvec (codegen-loc-to-x86opnd fs ffs lvec)))

    (if (ctx-loc-is-memory? lvec)
        (begin (x86-mov cgc (x86-rax) opvec)
               (set! opvec (x86-rax))))

    (if opt-nan-boxing
        (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
               (x86-and cgc (x86-rax) opvec)
               (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rax)))
               (x86-shr cgc (x86-rax) (x86-imm-int 11)) ;; 8 header + 2 (to get length from nbytes)
               (x86-mov cgc dest (x86-imm-int (to-64-value NB_MASK_FIX)))
               (x86-or cgc dest (x86-rax)))
        (begin (x86-mov cgc dest (x86-mem (- TAG_MEMOBJ) opvec))
               (x86-shr cgc dest (x86-imm-int 9))))))

;;
;; string-length
(define (codegen-p-string-length cgc fs ffs op reg inlined-cond? lstr str-cst?)

  (assert (not str-cst?) "Internal error. Unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd fs ffs lstr)))

    (if opt-nan-boxing
        (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
               (x86-and cgc (x86-rax) opstr)
               (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rax)))
               (x86-shr cgc (x86-rax) (x86-imm-int 10)) ;; 8 header + 2 (to get length from nbytes)
               (x86-mov cgc dest (x86-imm-int (to-64-value NB_MASK_FIX)))
               (x86-or cgc dest (x86-rax)))
        (begin (if (ctx-loc-is-memory? lstr)
                   (begin (x86-mov cgc (x86-rax) opstr)
                          (set! opstr (x86-rax))))
               (x86-mov cgc dest (x86-mem (- TAG_MEMOBJ) opstr))
               (x86-shr cgc dest (x86-imm-int 8))))))

;;
;; vector-ref
;; TODO val-cst? -> idx-cst?
(define (codegen-p-*vector-ref cgc fs ffs op reg inlined-cond? lvec lidx vec-cst? val-cst?)

  (assert (not (and vec-cst? val-cst?)) "Internal error")
  (assert (if vec-cst?
              (permanent-object? lvec)
              #t)
          "Internal error")

  (let* ((tdest (codegen-loc-to-x86opnd fs ffs reg)) ;; could be a reg (vector-ref) or freg (f64vector-ref)
         ;; If op is f64vector-ref, and boxed tagged float are used, set dest to xmm0.
         ;; Using xmm0 avoids the case where an unboxed float is stored in the 'dest' general register and gc is triggered by
         ;; float allocation. In this case, the unboxed float is handled like a tagged value by the gc.
         (dest (if (and (eq? op 'f64vector-ref)
                        (not opt-float-unboxing)
                        (not opt-nan-boxing))
                   (x86-xmm0)
                   tdest))
         (opvec (and (not vec-cst?) (codegen-loc-to-x86opnd fs ffs lvec)))
         (opidx (and (not val-cst?) (codegen-loc-to-x86opnd fs ffs lidx))))

    (if opt-nan-boxing
        (codegen-p-*vector-ref-nan cgc dest opvec lvec vec-cst? opidx lidx val-cst?)
        (codegen-p-*vector-ref-tag cgc dest opvec lvec vec-cst? opidx lidx val-cst?))

    (if (and (eq? op 'f64vector-ref)
             (not opt-float-unboxing)
             (not opt-nan-boxing))
        (begin ;; Alloc result flonum
               (gen-allocation-imm cgc STAG_FLONUM 8)
               ;; Write number
               (x86-movsd cgc (x86-mem -8 alloc-ptr) (x86-xmm0))
               ;; Put
               (x86-lea cgc tdest (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))))))

(define (codegen-p-*vector-ref-nan cgc dest opvec lvec vec-cst? opidx lidx val-cst?)

  (let ((use-selector #f))

    (cond (vec-cst?
            (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lvec 98)))
            (set! opvec (x86-rax)))
          ((x86-mem? opvec)
            (x86-mov cgc (x86-rax) opvec)
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
        (x86-lea cgc selector-reg (x86-mem (+ 8 (* 8 lidx)) opvec))
        (x86-lea cgc selector-reg (x86-mem 8 opvec opidx 3)))
    (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
    (x86-and cgc selector-reg (x86-rax))
    (if (x86-xmm? dest)
        (x86-movsd cgc dest (x86-mem 0 selector-reg))
        (x86-mov   cgc dest (x86-mem 0 selector-reg)))

    (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 100)))))

(define (codegen-p-*vector-ref-tag cgc dest opvec lvec vec-cst? opidx lidx val-cst?)

  (let ((use-selector #f))

    (cond (vec-cst?
            (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lvec 99)))
            (set! opvec (x86-rax)))
          ((x86-mem? opvec)
            (x86-mov cgc (x86-rax) opvec)
            (set! opvec  (x86-rax))))

    (if (and opidx
             (x86-mem? opidx))
        (if (eq? opvec (x86-rax))
            (begin (x86-mov cgc selector-reg opidx)
                   (set! opidx selector-reg)
                   (set! use-selector #t))
            (begin (x86-mov cgc (x86-rax) opidx)
                   (set! opidx (x86-rax)))))

    (let ((x86-op (if (x86-xmm? dest) x86-movsd x86-mov)))
      (if val-cst?
          (x86-op cgc dest (x86-mem (+ (- 8 TAG_MEMOBJ) (* 8 lidx)) opvec #f 1))
          (x86-op cgc dest (x86-mem (- 8 TAG_MEMOBJ) opvec opidx 1))))

    (if use-selector
        (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 100))))))

;;
;; string-ref
(define (codegen-p-string-ref cgc fs ffs op reg inlined-cond? lstr lidx str-cst? idx-cst?)

  (assert (or (not str-cst?)
              (and (##mem-allocated? lstr)
                   (not idx-cst?)))
          "Internal error")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (and (not str-cst?) (codegen-loc-to-x86opnd fs ffs lstr)))
        (opidx (and (not idx-cst?) (codegen-loc-to-x86opnd fs ffs lidx))))

    (if opt-nan-boxing
        (codegen-p-string-ref-nan cgc dest opstr opidx lstr lidx idx-cst?)
        (codegen-p-string-ref-tag cgc dest opstr opidx lstr lidx idx-cst?))))

(define (codegen-p-string-ref-nan cgc dest opstr opidx lstr lidx idx-cst?)

  (if idx-cst?
      (begin (if opstr
                 (begin (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
                        (x86-and cgc (x86-rax) opstr))
                 (x86-mov cgc (x86-rax) (x86-imm-int (object-address lstr))))
             (x86-mov cgc (x86-r64->r32 dest) (x86-mem (+ 8 (* 4 lidx)) (x86-rax)))
             (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_CHA)))
             (x86-or cgc dest (x86-rax)))
      (begin (if (not opstr)
                 (begin (x86-mov cgc (x86-rax) (x86-imm-int (object-address lstr)))
                        (set! opstr (x86-rax))))
             (if (x86-mem? opstr)
                 (begin (x86-mov cgc (x86-rax) opstr)
                        (set! opstr (x86-rax))))
             (cond ((and (x86-mem? opidx) (eq? opstr dest))
                      (x86-mov cgc (x86-rax) opidx)
                      (set! opidx (x86-rax)))
                   ((x86-mem? opidx)
                      (x86-mov cgc dest opidx)
                      (set! opidx dest)))
             (x86-lea cgc dest (x86-mem 8 opstr opidx 2))
             (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
             (x86-and cgc dest (x86-rax))
             (x86-mov cgc (x86-r64->r32 dest) (x86-mem 0 dest))
             (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_CHA)))
             (x86-or cgc dest (x86-rax)))))

(define (codegen-p-string-ref-tag cgc dest opstr opidx lstr lidx idx-cst?)

  (cond ((not opstr)
           (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding lstr 101)))
           (set! opstr (x86-rax)))
        ((x86-mem? opstr)
           (x86-mov cgc (x86-rax) opstr)
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
  (x86-mov cgc dest (x86-rax)))

;;
;; vector
;; An empty vector (of right size) already is created and is in 'vec-loc'
(define (codegen-p-vector cgc fs ffs csts?/locs vec-loc)
  (if opt-nan-boxing
      (codegen-p-vector-nan cgc fs ffs csts?/locs vec-loc)
      (codegen-p-vector-tag cgc fs ffs csts?/locs vec-loc)))

(define (codegen-p-vector-nan cgc fs ffs csts?/locs vec-loc)
  (define vec-opnd (codegen-loc-to-x86opnd fs ffs vec-loc))
  (define vec-len  (length csts?/locs))

  (if (x86-mem? vec-opnd) (error "Unimplemented case in codegen."))

  (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_VALUE_48)))
  (x86-and cgc vec-opnd (x86-rax))
  (let loop ((csts?/locs csts?/locs)
             (i vec-len))
    (if (not (null? csts?/locs))
        (let* ((cst? (caar csts?/locs))
               (loc  (cdar csts?/locs))
               (offset (* 8 (+ (- vec-len i) 1)))
               (opnd (and (not cst?) (codegen-loc-to-x86opnd fs ffs loc))))
          (cond ;; cst
                (cst?
                  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding loc 102)))
                  (x86-mov cgc (x86-mem offset vec-opnd) (x86-rax)))
                ;;
                ((x86-mem? opnd)
                  (x86-mov cgc (x86-rax) opnd)
                  (x86-mov cgc (x86-mem offset vec-opnd) (x86-rax)))
                (else
                  (x86-mov cgc (x86-mem offset vec-opnd) opnd)))
          (loop (cdr csts?/locs) (- i 1)))))
  (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
  (x86-or cgc vec-opnd (x86-rax)))

(define (codegen-p-vector-tag cgc fs ffs csts?/locs vec-loc)
  (define vec-opnd (codegen-loc-to-x86opnd fs ffs vec-loc))
  (define vec-len  (length csts?/locs))
  (let loop ((csts?/locs csts?/locs)
             (i vec-len))
    (if (not (null? csts?/locs))
        (let* ((cst? (caar csts?/locs))
               (loc  (cdar csts?/locs))
               (offset (- (* 8 (+ (- vec-len i) 1)) TAG_MEMOBJ))
               (opnd (if cst?
                         (x86-imm-int (obj-encoding loc 103))
                         (codegen-loc-to-x86opnd fs ffs loc))))
          (if (or (x86-imm? opnd)
                  (x86-mem? opnd))
              (begin (x86-mov cgc (x86-rax) opnd)
                     (set! opnd (x86-rax))))
          (x86-mov cgc (x86-mem offset vec-opnd) opnd)
          (loop (cdr csts?/locs) (- i 1))))))

;;
;; f64vector
;; An empty f64vector (of right size) already is created and is in 'vec-loc'
(define (codegen-p-f64vector cgc fs ffs csts?/locs vec-loc)
  (if opt-nan-boxing
      (codegen-p-f64vector-nan cgc fs ffs csts?/locs vec-loc)
      (codegen-p-f64vector-tag cgc fs ffs csts?/locs vec-loc)))

(define (codegen-p-f64vector-nan cgc fs ffs csts?/locs vec-loc)
  (define vec-opnd (codegen-loc-to-x86opnd fs ffs vec-loc))
  (define vec-len  (length csts?/locs))

  (if (x86-mem? vec-opnd) (error "Unimplemented case in codegen."))

  (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_VALUE_48)))
  (x86-and cgc vec-opnd (x86-rax))
  (let loop ((csts?/locs csts?/locs)
             (i vec-len))
    (if (not (null? csts?/locs))
        (let* ((cst? (caar csts?/locs))
               (loc  (cdar csts?/locs))
               (offset (* 8 (+ (- vec-len i) 1)))
               (opnd (and (not cst?) (codegen-loc-to-x86opnd fs ffs loc))))
          (cond ;; cst
                (cst?
                  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding loc 102)))
                  (x86-mov cgc (x86-mem offset vec-opnd) (x86-rax)))
                ;;
                ((x86-mem? opnd)
                  (x86-mov cgc (x86-rax) opnd)
                  (x86-mov cgc (x86-mem offset vec-opnd) (x86-rax)))
                ;;
                ((x86-xmm? opnd)
                  (x86-movsd cgc (x86-mem offset vec-opnd) opnd))
                (else
                  (x86-mov cgc (x86-mem offset vec-opnd) opnd)))
          (loop (cdr csts?/locs) (- i 1)))))
  (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_MEM)))
  (x86-or cgc vec-opnd (x86-rax)))

(define (codegen-p-f64vector-tag cgc fs ffs csts?/locs vec-loc)
  (define vec-opnd (codegen-loc-to-x86opnd fs ffs vec-loc))
  (define vec-len  (length csts?/locs))

  (let loop ((csts?/locs csts?/locs)
             (i vec-len))
    (if (not (null? csts?/locs))
        (let* ((cst? (caar csts?/locs))
               (loc  (cdar csts?/locs))
               (offset (- (* 8 (+ (- vec-len i) 1)) TAG_MEMOBJ))
               (opnd (if cst?
                         (x86-imm-int (to-64-value (flonum->ieee754 loc 'double)))
                         (codegen-loc-to-x86opnd fs ffs loc))))

          (if (or (x86-imm? opnd)
                  (x86-mem? opnd))
              (begin (x86-mov cgc (x86-rax) opnd)
                     (set! opnd (x86-rax))))

          ;; If opt-float-unboxing is #f and it's not a cst, the value needs to be unboxed
          (if (and (not opt-float-unboxing)
                   (not cst?))
              (begin (if (and opt-stats (not opt-float-unboxing))
                         (gen-inc-slot cgc 'flunbox))
                     (x86-mov cgc (x86-rax) (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opnd))
                     (set! opnd (x86-rax))))

          ;; Write value
          (let ((x86-op (if (x86-xmm? opnd) x86-movsd x86-mov)))
            (x86-op cgc (x86-mem offset vec-opnd) opnd))
          (loop (cdr csts?/locs) (- i 1))))))

;;
;; vector-set!
(define (codegen-p-vector-set! cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)

  (assert (not vec-cst?) "Internal error")
  (assert (or (not val-cst?)
              (not (##mem-allocated? lval))
              (and (##mem-allocated? lval)
                   (eq? (mem-allocated-kind lval) 'PERM)))
          "Internal error")

  (if opt-nan-boxing
      (codegen-p-vector-set!-nan cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)
      (codegen-p-vector-set!-tag cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)))

(define (codegen-p-vector-set!-nan cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)

  (let* ((dest (codegen-reg-to-x86reg reg))
         (opvec (codegen-loc-to-x86opnd fs ffs lvec))
         (opidx (and (not idx-cst?) (codegen-loc-to-x86opnd fs ffs lidx)))
         (opval (and (not val-cst?) (codegen-loc-to-x86opnd fs ffs lval))))

    ;; vector
    (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_VALUE_48)))
    (x86-and cgc (x86-rax) opvec)

    (cond
      ;; cst/cst
      ((and idx-cst? val-cst?)
         (x86-mov cgc dest (x86-imm-int (obj-encoding lval 104)))
         (x86-mov cgc (x86-mem (+ (* 8 lidx) 8) (x86-rax)) dest))
      ;;
      (idx-cst?
         (if (x86-mem? opval)
             (begin (x86-mov cgc dest opval)
                    (set! opval dest)))
         (x86-mov cgc (x86-mem (+ (* 8 lidx) 8) (x86-rax)) opval))
      ;;
      (val-cst?
         ;; index
         (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_32))
         (x86-and cgc selector-reg opidx)
         ;; value
         (x86-mov cgc dest (x86-imm-int (obj-encoding lval 105)))
         ;; op
         (x86-mov cgc (x86-mem 8 (x86-rax) selector-reg 3) dest)
         (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 106))))
      ;; ncst/ncst
      (else
         (cond ;; opval is mem
               ((x86-mem? opval)
                  (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_32))
                  (x86-and cgc selector-reg opidx)
                  (x86-mov cgc dest opval)
                  (x86-mov cgc (x86-mem 8 (x86-rax) selector-reg 3) dest)
                  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 107))))
               ;; opval is dest
               ((eq? opval dest)
                  (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_32))
                  (x86-and cgc selector-reg opidx)
                  (x86-mov cgc (x86-mem 8 (x86-rax) selector-reg 3) opval)
                  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 108))))
               ;; opval is reg !dest
               (else
                  (x86-mov cgc dest (x86-imm-int NB_MASK_VALUE_32))
                  (x86-and cgc dest opidx)
                  (x86-mov cgc (x86-mem 8 (x86-rax) dest 3) opval)))))

    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))


(define (codegen-p-vector-set!-tag cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)

  (let* ((dest (codegen-reg-to-x86reg reg))
         (opvec (codegen-loc-to-x86opnd fs ffs lvec))
         (opidx (if idx-cst?
                    (x86-imm-int (obj-encoding lidx 109))
                    (codegen-loc-to-x86opnd fs ffs lidx)))
         (opval (if val-cst?
                    (x86-imm-int (obj-encoding lval 110))
                    (codegen-loc-to-x86opnd fs ffs lval))))

    (if (x86-mem? opvec)
        (begin (x86-mov cgc (x86-rax) opvec)
               (set! opvec (x86-rax))))

    (cond ;; cst/cst
          ;; cst/mem
          ((or (and idx-cst? val-cst?)
               (and idx-cst? (x86-mem? opval)))
             (x86-mov cgc selector-reg opval)
             (x86-mov cgc (x86-mem (+ (* 8 lidx) (- 8 TAG_MEMOBJ)) opvec) selector-reg)
             (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 111))))
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
             (let* ((saved (if (eq? opvec (x86-rax)) (x86-rbx) #f))
                    (tmpidx (or saved (x86-rax))))
               (if saved (x86-ppush cgc saved))
               (x86-mov cgc selector-reg opval)
               (x86-mov cgc tmpidx opidx)
               (x86-shl cgc tmpidx (x86-imm-int 1))
               (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opvec tmpidx) selector-reg)
               (if saved (x86-ppop cgc saved))
               (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 112)))))
          ;; reg/reg
          ;; mem/reg
          (else
             (x86-mov cgc selector-reg opidx)
             (x86-shl cgc selector-reg (x86-imm-int 1))
             (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opvec selector-reg) opval)
             (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 113)))))

    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

;;
;; f64vector-set!
(define (codegen-p-f64vector-set! cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)

  (assert (not vec-cst?) "Internal error")
  (assert (or (not val-cst?)
              (not (##mem-allocated? lval))
              (and (##mem-allocated? lval)
                   (eq? (mem-allocated-kind lval) 'PERM)))
          "Internal error")

  (if opt-nan-boxing
      (codegen-p-f64vector-set!-nan cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)
      (codegen-p-f64vector-set!-tag cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)))

(define (codegen-p-f64vector-set!-nan cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)

  (let* ((dest (codegen-reg-to-x86reg reg))
         (opvec (codegen-loc-to-x86opnd fs ffs lvec))
         (opidx (and (not idx-cst?) (codegen-loc-to-x86opnd fs ffs lidx)))
         (opval (and (not val-cst?) (codegen-loc-to-x86opnd fs ffs lval))))

    ;; vector
    (x86-mov cgc (x86-rax) (x86-imm-int (to-64-value NB_MASK_VALUE_48)))
    (x86-and cgc (x86-rax) opvec)

    (cond
      ;; cst/cst
      ((and idx-cst? val-cst?)
         (x86-mov cgc dest (x86-imm-int (flonum->ieee754 lval 'double)))
         (x86-mov cgc (x86-mem (+ (* 8 lidx) 8) (x86-rax)) dest))
      ;;
      (idx-cst?
         (if (x86-mem? opval)
             (begin (x86-mov cgc dest opval)
                    (set! opval dest)))
         (let ((x86-op (if (x86-xmm? opval) x86-movsd x86-mov)))
           (x86-op cgc (x86-mem (+ (* 8 lidx) 8) (x86-rax)) opval)))
      ;;
      (val-cst?
         ;; index
         (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_32))
         (x86-and cgc selector-reg opidx)
         ;; value
         (x86-mov cgc dest (x86-imm-int (flonum->ieee754 lval 'double)))
         ;; op
         (x86-mov cgc (x86-mem 8 (x86-rax) selector-reg 3) dest)
         (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 106))))
      ;; ncst/ncst
      (else
         (cond ;; opval is mem
               ((x86-mem? opval)
                  (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_32))
                  (x86-and cgc selector-reg opidx)
                  (x86-mov cgc dest opval)
                  (x86-mov cgc (x86-mem 8 (x86-rax) selector-reg 3) dest)
                  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 107))))
               ;; opval is dest
               ((eq? opval dest)
                  (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_32))
                  (x86-and cgc selector-reg opidx)
                  (x86-mov cgc (x86-mem 8 (x86-rax) selector-reg 3) opval)
                  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0 108))))
               ;; opval is reg !dest
               (else
                  (x86-mov cgc dest (x86-imm-int NB_MASK_VALUE_32))
                  (x86-and cgc dest opidx)
                  (let ((x86-op (if (x86-xmm? opval) x86-movsd x86-mov)))
                    (x86-op cgc (x86-mem 8 (x86-rax) dest 3) opval))))))

    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

(define (codegen-p-f64vector-set!-tag cgc fs ffs op reg inlined-cond? lvec lidx lval vec-cst? idx-cst? val-cst?)

  (define tmp-used? #f)
  (define selector-used? #f)
  (define r8-saved? #f)

  (define dest  (codegen-reg-to-x86reg reg))
  (define opvec (codegen-loc-to-x86opnd fs ffs lvec))
  (define opidx (and (not idx-cst?) (codegen-loc-to-x86opnd fs ffs lidx)))
  (define opval (and (not val-cst?) (codegen-loc-to-x86opnd fs ffs lval)))

  (define (next-free)
    (cond ((not tmp-used?)
             (set! tmp-used? #t)
             (x86-rax))
          ((not selector-used?)
             (set! selector-used? #t)
             selector-reg)
          (else
             (x86-ppush cgc (x86-r8))
             (set! r8-saved? #t)
             (x86-r8))))

  ;; keep vec in a register
  (if (x86-mem? opvec)
      (let ((opnd (next-free)))
        (x86-mov cgc opnd opvec)
        (set! opvec opnd)))

  ;; keep opidx in a register
  (if (not idx-cst?)
      (let ((opnd (next-free)))
        (x86-mov cgc opnd opidx)
        (set! opidx opnd)))

  ;; keep unboxed val in a register
  (if (or val-cst?
          (not (x86-xmm? opval)))
      (let ((opnd (next-free)))
        (cond (val-cst?
                (x86-mov cgc opnd (x86-imm-int (flonum->ieee754 lval 'double))))
              ((and (x86-reg? opval)
                    (not (x86-xmm? opval)))
                (if (and opt-stats (not opt-float-unboxing))
                    (gen-inc-slot cgc 'flunbox))
                (x86-mov cgc opnd (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opval)))
              ((x86-mem? opval)
                (if (and opt-stats (not opt-float-unboxing))
                    (gen-inc-slot cgc 'flunbox))
                (x86-mov cgc opnd opval)
                (x86-mov cgc opnd (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opnd))))
        (set! opval opnd)))

  (let ((x86-op (if (and (not val-cst?) (x86-xmm? opval)) x86-movsd x86-mov)))
    (if idx-cst?
        (x86-op cgc (x86-mem (+ (* 8 lidx) (- 8 TAG_MEMOBJ)) opvec) opval)
        (begin (x86-shl cgc opidx (x86-imm-int 1))
               (x86-op cgc (x86-mem (- 8 TAG_MEMOBJ) opvec opidx) opval))))

  (if selector-used?
      (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0))))
  (if r8-saved?
      (x86-ppop cgc (x86-r8)))
  (x86-mov cgc dest (x86-imm-int (obj-encoding #!void))))
;;
;; string-set!
(define (codegen-p-string-set! cgc fs ffs op reg inlined-cond? lstr lidx lchr str-cst? idx-cst? chr-cst?)
  (if opt-nan-boxing
      (codegen-p-string-set!-nan cgc fs ffs op reg inlined-cond? lstr lidx lchr str-cst? idx-cst? chr-cst?)
      (codegen-p-string-set!-tag cgc fs ffs op reg inlined-cond? lstr lidx lchr str-cst? idx-cst? chr-cst?)))

(define (codegen-p-string-set!-nan cgc fs ffs op reg inlined-cond? lstr lidx lchr str-cst? idx-cst? chr-cst?)

  (assert (not str-cst?) "Internal error. Unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd fs ffs lstr))
        (opidx (and (not idx-cst?) (codegen-loc-to-x86opnd fs ffs lidx)))
        (opchr (if chr-cst?
                   (x86-imm-int (char->integer lchr))
                   (codegen-loc-to-x86opnd fs ffs lchr))))

    ;; unbox str ptr
    (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
    (x86-and cgc (x86-rax) opstr)

    ;; index
    (if (not idx-cst?)
        (begin (x86-mov cgc selector-reg (x86-imm-int NB_MASK_VALUE_32))
               (x86-and cgc selector-reg opidx)))

    ;; char
    (if (x86-mem? opchr)
        (begin (x86-mov cgc dest opchr)
               (set! opchr dest)))

    ;; op
    (if idx-cst?
        (x86-mov cgc (x86-mem (+ 8 (* 4 lidx)) (x86-rax)) (l32 opchr) 32)
        (begin (x86-mov cgc (x86-mem 8 (x86-rax) selector-reg 2) (l32 opchr) 32)
               (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0)))))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

(define (codegen-p-string-set!-tag cgc fs ffs op reg inlined-cond? lstr lidx lchr str-cst? idx-cst? chr-cst?)

  (assert (not str-cst?) "Internal error. Unexpected cst operand")

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd fs ffs lstr))
        (opidx (and (not idx-cst?) (codegen-loc-to-x86opnd fs ffs lidx)))
        (opchr (and (not chr-cst?) (codegen-loc-to-x86opnd fs ffs lchr))))

    (if (x86-mem? opstr)
        (begin (x86-mov cgc (x86-rax) opstr)
               (set! opstr (x86-rax))))


    (cond ((and idx-cst? chr-cst?)
             (x86-mov cgc (x86-mem (+ (- 8 TAG_MEMOBJ) (* 4 lidx)) opstr) (x86-imm-int (char->integer lchr)) 32))
          (idx-cst?
             (let ((mreg (if (eq? opstr dest) (x86-rax) dest)))
               (x86-mov cgc mreg opchr)
               (x86-shr cgc mreg (x86-imm-int 2))
               (x86-mov cgc (x86-mem (+ (- 8 TAG_MEMOBJ) (* 4 lidx)) opstr) (l32 mreg))))
          (chr-cst?
             (let ((mreg (if (eq? opstr dest) (x86-rax) dest)))
               (if (x86-mem? opidx)
                   (begin (x86-mov cgc mreg opidx)
                          (set! opidx mreg)))
               (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opstr opidx) (x86-imm-int (char->integer lchr)) 32)))
          (else
             (let ((mreg (if (eq? opstr dest) (x86-rax) dest)))
               ;; char
               (x86-mov cgc selector-reg opchr)
               (x86-shr cgc selector-reg (x86-imm-int 2))
               ;; index
               (if (x86-mem? opidx)
                   (begin (x86-mov cgc mreg opidx)
                          (set! opidx mreg)))
               ;;
               (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opstr opidx) selector-reg-32)
               (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0))))))

    (x86-mov cgc dest (x86-imm-int (obj-encoding #!void)))))

(define (codegen-p-gettime-ns cgc fs ffs op reg inlined-cond?)
  (let ((opnd (codegen-reg-to-x86reg reg)))
    ;; Get monotonic time in rax
    (gen-syscall-clock-gettime cgc)
    (if opt-nan-boxing
        (error "NYI (> 32 bits)")
        (begin
          (x86-mov cgc opnd (x86-rax))
          (x86-shl cgc opnd (x86-imm-int 2))))))

;;-----------------------------------------------------------------------------
;; Others
;;-----------------------------------------------------------------------------

(define (codegen-subtype cgc fs reg lval)
  (if opt-nan-boxing (error "NYI codegen nan"))
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

(define (codegen-make-vector-cst cgc fs reg len lval)

  (if opt-nan-boxing (error "NYI codegen nan"))

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

;;-----------------------------------------------------------------------------
;; Type checks
;;-----------------------------------------------------------------------------

(define (codegen-test-fixnum cgc op)
  (if opt-nan-boxing
      (begin
        (x86-mov cgc (x86-rax) op)
        (x86-shr cgc (x86-rax) (x86-imm-int NB_MASK_SHIFT))
        (x86-cmp cgc (x86-rax) (x86-imm-int NB_MASK_FIX_UNSHIFTED)))
      (x86-test cgc op (x86-imm-int 3) 64)))

(define (codegen-test-value cgc op obj)
  (let ((imm (obj-encoding obj 114)))
    (if (codegen-is-imm-64? imm)
        (begin
          (x86-mov cgc (x86-rax) (x86-imm-int imm))
          (x86-cmp cgc (x86-rax) op))
        (begin
          (x86-cmp cgc op (x86-imm-int imm))))))

(define (codegen-test-mem-obj cgc ast op loc type label-jump freg)
  (if opt-nan-boxing
      (codegen-test-mem-obj-nan cgc ast op loc type label-jump freg)
      (codegen-test-mem-obj-tag cgc ast op loc type label-jump freg)))

(define (codegen-test-mem-obj-nan cgc ast op loc type label-jump freg)
  (x86-mov cgc (x86-rax) op)
  (x86-shr cgc (x86-rax) (x86-imm-int 48))
  (x86-cmp cgc (x86-rax) (x86-imm-int #xFFFF))
  (x86-jne cgc label-jump)
  ;; Check stag
  (x86-mov cgc (x86-rax) (x86-imm-int NB_MASK_VALUE_48))
  (x86-and cgc (x86-rax) op)
  (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rax)))
  (x86-and cgc (x86-rax) (x86-imm-int 248))
  (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (ctx-type->stag type)))))

(define (codegen-test-mem-obj-tag cgc ast op loc type label-jump freg)
  ;; Check tag
  (x86-mov cgc (x86-rax) op)
  (x86-and cgc (x86-rax) (x86-imm-int 3))
  (x86-cmp cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
  (x86-jne cgc label-jump)
  ;; Check stag
  (if (ctx-loc-is-memory? loc)
      (begin
        (x86-mov cgc (x86-rax) op)
        (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) (x86-rax))))
      (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) op)))
  (x86-and cgc (x86-rax) (x86-imm-int 248)) ;; 0...011111000 to get type in object header
  ;; stag xxx << 3
  (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (ctx-type->stag type))))

  (if (and (ctx-type-flo? type)
           opt-float-unboxing)
      (let ((opnd (codegen-freg-to-x86reg freg)))
        (x86-jne cgc label-jump)
        (if (x86-mem? op)
            (begin (x86-mov cgc (x86-rax) op)
                   (set! op (x86-rax))))
        (if opt-stats
            (gen-inc-slot cgc 'flunbox))
        (x86-movsd cgc opnd (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) op)))))

(define (codegen-test-char cgc opval)
  (if opt-nan-boxing
      (begin
        (x86-mov cgc (x86-rax) opval)
        (x86-shr cgc (x86-rax) (x86-imm-int NB_MASK_SHIFT))
        (x86-cmp cgc (x86-rax) (x86-imm-int NB_MASK_CHA_UNSHIFTED)))
      (begin
        ;; char if val is tagged with TAG_SPECIAL and val > 0
        (x86-mov cgc (x86-rax) opval)
        (x86-mov cgc selector-reg (x86-imm-int SPECIAL_MASK))
        (x86-and cgc (x86-rax) selector-reg)
        (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0)))
        (x86-cmp cgc (x86-rax) (x86-imm-int TAG_SPECIAL)))))
