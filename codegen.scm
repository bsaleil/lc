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
;; x86 Registers

;; x86 registers map associate virtual register to x86-register
;; ex. ((r0 . (x86-rax)) (r1 . (x86-rbx)) ...)
(define codegen-regmap
  (foldr (lambda (el r)
           (cons (cons (string->symbol (string-append "r" (number->string el)))
                       (list-ref regalloc-regs el))
                 r))
         '()
         (build-list (length regalloc-regs) (lambda (l) l))))

(define base-ptr   (x86-rbp))
(define alloc-ptr  (x86-r9))
(define global-ptr (x86-r8))

;; NOTE: temporary register is always rax
;; NOTE: selector is always rcx
;; NOTE: stack pointer is always rsp

;;-----------------------------------------------------------------------------
;; x86 Codegen utils

;; TODO: use (codegen-push-n?)
(define (codegen-void cgc)
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

(define (codegen-set-bool cgc b reg)
  (let ((dest (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (obj-encoding b)))))

(define (codegen-push-n cgc imm n)
  (call-n n x86-push cgc (x86-imm-int (obj-encoding n))))

(define (codegen-push-tmp cgc)
  (x86-push cgc (x86-rax)))

(define (codegen-move-tmp cgc offset reg)
  (x86-mov cgc (x86-mem offset reg) (x86-rax)))

(define (codegen-clean-stack cgc nb)
  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 nb))))

(define (codegen-dispatch-imm cgc label-dispatch label-true label-false from-stack? cmp-val)
  (if from-stack?
    (x86-pop cgc (x86-rax)))
  (x86-cmp cgc (x86-rax) (x86-imm-int (obj-encoding cmp-val)))
  (x86-label cgc label-dispatch)
  (x86-je cgc label-true)
  (x86-jmp cgc label-false))



;;-----------------------------------------------------------------------------
;; Define
(define (codegen-define-id cgc)
  (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
  (x86-mov cgc (x86-mem (* 8 (length globals)) global-ptr) (x86-rax)))

(define (codegen-define-bind cgc pos reg lvalue)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd lvalue)))
    (if (ctx-loc-is-register? lvalue)
        (x86-mov cgc (x86-mem (* 8 pos) global-ptr) opval)
        (begin (x86-mov cgc (x86-rax) opval)
               (x86-mov cgc (x86-mem (* 8 pos) global-ptr) (x86-rax))))
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; Variables
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; get
(define (codegen-get-global cgc pos reg)
  (let ((dest  (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-mem (* 8 pos) global-ptr))))

(define (codegen-get-local cgc dest pos raw? mutable?)
  (if (or raw? (not mutable?))
      (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem (* pos 8) (x86-rsp))))
            ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (* pos 8) (x86-rsp)))))
      (begin
        (x86-mov cgc (x86-rax) (x86-mem (* pos 8) (x86-rsp)))
        (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
              ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))))))

(define (codegen-get-free cgc dest pos raw? mutable? closure-pos)
  (let ((offset (+ (- 16 TAG_MEMOBJ) (* 8 pos))))
    ;; Get closure in rax
    (x86-mov cgc (x86-rax) (x86-mem (* 8 closure-pos) (x86-rsp)))
    (if (or raw? (not mutable?))
        (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem offset (x86-rax))))
              ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))))
        ;; Real value required and variable is mutable
        (begin (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
               (cond ((eq? dest 'stack) (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                     ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))))))))

;;-----------------------------------------------------------------------------
;; set

(define (codegen-set-global cgc reg lval pos)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd lval)))
    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))
    (x86-mov cgc (x86-mem (* 8 pos) global-ptr) opval)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;; mutable object (local or free) already is in rax
(define (codegen-set-not-global cgc reg lvar lval)
  (let ((opvar (codegen-loc-to-x86opnd lvar))
        (opval (codegen-loc-to-x86opnd lval))
        (dest  (codegen-reg-to-x86reg reg)))

    (if (ctx-loc-is-memory? lvar)
        (begin (x86-mov cgc (x86-rax) opvar)
               (set! opvar (x86-rax))))

    (if (ctx-loc-is-memory? lval)
        (begin (x86-push cgc (x86-rbx))
               (x86-mov cgc (x86-rbx) opval)
               (set! opval (x86-rbx))))

    (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) opvar) opval)

    (if (ctx-loc-is-memory? lval)
        (x86-pop cgc (x86-rbx)))

    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; Special forms
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; If
(define (codegen-if cgc label-jump label-false label-true lcond)
  (let ((opcond (codegen-loc-to-x86opnd lcond)))
    (x86-cmp cgc opcond (x86-imm-int (obj-encoding #f)))
    (x86-label cgc label-jump)
    (x86-je  cgc label-false)
    (x86-jmp cgc label-true)))

;;-----------------------------------------------------------------------------
;; Begin
(define (codegen-begin-out cgc nb-expr)
  (x86-pop  cgc (x86-rax)) ;; Pop result of last expr
  (x86-add  cgc (x86-rsp) (x86-imm-int (* 8 nb-expr))) ;; Clean stack
  (x86-push cgc (x86-rax))) ;; Push result

;;-----------------------------------------------------------------------------
;; Bindings (let, let*, letrec)
;; TODO regalloc remove when finished
;(define (codegen-binding-clear cgc nb-slots)
;  (x86-pop cgc (x86-rax))
;  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 nb-slots)))
;  (x86-push cgc (x86-rax)))
;
;(define (codegen-letrec-end cgc nb-slots)
;  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 nb-slots))))
;
;(define (codegen-letrec-bind cgc from to)
;  (x86-mov cgc (x86-rax) (x86-mem (* 8 from) (x86-rsp)))
;  (x86-mov cgc (x86-rbx) (x86-mem (* 8 to) (x86-rsp)))
;  (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)) (x86-rax)))

;;-----------------------------------------------------------------------------
;; Do

;; Endo of do form. Clean stack and put res value on top of stack
(define (codegen-do-end cgc nb-clean)
  (x86-pop cgc (x86-rax)) ;; Get result from stack
  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 nb-clean))) ;; clean stack
  (x86-push cgc (x86-rax)))

;; Var is in rax
(define (codegen-do-bind-var cgc mutable? from to)
  (x86-mov cgc (x86-rax) (x86-mem from (x86-rsp)))
  (if mutable?
      (begin (x86-mov cgc (x86-rbx) (x86-mem to (x86-rsp))) ;; get mvar box
             (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)) (x86-rax)))
      (x86-mov cgc (x86-mem to (x86-rsp)) (x86-rax))))

;;-----------------------------------------------------------------------------
;; Values
;;-----------------------------------------------------------------------------

(define (codegen-loc-to-x86opnd loc)
  (if (ctx-loc-is-register? loc)
      (codegen-reg-to-x86reg loc)
      (codegen-mem-to-x86mem loc)))

(define (codegen-mem-to-x86mem mem)
  (x86-mem (- (* -8 mem) 8) base-ptr))

(define (codegen-reg-to-x86reg reg)
  (let ((r (assoc reg codegen-regmap)))
    (cdr r)))

;; TODO regalloc
;; TODO regalloc

;;-----------------------------------------------------------------------------
;; Literal
(define (codegen-literal cgc lit reg)
  (let ((dest (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (obj-encoding lit)))))

;;-----------------------------------------------------------------------------
;; Flonum
(define (codegen-flonum cgc immediate reg)
  (let ((header-word (mem-header 2 STAG_FLONUM))
        (dest (codegen-reg-to-x86reg reg)))
    (gen-allocation cgc #f STAG_FLONUM 2) ;; TODO #f
    ;; Write header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Write number
    (x86-mov cgc (x86-rax) (x86-imm-int immediate))
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
    ;; Move flonum to dest
    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;;-----------------------------------------------------------------------------
;; Symbol
(define (codegen-symbol cgc sym reg)
  (let ((qword (get-symbol-qword sym))
        (dest  (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int qword))))

;;-----------------------------------------------------------------------------
;; String
(define (codegen-string cgc str reg)
  (let* ((len (string-length str))
         (size (arithmetic-shift (bitwise-and (+ len 8) (bitwise-not 7)) -3))
         (header-word (mem-header (+ size 2) STAG_STRING))
         (dest (codegen-reg-to-x86reg reg)))

    (gen-allocation cgc #f STAG_STRING (+ size 2))
    ;; Write header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Write length
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (string-length str))))
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
    ;; Write chars
    (write-chars cgc str 0 16)
    ;; Push string
    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;; Write chars of the literal string 'str':
;; Write str[pos] char to [alloc-ptr+offset], and write next chars
(define (write-chars cgc str pos offset)
  (if (< pos (string-length str))
      (let* ((int (char->integer (string-ref str pos)))
             (encoded (if (> int 127)
                          (* -1 (- 256 int))
                          int)))
        (x86-mov cgc (x86-al) (x86-imm-int encoded))
        (x86-mov cgc (x86-mem offset alloc-ptr) (x86-al))
        (write-chars cgc str (+ pos 1) (+ offset 1)))))

;;-----------------------------------------------------------------------------
;; Pair
(define (codegen-pair cgc reg lcar lcdr)
  (let ((header-word (mem-header 3 STAG_PAIR))
        (dest  (codegen-reg-to-x86reg reg))
        (opcar (codegen-loc-to-x86opnd lcar))
        (opcdr (codegen-loc-to-x86opnd lcdr)))
    ;; Alloc
    (gen-allocation cgc #f STAG_PAIR 3)
    ;; Write object header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Write car
    (if (ctx-loc-is-memory? lcar)
        (begin (x86-mov cgc (x86-rax) opcar)
               (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
        (x86-mov cgc (x86-mem 8 alloc-ptr) opcar))
    ;; Write cdr
    (if (ctx-loc-is-memory? lcdr)
        (begin (x86-mov cgc (x86-rax) opcdr)
               (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rax)))
        (x86-mov cgc (x86-mem 16 alloc-ptr) opcdr))
    ;; Tag, move closure to dest
    (x86-mov cgc dest alloc-ptr)
    (x86-add cgc dest (x86-imm-int TAG_MEMOBJ))))

;;-----------------------------------------------------------------------------
;; Vector (all elements are pushed on the stack in reverse order: first at [RSP+0], second at [RSP+8], ...)
(define (codegen-vector cgc vector reg)
  (let ((label-loop (asm-make-label #f (new-sym 'label-loop)))
        (label-end  (asm-make-label #f (new-sym 'label-end)))
        (header-word (mem-header (+ 2 (vector-length vector)) STAG_VECTOR)))
   ;; Allocate array in alloc-ptr
   (gen-allocation cgc #f STAG_VECTOR (+ (vector-length vector) 2))
   ;; Write header
   (x86-mov cgc (x86-rax) (x86-imm-int header-word))
   (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
   ;; Write length
   (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (vector-length vector))))
   (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))

   (x86-mov cgc (x86-rax) (x86-imm-int 0))
   (x86-label cgc label-loop)
   (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (vector-length vector))))
   (x86-je cgc label-end)

     ;; Get val
     (x86-pop cgc (x86-rbx))
     (x86-mov cgc (x86-mem 16 (x86-rax) alloc-ptr) (x86-rbx))
     (x86-add cgc (x86-rax) (x86-imm-int 8))
     (x86-jmp cgc label-loop)

   (x86-label cgc label-end)
   (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
   (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; Functions
;;-----------------------------------------------------------------------------

;; Generate specialized function prologue with rest param and actual == formal
(define (codegen-prologue-rest= cgc)
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
  (x86-push cgc (x86-rax)))
  ;;; Shift closure
  ;(x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
  ;(x86-push cgc (x86-rax))
  ;;; Mov '() in rest param slot
  ;(x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
  ;(x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-rax)))

;; Generate specialized function prologue with rest param and actual > formal
(define (codegen-prologue-rest> cgc restlen)

  (define (gen-rest-lst cgc pos)
    (if (= pos restlen)
        (begin
            (x86-mov cgc (x86-mem (* (+ (- restlen 1) 2) 8) (x86-rsp)) (x86-rbx))
            (x86-pop cgc (x86-rcx))
            (x86-pop cgc (x86-rbx))
            (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- restlen 1)))))
        (let ((header (mem-header 3 STAG_PAIR))
              (offset (* 8 (+ pos 2)))) ;; +2 because we saved rbx and rcx
          ;; cdr is in rbx
          ;; Alloc pair
          ;(gen-allocation cgc #f STAG_PAIR 3)
          (x86-sub cgc (x86-r9) (x86-imm-int 24))
          ;; Write header
          (x86-mov cgc (x86-rax) (x86-imm-int header))
          (x86-mov cgc (x86-mem  0 alloc-ptr) (x86-rax))
          ;; Write car
          (x86-mov cgc (x86-rcx) (x86-mem offset (x86-rsp))) ;; car in rcx
          (x86-mov cgc (x86-mem  8 alloc-ptr) (x86-rcx))
          ;; Write cdr
          (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rbx))
          ;; Next cdr is this new pairs
          (x86-lea cgc (x86-rbx) (x86-mem TAG_MEMOBJ alloc-ptr))
          (gen-rest-lst cgc (+ pos 1)))))

  (x86-push cgc (x86-rbx))
  (x86-push cgc (x86-rcx))
  (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding '())))
  (gen-rest-lst cgc 0))

  ;;; Create a pair with
  ;;; car: [rsp+sp_offset]
  ;;; cdr: top of stack
  ;;; Then, push this pair and create next until pos == 0
  ;(define (gen-rest-lst cgc pos nb sp-offset)
  ;  (if (= pos 0)
  ;      ;; All pairs created, then change stack layout
  ;      (begin ;; Mov rest list to stack
  ;             (x86-pop cgc (x86-rax))
  ;             (x86-mov cgc (x86-mem (* nb 8) (x86-rsp)) (x86-rax))
  ;             ;; Update closure position
  ;             (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
  ;             (x86-mov cgc (x86-mem (- (* 8 nb) 8) (x86-rsp)) (x86-rax))
  ;             ;; Update rsp
  ;             (x86-add cgc (x86-rsp) (x86-imm-int (- (* 8 nb) 8))))
  ;      ;; Create a pair and continue
  ;      (begin ;; Alloc pair
  ;             (gen-allocation cgc #f STAG_PAIR 3)
  ;             (let ((header (mem-header 3 STAG_PAIR)))
  ;               ;; Write header in pair
  ;               (x86-mov cgc (x86-rax) (x86-imm-int header))
  ;               (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
  ;               ;; Get car from stack (arg slot) and write in pair
  ;               (x86-mov cgc (x86-rax) (x86-mem sp-offset (x86-rsp)))
  ;               (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
  ;               ;; Get cdr from stack (top of stack) and write in pair
  ;               (x86-pop cgc (x86-rax))
  ;               (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rax))
  ;               ;; Tag & push
  ;               (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
  ;               (x86-push cgc (x86-rax)))
  ;             ;; Create next pair
  ;             (gen-rest-lst cgc (- pos 1) nb (+ sp-offset 8)))))
  ;
  ;;; Build rest argument
  ;(x86-push cgc (x86-imm-int (obj-encoding '())))
  ;;; Gen code to create rest list from stack
  ;(gen-rest-lst cgc restlen restlen 16)) ;; 16 TODO

;; Generate generic function prologue
(define (codegen-prologue-gen cgc rest? nb-formal err-label)
  (if (not rest?)
      ;; If there is no rest param
      ;; Then we only have to check the number of arguments
      (begin
        (x86-cmp cgc (x86-rdi) (x86-imm-int (* nb-formal 4)))
        (x86-jne cgc err-label))
      ;; If there is a rest param
      ;; Then we have to handle 3 cases: actual>formal, actual=formal, actual<formal
      (let ((label-loop-end  (asm-make-label #f (new-sym 'rest-param-loop-end)))
            (label-end       (asm-make-label #f (new-sym 'rest-param-end)))
            (label-loop      (asm-make-label #f (new-sym 'rest-param-loop)))
            (label-eq        (asm-make-label #f (new-sym 'rest-param-eq)))
            (header-word     (mem-header 3 STAG_PAIR)))

        ;; Compare actual and formal
        (x86-cmp cgc (x86-rdi) (x86-imm-int (* nb-formal 4)))
        (x86-jl cgc err-label) ;; actual<formal, ERROR
        (x86-je cgc label-eq)                ;; actual=formal, jump to label-eq
                                             ;; actual>formal, continue

        ;; Case1: Actual > Formal
        ;; Loop-init
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
        (x86-mov cgc (x86-rbx) (x86-imm-int 8)) ;; rbx = arg-offset (first arg to copy is at [rsp+8])
        (x86-mov cgc (x86-rdx) (x86-rdi))       ;; Save args number in rdx
        ;; Loop-cond (if there is at least 1 arg to copy)
        (x86-label cgc label-loop)
        (x86-cmp cgc (x86-rdi) (x86-imm-int (* nb-formal 4)))
        (x86-je cgc label-loop-end)
          ;; Loop-body
          (x86-push cgc (x86-rax)) ;; TODO
          (gen-allocation cgc #f STAG_PAIR 3)                    ;; alloc pair p
          (x86-pop cgc (x86-rax))                                 ;;
          (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rax))          ;; p.cdr = rax (last pair)
          (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp) (x86-rbx))) ;; p.car = stack[arg-offset]
          (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))         ;;
          (x86-mov cgc (x86-rax) (x86-imm-int header-word))       ;; p.header = header-word
          (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))         ;;
          (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))   ;; rax = p (tagged)
          (x86-add cgc (x86-rbx) (x86-imm-int 8))                 ;; offset += 8 (to next arg)
          (x86-sub cgc (x86-rdi) (x86-imm-int 4))                 ;; rdi    -= 4 (update nb args to copy)
          (x86-jmp cgc label-loop)                                ;; goto loop
        ;; Loop-end
        (x86-label cgc label-loop-end)
        (x86-mov cgc (x86-mem (* -8 nb-formal) (x86-rsp) (x86-rdx) 1) (x86-rax)) ;; Mov rest list to stack
        (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; Update closure position
        (x86-mov cgc (x86-mem (* -8 (+ nb-formal 1)) (x86-rsp) (x86-rdx) 1) (x86-rax))
        (x86-lea cgc (x86-rsp) (x86-mem (* -8 (+ nb-formal 1)) (x86-rsp) (x86-rdx) 1)) ;; Update rsp
        (x86-jmp cgc label-end) ;; goto end

        ;; Case2: Actual == Formal
        (x86-label cgc label-eq)
        (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; Update closure position
        (x86-push cgc (x86-rax))
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '()))) ;; Insert rest list (null) in stack
        (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-rax))

        ;; END
        (x86-label cgc label-end))))

;; Alloc closure and write header
(define (codegen-closure-create cgc nb-free)
  (let* ((closure-size  (+ 2 nb-free)) ;; header, entry point
         (header-word (mem-header closure-size STAG_PROCEDURE)))
    ;; 1 - Alloc closure
    (gen-allocation cgc #f STAG_PROCEDURE closure-size)
    ;; 2 - Write closure header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))))

;; Write entry point in closure (do not use cctable)
(define (codegen-closure-ep cgc ep-loc)
  ;; Write entry point in closure
  (x86-mov cgc (x86-rax) (x86-mem (+ 8 (- (obj-encoding ep-loc) 1))))
  (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))

;; Write cctable ptr in closure (use multiple entry points)
(define (codegen-closure-cc cgc cctable-loc)
  (x86-mov cgc (x86-rax) (x86-imm-int cctable-loc))
  (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))

;; Push closure
(define (codegen-closure-put cgc reg)
  (let ((dest  (codegen-reg-to-x86reg reg)))
    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;; Generate function return using a return address
(define (codegen-return-rp cgc)
  (x86-pop cgc (x86-rdx))
  (x86-jmp cgc (x86-rdx)))

;; Generate function return using a crtable
(define (codegen-return-cr cgc crtable-offset)
  ;; rax contains ret val
  (x86-pop cgc (x86-rdx)) ;; Table must be in rdx
  (x86-mov cgc (x86-rbx) (x86-mem crtable-offset (x86-rdx)))
  (x86-mov cgc (x86-r11) (x86-imm-int crtable-offset)) ;; TODO (?)
  (x86-jmp cgc (x86-rbx)))

;;-----------------------------------------------------------------------------
;; Function calls
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Apply

;; Gen code for lco before apply (Prepare arguments from list)
(define (codegen-pre-apply cgc)
  ;; Remove lst and op from stack
  (x86-pop cgc (x86-rbx)) ;; lst
  (x86-pop cgc (x86-rax)) ;; op
  ;; Read and push all args from lst until we reach '()
  (let ((label-end  (asm-make-label #f (new-sym 'apply-args-end)))
        (label-loop (asm-make-label #f (new-sym 'apply-args-loop))))
    ;; RDI contains the number of arguments
    (x86-mov cgc (x86-rdi) (x86-imm-int 0))
    (x86-label cgc label-loop)
    ;; If current el is null, then jump to end
    (x86-cmp cgc (x86-rbx) (x86-imm-int (obj-encoding '())))
    (x86-je cgc label-end)
      ;; Else, push arg and update RDI
      (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)))           ;; Push car
      (x86-mov cgc (x86-rbx) (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx))) ;; Get cdr for next iteration
      (x86-inc cgc (x86-rdi))  ;; inc args number
      (x86-jmp cgc label-loop) ;; next iteration
    ;; All args are pushed
    (x86-label cgc label-end))
  ;; Encode nb args
  (x86-shl cgc (x86-rdi) (x86-imm-int 2))
  ;; Push closure
  (x86-push cgc (x86-rax)))

;;-----------------------------------------------------------------------------
;; Call sequence

;; Set nb args before call
(define (codegen-call-set-nbargs cgc nb)
  (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb))))

;; Generate function call using a single entry point
(define (codegen-call-ep cgc nb-args)
  (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; Get closure
  (if nb-args ;; If nb-args is given, move encoded number in rdi. Else, nb-args is already encoded in rdi
      (x86-mov cgc (x86-rdi) (x86-imm-int (* 4 nb-args))))
  (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
  (x86-jmp cgc (x86-rax)))

;;; Generate function call using a cctable and generic entry point
(define (codegen-call-cc-gen cgc)
  (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; Get closure
  (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
  (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rax)))
  (x86-jmp cgc (x86-rax)))

;; Generate function call using a cctable and specialized entry point
(define (codegen-call-cc-spe cgc idx ctx-imm nb-args)
;; TODO regalloc: WIP
    ;; Closure is in rax
    (let ((cct-offset (* 8 (+ 2 idx))))
      ;; 1 - Put ctx in r11
      (x86-mov cgc (x86-r11) (x86-imm-int ctx-imm))
      ;; 2- Get cc-table
      (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
      ;; 3 - If opt-max-versions is not #f, a generic version could be called. So we need to give nb-args
      (if opt-max-versions ;; TODO(?)
          (x86-mov cgc (x86-rdi) (x86-imm-int (* 4 nb-args))))
      ;; 4 - Get entry point in cc-table
      (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax)))
      ;; 5 - Jump to entry point
      (x86-jmp cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; Call continuation

;; Load continuation using specialized return points
(define (codegen-load-cont-cr cgc crtable-loc)
  (x86-mov cgc (x86-rax) (x86-imm-int crtable-loc))
  (x86-push cgc (x86-rax)))

(define (codegen-load-cont-rp cgc label-load-ret label-cont-stub)
  (x86-label cgc label-load-ret)
  (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref label-cont-stub 1)))
  (x86-push cgc (x86-rax)))

;;-----------------------------------------------------------------------------
;; Operators
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; N-ary arithmetic operators

;; Gen code for arithmetic operation on int/int
(define (codegen-num-ii cgc op reg lleft lright)
  (let ((labels-overflow (add-callback #f 0 (lambda (ret-addr selector)
                                              (error ERR_ARR_OVERFLOW))))
        (dest    (codegen-reg-to-x86reg reg))
        (opleft  (codegen-loc-to-x86opnd lleft))
        (opright (codegen-loc-to-x86opnd lright)))

    (if (not (eq? dest opleft))
        (x86-mov cgc dest opleft))

    (cond ((eq? op '+) (x86-add cgc dest opright))
          ((eq? op '-) (x86-sub cgc dest opright))
          ((eq? op '*) (x86-sar cgc dest (x86-imm-int 2))
                       (x86-imul cgc dest opright))
          (else (error "NYI" op)))
    (x86-jo cgc (list-ref labels-overflow 0))))

;; Gen code for arithmetic operation on float/float (also handles int/float and float/int)
(define (codegen-num-ff cgc op reg lleft leftint? lright rightint?)

  (let ((dest    (codegen-reg-to-x86reg reg))
        (opleft  (codegen-loc-to-x86opnd lleft))
        (opright (codegen-loc-to-x86opnd lright)))

    ;; Alloc result flonum
    (gen-allocation cgc #f STAG_FLONUM 2)

    (let ((x86-op (cdr (assoc op `((+ . ,x86-addsd) (- . ,x86-subsd) (* . ,x86-mulsd) (/ . ,x86-divsd))))))

    ;; Right operand
    (if rightint?
        ;; Right is register or mem and integer
        (begin (x86-mov cgc (x86-rax) opright)
               (x86-sar cgc (x86-rax) (x86-imm-int 2))  ;; untag integer
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
        ;; Left is register or mem and integer
        (begin (x86-mov cgc (x86-rax) opleft)
               (x86-sar cgc (x86-rax) (x86-imm-int 2)) ;; untag integer
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

    ;; Write header
    (x86-mov cgc (x86-rax) (x86-imm-int (mem-header 2 STAG_FLONUM)))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))

    ;; Write number
    (x86-movsd cgc (x86-mem 8 alloc-ptr) opleft)

    ;; Put
    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr)))))

;;-----------------------------------------------------------------------------
;; N-ary comparison operators

;; TODO regalloc: on ne DOIT PAS écraser les valeurs des opérandes qui peuvent servir plus tard
(define (codegen-cmp-ii cgc op reg lleft lright)
  (let ((label-end (asm-make-label #f (new-sym 'label-end)))
        (x86-op (cdr (assoc op `((< . ,x86-jl) (> . ,x86-jg) (<= . ,x86-jle) (>= . ,x86-jge) (= . ,x86-je)))))
        (dest (codegen-reg-to-x86reg reg))
        (opl  (codegen-loc-to-x86opnd lleft))
        (opr  (codegen-loc-to-x86opnd lright)))

    (if (and (ctx-loc-is-memory? lleft)
             (ctx-loc-is-memory? lright))
        (begin (x86-mov cgc (x86-rax) opl)
               (set! opl (x86-rax))))

    (x86-cmp cgc opl opr)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-op cgc label-end)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (x86-label cgc label-end)))

;; TODO regalloc: on ne DOIT PAS écraser les valeurs des opérandes qui peuvent servir plus tard
(define (codegen-cmp-ff cgc op reg lleft leftint? lright rightint?)

  (let ((label-end (asm-make-label #f (new-sym 'label-end)))
        (dest    (codegen-reg-to-x86reg reg))
        (opleft  (codegen-loc-to-x86opnd lleft))
        (opright (codegen-loc-to-x86opnd lright))
        (x86-op  (cdr (assoc op `((< . ,x86-jae) (> . ,x86-jbe) (<= . ,x86-ja) (>= . ,x86-jb) (= . ,x86-jne))))))

    (if leftint?
        ;; Left is integer, the compiler converts it to double precision FP
        (begin (x86-mov cgc dest opleft)
               (x86-sar cgc dest (x86-imm-int 2))  ;; untag integer
               (x86-cvtsi2sd cgc (x86-xmm0) dest)) ;; convert to double
        ;; Left is double precision FP
        (if (ctx-loc-is-memory? opleft)
            (begin (x86-mov cgc dest opleft)
                   (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) dest)))
            (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) opleft))))

    (if rightint?
        ;; Right is integer, the compiler converts it to double precision FP
        (begin (x86-mov cgc (x86-rax) opright)
               (x86-sar cgc (x86-rax) (x86-imm-int 2))
               (x86-cvtsi2sd cgc (x86-xmm1) (x86-rax))
               (x86-comisd cgc (x86-xmm0) (x86-xmm1)))
        ;; Right is double precision FP
        (if (ctx-loc-is-memory? opright)
            (begin (x86-mov cgc (x86-rax) opright)
                   (x86-comisd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
            (begin (x86-movsd (x86-xmm1) opright)
                   (x86-comisd cgc (x86-xmm0) (x86-xmm1)))))

    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (x86-op cgc label-end)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-label cgc label-end)))



;  (let ((label-jump (asm-make-label #f (new-sym 'label-jump)))
;        ;; DO NOT USE jg* and jl* WITH FP VALUES !
;        (x86-op (cdr (assoc op `((< . ,x86-jae) (> . ,x86-jbe) (<= . ,x86-ja) (>= . ,x86-jb) (= . ,x86-jne))))))
;
;    (x86-mov cgc (x86-rax) (x86-mem (* 8 lidx) (x86-rsp)))
;    (x86-mov cgc (x86-rbx) (x86-mem (* 8 ridx) (x86-rsp)))
;
;    (if leftint?
;        ;; Left is integer, the compiler converts it to double precision FP
;        (begin (x86-sar cgc (x86-rax) (x86-imm-int 2))  ;; untag integer
;               (x86-cvtsi2sd cgc (x86-xmm0) (x86-rax))) ;; convert to double
;        ;; Left is double precision FP
;        (x86-movsd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
;    (if rightint?
;        ;; Right is integer, the compiler converts it to double precision FP
;        (begin (x86-sar cgc (x86-rbx) (x86-imm-int 2))
;               (x86-cvtsi2sd cgc (x86-xmm1) (x86-rbx))
;               (x86-comisd cgc (x86-xmm0) (x86-xmm1)))
;        ;; Right is double precision FP
;        (x86-comisd cgc (x86-xmm0) (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx))))
;    (x86-label cgc label-jump)
;    (x86-op cgc (get-stub-label label-jump ctx))))

;;-----------------------------------------------------------------------------
;; Binary operators

(define (codegen-binop cgc op label-div0 reg lleft lright)
  (let* ((dest (codegen-reg-to-x86reg reg))
         (lopnd (codegen-loc-to-x86opnd lleft))
         (ropnd (codegen-loc-to-x86opnd lright))

         ;; TODO: save original opnd to restore rdx if needed
         (ordest dest)
         (orlopnd lopnd)
         (orropnd ropnd))

    (if (and (not (eq? ordest  (x86-rdx)))
             (not (eq? orlopnd (x86-rdx)))
             (not (eq? orropnd (x86-rdx))))
        (x86-push cgc (x86-rdx)))

    (x86-mov cgc (x86-rax) lopnd)

    (if (or (eq? ropnd (x86-rdx))
            (ctx-loc-is-memory? lright))
        (begin (x86-mov cgc dest (x86-rdx))
               (set! ropnd dest)))

    (x86-sar cgc (x86-rax) (x86-imm-int 2))
    (x86-sar cgc ropnd (x86-imm-int 2))
    (x86-cmp cgc ropnd (x86-imm-int 0)) ;; Check '/0'
    (x86-je  cgc label-div0)
    (x86-cqo cgc)
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

    (if (and (not (eq? ordest  (x86-rdx)))
             (not (eq? orlopnd (x86-rdx)))
             (not (eq? orropnd (x86-rdx))))
        (x86-pop cgc (x86-rdx)))))

;;-----------------------------------------------------------------------------
;; Primitives
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; SPECIAL $$print-flonum: call gambit at the moment. TODO: write print flonum asm code
(define (codegen-print-flonum cgc)
  (x86-pop cgc (x86-rax))
  ;; NOTE: This uses Gambit function to print a flonum (because LC uses the same flonum encoding)
  (gen-print-msg cgc (x86-rax) #f #f)
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

;;-----------------------------------------------------------------------------
;; not
(define (codegen-not cgc reg lval)
  (let ((label-done
          (asm-make-label cgc (new-sym 'done)))
        (dest (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd lval)))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (x86-cmp cgc opval dest)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-je  cgc label-done)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f))) ;; TODO: useless ?
    (x86-label cgc label-done)))

;;-----------------------------------------------------------------------------
;; eq?
(define (codegen-eq? cgc reg lleft lright)
  (let ((label-done (asm-make-label cgc (new-sym 'done)))
        (dest  (codegen-reg-to-x86reg reg))
        (opleft  (codegen-loc-to-x86opnd lleft))
        (opright (codegen-loc-to-x86opnd lright)))
    ;; If both are mem, move one in rax
    (if (and (ctx-loc-is-memory? lleft)
             (ctx-loc-is-memory? lright))
        (begin (x86-mov cgc (x86-rax) opleft)
               (set! opleft (x86-rax))))
    (x86-cmp cgc opleft opright)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-je  cgc label-done)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (x86-label cgc label-done)))

;;-----------------------------------------------------------------------------
;; car/cdr
(define (codegen-car/cdr cgc op reg lval)
  (let ((offset
          (if (eq? op 'car)
              (-  8 TAG_MEMOBJ)
              (- 16 TAG_MEMOBJ)))
        (dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd lval)))

    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (x86-mov cgc dest (x86-mem offset opval))))

;;-----------------------------------------------------------------------------
;; set-car!/set-cdr!
(define (codegen-scar/scdr cgc op reg lpair lval)
  (let ((offset
          (if (eq? op 'set-car!)
              (-  8 TAG_MEMOBJ)
              (- 16 TAG_MEMOBJ)))
        (dest (codegen-reg-to-x86reg reg))
        (oppair (codegen-loc-to-x86opnd lpair))
        (opval  (codegen-loc-to-x86opnd lval)))

    (if (ctx-loc-is-memory? lpair)
        (begin (x86-mov cgc (x86-rax) oppair)
               (set! oppair (x86-rax))))

    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc dest opval)
               (set! opval dest)))

    (x86-mov cgc (x86-mem offset oppair) opval)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; current-input/output-port
(define (codegen-current-io-port cgc op reg)
  (let ((block-offset (if (eq? op 'current-output-port) 8 24))
        (dest  (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (+ TAG_MEMOBJ block-offset block-addr)))))

;;-----------------------------------------------------------------------------
;; close-input/output-port
(define (codegen-close-io-port cgc reg lport)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opport (codegen-loc-to-x86opnd lport)))
    ;; Mov port to rax for syscall
    (x86-mov cgc (x86-rax) opport)
    (gen-syscall-close cgc)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; open-input/output-port
(define (codegen-open-io-file cgc op reg lval)
  (let* ((direction   (if (eq? op 'open-output-file) 'out 'in))
         (stag        (if (eq? direction 'in) STAG_IPORT STAG_OPORT))
         (header-word (mem-header 2 stag))
         (dest  (codegen-reg-to-x86reg reg))
         (opval (codegen-loc-to-x86opnd lval)))
    ;; Move operand to rax for syscall
    (x86-mov cgc (x86-rax) opval)
    ;; Gen 'open' syscall, file descriptor in rax
    (gen-syscall-open cgc direction)
    ;; Allocate port object
    (x86-shl cgc (x86-rax) (x86-imm-int 2)) ;; Encode descriptor (in case gen-alloc triggers GC)
    (x86-push cgc (x86-rax))
    (gen-allocation cgc #f stag 2)
    ;; Mov header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Mov descriptor
    (x86-pop cgc (x86-rax))
    (x86-shr cgc (x86-rax) (x86-imm-int 2)) ;; Decode descriptor
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
    ;;; Tag & push
    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;;-----------------------------------------------------------------------------
;; eof-object?
(define (codegen-eof? cgc reg lval)
  (let ((label-end (asm-make-label #f (new-sym 'label-end)))
        (dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd lval)))

    ;; If value is in memory, move it to rax (can't compare m64 and imm64)
    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))
    (x86-cmp cgc opval (x86-imm-int ENCODING_EOF))
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (x86-jne cgc label-end)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-label cgc label-end)))

;;-----------------------------------------------------------------------------
;; read-char
(define (codegen-read-char cgc reg lport)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opport (codegen-loc-to-x86opnd lport)))
    ;; Mov port to rax for syscall
    (x86-mov cgc (x86-rax) opport)
    ;; Gen 'read' syscall (read 1 byte), encoded value (char or eof) in rax
    (gen-syscall-read-char cgc)
    ;; Push encoded result
    (x86-mov cgc dest (x86-rax))))

;;-----------------------------------------------------------------------------
;; write-char
(define (codegen-write-char cgc reg lchar lport)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opport (codegen-loc-to-x86opnd lport))
        (opchar (codegen-loc-to-x86opnd lchar)))

    ;; Char on stack for syscall
    (if (ctx-loc-is-memory? lchar)
        (begin (x86-mov cgc (x86-rax) opchar)
               (set! opchar (x86-rax))))
    (x86-push cgc opchar)
    ;; Mov port to rax for syscall
    (x86-mov cgc (x86-rax) opport)
    ;; Gen 'read' syscall, encoded value (char or eof) in rax
    (gen-syscall-write-char cgc)
    (x86-pop cgc (x86-rax)) ;; Pop char
    ;; Put encoded result
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; char->integer/integer->char
(define (codegen-ch<->int cgc op reg lval)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd lval)))

    (if (not (eq? dest opval))
      (x86-mov cgc dest opval))

    (if (eq? op 'char->integer)
        (x86-xor cgc dest (x86-imm-int TAG_SPECIAL))
        (x86-or  cgc dest (x86-imm-int TAG_SPECIAL)))))

;;-----------------------------------------------------------------------------
;; make-string
(define (codegen-make-string cgc reg llen lval)
  (let* ((header-word (mem-header 3 STAG_STRING))
         (dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd llen))
         (opval (if lval (codegen-loc-to-x86opnd lval) #f))
         (label-loop (asm-make-label #f (new-sym 'make-string-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-string-end))))

    ;; Len is encoded in rax (if (make-vector 3) rax=12)
    ;; Nb chars to byte size
    (x86-mov cgc (x86-rax) oplen)
    (x86-shr cgc (x86-rax) (x86-imm-int 2))
    (x86-and cgc (x86-rax) (x86-imm-int (bitwise-not 7)))
    (x86-shr cgc (x86-rax) (x86-imm-int 1))

    ;; Alloc
    (gen-allocation cgc #f STAG_STRING 3 #t)

    ;; Dest reg = len
    (x86-mov cgc dest oplen)
    (x86-shr cgc dest (x86-imm-int 2)) ;; decode length

    ;; Move init val to register
    (if opval
        (begin (x86-mov cgc (x86-rax) opval)
               (x86-shr cgc (x86-rax) (x86-imm-int 2)))
        (x86-mov cgc (x86-rax) (x86-imm-int (quotient (obj-encoding #\0) 4))))

    (x86-label cgc label-loop)
    ;; if dest == 0 then jump to end
    (x86-cmp cgc dest (x86-imm-int 0))
    (x86-je cgc label-end)

      (x86-mov cgc (x86-mem 15 alloc-ptr dest) (x86-al))
      (x86-dec cgc dest)
      (x86-jmp cgc label-loop)

    ;; END:
    (x86-label cgc label-end)

    ;; Write length
    (if (ctx-loc-is-memory? llen)
        (begin (x86-mov cgc (x86-rax) oplen)
               (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
        (x86-mov cgc (x86-mem 8 alloc-ptr) oplen))

    ;; Len is encoded in rax (if (make-vector 3) rax=12)
    ;; Nb chars to byte size
    (x86-mov cgc (x86-rax) oplen)
    (x86-shr cgc (x86-rax) (x86-imm-int 2))
    (x86-and cgc (x86-rax) (x86-imm-int (bitwise-not 7)))
    (x86-shr cgc (x86-rax) (x86-imm-int 1))
    ;; Write header
    (x86-shl cgc (x86-rax) (x86-imm-int 6))
    (x86-add cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))

    ;; Put str
    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;;-----------------------------------------------------------------------------
;; make-vector
(define (codegen-make-vector cgc reg llen lval)

  (let* ((header-word (mem-header 2 STAG_VECTOR))
         (dest  (codegen-reg-to-x86reg reg))
         (oplen (codegen-loc-to-x86opnd llen))
         (opval (if lval (codegen-loc-to-x86opnd lval) #f))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    ;; Len is encoded in rax (if (make-vector 3) rax=12)
    (x86-mov cgc (x86-rax) oplen)
    ;; Alloc
    (gen-allocation cgc #f STAG_VECTOR 2 #t)

    (x86-mov cgc (x86-rax) oplen)
    (x86-shl cgc (x86-rax) (x86-imm-int 1))
    ;; If opval not in register, save rbx and use it
    (cond ((not lval)
             ;; No init val given, then use rbx with value 0
             (x86-push cgc (x86-rbx))
             (x86-mov cgc (x86-rbx) (x86-imm-int 0))
             (set! opval (x86-rbx)))
          ((not (ctx-loc-is-register? lval))
             ;; Init value id in memory, then use rbx with given init valuesss
             (x86-push cgc (x86-rbx))
             (x86-mov cgc (x86-rbx) opval)
             (set! opval (x86-rbx))))

    (x86-label cgc label-loop)
    (x86-cmp cgc (x86-rax) (x86-imm-int 0))
    (x86-je cgc label-end)

      (x86-mov cgc (x86-mem 8 alloc-ptr (x86-rax)) opval)
      (x86-sub cgc (x86-rax) (x86-imm-int 8))
      (x86-jmp cgc label-loop)

    (x86-label cgc label-end)

    ;; Restore rbx
    (if (or (not lval)
            (not (ctx-loc-is-register? lval)))
        (x86-pop cgc (x86-rbx)))

    ;; Write encoded length
    (if (ctx-loc-is-memory? oplen)
        (begin (x86-mov cgc (x86-rax) oplen)
               (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
        (x86-mov cgc (x86-mem 8 alloc-ptr) oplen))
    ;; Write header
    (x86-mov cgc (x86-rax) oplen)
    (x86-shl cgc (x86-rax) (x86-imm-int 6))
    (x86-add cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Put vector
    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;;-----------------------------------------------------------------------------
;; string->symbol
(define (codegen-str->sym cgc reg lstr)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd lstr)))

    (x86-push cgc opstr)
    (gen-interned-symbol cgc)
    (x86-pop cgc dest)))

;;-----------------------------------------------------------------------------
;; symbol->string
(define (codegen-sym->str cgc reg lsym)

  (let ((dest (codegen-reg-to-x86reg reg))
        (opsym (codegen-loc-to-x86opnd lsym)))

    (if (not (ctx-loc-is-register? lsym))
        (error "NYI codegen"))

    ;; Alloc string
    (x86-mov cgc dest opsym)
    (x86-sub cgc dest (x86-imm-int TAG_MEMOBJ))
    (x86-mov cgc dest (x86-mem 0 dest))
    (x86-shr cgc dest (x86-imm-int 8))
    (x86-shl cgc dest (x86-imm-int 2)) ;; Length
    (x86-mov cgc (x86-rax) dest) ;; Move for alloc
    (gen-allocation cgc #f STAG_STRING 0 #t)

    ;; Write len
    (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opsym))
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))

    ;; Write header
    (x86-mov cgc (x86-rax) (x86-mem (- 0 TAG_MEMOBJ) opsym))
    (x86-add cgc (x86-rax) (x86-imm-int (arithmetic-shift (- STAG_STRING STAG_SYMBOL) 3)))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))

    ;; qword size in dest
    (x86-shl cgc dest (x86-imm-int 1))

    (let ((label-end  (asm-make-label #f (new-sym 'label-end)))
          (label-loop (asm-make-label #f (new-sym 'label-loop))))

      (x86-label cgc label-loop)
      (x86-cmp cgc dest (x86-imm-int 16))
      (x86-jle cgc label-end)

        ;; Copy qword
        (x86-mov cgc (x86-rax) (x86-mem (* -1 (+ 8 TAG_MEMOBJ)) opsym dest))
        (x86-mov cgc (x86-mem -8 alloc-ptr dest) (x86-rax))
        (x86-sub cgc dest (x86-imm-int 8))
        (x86-jmp cgc label-loop)

      (x86-label cgc label-end))

    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;;-----------------------------------------------------------------------------
;; vector/string-length
(define (codegen-vec/str-length cgc reg lval)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd lval)))

    (if (ctx-loc-is-memory? opval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) opval))))

;;-----------------------------------------------------------------------------
;; vector-ref
(define (codegen-vector-ref cgc reg lvec lidx)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opvec (codegen-loc-to-x86opnd lvec))
        (opidx (codegen-loc-to-x86opnd lidx)))

    (cond ;; Both operands are in memory, use rax and dest registers
          ((and (ctx-loc-is-memory? lvec)
                (ctx-loc-is-memory? lidx))
             (x86-mov cgc dest opvec)
             (x86-mov cgc (x86-rax) opidx)
             (set! opvec dest)
             (set! opidx (x86-rax)))
          ;; Vector is in memory, use rax
          ((ctx-loc-is-memory? lvec)
             (x86-mov cgc (x86-rax) opvec)
             (set! opvec (x86-rax)))
          ;; Index is in memory, use rax
          ((ctx-loc-is-memory? lidx)
             (x86-mov cgc (x86-rax) opidx)
             (set! opidx (x86-rax))))

    ;; mov dest, [opvec + opidx*2 + 15]
    (x86-mov cgc dest (x86-mem (- 16 TAG_MEMOBJ) opvec opidx 1))))

;;-----------------------------------------------------------------------------
;; string-ref
(define (codegen-string-ref cgc reg lstr lidx)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd lstr))
        (opidx (codegen-loc-to-x86opnd lidx)))

  (if (ctx-loc-is-memory? lidx)
      (begin (x86-mov cgc (x86-rax) opidx)
             (set! opidx (x86-rax))))

  (if (ctx-loc-is-memory? lstr)
      (begin (x86-mov cgc dest opstr)
             (set! opstr dest)))

  (x86-shr cgc opidx (x86-imm-int 2)) ;; Decode position
  (x86-mov cgc (x86-al) (x86-mem (- 16 TAG_MEMOBJ) opidx opstr)) ;; Get Char
  (x86-and cgc (x86-rax) (x86-imm-int 255)) ;; Clear bits before al
  (x86-shl cgc (x86-rax) (x86-imm-int 2)) ;; Encode char
  (x86-add cgc (x86-rax) (x86-imm-int TAG_SPECIAL))
  (x86-mov cgc dest (x86-rax))))

;;-----------------------------------------------------------------------------
;; vector-set!
(define (codegen-vector-set! cgc reg lvec lidx lval)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opvec (codegen-loc-to-x86opnd lvec))
        (opidx (codegen-loc-to-x86opnd lidx))
        (opval (codegen-loc-to-x86opnd lval))
        (regsaved #f))

  (if (ctx-loc-is-memory? lvec)
      (begin (x86-mov cgc (x86-rax) opvec)
             (set! opvec (x86-rax))))

  (if (ctx-loc-is-memory? lidx)
      (begin (x86-mov cgc dest opidx)
             (set! opidx dest)))

  (if (ctx-loc-is-memory? lval)
      (if (and (eq? opvec (x86-rax))
               (eq? opidx dest))
          ;; both tmp regs are already used
          (let ((reg (if (eq? dest (x86-rbx)) (x86-rcx) (x86-rbx))))
            (x86-push cgc reg)
            (x86-mov cgc reg opval)
            (set! opval reg)
            (set! regsaved reg))
          ;; At least one is free, use it
          (if (eq? opvec (x86-rax))
              (begin (x86-mov cgc dest opval)
                     (set! opval dest))
              (begin (x86-mov cgc (x86-rax) opval)
                     (set! opval (x86-rax))))))

  (x86-shl cgc opidx (x86-imm-int 1))
  (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) opvec opidx) opval)
  (x86-mov cgc dest (x86-imm-int ENCODING_VOID))

  (if regsaved
      (x86-pop cgc regsaved))))

;;-----------------------------------------------------------------------------
;; string-set!
(define (codegen-string-set! cgc reg lstr lidx lchr)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd lstr))
        (opidx (codegen-loc-to-x86opnd lidx))
        (opchr (codegen-loc-to-x86opnd lchr))
        (regsaved #f))

  (x86-mov cgc (x86-rax) (x86-imm-int 1000000000))

  ;; Move idx to dest register if in memory
  (if (ctx-loc-is-memory? lidx)
      (begin (x86-mov cgc dest opidx)
             (set! opidx dest)))

  ;; Char must be in rax
  (x86-mov cgc (x86-rax) opchr)

  ;; Move str in a register if in memory
  (if (ctx-loc-is-memory? lstr)
      (if (ctx-loc-is-memory? lidx)
          ;; dest register is already used
          (let ((reg (if (eq? opidx (x86-rbx)) (x86-rdx) (x86-rbx))))
            (x86-push cgc reg)
            (x86-mov cgc reg opstr)
            (set! opstr reg)
            (set! regsaved reg))
          ;; use dest register
          (begin (x86-mov cgc dest opstr)
                 (set! opstr dest))))

  (x86-shr cgc (x86-rax) (x86-imm-int 2)) ;; char
  (x86-shr cgc opidx (x86-imm-int 2)) ;; get idx bytes
  (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) opstr opidx) (x86-al))
  (x86-shl cgc opidx (x86-imm-int 2)) ;; Restore idx

  (x86-mov cgc dest (x86-imm-int ENCODING_VOID))

  (if regsaved
      (x86-pop cgc regsaved))))





;;-----------------------------------------------------------------------------
;; list
(define (codegen-list cgc nb-els)
  (let ((label-list-loop (asm-make-label #f (new-sym 'list-loop)))
        (label-list-end  (asm-make-label #f (new-sym 'list-end))))
    ;; Remainging length in rdi
    (x86-mov cgc (x86-rdi) (x86-imm-int nb-els))
    ;; cdr on top of stack
    (x86-push cgc (x86-imm-int (obj-encoding '())))
    ;; LOOP
    (x86-label cgc label-list-loop)
    (x86-cmp cgc (x86-rdi) (x86-imm-int 0))
    (x86-je cgc label-list-end)

      (gen-allocation cgc #f STAG_PAIR 3)
      (x86-pop cgc (x86-rbx)) ;; pop cdr
      (x86-pop cgc (x86-rdx)) ;; pop car
      (x86-mov cgc (x86-mem  8 alloc-ptr) (x86-rdx))
      (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rbx))
      (x86-mov cgc (x86-rbx) (x86-imm-int (mem-header 3 STAG_PAIR)))
      (x86-mov cgc (x86-mem  0 alloc-ptr) (x86-rbx))
      (x86-lea cgc (x86-rbx) (x86-mem TAG_MEMOBJ alloc-ptr))
      (x86-push cgc (x86-rbx))
      (x86-sub cgc (x86-rdi) (x86-imm-int 1))
      (x86-jmp cgc label-list-loop)

    (x86-label cgc label-list-end)))

;;-----------------------------------------------------------------------------
;; Others
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; TCO
(define (codegen-tco-move-arg cgc from to)
  (x86-mov cgc (x86-rax) (x86-mem (* from 8) (x86-rsp)))
  (x86-mov cgc (x86-mem (* to 8) (x86-rsp))  (x86-rax)))

;;-----------------------------------------------------------------------------
;; Mutable var (creates mutable object, write variable and header and replace local with mutable object)
(define (codegen-mutable cgc lval)
  (let ((header-word (mem-header 2 STAG_MOBJECT))
        (opval (codegen-loc-to-x86opnd lval)))

    ;; Alloc mutable
    (gen-allocation cgc #f STAG_MOBJECT 2)
    ;; Write variable
    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc (x86-rax) opval)
               (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
        (x86-mov cgc (x86-mem 8 alloc-ptr) opval))
    ;; Write header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Replace local
    (if (ctx-loc-is-memory? lval)
        (begin (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
               (x86-mov cgc opval (x86-rax)))
        (x86-lea cgc opval (x86-mem TAG_MEMOBJ alloc-ptr)))))
