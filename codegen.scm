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
(define codegen-regmap
  (foldr (lambda (el r)
           (cons (cons (cons 'r el)
                       (list-ref regalloc-regs el))
                 r))
         '()
         (build-list (length regalloc-regs) (lambda (l) l))))

(define alloc-ptr  (x86-r9))
(define global-ptr (x86-r8))

;; NOTE: temporary register is always rax
;; NOTE: selector is always rcx
;; NOTE: stack pointer is always rsp

;;-----------------------------------------------------------------------------
;; x86 Codegen utils

(define-macro (neq? l r)
  `(not (eq? ,l ,r)))

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

;; TODO rename
(define (pick-reg used-regs)
  (define (pick-reg-h regs used)
    (if (null? regs)
        (error "Internal error")
        (if (not (member (car regs) used))
            (car regs)
            (pick-reg-h (cdr regs) used))))
  (pick-reg-h regalloc-regs used-regs))

;;-----------------------------------------------------------------------------
;; TODO

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

;; Unbox a value in src register to dst register
;; update operand
(define-macro (unbox! dst src)
  `(begin (x86-mov cgc ,dst (x86-mem (- 8 TAG_MEMOBJ) ,src))
          (set! ,(car src) ,(car dst))))

;;
(define-macro (chk-unbox! dst src mut?)
  `(if ,mut?
       (unbox! ,dst ,src)))

;; Find an unused register, save it, unmem from src to this register
;; update saved set
(define-macro (pick-unmem! src used-regs)
  (let ((sym (gensym)))
    `(let ((,sym (pick-reg ,used-regs)))
       (x86-push cgc ,sym)
       (set! fs (+ fs 1))
       (set! ##registers-saved## (cons ,sym ##registers-saved##))
       (unmem! ((lambda () ,sym)) ,src))))


;; Find an unused register, save it, unbox from src to this register
;; update saved set
(define-macro (pick-unbox! src used-regs)
  (let ((sym (gensym)))
    `(let ((,sym (pick-reg ,used-regs)))
       (x86-push cgc ,sym)
       (set! fs (+ fs 1))
       (set! ##registers-saved## (cons ,sym ##registers-saved##))
       (if (x86-reg? ,src)
           (unbox! ((lambda () ,sym)) ,src)
           (begin (x86-mov cgc ,sym ,src)
                  (unbox! ((lambda () ,sym)) ,src))))))


;; Check if src is in memory. If so, unmem
;; then, check if src is mutable. Is so, unbox
(define-macro (chk-unmem-unbox! dst src mut?)
  `(begin
     (if (x86-mem? ,src)
         (unmem! ,dst ,src))
     (if ,mut?
         (unbox! ,dst ,src))))

;; Check if src is in memory AND mutable. If so, unmem and unbox
;; Else, if src is mutable but not in memory, unbox directly
;; (Same as chk-unmem-unbox! no moves are generated if src is in memory and not mutable)
(define-macro (chk-unmem&unbox! dst src mut?)
  `(cond ((and (x86-mem? ,src) ,mut?)
          (chk-unmem-unbox! ,dst ,src ,mut?))
         (,mut?
            (unbox! ,dst ,src))))

;; Check if src is in memory. If so, pick-unmem (unmem in an available reg)
;; then, check if src is mutable. If so, unbox in register used by pick-unmem
;; or pick-unbox (unbox in an available reg)
(define-macro (chk-pick-unmem-unbox! src mut? used-regs)
  `(begin
    (if (x86-mem? ,src)
        (pick-unmem! ,src (append ,used-regs ##registers-saved##)))
    (if ,mut?
        (if (x86-mem? ,src)
            (unbox! ,src ,src)
            (pick-unbox! ,src (append ,used-regs ##registers-saved##))))))

(define-macro (chk-pick-unmem&unbox! src mut? used-regs)
  `(cond ((and (x86-mem? ,src) ,mut?)
          (chk-pick-unmem-unbox! ,src ,mut? ,used-regs))
         (,mut?
          (pick-unbox! ,src (append ,used-regs ##registers-saved##)))))

;;
(define-macro (restore-saved)
  `(for-each (lambda (el) (x86-pop cgc el)) ##registers-saved##))

;;-----------------------------------------------------------------------------
;; Define
(define (codegen-define-id cgc)
  (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
  (x86-mov cgc (x86-mem (* 8 nb-globals) global-ptr) (x86-rax)))

(define (codegen-define-bind cgc fs pos reg lvalue)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lvalue)))
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

(define (codegen-set-global cgc reg pos lval fs)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))
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
(define (codegen-if cgc fs label-jump label-false label-true lcond mut-cond?)
  (let ((opcond (lambda () (codegen-loc-to-x86opnd fs lcond)))
        (oprax  (lambda () (x86-rax))))

    (begin-with-cg-macro

      ;;
      ;; Unmem / Unbox code
      (chk-unmem&unbox! (oprax) (opcond) mut-cond?)

      ;;
      ;;
      (x86-cmp cgc (opcond) (x86-imm-int (obj-encoding #f)))
      (x86-label cgc label-jump)
      (x86-je  cgc label-false)
      (x86-jmp cgc label-true))))

;;-----------------------------------------------------------------------------
;; Begin
(define (codegen-begin-out cgc nb-expr)
  (x86-pop  cgc (x86-rax)) ;; Pop result of last expr
  (x86-add  cgc (x86-rsp) (x86-imm-int (* 8 nb-expr))) ;; Clean stack
  (x86-push cgc (x86-rax))) ;; Push result

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

(define (codegen-loc-to-x86opnd fs loc)
  (cond ((ctx-loc-is-register? loc)
         (codegen-reg-to-x86reg loc))
        ((ctx-loc-is-memory? loc)
         (codegen-mem-to-x86mem fs loc))
        (else (error "Internal error"))))

(define (codegen-mem-to-x86mem fs mem)
  (x86-mem (* 8 (- fs (cdr mem) 1)) (x86-rsp)))

(define (codegen-reg-to-x86reg reg)
  (cdr (assoc reg codegen-regmap)))

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
(define (codegen-pair cgc fs reg lcar lcdr car-cst? cdr-cst? mut-car? mut-cdr?)

 (let ((header-word (mem-header 3 STAG_PAIR))
       (dest  (codegen-reg-to-x86reg reg))
       (opcar (and (not car-cst?) (codegen-loc-to-x86opnd fs lcar)))
       (opcdr (and (not cdr-cst?) (codegen-loc-to-x86opnd fs lcdr))))
    ;; Alloc
   (gen-allocation cgc #f STAG_PAIR 3)
    ;; Write object header
   (x86-mov cgc (x86-rax) (x86-imm-int header-word))
   (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Write car
   (cond
     (car-cst?
       (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-imm-int (obj-encoding lcar)) 64))
     ((ctx-loc-is-memory? lcar)
      (x86-mov cgc (x86-rax) opcar)
      (if mut-car? (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
      (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
     (else
       (if mut-car? (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opcar)))
       (x86-mov cgc (x86-mem 8 alloc-ptr) (if mut-car? (x86-rax) opcar))))
    ;; Write cdr
   (cond
     (cdr-cst?
       (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-imm-int (obj-encoding lcdr)) 64))
     ((ctx-loc-is-memory? lcdr)
      (x86-mov cgc (x86-rax) opcdr)
      (if mut-cdr? (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
      (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rax)))
     (else
       (if mut-cdr? (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opcdr)))
       (x86-mov cgc (x86-mem 16 alloc-ptr) (if mut-cdr? (x86-rax) opcdr))))
   (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

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
(define (codegen-prologue-rest= cgc destreg)
  (let ((dest
          (and destreg (codegen-reg-to-x86reg destreg))))
    (if dest
        (x86-mov cgc dest (x86-imm-int (obj-encoding '())))
        (x86-push cgc (x86-imm-int (obj-encoding '()))))))

;; Generate specialized function prologue with rest param and actual > formal
(define (codegen-prologue-rest> cgc fs nb-rest-stack rest-regs destreg)

  (let ((regs
          (map (lambda (el) (codegen-loc-to-x86opnd fs el))
               rest-regs))
        (dest
          (and destreg (codegen-loc-to-x86opnd fs destreg)))
        (header-word     (mem-header 3 STAG_PAIR))
        (label-loop-end (asm-make-label #f (new-sym 'prologue-loop-end)))
        (label-loop     (asm-make-label #f (new-sym 'prologue-loop))))

    ;; TODO: Only one alloc

    (x86-mov cgc (x86-r14) (x86-imm-int (obj-encoding '())))
    ;; Stack
    (x86-mov cgc (x86-r15) (x86-imm-int (obj-encoding nb-rest-stack)))
    (x86-label cgc label-loop)
    (x86-cmp cgc (x86-r15) (x86-imm-int 0))
    (x86-je cgc label-loop-end)
    (gen-allocation cgc #f STAG_PAIR 3)
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem  0 alloc-ptr) (x86-rax))
    (x86-pop cgc (x86-rax))
    (x86-mov cgc (x86-mem  8 alloc-ptr) (x86-rax))
    (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-r14))
    (x86-lea cgc (x86-r14) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-sub cgc (x86-r15) (x86-imm-int (obj-encoding 1)))
    (x86-jmp cgc label-loop)
    (x86-label cgc label-loop-end)
    ;; Regs
    (for-each
      (lambda (src)
        (gen-allocation cgc #f STAG_PAIR 3)
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem  0 alloc-ptr) (x86-rax))
        (x86-mov cgc (x86-mem  8 alloc-ptr) src)
        (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-r14))
        (x86-lea cgc (x86-r14) (x86-mem TAG_MEMOBJ alloc-ptr)))
      regs)
    ;; Dest
    (if dest
        (x86-mov cgc dest (x86-r14))
        (x86-push cgc (x86-r14)))))


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
  (x86-mov cgc (x86-rax) (x86-mem crtable-offset (x86-rdx)))
  (x86-mov cgc (x86-r11) (x86-imm-int crtable-offset)) ;; TODO (?)
  (x86-jmp cgc (x86-rax)))

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
(define (codegen-call-ep cgc nb-args ep-loc)
  ;; TODO: use call/ret if opt-entry-points opt-return-points are #f
  (if nb-args ;; If nb-args given, move encoded in rdi, else nb-args is already encoded in rdi (apply)
      (x86-mov cgc (x86-rdi) (x86-imm-int (obj-encoding nb-args))))
  (if ep-loc
      (x86-jmp cgc (x86-mem (+ (obj-encoding ep-loc) 7) #f))
      (begin (x86-mov cgc (x86-rsi) (x86-rax))
             (x86-mov cgc (x86-rbp) (x86-mem (- 8 TAG_MEMOBJ) (x86-rsi)))
             (x86-jmp cgc (x86-rbp)))))

;;; Generate function call using a cctable and generic entry point
(define (codegen-call-cc-gen cgc cctable-loc)
  (if cctable-loc
      (x86-mov cgc (x86-rbp) (x86-imm-int cctable-loc))
      (begin
        (x86-mov cgc (x86-rsi) (x86-rax))
        (x86-mov cgc (x86-rbp) (x86-mem (- 8 TAG_MEMOBJ) (x86-rsi))))) ;; Get table
  (x86-jmp cgc (x86-mem 8 (x86-rbp)))) ;; Jump to generic entry point

;; Generate function call using a cctable and specialized entry point
(define (codegen-call-cc-spe cgc idx ctx-imm nb-args cctable-loc)
    ;; Closure is in rax
    (let ((cct-offset (* 8 (+ 2 idx))))
      ;; 1 - Put ctx in r11
      (x86-mov cgc (x86-r11) (x86-imm-int ctx-imm))
      ;; 2- Get cc-table
      (if cctable-loc
          (x86-mov cgc (x86-rbp) (x86-imm-int cctable-loc))
          (begin
            (x86-mov cgc (x86-rsi) (x86-rax))
            (x86-mov cgc (x86-rbp) (x86-mem (- 8 TAG_MEMOBJ) (x86-rsi)))))
      ;; 3 - If opt-max-versions is not #f, a generic version could be called. So we need to give nb-args
      (if opt-max-versions ;; TODO(?)
          (x86-mov cgc (x86-rdi) (x86-imm-int (* 4 nb-args))))
      ;; 4 - Jump to entry point from ctable
      (x86-jmp cgc (x86-mem cct-offset (x86-rbp)))))

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
(define (codegen-num-ii cgc fs op reg lleft lright lcst? rcst? mut-left? mut-right?)

  (assert (not (and lcst? rcst?)) "Internal codegen error")

  (let ((labels-overflow (add-callback #f 0 (lambda (ret-addr selector)
                                              (error ERR_ARR_OVERFLOW))))
        (dest (codegen-reg-to-x86reg reg))
        (opleft  (lambda () (and (not lcst?) (codegen-loc-to-x86opnd fs lleft))))
        (opright (lambda () (and (not rcst?) (codegen-loc-to-x86opnd fs lright))))
        (oprax   (lambda () (x86-rax))))

   (begin-with-cg-macro

     ;;
     ;; Unmem / Unbox code
     (if (not lcst?)
         ;; if (in memory?) unmem,unbox else unbox only
         (chk-unmem&unbox! (oprax) (opleft) mut-left?))
     (if (not rcst?)
         ;; if (in memory?) unmem,unbox else unbox only
         (if (eq? (opleft) (x86-rax))
             (chk-pick-unmem&unbox! (opright) mut-right? (list dest (opleft) (opright)))
             (chk-unmem&unbox! (oprax) (opright) mut-right?)))

     ;;
     ;; Primitive code

     ;; Handle cases like 1. 2. etc...
     (if (and lcst? (flonum? lleft))
         (set! lleft (##flonum->fixnum lleft)))
     (if (and rcst? (flonum? lright))
         (set! lright (##flonum->fixnum lright)))

     (cond
       (lcst?
         (cond ((eq? op '+) (x86-mov cgc dest (opright))
                            (x86-add cgc dest (x86-imm-int (obj-encoding lleft))))
               ((eq? op '-) (x86-mov cgc dest (x86-imm-int (obj-encoding lleft)))
                            (x86-sub cgc dest (opright)))
               ((eq? op '*) (x86-imul cgc dest (opright) (x86-imm-int lleft)))))
       (rcst?
         (cond ((eq? op '+) (x86-mov cgc dest (opleft))
                            (x86-add cgc dest (x86-imm-int (obj-encoding lright))))
               ((eq? op '-) (x86-mov cgc dest (opleft))
                            (x86-sub cgc dest (x86-imm-int (obj-encoding lright))))
               ((eq? op '*) (x86-imul cgc dest (opleft) (x86-imm-int lright)))))
       (else
         (x86-mov cgc dest (opleft))
         (cond ((eq? op '+) (x86-add cgc dest (opright)))
               ((eq? op '-) (x86-sub cgc dest (opright)))
               ((eq? op '*) (x86-sar cgc dest (x86-imm-int 2))
                            (x86-imul cgc dest (opright))))))

     (x86-jo cgc (list-ref labels-overflow 0)))))

;; Gen code for arithmetic operation on float/float (also handles int/float and float/int)
(define (codegen-num-ff cgc fs op reg lleft leftint? lright rightint? lcst? rcst?)

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
    (gen-allocation cgc #f STAG_FLONUM 2)

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

    ;; Write header
     (x86-mov cgc (x86-rax) (x86-imm-int (mem-header 2 STAG_FLONUM)))
     (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))

    ;; Write number
     (x86-movsd cgc (x86-mem 8 alloc-ptr) opleft)

    ;; Put
     (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr)))))

;;-----------------------------------------------------------------------------
;; N-ary comparison operators

(define (codegen-cmp-ii cgc fs op reg lleft lright lcst? rcst? mut-left? mut-right? inline-if-labels)

  (define-macro (if-inline expr)
    `(if inline-if-labels #f ,expr))

  (assert (not (and lcst? rcst?)) "Internal codegen error")

  (let* ((x86-op  (cdr (assoc op `((< . ,x86-jl) (> . ,x86-jg) (<= . ,x86-jle) (>= . ,x86-jge) (= . ,x86-je)))))
         (x86-iop (cdr (assoc op `((< . ,x86-jg) (> . ,x86-jl) (<= . ,x86-jge) (>= . ,x86-jle) (= . ,x86-je)))))
         (x86-inline-op  (cdr (assoc op `((< . ,x86-jge) (> . ,x86-jle) (<= . ,x86-jg) (>= . ,x86-jl) (= . ,x86-jne)))))
         (x86-inline-iop (cdr (assoc op `((< . ,x86-jle) (> . ,x86-jge) (<= . ,x86-jl) (>= . ,x86-jg) (= . ,x86-jne)))))
         (dest      (if-inline (codegen-reg-to-x86reg reg)))
         (label-end (if-inline (asm-make-label #f (new-sym 'label-end))))
         (opl (lambda () (and (not lcst?) (codegen-loc-to-x86opnd fs lleft))))
         (opr (lambda () (and (not rcst?) (codegen-loc-to-x86opnd fs lright))))
         (oprax (lambda () (x86-rax)))
         (selop x86-op)
         (selinop x86-inline-op))

    (begin-with-cg-macro

      ;;
      ;; Unmem / Unbox code
      (chk-unmem&unbox! (oprax) (opl) mut-left?)

      (if (eq? (opl) (x86-rax))
          (chk-pick-unmem&unbox! (opr) mut-right? (list dest opl opr))
          (chk-unmem&unbox! (oprax) (opr) mut-right?))

      ;; if the operands are both not mutable and both in memory, use rax
      (if (and (ctx-loc-is-memory? lleft)
               (ctx-loc-is-memory? lright))
          (unmem! (oprax) (opl)))

      ;;
      ;; Primitive code

      ;; If left is a cst, swap operands and use iop
      (if lcst?
          (begin
            (set! opl opr)
            (set! opr (lambda () (x86-imm-int (obj-encoding lleft))))
            (set! selop x86-iop)
            (set! selinop x86-inline-iop)))
      (if rcst?
          (set! opr (lambda () (x86-imm-int (obj-encoding lright)))))

      ;; Handle cases like 1. 2. etc...
      (if (and lcst? (flonum? lleft))
          (set! lleft (##flonum->fixnum lleft)))
      (if (and rcst? (flonum? lright))
          (set! lright (##flonum->fixnum lright)))

      (x86-cmp cgc (opl) (opr)))

    (if inline-if-labels
        (begin (x86-label cgc (car inline-if-labels)) ;; label-jump
               (selinop cgc (caddr inline-if-labels))
               (x86-jmp cgc (cadr inline-if-labels))) ;; jmp label-true
        (begin (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
               (selop cgc label-end)
               (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
               (x86-label cgc label-end)))))

(define (codegen-cmp-ff cgc fs op reg lleft leftint? lright rightint? lcst? rcst? inline-if-labels)

  (define-macro (if-inline expr)
    `(if inline-if-labels #f ,expr))

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
    (if inline-if-labels
        (begin (x86-label cgc (car inline-if-labels))
               (x86-op cgc (caddr inline-if-labels))
               (x86-jmp cgc (cadr inline-if-labels)))
        (begin (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
               (x86-op cgc label-end)
               (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
               (x86-label cgc label-end)))))

;;-----------------------------------------------------------------------------
;; Binary operators

(define (codegen-binop cgc fs op label-div0 reg lleft lright)

  ;; TODO: rewrite with mutable support
  (let* ((dest (codegen-reg-to-x86reg reg))
         (mod (if (eq? dest (x86-rdx)) 1 2))
         (lopnd (codegen-loc-to-x86opnd (+ fs mod) lleft))
         (ropnd (codegen-loc-to-x86opnd (+ fs mod) lright))
         ;; TODO: save original opnd to restore rdx if needed
         (ordest dest)
         (orlopnd lopnd)
         (orropnd ropnd))

    (if (and (neq? ordest  (x86-rdx)))
        (x86-push cgc (x86-rdx)))

    (let ((REG (pick-reg (list (x86-rax) (x86-rdx) lopnd ropnd dest))))
      (x86-push cgc REG)

      (x86-mov cgc (x86-rax) lopnd)
      (x86-mov cgc REG ropnd)

      (x86-sar cgc (x86-rax) (x86-imm-int 2))
      (x86-sar cgc REG (x86-imm-int 2))
      (x86-cmp cgc REG (x86-imm-int 0)) ;; Check '/0'
      (x86-je  cgc label-div0)
      (x86-cqo cgc)
      (x86-idiv cgc REG)

      (cond ((eq? op 'quotient)
             (x86-shl cgc (x86-rax) (x86-imm-int 2))
             (x86-mov cgc dest (x86-rax)))
            ((eq? op 'remainder)
             (x86-shl cgc (x86-rdx) (x86-imm-int 2))
             (x86-mov cgc dest (x86-rdx)))
            ((eq? op 'modulo)
             (x86-mov cgc (x86-rax) (x86-rdx))
             (x86-add cgc (x86-rax) REG)
             (x86-cqo cgc)
             (x86-idiv cgc REG)
             (x86-shl cgc (x86-rdx) (x86-imm-int 2))
             (x86-mov cgc dest (x86-rdx))))

      (x86-pop cgc REG)

      (if (and (neq? ordest  (x86-rdx)))
          (x86-pop cgc (x86-rdx))))))

    ;(if (and (neq? ordest  (x86-rdx)))
    ;    (begin (x86-push cgc (x86-rdx))
    ;           ;; Update loc with updated fs
    ;           (set! lopnd (codegen-loc-to-x86opnd (+ fs 1) lleft))
    ;           (set! ropnd (codegen-loc-to-x86opnd (+ fs 1) lright))))
    ;
    ;(x86-mov cgc (x86-rax) lopnd)
    ;
    ;(begin (x86-mov cgc dest (x86-rdx))
    ;       (set! ropnd dest))
    ;
    ;(x86-sar cgc (x86-rax) (x86-imm-int 2))
    ;(x86-sar cgc ropnd (x86-imm-int 2))
    ;(x86-cmp cgc ropnd (x86-imm-int 0)) ;; Check '/0'
    ;(x86-je  cgc label-div0)
    ;(x86-cqo cgc)
    ;(x86-idiv cgc ropnd)
    ;
    ;(cond ((eq? op 'quotient)
    ;       (x86-shl cgc (x86-rax) (x86-imm-int 2))
    ;       (x86-mov cgc dest (x86-rax)))
    ;      ((eq? op 'remainder)
    ;       (x86-shl cgc (x86-rdx) (x86-imm-int 2))
    ;       (x86-mov cgc dest (x86-rdx)))
    ;      ((eq? op 'modulo)
    ;       (x86-mov cgc (x86-rax) (x86-rdx))
    ;       (x86-add cgc (x86-rax) ropnd)
    ;       (x86-cqo cgc)
    ;       (x86-idiv cgc ropnd)
    ;       (x86-shl cgc (x86-rdx) (x86-imm-int 2))
    ;       (x86-mov cgc dest (x86-rdx))))
    ;
    ;(if (and (neq? ordest  (x86-rdx)))
    ;    (x86-pop cgc (x86-rdx)))))

;;-----------------------------------------------------------------------------
;; Primitives
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------

;; SPECIAL $$print-flonum: call gambit at the moment. TODO: write print flonum asm code
(define (codegen-print-flonum cgc fs reg loc)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opnd (codegen-loc-to-x86opnd fs loc)))

    (x86-mov cgc (x86-rax) opnd)
    ;; NOTE: This uses Gambit function to print a flonum (because LC uses the same flonum encoding)
    (gen-print-msg cgc (x86-rax) #f #f)
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; not
(define (codegen-not cgc fs reg lval mut-val?)
  (let ((label-done
          (asm-make-label cgc (new-sym 'done)))
        (dest (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))

    (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
    (cond ((and (ctx-loc-is-memory? lval) mut-val?)
           (x86-mov cgc (x86-rax) opval) ;; move mem to reg
           (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))) ;; unbox mutable
           (set! opval (x86-rax)))
          (mut-val?
             (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opval))
             (set! opval (x86-rax))))
    (x86-cmp cgc opval dest)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
    (x86-je  cgc label-done)
    (x86-mov cgc dest (x86-imm-int (obj-encoding #f))) ;; TODO: useless ?
    (x86-label cgc label-done)))

;;-----------------------------------------------------------------------------
;; eq?

;; TODO
(define (codegen-unbox cgc dst src)
  (if (x86-mem? src)
      (begin (x86-mov cgc dst src)
             (x86-mov cgc dst (x86-mem (- 8 TAG_MEMOBJ) dst)))
      (x86-mov cgc dst (x86-mem (- 8 TAG_MEMOBJ) src))))

;; Unbox in-place
;; TODO: remove codegen-unbox and use codegen-nunbox when all codgen functions use cg macros
(define (codegen-nunbox cgc fs loc)
  (let ((opnd (codegen-loc-to-x86opnd fs loc)))
    (if (ctx-loc-is-memory? loc)
        (begin (x86-mov cgc (x86-rax) opnd)
               (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
               (x86-mov cgc opnd (x86-rax)))
        (x86-mov cgc opnd (x86-mem (- 8 TAG_MEMOBJ) opnd)))))

(define (codegen-eq? cgc fs reg lleft lright lcst? rcst? mutl? mutr?)

  (define (cmp cgc opl opr)
    (cond ((x86-reg? opl)
           (x86-cmp cgc opl opr))
          ((x86-reg? opr)
           (x86-cmp cgc opr opl))
          ((x86-mem? opl)
           (x86-mov cgc (x86-rax) opl)
           (x86-cmp cgc (x86-rax) opr))
          ((x86-mem? opr)
           (x86-mov cgc (x86-rax) opr)
           (x86-cmp cgc (x86-rax) opl))
          (else
             (error "Internal error"))))

  (let ((dest (codegen-reg-to-x86reg reg))
        (label-done (asm-make-label #f (new-sym 'eq?_end_)))
        (lopnd (and (not lcst?) (codegen-loc-to-x86opnd fs lleft)))
        (ropnd (and (not rcst?) (codegen-loc-to-x86opnd fs lright))))

   (cond ((and lcst? rcst?)
          (x86-mov cgc dest (x86-imm-int (obj-encoding (eq? lleft lright)))))
         (lcst?
            (if mutr?
                (begin (codegen-unbox cgc (x86-rax) ropnd)
                       (cmp cgc (x86-imm-int (obj-encoding lleft)) (x86-rax)))
                (cmp cgc (x86-imm-int (obj-encoding lleft)) ropnd)))
         (rcst?
            (if mutl?
                (begin (codegen-unbox cgc (x86-rax) lopnd)
                       (cmp cgc (x86-rax) (x86-imm-int (obj-encoding lright))))
                (cmp cgc lopnd (x86-imm-int (obj-encoding lright)))))
         (else
            (cond ((and mutl? mutr?)
                   (if (or (eq? dest lopnd)
                           (eq? dest ropnd))
                       (error "Internal NYI error"))
                   (codegen-unbox cgc (x86-rax) lopnd)
                   (codegen-unbox cgc dest ropnd)
                   (cmp cgc (x86-rax) dest))
                  (mutl?
                    (codegen-unbox cgc (x86-rax) lopnd)
                    (cmp cgc (x86-rax) ropnd))
                  (mutr?
                    (codegen-unbox cgc (x86-rax) ropnd)
                    (cmp cgc (x86-rax) lopnd))
                  (else
                    (cmp cgc lopnd ropnd)))))

   (if (not (and lcst? rcst?))
       (begin
         (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
         (x86-je  cgc label-done)
         (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
         (x86-label cgc label-done)))))

;;-----------------------------------------------------------------------------
;; car/cdr
(define (codegen-car/cdr cgc fs op reg lval mut-val?)
  (let ((offset
          (if (eq? op 'car)
              (-  8 TAG_MEMOBJ)
              (- 16 TAG_MEMOBJ)))
        (dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))

    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))
    (if mut-val?
        (begin (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opval))
               (set! opval (x86-rax))))

    (x86-mov cgc dest (x86-mem offset opval))))

;;-----------------------------------------------------------------------------
;; set-car!/set-cdr!
(define (codegen-scar/scdr cgc fs op reg lpair lval val-cst? mut-val? mut-pair?)
  (let ((offset
          (if (eq? op 'set-car!)
              (-  8 TAG_MEMOBJ)
              (- 16 TAG_MEMOBJ)))
        (dest (codegen-reg-to-x86reg reg))
        (oppair (codegen-loc-to-x86opnd fs lpair))
        (opval (and (not val-cst?) (codegen-loc-to-x86opnd fs lval))))

    (if (ctx-loc-is-memory? lpair)
        (begin (x86-mov cgc (x86-rax) oppair)
               (set! oppair (x86-rax))))

    (if mut-pair?
        (begin (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) oppair))
               (set! oppair (x86-rax))))

    ;; Rax contains unboxed pair

    (cond
      (val-cst?
        (x86-mov cgc (x86-mem offset oppair) (x86-imm-int (obj-encoding lval)) 64))
      ((ctx-loc-is-memory? lval)
       (x86-mov cgc dest opval)
       (x86-mov cgc (x86-mem offset oppair) dest))
      (else
        (x86-mov cgc (x86-mem offset oppair) opval)))

    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; current-input/output-port
(define (codegen-current-io-port cgc op reg)
  (let ((block-offset (if (eq? op 'current-output-port) 8 24))
        (dest  (codegen-reg-to-x86reg reg)))
    (x86-mov cgc dest (x86-imm-int (+ TAG_MEMOBJ block-offset block-addr)))))

;;-----------------------------------------------------------------------------
;; close-input/output-port
(define (codegen-close-io-port cgc fs reg lport mut-port?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opport (lambda () (codegen-loc-to-x86opnd fs lport)))
        (oprax (lambda () (x86-rax))))

    (begin-with-cg-macro
      ;;
      ;; Unmem/ Unbox code
      ;; Mov port to rax for syscall
      (unmem! (oprax) (opport))
      (chk-unmem-unbox! (oprax) (opport) mut-port?)

      ;;
      ;; Primitive code
      (gen-syscall-close cgc)
      (x86-mov cgc dest (x86-imm-int ENCODING_VOID)))))

;;-----------------------------------------------------------------------------
;; open-input/output-port
(define (codegen-open-io-file cgc fs op reg lstr mut-str?)
  (let* ((direction   (if (eq? op 'open-output-file) 'out 'in))
         (stag        (if (eq? direction 'in) STAG_IPORT STAG_OPORT))
         (header-word (mem-header 2 stag))
         (dest  (codegen-reg-to-x86reg reg))
         (opval (lambda () (codegen-loc-to-x86opnd fs lstr)))
         (oprax (lambda () (x86-rax))))

    (begin-with-cg-macro

      ;;
      ;; Unmem / Unbox code
      ;; Move operand to rax for syscall
      (unmem! (oprax) (opval))
      (chk-unmem-unbox! (oprax) (opval) mut-str?)

      ;;
      ;; Primitive code
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
      (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr)))))

;;-----------------------------------------------------------------------------
;; eof-object?
(define (codegen-eof? cgc fs reg lval mut-val?)
  (let ((label-end (asm-make-label #f (new-sym 'label-end)))
        (dest  (codegen-reg-to-x86reg reg))
        (opval (lambda () (codegen-loc-to-x86opnd fs lval)))
        (oprax (lambda () (x86-rax))))

    (begin-with-cg-macro

      ;;
      ;; Unmem / Unbox code
      ;; Move operand to rax for syscall
      (chk-unmem-unbox! (oprax) (opval) mut-val?)

      ;;
      ;; Primitive code
      (x86-cmp cgc (opval) (x86-imm-int ENCODING_EOF))
      (x86-mov cgc dest (x86-imm-int (obj-encoding #f)))
      (x86-jne cgc label-end)
      (x86-mov cgc dest (x86-imm-int (obj-encoding #t)))
      (x86-label cgc label-end))))

;;-----------------------------------------------------------------------------
;; read-char
(define (codegen-read-char cgc fs reg lport mut-port?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opport (lambda () (codegen-loc-to-x86opnd fs lport)))
        (oprax  (lambda () (x86-rax))))

    (begin-with-cg-macro
      ;;
      ;; Unmem / Unbox code
      ;; Mov port to rax for syscall
      (unmem! (oprax) (opport))
      (chk-unmem-unbox! (oprax) (opport) mut-port?)

      ;;
      ;; Primitive code
      ;; Gen 'read' syscall (read 1 byte), encoded value (char or eof) in rax
      (gen-syscall-read-char cgc)
      ;; Push encoded result
      (x86-mov cgc dest (x86-rax)))))

;;-----------------------------------------------------------------------------
;; write-char
(define (codegen-write-char cgc fs reg lchar lport mut-char? mut-port?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opport (codegen-loc-to-x86opnd (+ fs 1) lport))
        (opchar (codegen-loc-to-x86opnd fs lchar)))

    ;; Char on stack for syscall
    (if (ctx-loc-is-memory? lchar)
        (begin (x86-mov cgc (x86-rax) opchar)
               (set! opchar (x86-rax))))
    (if mut-char?
        (begin (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opchar))
               (set! opchar (x86-rax))))
    (x86-push cgc opchar)
    ;;; Mov port to rax for syscall
    (x86-mov cgc (x86-rax) opport)
    (if mut-port?
        (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
    ;;; Gen 'read' syscall, encoded value (char or eof) in rax
    (gen-syscall-write-char cgc)
    (x86-pop cgc (x86-rax)) ;; Pop char
    ;; Put encoded result
    (x86-mov cgc dest (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; char->integer/integer->char
(define (codegen-ch<->int cgc fs op reg lval cst? mut-val?)

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

          (if mut-val?
              (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) dest)))

          (if (eq? op 'char->integer)
              (x86-xor cgc dest (x86-imm-int TAG_SPECIAL))
              (x86-or  cgc dest (x86-imm-int TAG_SPECIAL))))))))

;;-----------------------------------------------------------------------------
;; make-string
(define (codegen-make-string cgc fs reg llen lval mut-len? mut-val?)
  (let* ((header-word (mem-header 3 STAG_STRING))
         (dest  (codegen-reg-to-x86reg reg))
         (oplen (lambda () (codegen-loc-to-x86opnd fs llen)))
         (opval (lambda () (if lval (codegen-loc-to-x86opnd fs lval) #f)))
         (oprax (lambda () (x86-rax)))
         (label-loop (asm-make-label #f (new-sym 'make-string-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-string-end))))

    (begin-with-cg-macro

      ;;
      ;; Unmem / Unbox code
      (chk-pick-unmem-unbox! (oplen) mut-len? (list (opval) (oplen) dest))

      ;; Len is encoded in rax (if (make-vector 3) rax=12)
      ;; Nb chars to byte size
      (x86-mov cgc (x86-rax) (oplen))
      (x86-shr cgc (x86-rax) (x86-imm-int 2))
      (x86-and cgc (x86-rax) (x86-imm-int (bitwise-not 7)))
      (x86-shr cgc (x86-rax) (x86-imm-int 1))

      ;; Alloc
      (gen-allocation cgc #f STAG_STRING 3 #t)

      ;; Dest reg = len
      (x86-mov cgc dest (oplen))
      (x86-shr cgc dest (x86-imm-int 2)) ;; decode length

      ;; Move init val to register
      (if (opval)
          (begin (unmem! (oprax) (opval))
                 (chk-unmem-unbox! (oprax) (opval) mut-val?)
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
          (begin (x86-mov cgc (x86-rax) (oplen))
                 (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
          (x86-mov cgc (x86-mem 8 alloc-ptr) (oplen)))

      ;; Len is encoded in rax (if (make-vector 3) rax=12)
      ;; Nb chars to byte size
      (x86-mov cgc (x86-rax) (oplen))
      (x86-shr cgc (x86-rax) (x86-imm-int 2))
      (x86-and cgc (x86-rax) (x86-imm-int (bitwise-not 7)))
      (x86-shr cgc (x86-rax) (x86-imm-int 1))
      ;; Write header
      (x86-shl cgc (x86-rax) (x86-imm-int 6))
      (x86-add cgc (x86-rax) (x86-imm-int header-word))
      (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))

      ;; Put str
      (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr)))))

;;-----------------------------------------------------------------------------
;; make-vector
(define (codegen-make-vector cgc fs reg llen lval mut-len? mut-val?)

  (let* ((header-word (mem-header 2 STAG_VECTOR))
         (dest  (codegen-reg-to-x86reg reg))
         (oplen (lambda () (codegen-loc-to-x86opnd fs llen)))
         (opval (lambda () (if lval (codegen-loc-to-x86opnd fs lval) #f)))
         (label-loop (asm-make-label #f (new-sym 'make-vector-loop)))
         (label-end  (asm-make-label #f (new-sym 'make-vector-end))))

    (begin-with-cg-macro

      ;; Unmem / Unbox code
      ;; TODO: Remove memory checks in primitive code
      (chk-pick-unmem-unbox! (oplen) mut-len? (list (oplen) (opval) dest))
      (chk-pick-unmem-unbox! (opval) mut-val? (list (oplen) (opval) dest))

      ;;
      ;; Primitive code

      ;; Len is encoded in rax (if (make-vector 3) rax=12)
      (x86-mov cgc (x86-rax) (oplen))
      ;; Alloc
      (gen-allocation cgc #f STAG_VECTOR 2 #t)

      (x86-mov cgc (x86-rax) (oplen))
      (x86-shl cgc (x86-rax) (x86-imm-int 1))
      ;; If opval not in register, save rbx and use it
      (cond ((not lval)
               ;; No init val given, then use rbx with value 0
             (x86-push cgc (x86-rbx))
             (x86-mov cgc (x86-rbx) (x86-imm-int 0))
             ;(set! oplen (lam)(codegen-loc-to-x86opnd (+ fs 1) llen))
             (set! opval (lambda () (x86-rbx))))
            ((not (ctx-loc-is-register? lval))
               ;; Init value id in memory, then use rbx with given init value
             (x86-push cgc (x86-rbx))
             (x86-mov cgc (x86-rbx) (opval))
             ;(set! oplen (codegen-loc-to-x86opnd (+ fs 1) llen))
             (set! opval (lambda () (x86-rbx)))))

      (x86-label cgc label-loop)
      (x86-cmp cgc (x86-rax) (x86-imm-int 0))
      (x86-je cgc label-end)

      (x86-mov cgc (x86-mem 8 alloc-ptr (x86-rax)) (opval))
      (x86-sub cgc (x86-rax) (x86-imm-int 8))
      (x86-jmp cgc label-loop)

      (x86-label cgc label-end)

      ;; Restore rbx
      (if (or (not lval)
              (not (ctx-loc-is-register? lval)))
          (begin (x86-pop cgc (x86-rbx))))
                 ;(set! oplen (codegen-loc-to-x86opnd fs llen))))

      ;; Write encoded length
      (if (ctx-loc-is-memory? llen)
          (begin (x86-mov cgc (x86-rax) (oplen))
                 (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax)))
          (x86-mov cgc (x86-mem 8 alloc-ptr) (oplen)))
      ;; Write header
      (x86-mov cgc (x86-rax) (oplen))
      (x86-shl cgc (x86-rax) (x86-imm-int 6))
      (x86-add cgc (x86-rax) (x86-imm-int header-word))
      (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
      ;; Put vector
      (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr)))))

;;-----------------------------------------------------------------------------
;; string->symbol
(define (codegen-str->sym cgc fs reg lstr mut-str?)
  (let ((dest (codegen-reg-to-x86reg reg))
        (opstr (codegen-loc-to-x86opnd fs lstr)))

    (if mut-str?
        (begin (x86-mov cgc (x86-rax) opstr)
               (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
               (set! opstr (x86-rax))))

    (x86-push cgc opstr)
    (gen-interned-symbol cgc)
    (x86-pop cgc dest)))

;;-----------------------------------------------------------------------------
;; symbol->string
(define (codegen-sym->str cgc fs reg lsym mut-sym?)

  ;; TODO: if mut-sym? is not #t, save a register and us it
  ;;       to reduce memory moves

  (let ((dest (codegen-reg-to-x86reg reg))
        (opsym (codegen-loc-to-x86opnd fs lsym)))

    (x86-label cgc (asm-make-label #f (new-sym 'SYM_TO_STR_)))

    ;; Alloc string
    (x86-mov cgc dest opsym)
    (if mut-sym?
        (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) dest)))
    (x86-sub cgc dest (x86-imm-int TAG_MEMOBJ))
    (x86-mov cgc dest (x86-mem 0 dest))
    (x86-shr cgc dest (x86-imm-int 8))
    (x86-shl cgc dest (x86-imm-int 2)) ;; Length
    (x86-mov cgc (x86-rax) dest) ;; Move for alloc
    (gen-allocation cgc #f STAG_STRING 0 #t)

    ;; Write len
    (if mut-sym?
        (begin (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opsym))
               (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
        (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opsym)))
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))

    ;; Write header
    (if mut-sym?
        (begin (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opsym))
               (x86-mov cgc (x86-rax) (x86-mem (- 0 TAG_MEMOBJ) (x86-rax))))
        (x86-mov cgc (x86-rax) (x86-mem (- 0 TAG_MEMOBJ) opsym)))
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
      (if mut-sym?
          (begin (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opsym))
                 (x86-mov cgc (x86-rax) (x86-mem (* -1 (+ 8 TAG_MEMOBJ)) (x86-rax) dest)))
          (x86-mov cgc (x86-rax) (x86-mem (* -1 (+ 8 TAG_MEMOBJ)) opsym dest)))
      (x86-mov cgc (x86-mem -8 alloc-ptr dest) (x86-rax))
      (x86-sub cgc dest (x86-imm-int 8))
      (x86-jmp cgc label-loop)

      (x86-label cgc label-end))

    (x86-lea cgc dest (x86-mem TAG_MEMOBJ alloc-ptr))))

;;-----------------------------------------------------------------------------
;; vector/string-length
(define (codegen-vec/str-length cgc fs reg lval mut-val?)
  (let ((dest  (codegen-reg-to-x86reg reg))
        (opval (codegen-loc-to-x86opnd fs lval)))

    (if (ctx-loc-is-memory? lval)
        (begin (x86-mov cgc (x86-rax) opval)
               (set! opval (x86-rax))))

    (if mut-val?
        (begin (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) opval))
               (set! opval (x86-rax))))

    (x86-mov cgc dest (x86-mem (- 8 TAG_MEMOBJ) opval))))

;;-----------------------------------------------------------------------------
;; vector-ref
;; TODO val-cst? -> idx-cst?

(define (codegen-vector-ref cgc fs reg lvec lidx val-cst? idx-mut? vec-mut?)

  (let* ((dest (codegen-reg-to-x86reg reg))
         (opvec (lambda () (codegen-loc-to-x86opnd fs lvec)))
         (opidx (lambda () (and (not val-cst?) (codegen-loc-to-x86opnd fs lidx))))
         (oprax (lambda () (x86-rax)))
         (vec-mem? (ctx-loc-is-memory? lvec))
         (idx-mem? (ctx-loc-is-memory? lidx)))


    (begin-with-cg-macro
      ;;
      ;; Unmem / Unbox code
      (chk-unmem-unbox! (oprax) (opvec) vec-mut?)
      (if (not val-cst?)
          (if (eq? (opvec) (x86-rax))
              (chk-pick-unmem-unbox! (opidx) idx-mut? (list dest (opvec) (opidx)))
              (chk-unmem-unbox! (oprax) (opidx) idx-mut?)))

      ;;
      ;; Primitive code
      (if val-cst?
          (x86-mov cgc dest (x86-mem (+ (- 16 TAG_MEMOBJ) (* 8 lidx)) (opvec) #f 1))
          (x86-mov cgc dest (x86-mem (- 16 TAG_MEMOBJ) (opvec) (opidx) 1))))))

;;-----------------------------------------------------------------------------
;; string-ref
(define (codegen-string-ref cgc fs reg lstr lidx idx-cst? idx-mut? str-mut?)

  (let ((dest  (codegen-reg-to-x86reg reg))
        (opstr (lambda () (codegen-loc-to-x86opnd fs lstr)))
        (opidx (lambda () (and (not idx-cst?) (codegen-loc-to-x86opnd fs lidx))))
        (oprax (lambda () (x86-rax)))
        (str-mem? (ctx-loc-is-memory? lstr))
        (idx-mem? (ctx-loc-is-memory? lidx)))

    (begin-with-cg-macro
      ;;
      ;; Unmem / Unbox code
      ;; idx first because if both are moved, we prefer to have idx in rax
      (if (not idx-cst?)
          (chk-unmem-unbox! (oprax) (opidx) idx-mut?))
      (if (eq? (opidx) (x86-rax))
          (chk-pick-unmem-unbox! (opstr) str-mut? (list dest (opstr) (opidx)))
          (chk-unmem-unbox! (oprax) (opstr) str-mut?))

      ;;
      ;; Primitive code
      (if idx-cst?
          (x86-mov cgc (x86-al) (x86-mem (+ (- 16 TAG_MEMOBJ) lidx) (opstr)))
          (begin
            (x86-shr cgc (opidx) (x86-imm-int 2))
            (x86-mov cgc (x86-al) (x86-mem (- 16 TAG_MEMOBJ) (opidx) (opstr)))
            (if (neq? (opidx) (x86-rax))
                (x86-shl cgc (opidx) (x86-imm-int 2)))))

      ;; Clear bits before al
      (x86-and cgc (x86-rax) (x86-imm-int 255))
      (x86-shl cgc (x86-rax) (x86-imm-int 2))
      (x86-add cgc (x86-rax) (x86-imm-int TAG_SPECIAL))
      (x86-mov cgc dest (x86-rax)))))

;;-----------------------------------------------------------------------------
;; vector-set!
;; TODO: rewrite
(define (codegen-vector-set! cgc fs reg lvec lidx lval)
  (let* ((dest (codegen-reg-to-x86reg reg))
         (opvec (codegen-loc-to-x86opnd fs lvec))
         (opidx (codegen-loc-to-x86opnd fs lidx))
         (opval (codegen-loc-to-x86opnd fs lval))
         (regsaved #f)
         (REG1
           (foldr (lambda (curr res)
                    (if (not (member curr (list opvec opidx opvec)))
                        curr
                        res))
                  #f
                  regalloc-regs)))

   (assert (not (or (eq? dest opvec)
                    (eq? dest opval)))
           "Internal error")

   (cond ((and (ctx-loc-is-memory? lvec)
               (ctx-loc-is-memory? lval))
          (set! regsaved REG1)
          (x86-mov cgc dest opvec)
          (x86-mov cgc REG1 opval)
          (set! opvec dest)
          (set! opval REG1))
         ((ctx-loc-is-memory? lvec)
          (x86-mov cgc dest opvec)
          (set! opvec dest))
         ((ctx-loc-is-memory? lval)
          (x86-mov cgc dest opval)
          (set! opval dest)))

   (x86-mov cgc (x86-rax) opidx)
   (x86-shl cgc (x86-rax) (x86-imm-int 1))
   (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) opvec (x86-rax)) opval)
   (x86-mov cgc dest (x86-imm-int ENCODING_VOID))

   (if regsaved
       (x86-pop cgc regsaved))))


  ;(x86-shl cgc opidx (x86-imm-int 1))
  ;(x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) opvec opidx) opval)
  ;(x86-mov cgc dest (x86-imm-int ENCODING_VOID))


  ;(if (ctx-loc-is-memory? lvec)
  ;    (begin (x86-mov cgc (x86-rax) opvec)
  ;           (set! opvec (x86-rax))))
  ;
  ;(if (ctx-loc-is-memory? lidx)
  ;    (begin (x86-mov cgc dest opidx)
  ;           (set! opidx dest)))
  ;
  ;(if (ctx-loc-is-memory? lval)
  ;    (if (and (eq? opvec (x86-rax))
  ;             (eq? opidx dest))
  ;        ;; both tmp regs are already used
  ;        (let ((reg (if (eq? dest (x86-rbx)) (x86-rcx) (x86-rbx))))
  ;          (x86-push cgc reg)
  ;          (set! opval (codegen-loc-to-x86opnd (+ fs 1) lval))
  ;          (x86-mov cgc reg opval)
  ;          (set! opval reg)
  ;          (set! regsaved reg))
  ;        ;; At least one is free, use it
  ;        (if (eq? opvec (x86-rax))
  ;            (begin (x86-mov cgc dest opval)
  ;                   (set! opval dest))
  ;            (begin (x86-mov cgc (x86-rax) opval)
  ;                   (set! opval (x86-rax))))))

  ;(x86-shl cgc opidx (x86-imm-int 1))
  ;(x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) opvec opidx) opval)
  ;(x86-mov cgc dest (x86-imm-int ENCODING_VOID))
  ;
  ;(if regsaved
  ;    (x86-pop cgc regsaved))))

;;-----------------------------------------------------------------------------
;; string-set!
(define (codegen-string-set! cgc fs reg lstr lidx lchr idx-cst? chr-cst? mut-str? mut-idx? mut-chr?)

  (let ((dest (codegen-reg-to-x86reg reg))
        (opstr (lambda () (codegen-loc-to-x86opnd fs lstr)))
        (opidx (lambda () (and (not idx-cst?) (codegen-loc-to-x86opnd fs lidx))))
        (opchr (lambda () (and (not chr-cst?) (codegen-loc-to-x86opnd fs lchr))))
        (oprax (lambda () (x86-rax))))

    (begin-with-cg-macro

      ;;
      ;; Unmem / Unbox code
      (if (not chr-cst?)
          (begin (unmem! (oprax) (opchr)) ;; If not a cst, we want char in rax
                 (chk-unbox! (oprax) (opchr) mut-chr?)))

      (if (not idx-cst?)
          (if (eq? (opchr) (x86-rax))
              (begin (pick-unmem! (opidx) (list (opstr) (opidx) (opchr)))
                     (chk-unbox! (opidx) (opidx) mut-idx?))
              (begin (unmem! (oprax) (opidx))
                     (chk-unbox! (oprax) (opidx) mut-idx?))))

      (if (or (eq? (opidx) (x86-rax))
              (eq? (opchr) (x86-rax)))
          (chk-pick-unmem-unbox! (opstr) mut-str? (list (opstr) (opidx) (opchr)))
          (chk-unmem-unbox! (oprax) (opstr) mut-str?))

      ;;
      ;; Primitive code
      (if (not chr-cst?)
          (x86-shr cgc (opchr) (x86-imm-int 2)))
      (if (not idx-cst?)
          (x86-shr cgc (opidx) (x86-imm-int 2)))

      (cond ((and idx-cst? chr-cst?)
             (x86-mov cgc
                      (x86-mem (+ (- 16 TAG_MEMOBJ) lidx) (opstr))
                      (x86-imm-int (char->integer lchr))
                      8))
            (idx-cst?
               (x86-mov cgc
                        (x86-mem (+ (- 16 TAG_MEMOBJ) lidx) (opstr))
                        (x86-al)))
            (chr-cst?
              (x86-mov cgc
                       (x86-mem (- 16 TAG_MEMOBJ) (opstr) (opidx))
                       (x86-imm-int (char->integer lchr))
                       8))
            (else
              (x86-mov cgc
                       (x86-mem (- 16 TAG_MEMOBJ) (opstr) (opidx))
                       (x86-al)))) ;; If char is not a cst, it is in rax

      (x86-mov cgc dest (x86-imm-int ENCODING_VOID)))))

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
;; letrec set!
(define (codegen-letrec-set! cgc fs lto lfrom mut-from?)

  (let ((opto   (lambda () (codegen-loc-to-x86opnd fs lto)))
        (opfrom (lambda () (codegen-loc-to-x86opnd fs lfrom)))
        (oprax  (lambda () (x86-rax))))

    (begin-with-cg-macro

      ;;
      ;; Unmem / Unbox code

      (chk-unmem! (oprax) (opto))
      (if (eq? (opto) (x86-rax))
          (chk-pick-unmem-unbox! (opfrom) mut-from? (list (opto) (opfrom)))
          (chk-unmem-unbox! (oprax) (opfrom) mut-from?))

      ;;
      ;;

      (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (opto)) (opfrom)))))

;;-----------------------------------------------------------------------------
;; TCO
(define (codegen-tco-move-arg cgc from to)
  (x86-mov cgc (x86-rax) (x86-mem (* from 8) (x86-rsp)))
  (x86-mov cgc (x86-mem (* to 8) (x86-rsp))  (x86-rax)))

;;-----------------------------------------------------------------------------
;; Mutable var (creates mutable object, write variable and header and replace local with mutable object)
(define (codegen-mutable cgc fs lval)
  (let ((header-word (mem-header 2 STAG_MOBJECT))
        (opval (codegen-loc-to-x86opnd fs lval)))

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

;;-----------------------------------------------------------------------------
;; Time

(define (codegen-sys-clock-gettime-ns cgc reg)
  (let ((opnd (codegen-reg-to-x86reg reg)))

    ;; Get monotonic time in rax
    (gen-syscall-clock-gettime cgc)
    (x86-mov cgc opnd (x86-rax))
    (x86-shl cgc opnd (x86-imm-int 2))))
