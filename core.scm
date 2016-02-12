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

(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm")
(include "~~lib/_codegen#.scm")

;;-----------------------------------------------------------------------------

(include "x86-debug.scm")
(include "extern/Sort.scm")

;;--------------------------------------------------------------------------------
;; Compiler options

(define opt-stats                #f) ;; Print stats report
(define opt-time                 #f) ;; Print exec time in processor cycles
(define opt-verbose-jit          #f) ;; JIT Verbose debugging
(define opt-verbose-gc           #f) ;; GC  Verbose debugging
(define opt-count-calls          #f) ;; Count call for a given identifier
(define opt-all-tests            #f) ;; Remove type information (execute all type tests)
(define opt-max-versions         #f) ;; Limit of number of versions (#f=no limit, 0=only generic, ...)
(define opt-entry-points         #t) ;; Use multiple entry points (#t to use cc-tables, #f to use flat closures)
(define opt-return-points        #t) ;; Use multiple return points (#t to use cr-tables, #f to use a generic return point)
(define opt-overflow-fallback    #t) ;; Automatic fallback to generic entry point if cctable overflows
(define opt-propagate-functionid #f) ;; Propagate function identitie
(define mode-repl                #f) ;; REPL mode ?

;; TODO Move
(define (type-to-cridx type)
  (cond ((eq? type CTX_NUM)    8) ;; Start from 8 because of header
        ((eq? type CTX_CHAR)  16)
        ((eq? type CTX_BOOL)  24)
        ((eq? type CTX_CLO)   32)
        ((eq? type CTX_PAI)   40)
        ((eq? type CTX_VOID)  48)
        ((eq? type CTX_NULL)  56)
        ((eq? type CTX_VECT)  64)
        ((eq? type CTX_STR)   72)
        ((eq? type CTX_SYM)   80)
        ((eq? type CTX_IPORT) 88)
        ((eq? type CTX_OPORT) 96)
        ((eq? type CTX_FLO)   104)
        ((eq? type CTX_UNK)   112)
        ((eq? type CTX_MOBJ)  120)
        (else (pp type) (error ERR_INTERNAL))))

;; TODO Move
(define (cridx-to-type cridx)
  (cond ((= cridx  8)  CTX_NUM)
        ((= cridx 16)  CTX_CHAR)
        ((= cridx 24)  CTX_BOOL)
        ((= cridx 32)  CTX_CLO)
        ((= cridx 40)  CTX_PAI)
        ((= cridx 48)  CTX_VOID)
        ((= cridx 56)  CTX_NULL)
        ((= cridx 64)  CTX_VECT)
        ((= cridx 72)  CTX_STR)
        ((= cridx 80)  CTX_SYM)
        ((= cridx 88)  CTX_IPORT)
        ((= cridx 96)  CTX_OPORT)
        ((= cridx 104) CTX_FLO)
        ((= cridx 112) CTX_UNK)
        ((= cridx 120) CTX_MOBJ)
        (else (pp cridx) (error ERR_INTERNAL))))

;;-----------------------------------------------------------------------------

;; Forward declarations
(define run-gc #f)         ;; mem.scm
(define expand-tl #f)      ;; expand.scm
(define gen-ast #f)        ;; ast.scm
(define repl-print-lco #f) ;; main.scm
;; TODO regalloc
(define alloc-ptr #f)
(define base-ptr #f)
(define global-ptr #f)

;;
(define change-stype #f)
(define codegen-loc-to-x86opnd #f)
(define codegen-reg-to-x86reg #f)
(define ctx-clear #f)
(define identifier #f)

;;-----------------------------------------------------------------------------

;; Global ids
;; Contains a list of global ids with id,position
;; ex. '((foo 1) (bar 2) (fun 3))
(define globals '())
(define gids '()) ;; TODO : merge with 'globals'

;;-----------------------------------------------------------------------------

;; Tags
(define TAG_NUMBER  0)
(define TAG_MEMOBJ  1)
(define TAG_SPECIAL 2)

;; Sub tags
(define STAG_PROCEDURE 14)
(define STAG_PAIR       1)
(define STAG_MOBJECT    2)
(define STAG_VECTOR     0)
(define STAG_STRING    19)
(define STAG_CCTABLE   16)
(define STAG_SYMBOL     8)
(define STAG_IPORT     17)
(define STAG_OPORT     18)
(define STAG_FLONUM    30)

;; Context types
(define CTX_UNK   'unknown)
(define CTX_ALL   '*) ;;Represents all ctx types
(define CTX_NUM   'number)
(define CTX_CHAR  'char)
(define CTX_BOOL  'boolean)
(define CTX_CLO   'procedure)
(define CTX_PAI   'pair)
(define CTX_VOID  'void)
(define CTX_NULL  'null)
(define CTX_RETAD 'retAddr)
(define CTX_VECT  'vector)
(define CTX_STR   'string)
(define CTX_SYM   'symbol)
(define CTX_IPORT 'inport)
(define CTX_OPORT 'outport)
(define CTX_MOBJ  'mobject)
(define CTX_FLO   'float)

(define (CTX_CLOi cctable)
  (if opt-propagate-functionid
      (cons CTX_CLO cctable)
      CTX_CLO))

;;-----------------------------------------------------------------------------

;; Errors
(define ERR_MSG             "EXEC ERROR")
(define ERR_ARR_OVERFLOW    "ARITHMETIC OVERFLOW")
(define ERR_WRONG_NUM_ARGS  "WRONG NUMBER OF ARGUMENTS")
(define ERR_OPEN_INPUT_FILE  "CAN'T OPEN INPUT FILE")
(define ERR_OPEN_OUTPUT_FILE "CAN'T OPEN OUTPUT FILE")
(define ERR_READ_CHAR        "CAN'T READ CHAR")
(define ERR_WRITE_CHAR       "CAN'T WRITE CHAR")
(define ERR_DIVIDE_ZERO      "DIVIDE BY ZERO")
(define ERR_INTERNAL         "INTERNAL ERROR")
(define ERR_BEGIN            "ILL-FORMED BEGIN")
(define ERR_LET              "ILL-FORMED LET")
(define ERR_LET*             "ILL-FORMED LET*")
(define ERR_LETREC           "ILL-FORMED LETREC")
(define ERR_HEAP_NOT_8       "INTERNAL ERROR: heap size should be a multiple of 8")

(define (ERR_TYPE_EXPECTED type)
  (string-append (string-upcase type)
                 " EXPECTED"))

(define (ERR_UNKNOWN_VAR var)
  (if (string? var)
      (string-append "Can't find variable: " var)
      (string-append "Can't find variable: " (symbol->string var))))

;;-----------------------------------------------------------------------------

(define ENCODING_VOID -18) ;; encoded VOID
(define ENCODING_EOF  -14) ;; encoded EOF
(define NENCODING_EOF  -4) ;; non encoded EOF

;;--------------------------------------------------------------------------------

;; Return lazy code object which generates an error with 'msg'
(define (get-lazy-error msg)
  (make-lazy-code
    (lambda (cgc ctx)
      (gen-error cgc msg))))


;; Gen code for error
;; stop-exec? to #f to continue after error
(define (gen-error cgc err #!optional (stop-exec? #t))

  ;; Put error msg in RAX
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding err)))

  ;; If no msg print nothing
  (if (not (string=? err ""))
    ;; Call exec-error
    (push-pop-regs cgc
                   c-caller-save-regs ;; preserve regs for C call
                   (lambda (cgc)
                     (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
                     (x86-and  cgc (x86-rsp) (x86-imm-int -16))
                     (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
                     (x86-push cgc (x86-rdi))
                     (x86-call cgc label-exec-error) ;; call C function
                     (x86-pop  cgc (x86-rsp))))) ;; restore unaligned stack-pointer

  (if mode-repl
      (begin
        (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
        (x86-mov cgc (x86-rsp) (x86-mem 0 (x86-rax)))
        (gen-repl-call cgc)
        (x86-jmp cgc (x86-rbx)))
      (begin
        (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
        (x86-mov cgc (x86-rsp) (x86-mem 0 (x86-rax)))
        (x86-mov cgc (x86-rax) (x86-imm-int -1))
        (pop-regs-reverse cgc all-regs)
        (x86-ret cgc))))

;; The procedure exec-error is callable from generated machine code.
;; This function print the error message in rax
(c-define (exec-error sp) (long) void "exec_error" ""
  (let* ((err-enc (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rax-pos) 1) 8))))
        (err (encoding-obj err-enc)))
    (print "!!! ERROR : ")
    (println err)))

;;-----------------------------------------------------------------------------

(c-define (gc sp) (long) long "gc" ""
    (let ((alloc-size (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rax-pos) 1) 8)))))
      (run-gc sp alloc-size)))

(define (gen-gc-call cgc)
  (push-pop-regs
     cgc
     c-caller-save-regs ;; preserve regs for C call
     (lambda (cgc)
       (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
       (x86-and  cgc (x86-rsp) (x86-imm-int -16))
       (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
       (x86-push cgc (x86-rdi))
       (x86-call cgc label-gc) ;; call C function
       (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
       (x86-mov cgc alloc-ptr (x86-rax)) ;; TODO : Update alloc-ptr
       )))

;;-----------------------------------------------------------------------------

;; TODO: remove rax from c-caller-save-regs, then use gen-handler for this function
(define (gen-repl-call cgc)
  (push-pop-regs
    cgc
    c-caller-save-regs ;; preserve regs for C call
    (lambda (cgc)
      (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
      (x86-and  cgc (x86-rsp) (x86-imm-int -16))
      (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
      (x86-push cgc (x86-rdi))
      (x86-call cgc label-repl) ;; call C function
      (x86-pop cgc (x86-rsp))
      (x86-mov cgc (x86-rbx) (x86-rax))))) ;; Move version address to rbx

;;-----------------------------------------------------------------------------

(define (gen-print-slot cgc slot msg)
  (x86-push cgc (x86-rax))
  (gen-get-slot cgc slot (x86-rax))
  (gen-print-reg cgc msg (x86-rax))
  (x86-pop cgc (x86-rax)))

(define (gen-print-msg cgc msg newline? #!optional (literal? #t))

  (x86-push cgc (x86-rax))
  (x86-push cgc (x86-rcx))
  (x86-push cgc (x86-rdi))

  (if literal?
     (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding msg)))
     (x86-mov cgc (x86-rax) msg))

  (x86-mov cgc (x86-rdi) (x86-imm-int (if newline? (obj-encoding 1) (obj-encoding 0))))

  (push-pop-regs
       cgc
       c-caller-save-regs ;; preserve regs for C call
       (lambda (cgc)
         (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
         (x86-and  cgc (x86-rsp) (x86-imm-int -16))
         (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
         (x86-push cgc (x86-rdi))
         (x86-call cgc label-print-msg) ;; call C function
         (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
         ))

  (x86-pop cgc (x86-rdi))
  (x86-pop cgc (x86-rcx))
  (x86-pop cgc (x86-rax)))

(define (gen-print-reg cgc msg reg)

  (x86-push cgc (x86-rax))
  (x86-push cgc (x86-rcx))

  (x86-mov cgc (x86-rcx) reg)
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding msg)))

  (push-pop-regs
       cgc
       c-caller-save-regs ;; preserve regs for C call
       (lambda (cgc)
         (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
         (x86-and  cgc (x86-rsp) (x86-imm-int -16))
         (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
         (x86-push cgc (x86-rdi))
         (x86-call cgc label-print-msg-val) ;; call C function
         (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
         ))

  (x86-pop cgc (x86-rcx))
  (x86-pop cgc (x86-rax)))

;; TODO
(c-define (print-msg-val sp) (long) void "print_msg_val" ""
  (let* ((msg-enc (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rax-pos) 1) 8))))
         (val     (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8))))
         (msg     (encoding-obj msg-enc)))

    (print msg " ")
    (println val)))

;; TODO
(c-define (print-msg sp) (long) void "print_msg" ""
  (let* ((msg-enc  (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rax-pos) 1) 8))))
         (newline? (encoding-obj (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rdi-pos) 1) 8)))))
         (msg      (encoding-obj msg-enc)))

    (and (print msg) (= newline? 1) (newline))))

;; Repl function.
;; Get sexpr from stdin, gen-version for empty context and returns version address
(c-define (repl sp) (long) long "repl" ""
  (print "lc> ")
  (let ((r (read)))
    (if (or (eq? r 'quit) (eof-object? r))
        (begin (println "") (exit 0))
        (let* ((r (car (expand-tl (list r))))
               (lco (gen-ast r repl-print-lco))
               (addr (gen-version code-alloc lco (make-ctx '() '() -1))))
          addr))))

;;-----------------------------------------------------------------------------

;; The procedures do-callback* are callable from generated machine code.
;; RCX holds selector (CL)
(c-define (do-callback sp) (long) void "do_callback" ""
  (let* ((ret-addr
          (get-i64 (+ sp (* nb-c-caller-save-regs 8))))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (selector
          (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8))))

         (new-ret-addr
          (callback-fn ret-addr selector)))

    ;; replace return address
    (put-i64 (+ sp (* nb-c-caller-save-regs 8))
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8))
             0)))

;; Same behavior as 'do-callback' but calls callback with call site addr
;; The call code call the stub, then the stub call 'do-callback-fn'
;; Here is the stack when stub calls 'do-callback-fn' :
;; +---------------+
;; | Saved reg n   |  <<-- RSP
;; +---------------+
;; | Saved reg ... |
;; +---------------+
;; | Saved reg 1   |
;; +---------------+
;; | Return addr   | (return addr for do-callback-fn function)
;; +---------------+
;; | Call ctx id   |
;; +---------------+
;; | Return addr   | (continuation addr)
;; +---------------+
;; | Closure       |
;; +---------------+
;; | Call arg n    |
;; +---------------+
;; | Call arg ...  |
;; +---------------+
;; | Call arg 1    |
;; +---------------+
(c-define (do-callback-fn sp) (long) void "do_callback_fn" ""
  (let* ((ret-addr
          (get-i64 (+ sp (* nb-c-caller-save-regs 8))))

         ;; R11 is the still-box containing call-ctx
         (still-encoding
          (get-i64 (+ sp (* (- (- nb-c-caller-save-regs r11-pos) 1) 8))))

         (selector
          (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8))))

         ;; Get ctx from still-box address
         (ctx
           (if (= selector 1) ;; If called from generic ptr
             #f
             (still-ref->ctx still-encoding)))

         (nb-args
           (if (= selector 1)
               (let ((encoded (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rdi-pos) 1) 8)))))
                 (/ encoded 4))
               (- (length (ctx-stack ctx)) 2)))

         (closure
          ;; TODO regalloc: closure offset is computed from nb args on stack only
          (let ((stack-offset
                  (let ((r (- nb-args (length args-regs))))
                    (if (< r 0) 0 r))))
            (get-i64 (+ sp (* nb-c-caller-save-regs 8) 8 (* 8 stack-offset)))))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (new-ret-addr
          (callback-fn sp ctx ret-addr selector closure)))

    ;; replace return address
    (put-i64 (+ sp (* nb-c-caller-save-regs 8))
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8))
             0)))

;; The procedures do-callback* are callable from generated machine code.
;; RCX holds selector (CL)
(c-define (do-callback-cont sp) (long) void "do_callback_cont" ""
  (let* ((ret-addr
          (get-i64 (+ sp (* nb-c-caller-save-regs 8))))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (selector
          (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8))))

         (type-idx
          (get-i64 (+ sp (* (- (- nb-c-caller-save-regs r11-pos) 1) 8))))

         (table
          (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rdx-pos) 1) 8))))

         (type (cridx-to-type type-idx))

         (new-ret-addr
           (callback-fn ret-addr selector type table)))

    ;; replace return address
    (put-i64 (+ sp (* nb-c-caller-save-regs 8)) new-ret-addr)

    ;; reset selector
    (put-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8)) 0)))

;;-----------------------------------------------------------------------------
;; Interned symbols
;; TODO

(define label-interned-symbol #f)
(define sym-space-len 100000)
(define sym-space (make-mcb sym-space-len))
(define sym-alloc (##foreign-address sym-space))
(define interned-symbols (make-table test: eq?))

;; Allocate a new symbol and return encoded qword
(define (alloc-symbol sym)

  (define (copy-sym str pos offset)
    ;; SI =pos, combler avec des 0 pour avoir la bonne adresse (alignee sur 8)
    (if (= pos (string-length str))
       #f
       (begin (put-u8 (+ sym-alloc offset) (char->integer (string-ref str pos)))
              (copy-sym str (+ pos 1) (+ offset 1)))))

  (let* ((addr sym-alloc)
         (str  (symbol->string sym))
         (len  (string-length str))
         (nbbytes (arithmetic-shift (bitwise-and (+ len 8) (bitwise-not 7)) -3)))

    ;; Write header
    (put-i64 sym-alloc (mem-header (+ 2 nbbytes) STAG_SYMBOL))
    ;; Write encoded length
    (put-i64 (+ sym-alloc 8) (* 4 len))
    ;; Copy sym
    (copy-sym str 0 16)
    ;; Update sym alloc
    (set! sym-alloc (+ sym-alloc 16 (* 8 nbbytes)))
    ;; Return tagged symbol qword
    (+ addr TAG_MEMOBJ)))

;; Get symbol qworw from symbol 'sym'
;; Allocate a new symbol if not in table, and return existing if already in table
(define (get-symbol-qword sym)
  (let ((r (table-ref interned-symbols sym #f)))
    (if r
       ;; Symbol exists
       r
       ;; Symbol does not exist
       (let ((c (alloc-symbol sym)))
        (table-set! interned-symbols sym c)
        c))))

;; Entry point to create symbol from string at runtime
(c-define (interned-symbol sp) (long) void "interned_symbol" ""

  (let* ((str (get-i64 (+ sp (* (length all-regs) 8)))) ;; Get str from top of runtime stack
         (len (quotient (get-i64 (+ (- str TAG_MEMOBJ) 8)) 4)))

    ;; Get gambit symbol from lc string
    (define (lcstr->gsym addr len pos gstr)
      (if (= 0 len)
         (string->symbol gstr)
         (begin (string-set! gstr pos (integer->char (get-u8 addr)))
                (lcstr->gsym (+ addr 1) (- len 1) (+ pos 1) gstr))))

    (let* ((sym   (lcstr->gsym (+ (- str TAG_MEMOBJ) 16) len 0 (make-string len)))
           (qword (get-symbol-qword sym)))
      ;; Write symbol qword at top on runtime stack
      (put-i64 (+ sp (* (length all-regs) 8)) qword))))

;; Gen code to call interner-symbol at runtime
(define (gen-interned-symbol cgc)
  (push-pop-regs
     cgc
     ;;c-caller-save-regs ;; preserve regs for C call
     all-regs
     (lambda (cgc)
       (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
       (x86-and  cgc (x86-rsp) (x86-imm-int -16))
       (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
       (x86-push cgc (x86-rdi))
       (x86-call cgc label-interned-symbol) ;; call C function
       (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
       )))


;;-----------------------------------------------------------------------------

(define (init-labels cgc)

  (set! label-exec-error
        (asm-make-label
         cgc
         'exec-error
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,exec_error);")))))

  (set! label-print-msg
        (asm-make-label
         cgc
         'print-msg
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,print_msg);")))))

  (set! label-print-msg-val
        (asm-make-label
         cgc
         'print-msg-val
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,print_msg_val);")))))

  (set! label-repl
        (asm-make-label
         cgc
         'repl
         (##foreign-address
           ((c-lambda ()
                      (pointer void)
                      "___result = ___CAST(void*,repl);")))))

  (set! label-gc
        (asm-make-label
         cgc
         'gc
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,gc);")))))

  (set! label-do-callback
        (asm-make-label
         cgc
         'do_callback
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,do_callback);")))))

  (set! label-do-callback-fn
        (asm-make-label
         cgc
         'do_callback_fn
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,do_callback_fn);")))))

  (set! label-do-callback-cont
        (asm-make-label
         cgc
         'do_callback_cont
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,do_callback_cont);")))))

  (set! label-interned-symbol
        (asm-make-label
         cgc
         'interned_symbol
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,interned_symbol);")))))

  (set! label-dump-regs
          (asm-make-label
           cgc
           'dump-regs
           (##foreign-address
            ((c-lambda ()
                       (pointer void)
                       "___result = ___CAST(void*,dump_regs);")))))

  (set! label-dump-stack
          (asm-make-label
           cgc
           'dump-stack
           (##foreign-address
            ((c-lambda ()
                       (pointer void)
                       "___result = ___CAST(void*,dump_stack);")))))

  (set! label-breakpoint
        (asm-make-label
         cgc
         'break_point
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,break_point);"))))))



;;-----------------------------------------------------------------------------

;; Machine code block management

;; HEAP
(define from-space #f)
(define to-space   #f)
;; Set to 2gb (2000000000) to exec all benchmarks without GC (except lattice.scm)
;; Set to 7gb (7000000000) to exec all benchmarks without GC
(define space-len 7000000000)

(assert (= (modulo space-len 8) 0) ERR_HEAP_NOT_8)

(define init-to-space #f)
(define init-from-space #f)

;; CODE
(define code-len 12000000)
(define code-addr #f)
(define mcb #f)

(define (init-mcb)
  (set! mcb (make-mcb code-len))
  (set! code-addr (##foreign-address mcb))
  (let ((tspace (make-mcb space-len))
        (fspace (make-mcb space-len)))
    (set! init-from-space (+ (##foreign-address fspace) space-len))
    (set! init-to-space   (+ (##foreign-address tspace) space-len)))
  (set! from-space init-from-space)
  (set! to-space init-to-space))

;; BLOCK :
;; 0          8                       (nb-globals * 8 + 8)
;; +----------+----------+----------+----------+
;; | Bottom   | Global 1 |    ...   | Global n |
;; | stack    |          |          |          |
;; | addr     |          |          |          |
;; +----------+----------+----------+----------+

(define block #f)
(define global-offset 15) ;; [Stack addr], [def-out-port-header|def-out-port-fd], [def-in-port-header|def-in-port-fd], [heaplimit] + n empty slot (used for debug)
(define block-len (* 8 (+ global-offset 10000))) ;; 1 stack addr, 1000 globals
(define block-addr #f)
(define debug-slots '((calls . 6) (tests . 7) (extests . 8) (closures . 9) (time . 10) (other . 11)))

(define (init-block)
  (set! block (make-mcb block-len))
  (set! block-addr (##foreign-address block)))

(define (write-mcb code start)
  (let ((len (u8vector-length code)))
    (let loop ((i (fx- len 1)))
      (if (fx>= i 0)
          (begin
            (##machine-code-block-set! mcb (fx+ start i) (u8vector-ref code i))
            (loop (fx- i 1)))
          mcb))))

(define (code-gen arch addr gen #!optional ctx)

  (let* ((cgc (make-codegen-context))
         (endianness 'le))

    (asm-init-code-block cgc addr endianness)
    (codegen-context-listing-format-set! cgc 'nasm)
    (x86-arch-set! cgc arch)

    ;; If the code-gen function is called to generate function stub, use ctx
    (if ctx
      (gen cgc ctx)
      (gen cgc))

    (let ((code (asm-assemble-to-u8vector cgc)))
      (if opt-verbose-jit
          (begin
            (println "------------------------------------------------------------------------")
            (asm-display-listing cgc (current-output-port) #t)))
      (write-mcb code (- addr code-addr))
      (u8vector-length code))))

;;-----------------------------------------------------------------------------

;; Code and stub management

(define code-alloc #f)
(define stub-alloc #f)
(define stub-freelist #f)

(define (init-code-allocator)
  (init-block)
  (init-mcb)
  (set! code-alloc code-addr)
  (set! stub-alloc (+ code-addr code-len))
  (set! stub-freelist 0))

(define (code-add gen)
  (let ((len (code-gen 'x86-64 code-alloc gen)))
    (set! code-alloc (+ code-alloc len))))

(define (stub-add max-selector gen)
  (let* ((alloc
          (if (= stub-freelist 0)
              (begin
                (set! stub-alloc (- stub-alloc 16))
                stub-alloc)
              (let ((a stub-freelist))
                (set! stub-freelist (get-i64 a))
                a)))
         (stub-labels
          '()))
    (code-gen
     'x86-64
     alloc
     (lambda (cgc)
       (let loop ((i max-selector))
         (let ((label
                (asm-make-label
                 cgc
                 (string->symbol
                  (string-append "stub_"
                                 (number->string alloc 16)
                                 "_"
                                 (number->string i))))))
           (set! stub-labels (cons label stub-labels))
           (x86-label cgc label)
           (if (> i 0)
               (begin
                 (x86-inc cgc (x86-cl)) ;; increment selector
                 (loop (- i 1))))))
       (gen cgc)))
    stub-labels))

(define (create-stub label-handler max-selector . args)
  (let* ((len
          (length args))
         (obj
          (alloc-still-vector len))
         (stub-labels
          (stub-add
           max-selector
           (lambda (cgc)
             (call-handler cgc label-handler obj)))))
    (subvector-move! (list->vector args) 0 len obj 0)
    (if opt-verbose-jit
        (pp (list 'obj= obj)))
    stub-labels))

(define (call-handler cgc label-handler obj)
  (x86-call cgc label-handler)
  (asm-64   cgc (obj-encoding obj)))

(define (stub-reclaim stub-addr)
  (put-i64 stub-addr stub-freelist)
  (set! stub-freelist stub-addr))

;;-----------------------------------------------------------------------------

(define label-do-callback-handler #f)
(define label-do-callback-fn-handler #f)
(define label-do-callback-cont-handler #f)

(define (gen-handler cgc id label)
  (let ((label-handler (asm-make-label cgc id)))

    (x86-label cgc label-handler)

    (push-pop-regs
     cgc
     c-caller-save-regs ;; preserve regs for C call
     (lambda (cgc)
       (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
       (x86-and  cgc (x86-rsp) (x86-imm-int -16))
       (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
       (x86-push cgc (x86-rdi))
       (x86-call cgc label) ;; call C function
       (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
       ))

    (x86-ret cgc)

    label-handler))

(define (init-rtlib cgc)
  (let ((label-rtlib-skip (asm-make-label cgc 'rtlib_skip)))

    (x86-jmp cgc label-rtlib-skip)

    (set! label-do-callback-handler
          (gen-handler cgc 'do_callback_handler label-do-callback))

    (set! label-do-callback-fn-handler
          (gen-handler cgc 'do_callback_fn_handler label-do-callback-fn))

    (set! label-do-callback-cont-handler
          (gen-handler cgc 'do_callback_cont_handler label-do-callback-cont))

    (set! label-gc-trampoline (asm-make-label cgc 'gc_trampoline))

    (x86-label cgc label-gc-trampoline)
    (gen-gc-call cgc)
    (x86-ret cgc)

    (x86-label cgc label-rtlib-skip)

    (push-regs cgc all-regs)

    ;; Init debug slots
    (for-each (lambda (s)
                (gen-set-slot cgc (car s) 0))
              debug-slots)

    ;; Get RDTSC value if --time option is #t
    (if opt-time
       (begin
          (x86-mov   cgc (x86-rbx) (x86-imm-int (+ block-addr (* 9 8)))) ;; Get slot addr in rbx
          (x86-rdtsc cgc)                                   ;; rdx = hi, rax = lo
          (x86-shl   cgc (x86-rdx) (x86-imm-int 32))        ;; rdx = rdx << 32
          (x86-or    cgc (x86-rax) (x86-rdx))               ;; rax = lo | hi
          (x86-mov   cgc (x86-mem 0 (x86-rbx)) (x86-rax)))) ;; Mov time in time slot

    ;; Put bottom of the stack (after saving registers) at "block-addr + 0"
    (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
    (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-rsp))

    ;; Init base-ptr
    (x86-mov cgc base-ptr (x86-rsp))

    ;; Put heaplimit in heaplimit slot
    ;; TODO: remove cst slots
    (x86-mov cgc (x86-rcx) (x86-imm-int (- from-space space-len)))
    (x86-mov cgc (x86-mem (* 8 5) (x86-rax)) (x86-rcx))

    (x86-mov cgc (x86-rcx) (x86-imm-int 0))
    (x86-mov cgc alloc-ptr (x86-imm-int from-space))       ;; Heap addr in alloc-ptr
    (x86-mov cgc global-ptr (x86-imm-int (+ block-addr (* 8 global-offset)))) ;; Globals addr in r10

    (let ((label (asm-make-label #f (new-sym 'prog_begin))))
      (x86-label cgc label))))

(define (init)

  (init-code-allocator)

  ;; Create default ports in 'block'
  (let ((output-header (mem-header 2 STAG_OPORT))
        (input-header  (mem-header 2 STAG_IPORT)))
    ;; output
    (put-i64 (+ block-addr 8) output-header)
    (put-i64 (+ block-addr 16) 1)
    ;; input
    (put-i64 (+ block-addr 24) input-header)
    (put-i64 (+ block-addr 32) 0))

  (code-add
   (lambda (cgc)
     (init-labels cgc)
     (init-rtlib cgc))))

;;-----------------------------------------------------------------------------

(define (push-pop-regs cgc regs proc)
  (for-each (lambda (reg) (x86-push cgc reg)) regs)
  (proc cgc)
  (for-each (lambda (reg) (x86-pop cgc reg)) (reverse regs)))

(define (push-regs cgc regs)
  (for-each (lambda (reg) (x86-push cgc reg)) regs))

(define (pop-regs-reverse cgc regs)
  (for-each (lambda (reg) (x86-pop cgc reg)) (reverse regs)))

(define c-caller-save-regs ;; from System V ABI
  (list (x86-rdi) ;; 1st argument
        (x86-rsi) ;; 2nd argument
        (x86-rdx) ;; 3rd argument
        (x86-rcx) ;; 4th argument
        (x86-r8)  ;; 5th argument
        (x86-r9)  ;; 6th argument
        (x86-r10)
        (x86-r11)
        (x86-rax) ;; return register ;; TODO
        ))

(define all-regs
  (list (x86-rsp)
        (x86-rax)
        (x86-rbx)
        (x86-rcx)
        (x86-rdx)
        (x86-rbp)
        (x86-rsi)
        (x86-rdi)
        (x86-r8)
        (x86-r9)
        (x86-r10)
        (x86-r11)
        (x86-r12)
        (x86-r13)
        (x86-r14)
        (x86-r15)))

(define regalloc-regs
  (list (x86-rbx)
        (x86-rdx)
        (x86-rsi)
        (x86-rdi)
        (x86-r10)
        (x86-r11)
        (x86-r12)
        (x86-r13)
        (x86-r14)
        (x86-r15)))

(define nb-c-caller-save-regs
  (length c-caller-save-regs))

(define rdx-pos
  (- nb-c-caller-save-regs
     (length (member (x86-rdx) c-caller-save-regs))))

(define rcx-pos
  (- nb-c-caller-save-regs
     (length (member (x86-rcx) c-caller-save-regs))))

(define rdi-pos
  (- nb-c-caller-save-regs
     (length (member (x86-rdi) c-caller-save-regs))))

(define r11-pos
  (- nb-c-caller-save-regs
     (length (member (x86-r11) c-caller-save-regs))))

(define rax-pos
  (- nb-c-caller-save-regs
     (length (member (x86-rax) c-caller-save-regs))))

;;-----------------------------------------------------------------------------

(define new-sym-counters (make-table))

(define (new-sym sym)
  (let ((n (+ 1 (table-ref new-sym-counters sym 0))))
    (table-set! new-sym-counters sym n)
    (string->symbol (string-append (symbol->string sym) (number->string n)))))

;;-----------------------------------------------------------------------------

(define extremely-lazy? #f)

;;-----------------------------------------------------------------------------

;; List of all lazy code objects (for debug/measure purposes)
(define all-lazy-code '())

(define-type lazy-code
  constructor: make-lazy-code*
  generator
  versions
  flags
)

(define (lazy-code-nb-versions lazy-code)
  (table-length (lazy-code-versions lazy-code)))

(define (make-lazy-code generator)
  (let ((lc (make-lazy-code* generator (make-table) '())))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-cont generator)
  (let ((lc (make-lazy-code* generator (make-table) '(cont))))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-entry generator)
  (let ((lc (make-lazy-code* generator (make-table) '(entry))))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-ret generator)
  (let ((lc (make-lazy-code* generator (make-table) '(ret))))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (get-version lazy-code ctx)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-ref versions ctx #f)))

(define (put-version lazy-code ctx v)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-set! versions ctx v)))

;;-----------------------------------------------------------------------------
;; Identifier management

;;-----------------------------------------------------------------------------
;; Ctx TODO regalloc





;; TODO regalloc: uniformiser id, identifier et ident
(define (ctx-init-fn call-ctx enclosing-ctx args free-vars mutable-vars)

  ;; Construit le slot-loc initial pour une fonction
  ;; Pour le moment, les arguments sont passés sur la pile
  ;; donc chaque slot de pile est associé à l'emplacement mémoire
  (define (slot-loc-init-fn nb-args curr regs r)
    (cond ((= curr nb-args)
             (let* ((all (build-list (length regalloc-regs) (lambda (i)
                                                              (string->symbol
                                                                (string-append "r" (number->string i))))))
                    (free (set-sub all (set-sub args-regs regs '()) '())))
             (cons free
                   (append r '((1 . 1) (0 . 0))))))
          ((null? regs)
            (let ((next (cons (cons (+ 2 curr) (+ 2 (- curr (length args-regs)))) r)))
              (slot-loc-init-fn nb-args (+ curr 1) regs next)))
          (else
            (let ((next (cons (cons (+ 2 curr) (car regs)) r)))
              (slot-loc-init-fn nb-args (+ curr 1) (cdr regs) next)))))

  (define (env-init-fn args slot free-vars mutable-vars enclosing-ctx)

    (define (init-free free-vars enclosing-env i)
      (if (null? free-vars)
          '()
          (let* ((ident (cdr (assoc (car free-vars) enclosing-env)))
                 (type (ctx-identifier-type enclosing-ctx ident)))
            (cons (cons (car free-vars)
                        (make-identifier 'free
                                         '()
                                         (identifier-flags ident)
                                         (if (member 'mutable (identifier-flags ident))
                                             CTX_UNK ;; Can't write type of free mutable variable
                                             type)
                                         (string->symbol (string-append "f" (number->string i)))))
                  (init-free (cdr free-vars) enclosing-env (+ i 1))))))

    (define (init-local args slot)
      (if (null? args)
          '()
          (cons (cons (car args)
                      (make-identifier 'local
                                       (list slot)
                                       (if (member (car args) mutable-vars)
                                           '(mutable)
                                           '())
                                       #f
                                       #f))
                (init-local (cdr args) (+ slot 1)))))

    (append (init-free free-vars (ctx-env enclosing-ctx) 1)
            (init-local args slot)))

  (define fs
    (let ((r (- (length args) (length args-regs))))
      (if (>= r 0)
          (+ r 2)
          2)))

  (define free/slot-loc
      (slot-loc-init-fn
        (length args)
        0
        args-regs
        '()))

  ;;
  (make-ctx (if call-ctx
                (ctx-stack call-ctx)
                (append (make-list (length args) CTX_UNK) (list CTX_CLO CTX_RETAD)))
            (cdr free/slot-loc)
            (car free/slot-loc)
            (env-init-fn args 2 free-vars mutable-vars enclosing-ctx)
            (length args)
            fs))

(define (ctx-push ctx type loc #!optional (id #f)) ;; can only push reg ?

  (define (env-push-id env slot)
    (if (null? env)
        '()
        (let ((ident (car env)))
          (if (eq? (car ident) id)
              (cons
                (cons (car ident)
                      (make-identifier
                        (identifier-kind (cdr ident))
                        (cons slot (identifier-sslots (cdr ident)))
                        (identifier-flags (cdr ident))
                        (identifier-stype (cdr ident))
                        (identifier-cloc  (cdr ident))))
                (cdr env))
              (cons ident (env-push-id (cdr env) slot))))))

  (make-ctx
    ;; Stack
    (cons type (ctx-stack ctx))
    ;; Slot-loc
    (cons
      (cons (+ (ctx-lidx-to-slot ctx 0) 1) loc)
      (ctx-slot-loc ctx))
    ;; Free regs
    (set-sub (ctx-free-regs ctx) (list loc) '())
    ;; env
    (if id
        (env-push-id (ctx-env ctx) (length (ctx-stack ctx)))
        (ctx-env ctx))
    ;; nb-args
    (ctx-nb-args ctx)
    ;;
    (ctx-fs ctx)))

;;; TODO : remplacer les (ctx-pop (ctx-pop ...)) par ça
(define (ctx-pop-n ctx n)
  (if (= n 0)
      ctx
      (ctx-pop (ctx-pop-n ctx (- n 1)))))

(define (ctx-pop ctx)
  (let* ((slot (ctx-lidx-to-slot ctx 0))
         (stack (cdr (ctx-stack ctx)))
         (r (assoc-remove slot (ctx-slot-loc ctx)))
         (slot-loc
           (if (or (not (car r)) (ctx-loc-is-register? (cdar r)))
               (cdr r)
               (cons (cons #f (cdar r)) (cdr r))))
         (free-regs (if (and (car r) (ctx-loc-is-register? (cdar r)))
                        (cons (cdar r) (ctx-free-regs ctx))
                        (ctx-free-regs ctx)))
         (env
           (foldr (lambda (el r)
                    (if (member slot (identifier-sslots (cdr el)))
                        (cons (cons (car el)
                                    (make-identifier
                                      (identifier-kind (cdr el))
                                      (set-sub (identifier-sslots (cdr el)) (list slot) '())
                                      (identifier-flags (cdr el))
                                      (identifier-stype (cdr el))
                                      (identifier-cloc (cdr el))))
                              r)
                        (cons el r)))
                  '()
                  (ctx-env ctx))))
    (make-ctx stack slot-loc free-regs env (ctx-nb-args ctx) (ctx-fs ctx))))

;; TODO
(define (ctx-identifier-change-type ctx identifier type)

  (define (change-stack stack lidx)
    (append (list-head stack lidx)
            (cons type
                  (list-tail stack (+ lidx 1)))))

  (define (change-stack-n-slots stack slots)
    (if (null? slots)
        stack
        (change-stack-n-slots
          (change-stack stack (ctx-slot-to-lidx ctx (car slots)))
          (cdr slots))))

  (define (change-stype env identifier)
    (if (null? env)
        '()
        (let ((first (car env)))
          (if (eq? (cdr first) identifier)
              (cons (cons (car first)
                          (make-identifier
                            (identifier-kind identifier)
                            (identifier-sslots identifier)
                            (identifier-flags identifier)
                            type
                            (identifier-cloc identifier)))
                    (cdr env))
              (cons first
                    (change-stype (cdr env) identifier))))))


  (let* ((free? (eq? (identifier-kind identifier) 'free))
         (env
           (if free?
               (change-stype (ctx-env ctx) identifier)
               (ctx-env ctx))))

    (make-ctx
      (change-stack-n-slots (ctx-stack ctx) (identifier-sslots identifier))
      (ctx-slot-loc ctx)
      (ctx-free-regs ctx)
      env
      (ctx-nb-args ctx)
      (ctx-fs ctx))))

;; TODO lidx
;; TODO: utiliser ctx-identifier-change-type
(define (ctx-change-type ctx stack-idx type)

  (define (change-stack stack lidx)
    (append (list-head stack lidx)
            (cons type
                  (list-tail stack (+ lidx 1)))))

  (define (change-stype env identifier)
    (if (null? env)
        '()
        (let ((first (car env)))
          (if (eq? (cdr first) identifier)
              (error "NYI change-stype")
              (cons first (change-stype (cdr env) identifier))))))

  (define (change-stack-n-slots stack slots)
    (if (null? slots)
        stack
        (change-stack-n-slots
          (change-stack stack (ctx-slot-to-lidx ctx (car slots)))
          (cdr slots))))

  (let* ((slot (ctx-lidx-to-slot ctx stack-idx))
         (ident (ctx-ident-at ctx slot))
         (stack
           (if ident
               (change-stack-n-slots (ctx-stack ctx) (identifier-sslots (cdr ident)))
               (change-stack (ctx-stack ctx) stack-idx)))
         (env
           (if (and ident
                    (eq? (identifier-kind (cdr ident)) 'free))
               (change-stype (ctx-env ctx) identifier) ;; TODO: NYI change-stype
               (ctx-env ctx))))

    (make-ctx
      stack
      (ctx-slot-loc ctx)
      (ctx-free-regs ctx)
      env
      (ctx-nb-args ctx)
      (ctx-fs ctx))))

;;; TODO: cette fonction peut générer du code (spill), il faut donc faire attention ou on l'appelle, et lui envoyer le cgc
;;; TODO: ajouter un argument qui donne le nombre d'éléments sur la pile qui sont consommés pour cette opération (**)
;;; Retourne un registre vide
;;; Si un registre est vide, on le retourne
;;; Sinon, NYI:
;;;   * On regarde les n (**) éléments sur la pile virtuelle. Si un est associé à un registre, on peut le retourner car c'est une opérande de l'opération en cours donc il peut etre la destination
;;;   * Sinon, on libère un registre selon une certaine stratégie donnée (peu importe laquelle)
;;;     puis on le retourne
(define (ctx-get-free-reg cgc ctx #!optional (fixed-regs '())) ;; TODO : ne pas spiller les registres donnés dans fixed-regs

  (define (choose-reg)
    (let ((regs (build-list (length regalloc-regs)
                            (lambda (i)
                               (string->symbol
                               (string-append "r" (number->string i)))))))
      (car regs)))

  ;; Spill du registre reg vers un nouvel emplacement mémoire
  ;; TODO: fusionner code avec spill-free
  (define (spill-new ctx reg)
    (let ((loc (ctx-fs ctx)))
      (let* ((slot-loc
               (foldr (lambda (el r)
                        (if (eq? (cdr el) reg)
                            (cons (cons (car el) loc)
                                  r)
                            (cons el r)))
                      '()
                      (ctx-slot-loc ctx)))
             (free-regs (cons reg (ctx-free-regs ctx))))
        (cons loc
              (make-ctx
                (ctx-stack ctx)
                slot-loc
                free-regs
                (ctx-env ctx)
                (ctx-nb-args ctx)
                (+ (ctx-fs ctx) 1))))))

  ;; Spill du registre reg vers un emplacement mémoire libre existant
  (define (spill-free ctx reg)

    ;; Récupère une emplacement mémoire libre
    (define (get-free-mem-h slot-loc)
      (if (null? slot-loc)
          #f
          (if (not (caar slot-loc))
              (begin (assert (ctx-loc-is-memory? (cdar slot-loc)) "INTERNAL ERROR") (cdar slot-loc))
              (get-free-mem-h (cdr slot-loc)))))

    (let ((loc (get-free-mem-h (ctx-slot-loc ctx))))
      (if (not loc)
          #f
          (let* ((slot-loc
                   (foldr (lambda (el r)
                            (cond ((eq? (cdr el) reg)
                                     (cons (cons (car el) loc) r))
                                  ((eq? (cdr el) loc)
                                     r)
                                  (else
                                     (cons el r))))
                          '()
                          (ctx-slot-loc ctx)))
                 (free-regs (cons reg (ctx-free-regs ctx))))
            (cons loc
                  (make-ctx
                    (ctx-stack ctx)
                    slot-loc
                    free-regs
                    (ctx-env ctx)
                    (ctx-nb-args ctx)
                    (ctx-fs ctx)))))))

  ;; Retourne reg, mem, ctx
  (define (spill ctx)
    (let* ((reg (choose-reg))
           (loc-ctx (spill-free ctx reg)))
      (if loc-ctx
          ;;
          (let ((ropnd (codegen-reg-to-x86reg reg))
                (mopnd (codegen-loc-to-x86opnd (car loc-ctx))))
            (x86-mov cgc mopnd ropnd)
            (cons reg
                  (cdr loc-ctx)))
          ;;
          (let ((loc-ctx (spill-new ctx reg))
                (ropnd (codegen-reg-to-x86reg reg)))
            (x86-push cgc ropnd)
            (cons reg
                  (cdr loc-ctx))))))



  (let ((free-regs (ctx-free-regs ctx)))
    (if (null? free-regs)
        ;;
        (let ((loc-ctx (spill ctx)))
          loc-ctx)
        ;;
        (cons
          (car free-regs)
          ctx))))

;;
;; id-ctx est une liste qui contient des couples (id . lidx)
;; Cette fonction modifie tous les éléments du contexte pour associer l'identifiant de variable à l'index de la pile virtuelle
(define (ctx-bind ctx id-idx mvars)

  (define (remove-sslot slot env)
    (if (null? env)
        '()
        (let ((ident (car env)))
          (if (member slot (identifier-sslots (cdr ident)))
              (cons (cons (car ident)
                          (make-identifier
                            (identifier-kind (cdr ident))
                            (set-sub (identifier-sslots (cdr ident)) (list slot) '())
                            (identifier-flags (cdr ident))
                            (identifier-stype (cdr ident))
                            (identifier-cloc (cdr ident))))
                    (remove-sslot slot (cdr env)))
              (cons ident (remove-sslot slot (cdr env)))))))

  (define (bind-one ctx id-idx)
    (let ((identifier
            (make-identifier
              'local
              (list (ctx-lidx-to-slot ctx (cdr id-idx)))
              (if (member (car id-idx) mvars)
                  '(mutable)
                  '())
              #f
              #f)))
      (make-ctx
        (ctx-stack ctx)
        (ctx-slot-loc ctx)
        (ctx-free-regs ctx)
        (cons (cons (car id-idx) identifier)
              (remove-sslot (ctx-lidx-to-slot ctx (cdr id-idx))
                            (ctx-env ctx)))
        (ctx-nb-args ctx)
        (ctx-fs ctx))))

  (foldr (lambda (el ctx) (bind-one ctx el))
         ctx
         id-idx))

;; TODO: utilisé que dans le letrec(?) faire ça autrement
(define (ctx-move-type ctx lfrom lto)
  (make-ctx
    (let ((old (ctx-stack ctx)))
      (append (list-head old lto) (cons (list-ref old lfrom) (list-tail old (+ lto 1)))))
    (ctx-slot-loc ctx)
    (ctx-free-regs ctx)
    (ctx-env ctx)
    (ctx-nb-args ctx)
    (ctx-fs ctx)))

(define (ctx-stack-push ctx type)
  (make-ctx (cons type (ctx-stack ctx))
            (ctx-slot-loc ctx)
            (ctx-free-regs ctx)
            (ctx-env ctx)
            (ctx-nb-args ctx)
            (ctx-fs ctx)))

;; ids est une liste de symboles qui correspond aux identifiants
;; Cette fonction supprime les liaisons du contexte (libere registres/memoire et enleve les identifiers)
(define (ctx-unbind ctx ids)
  (define (env-remove-id env id)
    (if (null? env)
        '()
        (if (eq? (car (car env)) id)
            (cdr env)
            (cons (car env) (env-remove-id (cdr env) id)))))
  (define (remove-id ctx id)
    (make-ctx (ctx-stack ctx)
              (ctx-slot-loc ctx)
              (ctx-free-regs ctx)
              (env-remove-id (ctx-env ctx) id)
              (ctx-nb-args ctx)
              (ctx-fs ctx)))
  (foldr (lambda (el ctx) (remove-id ctx el))
         ctx
         ids))

;; TODO comment + mov?
;; TODO: l'environnement n'est pas modifié et est géré séparément TODO uniformiser ca dans les fonctions ctx-...
(define (ctx-move-lidx ctx lfrom lto)

  (define (get-loc slot-loc slot)
    (if (null? slot-loc)
        (error "NYD error")
        (if (eq? (caar slot-loc) slot)
            (cdar slot-loc)
            (get-loc (cdr slot-loc) slot))))

  (define (slot-loc-move slot-loc sfrom sto)
    (foldr (lambda (el r)
             (cond ;; Si on trouve le slot from, on remplace par le slot to
                   ((eq? (car el) sfrom)
                     (cons (cons sto (cdr el))
                           r))
                   ;; Si on trouve le slot to, on libere
                   ((eq? (car el) sto)
                     (if (ctx-loc-is-register? (cdr el))
                         r
                         (cons (cons #f (cdr el)) r)))
                   ;;
                   (else (cons el r))))
           '()
           slot-loc))
  (let* ((stack
           (let ((old (ctx-stack ctx)))
             (append (list-head old lto) (cons (list-ref old lfrom) (list-tail old (+ lto 1))))))
         (loc (get-loc (ctx-slot-loc ctx) (ctx-lidx-to-slot ctx lto)))
         (slot-loc
           (slot-loc-move (ctx-slot-loc ctx) (ctx-lidx-to-slot ctx lfrom) (ctx-lidx-to-slot ctx lto)))
         (free-regs
           (if (ctx-loc-is-register? loc)
               (cons loc (ctx-free-regs ctx))
               (ctx-free-regs ctx)))
         (env
           (ctx-env ctx))
         (nb-args (ctx-nb-args ctx))
         (fs (ctx-fs ctx)))

  (make-ctx stack slot-loc free-regs env nb-args fs)))

;; TODO regalloc
;;--------------------------------------------------
;; TESTS
;;--------------------------------------------------
;
;;; TODO: uniformiser et placer
;; TODO: not 3 & 5 because rdi and R11 are used for ctx, nb-args
(define args-regs '(r0 r1 r2 r4 r6)) ;; TODO

(define (ctx-get-call-args-moves ctx nb-args)

   (define slot-loc (ctx-slot-loc ctx))
   (define len-stack (length (ctx-stack ctx)))
   (define all-reg
     (build-list (length regalloc-regs) (lambda (i)
                                          (string->symbol
                                            (string-append "r" (number->string i))))))

   ;; Check if value in location 'loc' is used for
   ;; a next argument (if so, we can' overwrite it)
   (define (is-used-after? loc curr-slot lim-slot)
     (foldr (lambda (el r)
              (or (and (eq? (cdr el) loc)
                       (> (car el) curr-slot)
                       (<= (car el) lim-slot))
                  r))
            #f
            slot-loc))

   ;; Return the ordered locations
   (define (pushed-locs)
     (define (pushed-locs-h lidx)
       (if (< lidx 0)
           '()
           (let ((r (pushed-locs-h (- lidx 1))))
             (cons
               (ctx-get-loc ctx (ctx-lidx-to-slot ctx lidx))
               r))))
     (pushed-locs-h (- nb-args (length args-regs) 1)))

   ;; Return the registers unused for the function call
   ;; A register is unused if it is free or associated to
   ;; a value which is not an argument
   (define (get-unused-regs)
     (let* ((slot-start (- len-stack nb-args))
            (slot-end   (+ slot-start (length args-regs) -1)))
       (foldr (lambda (el r)
                (if (or (not (car el))
                        (and (>= (car el) slot-start)
                             (<= (car el) slot-end)))
                    r
                    (if (ctx-loc-is-register? (cdr el))
                        (cons (cdr el) r)
                        r)))
              '()
              slot-loc)))

   ;; Compute and return all moves needed to put the arguments in the
   ;; calling convention registers
   ;; Return a pair with:
   ;; moves in car
   ;; moves needed to retrieve saved values in unused registers
   (define (move-regs unregs)
     (define (rec-loop rem lidx args-regs unregs)
       (if (= rem 0)
           (cons '() '())
           (let* ((src (ctx-get-loc ctx (ctx-lidx-to-slot ctx lidx)))
                  (dst (car args-regs)))
             (cond ((eq? src dst)
                      (let* ((moves/save (rec-loop (- rem 1) (- lidx 1) (cdr args-regs) unregs))
                             (moves (car moves/save))
                             (save  (cdr moves/save)))
                        moves/save))
                   ((is-used-after? dst (ctx-lidx-to-slot ctx lidx) (ctx-lidx-to-slot ctx (- lidx rem -1)))
                      (let* ((moves/save (rec-loop (- rem 1) (- lidx 1) (cdr args-regs) (cdr unregs)))
                             (moves (car moves/save))
                             (save  (cdr moves/save)))
                        (cons
                          (cons (cons (car unregs) src) moves)
                          (cons (cons dst (car unregs)) save))))
                   (else
                        (let* ((moves/save (rec-loop (- rem 1) (- lidx 1) (cdr args-regs) unregs))
                               (moves (car moves/save))
                               (save  (cdr moves/save)))
                          (cons
                            (cons (cons dst src) moves)
                            save)))))))

     (let ((moves/save
             (rec-loop
               (min nb-args (length args-regs))
               (- nb-args 1)
               args-regs
               unregs)))
       (append (car moves/save) (cdr moves/save))))

 ;;; GO
 (let* (;; 1 - Get unused registers
        (unregs
          (set-sub (append (ctx-free-regs ctx)
                           (get-unused-regs))
                   args-regs
                   '()))
        ;; 2 - Get needed moves
        (moves  (move-regs unregs))
        ;; 3 - Get locs to push
        (locs   (pushed-locs)))

 (cons locs moves)))

;;-----------------------------------------------------------------------------

;; Add callback
(define (add-callback cgc max-selector callback-fn)
  (create-stub label-do-callback-handler max-selector callback-fn))

;; Add function callback
(define (add-fn-callback cgc max-selector callback-fn)
  (create-stub label-do-callback-fn-handler max-selector callback-fn))

;; Add continuation callback
(define (add-cont-callback cgc max-selector callback-fn)
  (create-stub label-do-callback-cont-handler max-selector callback-fn))

;;-----------------------------------------------------------------------------
;; Lazy-code generation

(define (jump-to-version cgc lazy-code ctx #!optional (cleared-ctx #f))

  (let ((label-dest (get-version lazy-code ctx)))
    (if label-dest

        ;; That version has already been generated, so just jump to it
        (x86-jmp cgc label-dest)

        ;; That version is not yet generated, so generate it
        (if (and opt-max-versions
                 (not cleared-ctx)
                 (>= (lazy-code-nb-versions lazy-code) opt-max-versions))
          ;; Maxversions is not #f and limit is reached, then remove
          ;; type information to generate a generic version
          ;; Recursive call (maybe this version already exists)
          (jump-to-version cgc lazy-code (ctx-clear ctx) #t)

          ;; Generate that version inline
          (let ((label-version (asm-make-label cgc (new-sym 'version))))
            (put-version lazy-code ctx label-version)
            (x86-label cgc label-version)
            ((lazy-code-generator lazy-code) cgc ctx))))))

;; Generate a continuation
(define (gen-version-continuation load-ret-label lazy-code ctx)

  (if opt-verbose-jit
      (begin
        (print "GEN VERSION CONTINUATION (RP)")
        (print " >>> Patch label ")
        (pp load-ret-label)
        (println " with ctx:")
        (pp ctx)))

  (let ((continuation-label (asm-make-label #f (new-sym 'continuation_)))
        (load-addr (asm-label-pos load-ret-label)))

    ;; Generate lazy-code
    (code-add
      (lambda (cgc)
        (asm-align cgc 4 0 #x90)
        (x86-label cgc continuation-label)
        ((lazy-code-generator lazy-code) cgc ctx)))

    (patch-continuation load-addr continuation-label)))

;; Generate continuation using cr table (patch cr entry)
(define (gen-version-continuation-cr lazy-code ctx type table)

  (if opt-verbose-jit
      (begin
        (print "GEN VERSION CONTINUATION (CR)")
        (print " >>> Patch table with type ")
        (print type)
        (println " and ctx:")
        (pp ctx)))

  (let ((label-dest (get-version lazy-code ctx)))

    (if (not label-dest)
        ;; That version is not yet generated, so generate it
        (let ((continuation-label (asm-make-label #f (new-sym 'continuation_))))
          (code-add
            (lambda (cgc)
              (asm-align cgc 4 0 #x90)
              (x86-label cgc continuation-label)
              ((lazy-code-generator lazy-code) cgc ctx)))
          (put-version lazy-code ctx continuation-label)
          (set! label-dest continuation-label)))

    (patch-continuation-cr label-dest type table)))

;; Generate an entry point
(define (gen-version-fn ast closure lazy-code gen-ctx call-ctx generic)

  (if opt-verbose-jit
      (begin
        (print "GEN VERSION FN")
        (print " >>> ")
        (pp gen-ctx)
        (pp call-ctx)))

  (let ((label-dest (get-version lazy-code gen-ctx)))

    (if (not label-dest)
       ;; That version is not yet generated, so generate it
       (let ((fn-label (asm-make-label #f (new-sym 'fn_entry_))))
          (code-add
             (lambda (cgc)
                (asm-align cgc 4 0 #x90)
                (x86-label cgc fn-label)
                ((lazy-code-generator lazy-code) cgc gen-ctx)))
          (put-version lazy-code gen-ctx fn-label)
          (set! label-dest fn-label)))

    ;; If generic is #t, patch generic slot in closure.
    ;; Else patch cc-table slot for this ctx
    (if generic
      (patch-generic ast closure label-dest)
      (patch-closure closure call-ctx label-dest))))

(define (gen-version jump-addr lazy-code ctx #!optional (cleared-ctx #f))

  (if opt-verbose-jit
      (begin
        (print "GEN VERSION")
        (print " >>> ")
        (pp ctx)))

  (let ((label-dest (get-version lazy-code ctx)))
    (if label-dest

        ;; That version has already been generated, so just patch jump
        (let ((dest-addr (asm-label-pos label-dest)))
          (patch-jump jump-addr dest-addr)
          dest-addr)

        ;; That version is not yet generated, so generate it
        (if (and opt-max-versions
                 (not cleared-ctx)
                 (>= (lazy-code-nb-versions lazy-code) opt-max-versions))
          ;; Maxversions is not #f and limit is reached, then remove
          ;; type information to generate a generic version
          ;; Recursive call (maybe this version already exists)
          (gen-version jump-addr lazy-code (ctx-clear ctx) #t)

          ;; Generate that version inline
          (begin
            (cond ((= (+ jump-addr (jump-size jump-addr)) code-alloc)
                   ;; (fall-through optimization)
                   ;; the jump is the last instruction previously generated, so
                   ;; just overwrite the jump
                   (if opt-verbose-jit (println ">>> fall-through-optimization"))
                   (set! code-alloc jump-addr))
                  (else
                   ;; Else we need to patch the jump
                   (patch-jump jump-addr code-alloc)))
            ;; Generate
            (let ((label-version (asm-make-label #f (new-sym 'version))))
              (put-version lazy-code ctx label-version)
              (code-add
               (lambda (cgc)
                 (asm-align cgc 4 0 #x90)
                 (x86-label cgc label-version)
                 ((lazy-code-generator lazy-code) cgc ctx)))
              (asm-label-pos label-version)))))))

;;-----------------------------------------------------------------------------

;; Patch load at a call site to load continuation addr instead of continuation stub addr
(define (patch-continuation load-addr continuation-label)
  (if opt-verbose-jit
      (begin
        (println ">>> patching mov at "
                 (number->string load-addr 16)
                 " : now load "
                 (asm-label-name continuation-label)
                 " (" (number->string (asm-label-pos continuation-label) 16) ")")))

  (code-gen 'x86-64 load-addr (lambda (cgc) (x86-mov cgc (x86-rax) (x86-imm-int (asm-label-pos continuation-label)))))
  (asm-label-pos continuation-label))

;; Patch continuation if using cr (write label addr in table instead of compiler stub addr)
(define (patch-continuation-cr continuation-label type table)
    ;; TODO: msg if opt-verbose-jit (see patch-continuation)
    (put-i64 (+ (type-to-cridx type) table) (asm-label-pos continuation-label))
    (asm-label-pos continuation-label))

;; Patch jump at jump-addr: change jump destination to dest-addr
(define (patch-jump jump-addr dest-addr)

  (if opt-verbose-jit
    (println ">>> patching jump at "
             (number->string jump-addr 16)
             " -> "
             (number->string dest-addr 16)))

  (if (not (= jump-addr dest-addr))
      (let ((size (jump-size jump-addr)))
        (put-i32 (- (+ jump-addr size) 4)
                 (- dest-addr (+ jump-addr size))))))

;; Patch generic slot in closure
(define (patch-generic ast closure label)

  (let ((label-addr (asm-label-pos  label))
        (label-name (asm-label-name label)))

  (if opt-verbose-jit
      (println ">>> patching generic slot of closure " (number->string closure 16)
               ": now contains label "
               label-name
               " (" (number->string label-addr 16) ")"))

  (if opt-entry-points
      (let ((table-addr (get-i64 (- (+ closure 8) TAG_MEMOBJ))))
        (put-i64 (+ table-addr 8) label-addr))
      (let ((eploc (get-entry-points-loc ast #f)))
        (put-i64 (- (+ closure 8) TAG_MEMOBJ) label-addr)       ;; Patch closure
        (put-i64 (+ 8 (- (obj-encoding eploc) 1)) label-addr))) ;; Patch still vector containing code addr


  label-addr))

;; Patch closure
(define (patch-closure closure ctx label)

  (let* ((label-addr (asm-label-pos  label))
         (label-name (asm-label-name label))
         (index (or (get-closure-index ctx) -1)) ;; -1 TODO to patch generic
         (offset (+ 16 (* index 8)))) ;; +16 (header & generic)

    (if opt-verbose-jit
        (println ">>> patching closure " (number->string closure 16) " at "
                 (number->string (+ closure offset) 16)
                 " : slot contains now label "
                 label-name
                 " ("
                 (number->string label-addr 16)
                 ")"))

    (let ((cctable-addr (get-i64 (- (+ closure 8) TAG_MEMOBJ)))) ;; +8(header) - 1(tag)
      (put-i64 (+ cctable-addr offset) label-addr))

    label-addr))

(define (jump-size jump-addr)
  (if (= (get-u8 jump-addr) #x0f) 6 5))


;; Gen fatal dynamic type test
;; fatal means that if type test fails, it stops execution
;; Check type 'type' for stack slot at 'stack-idx' and jump to 'succ' if succeess
(define (gen-fatal-type-test type stack-idx succ #!optional ast)
(let ((lazy-error
         (make-lazy-code
            (lambda (cgc ctx)
               (if (or (eq? type CTX_FLO) (eq? type CTX_NUM))
                 (gen-error cgc (ERR_TYPE_EXPECTED CTX_NUM))
                 (gen-error cgc (ERR_TYPE_EXPECTED type)))))))
  (gen-dyn-type-test type stack-idx succ lazy-error ast)))

;; Create lazy code for type test of stack slot (stack-idx)
;; jump to lazy-success if test succeeds
;; jump to lazy-fail if test fails
(define (gen-dyn-type-test type stack-idx lazy-success lazy-fail #!optional ast)

  (make-lazy-code
     (lambda (cgc ctx)

       ;; TODO: plus nettoyer tout ca
       (let* ((ctx-success (ctx-change-type ctx stack-idx type))
              (ctx-success-known ctx);; If know type is tested type, do not change ctx
              (ctx-fail ctx)
              (known-type (list-ref (ctx-stack ctx) stack-idx)))
         (cond ;; known == expected
               ((or (eq? known-type type)
                    (and (pair? known-type) (eq? (car known-type) type)))
                  (jump-to-version cgc lazy-success ctx-success-known))
               ;; known != expected && known != unknown
               ((not (eq? known-type CTX_UNK))
                  (jump-to-version cgc lazy-fail ctx-fail))
               ;; known == unknown
               (else
                 (let* ((label-jump (asm-make-label cgc (new-sym 'patchable_jump)))
                        (stub-labels
                              (add-callback cgc 1
                                (let ((prev-action #f))

                                  (lambda (ret-addr selector)
                                    (let ((stub-addr (- ret-addr 5 2))
                                          (jump-addr (asm-label-pos label-jump)))

                                      (if opt-verbose-jit
                                          (begin
                                            (println ">>> selector= " selector)
                                            (println ">>> prev-action= " prev-action)))

                                      (if (not prev-action)

                                          (begin (set! prev-action 'no-swap)
                                                 (if (= selector 1)

                                                    ;; overwrite unconditional jump
                                                    (gen-version (+ jump-addr 6) lazy-fail ctx-fail)

                                                    (if (= (+ jump-addr 6 5) code-alloc)

                                                      (begin (if opt-verbose-jit (println ">>> swapping-branches"))
                                                             (set! prev-action 'swap)
                                                             ;; invert jump direction
                                                             (put-u8 (+ jump-addr 1) (fxxor 1 (get-u8 (+ jump-addr 1))))
                                                             ;; make conditional jump to stub
                                                             (patch-jump jump-addr stub-addr)
                                                             ;; overwrite unconditional jump
                                                             (gen-version
                                                             (+ jump-addr 6)
                                                             lazy-success
                                                             ctx-success))

                                                      ;; make conditional jump to new version
                                                      (gen-version jump-addr lazy-success ctx-success))))

                                          (begin ;; one branch has already been patched
                                                 ;; reclaim the stub
                                                 (release-still-vector (get-scmobj ret-addr))
                                                 (stub-reclaim stub-addr)
                                                 (if (= selector 0)
                                                    (gen-version (if (eq? prev-action 'swap) (+ jump-addr 6) jump-addr) lazy-success ctx-success)
                                                    (gen-version (if (eq? prev-action 'swap) jump-addr (+ jump-addr 6)) lazy-fail ctx-fail))))))))))

       (if opt-verbose-jit
           (println ">>> Gen dynamic type test at index " stack-idx))

       ;; If 'opt-stats' option, then inc tests slot
       (if opt-stats
        (gen-inc-slot cgc 'tests))

       (let* ((lval (ctx-get-loc ctx (ctx-lidx-to-slot ctx stack-idx)))
              (opval (codegen-loc-to-x86opnd lval)))

         (cond ;; Number type check
               ((eq? type CTX_NUM)
                 (x86-mov cgc (x86-rax) (x86-imm-int 3))
                 (x86-and cgc (x86-rax) opval))
               ;; Null type test
               ((eq? type CTX_NULL)
                 (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                 (x86-cmp cgc (x86-rax) opval))
               ;; Char type check
               ((eq? type CTX_CHAR)
                  (x86-mov cgc (x86-rax) (x86-imm-int (+ (* -1 (expt 2 63)) TAG_SPECIAL)))
                  (x86-and cgc (x86-rax) opval)
                  (x86-cmp cgc (x86-rax) (x86-imm-int TAG_SPECIAL))) ;; TODO: not enough ? TAG_SPECIAL is uses by other types ?
               ;; Procedure type test
               ((member type (list CTX_FLO CTX_CLO CTX_PAI CTX_SYM CTX_VECT CTX_STR CTX_IPORT CTX_OPORT))
                  ;; Vérifier le tag memobj
                  ;; extraire le tag du header
                  ;; Vérifier le tag stag

                  ;; Check tag
                  (x86-mov cgc (x86-rax) opval)
                  (x86-and cgc (x86-rax) (x86-imm-int 3))
                  (x86-cmp cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                  (x86-jne cgc label-jump)
                  ;; Check stag
                  (if (ctx-loc-is-memory? lval)
                      (begin
                        (x86-mov cgc (x86-rax) opval)
                        (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) (x86-rax))))
                      (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) opval)))
                  (x86-and cgc (x86-rax) (x86-imm-int 248)) ;; 0...011111000 to get type in object header
                  ;; stag xxx << 3
                  (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (cond ((eq? type CTX_FLO)  STAG_FLONUM)
                                                                 ((eq? type CTX_CLO)  STAG_PROCEDURE)
                                                                 ((eq? type CTX_SYM)  STAG_SYMBOL)
                                                                 ((eq? type CTX_STR)  STAG_STRING)
                                                                 ((eq? type CTX_IPORT) STAG_IPORT)
                                                                 ((eq? type CTX_OPORT) STAG_OPORT)
                                                                 ((eq? type CTX_VECT) STAG_VECTOR)
                                                                 ((eq? type CTX_PAI)  STAG_PAIR))))))
               ;; Other
               (else (error "Unknown type " type)))

         (x86-label cgc label-jump)
         (x86-je cgc (list-ref stub-labels 0))
         (x86-jmp cgc (list-ref stub-labels 1))))))))))

;;-----------------------------------------------------------------------------
;; Global cc table

;; Current fixed global-cc-table max size
(define global-cc-table-maxsize 500)
;; Current shape of the global cc table
(define global-cc-table (make-table))

(define global-cr-table-maxsize 16) ;; TODO number of types

;; Get closure index associated to ctx. If ctx is not in the
;; global cc table, then this function adds it and returns
;; the new associated index. Index starts from 0.
;; Store and compare ctx-stack in enough because environment is
;; the same for all versions of a lazy-object.
(define (get-closure-index ctx)
  (let ((res (table-ref global-cc-table (ctx-stack ctx) #f)))
    (if res
      ;; Ctx exists in global table
      res
      ;; Ctx does not exists yet
      (if (= (table-length global-cc-table) global-cc-table-maxsize)
        ;; Global table is full
        (if opt-overflow-fallback
          #f
          (error "Global entry points table overflow!"))
        ;; Global table is not full
        (let ((value (table-length global-cc-table)))
          (table-set! global-cc-table (ctx-stack ctx) value)
          value)))))

;; Associates a ctx to the address of a still-vector of length 1 containing only this ctx.
;; This table keep a reference to all ctx of call-sites because these ctx must
;; NOT be collected by scheme GC.
;; This table also keep the address of the still-box associated to the ctx because we can't release
;; this box when version is generated in case of the same call site is used with a
;; non yet generated procedure.
(define ctx-boxes (make-table))

;; Get ctx object from still-vector address
(define (still-ref->ctx addr)
  (let ((v (encoding-obj addr)))
    (vector-ref v 0)))

;; Get still-vector address from ctx
;; This still vector of length 1 contains only ctx
(define (ctx->still-ref ctx)
  (let ((r (table-ref ctx-boxes ctx #f)))
    (if r
      r
      (let ((v (alloc-still-vector 1)))
        (vector-set! v 0 ctx)
        (table-set! ctx-boxes ctx (obj-encoding v))
        (obj-encoding v)))))
