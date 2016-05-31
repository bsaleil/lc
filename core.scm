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
(define opt-overflow-fallback    #f) ;; Automatic fallback to generic entry point if cctable overflows
(define opt-use-lib              #t) ;; Use scheme std lib (see lib/ folder)
(define opt-vers-regalloc        #t) ;; Use register allocation for code specialization
(define opt-dump-bin             #f) ;; Print generated binary bytes to stdout

;;-----------------------------------------------------------------------------

;; Forward declarations
(define init-mss #f)
(define get___heap_limit-addr  #f)
(define get___alloc_still-addr #f)
(define get-pstate-addr #f)
(define run-gc #f)         ;; mem.scm
(define expand-tl #f)      ;; expand.scm
(define gen-ast #f)        ;; ast.scm
(define alloc-ptr #f)
(define global-ptr #f)
(define get-entry-points-loc #f)
(define codegen-loc-to-x86opnd #f)
(define ctime-entries-get #f)

(define get-heap_limit-addr #f)
(define get-hp-addr #f)
(define global-offset #f)
(define selector-reg #f)
(define x86-call-label-aligned-ret #f)

;;-----------------------------------------------------------------------------

;; Global ids
;; Contains a list of global ids with id,position
;; ex. '((foo 1) (bar 2) (fun 3))
(define globals (make-table))
(define nb-globals 0)
(define gids (make-table)) ;; TODO : merge with 'globals'

;;-----------------------------------------------------------------------------

;; Tags
(define TAG_NUMBER  0)
(define TAG_MEMOBJ  1)
(define TAG_SPECIAL 2)
(define TAG_PAIR    3)
;; Char mask is 100000...00011
(define SPECIAL_MASK   (bitwise-not (- (expt 2 63) 4)))

;; STags
(define STAG_VECTOR     0)
(define STAG_PAIR       1)
(define STAG_MOBJECT    5)
(define STAG_SYMBOL     8)
(define STAG_PROCEDURE 14)
(define STAG_IPORT     17)
(define STAG_OPORT     18)
(define STAG_STRING    19)
(define STAG_FLONUM    30)

;; Context types
(define CTX_ALL   '*) ; Represents all ctx types
(define CTX_UNK   'unknown)
(define CTX_INT   'integer)
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
(define CTX_FLO   'float)

(define type-cridx
  `((,CTX_INT  .  8) ;; Start from 8 because of header
    (,CTX_CHAR . 16)
    (,CTX_BOOL . 24)
    (,CTX_CLO  . 32)
    (,CTX_PAI  . 40)
    (,CTX_VOID . 48)
    (,CTX_NULL . 56)
    (,CTX_VECT . 64)
    (,CTX_STR  . 72)
    (,CTX_SYM  . 80)
    (,CTX_IPORT . 88)
    (,CTX_OPORT . 96)
    (,CTX_FLO   . 104)
    (,CTX_UNK   . 112)))

(define (type-to-cridx type)
  (cdr (assoc type type-cridx)))

(define (cridx-to-type cridx)
  (car (find (lambda (el) (= (cdr el) cridx))
             type-cridx)))

;;-----------------------------------------------------------------------------

;; Errors
(define ERR_MSG              "EXEC ERROR")
(define ERR_ARR_OVERFLOW     "ARITHMETIC OVERFLOW")
(define ERR_WRONG_NUM_ARGS   "WRONG NUMBER OF ARGUMENTS")
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
;; Runtime error

;; Return lazy code object which generates an error with 'msg'
(define (get-lazy-error msg)
  (make-lazy-code
    (lambda (cgc ctx)
      (gen-error cgc msg))))

(define (gen-error cgc err #!optional (stop-exec? #t))
  ;; Put error msg in RAX
  (let ((r1 (car regalloc-regs)))
    (x86-push cgc r1)
    (x86-mov cgc r1 (x86-imm-int (obj-encoding err)))
    (x86-call-label-aligned-ret cgc label-rt-error-handler)
    (x86-pop cgc r1)))

;; The procedure exec-error is callable from generated machine code.
;; This function print the error message in rax
(c-define (rt-error sp) (long) void "rt_error" ""
  (let* ((err-enc (get-i64 (+ sp (reg-sp-offset 0))))
         (err (encoding-obj err-enc)))
    (print "!!! ERROR : ")
    (println err)))

;;-----------------------------------------------------------------------------

(c-define (gc sp) (long) long "gc" ""
    ;; TODO remove old gc related code
    (error "NYI GC"))

(define (gen-gc-call cgc)

  ;; Move all regalloc regs (register roots) to stack
  ;; then, we have only global and stack roots
  (push-pop-regs
     cgc
     regalloc-regs
     (lambda (cgc)
       (push-pop-regs
          cgc
          (set-sub c-caller-save-regs (list alloc-ptr) '()) ;; preserve regs for C call
          (lambda (cgc)
            (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
            (x86-and  cgc (x86-rsp) (x86-imm-int -16))
            (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
            (x86-push cgc (x86-rdi))
            (x86-call cgc label-gc) ;; call C function
            (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
            (x86-mov cgc alloc-ptr (x86-rax))))))) ;; TODO : Update alloc-ptr

;;-----------------------------------------------------------------------------

(define (gen-print-* cgc label-handler p1 p2)
  ;; Save rax & rcx.
  ;; We want this function not modify any register
  (let ((r1 (car regalloc-regs))
        (r2 (cadr regalloc-regs)))

    (x86-push cgc r1)
    (x86-push cgc r2)

    (x86-mov cgc r1 p1)
    (x86-mov cgc r2 p2)

    (x86-call-label-aligned-ret cgc label-handler)

    (x86-pop cgc r2)
    (x86-pop cgc r1)))

(define (gen-print-obj cgc reg newline?)
  (gen-print-*
    cgc
    label-print-msg-handler
    reg
    (x86-imm-int (obj-encoding newline?))))

(define (gen-print-reg cgc msg reg)
  (gen-print-*
    cgc
    label-print-msg-val-handler
    (x86-imm-int (obj-encoding msg))
    reg))

;; Print msg which is encoded in rax
;; Print val which is in rcx
(c-define (print-msg-val sp) (long) void "print_msg_val" ""
  (let* ((msg (encoding-obj (get-i64 (+ sp (reg-sp-offset 0)))))
         (val (get-i64 (+ sp (reg-sp-offset 1)))))
    (print msg " ")
    (println val)))

;; Print msg which is encoded in rax
;; if rcx contains #t, print a newline too
(c-define (print-msg sp) (long) void "print_msg" ""
  (let* ((msg      (encoding-obj (get-i64 (+ sp (reg-sp-offset 0)))))
         (newline? (= (encoding-obj (get-i64 (+ sp (reg-sp-offset 1)))) 1)))

    (and (print msg) (force-output) newline? (newline))))

;;-----------------------------------------------------------------------------

;; TODO WIP
(c-define (gambit-call sp) (long) void "gambit_call" ""
  (let* ((nargs
           (encoding-obj (get-i64 (+ sp (* (+ (length regalloc-regs) 3) 8)))))
         (op-sym
           (encoding-obj (get-i64 (+ sp (* (+ (length regalloc-regs) 2) 8)))))
         (args
           (reverse
             (let loop ((n nargs) (offset 4))
               (if (= n 0)
                   '()
                   (let ((word (get-u64 (+ sp (* (+ (length regalloc-regs) offset) 8)))))
                     (cons (encoding-obj word)
                           (loop (- n 1) (+ offset 1))))))))

         (op-fn (eval op-sym)))

    (let ((retval (apply op-fn args)))

      (put-i64 (+ sp (* (+ (length regalloc-regs) 2) 8))
               (obj-encoding retval)))))

;;-----------------------------------------------------------------------------

;; The procedures do-callback* are callable from generated machine code.
;; RCX holds selector (CL)
(c-define (do-callback sp) (long) void "do_callback" ""
  (let* ((ret-addr
          (get-i64 (+ sp (* (+ (length regalloc-regs) 1) 8))))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (selector
          (encoding-obj (get-i64 (+ sp (selector-sp-offset)))))

         (new-ret-addr
          (callback-fn ret-addr selector)))

    ;; replace return address
    (put-i64 (+ sp (* (+ (length regalloc-regs) 1) 8))
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ sp (selector-sp-offset))
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
          (get-i64 (+ sp (* (+ (length regalloc-regs) 1) 8))))

         (selector
          (encoding-obj (get-i64 (+ sp (selector-sp-offset)))))

         (ctx-idx
          (if opt-entry-points
              (encoding-obj (get-i64 (+ sp (reg-sp-offset-r (x86-r11)))))
              #f))

         (stack
           (if (or (not opt-entry-points) (= selector 1))
               #f
               (global-cc-get-ctx ctx-idx)))

         ;; Closure is used as a Gambit procedure to keep an updated reference
         (closure
          (encoding-obj (get-i64 (+ sp (reg-sp-offset-r (x86-rsi))))))

         (nb-args
           (if (or (not opt-entry-points) (= selector 1))
               (let ((encoded (get-i64 (+ sp (reg-sp-offset-r (x86-rdi))))))
                 (arithmetic-shift encoded -2))
               (- (length stack) 2)))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (new-ret-addr
          (callback-fn sp stack ret-addr selector closure)))

    ;; replace return address
    (put-i64 (+ sp (* (+ (length regalloc-regs) 1) 8))
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ sp (selector-sp-offset))
             0)))

;; The procedures do-callback* are callable from generated machine code.
;; RCX holds selector (CL)
(c-define (do-callback-cont sp) (long) void "do_callback_cont" ""

  (let* ((ret-addr
          (get-i64 (+ sp (* (+ (length regalloc-regs) 1) 8))))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (selector
          (encoding-obj (get-i64 (+ sp (selector-sp-offset)))))

         (type-idx
          (encoding-obj (get-i64 (+ sp (reg-sp-offset-r (x86-r11))))))

         (table
          (get-i64 (+ sp (reg-sp-offset-r (x86-rdx)))))

         (type (cridx-to-type type-idx))

         (new-ret-addr
           (callback-fn ret-addr selector type table)))

    ;; replace return address
    (put-i64 (+ sp (* (+ (length regalloc-regs) 1) 8))
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ sp (selector-sp-offset))
             0)))

;;-----------------------------------------------------------------------------
;; Interned symbols
;; TODO

(define sym-space-len 100000)
(define sym-space (make-mcb sym-space-len))
(define sym-alloc (##foreign-address sym-space))

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

;;-----------------------------------------------------------------------------

;; Create label used by generated machine code to call c-define functions
(define-macro (set-cdef-label! label sym c-code)
  `(set! ,label
         (asm-make-label
           cgc
           ,sym
           (##foreign-address
            ((c-lambda ()
                       (pointer void)
                       ,c-code))))))

(define (init-labels cgc)
  ;; TODO WIP
  (set-cdef-label! label-gc               'gc               "___result = ___CAST(void*,gc);")

  (set-cdef-label! label-print-msg        'print-msg        "___result = ___CAST(void*,print_msg);")
  (set-cdef-label! label-print-msg-val    'print-msg-val    "___result = ___CAST(void*,print_msg_val);")
  (set-cdef-label! label-rt-error         'rt_error         "___result = ___CAST(void*,rt_error);")
  (set-cdef-label! label-gambit-call      'gambit_call      "___result = ___CAST(void*,gambit_call);")
  (set-cdef-label! label-do-callback      'do_callback      "___result = ___CAST(void*,do_callback);")
  (set-cdef-label! label-do-callback-fn   'do_callback_fn   "___result = ___CAST(void*,do_callback_fn);")
  (set-cdef-label! label-do-callback-cont 'do_callback_cont "___result = ___CAST(void*,do_callback_cont);")
  (set-cdef-label! label-breakpoint       'break_point      "___result = ___CAST(void*,break_point);"))

;;-----------------------------------------------------------------------------

;; Machine code block management

;; HEAP
(define from-space #f)
(define to-space   #f)
;; Set to 2gb (2000000000) to exec all benchmarks without GC (except lattice.scm)
;; Set to 7gb (7000000000) to exec all benchmarks without GC
(define space-len 7000000000)
;(define space-len 4000000000)
;(define space-len 1000000000)

(assert (= (modulo space-len 8) 0) ERR_HEAP_NOT_8)

(define init-to-space #f)
(define init-from-space #f)

;; CODE
(define code-len 12000000)
(define code-addr #f)
(define mcb #f)

;; User stack (ustack) is the used by generated machine code
;; This stack is a scheme object. This allows the Gambit GC to scan the stack
;; to find roots.
;; Process stack (pstack) is still used for each call to c code (stubs and others)
(define ustack #f)
(define ustack-init #f) ;; initial rsp value (right of the stack)
(define ustack-len 1000000) ;; 1M

;; TODO: use real pstack
(define pstack #f)
(define pstack-init #f)
(define pstack-len 500000) ;; 512ko

(define (init-mcb)
  (set! mcb (make-mcb code-len))
  (set! code-addr (##foreign-address mcb))
  (let ((tspace (make-mcb space-len))
        (fspace (make-mcb space-len)))
    (set! init-from-space (+ (##foreign-address fspace) space-len))
    (set! init-to-space   (+ (##foreign-address tspace) space-len)))
    ;(println "### Code segment: " code-addr " -> " (+ code-addr code-len))
    ;(println "### Initial from space: " (##foreign-address fspace) " -> " (+ (##foreign-address fspace) space-len))
    ;(println "### Initial to space  : " (##foreign-address tspace) " -> " (+ (##foreign-address tspace) space-len)))
  (set! from-space init-from-space)
  (set! to-space init-to-space)
  (init-mss)
  (set! ustack (make-vector (/ ustack-len 8)))
  (set! ustack-init (+ (- (obj-encoding ustack) 1) 8 ustack-len))

  ;;; stack
  ;(set! ustack (make-u8vector ustack-len))
  ;(set! ustack-init (+ (- (obj-encoding ustack) 1) 8 ustack-len))
  ;;; Align initial stack pointer value to avoid weird segfault
  ;(let ((extra (modulo ustack-init 8)))
  ;  (set! ustack-init (- ustack-init extra)))
  ;; TODO: use real pstack
  (set! pstack (make-u8vector pstack-len))
  (set! pstack-init (+ (- (obj-encoding pstack) 1) 8 pstack-len))
  (let ((extra (modulo pstack-init 16)))
    (set! pstack-init (- pstack-init extra)))
  (assert (= (modulo pstack-init 16) 0) "Internal ERROR"))

;; BLOCK :
;; 0          8                       (nb-globals * 8 + 8)
;; +----------+----------+----------+----------+
;; | Bottom   | Global 1 |    ...   | Global n |
;; | stack    |          |          |          |
;; | addr     |          |          |          |
;; +----------+----------+----------+----------+

(define globals-space #f)
(define globals-len 10000) ;; 1024 globals
(define globals-addr #f)
(define block #f)
(define block-len 15)
(define block-addr #f)
(define debug-slots '((calls . 6) (tests . 7) (extests . 8) (closures . 9) (time . 10) (other . 11)))

(define (init-block)
  (set! globals-space (alloc-still-vector-0 globals-len))
  (set! globals-addr (+ (- (obj-encoding globals-space) 1) 8))

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
                (set! stub-alloc (- stub-alloc 24))
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
                 #f
                 (string->symbol
                  (string-append "stub_"
                                 (number->string alloc 16)
                                 "_"
                                 (number->string i))))))
           (set! stub-labels (cons label stub-labels))
           (x86-label cgc label)
           (if (> i 0)
               (begin
                 (x86-add cgc (x86-ecx) (x86-imm-int (obj-encoding 1))) ;; increment selector
                 (x86-nop cgc)
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
    ;(if opt-verbose-jit
    ;    (pp (list 'obj= obj)))
    stub-labels))

(define (call-handler cgc label-handler obj)
  (x86-call-label-aligned-ret cgc label-handler)
  (asm-64   cgc (obj-encoding obj)))

(define (stub-reclaim stub-addr)
  (put-i64 stub-addr stub-freelist)
  (set! stub-freelist stub-addr))

;;-----------------------------------------------------------------------------

(define label-heap-limit-handler       #f)
(define label-alloc-still-handler      #f)
(define label-gambit-call-handler      #f)
(define label-do-callback-handler      #f)
(define label-do-callback-fn-handler   #f)
(define label-do-callback-cont-handler #f)
(define label-breakpoint-handler       #f)
(define label-rt-error-handler         #f)
(define label-print-msg-handler        #f)
(define label-print-msg-val-handler    #f)

(define (gen-addr-handler cgc id addr cargs-generator)
  (let ((label-handler (asm-make-label cgc id)))

    (x86-label cgc label-handler)

    ;; Save regalloc-regs to ustack because stub could trigger GC,
    ;; then registers are treated as stack roots
    ;; Save selector to allow stub to access its value
    (push-pop-regs
      cgc
      (cons selector-reg regalloc-regs)
      (lambda (cgc)
        ;; Update gambit heap ptr from LC heap ptr
        (x86-mov cgc (x86-mem (get-hp-addr)) alloc-ptr)
        ;; Move ustack in c first arg register
        (x86-mov cgc (x86-rdi) (x86-rsp))
        ;; Set rsp to pstack
        (x86-mov cgc (x86-rsp) (x86-imm-int pstack-init))
        ;; Save ustack ptr to pstack
        (x86-push cgc (x86-rdi))
        ;; Save c caller save registers
        (push-pop-regs
          cgc
          (set-sub c-caller-save-regs regalloc-regs '())
          (lambda (cgc)
            (cargs-generator cgc) ;; Gen c args
            ;; Aligned call to addr
            (x86-mov cgc (x86-rax) (x86-imm-int addr))
            (x86-call-label-aligned-ret cgc (x86-rax))))
        ;; Update LC heap ptr and heap limit from Gambit heap ptr and heap limit
        (let ((r1 selector-reg)
              (r2 alloc-ptr))
          ;; heap limit
          (x86-mov cgc r1 (x86-mem (get-heap_limit-addr)))
          (x86-mov cgc r2 (x86-imm-int block-addr))
          (x86-mov cgc (x86-mem (* 8 5) r2) r1)
          ;; hp
          (x86-mov cgc alloc-ptr (x86-mem (get-hp-addr))))
        ;; Set rsp to saved ustack sp
        (x86-pop cgc (x86-rsp))))

    label-handler))

(define (gen-handler cgc id label)
  (let ((label-handler (asm-make-label cgc id)))

    (x86-label cgc label-handler)

    ;; Save regalloc-regs to ustack because stub could trigger GC,
    ;; then registers are treated as stack roots
    ;; Save selector to allow stub to access its value
    (push-pop-regs
      cgc
      (cons selector-reg regalloc-regs)
      (lambda (cgc)
        ;; Update gambit heap ptr from LC heap ptr
        (x86-mov cgc (x86-mem (get-hp-addr)) alloc-ptr)
        ;; Move ustack in c first arg register
        (x86-mov cgc (x86-rdi) (x86-rsp))
        ;; Set rsp to pstack
        (x86-mov cgc (x86-rsp) (x86-imm-int pstack-init))
        ;; Save ustack ptr to pstack
        (x86-push cgc (x86-rdi))
        ;; Save c caller save registers
        (push-pop-regs
          cgc
          (set-sub c-caller-save-regs regalloc-regs '())
          (lambda (cgc)
            ;; Aligned call to label
            (x86-call-label-aligned-ret cgc label)))
        ;; Update LC heap ptr and heap limit from Gambit heap ptr and heap limit
        (let ((r1 selector-reg)
              (r2 alloc-ptr))
          ;; heap limit
          (x86-mov cgc r1 (x86-mem (get-heap_limit-addr)))
          (x86-mov cgc r2 (x86-imm-int block-addr))
          (x86-mov cgc (x86-mem (* 8 5) r2) r1)
          ;; hp
          (x86-mov cgc alloc-ptr (x86-mem (get-hp-addr))))
        ;; Set rsp to saved ustack sp
        (x86-pop cgc (x86-rsp))))

    label-handler))

(define (init-rtlib cgc)
  (let ((label-rtlib-skip (asm-make-label cgc 'rtlib_skip)))

    (x86-jmp cgc label-rtlib-skip)

    ;; heap_limit
    (set! label-heap-limit-handler
          (gen-addr-handler cgc 'heap_limit_handler (get___heap_limit-addr) (lambda (cgc) #f)))
    (x86-ret cgc)

    ;; heap_limit
    (set! label-alloc-still-handler
          (gen-addr-handler cgc 'alloc_still_handler (get___alloc_still-addr)
            (lambda (cgc)
              ;; rdi rsi rdx (pstate, stag, len)
              ;;(x86-mov cgc (x86-rdi) (x86-imm-int 0))
              (x86-mov cgc (x86-rsi) (x86-mem (* 8 (+ (length regalloc-regs) 2)) (x86-rdi)))
              (x86-mov cgc (x86-rdx) (x86-mem (* 8 (+ (length regalloc-regs) 4)) (x86-rdi)))
              (x86-mov cgc (x86-rdi) (x86-imm-int (get-pstate-addr))))))
    (x86-ret cgc)

    (set! label-gambit-call-handler
          (gen-handler cgc 'gambit_call_handler label-gambit-call))
    (x86-ret cgc)

    ;; do_callback
    (set! label-do-callback-handler
          (gen-handler cgc 'do_callback_handler label-do-callback))
    (x86-ret cgc)

    ;; do_callback_fn
    (set! label-do-callback-fn-handler
          (gen-handler cgc 'do_callback_fn_handler label-do-callback-fn))
    (x86-ret cgc)

    ;; do_callback_cont
    (set! label-do-callback-cont-handler
          (gen-handler cgc 'do_callback_cont_handler label-do-callback-cont))
    (x86-ret cgc)

    ;; breakpoint
    (set! label-breakpoint-handler
          (gen-handler cgc 'breakpoint_handler label-breakpoint))
    (x86-ret cgc)

    ;; Runtime error
    (set! label-rt-error-handler
          (gen-handler cgc 'rt_error_handler label-rt-error))
    (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
    (x86-mov cgc (x86-rsp) (x86-mem 0 (x86-rax)))
    (x86-mov cgc (x86-rax) (x86-imm-int -1))
    (pop-regs-reverse cgc all-regs)
    (x86-ret cgc)

    ;; Print msg
    (set! label-print-msg-handler
          (gen-handler cgc 'print_msg_handler label-print-msg))
    (x86-ret cgc)

    ;; Print msg val
    (set! label-print-msg-val-handler
          (gen-handler cgc 'print_msg_val_handler label-print-msg-val))
    (x86-ret cgc)

    ;; -------------------------

    ;; Runtime GC call
    ;(set! label-gc-trampoline (asm-make-label cgc 'gc_trampoline))
    ;(x86-label cgc label-gc-trampoline)
    ;(gen-gc-call cgc)
    ;(x86-ret cgc)

    (x86-label cgc label-rtlib-skip)

    ;; Save all regs to pstack (because the GC must *not* scan these values)
    (push-regs cgc all-regs)

    ;; Put bottom of the pstack (after saving registers) at "block-addr + 0"
    (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
    (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-rsp))

    ;; Set rsp to ustack init
    (x86-mov cgc (x86-rsp) (x86-imm-int ustack-init))

    ;; Init debug slots
    ;(for-each (lambda (s)
    ;            (gen-set-slot cgc (car s) 0))
    ;          debug-slots)

    ;; Put heaplimit in heaplimit slot
    ;; TODO: remove cst slots
    (x86-mov cgc (x86-rcx) (x86-mem (get-heap_limit-addr)))
    (x86-mov cgc (x86-mem (* 8 5) (x86-rax)) (x86-rcx))

    (x86-mov cgc (x86-rcx) (x86-imm-int 0))
    (x86-mov cgc alloc-ptr (x86-mem (get-hp-addr)))       ;; Heap addr in alloc-ptr
    (x86-mov cgc global-ptr (x86-imm-int globals-addr)) ;; Globals addr in r10

    ;; Set all registers used for regalloc to 0
    (for-each (lambda (el)
                (x86-mov cgc el (x86-imm-int 0)))
              regalloc-regs)

    (let ((label (asm-make-label #f (new-sym 'prog_begin))))
      (x86-label cgc label))))

(define (init)

  (init-code-allocator)

  ;;; Create default ports in 'block'
  ;(let ((output-header (mem-header 2 STAG_OPORT))
  ;      (input-header  (mem-header 2 STAG_IPORT)))
  ;  ;; output
  ;  (put-i64 (+ block-addr 8) output-header)
  ;  (put-i64 (+ block-addr 16) 1)
  ;  ;; input
  ;  (put-i64 (+ block-addr 24) input-header)
  ;  (put-i64 (+ block-addr 32) 0))

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
  (list (x86-rdi)  ;; 1st argument
        (x86-rsi)  ;; 2nd argument
        (x86-rdx)  ;; 3rd argument
        (x86-rcx)  ;; 4th argument
        (x86-r8)   ;; 5th argument
        (x86-r9))) ;; 6th argument


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
        (x86-r15)
        (x86-rbp)))

(define nb-c-caller-save-regs
  (length c-caller-save-regs))

(define (reg-sp-offset idx)
  (assert (< idx (length regalloc-regs)) "Internal error")
  (* 8 (- (length regalloc-regs) idx 1)))

(define (reg-sp-offset-r reg)
  (assert (member reg regalloc-regs) "Internal error")
  (* 8 (- (length (member reg regalloc-regs)) 1)))

(define (selector-sp-offset)
  (* 8 (length regalloc-regs)))

;;-----------------------------------------------------------------------------

(define new-sym-counters (make-table))

(define (new-sym sym)
  (let ((n (+ 1 (table-ref new-sym-counters sym 0))))
    (table-set! new-sym-counters sym n)
    (string->symbol (string-append (symbol->string sym) (number->string n)))))

;;-----------------------------------------------------------------------------

;; List of all lazy code objects (for debug/measure purposes)
(define all-lazy-code '())

(define-type lazy-code
  constructor: make-lazy-code*
  generator
  versions
  flags
  ;; rctx is used when register allocation is not used to specialize code
  ;; then a rctx (ctx with only slot-loc, free-regs, and fs) is associated to each lazy-code
  rctx)


(define (lazy-code-nb-versions lazy-code)
  (table-length (lazy-code-versions lazy-code)))

(define (make-lazy-code generator)
  (let ((lc (make-lazy-code* generator (make-table) '() #f)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-cont generator)
  (let ((lc (make-lazy-code* generator (make-table) '(cont) #f)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-entry generator)
  (let ((lc (make-lazy-code* generator (make-table) '(entry) #f)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-ret generator)
  (let ((lc (make-lazy-code* generator (make-table) '(ret) #f)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (get-version lazy-code ctx)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-ref versions ctx #f)))

(define (put-version lazy-code ctx v)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-set! versions ctx v)))

;;-----------------------------------------------------------------------------
;; Ctx TODO regalloc

;; TODO: merge cases EB
(define (apply-moves cgc ctx moves #!optional tmpreg)

  (define (apply-move move)
    (cond ((eq? (car move) (cdr move)) #f)
          ((eq? (car move) 'fs)
           (x86-sub cgc (x86-rsp) (x86-imm-int (* 8 (cdr move)))))
          ((and (ctx-loc-is-register? (car move))
                (ctx-loc-is-memory?   (cdr move)))
           (let ((src (codegen-loc-to-x86opnd (ctx-fs ctx) (car move)))
                 (dst (codegen-loc-to-x86opnd (ctx-fs ctx) (cdr move))))
             (x86-mov cgc dst src)))
          ((and (ctx-loc-is-memory?   (car move))
                (ctx-loc-is-register? (cdr move)))
           (let ((src (codegen-loc-to-x86opnd (ctx-fs ctx) (car move)))
                 (dst (codegen-loc-to-x86opnd (ctx-fs ctx) (cdr move))))
             (x86-mov cgc dst src)))
          ((and (ctx-loc-is-register? (car move))
                (ctx-loc-is-register? (cdr move)))
           (let ((src (codegen-loc-to-x86opnd (ctx-fs ctx) (car move)))
                 (dst (codegen-loc-to-x86opnd (ctx-fs ctx) (cdr move))))
             (x86-mov cgc dst src)))
          ((and (ctx-loc-is-register? (car move))
                (eq? 'rtmp (cdr move)))
           (let ((src (codegen-loc-to-x86opnd (ctx-fs ctx) (car move)))
                 (dst (if tmpreg
                          (codegen-loc-to-x86opnd (ctx-fs ctx) tmpreg)
                          (x86-rax))))
             (x86-mov cgc dst src)))
          ((and (ctx-loc-is-register? (cdr move))
                (eq? 'rtmp (car move)))
           (let ((dst (codegen-loc-to-x86opnd (ctx-fs ctx) (cdr move)))
                 (src (if tmpreg
                          (codegen-loc-to-x86opnd (ctx-fs ctx) tmpreg)
                          (x86-rax))))
             (x86-mov cgc dst src)))
          (else (pp move) (error "NYI apply-moves"))))

  (if (and (= (length moves) 2)       ;; Only two moves
           (equal? (car moves) '(fs . 1)) ;; First is fs = fs + 1
           (equal? (cdadr moves)
                   (cons 'm (- (ctx-fs ctx) 1)))) ;; Second is a move to new cell
      (let ((loc (caadr moves)))
        (x86-push cgc (codegen-loc-to-x86opnd (ctx-fs ctx) loc)))
      (if (not (null? moves))
          (begin (apply-move (car moves))
                 (apply-moves cgc ctx (cdr moves) tmpreg)))))

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
;; JIT

(define (gen-version-* cgc lazy-code ctx label-sym fn-verbose fn-patch fn-codepos)

  (if opt-verbose-jit
      (fn-verbose))

  (let* ((label-dest (get-version lazy-code ctx))
         (new-version? (not label-dest)))
    (if (not label-dest)
        ;; That version is not yet generated, so generate it
        (let ((version-label (asm-make-label #f (new-sym label-sym))))
          (set! code-alloc (fn-codepos))
          (if cgc
              ;; we already have cgc, generate code
              (begin (x86-label cgc version-label)
                     ((lazy-code-generator lazy-code) cgc ctx))
              ;; add code to current code-alloc position
              (code-add
                (lambda (cgc)
                  (asm-align cgc 4 0 #x90)
                  (x86-label cgc version-label)
                  ((lazy-code-generator lazy-code) cgc ctx))))
          (put-version lazy-code ctx version-label)
          (set! label-dest version-label)))
    (fn-patch label-dest new-version?)))

;; #### FIRST LAZY CODE OBJECT
;; This is a special gen-version used to generate the first lco of the program
(define (gen-version-first lazy-code ctx)

  (define (fn-verbose) #f)

  (define (fn-codepos) code-alloc)

  (define (fn-patch label-dest new-version?)
    (asm-label-pos label-dest))

  (gen-version-* #f lazy-code ctx 'version_ fn-verbose fn-patch fn-codepos))

;; #### LAZY CODE OBJECT
;; Generate a lco. Handle fall-through optimization
(define (gen-version jump-addr lazy-code ctx #!optional (cleared-ctx #f))

  (define (fn-verbose)
    (print "GEN VERSION")
    (print " >>> ")
    (pp lazy-code)
    (pp ctx))

  (define fall-through? #f)

  (define (fn-codepos)
    (if (= (+ jump-addr (jump-size jump-addr)) code-alloc)
        (begin ;; (fall-through optimization)
               ;; the jump is the last instruction previously generated, so
               ;; just overwrite the jump
               (set! fall-through? #t)
               (if opt-verbose-jit (println ">>> fall-through-optimization"))
               jump-addr)
        code-alloc))

  (define (fn-patch label-dest new-version?)
    (if (not fall-through?)
        (patch-jump jump-addr (asm-label-pos label-dest)))
    (asm-label-pos label-dest))

  (gen-version-* #f lazy-code ctx 'version_ fn-verbose fn-patch fn-codepos))

;; #### CONTINUATION
;; Generate a continuation
(define (gen-version-continuation load-ret-label lazy-code ctx)

  (define (fn-verbose)
    (print "GEN VERSION CONTINUATION (RP)")
    (print " >>> Patch label ")
    (pp load-ret-label)
    (println " with ctx:")
    (pp ctx))

  (define (fn-patch label-dest new-version?)
    (let ((load-addr (asm-label-pos load-ret-label)))
      (patch-continuation load-addr label-dest)))

  (define (fn-codepos)
    code-alloc)

  (gen-version-* #f lazy-code ctx 'continuation_ fn-verbose fn-patch fn-codepos))

;; #### CONTINUATION CR
;; Generate continuation using cr table (patch cr entry)
(define (gen-version-continuation-cr lazy-code ctx type table)

  (define (fn-verbose)
    (print "GEN VERSION CONTINUATION (CR)")
    (print " >>> Patch table with type ")
    (print type)
    (println " and ctx:")
    (pp ctx))

  (define (fn-patch label-dest new-version?)
    (patch-continuation-cr label-dest type table))

  (define (fn-codepos)
    code-alloc)

  (gen-version-* #f lazy-code ctx 'continuation_ fn-verbose fn-patch fn-codepos))

;; #### FUNCTION ENTRY
;; Generate an entry point
(define (gen-version-fn ast closure lazy-code gen-ctx call-ctx generic global-opt-sym)

  (define ep-loc
    (and global-opt-sym
         (ctime-entries-get global-opt-sym)))

  (define (fn-verbose)
    (print "GEN VERSION FN")
    (print " >>> ")
    (pp gen-ctx)
    (pp call-ctx))

  (define (fn-patch label-dest new-version?)
    (cond ((not opt-entry-points)
           (patch-closure-ep closure label-dest ep-loc)) ;; TODO WIP multiple patch for same closure(?)
          (generic
           (patch-generic ast closure label-dest ep-loc global-opt-sym))
          (else
           (patch-closure closure call-ctx label-dest ep-loc))))

  (define (fn-codepos)
    code-alloc)

  (gen-version-* #f lazy-code gen-ctx 'fn_entry_ fn-verbose fn-patch fn-codepos))


;; #### LAZY CODE OBJECT
;; Generate a normal lazy code object with known cgc and jump to it
(define (jump-to-version cgc lazy-code ctx)

  (define (fn-verbose) #f)

  (define (fn-patch label-dest new-version?)
    (if (not new-version?)
        (x86-jmp cgc label-dest)))

  (define (fn-codepos)
    code-alloc)

  (gen-version-* cgc lazy-code ctx 'version_ fn-verbose fn-patch fn-codepos))

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
(define (patch-generic ast closure label ep-loc global-opt-sym)

  (let ((label-addr (asm-label-pos  label))
        (label-name (asm-label-name label)))

   (if opt-verbose-jit
       (let ((closure-id
               (if ep-loc
                   "#f"
                   (number->string closure 16))))
         (println ">>> patching generic slot of closure " closure-id
                  ": now contains label "
                  label-name
                  " (" (number->string label-addr 16) ")")))

   ;; TODO
   (if global-opt-sym
       (let* ((r (table-ref globals global-opt-sym #f))
              (addr (+ globals-addr (* 8 (cdr r))))
              (curr-closure (get-i64 addr)))
         (set! closure curr-closure)))
   ;;TODO

   (let ((table-addr
           (or ep-loc
               (get-i64 (+ (- (obj-encoding closure) TAG_MEMOBJ) 8)))))
     (put-i64 (+ table-addr 8) label-addr))
   label-addr))

;; Patch closure
;; TODO WIP Move ep-loc arg next to closure
(define (patch-closure closure ctx label ep-loc)

  (let* ((label-addr (asm-label-pos  label))
         (label-name (asm-label-name label))
         (index (or (get-closure-index ctx) -1)) ;; -1 TODO to patch generic
         (offset (+ 16 (* index 8)))) ;; +16 (header & generic)

    ;(if opt-verbose-jit
    ;    (println ">>> patching cctable " (number->string cctable 16) " at "
    ;             (number->string (+ cctable offset) 16)
    ;             " : slot contains now label "
    ;             label-name
    ;             " ("
    ;             (number->string label-addr 16)
    ;             ")"))

    (let ((cctable-addr
            (or ep-loc
                (get-i64 (- (+ (obj-encoding closure) 8) TAG_MEMOBJ))))) ;; +8(header) - 1(tag)
      (put-i64 (+ cctable-addr offset) label-addr))
    label-addr))

;; Patch closure when opt-entry-points is #f (only one ep)
(define (patch-closure-ep closure label ep-loc)

  (define label-addr (asm-label-pos label))

  (define (patch-globalopt loc)
    ;; Patch box which contains entry point
    (put-i64 (+ (- (obj-encoding loc) TAG_MEMOBJ) 8) label-addr))

  (define (patch-noopt)
    ;; TODO: use still box to patch entry point from ast (give by gen-version-fn) ??
    (put-i64 (+ (- (obj-encoding closure) TAG_MEMOBJ) 8) label-addr))

  (println "PATCH EP C")

  (if ep-loc
      (patch-globalopt ep-loc)
      (patch-noopt))
  label-addr)

(define (jump-size jump-addr)
  (if (= (get-u8 jump-addr) #x0f) 6 5))


;; Gen fatal dynamic type test
;; fatal means that if type test fails, it stops execution
;; Check type 'type' for stack slot at 'stack-idx' and jump to 'succ' if succeess
(define (gen-fatal-type-test type stack-idx succ #!optional ast)
 (let ((lazy-error
          (make-lazy-code
             (lambda (cgc ctx)
                (if (or (eq? type CTX_FLO) (eq? type CTX_INT))
                  (gen-error cgc (ERR_TYPE_EXPECTED CTX_INT))
                  (gen-error cgc (ERR_TYPE_EXPECTED type)))))))
   (gen-dyn-type-test type stack-idx succ lazy-error ast)))

;; Create lazy code for type test of stack slot (stack-idx)
;; jump to lazy-success if test succeeds
;; jump to lazy-fail if test fails
(define (gen-dyn-type-test type stack-idx lazy-success lazy-fail #!optional ast)

  (make-lazy-code
     (lambda (cgc ctx)

       ;; TODO: plus nettoyer tout ca
       (let* ((ctx-success (ctx-set-type ctx stack-idx type))
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
                        (stub-first-label-addr #f)
                        (stub-labels
                              (add-callback cgc 1
                                (let ((prev-action #f))

                                  (lambda (ret-addr selector)
                                    (let ((stub-addr stub-first-label-addr)
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

                  (set! stub-first-label-addr
                        (min (asm-label-pos (list-ref stub-labels 0))
                             (asm-label-pos (list-ref stub-labels 1))))

                  (if opt-verbose-jit
                      (println ">>> Gen dynamic type test at index " stack-idx))

                  ;; If 'opt-stats' option, then inc tests slot
                  (if opt-stats
                   (gen-inc-slot cgc 'tests))

                  ;; TODO: utiliser un registre si la variable est mutable
                  (let* ((mutable? (ctx-is-mutable? ctx stack-idx))
                         (lval (ctx-get-loc ctx stack-idx))
                         (opval (codegen-loc-to-x86opnd (ctx-fs ctx) lval)))

                    (if mutable?
                        (begin (x86-push cgc opval)
                               (x86-mov cgc opval (x86-mem (- 8 TAG_MEMOBJ) opval))))

                    (cond ;; Number type check
                          ((eq? type CTX_INT)
                           (x86-mov cgc (x86-rax) (x86-imm-int 3))
                           (x86-and cgc (x86-rax) opval))
                          ;; Null type test
                          ((eq? type CTX_NULL)
                           (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                           (x86-cmp cgc (x86-rax) opval))
                          ;; Pair type test
                          ((eq? type CTX_PAI)
                           (x86-mov cgc (x86-rax) (x86-imm-int 3))
                           (x86-and cgc (x86-rax) opval)
                           (x86-cmp cgc (x86-rax) (x86-imm-int TAG_PAIR)))
                          ;; Char type check
                          ((eq? type CTX_CHAR)
                           ;; char if val is tagged with TAG_SPECIAL and val > 0
                           (x86-mov cgc (x86-rax) opval)
                           (x86-mov cgc selector-reg (x86-imm-int SPECIAL_MASK))
                           (x86-and cgc (x86-rax) selector-reg)
                           (x86-mov cgc selector-reg (x86-imm-int 0))
                           (x86-cmp cgc (x86-rax) (x86-imm-int TAG_SPECIAL)))
                          ;; Procedure type test
                          ((member type (list CTX_FLO CTX_CLO CTX_SYM CTX_VECT CTX_STR CTX_IPORT CTX_OPORT))
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
                                                                          ((eq? type CTX_VECT) STAG_VECTOR))))))
                          ;; Other
                          (else (error "Unknown type " type)))

                    (if mutable?
                        (x86-pop cgc opval))

                    (x86-label cgc label-jump)
                    (x86-je cgc (list-ref stub-labels 0))
                    (x86-jmp cgc (list-ref stub-labels 1))))))))))

;;-----------------------------------------------------------------------------
;; Interprocedural BBV (cr/cc-tables)

;; Current fixed global-cc-table max size
(define global-cc-table-maxsize 500) ;; 500
(define global-cr-table-maxsize (length type-cridx)) ;; TODO number of types
;; Holds the current shape of the global cc table
(define global-cc-table (make-table))

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

(define (global-cc-get-ctx ctx-idx)
  (define (get lst)
    (if (null? lst)
        (error "Internal error")
        (let ((first (car lst)))
          (if (eq? (cdr first) ctx-idx)
              (car first)
              (get (cdr lst))))))
  (get (table->list global-cc-table)))
