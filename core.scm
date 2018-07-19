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

(include "config.scm")
(include "x86-debug.scm")

;;--------------------------------------------------------------------------------
;; Compiler options

(define opt-static-mode #f)
(define opt-static-pass #f)

(define opt-inlining-limit       #f) ;; Control gambit inlining-limit declaration
(define opt-ctime                #f) ;; Print compilation time
(define opt-stats                #f) ;; Print stats report
(define opt-stats-full           #f) ;; Print full stats report
(define opt-export-locat-info    #f) ;; Pretty print number of versions for each locat object
(define opt-time                 #f) ;; Print exec time in processor cycles
(define opt-verbose-jit          #f) ;; JIT Verbose debugging
(define opt-entry-points         #t) ;; Use multiple entry points (#t to use cc-tables, #f to use flat closures)
(define opt-return-points        #t) ;; Use multiple return points (#t to use cr-tables, #f to use a generic return point)
(define opt-overflow-fallback    #f) ;; Automatic fallback to generic entry point if cctable overflows
(define opt-use-lib              #t) ;; Use scheme std lib (see lib/ folder)
(define opt-vers-regalloc        #t) ;; Use register allocation for code specialization
(define opt-dump-bin             #f) ;; Print generated binary bytes to stdout
(define opt-cc-max               #f) ;; Global cctable max size
(define opt-cr-max               #f) ;; Global crtable max size
(define opt-const-vers           #f) ;; Use cst information in code versioning
(define opt-call-max-len         #f) ;; Max number of args allowed when using a specialized entry point (use a generic ep if nb-args > opt-call-max-len)
(define opt-closest-cx-overflow  #t) ;; Use the closest ctx associated to an existing slot of the cx table when the table oferflows (if possible) instead of using generic ctx
(define opt-lazy-inlined-call    #t) ;; The function body is inlined at a call if (1) the identity of the callee is known, (2) there is no version of the function for this ctx
(define opt-nan-boxing           #f) ;; Use nan boxing istead of tagging to encode values
(define opt-float-unboxing       #t) ;; Use automatic float unboxing based on type specialization
(define opt-disable-pair-tag     #f) ;; Use the generic TAG_MEMOBJ tag for pairs instead of the specific TAG_PAIR tag

(define opt-propagate-continuation #f) ;; TODO
(define opt-regalloc-inlined-call #f)  ;; TODO



;; This is the list of cst types used for versioning (if opt-const-vers is #t)
;; All csts types are enabled by default
(define opt-cv-preds
  (list ctx-type-cha? ctx-type-voi?
        ctx-type-nul? ctx-type-int?
        ctx-type-boo? ctx-type-pai?
        ctx-type-vec? ctx-type-fec?
        ctx-type-str? ctx-type-sym?
        ctx-type-flo? ctx-type-clo?))

;; Is the type a cst used for versioning ?
(define (const-versioned? type)
  (and opt-const-vers
       (ctx-type-cst? type)
       (let loop ((preds opt-cv-preds))
         (if (null? preds)
             #f
             (or ((car preds) type)
                 (loop (cdr preds)))))))

;; Macro to compute compilation time
(define user-compilation-time 0)
(define-macro (run-add-to-ctime f)
  (let ((tmp (gensym)))
    `(if opt-ctime
         (let ((,tmp (##exec-stats ,f)))
           (set! user-compilation-time
                 (+ (- (+ (cdr (assoc 'user-time ,tmp))
                          (cdr (assoc 'sys-time  ,tmp)))
                       (+ (cdr (assoc 'gc-user-time ,tmp))
                          (cdr (assoc 'gc-sys-time ,tmp))))
                    user-compilation-time))
           (cdr (assoc 'result ,tmp)))
         (,f))))

;;-----------------------------------------------------------------------------

;; Forward declarations
(define x86-upush #f)
(define x86-upop  #f)
(define x86-upush-l #f)
(define x86-upop-l  #f)
(define x86-ppush #f)
(define x86-ppop  #f)
(define x86-usp   #f)
(define x86-pcall #f)
(define asc-entry-load-get #f)
(define asc-entry-load-clear #f)
(define asc-globalfn-entry-get #f)
(define global-closures-get #f)
(define gen-closure #f)

(define init-c #f)
(define get___heap_limit-addr  #f)
(define get___alloc_still-addr #f)
(define get-pstate-addr #f)
(define run-gc #f)         ;; mem.scm
(define immediate-to-xmm #f)
(define expand-tl #f)      ;; expand.scm
(define gen-ast #f)        ;; ast.scm
(define alloc-ptr #f)
(define global-ptr #f)
(define entry-points-locs #f)
(define codegen-loc-to-x86opnd #f)
(define codegen-test-fixnum #f)
(define atom-node-make #f)
(define mlc-gambit-call #f)

(define get-heap_limit-addr #f)
(define get-hp-addr #f)
(define global-offset #f)
(define selector-reg #f)
(define x86-call-label-aligned-ret #f)
(define codegen-freg-to-x86reg #f)

(define OFFSET_FLONUM #f)
(define gen-allocation-imm #f)

(define codegen-prologue-rest> #f)
(define gen-drop-float #f)
(define asc-cnnum-table-get #f)
(define make-lco-id #f)
(define is-lco-id? #f)

(define codegen-test-value #f)
(define codegen-test-mem-obj #f)
(define codegen-test-value #f)
(define codegen-test-char #f)

(define write_lc_global #f)
(define write_lc_stack #f)
(define write_lc_stack_ptr #f)
(define write_lc_stack_desc #f)
(define write_lc_stack_usedesc #f)
(define write_desc_intraprocedural #f)
(define write_stubs_limits #f)
(define get_lc_stack_ptr_addr #f)
(define block_gc #f)
(define unblock_gc #f)
(define selector-init-val #f)

;;-----------------------------------------------------------------------------

;; Object life
(define LIFE_MOVE  0)
(define LIFE_STILL 5)
(define LIFE_PERM  6)

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
(define STAG_MOBJECT    5) ;; TODO: rename to STAG_BOX
(define STAG_SYMBOL     8)
(define STAG_PROCEDURE 14)
(define STAG_IPORT     17)
(define STAG_OPORT     18)
(define STAG_STRING    19)
(define STAG_F64VECTOR 29)
(define STAG_FLONUM    30)

;;-----------------------------------------------------------------------------
;; ctx-types jit implementation

;; Generic ctx-type instances
;; Used to avoid type creation for simple operations
;; MUST NOT BE MUTATED
;; NOTE: we need to reorganize 'primitives' data structure in ast.scm
;; to replace ATX_* uses by symbols uses to identify types
(define ATX_ALL 'CTX_SPECIAL_ALL) ; Represents all ctx types
(define ATX_NUM 'CTX_SPECIAL_NUM) ; Represents number types
(define ATX_UNK (make-ctx-tunk))
(define ATX_CHA (make-ctx-tcha))
(define ATX_VOI (make-ctx-tvoi))
(define ATX_NUL (make-ctx-tnul))
(define ATX_RET (make-ctx-tret))
(define ATX_INT (make-ctx-tint))
(define ATX_BOO (make-ctx-tboo))
(define ATX_BOX (make-ctx-tbox))
(define ATX_PAI (make-ctx-tpai))
(define ATX_VEC (make-ctx-tvec))
(define ATX_FEC (make-ctx-tfec))
(define ATX_STR (make-ctx-tstr))
(define ATX_SYM (make-ctx-tsym))
(define ATX_IPO (make-ctx-tipo))
(define ATX_FLO (make-ctx-tflo))
(define ATX_OPO (make-ctx-topo))
(define ATX_CLO (make-ctx-tclo))

(define ATX_TYPE_LIST
    (list ATX_ALL ATX_NUM ATX_UNK ATX_CHA ATX_VOI
          ATX_NUL ATX_RET ATX_INT ATX_BOO ATX_BOX
          ATX_PAI ATX_VEC ATX_FEC ATX_STR ATX_SYM
          ATX_IPO ATX_FLO ATX_OPO ATX_CLO))

(define (ctx-type->stag type)
  (cond ((ctx-type-box? type) STAG_MOBJECT)
        ((ctx-type-pai? type) STAG_PAIR)
        ((ctx-type-vec? type) STAG_VECTOR)
        ((ctx-type-fec? type) STAG_F64VECTOR)
        ((ctx-type-str? type) STAG_STRING)
        ((ctx-type-sym? type) STAG_SYMBOL)
        ((ctx-type-flo? type) STAG_FLONUM)
        ((ctx-type-clo? type) STAG_PROCEDURE)
        (else (pp type) (error "Internal error (ctx-type->stag)"))))

;;-----------------------------------------------------------------------------

;; Errors
(define ERR_MSG              "!!! ERROR - EXEC ERROR")
(define ERR_ARR_OVERFLOW     "!!! ERROR - ARITHMETIC OVERFLOW")
(define ERR_WRONG_NUM_ARGS   "!!! ERROR - WRONG NUMBER OF ARGUMENTS")
(define ERR_OPEN_INPUT_FILE  "!!! ERROR - CAN'T OPEN INPUT FILE")
(define ERR_OPEN_OUTPUT_FILE "!!! ERROR - CAN'T OPEN OUTPUT FILE")
(define ERR_READ_CHAR        "!!! ERROR - CAN'T READ CHAR")
(define ERR_WRITE_CHAR       "!!! ERROR - CAN'T WRITE CHAR")
(define ERR_DIVIDE_ZERO      "!!! ERROR - DIVIDE BY ZERO")
(define ERR_INTERNAL         "!!! ERROR - INTERNAL ERROR")
(define ERR_BEGIN            "!!! ERROR - ILL-FORMED BEGIN")
(define ERR_LET              "!!! ERROR - ILL-FORMED LET")
(define ERR_LET*             "!!! ERROR - ILL-FORMED LET*")
(define ERR_LETREC           "!!! ERROR - ILL-FORMED LETREC")
(define ERR_CHECK            "!!! ERROR - CHECK FAILED")

(define (ERR_TYPE_EXPECTED type)
  (string-append (string-upcase (ctx-type-symbol type))
                 " EXPECTED"))

(define ERR_NUMBER_EXPECTED "NUMBER EXPECTED")

(define (ERR_UNKNOWN_VAR var)
  (if (string? var)
      (string-append "Can't find variable: " var)
      (string-append "Can't find variable: " (symbol->string var))))

;;-----------------------------------------------------------------------------

(define ENCODING_VOID -18) ;; encoded VOID
(define ENCODING_EOF  -14) ;; encoded EOF

;;--------------------------------------------------------------------------------
;; Runtime error

;; Return lazy code object which generates an error with 'msg'
(define (get-lazy-error msg)
  (make-lazy-code
    (make-lco-id 104)
    (lambda (cgc ctx)
      (gen-error cgc msg))))

(define (gen-error cgc err #!optional (stop-exec? #t))
  ;; Put error msg in RAX
  (let ((r1 (car regalloc-regs)))
    (x86-upush cgc r1)
    (x86-mov cgc r1 (x86-imm-int (obj-encoding err 1)))
    (x86-pcall cgc label-rt-error-handler)
    (x86-upop cgc r1)))

;; The procedure exec-error is callable from generated machine code.
;; This function print the error message in rax
(c-define (rt-error usp psp) (long long) void "rt_error" ""
  (block_gc 0)
  (let ((msg (encoding-obj (get-i64 (+ usp (reg-sp-offset-r (x86-rbx)))))))
    (if (not (equal? msg ""))
        (println msg))
    (exit 0)))

;;-----------------------------------------------------------------------------

(c-define (gambit-process-statistics usp psp) (long long) void "gambit_process_statistics" ""
  (block_gc 7)
  (let ((v (##process-statistics)))
    (put-i64 (+ usp 88)
             (##object->encoding v)))
  (unblock_gc))

(c-define (gambit-str-to-sym-tag usp psp) (long long) void "gambit_str_to_sym_tag" ""
  (block_gc 1)
  (let* ((encoding48 (get-u48 (+ usp 88)))
         (str (##encoding->object encoding48))
         (sym (string->symbol str)))
    (put-i64 (+ usp 88)
             (##object->encoding sym)))
  (unblock_gc))

(c-define (gambit-str-to-sym-nan usp psp) (long long) void "gambit_str_to_sym_nan" ""
  (block_gc 2)
  (let* ((encoding48 (get-u48 (+ usp 88)))
         (str (##encoding->object (+ encoding48 TAG_MEMOBJ)))
         (sym (string->symbol str)))
    (put-i64 (+ usp 88)
             (##object->encoding sym)))
  (unblock_gc))

(c-define (gambit-call usp psp) (long long) void "gambit_call" ""
  (block_gc 3)
  (let* ((nargs
           (encoding-obj (get-i64 (+ usp (* (+ (length regalloc-regs) 2) 8)))))
         (op-sym
           (let* ((woffset (+ usp (* (+ (length regalloc-regs) 1) 8)))
                  (getter (lambda () (get-u64 woffset)))
                  (word (get-u64 woffset)))
             (if opt-nan-boxing
                 (nanboxing-encoding-obj word getter)
                 (tagging-encoding-obj word))))
         (args
           (reverse
             (let loop ((n nargs) (offset 3))
               (if (= n 0)
                   '()
                   (let* ((woffset (+ usp (* (+ (length regalloc-regs) offset) 8)))
                          (getter (lambda () (get-u64 woffset)))
                          (word (get-u64 woffset)))
                     (cons (if opt-nan-boxing
                               (nanboxing-encoding-obj word getter)
                               (tagging-encoding-obj word))
                           (loop (- n 1) (+ offset 1))))))))
         (op-fn (eval op-sym)))

    (let ((retval (apply op-fn args)))
      (put-i64 (+ usp (* 8 (+ (length regalloc-regs) 1)))
               (obj-encoding retval 4))))
  (unblock_gc))

;;-----------------------------------------------------------------------------

;; The procedures do-callback* are callable from generated machine code.
;; RCX holds selector (CL)
(c-define (do-callback usp psp) (long long) void "do_callback" ""
  (block_gc 4)
  (write_lc_stack_ptr usp)
  (write_lc_stack_usedesc 3)
  (write_lc_stack_desc (vector-ref (get-scmobj (get-i64 psp)) 1))
  (unblock_gc)
  (let* ((ret-addr (get-i64 psp))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (selector
           (tagging-encoding-obj (get-u48 (- psp 16))))

         (new-ret-addr
          (run-add-to-ctime
            (lambda ()
              (callback-fn ret-addr selector)))))

    ;; replace return address
    (put-i64 psp
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ usp (selector-sp-offset))
             selector-init-val)
    (write_lc_stack_usedesc 0)))

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
(c-define (do-callback-fn usp psp) (long long) void "do_callback_fn" ""
  (block_gc 5)
  (write_lc_stack_ptr usp)
  (write_lc_stack_usedesc 1)
  (if (or (not opt-entry-points)
          (= (tagging-encoding-obj (get-u48 (- psp 16))) 1))
      ;; TODO: move in ctx
      (let* ((nargs (encoding-obj (get-i64 (+ usp (reg-sp-offset-r (x86-rdi))))))
             (fs (+ (max (- nargs nb-args-regs) 0) 1))
             (fo (arithmetic-shift (- (expt 2 fs) 1) 8))
             (desc (+ fs fo)))
        (write_lc_stack_desc desc))
      (let ((cc-idx (encoding-obj (get-i64 (+ usp (reg-sp-offset-r (x86-r11)))))))
        (write_lc_stack_desc (get-cc-gc-map-desc cc-idx))))
  (unblock_gc)
  (let* ((ret-addr (get-i64 psp))

         (selector
          (tagging-encoding-obj (get-u48 (- psp 16))))

         (cc-idx
          (if opt-entry-points
              (encoding-obj (get-i64 (+ usp (reg-sp-offset-r (x86-r11)))))
              #f))

         (cc-idx-data
           (if (or (not opt-entry-points) (= selector 1))
               #f
               (cctable-get-data cc-idx)))

         (stack  (and cc-idx-data (cdr cc-idx-data)))
         (cn-num (and cc-idx-data (car cc-idx-data)))

         ;; Closure is used as a Gambit procedure to keep an updated reference
         (closure
          (and (not opt-entry-points)
               (let ((u48 (get-u48 (+ usp (reg-sp-offset-r (x86-rsi))))))
                 (if opt-nan-boxing
                     (let ((tag (get-msb-u16 (+ usp (reg-sp-offset-r (x86-rsi))))))
                       (if (= tag NB_MASK_MEM_UNSHIFTED)
                           (##encoding->object (+ u48 TAG_MEMOBJ))
                           #f))
                     (if (= (bitwise-and u48 #b11) TAG_MEMOBJ)
                         (##encoding->object u48)
                         #f)))))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (new-ret-addr
           (run-add-to-ctime
             (lambda ()
               (callback-fn stack cc-idx cn-num ret-addr selector closure)))))

    ;; replace return address
    (put-i64 psp new-ret-addr)

    ;; reset selector
    (put-i64 (+ usp (selector-sp-offset))
             selector-init-val)
    (write_lc_stack_usedesc 0)))
    ;(println "exit do_callback_fn")))

;; The procedures do-callback* are callable from generated machine code.
;; RCX holds selector (CL)
(c-define (do-callback-cont usp psp) (long long) void "do_callback_cont" ""
  (block_gc 6)
  (write_lc_stack_ptr usp)
  (if (##bignum? (get-u48 (- psp 16))) (error "N1"))
  (if (= (tagging-encoding-obj (get-u48 (- psp 16))) 1)
      (error "NYI"))
  (if (##bignum? (get-i64 (+ usp (reg-sp-offset-r (x86-r11)))))
      (error "N2"))
  (let ((cr-idx (encoding-obj (get-i64 (+ usp (reg-sp-offset-r (x86-r11)))))))
    (write_lc_stack_ptr usp)
    (write_lc_stack_usedesc 2)
    (write_lc_stack_desc (get-cr-gc-map-desc cr-idx)))
  (unblock_gc)

  (let* ((ret-addr (get-i64 psp))

         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))

         (selector
          (tagging-encoding-obj (get-u48 (- psp 16))))

         (type-idx
          (encoding-obj (get-i64 (+ usp (reg-sp-offset-r (x86-r11))))))

         (type
           (if (or (not opt-return-points) (= selector 1))
               #f
               (crtable-get-data type-idx)))

         (new-ret-addr
           (run-add-to-ctime
             (lambda ()
               (callback-fn ret-addr selector type)))))

    ;; replace return address
    (put-i64 psp
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ usp (selector-sp-offset))
             selector-init-val)
    (write_lc_stack_usedesc 0)))
    ;(println "exit do_callback_cont")))

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
  (set-cdef-label! label-rt-error          'rt_error          "___result = ___CAST(void*,rt_error);")
  (set-cdef-label! label-gambit-call       'gambit_call       "___result = ___CAST(void*,gambit_call);")
  (set-cdef-label! label-gambit-process-statistics 'gambit_process_statistics "___result = ___CAST(void*,gambit_process_statistics);")
  (set-cdef-label! label-gambit-str-to-sym-tag 'gambit_str_to_sym_tag "___result = ___CAST(void*,gambit_str_to_sym_tag);")
  (set-cdef-label! label-gambit-str-to-sym-nan 'gambit_str_to_sym_nan "___result = ___CAST(void*,gambit_str_to_sym_nan);")
  (set-cdef-label! label-do-callback       'do_callback       "___result = ___CAST(void*,do_callback);")
  (set-cdef-label! label-do-callback-fn    'do_callback_fn    "___result = ___CAST(void*,do_callback_fn);")
  (set-cdef-label! label-do-callback-cont  'do_callback_cont  "___result = ___CAST(void*,do_callback_cont);"))

;;-----------------------------------------------------------------------------

;; Machine code block management

;; CODE
(define code-len 6000000)
(define code-addr #f)
(define mcb #f)

;; STUB SPACE
(define ssb-len 6000000)
(define ssb-addr #f)
(define ssb #f)

;; User stack (ustack) is the used by generated machine code
;; This stack is a scheme object. This allows the Gambit GC to scan the stack
;; to find roots.
;; Process stack (pstack) is still used for each call to c code (stubs and others)
(define ustack #f)
(define ustack-init #f) ;; initial sp value (right of the stack)
(define ustack-len 1000000) ;; 1M
(define (init-mcb)
  (set! mcb (make-mcb code-len))
  (set! ssb (make-mcb ssb-len))
  (set! code-addr (##foreign-address mcb))
  (set! ssb-addr (##foreign-address ssb))
  (write_stubs_limits ssb-addr (+ ssb-addr ssb-len))
  (begin (set! ustack (make-u64vector ustack-len #xFFFE000000000000))
         (write_lc_stack (object-address ustack)))
  (set! ustack-init (+ (object-address ustack) 8 (* 8 ustack-len))))

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
(define block-addr #f)
(define stats-slots
  (let loop ((idx 14) (atxs ATX_TYPE_LIST))
    (if (null? atxs)
        '()
        (cons (cons (car atxs) idx)
              (loop (+ idx 1) (cdr atxs))))))
(define debug-slots
  (append '((calls . 6) (tests . 7) (extests . 8) (closures . 9) (time . 10) (other . 11) (flbox . 12) (flunbox . 13))
          stats-slots))
(define block-len (+ 6 (length debug-slots)))

(define (init-block)
  (if opt-nan-boxing
      (begin (set! globals-space (make-u64vector globals-len #xFFFE000000000000))
             (write_lc_global (object-address globals-space)))
      (set! globals-space (alloc-still-vector-i64 globals-len 0)))
  (set! globals-addr (+ (object-address globals-space) 8))
  (set! block (alloc-perm-u64vector block-len))
  (assert (perm-object? block) "Init error, block should be allocated as a permanent object")
  (set! block-addr (+ (object-address block) 8))
  (let loop ((i 0))
    (if (< i (u64vector-length block))
        (begin (put-i64 (+ block-addr (* i 8)) 0)
               (loop (+ i 1))))))

(define (write-mcb code start)
  (let ((len (u8vector-length code)))
    (let loop ((i (fx- len 1)))
      (if (fx>= i 0)
          (begin
            (##machine-code-block-set! mcb (fx+ start i) (u8vector-ref code i))
            (loop (fx- i 1)))
          mcb))))

(define (code-gen arch addr gen #!optional ctx)

  (define (gen-code cgc)
    (let ((code (asm-assemble-to-u8vector cgc)))
      (if opt-verbose-jit
          (begin
            (println "------------------------------------------------------------------------")
            (asm-display-listing cgc (current-output-port) #t)
            (force-output)))
      (write-mcb code (- addr code-addr))
      (u8vector-length code)))

  (let* ((cgc (make-codegen-context))
         (endianness 'le))

    (asm-init-code-block cgc addr endianness)
    (codegen-context-listing-format-set! cgc 'nasm)
    (x86-arch-set! cgc arch)

    ;; If the code-gen function is called to generate function stub, use ctx
    (if ctx
      (gen cgc ctx)
      (gen cgc))

    (if opt-static-mode
        0
        (gen-code cgc))))

;;-----------------------------------------------------------------------------

;; Code and stub management

(define code-alloc #f)
(define stub-alloc #f)
(define stub-freelist #f)

(define (init-code-allocator)
  (init-block)
  (init-mcb)
  (set! code-alloc code-addr)
  (set! stub-alloc (+ ssb-addr ssb-len))
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
                 (x86-add cgc selector-reg (x86-imm-int (tagging-obj-encoding 1))) ;; increment selector
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
  (x86-pcall cgc label-handler)
  (asm-64   cgc (tagging-obj-encoding obj 9)))

(define (stub-reclaim stub-addr)
  (put-i64 stub-addr stub-freelist)
  (set! stub-freelist stub-addr))

;;-----------------------------------------------------------------------------

(define label-heap-limit-handler                #f)
(define label-alloc-still-handler               #f)
(define label-gambit-call-handler               #f)
(define label-gambit-process-statistics-handler #f)
(define label-gambit-str-to-sym-handler         #f)
(define label-do-callback-handler               #f)
(define label-do-callback-fn-handler            #f)
(define label-do-callback-cont-handler          #f)
(define label-rt-error-handler                  #f)
(define label-err-wrong-num-args                #f)

(define (gen-addr-handler cgc id addr cargs-generator)
  (let ((label-handler (asm-make-label cgc id)))

    (x86-label cgc label-handler)

    ;; Save regalloc-regs to ustack because stub could trigger GC,
    ;; then registers are treated as stack roots
    ;; Save selector to allow stub to access its value
    (upush-pop-regs
      cgc
      (cons selector-reg regalloc-regs)
      (lambda (cgc)
        ;; Update gambit heap ptr from LC heap ptr
        (x86-mov cgc (x86-rax) (x86-imm-int (get-hp-addr)))
        (x86-mov cgc (x86-mem 0 (x86-rax)) alloc-ptr)
        ;; Set usp as the first c-arg
        (x86-mov cgc (x86-rdi) (x86-usp))
        ;; Save ustack ptr to pstack
        (x86-ppush cgc (x86-usp))
        ;; Save c caller save registers
        (ppush-pop-regs
          cgc
          (set-sub c-caller-save-regs regalloc-regs)
          (lambda (cgc)
            (ppush-pop-xmm
              cgc
              xmm-regs
              (lambda (cgc)
                (cargs-generator cgc) ;; Gen c args
                ;; Aligned call to addr
                (x86-mov  cgc (x86-rax) (x86-rsp)) ;; align stack-pointer for C call
                (x86-and  cgc (x86-rsp) (x86-imm-int -16))
                (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
                (x86-ppush cgc (x86-rax))
                (x86-mov cgc (x86-rax) (x86-imm-int addr))
                (x86-pcall cgc (x86-rax))
                (x86-ppop cgc (x86-rsp))))))
        ;; Update LC heap ptr and heap limit from Gambit heap ptr and heap limit
        (let ((r1 selector-reg)
              (r2 alloc-ptr))
          ;; heap limit
          (x86-mov cgc r1 (x86-imm-int (get-heap_limit-addr)))
          (x86-mov cgc r1 (x86-mem 0 r1))
          (x86-mov cgc r2 (x86-imm-int block-addr))
          (x86-mov cgc (x86-mem (* 8 5) r2) r1)
          ;; hp
          (x86-mov cgc alloc-ptr (x86-imm-int (get-hp-addr)))
          (x86-mov cgc alloc-ptr (x86-mem 0 alloc-ptr)))
        ;; Set rsp to saved ustack sp
        (x86-ppop cgc (x86-usp))))

    label-handler))

(define (gen-handler cgc id label)
  (let ((label-handler (asm-make-label cgc id)))

    (x86-label cgc label-handler)

    ;; Save regalloc-regs to ustack because stub could trigger GC,
    ;; then registers are treated as stack roots
    ;; Save selector to allow stub to access its value
    (upush-pop-regs
      cgc
      (cons selector-reg regalloc-regs)
      (lambda (cgc)
        ;; Update gambit heap ptr from LC heap ptr
        (x86-mov cgc (x86-rax) (x86-imm-int (get-hp-addr)))
        (x86-mov cgc (x86-mem 0 (x86-rax)) alloc-ptr)
        ;; Set usp & psp as the first & second c-args
        (x86-mov cgc (x86-rdi) (x86-usp))       ;; NOTE: will not be correctly restored if rdi is not a reg-alloc reg
        (x86-mov cgc (x86-rsi) (x86-rsp)) ;; NOTE: will not be correctly restored if rsi is not a reg-alloc reg
        ;; Save ustack ptr to pstack
        (x86-ppush cgc (x86-usp))
        ;; Save c caller save registers
        (ppush-pop-regs
          cgc
          (set-sub c-caller-save-regs regalloc-regs)
          (lambda (cgc)
            (ppush-pop-xmm
              cgc
              xmm-regs
              (lambda (cgc)
                ;; Aligned call to label
                (x86-mov  cgc (x86-rax) (x86-rsp)) ;; align stack-pointer for C call
                (x86-and  cgc (x86-rsp) (x86-imm-int -16))
                (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
                (x86-ppush cgc (x86-rax))
                (x86-pcall cgc label)
                (x86-ppop cgc (x86-rsp))))))
        ;; Update LC heap ptr and heap limit from Gambit heap ptr and heap limit
        (let ((r1 selector-reg)
              (r2 alloc-ptr))
          ;; heap limit
          (x86-mov cgc r1 (x86-imm-int (get-heap_limit-addr)))
          (x86-mov cgc r1 (x86-mem 0 r1))
          (x86-mov cgc r2 (x86-imm-int block-addr))
          (x86-mov cgc (x86-mem (* 8 5) r2) r1)
          ;; hp
          (x86-mov cgc alloc-ptr (x86-imm-int (get-hp-addr)))
          (x86-mov cgc alloc-ptr (x86-mem 0 alloc-ptr)))
        ;; Set rsp to saved ustack sp
        (x86-ppop cgc (x86-usp))))

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

              (let* (;; NOTE: Must match the registers saved using ppush-pop-regs in gen-addr-handler
                     (saved-offset (+ (length xmm-regs) (length (set-sub c-caller-save-regs regalloc-regs))))
                     (stag-offset  (* 8 (+ saved-offset 2))))

                ;; stag is not encoded, then it is in pstack
                ;;alloc_still_handler
                (let ((addr (get_lc_stack_ptr_addr)))
                  (x86-mov cgc (x86-rax) (x86-imm-int addr))
                  (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-usp)))
                (x86-mov cgc (x86-rdi) (x86-mem stag-offset (x86-rsp)))
                (x86-mov cgc (x86-rsi) (x86-mem (* 8 (+ (length regalloc-regs) 2)) (x86-rbp)))))))
                ;(x86-mov cgc (x86-rdi) (x86-imm-int (get-pstate-addr)))))))
    (x86-ret cgc)

    (set! label-gambit-call-handler
          (gen-handler cgc 'gambit_call_handler label-gambit-call))
    (x86-ret cgc)

    (set! label-gambit-process-statistics-handler
          (gen-handler cgc 'gambit_process_statistics label-gambit-process-statistics))
    (x86-ret cgc)

    (set! label-gambit-str-to-sym-handler
          (if opt-nan-boxing
              (gen-handler cgc 'gambit_str_to_sym_handler label-gambit-str-to-sym-nan)
              (gen-handler cgc 'gambit_str_to_sym_handler label-gambit-str-to-sym-tag)))
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

    ;; Runtime error
    (set! label-rt-error-handler
          (gen-handler cgc 'rt_error_handler label-rt-error))
    (x86-ret cgc)

    (set! label-err-wrong-num-args (asm-make-label #f 'err_wrong_num_args))
    (x86-label cgc label-err-wrong-num-args)
    (gen-error cgc ERR_WRONG_NUM_ARGS)

    ;; -------------------------

    (x86-label cgc label-rtlib-skip)

    ;; Save all regs to pstack (because the GC must *not* scan these values)
    (ppush-xmm cgc)
    (ppush-regs cgc all-regs)

    ;; Set usp to ustack init
    (x86-mov cgc (x86-usp) (x86-imm-int ustack-init))

    ;; Put heaplimit in heaplimit slot
    ;; TODO: remove cst slots
    (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
    (x86-mov cgc (x86-rcx) (x86-imm-int (get-heap_limit-addr)))
    (x86-mov cgc (x86-rcx) (x86-mem 0 (x86-rcx)))
    (x86-mov cgc (x86-mem (* 8 5) (x86-rax)) (x86-rcx))

    (x86-mov cgc (x86-rcx) (x86-imm-int selector-init-val))
    ;; Heap addr in alloc-ptr
    (x86-mov cgc alloc-ptr (x86-imm-int (get-hp-addr)))
    (x86-mov cgc alloc-ptr (x86-mem 0 alloc-ptr))
    (x86-mov cgc global-ptr (x86-imm-int globals-addr)) ;; Globals addr in r10

    ;; Set all registers used for regalloc to 0
    (for-each (lambda (el)
                (x86-mov cgc el (x86-imm-int (obj-encoding 0))))
              regalloc-regs)

    (let ((label (asm-make-label #f (new-sym 'prog_begin))))
      (x86-label cgc label))))

(define (init-backend)

  (if opt-disable-pair-tag
      (set! TAG_PAIR TAG_MEMOBJ))

  (write_desc_intraprocedural (not opt-return-points))

  (init-c)
  (init-code-allocator)
  (init-interprocedural)
  (init-values)

  (code-add
   (lambda (cgc)
     (init-labels cgc)
     (init-rtlib cgc))))

;;-----------------------------------------------------------------------------

(define (upush-pop-regs cgc regs proc)
  (x86-upush-l cgc regs)
  (proc cgc)
  (x86-upop-l cgc (reverse regs)))

(define (ppush-pop-regs cgc regs proc)
  (for-each (lambda (reg) (x86-ppush cgc reg)) regs)
  (proc cgc)
  (for-each (lambda (reg) (x86-ppop cgc reg)) (reverse regs)))

(define (ppush-pop-xmm cgc xmm-regs proc)
  (x86-sub cgc (x86-rsp) (x86-imm-int (* 8 (length xmm-regs))))
  (let loop ((n 0) (xmms xmm-regs))
    (if (not (null? xmms))
        (begin
          (x86-movsd cgc (x86-mem (* 8 n) (x86-rsp)) (car xmms))
          (loop (+ n 1) (cdr xmms)))))
  (proc cgc)
  (let loop ((n 0) (xmms xmm-regs))
    (if (not (null? xmms))
        (begin
          (x86-movsd cgc (car xmms) (x86-mem (* 8 n) (x86-rsp)))
          (loop (+ n 1) (cdr xmms)))))
  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (length xmm-regs)))))

(define (ppush-xmm cgc)
  (x86-sub cgc (x86-rsp) (x86-imm-int (* 8 (length xmm-regs))))
  (let loop ((n 0) (xmms xmm-regs))
    (if (not (null? xmms))
        (begin
          (x86-movsd cgc (x86-mem (* 8 n) (x86-rsp)) (car xmms))
          (loop (+ n 1) (cdr xmms))))))

(define (ppop-xmm cgc)
  (let loop ((n 0) (xmms xmm-regs))
    (if (not (null? xmms))
        (begin
          (x86-movsd cgc (car xmms) (x86-mem (* 8 n) (x86-rsp)))
          (loop (+ n 1) (cdr xmms)))))
  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (length xmm-regs)))))

(define (upush-regs cgc regs)
  (for-each (lambda (reg) (x86-upush cgc reg)) regs))

(define (ppush-regs cgc regs)
  (for-each (lambda (reg) (x86-ppush cgc reg)) regs))

(define (upop-regs-reverse cgc regs)
  (for-each (lambda (reg) (x86-upop cgc reg)) (reverse regs)))

(define (ppop-regs-reverse cgc regs)
  (for-each (lambda (reg) (x86-ppop cgc reg)) (reverse regs)))

(define c-caller-save-regs ;; from System V ABI
  (list (x86-rdi)  ;; 1st argument
        (x86-rsi)  ;; 2nd argument
        (x86-rdx)  ;; 3rd argument
        (x86-rcx)  ;; 4th argument
        (x86-r8)   ;; 5th argument
        (x86-r9)   ;; 6th argument
        (x86-r10)
        (x86-r11)))

(define all-regs
  (list (x86-rax)
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

(define n-regalloc-regs 10)
(define regalloc-regs
  (list (x86-rbx)
        (x86-r15)
        (x86-rsi)
        (x86-rdi)
        (x86-r10)
        (x86-r11)
        (x86-r12)
        (x86-r13)
        (x86-r14)
        (x86-rdx)))

(define regalloc-fregs
  (list                         (x86-xmm2)  (x86-xmm3)  (x86-xmm4)
        (x86-xmm5)  (x86-xmm6)  (x86-xmm7)  (x86-xmm8)  (x86-xmm9)
        (x86-xmm10) (x86-xmm11) (x86-xmm12) (x86-xmm13) (x86-xmm14)
        (x86-xmm15)))

(define xmm-regs (cons (x86-xmm0)
                       (cons (x86-xmm1)
                             regalloc-fregs)))

(define nb-c-caller-save-regs
  (length c-caller-save-regs))

(define (reg-sp-offset idx)
  (assert (< idx (length regalloc-regs)) "Internal error (reg-sp-offset)")
  (* 8 (- (length regalloc-regs) idx 1)))

(define (reg-sp-offset-r reg)
  (assert (member reg regalloc-regs) "Internal error (reg-sp-offset-r)")
  (* 8 (- (length (member reg regalloc-regs)) 1)))

(define (selector-sp-offset)
  (* 8 (length regalloc-regs)))

;;-----------------------------------------------------------------------------

(define new-sym-counters (make-table))

(define (new-sym sym)
  (let ((n (+ 1 (table-ref new-sym-counters sym 0))))
    (table-set! new-sym-counters sym n)
    (string->symbol (string-append (symbol->string sym) (number->string n)))))

(define fn-count 0)
(define cn-count 0)

(define-macro (new-cx-num cpt)
  `(let ((n ,cpt))
     (set! ,cpt (+ ,cpt 1))
     n))

(define (new-fn-num) (new-cx-num fn-count))
(define (new-cn-num) (new-cx-num cn-count))

;;-----------------------------------------------------------------------------

;; List of all lazy code objects (for debug/measure purposes)
(define all-lazy-code '())

;; 'versions' is a table associating a version (label) to a ctx
;; This version could be a 'real' version, which is a full version specialized for this ctx
;; or a 'not real' version, which is a version containing only merge code and a jump to the generic version
(define-type lazy-code
  constructor: make-lazy-code*
  (ast lc-get-ast lc-set-ast)
  generator
  ;; 'versions' is a table which associates a version (a label) to a context
  ;; if the lco has the flag 'entry (i.e. it's a prologue lco), the table
  ;; associates a version (a label) to a *stack* instead.
  ;versions
  flags     ;; entry, return, continuation, rest
  lco-true  ;; lco of true branch if it's a cond lco
  lco-false ;; lco of false branch if it's a cond lco)
  )

(define (lazy-code-ast lco #!optional (with-id #f))
  (let ((ast (lc-get-ast lco)))
    (if (and (not with-id) (is-lco-id? ast))
        #f
        ast)))

(define (lazy-code-ast-set! lco)
  (error "Internal error"))

(define (lazy-code-f-*! flag)
  (lambda (lazy-code)
    (let ((flags (lazy-code-flags lazy-code)))
      (if (not (member flag flags))
          (lazy-code-flags-set! lazy-code (cons flag flags))))))

(define lazy-code-f-cont!  (lazy-code-f-*! 'cont))
(define lazy-code-f-entry! (lazy-code-f-*! 'entry))
(define lazy-code-f-rest!  (lazy-code-f-*! 'rest))

(define (lazy-code-entry? lazy-code)
  (member 'entry (lazy-code-flags lazy-code)))

(define (lazy-code-rest? lazy-code)
  (and (member 'entry  (lazy-code-flags lazy-code))
       (member 'rest   (lazy-code-flags lazy-code))))

(define (make-lazy-code ast generator)
  (let ((lc (make-lazy-code* ast generator '() #f #f)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-entry ast rest? generator)
  (let* ((flags (if rest? '(rest entry) '(entry)))
         (lc (make-lazy-code* ast generator flags #f #f)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-ret ast generator)
  (let ((lc (make-lazy-code* ast generator '(ret) #f #f)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

(define (make-lazy-code-cond ast lco-true lco-false generator)
  (let ((lc (make-lazy-code* ast generator '(cond) lco-true lco-false)))
    (set! all-lazy-code (cons lc all-lazy-code))
    lc))

;;-----------------------------------------------------------------------------
;; ctx regalloc

;; Apply list of moves
;; Possible moves are (src . dst):
;; (loc1  . loc2)  -> move loc1 to loc2
;; ('rtmp . loc)  -> move temporary register to loc (temporary is tmpreg or rax)
;; (loc   . 'rtmp)  -> move loc to temporary register (temporary is tmpreg or rax)
;; (fs    . n)       -> add n words to sp
(define (apply-moves cgc ctx moves #!optional tmpreg)

  ;; Filter moves
  ;; Return a pair with fs to add in first and moves except fs moves
  ;; ex. (4 . ((r0 . r1) (r2 . r3)))
  (define (filter-fs moves fs ffs filtered)
    (if (null? moves)
        (cons (cons fs ffs) (reverse filtered))
        (let ((move (car moves)))
          (cond
            ((eq? (car move) 'fs)  (filter-fs (cdr moves) (+ fs (cdr move)) ffs filtered))
            ((eq? (car move) 'ffs) (filter-fs (cdr moves) fs (+ ffs (cdr move)) filtered))
            (else (filter-fs (cdr moves) fs ffs (cons move filtered)))))))

  ;; Return tmp register to use for 'rtmp operand
  ;; Use given temporary register if given, rax otherwise
  (define (get-tmp)
    (if tmpreg
        (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) tmpreg)
        (x86-rax)))

  ;; Is selector used to apply moves? If it is used, reset it after all moves
  (define selector-used? #f)

  ;; Select a register to be used as tmp register for mov instruction
  ;; (mov mem mem, etc...)
  (define (select-mov-tmp-reg next-moves)
    (define (tmp-used-after moves)
      (cond ((null? moves) #f)
            ((eq? (caar moves) 'rtmp) #t) ;; rtmp is first used as a src, then #t
            ((eq? (cdar moves) 'rtmp) #f) ;; rtmp is first used as a dst, then #f
            (else (tmp-used-after (cdr moves)))))
    (if (and (eq? (get-tmp) (x86-rax))
             (tmp-used-after next-moves))
        (begin
          (set! selector-used? #t)
          selector-reg)
        (x86-rax)))

  ;; Apply move. A move is one of possible moves except fs move
  ;;
  (define (apply-filtered moves fs-offset ffs-offset)
    (if (not (null? moves))
        (let* ((move (car moves))
               ;; Compute x86 dst operand
               (dst
                 (cond ((not (cdr move)) #f)
                       ((eq? (cdr move) 'rtmp)
                          ;; Check that if move is fr -> rtmp
                          ;; then rtmp must be rax or selector to avoid memory corruption
                          (assert (if (ctx-loc-is-fregister? (car move))
                                      (let ((tmp (get-tmp)))
                                        (or (eq? tmp (x86-rax))
                                            (eq? tmp selector-reg)))
                                      #t)
                                  "Internal error")
                          (get-tmp))
                       (else (codegen-loc-to-x86opnd (- (ctx-fs ctx) fs-offset) (- (ctx-ffs ctx) ffs-offset) (cdr move)))))
               ;; Compute x86 src operand
               (src
                 (cond ((and (pair? (car move))
                             (eq? (caar move) 'flbox))
                          ;; Check move validity
                          (assert (or (and (eq? (cadar move) 'const) (flonum? (cddar move)))
                                      (ctx-loc-is-fregister? (cdar move)))
                                  "Internal error, unexpected operand in move")
                          (if (eq? (cadar move) 'const)
                              (begin
                                (assert (eq? (mem-allocated-kind (cddar move)) 'PERM) "Internal error")
                                (x86-imm-int (obj-encoding (cddar move) 11)))
                              (let* ((loc (cdar move))
                                     (opnd (codegen-loc-to-x86opnd (- (ctx-fs ctx) fs-offset) (- (ctx-ffs ctx) ffs-offset) loc)))
                                (if opt-nan-boxing
                                    ;; NaN Boxing, boxing fl does not require mem allocation
                                    opnd
                                    ;; Tagging, boxing fl requires mem allocation
                                    (begin
                                      (gen-allocation-imm cgc STAG_FLONUM 8 (ctx->gc-map-desc ctx))
                                      (if (x86-mem? opnd)
                                          (begin
                                            (x86-mov cgc (x86-rax) opnd)
                                            (x86-mov cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) (x86-rax)))
                                          (x86-movsd cgc (x86-mem (+ -16 OFFSET_FLONUM) alloc-ptr) opnd))
                                      (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))
                                      (x86-rax))))))
                       ((and (pair? (car move))
                             (or (eq? (caar move) 'constfn)
                                 (eq? (caar move) 'constcont)))
                          ;; If src is a constfn, create closure in move
                          (car move))
                       ((and (pair? (car move))
                             (eq? (caar move) 'const))
                          (cond ((and (flonum? (cdar move)) (not opt-nan-boxing))
                                   (x86-imm-int (get-i64 (+ (- OFFSET_FLONUM TAG_MEMOBJ) (obj-encoding (cdar move) 12)))))
                                (else
                                   (x86-imm-int (obj-encoding (cdar move) 13)))))
                       ((eq? (car move) 'rtmp)
                          (get-tmp))
                       (else
                          (codegen-loc-to-x86opnd (- (ctx-fs ctx) fs-offset) (- (ctx-ffs ctx) ffs-offset) (car move))))))

          (cond ;; dst is #f, src needs to be a cst (cst -> cst)
                ((not dst)
                   ;; TODO: assert src is a cst.
                   ;; However, it's possible that we move loc -> cst. for example if we merge two ctx, dst ctx has a cst and src ctx lost informtion
                   ;; TODO wip: then, check that this case is correctly handled in steps functions, this kind of move should not be here
                   #f)
                ;; Same operands, useless move
                ((eq? src dst) #f)
                ;; Need to create an empty closure
                ((and (pair? src) (eq? (car src) 'constfn))
                   (let ((entry-obj (car (asc-globalfn-entry-get (cdr src)))))
                     (if (ctx-loc-is-register? (cdr move))
                         (gen-closure cgc (cdr move) #f entry-obj '())
                         (let ((r (select-mov-tmp-reg (cdr moves))))
                           ;; select-mov-tmp-reg should return loc AND x86 opnd, then loc is used in gen-closure
                           (assert (eq? r (x86-rax)) "Internal error, nyi")
                           (gen-closure cgc 'tmp #f entry-obj '())
                           (x86-mov cgc dst r)))))
                ;; TODO optimize move if dst is a register
                ((and (pair? src) (eq? (car src) 'constcont))
                  (x86-label cgc (asm-make-label #f (new-sym 'SYM_OPT_)))
                  (let ((table (asc-cnnum-table-get (cdr src))))
                    (error "NYI nan boxing")
                    (x86-mov cgc (x86-rax) (x86-imm-int (- (obj-encoding table 14) TAG_MEMOBJ)))
                    (x86-mov cgc dst (x86-rax))))
                ;; Both in memory, use rax
                ((and (or (x86-mem? src)
                          (x86-imm? src))
                      (x86-mem? dst))
                   (let ((r (select-mov-tmp-reg (cdr moves))))
                     (x86-mov cgc r src)
                     (x86-mov cgc dst r)))
                ;; Dest is xmm
                ((and (x86-reg? dst)
                      (x86-xmm? dst))
                   (cond ;; imm -> xmm
                         ((x86-imm? src)
                            (immediate-to-xmm cgc dst (cdar move)))
                         ;; mem -> xmm
                         ((x86-mem? src)
                            (x86-movsd cgc dst src))
                         ;; xmm -> xmm
                         ((x86-xmm? src)
                            (x86-movsd cgc dst src))
                         ;; reg -> xmm
                         ((x86-reg? src)
                            (x86-movd/movq cgc dst src))
                         ;;
                         (else (error "NYI apply-moves"))))
                ((and (x86-reg? src)
                      (x86-xmm? src))
                   ;; NOTE: dst can't be xmm
                   (if (x86-reg? dst)
                       (x86-movd/movq cgc dst src)
                       (x86-movsd cgc dst src)))
                ;; direct x86 mov is possible
                (else
                   (x86-mov cgc dst src)))
          ;;
          (apply-filtered (cdr moves) fs-offset ffs-offset))))

  (let ((r (filter-fs moves 0 0 '())))
    ;; Update sp
    (if (> (caar r) 0)
        (x86-sub cgc (x86-usp) (x86-imm-int (* 8 (caar r)))))
    (if (> (cdar r) 0)
        (x86-sub cgc (x86-rsp) (x86-imm-int (* 8 (cdar r)))))
    ;; Apply other moves
    (apply-filtered (cdr r) (if (< (caar r) 0) (caar r) 0) (if (< (cdar r) 0) (cdar r) 0))
    (if (< (caar r) 0)
        (x86-add cgc (x86-usp) (x86-imm-int (* -8 (caar r)))))
    (if (< (cdar r) 0)
        (x86-add cgc (x86-rsp) (x86-imm-int (* -8 (cdar r)))))
    (if selector-used?
        (x86-mov cgc selector-reg (x86-imm-int selector-init-val)))))


;;-----------------------------------------------------------------------------

;; Add callback
(define (add-callback cgc max-selector gc-desc callback-fn)
  (create-stub label-do-callback-handler max-selector callback-fn gc-desc))

;; Add function callback
(define (add-fn-callback max-selector fn-num callback-fn)
  (create-stub label-do-callback-fn-handler max-selector callback-fn fn-num))

;; Add continuation callback
(define (add-cont-callback cgc max-selector callback-fn)
  (create-stub label-do-callback-cont-handler max-selector callback-fn))

;;-----------------------------------------------------------------------------
;; JIT

(define (gen-version-* cgc lazy-code ctx label-sym fn-verbose fn-patch fn-codepos fn-opt-label #!optional (fn-block-prefix #f))

  (define (generate-moves ctx moves label-dest)
    (assert (not (null? moves)) "Internal error")
    (let ((label (asm-make-label #f (new-sym 'prologue_merge_))))
      (set! code-alloc (fn-codepos))
      (if cgc
          ;;
          (begin (x86-label cgc label)
                 (apply-moves cgc ctx moves)
                 (if label-dest
                     (x86-jmp cgc label-dest)))
          ;;
          (code-add
            (lambda (cgc)
              (asm-align cgc 4 0 #x90)
              (x86-label cgc label)
              (apply-moves cgc ctx moves)
              (if label-dest
                  (x86-jmp cgc label-dest)))))
      ;;
      label))

  (define (generate-merge-code src-ctx dst-ctx label-dest)
    (let ((moves (ctx-regalloc-merge-moves src-ctx dst-ctx)))
      (if (null? moves)
          ;; No merge code is generated, return label-dest
          label-dest
          ;; Genereate moves
          (generate-moves dst-ctx moves label-dest))))

  ;; Todo: merge with generate-new-version
  (define (generate-generic ctx label-merge callback)
    (let ((version-label (asm-make-label #f (new-sym label-sym))))
      (callback (or label-merge version-label) version-label)
      (apply-types-lost)
      ;; NOTE: generate-generic is NEVER called without a previous call to generate-merge-code
      ;;       generate-merge-code calls fn-codepos
      ;(set! code-alloc (fn-codepos))
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
      version-label))

  (define (generate-new-version ctx strat-callback)
    (let ((version-label (asm-make-label #f (new-sym label-sym))))
      (strat-callback version-label)
      (set! code-alloc (fn-codepos))
      (if cgc
          ;; we already have cgc, generate code
          (begin (and fn-block-prefix (fn-block-prefix cgc))
                 (x86-label cgc version-label)
                 ((lazy-code-generator lazy-code) cgc ctx))
          ;; add code to current code-alloc position
          (code-add
            (lambda (cgc)
              (asm-align cgc 4 0 #x90)
              (and fn-block-prefix (fn-block-prefix cgc))
              (x86-label cgc version-label)
              ((lazy-code-generator lazy-code) cgc ctx))))

      (if fn-opt-label
          (fn-opt-label version-label)
          version-label)))

  (if opt-verbose-jit
      (fn-verbose))
  (set! ctx (ctx-free-dead-locs ctx (lazy-code-ast lazy-code)))

  (let* ((r (strat-get-version lazy-code ctx))
         (version  (car r))
         (vctx     (cadr r))
         (callback (caddr r)))
  (cond
    ;; A version exists for this exact context
    ((and version (eq? vctx ctx))
       (callback version)
       (fn-patch version #f))
    ;; A version exists but another context is used
    (version
       (if (or (not opt-entry-points) (not opt-return-points))
           (error "NYI case")) ;; gc desc needs to be added to the version header
       (let ((label-merge (generate-merge-code ctx vctx version)))
         (callback label-merge)
         (fn-patch label-merge (not (eq? label-merge version)))))
    ;; No version, but we can generate one for this exact context
    ((eq? vctx ctx)
       (let ((label (generate-new-version ctx callback)))
         (fn-patch label #t)))
    ;; No version, and we need to generate one for another context
    (else
       (if (or (not opt-entry-points) (not opt-return-points))
           (error "NYI case")) ;; gc desc needs to be added to the version header
       (let* ((label-merge   (generate-merge-code ctx vctx #f))
              (label-version (generate-generic vctx label-merge callback))
              (label-first   (or label-merge label-version)))
       (fn-patch label-first #t))))))

;; #### FIRST LAZY CODE OBJECT
;; This is a special gen-version used to generate the first lco of the program
(define (gen-version-first lazy-code ctx)

  (define (fn-verbose) #f)

  (define (fn-codepos) code-alloc)

  (define (fn-patch label-dest new-version?) #f)

  (gen-version-* #f lazy-code ctx 'version_ fn-verbose fn-patch fn-codepos #f))

;; #### LAZY CODE OBJECT
;; Generate a lco. Handle fall-through optimization
(define (gen-version jump-addr lazy-code ctx)

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

  (gen-version-* #f lazy-code ctx 'version_ fn-verbose fn-patch fn-codepos #f))

;; #### CONTINUATION
;; Generate a continuation
(define (gen-version-continuation load-ret-label lazy-code ctx #!optional (gc-desc #f))

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

  (define (fn-block-prefix cgc)
    (if gc-desc
        (asm-64 cgc gc-desc)))

  (gen-version-* #f lazy-code ctx 'continuation_ fn-verbose fn-patch fn-codepos #f fn-block-prefix))

;; #### CONTINUATION CR
;; Generate continuation using cr table (patch cr entry)
(define (gen-version-continuation-cr lazy-code ctx type table generic?)

  (define (fn-verbose)
    (print "GEN VERSION CONTINUATION (CR)")
    (print " >>> Patch table with type ")
    (print type)
    (println " and ctx:")
    (pp ctx))

  (define (fn-patch label-dest new-version?)
    (patch-continuation-cr label-dest type table generic?))

  (define (fn-codepos)
    code-alloc)

  (gen-version-* #f lazy-code ctx 'continuation_ fn-verbose fn-patch fn-codepos #f))

;; #### FUNCTION ENTRY
;; Generate an entry point
(define (gen-version-fn ast closure entry-obj lazy-code gen-ctx cc-idx generic)

  (define (fn-verbose)
    (print "GEN VERSION FN")
    (pp lazy-code)
    (print " >>> ")
    (pp gen-ctx))

  (define patch-label #f)

  (define (tmp-fn-patch label-dest new-version?)
    (set! patch-label label-dest)
    (asm-label-pos label-dest))

  (define (fn-patch label-dest new-version?)

    (cond ((not opt-entry-points)
           (patch-closure-ep ast closure entry-obj label-dest))
          (generic
           (patch-generic ast entry-obj #f label-dest))
          (else
           (patch-closure entry-obj cc-idx label-dest))))

  (define (fn-codepos)
    code-alloc)

  (define (fn-opt-label version-label)
    (define entry-pos (asm-label-pos version-label))
    ;; Ignore all nop instructions (byte 0x90)
    (let loop ((i 0) (opcode (get-u8 entry-pos)))
      (if (not (= opcode #x90))
          (set! entry-pos (+ entry-pos i))
          (loop (+ i 1) (get-u8 (+ entry-pos (+ i 1))))))
    ;;
    (let* ((opcode (get-u8 entry-pos)))
      (cond ;; First instruction is a jmp rel8
            ((= opcode #xeb)
               ;; Get destination from jump instruction
               (let ((jmpdest (+ entry-pos (get-i8 (+ entry-pos 1)) 2)))
                 ;; The new version label is a new label built from jmp destination address
                 (asm-make-label #f (new-sym 'fn_entry_) jmpdest)))
            ;; First instruction is a jmp rel32
            ((= opcode #xe9)
               (let ((jmpdest (+ entry-pos (get-i32 (+ entry-pos 1)) 5)))
                 (asm-make-label #f (new-sym 'fn_entry_) jmpdest)))
            (else
               version-label))))

  (define block-label (asm-make-label #f (new-sym 'rest_block_)))
  (define to-version-label (asm-make-label #f (new-sym 'block_to_version)))

  (let* ((rest-param (lazy-code-rest? lazy-code))
         (ctx gen-ctx)
         (nb-actual (ctx-nb-actual ctx))
         (nb-formal (ctx-nb-args ctx)))

    (cond ;; rest AND actual == formal
          ((and opt-entry-points (not generic) rest-param (= nb-actual (- nb-formal 1))) ;; -1 rest
             (set! ctx (ctx-push ctx (make-ctx-tnulc '()) #f))
             (set! ctx (ctx-reset-nb-actual ctx))
             (assert (null? (ctx-free-mems ctx)) "Internal error")
             (gen-version-* #f lazy-code ctx 'fn_entry_ fn-verbose fn-patch fn-codepos fn-opt-label))
          ;; rest AND actual > formal
          ((and opt-entry-points (not generic) rest-param (> nb-actual (- nb-formal 1)))
             (code-add
               (lambda (cgc)
                 (asm-align cgc 4 0 #x90)
                 (x86-label cgc block-label)
                 (set! ctx (gen-drop-float cgc ctx ast 0 (- nb-actual (- nb-formal 1))))
                 (let* ((nb-extra (- nb-actual (- nb-formal 1)))
                        (rloc
                          (let ((r (ctx-get-loc ctx (- nb-extra 1))))
                            (or r ;; loc exists
                                ;; Else, get a free register
                                (let* ((r (ctx-get-free-reg #f ctx #f 0))
                                       (moves (car r))
                                       (reg (cadr r))
                                       (nctx (caddr r)))
                                  (apply-moves cgc nctx moves)
                                  (set! ctx nctx)
                                  reg))))
                        (locs
                          (let loop ((idx (- nb-extra 1))
                                     (locs '()))
                            (if (< idx 0)
                                locs
                                (let ((loc (ctx-get-loc ctx idx)))
                                  (if loc
                                      (loop (- idx 1) (cons loc locs))
                                      (let* ((type (ctx-get-type ctx idx))
                                             (cst (ctx-type-cst type)))
                                        (if (ctx-type-clo? type)
                                            (loop (- idx 1) (cons (cons 'cf cst) locs))
                                            (loop (- idx 1) (cons (cons 'c cst) locs))))))))))
                   (let* ((nctx (ctx-pop-n ctx (- nb-extra 1)))
                          (nctx (ctx-set-type nctx 0 (make-ctx-tpai) #f))
                          (nctx (ctx-set-loc nctx (stack-idx-to-slot nctx 0) rloc)))
                     (codegen-prologue-rest> cgc (ctx->gc-map-desc ctx) (ctx-fs ctx) (ctx-ffs ctx) locs rloc)
                     (let ((nfree-mems (length (ctx-free-mems nctx))))
                       (if (> nfree-mems 0)
                           (begin
                             (x86-add cgc (x86-usp) (x86-imm-int (* 8 (length (ctx-free-mems nctx)))))
                             (set! nctx (ctx-remove-free-mems nctx)))))
                     (x86-label cgc to-version-label)
                     (x86-nop cgc) (x86-nop cgc) (x86-nop cgc) (x86-nop cgc)
                     (x86-nop cgc) (x86-nop cgc) (x86-nop cgc) (x86-nop cgc)
                     (set! ctx nctx)))))
             (set! ctx (ctx-reset-nb-actual ctx))
             (let ((version-addr
                     (gen-version-* #f lazy-code ctx 'fn_entry_ fn-verbose tmp-fn-patch fn-codepos fn-opt-label)))
               (let ((tmp code-alloc))
                 (set! code-alloc (asm-label-pos to-version-label))
                 (code-add
                   (lambda (cgc)
                     (let ((sym (string->symbol (string-append (number->string version-addr 16) "_"))))
                       (x86-jmp cgc (asm-make-label #f (new-sym sym) version-addr)))))
                 (set! code-alloc tmp))
               (fn-patch block-label #t)
               (asm-label-pos block-label)))
          ;;
          (else
            (set! ctx (ctx-reset-nb-actual ctx))
            (gen-version-* #f lazy-code ctx 'fn_entry_ fn-verbose fn-patch fn-codepos fn-opt-label)))))

;; #### LAZY CODE OBJECT
;; Generate a normal lazy code object with known cgc and jump to it
(define (jump-to-version cgc lazy-code ctx)

  (define (fn-verbose) #f)

  (define (fn-patch label-dest new-version?)
    (if (not new-version?)
        (x86-jmp cgc label-dest)))

  (define (fn-codepos)
    code-alloc)

  (gen-version-* cgc lazy-code ctx 'version_ fn-verbose fn-patch fn-codepos #f))

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
(define (patch-continuation-cr continuation-label type crtable generic?)
  ;; TODO: msg if opt-verbose-jit (see patch-continuation)
  (define label-pos (asm-label-pos continuation-label))
  (if generic?
      (##u64vector-set! crtable 1 label-pos)
      (##u64vector-set! crtable (+ 2 (crtable-get-idx type)) label-pos))
  label-pos)

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
(define (patch-generic ast cctable stack label)

  (let ((label-addr (asm-label-pos  label))
        (label-name (asm-label-name label)))

   (##u64vector-set! cctable 0 label-addr) ;; Patch generic slot
   label-addr))

;; Patch all call sites previously generated by the compiler
;; using a direct jump to this stub
(define (patch-direct-jmp-labels entry-obj cc-idx eplabel)
  (let ((patched-labels (asc-entry-load-get entry-obj cc-idx))
        (tmp code-alloc))
    ;; Clear table entry
    (asc-entry-load-clear entry-obj cc-idx)
    ;; Patch all labels
    (let loop ((labels patched-labels))
      (if (not (null? labels))
          (let ((plabel (car labels)))
            (if opt-verbose-jit
                (begin
                  (println "Patch direct stub jump at " (number->string (asm-label-pos plabel) 16))
                  (println "  > now jump to " (number->string (asm-label-pos eplabel) 16))))
            (set! code-alloc (asm-label-pos plabel))
            (code-add (lambda (cgc) (x86-jmp cgc eplabel)))
            (loop (cdr labels)))))
    ;; Restore code-alloc
    (set! code-alloc tmp)))

;; Patch closure
(define (patch-closure cctable cc-idx label)

  (assert (integer? cc-idx) "Internal error")

  (let* ((label-addr (asm-label-pos  label))
         (label-name (asm-label-name label)))

    ;; Patch cctable entry (+ 1 for generic ep)
    (##u64vector-set! cctable (+ cc-idx 1) label-addr)

    ;;
    (patch-direct-jmp-labels cctable cc-idx label)

    label-addr))

;; Patch closure when opt-entry-points is #f (only one ep)
(define (patch-closure-ep ast closure entryvec label)

  (define label-addr (asm-label-pos label))

  ;; Patch entry vector
  (vector-set! entryvec 0 (quotient label-addr 4))

  ;; Patch current closure
  (if closure
      ;; if known closure, patch it
      (put-i64 (+ (- (tagging-obj-encoding closure 15) TAG_MEMOBJ) 8) label-addr))

  ;;
  (patch-direct-jmp-labels entryvec -1 label)

  label-addr)

(define (jump-size jump-addr)
  (if (= (get-u8 jump-addr) #x0f) 6 5))

;; Gen fatal dynamic type test
;; fatal means that if type test fails, it stops execution
;; Check type 'type' for stack slot at 'stack-idx' and jump to 'succ' if succeess
(define (gen-fatal-type-test ctx-type stack-idx succ #!optional ast)
 (let ((lazy-error
          (make-lazy-code
             (make-lco-id 101)
             (lambda (cgc ctx)
               (if (not opt-static-mode)
                   (begin
                     (println ERR_CHECK)
                     (pp ast)))
               (if (or (ctx-type-flo? ctx-type) (ctx-type-int? ctx-type))
                   (gen-error cgc ERR_NUMBER_EXPECTED)
                   (gen-error cgc (ERR_TYPE_EXPECTED ctx-type)))))))

                   (gen-dyn-type-test ctx-type stack-idx succ lazy-error ast)))
;; Create lazy code for type test of stack slot (stack-idx)
;; jump to lazy-success if test succeeds
;; jump to lazy-fail if test fails
(define (gen-dyn-type-test ctx-type stack-idx lazy-success lazy-fail #!optional ast)

  (define lazy-funbox
    (make-lazy-code
      (make-lco-id 105)
      (lambda (cgc ctx)
        ;; alloc register
        (let* ((r (ctx-get-free-freg ast ctx lazy-success 0))
               (moves (car r))
               (freg  (cadr r))
               (ctx   (caddr r))
               (dest (codegen-freg-to-x86reg freg))
               (loc  (ctx-get-loc ctx stack-idx))
               (opnd (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) loc)))
          ;; apply moves
          (apply-moves cgc ctx moves)
          ;; unbox value
          (if opt-nan-boxing
              ;; NaN-boxing lazy unboxing
              (if (x86-mem? opnd)
                  (x86-movsd cgc dest opnd)
                  (x86-movd/movq cgc dest opnd))
              ;; Tagging lazy unboxing
              (begin
                (if (x86-mem? opnd)
                    (begin (x86-mov cgc (x86-rax) opnd)
                           (set! opnd (x86-rax))))
                (if opt-stats
                    (gen-inc-slot cgc 'flunbox))
                (x86-movsd cgc dest (x86-mem (- OFFSET_FLONUM TAG_MEMOBJ) opnd))))
          ;; update ctx
          (let* ((type-ctor (ctx-type-ctor ctx-type))
                 (ctx
                   (let* ((ident (ctx-ident-at ctx stack-idx))
                                 (tctx (ctx-set-type ctx stack-idx (type-ctor) #t)))
                          (if ident
                              (foldr (lambda (slot ctx) (ctx-set-loc ctx slot freg))
                                     tctx
                                     (identifier-sslots (cdr ident)))
                              (ctx-set-loc tctx (stack-idx-to-slot tctx stack-idx) freg)))))
            (jump-to-version cgc lazy-success ctx))))))

  (make-lazy-code
     (make-lco-id 102)
     (lambda (cgc ctx)

       (define type-ctor (ctx-type-ctor ctx-type))

       (let* ((ctx-success-known ctx);; If know type is tested type, do not change ctx (TODO?)
              (ctx-fail ctx)
              (known-type  (ctx-get-type ctx stack-idx)))

         (cond ;; known == expected
               ((ctx-type-is-a? known-type ctx-type)
                (jump-to-version cgc lazy-success ctx-success-known))
               ;; known != expected && known != unknown
               ((not (ctx-type-unk? known-type))
                (jump-to-version cgc lazy-fail ctx-fail))
               ;; known == unknown
               (opt-static-mode
                (error "NYI")
                (jump-to-version cgc lazy-fail ctx-fail)
                (jump-to-version cgc lazy-success ctx-success-known))
               (else
                 (let* (;;
                        (lazy-success
                          (if (and (ctx-type-flo? ctx-type)
                                   opt-float-unboxing)
                              lazy-funbox
                              lazy-success))
                        (ctx-success
                          (if (and (ctx-type-flo? ctx-type)
                                   opt-float-unboxing)
                              ctx
                              (ctx-set-type ctx stack-idx (type-ctor) #t)))
                        (x86-op x86-je)
                        (label-jump (asm-make-label cgc (new-sym 'patchable_jump)))
                        (stub-first-label-addr #f)
                        (stub-labels
                              (add-callback cgc 1 (ctx->gc-map-desc ctx)
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

                  (let* ((lval (ctx-get-loc ctx stack-idx))
                         (opval (codegen-loc-to-x86opnd (ctx-fs ctx) (ctx-ffs ctx) lval)))

                    (cond ;; Number type check
                          ((ctx-type-int? ctx-type)
                           (codegen-test-fixnum cgc opval))
                          ;; Null type test
                          ((ctx-type-nul? ctx-type)
                           (codegen-test-value cgc opval '()))
                          ;; Pair type test
                          ((and (ctx-type-pai? ctx-type) (not opt-nan-boxing) (not opt-disable-pair-tag))
                           (x86-mov cgc (x86-rax) (x86-imm-int 3))
                           (x86-and cgc (x86-rax) opval)
                           (x86-cmp cgc (x86-rax) (x86-imm-int TAG_PAIR)))
                          ;; Char type check
                          ((ctx-type-cha? ctx-type)
                           (codegen-test-char cgc opval))
                          ;; Float nan boxing (NOTE: move to codegen-test-mem-obj)
                          ((and (ctx-type-flo? ctx-type) opt-nan-boxing)
                           (x86-mov cgc (x86-rax) opval)
                           (x86-shr cgc (x86-rax) (x86-imm-int 48))
                           (x86-cmp cgc (x86-rax) (x86-imm-int NB_MASK_FLO_MAX_UNSHIFTED))
                           (set! x86-op x86-jle))
                          ;; Procedure type test
                          ((ctx-type-mem-allocated? ctx-type)
                            (codegen-test-mem-obj cgc ast opval lval ctx-type label-jump))
                          ;; Other
                          (else (error "Unknown type " ctx-type)))

                    (x86-label cgc label-jump)
                    (x86-op cgc  (list-ref stub-labels 0))
                    (x86-jmp cgc (list-ref stub-labels 1))))))))))

;; TODO WIP
(define (gen-overflow-test lazy2 lazy1)

  (make-lazy-code
     (make-lco-id 103)
     (lambda (cgc ctx)

       (if opt-static-mode
           ;; static mode
           (begin (jump-to-version cgc lazy1 ctx)
                  (jump-to-version cgc lazy2 ctx))
           ;; dynamic mode
           (let* ((label-jump (asm-make-label cgc (new-sym 'patchable_jump)))
                  (stub-first-label-addr #f)
                  (stub-labels
                        (add-callback cgc 1 (ctx->gc-map-desc ctx)
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
                                              (gen-version (+ jump-addr 6) lazy1 ctx)
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
                                                        lazy2
                                                        ctx))
                                                ;; make conditional jump to new version
                                                (gen-version jump-addr lazy2 ctx))))
                                    (begin ;; one branch has already been patched
                                           ;; reclaim the stub
                                           (release-still-vector (get-scmobj ret-addr))
                                           (stub-reclaim stub-addr)
                                           (if (= selector 0)
                                              (gen-version (if (eq? prev-action 'swap) (+ jump-addr 6) jump-addr) lazy2 ctx)
                                              (gen-version (if (eq? prev-action 'swap) jump-addr (+ jump-addr 6)) lazy1 ctx))))))))))
            (set! stub-first-label-addr
                  (min (asm-label-pos (list-ref stub-labels 0))
                       (asm-label-pos (list-ref stub-labels 1))))

            (x86-label cgc label-jump)
            (x86-jo cgc  (list-ref stub-labels 0))
            (x86-jmp cgc (list-ref stub-labels 1)))))))

;;-----------------------------------------------------------------------------
;; Interprocedural BBV (cr/cc-tables)

;; Current fixed global-cc-table max size
(define global-cc-table-maxsize  #f)
(define global-cr-table-maxsize  #f)
;; Holds the current shape of the global cc table
(define global-cc-table (make-table test: equal?))
(define global-cr-table (make-table))

(define global-cc-gc-map-desc #f)
(define global-cr-gc-map-desc #f)

;; Get cc/cr table index associated to ctx. If ctx is not in the
;; global cc/cr table, then this function adds it and returns
;; the new associated index. Index starts from 0.
;; Store and compare ctx-stack in enough because environment is
;; the same for all versions of a lazy-object.
(define (cxtable-get-idx global-table name global-table-maxsize)
  (lambda (data)
    (let ((res (table-ref global-table data #f)))
      (if res
        ;; Ctx exists in global table
        res
        ;; Ctx does not exists yet
        (if (= (table-length global-table) global-table-maxsize)
          ;; Global table is full
          (if opt-overflow-fallback
              #f
              (error (string-append "Global " name " table overflow!")))
          ;; Global table is not full
          (let ((value (table-length global-table)))
            (table-set! global-table data value)
            value))))))

(define cctable-get-idx #f)
(define crtable-get-idx #f)

(define (cxtable-get-data global-table)
  (lambda (idx)
    (let loop ((lst (table->list global-table)))
      (if (null? lst)
          (error "Internal error")
          (let ((first (car lst)))
            (if (eq? (cdr first) idx)
                (car first)
                (loop (cdr lst))))))))

(define cctable-get-data (cxtable-get-data global-cc-table))
(define crtable-get-data (cxtable-get-data global-cr-table))

(define (set-cx-gc-map-desc table idx desc)
  (put-i64 (+ (object-address table) (* 8 (+ idx 1))) desc))
(define (get-cx-gc-map-desc table idx)
  (get-u64 (+ (object-address table) (* 8 (+ idx 1)))))

(define (set-cc-gc-map-desc idx desc) (set-cx-gc-map-desc global-cc-gc-map-desc idx desc))
(define (set-cr-gc-map-desc idx desc) (set-cx-gc-map-desc global-cr-gc-map-desc idx desc))
(define (get-cc-gc-map-desc idx)      (get-cx-gc-map-desc global-cc-gc-map-desc idx))
(define (get-cr-gc-map-desc idx)      (get-cx-gc-map-desc global-cr-gc-map-desc idx))

(define (init-interprocedural)
  (set! global-cc-table-maxsize (or opt-cc-max 610))
  (set! global-cr-table-maxsize (or opt-cr-max 200))
  (set! global-cc-gc-map-desc (alloc-perm-u64vector (or opt-cc-max 610)))
  (set! global-cr-gc-map-desc (alloc-perm-u64vector (or opt-cr-max 200)))
  (set! cctable-get-idx (cxtable-get-idx global-cc-table "cc" global-cc-table-maxsize))
  (set! crtable-get-idx (cxtable-get-idx global-cr-table "cr" global-cr-table-maxsize)))
