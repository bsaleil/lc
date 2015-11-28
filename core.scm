
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

(define mem-header #f)
(define ctx-change-type #f)
(define get-entry-points-loc #f)

;; Forward declarations
(define run-gc #f)         ;; mem.scm
(define expand-tl #f)      ;; expand.scm
(define gen-ast #f)        ;; ast.scm
(define repl-print-lco #f) ;; main.scm

;;-----------------------------------------------------------------------------

;; Global ids
;; Contains a list of global ids with id,position
;; ex. '((foo 1) (bar 2) (fun 3))
(define globals '())
(define gids '()) ;; TODO : merge with 'globals'

(define label-print-msg        #f)
(define label-print-msg-val    #f)
(define label-exec-error       #f)
(define label-gc               #f)
(define label-do-callback      #f)
(define label-do-callback-fn   #f)
(define label-do-callback-cont #f)
(define label-repl #f)

;;-----------------------------------------------------------------------------

;; Tags
(define TAG_NUMBER  0) ;; Must be 0
(define TAG_MEMOBJ  1)
(define TAG_SPECIAL 2)

;; Sub tag
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
(define CTX_ALL   '*) ;; cst reprsenting all ctx types
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

;; Exec errors
(define ERR_MSG             "EXEC ERROR")
(define ERR_ARR_OVERFLOW    "ARITHMETIC OVERFLOW")
(define ERR_WRONG_NUM_ARGS  "WRONG NUMBER OF ARGUMENTS")
(define ERR_TYPE_EXPECTED   (lambda (type)
                              (string-append (string-upcase type)
                                             " EXPECTED")))
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

(define (ERR_UNKNOWN_VAR var)
  (if (string? var)
      (string-append "Can't find variable: " var)
      (string-append "Can't find variable: " (symbol->string var))))

(define ERR_HEAP_NOT_8       "INTERNAL ERROR: heap size should be a multiple of 8")

;;
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

         (closure
          (get-i64 (+ sp (* nb-c-caller-save-regs 8) 8)))

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

  (let* ((str (get-i64 (+ sp (* nb-c-caller-save-regs 8)))) ;; Get str from top of runtime stack
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
      (put-i64 (+ sp (* nb-c-caller-save-regs 8)) qword))))

;; Gen code to call interner-symbol at runtime
(define (gen-interned-symbol cgc)
  (push-pop-regs
     cgc
     c-caller-save-regs ;; preserve regs for C call
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
;(define space-len 4000000000)
(define space-len 7000000000)
(define alloc-ptr (x86-r12))

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

    ;; Put heaplimit in heaplimit slot
    ;; TODO: remove cst slots
    (x86-mov cgc (x86-rcx) (x86-imm-int (- from-space space-len)))
    (x86-mov cgc (x86-mem (* 8 5) (x86-rax)) (x86-rcx))

    (x86-mov cgc (x86-rcx) (x86-imm-int 0))
    (x86-mov cgc alloc-ptr (x86-imm-int from-space))       ;; Heap addr in alloc-ptr
    (x86-mov cgc (x86-r10) (x86-imm-int (+ block-addr (* 8 global-offset)))) ;; Globals addr in r10
    ))

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

;; Identifier in environment associated to an identifier
(define-type identifier
  constructor: make-identifier*
  type   ;; 'free or 'local
  offset ;; offset in closure or stack
  pos    ;; set of offset where the identifier is located
  flags  ;; list of flags (possible flags : mutable)
  stype  ;; ctx type if identifier is a free var
)

(define (make-identifier type offset pos flags stype)
  (make-identifier* type offset (sort pos <) flags stype))

(define (identifier-mutable? id)
  (member 'mutable (identifier-flags id)))

;; Add pos to identifier object
(define (identifier-add-pos identifier pos)
  (if (member pos (identifier-pos identifier))
    identifier
    (make-identifier (identifier-type identifier)
                     (identifier-offset identifier)
                     (cons pos (identifier-pos identifier))
                     (identifier-flags identifier)
                     (identifier-stype identifier))))

;; Remove pos from identifier object
(define (identifier-remove-pos identifier pos)
  (make-identifier (identifier-type    identifier)
                   (identifier-offset  identifier)
                   (set-sub (identifier-pos identifier) (list pos) '())
                   (identifier-flags   identifier)
                   (identifier-stype   identifier)))

;; Reset pos of given identifier
(define (identifier-reset-pos identifier)
  (make-identifier (identifier-type identifier)
                   (identifier-offset identifier)
                   '()
                   (identifier-flags identifier)
                   (identifier-stype identifier)))

;;-----------------------------------------------------------------------------
;; Ctx
;; TODO : fonction pour supprimer des ids de l'environnement pour eviter des list-tails, et mettre dans let, begin et do

(define-type ctx
  stack   ;; compile time stack, containing types
  env     ;; compile time environment
  nb-args ;; nb of arguments of current frame
)

(define (ctx-get-top-pos ctx)
  (- (length (ctx-stack ctx)) 2))

(define (ctx-idx-to-pos ctx idx)
  (- (length (ctx-stack ctx)) idx 2))

(define (ctx-pos-to-idx ctx pos)
  (- (length (ctx-stack ctx)) pos 2))

(define (ctx-change-stack-type ctx pos type)
  (let ((idx (ctx-pos-to-idx ctx pos)))
    (make-ctx (append (list-head (ctx-stack ctx) idx) (list type) (list-tail (ctx-stack ctx) (+ idx 1)))
              (ctx-env ctx)
              (ctx-nb-args ctx))))

(define (ctx-change-stack-types ctx positions type)
  (if (null? positions)
    ctx
    (ctx-change-stack-types (ctx-change-stack-type ctx (car positions) type)
                            (cdr positions)
                            type)))

;; Remove pos from ctx identifiers
(define (ctx-remove-pos ctx pos)
  (make-ctx (ctx-stack ctx)
            (env-remove-pos (ctx-env ctx) pos)
            (ctx-nb-args ctx)))

;; Reset pos in identifier object associated to id 'sym'
(define (ctx-reset-pos ctx sym)
  (make-ctx (ctx-stack ctx)
            (env-reset-pos (ctx-env ctx) sym)
            (ctx-nb-args ctx)))

;; Move nb slot from l-from to l-to (ascending)
(define (ctx-mov-nb ctx nb l-from l-to)
  (if (= nb 0)
     ctx
     (ctx-mov-nb (ctx-move ctx l-from l-to)
                 (- nb 1)
                 (+ l-from 1)
                 (+ l-to 1))))

;; Apply 'nb' push to ctx
(define (ctx-push-nb ctx ctx-type nb)
  (if (= nb 0)
    ctx
    (ctx-push-nb (ctx-push ctx ctx-type)
                 ctx-type
                 (- nb 1))))

;; Apply 'nb' pop to ctx
(define (ctx-pop-nb ctx nb)
  (if (= nb 0)
    ctx
    (ctx-pop-nb (ctx-pop ctx)
                (- nb 1))))

;; Pop ctx
(define (ctx-pop ctx)
  (let* ((pos  (ctx-get-top-pos ctx))    ;; Get pos
         (nctx (ctx-remove-pos ctx pos)) ;; Remove pos from identifiers
         (stack (cdr (ctx-stack nctx)))) ;; Remove first from stack
         ;; Return nex ctx
    (make-ctx stack (ctx-env nctx) (ctx-nb-args nctx))))

;; Push value to ctx
;; Update stack and env if sym
(define (ctx-push ctx ctx-type #!optional (sym #f))
  (let ((stack (cons ctx-type (ctx-stack ctx)))
        (env   (if sym
                 (env-push-id (ctx-env ctx) ctx sym)
                 (ctx-env ctx))))
    (make-ctx stack env (ctx-nb-args ctx))))

;; Remove all ctx information.
;; Return the 'generic' version of this context
(define (ctx-clear ctx)
  (make-ctx ;; Remove stack information
            (make-list (length (ctx-stack ctx)) CTX_UNK)
            ;; Remove env information
            (foldr (lambda (id others)
                     (cons (cons (car id)
                                 (make-identifier (identifier-type (cdr id))
                                                  (identifier-offset (cdr id))
                                                  '()
                                                  (identifier-flags (cdr id))
                                                  CTX_UNK))
                           others))
                   '()
                   (ctx-env ctx))
            (ctx-nb-args ctx)))

;; Change type at 'idx' to 'type'
;; Change types at all other positions of the id(s) at this slot.
(define (ctx-change-type ctx idx type)

  (define (change-for-identifiers env pos ctx)
    (if (null? env)
      ctx
      (let* ((idpair     (car env))
             (identifier (cdr idpair)))
        (if (member pos (identifier-pos identifier))
          (change-for-identifiers (cdr env)
                                  pos
                                  (ctx-change-stack-types ctx (identifier-pos identifier) type))
          (change-for-identifiers (cdr env) pos ctx)))))

  (let* ((pos (ctx-idx-to-pos ctx idx))
         (nctx  (change-for-identifiers (ctx-env ctx) pos ctx))
         (mctx  (ctx-change-stack-type nctx pos type)))
    mctx))

;; Move type from index 'from' to index 'to'
(define (ctx-stack-move stack idx-from idx-to)
   (if (eq? (list-ref stack idx-to) CTX_MOBJ)
       stack
       (append (list-head stack idx-to)
               (cons (list-ref stack idx-from)
                     (list-tail stack (+ idx-to 1))))))

;; Move slot from stack index 'from' to stack index 'to'
;; Optionnally update env
(define (ctx-move ctx idx-from idx-to #!optional (update-env? #t))
   (let ((pos-from (ctx-idx-to-pos ctx idx-from))
         (pos-to   (ctx-idx-to-pos ctx idx-to)))

     (let ((env   (if update-env?
                      (env-move (ctx-env ctx) pos-from pos-to)
                      (ctx-env ctx)))
           (stack (ctx-stack-move (ctx-stack ctx) idx-from idx-to)))

       (make-ctx stack env (ctx-nb-args ctx)))))

;;-------------------------

;; Remove pos from env
(define (env-remove-pos env pos)
  (if (null? env)
    '()
    (let* ((idpair     (car env))
           (idsym      (car idpair))
           (identifier (cdr idpair)))
      (if (member pos (identifier-pos identifier))
        (cons (cons idsym (identifier-remove-pos identifier pos))
              (env-remove-pos (cdr env) pos))
        (cons idpair (env-remove-pos (cdr env) pos))))))

;; Reset pos of given idsym
(define (env-reset-pos env sym)
  (if (null? env)
    '()
    (let* ((idpair     (car env))
           (idsym      (car idpair))
           (identifier (cdr idpair)))
      (if (eq? idsym sym)
         (cons (cons idsym (identifier-reset-pos identifier))
               (cdr env))
         (cons idpair (env-reset-pos (cdr env) sym))))))

;; Mov slot 'from' to 'to' (update identifiers positions)
(define (env-move env pos-from pos-to)
  (if (null? env)
    '()
    (let* ((idpair (car env))
           (idsym  (car idpair))
           (identifier (cdr idpair)))
      (cond
        ;; If id contains 'from', add 'to'
        ((and (member pos-from (identifier-pos identifier))
              (not (member pos-to (identifier-pos identifier))))
           (cons (cons idsym (identifier-add-pos identifier pos-to))
                 (env-move (cdr env) pos-from pos-to)))
        ;; If id contains 'to', remove it
        ((and (member pos-to (identifier-pos identifier))
              (not (member pos-from (identifier-pos identifier))))
           (cons (cons idsym (identifier-remove-pos identifier pos-to))
                 (env-move (cdr env) pos-from pos-to)))
        (else
          (cons idpair (env-move (cdr env) pos-from pos-to)))))))

;; Push identifier on top of the stack (update identifiers positions)
(define (env-push-id env ctx idsym-push)
  (if (null? env)
    '()
    (let* ((idpair (car env))
           (idsym  (car idpair))
           (identifier (cdr idpair)))
      (if (eq? idsym idsym-push)
          (cons (cons idsym (identifier-add-pos identifier (+ (ctx-get-top-pos ctx) 1)))
                (env-push-id (cdr env) ctx idsym-push))
          (cons idpair (env-push-id (cdr env) ctx idsym-push))))))

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
  ;; TODO: print msg if opt-verbose-jit (see gen-version-fn)
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
  ;; TODO: print msg if opt-verbose-jit (see gen-version-fn)

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
              (ctx-success-known ctx);; Is know type is tested type, do not change ctx
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


       (cond ;; Number type test
             ((eq? type CTX_NUM)
                 (x86-mov cgc (x86-rax) (x86-imm-int 3)) ;; rax = 0...011b
                 (x86-and cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp))))
                 ;(x86-cmp cgc (x86-rax) (x86-imm-int TAG_NUMBER)))
             ;; Char type test
             ((eq? type CTX_CHAR)
                 (x86-mov cgc (x86-rax) (x86-imm-int (+ (* -1 (expt 2 63)) TAG_SPECIAL)))
                 (x86-and cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp)))
                 (x86-cmp cgc (x86-rax) (x86-imm-int TAG_SPECIAL)))
                 ;(x86-and cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp)))
                 ;(x86-cmp cgc (x86-rax) (x86-imm-int TAG_SPECIAL)))
             ;; Procedure type test
             ((member type (list CTX_FLO CTX_CLO CTX_PAI CTX_SYM CTX_VECT CTX_STR CTX_IPORT CTX_OPORT))
                 (x86-mov cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp)))
                 (x86-mov cgc (x86-rbx) (x86-rax)) ;; value in rax and rbx
                 (x86-and cgc (x86-rax) (x86-imm-int 3))
                 (x86-cmp cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                 (x86-jne cgc label-jump)
                 (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) (x86-rbx)))
                 (x86-and cgc (x86-rax) (x86-imm-int 248)) ;; 0...011111000 to get type in object header
                 ;; STAG_XXX << 3
                 (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (cond ((eq? type CTX_FLO)  STAG_FLONUM)
                                                                ((eq? type CTX_CLO)  STAG_PROCEDURE)
                                                                ((eq? type CTX_SYM)  STAG_SYMBOL)
                                                                ((eq? type CTX_STR)  STAG_STRING)
                                                                ((eq? type CTX_IPORT) STAG_IPORT)
                                                                ((eq? type CTX_OPORT) STAG_OPORT)
                                                                ((eq? type CTX_VECT) STAG_VECTOR)
                                                                ((eq? type CTX_PAI)  STAG_PAIR))))))
             ;; Other
             (else (error "Unknown type" type)))
       (x86-label cgc label-jump)
       (x86-je cgc (list-ref stub-labels 0))
       (x86-jmp cgc (list-ref stub-labels 1)))))))))

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
