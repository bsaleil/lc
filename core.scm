
(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm")
(include "~~lib/_codegen#.scm")

;;-----------------------------------------------------------------------------

(include "x86-debug.scm")

;;-----------------------------------------------------------------------------

;; Compiler options
(define verbose-jit          #f) ;; JIT Verbose debugging
(define verbose-gc           #f) ;; GC  Verbose debugging
(define count-calls          #f) ;; Count call for a given identifier
(define print-ccsize         #f) ;; Print size of global cc table after exec
(define all-tests            #f) ;; Remove type information (execute all type tests)
(define count-tests          #f) ;; Count type tests and print number
(define count-closures       #f) ;; Count created closures and print number
(define print-versions       #f) ;; Print number min and max of versions of lazy codes
(define print-versions-full  #f) ;; Print number detailed number of versions

;;-----------------------------------------------------------------------------

(define mem-header #f)
(define run-gc #f) ;; Forward declaration (see mem.scm)
(define ctx-change-type #f)

;;-----------------------------------------------------------------------------

;; Global ids
;; Contains a list of global ids with id,position
;; ex. '((foo 1) (bar 2) (fun 3))
(define globals '())
(define gids '()) ;; TODO : merge with 'globals'

(define fake-stack (list 'FAKE))

(define label-print-msg      #f)
(define label-print-msg-val  #f)
(define label-exec-error     #f)
(define label-gc             #f)
(define label-do-callback    #f)
(define label-do-callback-fn #f)

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

;; TODO : merge with 'globals'
(define gret `(
   (length . ,CTX_NUM)
   (exact? . ,CTX_BOOL)
   (list?  . ,CTX_BOOL)
   (port?  . ,CTX_BOOL)
   (boolean? . ,CTX_BOOL)
   (max    . ,CTX_NUM)
   (min    . ,CTX_NUM)
   (equal? . ,CTX_BOOL)
   (list->vector . ,CTX_VECT)
   (list->string . ,CTX_STR)
   (number->string . ,CTX_STR)
   (string->number . ,CTX_NUM)
   (string-append . ,CTX_STR)
   (string-copy . ,CTX_STR)
   (string=? . ,CTX_BOOL)
   (string<? . ,CTX_BOOL)
   (string . ,CTX_STR)
   (substring . ,CTX_STR)
   (vector . ,CTX_VECT)
   (char<? . ,CTX_BOOL)
   (char>? . ,CTX_BOOL)
   (char<=? . ,CTX_BOOL)
   (char>=? . ,CTX_BOOL)
   (char-alphabetic? . ,CTX_BOOL)
   (char-numeric? . ,CTX_BOOL)
   (char-whitespace? . ,CTX_BOOL)
   (char-upper-case? . ,CTX_BOOL)
   (char-lower-case? . ,CTX_BOOL)
   (char-upcase . ,CTX_CHAR)
   (char-downcase . ,CTX_CHAR)
   (char-ci=? . ,CTX_BOOL)
   (char-ci<? . ,CTX_BOOL)
   (char-ci>? . ,CTX_BOOL)
   (char-ci<=? . ,CTX_BOOL)
   (char-ci>=? . ,CTX_BOOL)
   (exact? . ,CTX_BOOL)
   (abs . ,CTX_NUM)
   (/ . CTX_NUM)
   (zero? . CTX_BOOL)
   (positive? . CTX_BOOL)
   (negative? . CTX_BOOL)
   (even? . CTX_BOOL)
   (odd? . CTX_BOOL)
   (expt . CTX_NUM)
))

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

(define ERR_BEGIN            "ILL-FORMED BEGIN")
(define ERR_LET              "ILL-FORMED LET")
(define ERR_LET*             "ILL-FORMED LET*")
(define ERR_LETREC           "ILL-FORMED LETREC")

;;
(define ENCODING_VOID -18) ;; encoded VOID
(define ENCODING_EOF  -14) ;; encoded EOF
(define NENCODING_EOF  -4) ;; non encoded EOF

;;-----------------------------------------------------------------------------

(define (alloc-still-vector len)
  ((c-lambda (int)
             scheme-object
             "___result = ___EXT(___make_vector) (___PSTATE, ___arg1, ___FAL);")
   len))

(define (release-still-vector vect)
  ((c-lambda (scheme-object)
             void
             "___EXT(___release_scmobj) (___arg1);")
   vect))

(define (get-i64 addr)
  ((c-lambda (int64) long "___result = *___CAST(___S64*,___arg1);")
   addr))

(define (put-i64 addr val)
  ((c-lambda (int64 int64) void "*___CAST(___S64*,___arg1) = ___arg2;")
   addr
   val))

(define (get-i32 addr)
  ((c-lambda (int64) long "___result = *___CAST(___S32*,___arg1);")
   addr))

(define (put-i32 addr val)
  ((c-lambda (int64 int32) void "*___CAST(___S32*,___arg1) = ___arg2;")
   addr
   val))

(define (get-u8 addr)
  ((c-lambda (int64) long "___result = *___CAST(___U8*,___arg1);")
   addr))

(define (put-u8 addr val)
  ((c-lambda (int64 unsigned-int8) void "*___CAST(___U8*,___arg1) = ___arg2;")
   addr
   val))

(define (get-scmobj addr)
  ((c-lambda (int64) scheme-object "___result = *___CAST(___SCMOBJ*,___arg1);")
   addr))

(define (put-scmobj addr val)
  ((c-lambda (int64 scheme-object) void "*___CAST(___SCMOBJ*,___arg1) = ___arg2;")
   addr
   val))

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

;; Gen code for error
;; stop-exec? to #f to continue after error
(define (gen-error cgc err #!optional (stop-exec? #t))

  ;; Put error msg in RAX
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding err)))

  ;; If no msg print nothing
  (if (not (string=? err ""))
    ;; Call exec-error
    (push-pop-regs
       cgc
       c-caller-save-regs ;; preserve regs for C call
       (lambda (cgc)
         (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
         (x86-and  cgc (x86-rsp) (x86-imm-int -16))
         (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
         (x86-push cgc (x86-rdi))
         (x86-call cgc label-exec-error) ;; call C function
         (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
         )))

  (if stop-exec?
    (begin
      (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
      (x86-mov cgc (x86-rsp) (x86-mem 0 (x86-rax)))
      (x86-mov cgc (x86-rax) (x86-imm-int -1))
      (pop-regs-reverse cgc prog-regs)
      (x86-ret cgc)
      (x86-ret cgc)))
)

;; The procedure exec-error is callable from generated machine code.
;; This function print the error message in rax
(c-define (exec-error sp) (long) void "exec_error" ""
  (let* ((err-enc (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rax-pos) 1) 8))))
        (err (encoding-obj err-enc)))
    (print "!!! ERROR : ")
    (println err)))

;;-----------------------------------------------------------------------------
;; Measurement

(define (gen-print-slot cgc slot msg)
  (x86-push cgc (x86-rax))
  (gen-get-slot cgc slot (x86-rax))
  (gen-print-reg cgc msg (x86-rax))
  (x86-pop cgc (x86-rax)))

(define (gen-print-msg cgc msg)
  
  (x86-push cgc (x86-rax))
  (x86-push cgc (x86-rcx))
  
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding msg)))
  
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
  (let* ((msg-enc (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rax-pos) 1) 8))))
         (msg     (encoding-obj msg-enc)))
    
    (println msg)))

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
         
         ;; Get ctx from still-box address
         (ctx
           (let ((rctx (still-ref->ctx still-encoding)))
             (if (equal? (ctx-stack rctx) fake-stack)
               ;; If ctx contains fake stack, it's a call from apply, then read nb-args and create new ctx
               (let ((nb-args (get-i64 (+ sp (* (- (- nb-c-caller-save-regs rdi-pos) 1) 8)))))
                 (make-ctx (cons CTX_CLO (append (make-list nb-args CTX_UNK) (list CTX_RETAD))) (ctx-env rctx) (ctx-nb-args rctx)))
               rctx)))
         
         (closure
          (get-i64 (+ sp (* nb-c-caller-save-regs 8) 8)))
                  
         (callback-fn
          (vector-ref (get-scmobj ret-addr) 0))
                 
         (new-ret-addr
          (callback-fn sp ctx ret-addr 0 closure)))
                
    ;; replace return address
    (put-i64 (+ sp (* nb-c-caller-save-regs 8))
             new-ret-addr)

    ;; reset selector
    (put-i64 (+ sp (* (- (- nb-c-caller-save-regs rcx-pos) 1) 8))
             0)))

;;-----------------------------------------------------------------------------
;; Interned symbols
;; TODO

(define label-interned-symbol #f)
(define sym-space-len 100000)
(define sym-space (##make-machine-code-block sym-space-len))
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
;(define heap-len 400000000)
(define heap-len  500000000)
(define heap-addr  #f)
(define from-space #f)
(define to-space   #f)
(define space-len (quotient heap-len 2))
(define alloc-ptr (x86-r12))

;; CODE
(define code-len 1200000000)
(define code-addr #f)

;; MCB :
;; 0                 code-len              mcb-len
;; +--------------------+--------------------+
;; |    Code            |    Heap            |
;; |                    |                    |
;; |                    |                    |
;; +--------------------+--------------------+

(define mcb #f)
(define mcb-len (+ heap-len code-len))
(define mcb-addr #f)

(define (init-mcb)
  (set! mcb (##make-machine-code-block mcb-len))
  (set! mcb-addr (##foreign-address mcb))
  (set! code-addr mcb-addr)
  (set! heap-addr (+ mcb-addr code-len))
  (set! from-space heap-addr)
  (set! to-space (+ from-space space-len)))

;; BLOCK :
;; 0          8                       (nb-globals * 8 + 8)
;; +----------+----------+----------+----------+
;; | Bottom   | Global 1 |    ...   | Global n |
;; | stack    |          |          |          |
;; | addr     |          |          |          |
;; +----------+----------+----------+----------+

(define block #f)
(define global-offset 10) ;; [Stack addr], [def-out-port-header|def-out-port-fd], [def-in-port-header|def-in-port-fd] + n empty slot (used for debug)
(define block-len (* 8 (+ global-offset 10000))) ;; 1 stack addr, 1000 globals
(define block-addr #f)
(define debug-slots '((calls . 5) (tests . 6) (extests . 7) (closures . 8) (other . 9)))

(define (init-block)
  (set! block (##make-machine-code-block block-len))
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
      (if verbose-jit
          (begin
            (println "------------------------------------------------------------------------")
            (asm-display-listing cgc (current-output-port) #t)))
      (write-mcb code (- addr mcb-addr))
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
  (set! stub-alloc (+ mcb-addr mcb-len))
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
    (if verbose-jit
        (pp (list 'obj= obj)))
    stub-labels))

(define (call-handler cgc label-handler obj)
  (x86-call cgc label-handler)
  (asm-64   cgc (obj-encoding obj)))

(define (stub-reclaim stub-addr)
  (put-i64 stub-addr stub-freelist)
  (set! stub-freelist stub-addr))

(define (obj-encoding obj)
  (let ((n (##object->encoding obj)))
    (if (>= n (expt 2 63)) (- n (expt 2 64)) n)))

(define (encoding-obj encoding)
  (##encoding->object encoding))

;;-----------------------------------------------------------------------------

(define label-do-callback-handler #f)
(define label-do-callback-fn-handler #f)

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
    
    (x86-label cgc label-rtlib-skip)
    
    (push-regs cgc prog-regs)
    
    ;; Init debug slots
    (for-each (lambda (s)
                (gen-set-slot cgc (car s) 0))
              debug-slots)
    
    ;; Put bottom of the stack (after saving registers) at "block-addr + 0"
    (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
    (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-rsp))
    
    (x86-mov cgc (x86-rcx) (x86-imm-int 0))
    (x86-mov cgc alloc-ptr  (x86-imm-int heap-addr))       ;; Heap addr in alloc-ptr
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
        (x86-rax) ;; return register
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

(define prog-regs ;; Registers at entry and exit points of program
  (list (x86-rcx)
        (x86-rbx)
        (x86-rdx)
        alloc-ptr
        (x86-r10) ;; Globals
        (x86-r15)
        ))

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

(define (make-lazy-code generator)
  (let ((lc (make-lazy-code* generator (make-table) '())))
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
  type   ;; 'free or 'local
  offset ;; offset in closure or stack
  pos    ;; set of offset where the identifier is located
  flags  ;; list of flags (possible flags : mutable)
  stype  ;; ctx type if identifier is a free var
)

(define (identifier-mutable? id)
  (member 'mutable (identifier-flags id)))

;; Add pos to identifier object 'id'
(define (identifier-addpos id pos)
  ;; Add only if not member
  (if (member pos (identifier-pos id))
     id
     (make-identifier (identifier-type id)
                      (identifier-offset id)
                      (cons pos (identifier-pos id))
                      (identifier-flags id)
                      (identifier-stype id))))

;; Remove pos from identifier object 'id'
(define (identifier-rmpos id pos)
  ;; Get new pos set
  (let ((npos (foldr (lambda (x y)
                       (if (= x pos)
                          y
                          (cons x y)))
                     '()
                     (identifier-pos id))))
    
    ;; Return new identifier
    (make-identifier (identifier-type id)
                     (identifier-offset id)
                     npos
                     (identifier-flags id)
                     (identifier-stype id))))

;;-----------------------------------------------------------------------------
;; Ctx management
;; TODO : fonction pour supprimer des ids de l'environnement pour eviter des list-tails, et mettre dans let, begin et do

(define nbctx 0)

(define-type ctx
  constructor: make-ctx*
  id      ;; TODO
  stack   ;; compile time stack, containing types
  env     ;; compile time environment
  nb-args ;; nb of arguments of current frame
)

(define (make-ctx stack env nb-args)
  (let ((id nbctx))
    (set! nbctx (+ nbctx 1))
    (make-ctx* id stack env nb-args)))

;; Apply 'nb' push to ctx
(define (ctx-push-nb ctx ctx-type nb)
  (if (= nb 0)
    ctx
    (ctx-push-nb (ctx-push ctx ctx-type)
                 ctx-type
                 (- nb 1))))

;; Push value to ctx
;; (Add type to stack and update identifier pos if sym (identifier symbol) given)
(define (ctx-push ctx ctx-type #!optional (sym #f))
  ;; Update pos in identifiers
  (define (push-pos env sym)
    (if (null? env)
       '()
       (let ((id (car env)))
         (if (eq? (car id) sym)
            (let ((identifier (identifier-addpos (cdr id) (- (length (ctx-stack ctx)) 1))))
              (cons (cons sym identifier)
                    (push-pos (cdr env) sym)))
            (cons id (push-pos (cdr env) sym))))))
  (if sym
    ;; If sym, update env and stack
    (let* ((env (push-pos (ctx-env ctx) sym))
           (stack (cons ctx-type (ctx-stack ctx))))
      (make-ctx stack env (ctx-nb-args ctx)))
    ;; Else, update only stack
    (make-ctx (cons ctx-type (ctx-stack ctx)) (ctx-env ctx) (ctx-nb-args ctx))))

;; Apply 'nb' pop to ctx
(define (ctx-pop-nb ctx nb)
  (if (= nb 0)
     ctx
     (ctx-pop-nb (ctx-pop ctx)
                 (- nb 1))))

;; Pop value from ctx
;; (Remove type from stack and update identifier pos)
(define (ctx-pop ctx)
  (let ((pos (- (length (ctx-stack ctx)) 2))) ;; Get pos from idx
    ;; Remove pos in identifiers
    (define (rm-pos env)
      (if (null? env)
        '()
        (let ((id (car env)))
          (cons (cons (car id)
                      (identifier-rmpos (cdr id) pos))
                (rm-pos (cdr env))))))
  (let ((env (rm-pos (ctx-env ctx)))
        (stack (cdr (ctx-stack ctx))))
    (make-ctx stack env (ctx-nb-args ctx)))))

;; Reset pos in identifier object associated to id 'sym'
(define (ctx-reset-pos ctx sym)
  (define (ctx-reset-pos-h env sym)
    (if (null? env)
       '()
       (let ((l (car env)))
         (if (eq? (car l) sym)
            (cons (cons sym
                        (make-identifier (identifier-type    (cdr l))
                                         (identifier-offset  (cdr l))
                                         ;; Set pos to a set of length 1 chich contains only offset
                                         (list (identifier-offset  (cdr l)))
                                         (identifier-flags   (cdr l))
                                         (identifier-stype   (cdr l))))
                  (cdr env))
            (cons (car env)
                  (ctx-reset-pos-h (cdr env) sym))))))
  
  (make-ctx (ctx-stack ctx)
            (ctx-reset-pos-h (ctx-env ctx) sym)
            (ctx-nb-args ctx)))

;; Change type of identifier located at 'stack-idx' (stack-idx must be converted to slot pos)
(define (ctx-change-type ctx stack-idx type)
  ;; Stack idx to slot pos
  (let ((pos (- (length (ctx-stack ctx)) stack-idx 2)))
    ;; Update stack types for each pos in 'pos' set
    (define (update-stack curr stack pos)
      (if (= curr 0)
         stack
         (if (member curr pos)
            (cons type        (update-stack (- curr 1) (cdr stack) pos))
            (cons (car stack) (update-stack (- curr 1) (cdr stack) pos)))))
    ;; Get pair (sym . identifier object) corresponding to identifier at pos 'pos'
    (define (get-id-at env pos)
      (if (null? env)
         #f
         (let ((envl (car env)))
           (if (member pos (identifier-pos (cdr envl)))
              envl
              (get-id-at (cdr env) pos)))))
    ;;
    (let* ((id (get-id-at (ctx-env ctx) pos))
           (positions
             (if id
                (identifier-pos (cdr id))
                (list pos)))
           (stack (update-stack (- (length (ctx-stack ctx)) 2) (ctx-stack ctx) positions)))
      (make-ctx stack
                (ctx-env ctx)
                (ctx-nb-args ctx)))))

;; Move nb slot from l-from to l-to (ascending)
(define (ctx-mov-nb ctx nb l-from l-to)
  (if (= nb 0)
     ctx
     (ctx-mov-nb (ctx-move ctx l-from l-to)
                 (- nb 1)
                 (+ l-from 1)
                 (+ l-to 1))))

;; Move slot from stack index 'l-from' to stack-index 'l-to'
;; Optionnally update env.
;; If update-env? is #f, checks if to type is mobj
(define (ctx-move ctx l-from l-to #!optional (update-env? #t))
    
  (let ((pos-from (- (length (ctx-stack ctx)) l-from 2)) ;; stack-idx to slot pos
        (pos-to   (- (length (ctx-stack ctx)) l-to 2)))  ;; stack-idx to slot pos
    
    (define (update-env env)
      (if (null? env)
        '()
        (let ((id (car env)))
          (cond ;; a - if id contains 'from', add 'to'
                ((and (member pos-from (identifier-pos (cdr id)))
                      (not (member pos-to (identifier-pos (cdr id)))))
                    (cons (cons (car id) (identifier-addpos (cdr id) pos-to))
                          (update-env (cdr env))))
                ;; b - if id contains 'to', remove 'from'
                ((and (member pos-to (identifier-pos (cdr id)))
                      (not (member pos-from (identifier-pos (cdr id)))))
                    (cons (cons (car id) (identifier-rmpos (cdr id) pos-to))
                          (update-env (cdr env))))
                (else
                    (cons id (update-env (cdr env))))))))
    
    ;; Update types in stack
    (define (update-stack stack)
      (if (and (not update-env?)
               (eq? (list-ref stack l-to) CTX_MOBJ)) ;; To-type is mobj
        stack
        (append (list-head stack l-to)
                (cons (list-ref stack l-from)
                      (list-tail stack (+ l-to 1))))))
    
    (let (;; 1 - Update ctx-env
          (env (if update-env?
                  (update-env (ctx-env ctx))
                  (ctx-env ctx)))
          ;; 2 - Update ctx-stack
          (stack (update-stack (ctx-stack ctx))))
      
      (make-ctx stack env (ctx-nb-args ctx)))))

;;-----------------------------------------------------------------------------

(define (jump-to-version cgc lazy-code ctx)

  (let ((label-dest (get-version lazy-code ctx)))
    (if label-dest

        ;; that version has already been generated, so just jump to it
        (x86-jmp cgc label-dest)

        ;; generate that version inline
        (let ((label-version (asm-make-label cgc (new-sym 'version))))
          (put-version lazy-code ctx label-version)
          (x86-label cgc label-version)
          ((lazy-code-generator lazy-code) cgc ctx)))))

;; Add callback
(define (add-callback cgc max-selector callback-fn)
  (create-stub label-do-callback-handler max-selector callback-fn))

;; Add function callback
(define (add-fn-callback cgc max-selector callback-fn) ;; NOTE : is all selector mechanism useful here ?
  (create-stub label-do-callback-fn-handler max-selector callback-fn))

;; Generate a continuation
;; First generate continuation lazy-code
;; Then patch mov before call to mov continuation address instead of stub address
(define (gen-version-continuation load-ret-label lazy-code ctx)
  
  (let ((continuation-label (asm-make-label #f (new-sym 'continuation_)))
        (load-addr (asm-label-pos load-ret-label)))

    ;; Generate lazy-code
    (code-add 
      (lambda (cgc)
        (asm-align cgc 4 0 #x90)
        (x86-label cgc continuation-label)
        ((lazy-code-generator lazy-code) cgc ctx)))
    
    (patch-continuation load-addr continuation-label)))

;; Generate a function
;; First generate function lazy-code
;; Then patch closure slot to jump directly to generated function
(define (gen-version-fn closure lazy-code ctx)
  
  (if verbose-jit
      (begin
        (print "GEN VERSION FN")
        (print " >>> ")
        (pp ctx)))

  ;; the jump instruction (call) at address "call-addr" must be redirected to
  ;; jump to the machine code corresponding to the version of
  ;; "lazy-code" for the context "ctx"

  (let ((label-dest (get-version lazy-code ctx)))
    (if label-dest

        ;; That version has already been generated, so just patch closure
        (let ((dest-addr (asm-label-pos label-dest)))
         (patch-closure closure ctx label-dest))

        ;; That version is not yet generated, so generate it and then patch call
        (let ((fn-label (asm-make-label #f (new-sym 'fn_entry_))))

            
          ;; Gen version
          (code-add
             (lambda (cgc)
                (asm-align cgc 4 0 #x90)
                (x86-label cgc fn-label)
                ((lazy-code-generator lazy-code) cgc ctx)))
          
          ;; Put version matching this ctx
          (put-version lazy-code ctx fn-label)
          ;; Patch closure
          (patch-closure closure ctx fn-label)))))

(define (gen-version jump-addr lazy-code ctx)

  (if verbose-jit
      (begin
        (print "GEN VERSION")
        (print " >>> ")
        (pp ctx)))

  ;; the jump instruction at address "jump-addr" must be redirected to
  ;; jump to the machine code corresponding to the version of
  ;; "lazy-code" for the context "ctx"

  (let ((label-dest (get-version lazy-code ctx)))
    (if label-dest

        (let ((dest-addr (asm-label-pos label-dest)))

          ;; that version has already been generated, so just patch jump

          (patch-jump jump-addr dest-addr)

          dest-addr)

        (begin

          (cond ((= (+ jump-addr (jump-size jump-addr)) code-alloc)
                 ;; (fall-through optimization)
                 ;; the jump is the last instruction previously generated, so
                 ;; just overwrite the jump
                 (if verbose-jit (println ">>> fall-through-optimization"))
                 (set! code-alloc jump-addr)) 

                (else
                 (patch-jump jump-addr code-alloc)))

          ;; generate that version inline
          (let ((label-version (asm-make-label #f (new-sym 'version))))
            (put-version lazy-code ctx label-version)
            (code-add
             (lambda (cgc)
               (x86-label cgc label-version)
               ((lazy-code-generator lazy-code) cgc ctx)))
            (asm-label-pos label-version))))))

;; Patch load at a call site to load continuation addr instead of continuation stub addr
(define (patch-continuation load-addr continuation-label)
  (if verbose-jit
      (begin
        (println ">>> patching mov at "
                 (number->string load-addr 16)
                 " : now load "
                 (asm-label-name continuation-label)
                 " (" (number->string (asm-label-pos continuation-label) 16) ")")))
  
  (code-gen 'x86-64 load-addr (lambda (cgc) (x86-mov cgc (x86-rax) (x86-imm-int (asm-label-pos continuation-label)))))
  (asm-label-pos continuation-label))

;; Patch jump at jump-addr: change jump destination to dest-addr
(define (patch-jump jump-addr dest-addr)

  (if verbose-jit
    (println ">>> patching jump at "
             (number->string jump-addr 16)
             " -> "
             (number->string dest-addr 16)))

  (if (not (= jump-addr dest-addr))
      (let ((size (jump-size jump-addr)))
        (put-i32 (- (+ jump-addr size) 4)
                 (- dest-addr (+ jump-addr size))))))

;; Patch closure
(define (patch-closure closure ctx label)
  
  (let* ((label-addr (asm-label-pos  label))
         (label-name (asm-label-name label))
         (index (get-closure-index ctx))
         (offset (+ 8 (* index 8)))) ;; +8 because of header
    
    (if verbose-jit
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

;; Return n firsts elements of lst
(define (list-head lst n)
  (cond ((= n 0) '())
        ((null? lst) (error "Not enough els"))
        (else (cons (car lst) (list-head (cdr lst) (- n 1))))))

;; Gen FATAL dynamic type test
;; FATAL means that if type test fails, then it stops execution
;; Check type 'type' for stack slot at 'stack-idx' and jump to 'succ' if succeess
(define (gen-fatal-type-test type stack-idx succ ast)
  
    (make-lazy-code
       (lambda (cgc ctx)
         
         (let (;; Lazy type error
               (lazy-fail
                 (make-lazy-code
                   (lambda (cgc ctx)
                     (gen-error cgc (ERR_TYPE_EXPECTED type)))))
               ;; Current information on type
               (known-type (list-ref (ctx-stack ctx) stack-idx)))

           ;; If 'all-tests' option enabled, then remove type information
           (if all-tests
              (set! known-type CTX_UNK))
           
           (cond ;; Known type is the expected type
                 ((eq? known-type type)          (jump-to-version cgc succ ctx))
                 ;; Known type is not the expected type and not unknown then type error
                 ((not (eq? known-type CTX_UNK)) (jump-to-version cgc lazy-fail ctx))
                 ;; Known type is unknown
                 (else                   
                   (let* ((nctx (ctx-change-type ctx stack-idx type))
                          (ctx-succ (make-ctx (ctx-stack nctx) (ctx-env nctx) (ctx-nb-args nctx)))
                          
                          (label-jump (asm-make-label cgc (new-sym 'patchable_jump)))
                          (stub-labels
                                (add-callback cgc 1
                                  (let ((prev-action #f))

                                    (lambda (ret-addr selector)
                                      (let ((stub-addr (- ret-addr 5 2))
                                            (jump-addr (asm-label-pos label-jump)))
                                      
                                        (if verbose-jit
                                            (begin
                                              (println ">>> selector= " selector)
                                              (println ">>> prev-action= " prev-action)))
                                      
                                        (if (not prev-action)
                                            
                                            (begin (set! prev-action 'no-swap)
                                                   (if (= selector 1)
                                                    
                                                      ;; overwrite unconditional jump
                                                      (gen-version (+ jump-addr 6) lazy-fail ctx)
                                                    
                                                      (if (= (+ jump-addr 6 5) code-alloc)

                                                        (begin (if verbose-jit (println ">>> swapping-branches"))
                                                               (set! prev-action 'swap)
                                                               ;; invert jump direction
                                                               (put-u8 (+ jump-addr 1) (fxxor 1 (get-u8 (+ jump-addr 1))))
                                                               ;; make conditional jump to stub
                                                               (patch-jump jump-addr stub-addr)
                                                               ;; overwrite unconditional jump
                                                               (gen-version
                                                               (+ jump-addr 6)
                                                               succ
                                                               ctx-succ))

                                                        ;; make conditional jump to new version
                                                        (gen-version jump-addr succ ctx-succ))))

                                            (begin ;; one branch has already been patched
                                                   ;; reclaim the stub
                                                   (release-still-vector (get-scmobj ret-addr))
                                                   (stub-reclaim stub-addr)
                                                   (if (= selector 0)
                                                      (gen-version (if (eq? prev-action 'swap) (+ jump-addr 6) jump-addr) succ ctx-succ)
                                                      (gen-version (if (eq? prev-action 'swap) jump-addr (+ jump-addr 6)) lazy-fail ctx))))))))))

                     
                   (if verbose-jit
                       (println ">>> Gen dynamic type test at index " stack-idx))
                   
                   ;; If 'count-tests' option enabled, then inc slot
                   (if count-tests
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
                         ((member type (list CTX_CLO CTX_PAI CTX_STR CTX_VECT CTX_SYM CTX_IPORT CTX_OPORT))
                           
                             (x86-mov cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp)))
                             (x86-mov cgc (x86-rbx) (x86-rax)) ;; value in rax and rbx
                             (x86-and cgc (x86-rax) (x86-imm-int 3))
                             (x86-cmp cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                             (x86-jne cgc (list-ref stub-labels 1)) ;; it is not a memory allocated object then error
                             (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) (x86-rbx)))
                             (x86-and cgc (x86-rax) (x86-imm-int 248)) ;; 0...011111000 to get type in object header
                             ;; STAG_XXX << 3
                             (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (cond ((eq? type CTX_CLO)   STAG_PROCEDURE)
                                                                            ((eq? type CTX_STR)   STAG_STRING)
                                                                            ((eq? type CTX_SYM)   STAG_SYMBOL)
                                                                            ((eq? type CTX_VECT)  STAG_VECTOR)
                                                                            ((eq? type CTX_IPORT) STAG_IPORT)
                                                                            ((eq? type CTX_OPORT) STAG_OPORT)
                                                                            ((eq? type CTX_PAI)   STAG_PAIR))))))
                         ;; Other
                         (else (error "Unknown type " type)))
                   (x86-label cgc label-jump)
                   (x86-je cgc (list-ref stub-labels 0))
                   (x86-jmp cgc (list-ref stub-labels 1)))))))))

;; TODO Factoriser avec gen-fatal-...

;; TODO
;; Create lazy code for type test of stack slot (stack-idx)
;; jump to lazy-success if test succeeds
;; jump to lazy-fail if test fails
;; Create lazy code for type test of stack slot (stack-idx)
;; jump to lazy-success if test succeeds
;; jump to lazy-fail if test fails
(define (gen-dyn-type-test type stack-idx ctx-success lazy-success ctx-fail lazy-fail)

    (make-lazy-code
       (lambda (cgc ctx)
         (let* ((label-jump (asm-make-label cgc (new-sym 'patchable_jump)))
                (stub-labels
                      (add-callback cgc 1
                        (let ((prev-action #f))

                          (lambda (ret-addr selector)
                            (let ((stub-addr (- ret-addr 5 2))
                                  (jump-addr (asm-label-pos label-jump)))
                            
                              (if verbose-jit
                                  (begin
                                    (println ">>> selector= " selector)
                                    (println ">>> prev-action= " prev-action)))
                            
                              (if (not prev-action)
                                  
                                  (begin (set! prev-action 'no-swap)
                                         (if (= selector 1)
                                          
                                            ;; overwrite unconditional jump
                                            (gen-version (+ jump-addr 6) lazy-fail ctx-fail)
                                          
                                            (if (= (+ jump-addr 6 5) code-alloc)

                                              (begin (if verbose-jit (println ">>> swapping-branches"))
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

         (if verbose-jit
             (println ">>> Gen dynamic type test at index " stack-idx))
         
         ;; If 'count-tests' option enabled, then inc slot
         (if count-tests
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
               ((member type (list CTX_CLO CTX_PAI CTX_SYM CTX_VECT CTX_STR CTX_IPORT CTX_OPORT))      
                   (x86-mov cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp)))
                   (x86-mov cgc (x86-rbx) (x86-rax)) ;; value in rax and rbx
                   (x86-and cgc (x86-rax) (x86-imm-int 3))
                   (x86-cmp cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                   (x86-jne cgc label-jump)
                   (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) (x86-rbx)))
                   (x86-and cgc (x86-rax) (x86-imm-int 248)) ;; 0...011111000 to get type in object header
                   ;; STAG_XXX << 3
                   (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (cond ((eq? type CTX_CLO)  STAG_PROCEDURE)
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
         (x86-jmp cgc (list-ref stub-labels 1))))))

;;-----------------------------------------------------------------------------
;; Global cc table

;; Current fixed global-cc-table max size
(define global-cc-table-maxsize 250)
;; Current shape of the global cc table
(define global-cc-table (make-table))

;; Get closure index associated to ctx. If ctx is not in the
;; global cc table, then this function adds it and returns
;; the new associated index. Index starts from 0.
;; Store and compare ctx-stack in enough because environment is
;; the same for all versions of a lazy-object.
(define (get-closure-index ctx)
  (if (= (table-length global-cc-table) global-cc-table-maxsize)
     (error "CC-TABLE OVERFLOW"))
  (let ((res (table-ref global-cc-table (ctx-stack ctx) #f)))
    (if res
      res
      (let ((value (table-length global-cc-table)))
        (table-set! global-cc-table (ctx-stack ctx) value)
        value))))

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

;;-----------------------------------------------------------------------------
;; Utils

;; Build list of length n with optional init value
(define (make-list n #!optional (init #f))
  (if (= 0 n)
    '()
    (cons init (make-list (- n 1) init))))

;; Returns a newly allocated string which is a copy of str with all chars upcase
;; Accepts symbols
(define (string-upcase str)
  (define (string-upcase-h str pos newstr)
    (if (= pos (string-length str))
      newstr
      (begin (string-set! newstr pos (char-upcase (string-ref str pos)))
             (string-upcase-h str (+ pos 1) newstr))))
  (if (symbol? str)
    (let ((s (symbol->string str)))
      (string-upcase-h s 0 (make-string (string-length s))))
    (string-upcase-h str 0 (make-string (string-length str)))))


;; Foldr
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))