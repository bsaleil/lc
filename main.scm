;; File: lazy-comp4.scm

;; June 26, 2014

;; rm lazy-comp4.o* ; gsc lazy-comp4.scm ; gsc -i lazy-comp4.o1

;;-----------------------------------------------------------------------------

;; Import functions for generating and executing machine code.

(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm")
(include "~~lib/_codegen#.scm")

;;-----------------------------------------------------------------------------

(include "native.scm")
(include "ast.scm")
(include "x86-debug.scm")
(include "expand.scm")

;;-----------------------------------------------------------------------------

;; NOTE : Should be a macro
(define dev-log #f)

;;-----------------------------------------------------------------------------

;; Types tag
(define TAG_NUMBER  0)
(define TAG_MEMOBJ 1)
(define ENCODING_VOID -18)

;; Types subtag
(define STAG_PROCEDURE 14)
(define STAG_PAIR    1)
(define STAG_MOBJECT 2)
(define STAG_VECTOR  0)

;; Context
(define CTX_NUM   'number)
(define CTX_BOOL  'boolean)
(define CTX_CLO   'closure)
(define CTX_PAI   'pair)
(define CTX_UNK   'unknown)
(define CTX_VOID  'void)
(define CTX_NULL  'null)
(define CTX_CTXID 'ctx)
(define CTX_RETAD 'retAddr)
(define CTX_VECT  'vector)

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

;; Errors
(define ERR_MSG           "EXEC ERROR")
(define ERR_NUM_EXPECTED  "NUMBER EXPECTED")
(define ERR_PRO_EXPECTED  "PROCEDURE EXPECTED")
(define ERR_PAIR_EXPECTED "PAIR EXPECTED")
(define ERR_ARR_OVERFLOW  "ARITHMETIC OVERFLOW")

;; Gen code for error
;; stop-exec? to #f to continue after error
(define (gen-error cgc err #!optional (stop-exec? #t))

  ;; Put error msg in RAX
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding err)))

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
       ))

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
         
         (ctx-id (get-i64 (+ sp (* nb-c-caller-save-regs 8) 8)))
         (ctx (cdr (assoc ctx-id ctx_ids)))
         
         (closure
          (get-i64 (+ sp (* nb-c-caller-save-regs 8) 24)))
         
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

;; The label for procedure do-callback
(define label-exec-error     #f)
(define label-do-callback    #f)
(define label-do-callback-fn #f)

(define (init-labels cgc)

  (set! label-exec-error
        (asm-make-label
         cgc
         'exec-error
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,exec_error);")))))

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
                       "___result = ___CAST(void*,dump_stack);"))))))

;;-----------------------------------------------------------------------------

;; Machine code block management

;; HEAP
(define heap-len 50000)
(define heap-addr #f)
(define alloc-ptr (x86-r12))

;; CODE
(define code-len 50000)
(define code-addr #f)

;; MCB :
;; 0                 heap-len              mcb-len
;; +--------------------+--------------------+
;; |    Heap            |    Code            |
;; |                    |                    |
;; |                    |                    |
;; +--------------------+--------------------+

(define mcb #f)
(define mcb-len (+ heap-len code-len))
(define mcb-addr #f)

(define (init-mcb)
  (set! mcb (##make-machine-code-block mcb-len))
  (set! mcb-addr (##foreign-address mcb))
  (set! heap-addr (+ mcb-addr 8))
  (set! code-addr (+ mcb-addr heap-len)))

;; BLOCK :
;; 0          8                       (nb-globals * 8 + 8)
;; +----------+----------+----------+----------+
;; | Bottom   | Global 1 |    ...   | Global n |
;; | stack    |          |          |          |
;; | addr     |          |          |          |
;; +----------+----------+----------+----------+

(define block #f)
;; TODO : fixed number of globals
(define block-len (* 8 (+ 1 100))) ;; 1 stack addr, 100 globals
(define block-addr #f)

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
      (if dev-log
          (begin
            (println "------------------------------------------------------------------------")
            (asm-display-listing cgc (current-output-port) #t)))
      (write-mcb code (- addr mcb-addr))
      (u8vector-length code))))

;;-----------------------------------------------------------------------------

;; Code and stub management

(define functions '())
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
    (if dev-log
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
    
    (gen-native cgc)
    
    (x86-label cgc label-rtlib-skip)

    (push-regs cgc prog-regs)
    
    ;; Put bottom of the stack (after saving registers) at "block-addr + 0"
    (x86-mov cgc (x86-rax) (x86-imm-int block-addr))
    (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-rsp))
    
    (x86-mov cgc (x86-rcx) (x86-imm-int 0))
    (x86-mov cgc alloc-ptr  (x86-imm-int heap-addr))       ;; Heap addr ub alloc-ptr
    (x86-mov cgc (x86-r10) (x86-imm-int (+ block-addr 8))) ;; Globals addr in r10
    ))

(define (init)

  (init-code-allocator)

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
  (list (x86-rax)
        (x86-rbx)
        (x86-rcx)
        (x86-rdx)
        (x86-rsp)
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
        alloc-ptr
        (x86-r10) ;; Globals
        ))

(define nb-c-caller-save-regs
  (length c-caller-save-regs))

;; TODO : *-pos: "nb-c-caller-save-regs" as parameter
(define rdx-pos
  (- nb-c-caller-save-regs
     (length (member (x86-rdx) c-caller-save-regs))))

(define rcx-pos
  (- nb-c-caller-save-regs
     (length (member (x86-rcx) c-caller-save-regs))))

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

(define-type lazy-code
  constructor: make-lazy-code*
  generator
  versions
)

(define (make-lazy-code generator)
  (make-lazy-code* generator (make-table)))

(define (get-version lazy-code ctx)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-ref versions ctx #f)))

(define (put-version lazy-code ctx v)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-set! versions ctx v)))

;; Identifier in environment associated to an identifier
(define-type identifier
  type   ;; 'free or 'local
  offset ;; offset in closure or stack
  flags  ;; List of flags (possible flags : mutable)
)

(define (identifier-mutable? id)
  (member 'mutable (identifier-flags id)))

(define-type ctx
  stack ;; compile time stack, containing types
  env   ;; compile time environment
  nb-args ;;  TODO
)

(define (ctx-push ctx type)
  (make-ctx (cons type (ctx-stack ctx)) (ctx-env ctx) (ctx-nb-args ctx)))

(define (ctx-pop ctx)
  (make-ctx (cdr (ctx-stack ctx)) (ctx-env ctx) (ctx-nb-args ctx)))

(define (ctx-pop-nb ctx nb-pop)
  (cond ((= nb-pop 0) ctx)
        ((null? (ctx-stack ctx)) (error "pop empty ctx"))
        (else (ctx-pop-nb (ctx-pop ctx) (- nb-pop 1)))))

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
        (x86-label cgc continuation-label)
        ((lazy-code-generator lazy-code) cgc ctx)))
    
    (patch-continuation load-addr continuation-label)))
  
;; Generate a function
;; First generate function lazy-code
;; Then patch closure slot to jump directly to generated function
(define (gen-version-fn closure lazy-code ctx)
  
  (if dev-log
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
                (x86-label cgc fn-label)
                ((lazy-code-generator lazy-code) cgc ctx)))
          ;; Put version matching this ctx
          (put-version lazy-code ctx fn-label)
          ;; Patch closure
          (patch-closure closure ctx fn-label)))))

(define (gen-version jump-addr lazy-code ctx)

  (if dev-log
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
                 (if dev-log (println ">>> fall-through-optimization"))
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
  (if dev-log
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

  (if dev-log
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
         (offset (* index 8)))
    
    (if dev-log
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

;; Gen ast for a list of exprs with successor 'succ'
(define (gen-ast-l lst succ)
  (cond ((null? lst) (error "Empty list"))
        ((= (length lst) 1) (gen-ast (car lst) succ))
        (else (gen-ast (car lst) (gen-ast-l (cdr lst) succ)))))

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
                            
                              (if dev-log
                                  (begin
                                    (println ">>> selector= " selector)
                                    (println ">>> prev-action= " prev-action)))
                            
                              (if (not prev-action)
                                  
                                  (begin (set! prev-action 'no-swap)
                                         (if (= selector 1)
                                          
                                            ;; overwrite unconditional jump
                                            (gen-version (+ jump-addr 6) lazy-fail ctx-fail)
                                          
                                            (if (= (+ jump-addr 6 5) code-alloc)

                                              (begin (if dev-log (println ">>> swapping-branches"))
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

         (if dev-log
             (println ">>> Gen dynamic type test at index " stack-idx))
         
         (cond ;; Number type test
               ((eq? type CTX_NUM)
                   (x86-mov cgc (x86-rax) (x86-imm-int 3)) ;; rax = 0...011b
                   (x86-and cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp)))
                   (x86-cmp cgc (x86-rax) (x86-imm-int 0)))
               ;; Procedure type test
               ;((eq? type 'procedure)
               ((member type (list CTX_CLO CTX_PAI))
                   (x86-mov cgc (x86-rax) (x86-mem (* 8 stack-idx) (x86-rsp)))
                   (x86-mov cgc (x86-rbx) (x86-rax)) ;; value in rax and rbx
                   (x86-and cgc (x86-rax) (x86-imm-int 3))
                   (x86-cmp cgc (x86-rax) (x86-imm-int 1))
                   (x86-jne cgc (list-ref stub-labels 1)) ;; it is not a memory allocated object then error
                   (x86-mov cgc (x86-rax) (x86-mem (* -1 TAG_MEMOBJ) (x86-rbx)))
                   (x86-and cgc (x86-rax) (x86-imm-int 248)) ;; 0...011111000 to get type in object header
                   ;; STAG_XXX << 3
                   (x86-cmp cgc (x86-rax) (x86-imm-int (* 8 (cond ((eq? type CTX_CLO) STAG_PROCEDURE)
                                                                  ((eq? type CTX_PAI) STAG_PAIR))))))
               ;; Other
               (else (error "Unknown type")))
         (x86-label cgc label-jump)
         (x86-je cgc (list-ref stub-labels 0))
         (x86-jmp cgc (list-ref stub-labels 1))))))

;;-----------------------------------------------------------------------------

(define (lazy-exprs exprs succ)
  (if (null? exprs)
      (or succ
          (make-lazy-code
            (lambda (cgc ctx)
              (x86-pop cgc (x86-rax))
              (x86-add cgc (x86-rsp) (x86-imm-int (* (- (length (ctx-stack ctx)) 1) 8)))
              (pop-regs-reverse cgc prog-regs)
              (x86-ret cgc))))
      (gen-ast (car exprs)
               (lazy-exprs (cdr exprs) succ))))

(define (exec lib prog)
  
  (init)
  
  (let* ((lazy-prog (lazy-exprs prog #f))
         (lazy-lib  (lazy-exprs lib  lazy-prog)))
    
    (gen-version code-alloc
                 lazy-lib
                 (make-ctx '() '() -1)))
  
  ;(print "RESULT = ")
  (##machine-code-block-exec mcb))
  ;(pp (##machine-code-block-exec mcb)))

;; Get lib
(define lib  (expand (read-all (open-input-file "./lib.scm"))))

;; Get prog
(define prog (expand (read-all)))

;; Exec lib and prog
(exec lib prog)