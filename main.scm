;; File: lazy-comp4.scm

;; June 26, 2014

;; rm lazy-comp4.o* ; gsc lazy-comp4.scm ; gsc -i lazy-comp4.o1

;;-----------------------------------------------------------------------------

;; Import functions for generating and executing machine code.

(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm")
(include "~~lib/_codegen#.scm")

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

;; The procedure do-callback is callable from generated machine code.

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

;; The label for procedure do-callback

(define label-do-callback #f)

(define (init-labels cgc)

  (set! label-do-callback
        (asm-make-label
         cgc
         'do_callback
         (##foreign-address
          ((c-lambda ()
                     (pointer void)
                     "___result = ___CAST(void*,do_callback);"))))))

;;-----------------------------------------------------------------------------

;; Machine code block management

(define mcb-len 100000)
(define fun-len 100000)
(define mcb #f)
(define mcb-addr #f)
(define fun-addr #f)

(define (init-mcb)
  (set! mcb (##make-machine-code-block (+ mcb-len fun-len)))
  (set! mcb-addr (##foreign-address mcb))
  (set! fun-addr (+ (##foreign-address mcb) mcb-len)))

(define (write-mcb code start)
  (let ((len (u8vector-length code)))
    (let loop ((i (fx- len 1)))
      (if (fx>= i 0)
          (begin
            (##machine-code-block-set! mcb (fx+ start i) (u8vector-ref code i))
            (loop (fx- i 1)))
          mcb))))

(define (code-gen arch addr gen #!optional (show-listing? #t))
  (let* ((cgc (make-codegen-context))
         (endianness 'le))

    (asm-init-code-block cgc addr endianness)
    (codegen-context-listing-format-set! cgc 'nasm)
    (x86-arch-set! cgc arch)

    (gen cgc)

    (let ((code (asm-assemble-to-u8vector cgc)))
      (if show-listing?
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
  (init-mcb)
  (set! code-alloc mcb-addr)
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
    (pp (list 'obj= obj))
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

;;-----------------------------------------------------------------------------

(define label-do-callback-handler #f)

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

    (x86-ret  cgc)

    label-handler))

(define (init-rtlib cgc)
  (let ((label-rtlib-skip (asm-make-label cgc 'rtlib_skip)))

    (x86-jmp cgc label-rtlib-skip)

    (set! label-do-callback-handler
          (gen-handler cgc 'do_callback_handler label-do-callback))

    (x86-label cgc label-rtlib-skip)

    (x86-push cgc (x86-rdx))
    (x86-push cgc (x86-rsi))
    (x86-push cgc (x86-rdi))
    (x86-mov  cgc (x86-rcx) (x86-imm-int 0))))

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

(define nb-c-caller-save-regs
  (length c-caller-save-regs))

(define rcx-pos
  (- nb-c-caller-save-regs
     (length (member (x86-rcx) c-caller-save-regs))))

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

(define-type ctx
  stack ;; compile time stack, containing types
)

(define (ctx-push ctx type)
  (make-ctx (cons type (ctx-stack ctx))))

(define (ctx-pop ctx)
  (make-ctx (cdr (ctx-stack ctx))))

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

(define (add-callback cgc max-selector callback-fn)
  (create-stub label-do-callback-handler max-selector callback-fn))

(define (gen-version jump-addr lazy-code ctx)

  (print ">>> ")
  (pp ctx)

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
                 (println ">>> fall-through-optimization")
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

(define (patch-jump jump-addr dest-addr)

  (println ">>> patching jump at "
           (number->string jump-addr 16)
           " -> "
           (number->string dest-addr 16))

  (if (not (= jump-addr dest-addr))
      (let ((size (jump-size jump-addr)))
        (put-i32 (- (+ jump-addr size) 4)
                 (- dest-addr (+ jump-addr size))))))

(define (jump-size jump-addr)
  (if (= (get-u8 jump-addr) #x0f) 6 5))

(define (gen-ast ast succ)

  (cond ((or (number? ast) (boolean? ast))
         (make-lazy-code
          (lambda (cgc ctx)
            (x86-push cgc (x86-imm-int (obj-encoding ast)))
            (jump-to-version cgc
                             succ
                             (ctx-push ctx
                                       (cond ((number? ast) 'num)
                                             ((boolean? ast) 'bool)))))))

        ((symbol? ast) ;; a, b, c refer to the arguments of the function
         (make-lazy-code
          (lambda (cgc ctx)
            (let* ((fs (length (ctx-stack ctx)))
                   (pos (- fs 1 (cdr (assoc ast '((a . 0) (b . 1) (c . 2)))))))
              (x86-push cgc (x86-mem (* pos 8) (x86-rsp)))
              (jump-to-version cgc
                               succ
                               (ctx-push ctx
                                         (list-ref (ctx-stack ctx) pos)))))))

        ((pair? ast)
         (let ((op (car ast)))

           (cond ((member op '(+ - * < =))
                  (let* ((lazy-code2
                          (make-lazy-code
                           (lambda (cgc ctx)
                             (x86-pop cgc (x86-rax))
                             (case op
                               ((+)
                                (x86-add cgc (x86-mem 0 (x86-rsp)) (x86-rax)))
                               ((-)
                                (x86-sub cgc (x86-mem 0 (x86-rsp)) (x86-rax)))
                               ((*)
                                (x86-imul cgc (x86-mem 0 (x86-rsp)) (x86-rax)))
                               ((<)
                                (let ((label-done
                                       (asm-make-label cgc (new-sym 'done))))
                                  (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                  (x86-jl  cgc label-done)
                                  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                  (x86-label cgc label-done)
                                  (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))))
                               ((=)
                                (let ((label-done
                                       (asm-make-label cgc (new-sym 'done))))
                                  (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                                  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                  (x86-je  cgc label-done)
                                  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                  (x86-label cgc label-done)
                                  (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))))
                               (else
                                (error "unknown op" op)))
                             (jump-to-version cgc
                                              succ
                                              (ctx-push
                                               (ctx-pop (ctx-pop ctx))
                                               (cond ((member op '(+ - *)) 'num)
                                                     ((member op '(< =)) 'bool)))))))
                         (lazy-code1
                          (gen-ast
                           (caddr ast)
                           lazy-code2)))
                    (gen-ast
                     (cadr ast)
                     lazy-code1)))

                 ((eq? op 'if)
                  (let* ((lazy-code0
                          (gen-ast (cadddr ast) succ))
                         (lazy-code1
                          (gen-ast (caddr ast) succ))
                         (lazy-code-test
                          (make-lazy-code
                           (lambda (cgc ctx)
                             (let* ((ctx0
                                     (ctx-pop ctx))

                                    (ctx1
                                     ctx0)

                                    (label-jump
                                     (asm-make-label
                                      cgc
                                      (new-sym 'patchable_jump)))

                                    (stub-labels
                                     (add-callback
                                      cgc
                                      1
                                      (let ((prev-action #f))
                                        (lambda (ret-addr selector)
                                          (let ((stub-addr
                                                 (- ret-addr 5 2))
                                                (jump-addr
                                                 (asm-label-pos label-jump)))

                                            (println ">>> selector= " selector)
                                            (println ">>> prev-action= " prev-action)

                                            (if (not prev-action)

                                                (begin

                                                  (set! prev-action 'no-swap)

                                                  (if (= selector 1)

                                                      ;; overwrite unconditional jump
                                                      (gen-version
                                                       (+ jump-addr 6)
                                                       lazy-code1
                                                       ctx1)

                                                      (if (= (+ jump-addr 6 5) code-alloc)

                                                          (begin

                                                            (println ">>> swapping-branches")

                                                            (set! prev-action 'swap)

                                                            ;; invert jump direction
                                                            (put-u8 (+ jump-addr 1)
                                                                    (fxxor 1 (get-u8 (+ jump-addr 1))))

                                                            ;; make conditional jump to stub
                                                            (patch-jump jump-addr stub-addr)

                                                            ;; overwrite unconditional jump
                                                            (gen-version
                                                             (+ jump-addr 6)
                                                             lazy-code0
                                                             ctx0))

                                                          ;; make conditional jump to new version
                                                          (gen-version
                                                           jump-addr
                                                           lazy-code0
                                                           ctx0))))

                                                (begin

                                                  ;; one branch has already been patched

                                                  ;; reclaim the stub
                                                  (release-still-vector
                                                   (get-scmobj ret-addr))
                                                  (stub-reclaim stub-addr)

                                                  (if (= selector 0)

                                                      (gen-version
                                                       (if (eq? prev-action 'swap)
                                                           (+ jump-addr 6)
                                                           jump-addr)
                                                       lazy-code0
                                                       ctx0)

                                                      (gen-version
                                                       (if (eq? prev-action 'swap)
                                                           jump-addr
                                                           (+ jump-addr 6))
                                                       lazy-code1
                                                       ctx1))))))))))

                               (x86-pop cgc (x86-rax))
                               (x86-cmp cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                               (x86-label cgc label-jump)
                               (x86-je  cgc (list-ref stub-labels 0))
                               (x86-jmp cgc (list-ref stub-labels 1)))))))
                    (gen-ast
                     (cadr ast)
                     lazy-code-test)))

                 ((eq? op 'Define)
                    (let ((lazy-ret (make-lazy-code (lambda (cgc ctx) (x86-ret cgc)))))
                       (make-lazy-code (lambda (cgc ctx)
                                          (let ((stub-labels (add-callback cgc
                                                                           0
                                                                           (lambda (ret-addr selector)
                                                                              (println "EXEC CALLBACK")
                                                                              (println "   >> selector = ")
                                                                              (println selector)
                                                                              (gen-version -1 lazy-ret ctx)))))
                                             (set! functions (cons (cons (cadr ast) (list-ref stub-labels 0)) functions))
                                             (jump-to-version cgc succ ctx))))))

                 (else
                  ...))))

        (else
         (error "unknown ast" ast))))

;; TODO
(define functions '())

;;-----------------------------------------------------------------------------

(define (lazy-exprs exprs)
  (if (null? exprs)
    (make-lazy-code
      (lambda (cgc ctx)
        (x86-pop cgc (x86-rax))
        (x86-add cgc (x86-rsp) (x86-imm-int (* (- (length (ctx-stack ctx)) 3) 8)))
        (x86-pop cgc (x86-rdi))
        (x86-pop cgc (x86-rsi))
        (x86-pop cgc (x86-rdx))
        (x86-ret cgc)))
    (gen-ast (car exprs)
             (lazy-exprs (cdr exprs)))))

(define (compile-lambda params expr)

  (init)

  (let ((lazy-code (lazy-exprs expr)))
    (gen-version code-alloc
                 lazy-code
                 (make-ctx (map (lambda (x) 'unknown) params)))
    (lambda (#!optional (arg1 0) (arg2 0) (arg3 0))
      (##machine-code-block-exec mcb arg1 arg2 arg3))))

(define (test expr)
  (println "******************************************************************")
  (print "*** TESTING: ")
  (pretty-print (list 'define 'f (list 'lambda '(a b) expr)))
  (let ((f (compile-lambda '(a b) expr)))

    (define (t . args)
      (let ((result (apply f (reverse args))))
        (print "*** RESULT: ")
        (pretty-print (list (cons 'f args) '=> result))))

    (t 1 2)
    ;(t 2 1)
    ;(t 1 2)
    ;(t 2 1)
))

;(test '(if (< a b) 11 22))

(test '((Define RR 10)))

;(test '((if #t 10 20)))

;(test '(- (if (< a b) 11 22) (if (< b a) 33 44)))

;;-----------------------------------------------------------------------------
