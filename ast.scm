
(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; Base ctx for procedure call
;; TODO : cons where used
(define base-ctx (list CTX_CLO))

;; TODO : RCX global ? (used by if/else stubs)

;;-----------------------------------------------------------------------------
;; Primitives

;; Primitives for type tests
(define prim-tests '(
  $output-port?
  $input-port?
  $symbol?
  $string?
  $char?
  $vector?
  $number?
  $procedure?
  $pair?))

;; Primitives for operators
(define prim-operators '(
  $+
  $-
  $*
  $quotient
  $modulo
  $<
  $>
  $=
))

;; Primitives for funtions
(define prim-fn '(
  $eof-object?
  $write-char
  $read-char
  $close-output-port
  $close-input-port
  $open-output-file
  $open-input-file
  $error
  $string-set!
  $make-string
  $symbol->string
  $string->symbol
  $string-length
  $string-ref
  $integer->char
  $char->integer
  $vector-set!
  $vector-ref
  $vector-length
  $make-vector
  $cons
  $set-car!
  $set-cdr!
  $car
  $cdr
))

;;-----------------------------------------------------------------------------
;; AST DISPATCH

;; Gen lazy code from a list of exprs
(define (gen-ast-l lst succ)
  (cond ((null? lst) (error "Empty list"))
        ((= (length lst) 1) (gen-ast (car lst) succ))
        (else (gen-ast (car lst) (gen-ast-l (cdr lst) succ)))))

;; Gen lazy code from ast
(define (gen-ast ast succ)
  (cond ;; String
        ((string? ast)  (mlc-string ast succ #f))
        ;; Symbol
        ((symbol? ast)  (mlc-identifier ast succ))
        ;; Literal
        ((literal? ast) (mlc-literal ast succ))
        ;; Pair
        ((pair? ast)
         (let ((op (car ast)))
           (cond ;; Special with call
                 ((eq? op '$$putchar) (mlc-special-c ast succ))
                 ;; Special without call
                 ((member op prim-fn) (mlc-special-nc ast succ))
                 ;; Quote
                 ((eq? 'quote (car ast)) (mlc-quote (cadr ast) succ))
                 ;; Set!
                 ((eq? 'set! (car ast)) (mlc-set! ast succ))
                 ;; Lambda
                 ((eq? op 'lambda) (mlc-lambda ast succ))
                 ;; Begin
                 ((eq? op 'begin) (mlc-begin ast succ))
                 ;; Do
                 ((eq? op 'do) (mlc-do ast succ))
                 ;; Binding
                 ((member op '(let let* letrec)) (mlc-binding ast succ op))
                 ;; Operator num
                 ((member op prim-operators) (mlc-op-num ast succ op))
                 
                 ((member op '(+ - *)) (mlc-op-numn ast succ op))
                 ;; Operator gen
                 ((eq? op '$eq?) (mlc-op-gen ast succ op))
                 ;; Tests
                 ((member op prim-tests) (mlc-test ast succ))
                 ;; If
                 ((eq? op 'if) (mlc-if ast succ))
                 ;; Define
                 ((eq? op 'define) (mlc-define ast succ))
                 ;; Apply
                 ((eq? op '$apply) (mlc-apply ast succ))
                 ;; Call expr
                 (else (mlc-call ast succ)))))
        ;; *unknown*
        (else
         (error "unknown ast" ast))))

;;-----------------------------------------------------------------------------
;; LITERALS

;;
;; Make lazy code from num/bool/char/null literal
;;
(define (mlc-literal ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      (if (and (number? ast) (>= ast 536870912)) ;; 2^(32-1-2) (32bits-sign-tags)
          (begin (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding ast)))
                 (x86-push cgc (x86-rax)))
          (x86-push cgc (x86-imm-int (obj-encoding ast))))
      (jump-to-version cgc
                       succ
                       (ctx-push ctx
                                 (cond ((number? ast)  CTX_NUM)
                                       ((boolean? ast) CTX_BOOL)
                                       ((char? ast)    CTX_CHAR)
                                       ((null? ast)    CTX_NULL)))))))
;;
;; Make lazy code from symbol literal
;;
(define (mlc-symbol ast succ)
  (mlc-string (symbol->string ast) succ #t))

;;
;; Make lazy code from vector literal
;;
(define (mlc-vector ast succ)
  
  ;; Generate lazy object which pop object from stack
  ;; and mov it to vector slot and jump to next element
  (define (lazy-vector-set-gen idx)
    (make-lazy-code
      (lambda (cgc ctx)
        (x86-pop cgc (x86-rax)) ;; el
        (x86-pop cgc (x86-rbx)) ;; vector
        (x86-mov cgc (x86-mem (- (+ 16 (* idx 8)) TAG_MEMOBJ) (x86-rbx)) (x86-rax))
        (x86-push cgc (x86-rbx)) ;; TODO + Pas besoin des deux si l'élément est un literal ?
        (if (= idx (- (vector-length ast) 1))
           (jump-to-version cgc
                            succ
                            (ctx-pop ctx))
           (jump-to-version cgc
                            (lazy-el-gen (+ idx 1))
                            (ctx-pop ctx))))))
    
  ;; Generate lazy-object which gen and push the value
  ;; at vector[idx].
  (define (lazy-el-gen idx)
    (gen-ast (vector-ref ast idx)
             (lazy-vector-set-gen idx)))                  
  
  ;; Main lazy code  
  (make-lazy-code
    (lambda (cgc ctx)
      (let ((header-word (mem-header (+ 2 (vector-length ast)) STAG_VECTOR)))
        ;; Alloc
        (gen-allocation cgc ctx STAG_VECTOR (+ (vector-length ast) 2))
        ;; Write header
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem (- (* -8 (vector-length ast)) 16) alloc-ptr) (x86-rax))
        ;; Write length
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (vector-length ast))))
        (x86-mov cgc (x86-mem (- (* -8 (vector-length ast)) 8) alloc-ptr) (x86-rax))
        ;; Push vector
        (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ (* 8 (+ (vector-length ast) 2))) alloc-ptr))
        (x86-push cgc (x86-rax))
        (jump-to-version cgc
                         (lazy-el-gen 0)
                         (ctx-push ctx CTX_VECT))))))

;;
;; Make lazy code from string literal
;;
(define (mlc-string ast succ symbol?)
  (make-lazy-code
    (lambda (cgc ctx)
      (let* ((len (string-length ast))
             (size (arithmetic-shift (bitwise-and (+ len 8) (bitwise-not 7)) -3))
             (header-word (mem-header (+ size 2) (if symbol?
                                                     STAG_SYMBOL
                                                     STAG_STRING))))
        
        (gen-allocation cgc ctx (if symbol? STAG_SYMBOL STAG_STRING) (+ size 2))
        
        ;; Write header
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem (- (* -8 size) 16) alloc-ptr) (x86-rax))
        ;; Write length
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding (string-length ast))))
        (x86-mov cgc (x86-mem (- (* -8 size) 8) alloc-ptr) (x86-rax)) 
        ;; Write chars
        (write-chars cgc ast 0 (* -8 size))
        ;; Push string
        (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ (* 8 (+ size 2))) alloc-ptr))
        (x86-push cgc (x86-rax))
        (jump-to-version cgc succ (ctx-push ctx (if symbol? CTX_SYM CTX_STR)))))))
      
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

;;
;; Make lazy code from QUOTE
;;
(define (mlc-quote ast succ)
  (cond ((pair? ast)
         (let* ((lazy-pair (mlc-pair succ))
                (lazy-cdr  (mlc-quote (cdr ast) lazy-pair)))
           (mlc-quote (car ast) lazy-cdr)))
        ((symbol? ast)
            (mlc-symbol ast succ))
        ((vector? ast)
            (mlc-vector ast succ))            
        (else (gen-ast ast succ))))

;;-----------------------------------------------------------------------------

;;
;; Make lazy code from SET!
;;
(define (mlc-set! ast succ)
  (let* ((id (cadr ast))
         (lazy-set
            (make-lazy-code
               (lambda (cgc ctx)
                  (let ((glookup-res (assoc id globals)))
                     (if glookup-res
                        ;; Global var
                        (gen-set-globalvar cgc ctx glookup-res)
                           (let ((res (assoc id (ctx-env ctx))))
                              (if res
                                 (if (eq? (identifier-type (cdr res)) 'free)
                                    (gen-set-freevar  cgc ctx res)  ;; Free var
                                    (gen-set-localvar cgc ctx res)) ;; Local var
                                 (error "Can't find variable: " id)))))
                  
                  (x86-push cgc (x86-imm-int ENCODING_VOID))
                  (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID))))))

     (gen-ast (caddr ast) lazy-set)))

;;
;; Make lazy code from SYMBOL
;;
(define (mlc-identifier ast succ)
  (make-lazy-code
    (lambda (cgc ctx)
      ;; Lookup in local env
      (let* ((res (assoc ast (ctx-env ctx)))
             (ctx-type (if res
                          (if (eq? (identifier-type (cdr res)) 'free)
                             ;; Free var
                             (gen-get-freevar  cgc ctx res 'stack #f)
                             ;; Local var
                             (gen-get-localvar cgc ctx res 'stack #f))
                          (let ((res (assoc ast globals)))
                             (if res
                                ;; Global var
                                (gen-get-globalvar cgc ctx res 'stack)
                                ;; Unknown
                                (error "Can't find variable: " ast))))))
           
           (jump-to-version cgc succ (ctx-push ctx ctx-type))))))

;;
;; Make lazy code from DEFINE
;;
(define (mlc-define ast succ)
  (let* ((identifier (cadr ast))
         (lazy-bind (make-lazy-code (lambda (cgc ctx)
                                     (x86-pop cgc (x86-rax))
                                     (let* ((res (assoc identifier globals)) ;; Lookup in globals
                                            (pos (cdr res)))                 ;; Get global pos
                                                   
                                       (x86-mov cgc (x86-mem (* 8 pos) (x86-r10)) (x86-rax)))

                                     (x86-push cgc (x86-imm-int ENCODING_VOID))

                                     (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID)))))
         (lazy-val (gen-ast (caddr ast) lazy-bind)))

    (make-lazy-code (lambda (cgc ctx)
                      (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
                      (x86-mov cgc (x86-mem (* 8 (length globals)) (x86-r10)) (x86-rax))
                      (set! globals (cons (cons identifier (length globals)) globals))
                      (jump-to-version cgc lazy-val ctx)
                      ))))

;;-----------------------------------------------------------------------------
;; SPECIAL

;;
;; Make lazy code from SPECIAL FORM (called specials)
;;
(define (mlc-special-c ast succ)
  (let* ((name (car ast))
         (label (cond ((eq? name '$$putchar) label-$$putchar)
                      (else "NYI special")))
         (lazy-special (make-lazy-code
                         (lambda (cgc ctx)
                           
                           (x86-call cgc label)
                           (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID))))))
    (if (> (length (cdr ast)) 0)
        (gen-ast-l (cdr ast) lazy-special)
        lazy-special)))

;;
;; Make lazy code from SPECIAL FORM (inlined specials)
;;
(define (mlc-special-nc ast succ)
  (let* ((special (car ast))
         (lazy-special
           (cond ;; ERROR
                 ((eq? special '$error)
                    (make-lazy-code
                      (lambda (cgc ctx)
                         (gen-error cgc ""))))
                 ;; CONS
                 ((eq? special '$cons) (mlc-pair succ))
                 ;; CAR & CDR
                 ((member special '($car $cdr))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((offset
                              (if (eq? special '$car)
                                  (-  8 TAG_MEMOBJ)
                                  (- 16 TAG_MEMOBJ))))
                        (x86-pop cgc (x86-rax))
                        (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
                        (x86-push cgc (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_UNK))))))
                 ;; SET-CAR! & SET-CDR!
                 ((member special '($set-car! $set-cdr!))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((offset
                              (if (eq? special '$set-car!)
                                  (-  8 TAG_MEMOBJ)
                                  (- 16 TAG_MEMOBJ))))
                        (x86-pop cgc (x86-rax)) ;; val
                        (x86-pop cgc (x86-rbx)) ;; pair
                        (x86-mov cgc (x86-mem offset (x86-rbx)) (x86-rax))
                        (x86-push cgc (x86-rbx))
                        (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_PAI))))))
                 ;; CLOSE-INPUT-PORT
                 ((member special '($close-output-port $close-input-port))
                   (make-lazy-code
                     (lambda (cgc ctx)
                       (gen-syscall-close cgc)
                       (x86-push cgc (x86-imm-int ENCODING_VOID))
                       (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VOID)))))
                 ;; OPEN-INPUT-FILE
                 ;; TODO output port STAG ET CTX
                 ((member special '($open-output-file $open-input-file))
                   (make-lazy-code
                     (lambda (cgc ctx)
                       (let* ((direction   (if (eq? special '$open-output-file) 'out 'in))
                              (stag        (if (eq? direction 'in) STAG_IPORT STAG_OPORT))
                              (header-word (mem-header 2 stag)))
                         ;; Gen 'open' syscall, file descriptor in rax
                         (gen-syscall-open cgc direction)
                         (x86-mov cgc (x86-rbx) (x86-rax))
                         ;; Allocate port object
                         (gen-allocation cgc ctx stag 2)
                         ;; Mov header
                         (x86-mov cgc (x86-rax) (x86-imm-int header-word))
                         (x86-mov cgc (x86-mem -16 alloc-ptr) (x86-rax))
                         ;; Mov descriptor
                         (x86-mov cgc (x86-mem  -8 alloc-ptr) (x86-rbx))
                         ;; Tag & push
                         (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))
                         (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                         ;; Jump to succ
                         (jump-to-version cgc succ (ctx-push (ctx-pop ctx) (if (eq? direction 'in) CTX_IPORT CTX_OPORT)))))))
                 ;; EOF-OBJECT?
                 ((eq? special '$eof-object?)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((label-end (asm-make-label #f (new-sym 'label-end))))
                        (x86-pop cgc (x86-rax))
                        (x86-cmp cgc (x86-rax) (x86-imm-int ENCODING_EOF))
                        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                        (x86-jne cgc label-end)
                          (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                        (x86-label cgc label-end)
                        (x86-push cgc (x86-rax))
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL))))))
                 ;; READ-CHAR
                 ((eq? special '$read-char)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      ;; Gen 'read' syscall (read 1 byte), encoded value (char or eof) in rax
                      (gen-syscall-read-char cgc)
                      ;; Push encoded result
                      (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))
                      ;; Jump to succ
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_CHAR)))))
                 ;; WRITE-CHAR
                 ((eq? special '$write-char)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      ;; Gen 'read' syscall (read 1 byte), encoded value (char or eof) in rax
                      (gen-syscall-write-char cgc)
                      (x86-add cgc (x86-rsp) (x86-imm-int 16)) ;; TODO
                      ;; Push encoded result
                      (x86-push cgc (x86-imm-int ENCODING_VOID))
                      ;; Jump to succ
                      (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_VOID)))))
                 ;; CHAR<->INTEGER
                 ((member special '($char->integer $integer->char))
                  (let ((lazy-charint
                          (make-lazy-code
                            (lambda (cgc ctx)
                              (if (eq? special '$char->integer)
                                  (x86-xor cgc (x86-mem 0 (x86-rsp)) (x86-imm-int TAG_SPECIAL) 8)
                                  (x86-or  cgc (x86-mem 0 (x86-rsp)) (x86-imm-int TAG_SPECIAL) 8))
                              (jump-to-version cgc succ (ctx-push (ctx-pop ctx)
                                                                  (cond ((eq? special '$char->integer) CTX_NUM)
                                                                        ((eq? special '$integer->char) CTX_CHAR))))))))
                    (gen-fatal-type-test (if (eq? special '$char->integer)
                                            CTX_CHAR
                                            CTX_NUM)
                                         0
                                         lazy-charint)))
                 ;; MAKE-STRING
                 ((eq? special '$make-string)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let* ((header-word (mem-header 3 STAG_STRING)))
                        
                        (x86-pop cgc (x86-rax)) ;; Pop encoded length
                        (x86-mov cgc (x86-rbx) (x86-rax))
                        
                        ;; Nb chars to byte size
                        (x86-shr cgc (x86-rax) (x86-imm-int 2))
                        (x86-and cgc (x86-rax) (x86-imm-int (bitwise-not 7)))
                        (x86-shr cgc (x86-rax) (x86-imm-int 1))
                        (x86-mov cgc (x86-rdx) (x86-rax))
                        
                        ;; Alloc
                        (gen-allocation cgc ctx STAG_STRING 3 #t)
                        
                        ;; Get string position in RCX
                        (x86-mov cgc (x86-r15) (x86-rdx))
                        (x86-shl cgc (x86-r15) (x86-imm-int 1))
                        (x86-neg cgc (x86-r15))
                        (x86-add cgc (x86-r15) alloc-ptr)
                        (x86-sub cgc (x86-r15) (x86-imm-int 24))
                        
                        ;; Fill string
                        (x86-push cgc (x86-rbx))
                        
                        ;; RCX contient la position du vecteur
                        (x86-mov cgc (x86-rax) (x86-r15))
                        
                        (let ((label-loop (asm-make-label cgc (new-sym 'fill-vector-loop)))
                              (label-end  (asm-make-label cgc (new-sym 'fill-vector-end))))
                        
                        ;; LOOP:
                        ;;   if (rbx == 0) jump END
                        (x86-label cgc label-loop)
                        (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
                        (x86-jle  cgc label-end)
                        
                          ;; Init string slot
                          (x86-mov cgc (x86-mem 16 (x86-rax)) (x86-imm-int 0) 64) ;; Write 0 in 8 chars
                          ;; Update offset and remaining elements nb
                          (x86-add cgc (x86-rax) (x86-imm-int 8))
                          ;; Loop
                          (x86-sub cgc (x86-rbx) (x86-imm-int 32)) ;; Remove 8 to encoded number (=8*4=32)
                          (x86-jmp cgc label-loop)
                        
                        ;; END:
                        (x86-label cgc label-end)
                        (x86-pop cgc (x86-rbx)))  
                        
                        ;; Write encoded length
                        (x86-mov cgc (x86-mem 8 (x86-r15)) (x86-rbx))
                        
                        ;; Write header
                        (x86-shl cgc (x86-rdx) (x86-imm-int 6))
                        (x86-add cgc (x86-rdx) (x86-imm-int header-word))
                        (x86-mov cgc (x86-mem 0 (x86-r15)) (x86-rdx))
                        
                        ;; Push vector
                        (x86-add cgc (x86-r15) (x86-imm-int TAG_MEMOBJ))
                        (x86-push cgc (x86-r15))
                        
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_STR))))))
                        
                        
                 ;; MAKE-VECTOR
                 ((eq? special '$make-vector)
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let* ((header-word (mem-header 2 STAG_VECTOR))
                             (header-reg (x86-rbx)))
                        
                        (x86-pop cgc (x86-rax)) ;; Pop encoded length
                        (x86-mov cgc (x86-rbx) (x86-rax))
                        
                        ;; Alloc
                        (gen-allocation cgc ctx STAG_VECTOR 2 #t)
                        
                        ;; Get vector position in RCX
                        (x86-mov cgc (x86-rax) (x86-rbx))
                        (x86-shl cgc (x86-rax) (x86-imm-int 1))
                        (x86-add cgc (x86-rax) (x86-imm-int 16))
                        (x86-mov cgc (x86-r15) alloc-ptr)
                        (x86-sub cgc (x86-r15) (x86-rax))
                        
                        ;; Fill vector
                        (x86-push cgc (x86-r15))
                        (x86-push cgc (x86-rbx))
                        
                          ;; Init value in RAX (0)
                          (x86-mov cgc (x86-rax) (x86-imm-int 0))
                          
                          (let ((label-loop (asm-make-label cgc (new-sym 'fill-vector-loop)))
                                (label-end  (asm-make-label cgc (new-sym 'fill-vector-end))))
                          
                          ;; LOOP:
                          ;;    if (rbx == 0) jump END
                          (x86-label cgc label-loop)
                          (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
                          (x86-je  cgc label-end)
                          
                            ;; Init vector slot
                            (x86-mov cgc (x86-mem 16 (x86-r15)) (x86-rax))
                            ;; Update offset and remaining elements nb
                            (x86-add cgc (x86-r15) (x86-imm-int 8))
                            (x86-sub cgc (x86-rbx) (x86-imm-int 4))
                            ;; loop
                            (x86-jmp cgc label-loop)
                        
                        ;; END:  
                        (x86-label cgc label-end)
                        (x86-pop cgc (x86-rbx))
                        (x86-pop cgc (x86-r15)))
                        
                        ;; Write encoded length
                        (x86-mov cgc (x86-mem 8 (x86-r15)) (x86-rbx))
                        
                        ;; Write header
                        (x86-shl cgc (x86-rbx) (x86-imm-int 6))
                        (x86-add cgc (x86-rbx) (x86-imm-int header-word))
                        (x86-mov cgc (x86-mem 0 (x86-r15)) (x86-rbx))
                        
                        ;; Push vector
                        (x86-add cgc (x86-r15) (x86-imm-int TAG_MEMOBJ))
                        (x86-push cgc (x86-r15))
                        
                        ;;
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VECT))))))
                 
                 ;; STRING<->SYMBOL
                 ;; TODO : tags
                 ((member special '($string->symbol $symbol->string))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      
                      ;; Alloc
                      (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                      (x86-sub cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                      (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rax)))
                      (x86-shr cgc (x86-rax) (x86-imm-int 8))
                      (x86-shl cgc (x86-rax) (x86-imm-int 2))
                      (x86-mov cgc (x86-rbx) (x86-rax))
                      (gen-allocation cgc ctx STAG_SYMBOL 0 #t)
                
                      ;; Symbol address in rbx
                      (x86-shl cgc (x86-rbx) (x86-imm-int 1))
                      (x86-neg cgc (x86-rbx))
                      (x86-add cgc (x86-rbx) alloc-ptr)
                      
                      ;; String address in rax
                      (x86-pop cgc (x86-rax))
                      (x86-sub cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                      
                      ;; Mov length in symbol
                      (x86-mov cgc (x86-r15) (x86-mem 8 (x86-rax)))
                      (x86-mov cgc (x86-mem 8 (x86-rbx)) (x86-r15))
                      
                      ;; Mov header in symbol
                      (x86-mov cgc (x86-r15) (x86-mem 0 (x86-rax)))
                      (if (eq? special '$string->symbol)
                         (x86-sub cgc (x86-r15) (x86-imm-int (arithmetic-shift (- STAG_STRING STAG_SYMBOL) 3)))
                         (x86-add cgc (x86-r15) (x86-imm-int (arithmetic-shift (- STAG_STRING STAG_SYMBOL) 3))))
                      (x86-mov cgc (x86-mem 0 (x86-rbx)) (x86-r15))
                      
                      ;; Encoded length in r15
                      (x86-shr cgc (x86-r15) (x86-imm-int 8))
                      (x86-shl cgc (x86-r15) (x86-imm-int 3))
                                            
                      ;; If encoded length == 16
                      ;;    jump label-fin
                      (let ((label-loop (asm-make-label cgc (new-sym 'label-loop)))
                            (label-fin  (asm-make-label cgc (new-sym 'label-fin))))
                        
                        (x86-label cgc label-loop)
                        (x86-cmp cgc (x86-r15) (x86-imm-int 16))
                        (x86-jle cgc label-fin)
                        
                          (x86-mov cgc (x86-rdx) (x86-mem -8 (x86-r15) (x86-rax)))
                          (x86-mov cgc (x86-mem -8 (x86-r15) (x86-rbx)) (x86-rdx))
                          (x86-sub cgc (x86-r15) (x86-imm-int 8))
                          (x86-jmp cgc label-loop)
                      
                        (x86-label cgc label-fin))
                        
                      (x86-add cgc (x86-rbx) (x86-imm-int TAG_MEMOBJ))
                      (x86-push cgc (x86-rbx))
                      
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_SYM)))))
                      
                      
                      
                 
                 
                 ;; VECTOR-LENGTH & STRING-LENGTH
                 ((member special '($vector-length $string-length))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (x86-pop cgc (x86-rax)) ;; Pop vector
                      (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))
                      (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_NUM)))))
                 
                 ;; VECTOR-REF & STRING-REF
                 ((member special '($vector-ref $string-ref))
                  (make-lazy-code
                    (lambda (cgc ctx)
                      (let ((vr? (eq? special '$vector-ref)))
                        (x86-pop cgc (x86-rax)) ;; Pop index
                        (x86-pop cgc (x86-rbx)) ;; Pop vector
                        (cond (vr?
                                (x86-shl cgc (x86-rax) (x86-imm-int 1))
                                (x86-add cgc (x86-rbx) (x86-rax))
                                (x86-push cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx)))
                                (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_UNK)))
                              ((not vr?)
                               (x86-shr cgc (x86-rax) (x86-imm-int 2)) ;; Decode position
                               (x86-mov cgc (x86-al) (x86-mem (- 16 TAG_MEMOBJ) (x86-rax) (x86-rbx))) ;; Get Char
                               (x86-and cgc (x86-rax) (x86-imm-int 255)) ;; Clear bits before al
                               (x86-shl cgc (x86-rax) (x86-imm-int 2)) ;; Encode char
                               (x86-add cgc (x86-rax) (x86-imm-int TAG_SPECIAL))
                               (x86-push cgc (x86-rax)) ;; Push char
                               (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_CHAR))))))))                         
                        
                 
                 ;; VECTOR-SET! & STRING-SET!
                 ((member special '($vector-set! $string-set!))
                  (make-lazy-code
                    (lambda (cgc ctx)
                        (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))  ;; Get index
                        (x86-mov cgc (x86-rbx) (x86-mem 16 (x86-rsp))) ;; Get vector
                        (x86-mov cgc (x86-rdx) (x86-mem 0 (x86-rsp)))  ;; Get new value
                        
                        (cond ((eq? special '$vector-set!)
                                (x86-shl cgc (x86-rax) (x86-imm-int 1))
                                (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx) (x86-rax)) (x86-rdx)))
                              
                              (else
                                (x86-shr cgc (x86-rdx) (x86-imm-int 2))
                                (x86-shr cgc (x86-rax) (x86-imm-int 2))
                                (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx) (x86-rax)) (x86-dl))))
                        
                        (x86-add cgc (x86-rsp) (x86-imm-int 24))
                        (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
                        (x86-push cgc (x86-rax))
                        
                        (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 3) CTX_VOID)))))
                 
                 ;; OTHERS
                 (else (error "NYI")))))
    
    ;; Build lazy objects chain
    (cond ;; $error
          ((eq? special '$error)
             lazy-special)
          ;; $cons, $string-ref, $vector-ref, $set-car!, $set-cdr!, $write-char
          ((member special '($cons $string-ref $vector-ref $set-car! $set-cdr! $write-char))
           (let ((lazy-right (gen-ast (caddr ast) lazy-special)))
             (gen-ast (cadr ast) lazy-right)))
          ;; $car, $cdr, $make-vector, $string->symbol, $symbol->string, ...
          ((member special '($eof-object? $read-char $close-output-port $close-input-port $open-output-file $open-input-file $symbol->string $string->symbol $string-length $integer->char $char->integer $vector-length $make-string $make-vector $car $cdr))
           (gen-ast (cadr ast) lazy-special))
          ;; $string-set!, $vector-set!
          ((member special '($string-set! $vector-set!))
           (let* ((lazy-value (gen-ast (cadddr ast) lazy-special))
                  (lazy-index (gen-ast (caddr  ast) lazy-value)))
             (gen-ast (cadr ast) lazy-index)))
          ;; Others
          (else (error "NYI")))))

;;
;; Make lazy code from LAMBDA
;;
(define (mlc-lambda ast succ)
  (let* (;; Lambda free vars
         (fvars #f)
         ;; Lambda mutable vars
         (mvars #f)
         ;; Saved env
         (saved-env #f)
         ;; Rest param ?
         (rest-param (or (and (not (list? (cadr ast))) (not (pair? (cadr ast)))) ;; (foo . rest)
                         (and (pair? (cadr ast)) (not (list? (cadr ast)))))) ;; (foo a..z . rest)
         ;; Params list
         (params
           (if rest-param
              (formal-params (cadr ast))
              (cadr ast)))
         ;; Flatten list of param (include rest param)
         (all-params (flatten (cadr ast)))
         ;; Lazy lambda return
         (lazy-ret (make-lazy-code-ret ;; Lazy-code with 'ret flag
                     (lambda (cgc ctx)
                       ;; TODO : wrong ?
                       ;; Here the stack is :
                       ;;         RSP
                       ;;     | ret-val | ret-addr | closure | arg n | ... | arg 1 |
                       ;; Or if rest :
                       ;;     | ret-val | ret-addr | closure | rest | arg n | ... | arg 1 |
                       (let ((retval-offset
                                (if rest-param
                                   (* 8 (+ 2 (length params)))
                                   (* 8 (+ 1 (length params))))))
                         
                         ;; TODO
                         ;; Pop return value
                         (x86-pop  cgc (x86-rax))
                         ;; Swap return value (rax) and return address ([rsp+offset])
                         ;(x86-xchg cgc (x86-rax) (x86-mem retval-offset (x86-rsp)))
                         ;; Update SP to return value
                         (x86-add  cgc (x86-rsp) (x86-imm-int retval-offset))
                         ;; Jump to continuation
                         (x86-ret cgc)))))
         ;; Lazy lambda body
         (lazy-body (gen-ast (caddr ast) lazy-ret))
         ;; Lazy function prologue : creates rest param if any, transforms mutable vars, ...
         (lazy-prologue (make-lazy-code
                           (lambda (cgc ctx)
                              (let* ((actual-p (- (length (ctx-stack ctx)) (length base-ctx) 1)) ;; 1 for return address
                                     (formal-p (ctx-nb-args ctx)))

                                (if (or (and (not rest-param) (not (= actual-p formal-p)))
                                        (and rest-param (< actual-p formal-p)))
                                   ;; Wrong number of arguments
                                   (begin (pp ast) (gen-error cgc ERR_WRONG_NUM_ARGS))
                                   ;; Correct number of arguments
                                   (begin (cond ;; Rest param declared but not given
                                                ((and rest-param (= actual-p formal-p))
                                                    ;; Shift closure
                                                    (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                                                    (x86-push cgc (x86-rax))
                                                    ;; Mov '() in rest param slot
                                                    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                                                    (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-rax))
                                                    ;; Add type information to ctx
                                                    ;; TODO
                                                    (let* ((cstack (append base-ctx (list 'pair) (list-tail (ctx-stack ctx) (length base-ctx))))
                                                           (cnbargs (+ (ctx-nb-args ctx) 1))
                                                           (cenv (ctx-env ctx))
                                                           (cctx (make-ctx cstack cenv cnbargs)))
                                                     (set! ctx cctx)))
                                                    ;(ctx-stack-set! ctx (append base-ctx (list 'pair) (list-tail (ctx-stack ctx) (length base-ctx))))
                                                    ;(ctx-nb-args-set! ctx (+ (ctx-nb-args ctx) 1)))
                                                ;; Rest param declared and given
                                                ((and rest-param (> actual-p formal-p))
                                                 
                                                    (gen-rest-lst cgc ctx (- actual-p formal-p))
                                                    
                                                    ;; TODO
                                                    (let* ((cstack (append base-ctx '(pair) (list-tail (ctx-stack ctx) (- (length (ctx-stack ctx)) formal-p 1))))
                                                           (cnbargs (+ (ctx-nb-args ctx) 1))
                                                           (cenv (ctx-env ctx))
                                                           (cctx (make-ctx cstack cenv cnbargs)))
                                                      (set! ctx cctx))
                                                    ;(ctx-stack-set! ctx (append base-ctx '(pair) (list-tail (ctx-stack ctx) (- (length (ctx-stack ctx)) formal-p 1)))) ;; 1 for return address ;; TODO : write context one time
                                                    ;(ctx-nb-args-set! ctx (+ (ctx-nb-args ctx) 1))
                                                    ))
                                          
                                          (gen-mutable cgc ctx mvars)
                                          (jump-to-version cgc lazy-body ctx))))))))

    ;; Lazy closure generation
    (make-lazy-code
      (lambda (cgc ctx)
        
        (let* (;; Lambda stub
               (stub-labels (add-fn-callback cgc
                                             0
                                             (lambda (sp ctx ret-addr selector closure)
                                               
                                               ;; Extends env with params and free vars
                                               (let* ((env (build-env mvars all-params 0 (build-fenv saved-env mvars fvars 0)))
                                                      (ctx (make-ctx (ctx-stack ctx) env (length params))))
                                                 (gen-version-fn closure lazy-prologue ctx)))))
               (stub-addr (vector-ref (list-ref stub-labels 0) 1)))
          
          ;; SAVE ENVIRONMENT
          (set! saved-env (ctx-env ctx))
          ;; COMPUTE FREE VARS
          (set! fvars (free-vars (caddr ast) all-params ctx))
          ;; COMPUTE MUTABLE VARS
          (set! mvars (mutable-vars (caddr ast) all-params))
          
          (let* ((closure-size (+ 2 (length fvars)))
                 (total-size (+ closure-size global-cc-table-maxsize 1)) ;; CCtable header -> +1
                 (header-word (mem-header closure-size STAG_PROCEDURE)))
            
            ;; ALLOC
            (gen-allocation cgc ctx STAG_PROCEDURE total-size)
            
            ;; 1 - WRITE OBJECT HEADER
            (x86-mov cgc (x86-rax) (x86-imm-int header-word))
            (x86-mov cgc (x86-mem (* -8 total-size) alloc-ptr) (x86-rax))

            ;; 2 - WRITE CC TABLE LOCATION
            (x86-lea cgc (x86-rax) (x86-mem (- (* -8 global-cc-table-maxsize) 8) alloc-ptr)) ;; CCtable header -> -8
            (x86-mov cgc (x86-mem (+ 8 (* -8 total-size)) alloc-ptr) (x86-rax))

            ;; 3 - WRITE FREE VARS
            (gen-free-vars cgc fvars ctx (+ 16 (* -8 total-size)))

            ;; 4 - WRITE CC TABLE
            (let ((cc-header (mem-header (+ 1 global-cc-table-maxsize) STAG_CCTABLE)))
              (x86-mov cgc (x86-rax) (x86-imm-int cc-header))
              (x86-mov cgc (x86-mem (+ (* 8 closure-size) (* -8 total-size)) alloc-ptr) (x86-rax)))
            (gen-cc-table cgc stub-addr (+ 8 (* 8 closure-size) (* -8 total-size)))
              
            ;; TAG AND PUSH CLOSURE
            (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ (* 8 total-size)) alloc-ptr))
            (x86-push cgc (x86-rax)))

          ;; Jump to next
          (jump-to-version cgc
                           succ
                           (ctx-push ctx
                                     CTX_CLO)))))))

;;
;; Make lazy code from BEGIN
;; 
(define (mlc-begin ast succ)
  (cond ;; There is no body
        ((null? (cdr ast))
           (if (member 'ret (lazy-code-flags succ))
             ;; No body and succ is a ret object
             (error ERR_BEGIN)
             ;; No body and succ is NOT a ret object
             (make-lazy-code
               (lambda (cgc ctx)
                 (x86-push cgc (x86-imm-int ENCODING_VOID))
                 (jump-to-version cgc succ (ctx-push ctx CTX_VOID))))))
        ;; Only one body
        ((= (length (cdr ast)) 1)
           (gen-ast (cadr ast) succ))
        ;; >1 body
        (else
           (let (;; LAZY BEGIN OUT
                 (lazy-begin-out
                  (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                          make-lazy-code-ret
                          make-lazy-code)))
                    (make-lc
                      (lambda (cgc ctx)
                        (let ((ctx-type (car (ctx-stack ctx))))
                          (x86-pop  cgc (x86-rax))
                          (x86-add  cgc (x86-rsp) (x86-imm-int (* 8 (- (length (cdr ast)) 1))))
                          (x86-push cgc (x86-rax))
                          (jump-to-version cgc
                                           succ
                                           (ctx-push (ctx-pop-nb ctx (length (cdr ast))) ctx-type))))))))
             ;; LAZY BODIES
             (gen-ast-l (cdr ast) lazy-begin-out)))))

;;-----------------------------------------------------------------------------
;; Bdingings (let, letrec, let*)

;; TODO: Implement #!unbound ?
;; NOTE: Letrec: All ids are considered as mutable. Analysis to detect recursive use of ids?

;; Entry point to compile let, letrec
(define (mlc-binding ast succ op)
  (let ((ids (map car (cadr ast)))
        (bodies (cddr ast)))
    
    (cond ;; No bindings, it is a begin
          ((null? ids) (mlc-begin (cons 'begin bodies) succ))
          ;; No body, error
          ((null? bodies) (error (cond ((eq? op 'let)    ERR_LET)
                                       ((eq? op 'letrec) ERR_LETREC)
                                       ((eq? op 'let*)   ERR_LET*))))
          ;; >= 1 body
          (else
            (let* (;; LAZY LET OUT
                   ;; Clean ctx-stack and env, update rsp
                   (lazy-let-out
                     (let ((make-lc (if (member 'ret (lazy-code-flags succ))
                                      make-lazy-code-ret
                                      make-lazy-code)))
                       (make-lc ;; If succ is a ret object, then last object of begin is also a ret object
                         (lambda (cgc ctx)
                           (let* ((ctx-type (car (ctx-stack ctx)))
                                  (stack (ctx-stack (ctx-push (ctx-pop-nb ctx (+ (length ids) (length bodies))) ctx-type)))
                                  (env (list-tail (ctx-env ctx) (length ids)))
                                  (nb-args (ctx-nb-args ctx))
                                  (nctx (make-ctx stack env nb-args)))
                           
                            (x86-pop cgc (x86-rax))
                            (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (+ (length ids) (length bodies) -1))))
                            (x86-push cgc (x86-rax))
                            (jump-to-version cgc succ nctx))))))
                   ;; LAZY BODIES
                   (lazy-bodies (gen-ast-l bodies lazy-let-out)))
            
            ;; Delegate with bodies as successor
            (cond ((eq? op 'let)    (mlc-let ast lazy-bodies))
                  ((eq? op 'letrec) (mlc-letrec ast lazy-bodies))
                  ((eq? op 'let*)   (mlc-let* ast lazy-bodies))
                  (else (error "Unknown ast"))))))))
   
;;
;; Make lazy code from LET*
;;
(define (mlc-let* ast succ)
  ;; Build lazy objects chain for let* bindings
  (define (gen-let*-bindings ids values mvars)
    (if (null? ids)
      succ
      (let ((lazy-bind
              (make-lazy-code
                (lambda (cgc ctx)
                  (let* ((start (- (length (ctx-stack ctx)) 2))
                         (env (build-env mvars (list (car ids)) start (ctx-env ctx)))
                         (nctx (make-ctx (ctx-stack ctx) env (ctx-nb-args ctx))))
                      ;; If this id is mutable then gen mobject
                      (if (member (car ids) mvars)
                        (gen-mutable cgc nctx (list (car ids))))
                      ;; Jump to next id (or succ) with new ctx
                      (jump-to-version cgc
                                       (gen-let*-bindings (cdr ids) (cdr values) mvars)
                                       nctx))))))
        ;; Gen value
        (gen-ast (car values) lazy-bind))))

  (let* ((ids (map car (cadr ast)))
         (values (map cadr (cadr ast)))
         (mvars (mutable-vars ast ids)))
    ;; Init call with all ids
    (gen-let*-bindings ids values mvars)))
          
;;
;; Make lazy code from LETREC
;;
(define (mlc-letrec ast succ)
  (let* ((ids (map car (cadr ast)))
         (values (map cadr (cadr ast)))
         ;; 3 - Bind values to their locations and jump to bodies
         (lazy-let-mid
            (make-lazy-code
               (lambda (cgc ctx)
                  (let ((ctx (gen-letrec-binds cgc ctx ids)))
                     (jump-to-version cgc succ ctx)))))
         ;; 2 - Gen all bound values
         (lazy-values (gen-ast-l values lazy-let-mid)))
    
    ;; 1 - Push initial values, Update env, ctx,
    ;;     gen mutable and jump to values 
    (make-lazy-code
       (lambda (cgc ctx)
          (let* ((stack (append (make-list (length ids) CTX_BOOL) (ctx-stack ctx)))
                 (start (- (length stack) (length ids) 1))
                 (env (build-env ids ids start (ctx-env ctx))) ;; All ids are considered as mutable
                 (nctx (make-ctx stack env (ctx-nb-args ctx))))
             ;; Create values on stack (initial value is #f)
             (call-n (length ids) x86-push cgc (x86-imm-int (obj-encoding #f)))
             (gen-mutable cgc nctx ids)
             (jump-to-version cgc lazy-values nctx))))))

;; Mov values to their locations (letrec bind)
(define (gen-letrec-binds cgc ctx all-ids)
  
  (define (gen-letrec-binds-h cgc ctx ids from to)
    (if (null? ids)
      ;; No more id, update rsp and return new ctx
      (begin (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (length all-ids))))
             (ctx-pop-nb ctx (length all-ids)))
      ;; Bind id
      (let* ((ctx-type (list-ref (ctx-stack ctx) from))
             (nstack (append (list-head (ctx-stack ctx) to) (cons ctx-type (list-tail (ctx-stack ctx) (+ to 1)))))
             (nctx (make-ctx nstack (ctx-env ctx) (ctx-nb-args ctx))))
        ;; Get val and mov to location
        (x86-mov cgc (x86-rax) (x86-mem (* 8 from) (x86-rsp)))
        (x86-mov cgc (x86-rbx) (x86-mem (* 8 to) (x86-rsp)))
        (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)) (x86-rax))
        (gen-letrec-binds-h cgc nctx (cdr ids) (+ from 1) (+ to 1)))))
  
  ;; Initial call
  (gen-letrec-binds-h cgc ctx all-ids 0 (length all-ids)))
 
;;
;; Make lazy code from LET*
;;
;; TODO ?

  
;;
;; Make lazy code from LET
;;
(define (mlc-let ast succ)
  (let* ((ids (map car (cadr ast)))
         (values (map cadr (cadr ast)))
         ;; 2 - Update env, ctx, gen mutable and jump to bodies
         (lazy-let-in
            (make-lazy-code
               (lambda (cgc ctx)
                  (let* ((mvars (mutable-vars ast ids))
                         (start (- (length (ctx-stack ctx)) (length ids) 1))
                         (env (build-env mvars ids start (ctx-env ctx)))
                         (nctx (make-ctx (ctx-stack ctx) env (ctx-nb-args ctx))))
                     ;; Gen mutable vars
                     (gen-mutable cgc nctx mvars)
                     ;; Jump to first body
                     (jump-to-version cgc succ nctx))))))
      ;; 1 - Gen all bound values
      (gen-ast-l values lazy-let-in)))

;;-----------------------------------------------------------------------------
;; Conditionals

;;
;; Make lazy code from IF
;;
(define (mlc-if ast succ)
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

                                (if verbose-jit
                                    (begin
                                      (println ">>> selector= " selector)
                                      (println ">>> prev-action= " prev-action)))

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

                                                (if verbose-jit (println ">>> swapping-branches"))

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

;;-----------------------------------------------------------------------------
;;
;; Make lazy code from DO
;;
;; NOTE: optimization: if <step> is <variable> it is useless to compile
;;       and move in stack, we can use the value of previous iteration
;;
;; Lazy objects chain :
;;
;; +-------+  +---------+  +------+  +----------+  +------------+  +-----+  +------+
;; | inits |->| ctx-add |->| test |->| dispatch |->| test-exprs |->| end |->| succ |-> ...
;; +-------+  +---------+  +------+  +----------+  +------------+  +-----+  +------+
;;                            ^           |
;;                            |           V
;;                            |       +--------+
;;                            |       | bodies |
;;                            |       +--------+
;;                            |           |
;;                            |           V
;;                         +------+   +-------+
;;                         | bind |<--| steps |
;;                         +------+   +-------+
(define (mlc-do ast succ)
  
  ;; Get list of steps
  (define (get-steps variables)
    (if (null? variables)
      '()
      (let ((variable (car variables)))
        (if (null? (cddr variable))
          ;; No step, use variable
          (cons (car variable)   (get-steps (cdr variables)))
          ;; Step exists, add step
          (cons (caddr variable) (get-steps (cdr variables)))))))
  
  (let* (;; DO components
         (test       (car (caddr ast)))
         (test-exprs (if (null? (cdr (caddr ast)))
                         '(#f)
                         (cdr (caddr ast))))
         (variables  (map car (cadr ast)))
         (inits      (map cadr (cadr ast)))
         (steps      (get-steps (cadr ast)))
         (bodies     (cdddr ast))
         (mvars      (mutable-vars ast variables))
         ;; LAZY-END
         (lazy-end
           ;; Last object. Executed if test evaluates to true
           ;; Update ctx and rsp
           (make-lazy-code
             (lambda (cgc ctx)
               
               (let* ((stack (cons (car (ctx-stack ctx)) (list-tail (ctx-stack ctx) (+ (length test-exprs) (length variables)))))
                      (env (list-tail (ctx-env ctx) (length variables)))
                      (nctx (make-ctx stack env (ctx-nb-args ctx))))
               (x86-pop cgc (x86-rax))
               (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (+ (length variables) (length test-exprs) -1))))
               (x86-push cgc (x86-rax))
               
               (jump-to-version cgc succ nctx)))))
         ;; LAZY-TEST
         (lazy-test #f) ;; do test
         ;; LAZY-BIND
         (lazy-bind
           (make-lazy-code
             (lambda (cgc ctx)
               ;; Update variables
               (do ((it   variables (cdr it))
                    (from (- (* 8 (length variables)) 8) (- from 8))
                    (to   (- (* 8 (+ (length variables) (length variables) (length bodies))) 8) (- to 8)))
                   ((null? it) #f)
                   (x86-mov cgc (x86-rax) (x86-mem from (x86-rsp)))
                   (if (member (car it) mvars)
                     (begin (x86-mov cgc (x86-rbx) (x86-mem to (x86-rsp))) ;; mvar box
                            (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)) (x86-rax)))
                     (x86-mov cgc (x86-mem to (x86-rsp)) (x86-rax))))
               
               ;; MAJ RSP
               (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (+ (length variables) (length bodies)))))
               
               (let* ((stack (append (list-head (ctx-stack ctx) (length variables))
                                     (list-tail (ctx-stack ctx) (+ (* 2 (length variables)) (length bodies)))))
                      (nctx (make-ctx stack (ctx-env ctx) (ctx-nb-args ctx))))
                 (jump-to-version cgc lazy-test nctx)))))
         ;; LAZY-STEP
         (lazy-steps
           (gen-ast-l steps lazy-bind))
         ;; LAZY-BODY
         (lazy-body
           (if (null? bodies)
              lazy-steps ;; Jump directly to lazy-steps if no body
              (gen-ast-l bodies lazy-steps)))
         ;; LAZY-TEST-EXPRS
         (lazy-test-exprs (gen-ast-l test-exprs lazy-end))
         ;; LAZY-DISPATCH: Read result of test and jump to test-exors if #t or body if #f
         (lazy-dispatch (get-lazy-dispatch lazy-test-exprs lazy-body #t #f))
         ;; LAZY-ADD-CTX
         (lazy-add-ctx
           ;; Add variables to env and gen mutable vars
           (make-lazy-code
             (lambda (cgc ctx)
               (let* ((start (- (length (ctx-stack ctx)) (length variables) 1))
                      (env  (build-env mvars variables start (ctx-env ctx)))
                      (nctx (make-ctx (ctx-stack ctx) env (ctx-nb-args ctx))))
                 ;; Gen mutable vars
                 (gen-mutable cgc nctx mvars)
                 
                 ;; Jump to test with new ctx
                 (jump-to-version cgc lazy-test nctx))))))
         
    ;; Lazy-dispatch exists then generate test ast
    (set! lazy-test (gen-ast test lazy-dispatch))
    
    ;; Return first init lazy object
    (gen-ast-l inits lazy-add-ctx)))

;; Create a new lazy code object.
;; Takes the value from stack and cmp to #f
;; If == #f jump to lazy-fail
;; If != #f jump to lazy-success
(define (get-lazy-dispatch lazy-success lazy-fail from-stack? cmp-val)

    (make-lazy-code
       (lambda (cgc ctx)
         
         (let* ((ctx-out (if from-stack?
                            (ctx-pop ctx)
                            ctx))
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
                                            (gen-version (+ jump-addr 6) lazy-success ctx-out)
                                          
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
                                                     lazy-fail
                                                     ctx-out))

                                              ;; make conditional jump to new version
                                              (gen-version jump-addr lazy-fail ctx-out))))

                                  (begin ;; one branch has already been patched
                                         ;; reclaim the stub
                                         (release-still-vector (get-scmobj ret-addr))
                                         (stub-reclaim stub-addr)
                                         (if (= selector 0)
                                            (gen-version (if (eq? prev-action 'swap) (+ jump-addr 6) jump-addr) lazy-fail ctx-out)
                                            (gen-version (if (eq? prev-action 'swap) jump-addr (+ jump-addr 6)) lazy-success ctx-out))))))))))

         (if from-stack?
          (x86-pop cgc (x86-rax)))
         (x86-cmp cgc (x86-rax) (x86-imm-int (obj-encoding cmp-val)))
         (x86-label cgc label-jump)
         (x86-je cgc (list-ref stub-labels 0))
         (x86-jmp cgc (list-ref stub-labels 1))))))

;;-----------------------------------------------------------------------------
;; APPLY & CALL
 
;;
;; Make lazy code from APPLY
;;
;; TODO : factoriser avec mlc-call
(define (mlc-apply ast succ)

  (let* (;; LAZY CALL
         (lazy-call
          (make-lazy-code
            ;; Remove apply op and lst, push args from lst,
            ;; push closure, and call
            (lambda (cgc ctx)
              
              ;; Remove lst and op from stack
              (x86-pop cgc (x86-rax)) ;; lst
              (x86-pop cgc (x86-rbx)) ;; op

              ;; Read and push all args from lst until we reach '()
              (let ((label-end  (asm-make-label #f (new-sym 'apply-args-end)))
                    (label-loop (asm-make-label #f (new-sym 'apply-args-loop))))
                ;; RDI contains the number of arguments
                (x86-mov cgc (x86-rdi) (x86-imm-int 0))
                (x86-label cgc label-loop)
                ;; If current el is null, then jump to end
                (x86-cmp cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                (x86-je cgc label-end)
                  ;; Else, push arg and update RDI
                  (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))           ;; Push car
                  (x86-mov cgc (x86-rax) (x86-mem (- 16 TAG_MEMOBJ) (x86-rax))) ;; Get cdr for next iteration
                  (x86-inc cgc (x86-rdi))  ;; inc args number
                  (x86-jmp cgc label-loop) ;; next iteration
                ;; All args are pushed
                (x86-label cgc label-end))

              ;; Push closure
              (x86-push cgc (x86-rbx))

              (let* ((call-ctx (make-ctx fake-stack '() -1))
                     (cct-offset    (* 8 (+ 1 (get-closure-index call-ctx)))))
                ;; Put ctx with fake stack in r11 because we don't know the type/number of arguments
                (x86-mov cgc (x86-r11) (x86-imm-int (ctx->still-ref call-ctx)))

                ;; 1 - Get cc-table
                (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)))

                ;; 2 - Get entry point in cc-table
                (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax)))

                ;; 3 - Jump
                (x86-jmp cgc (x86-rax))))))
        
         ;; LAZY APPLY
         (lazy-apply
           ;; Push operator and args lst 
           (let ((lazy-right (gen-ast (caddr ast) lazy-call)))
             (gen-ast (cadr ast) lazy-right))))
    
    ;; Create stub and push ret addr
    (make-lazy-code
      (lambda (cgc ctx)
        (let* (;; Flag in stub : is the continuation already generated ?
               (gen-flag #f)
               ;; Label for return address loading
               (load-ret-label (asm-make-label cgc (new-sym 'load-ret-addr)))
               ;; Continuation stub
               (stub-labels (add-callback cgc
                                          0
                                          (lambda (ret-addr selector)
                                              (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag = continuation addr
                                                 (set! gen-flag (gen-version-continuation load-ret-label
                                                                                          (make-lazy-code ;; TODO : move 
                                                                                            (lambda (cgc ctx)
                                                                                              (x86-push cgc (x86-rax))
                                                                                              (jump-to-version cgc succ (ctx-push ctx CTX_UNK))))
                                                                                          ctx)))
                                              gen-flag))))
          ;; Return address (continuation label)
          (x86-label cgc load-ret-label)
          (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1)))
          (x86-push cgc (x86-rax))

          (jump-to-version cgc lazy-apply (ctx-push ctx CTX_RETAD)))))))

;;
;; Make lazy code from CALL EXPR
;;
(define (mlc-call ast succ)
  
  (let* (;; Tail call if successor's flags set contains 'ret flag
         (tail (member 'ret (lazy-code-flags succ)))
         ;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc (ERR_TYPE_EXPECTED CTX_CLO)))))
         ;; Lazy call
         (lazy-call (make-lazy-code (lambda (cgc ctx)
                                        ;; Call ctx in rdx
                                        (let* ((call-stack    (if tail
                                                                (append base-ctx (list-head (cdr (ctx-stack ctx)) (length args)) (list CTX_RETAD))
                                                                (append base-ctx (list-head (cdr (ctx-stack ctx)) (+ (length args) 1))))) ;; CTX : list CTX_RETAD au lieu de +1 ?
                                               (call-ctx      (make-ctx call-stack '() -1))
                                               (cct-offset    (* 8 (+ 1 (get-closure-index call-ctx)))))

                                        (if tail 
                                          (tail-shift cgc
                                                      ;; Nb slots to shift
                                                      (+ (length args) 1) ;; +1 closure
                                                      ;; Initial from slot
                                                      (length args)
                                                      ;; Initial to slot
                                                      (- (length (ctx-stack ctx)) 2)))
                                        
                                        ;; 0 - R11 = still-box address containing call-ctx
                                        (x86-mov cgc (x86-r11) (x86-imm-int (ctx->still-ref call-ctx)))
                                        
                                        ;; 1 - Get cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))                ;; get closure
                                        (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))) ;; get cc-table

                                        ;; 2 - Get entry point in cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax)))
                                        
                                        ;; 3 - Jump to entry point
                                        (x86-jmp cgc (x86-rax))))))
         ;; Lazy main
         (lazy-main (gen-fatal-type-test CTX_CLO 0 lazy-call))
         ;; Lazy callee
         (lazy-operator (gen-ast (car ast) lazy-main)))

    ;; Build first lazy code
    ;; This lazy code creates continuation stub and push return address
    (if tail
        (if (> (length args) 0)
          ;; If args, then compile args
          (gen-ast-l args lazy-operator)
          ;; Else, compile call
          lazy-operator)
        (make-lazy-code
           (lambda (cgc ctx)
              (let* (;; Flag in stub : is the continuation already generated ?
                     (gen-flag #f)
                     ;; Label for return address loading
                     (load-ret-label (asm-make-label cgc (new-sym 'load-ret-addr)))
                     ;; Continuation stub
                     (stub-labels (add-callback cgc
                                                0
                                                (lambda (ret-addr selector)
                                                    (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag = continuation addr
                                                       (set! gen-flag (gen-version-continuation load-ret-label
                                                                                                (make-lazy-code ;; TODO : move 
                                                                                                  (lambda (cgc ctx)
                                                                                                    (x86-push cgc (x86-rax))
                                                                                                    (jump-to-version cgc succ (ctx-push ctx CTX_UNK))))
                                                                                                ctx)))
                                                     gen-flag))))
                 ;; Return address (continuation label)
                 (x86-label cgc load-ret-label)
                 (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1)))
                 (x86-push cgc (x86-rax))
                  
                 (let ((succ (if (> (length args) 0)
                                ;; If args, then compile args
                                (gen-ast-l args lazy-operator)
                                ;; Else, compile call
                                lazy-operator)))
                            
                    (jump-to-version cgc succ (ctx-push ctx CTX_RETAD)))))))))

;;-----------------------------------------------------------------------------
;; Operators

;; TODO : check if redefined
;; TODO : WIP
;; Will handle all operators with possibly multiple args
;; The other function will only handle fixed args operators
(define (mlc-op-numn ast succ op)
  
  (let* (;; Operands number
         (nb-opnd (length (cdr ast)))
         ;; Overflow stub
         (stub-labels (add-callback #f 0 (lambda (ret-addr selector)
                                            (error ERR_ARR_OVERFLOW))))
         ;; Lazy operation
         (lazy-op
           (make-lazy-code
             (lambda (cgc ctx)
              
               ;; NOTE: Optimization: if only 2 opnds we can write :
               ;;       pop rax
               ;;       sub [rsp+0], rax
               ;; -> less instructions, and no rsp update
               ;; Compute substraction with operands from stack
               (define (compute- offset nb total)
                 (if (= nb 0)
                     (x86-mov cgc (x86-mem (* 8 (- total 1)) (x86-rsp)) (x86-rax))
                     (begin (x86-sub cgc (x86-rax) (x86-mem offset (x86-rsp)))
                            (x86-jo cgc (list-ref stub-labels 0))
                            (compute- (- offset 8) (- nb 1) total))))
              
               ;; Compute addition with operands from stack
               (define (compute+ offset nb)
                 (if (= nb 0)
                   (begin (x86-add cgc (x86-mem offset (x86-rsp)) (x86-rax))
                          (x86-jo cgc (list-ref stub-labels 0)))
                   (begin (x86-add cgc (x86-rax) (x86-mem offset (x86-rsp)))
                          (x86-jo cgc (list-ref stub-labels 0))
                          (compute+ (+ offset 8) (- nb 1)))))
               
               ;; Compute multiplication with operands from stack
               (define (compute* offset nb)
                 (if (= nb 0)
                   (begin (x86-sar cgc (x86-rax) (x86-imm-int 2))
                          (x86-imul cgc (x86-rax) (x86-mem offset (x86-rsp)))
                          (x86-jo cgc (list-ref stub-labels 0))
                          (x86-mov cgc (x86-mem offset (x86-rsp)) (x86-rax)))                          
                   (begin (x86-sar cgc (x86-rax) (x86-imm-int 2))
                          (x86-imul cgc (x86-rax) (x86-mem offset (x86-rsp)))
                          (x86-jo cgc (list-ref stub-labels 0))
                          (compute* (+ offset 8) (- nb 1)))))
               
               (cond ((eq? op '+)
                         (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp))) ;; RAX = firsts
                         (compute+ 8 (- nb-opnd 2)))
                     ((eq? op '-)
                         (x86-mov cgc (x86-rax) (x86-mem (* 8 (- nb-opnd 1)) (x86-rsp))) ;; RAX = first opnd
                         (compute- (* 8 (- nb-opnd 2)) (- nb-opnd 1) nb-opnd))
                     ((eq? op '*)
                         (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                         (compute* 8 (- nb-opnd 2))))
               
               ;; Update RSP
               (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- nb-opnd 1))))
               ;; Jump to succ
               (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx nb-opnd) CTX_NUM))))))
    
    ;; Build lazy objects chain :
    ;;   opnd1->test1->...->opndn->testn->succ
    ;; Opnd is the operands list
    (define (build-chain opnds succ)
      (if (null? opnds)
        succ
        (let* ((next (build-chain (cdr opnds) succ))
               (test (gen-fatal-type-test CTX_NUM 0 next)))
          (gen-ast (car opnds) test))))
    
    (cond ;; No opnd, push 0 and jump to succ
          ((= (length (cdr ast)) 0)
             (cond ((eq? op '+) (gen-ast 0 succ))
                   ((eq? op '-) (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_WRONG_NUM_ARGS))))
                   ((eq? op '*) (gen-ast 1 succ))))
          ;; 1 opnd,  push opnd, test type, and jump to succ
          ((= (length (cdr ast)) 1)
             (cond ((eq? op '+) (build-chain (cdr ast) succ))
                   ((eq? op '-) (gen-ast (list '$* -1 (cadr ast)) succ)) ;; TODO $* until * is supported
                   ((eq? op '*) (build-chain (cdr ast) succ))))
          ;; >1 opnd, build chain
          (else (build-chain (cdr ast) lazy-op)))))

;;
;; Make lazy code from NUMBER OPERATOR
;;
(define (mlc-op-num ast succ op)
  (letrec (   ;; Lazy code used if type test fail
              (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc (ERR_TYPE_EXPECTED CTX_NUM)))))
              ;; Lazy code of left operand
              (lazy-ast-left  (gen-ast (cadr ast)  lazy-ast-right))
              ;; Lazy code of right operand
              (lazy-ast-right (gen-ast (caddr ast) lazy-main))
              ;; Gen operation code (assumes both operands are num)
              (lazy-code-op (make-lazy-code
                              (lambda (cgc ctx)

                                ;; Arithmetic overflow
                                (let* ((ctx-overflow (ctx-pop ctx)) ;; First operand pop
                                       (lazy-overflow (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_ARR_OVERFLOW))))
                                       (label-jo (asm-make-label #f (new-sym 'jump-overflow)))
                                       ;; TODO : new kind of stub (add-callback without ret-addr and without selector?)
                                       ;; TODO : overflow stub could be global
                                       (stub-labels (add-callback cgc
                                                                  0
                                                                  (lambda (ret-addr selector)
                                                                    (gen-version (vector-ref label-jo 1) lazy-overflow ctx-overflow)))))

                                (x86-pop cgc (x86-rbx))
                                (case op
                                  (($+)
                                   (begin
                                     (x86-add cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  (($-)
                                   (begin
                                     (x86-sub cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  (($*)
                                   (begin (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                     (x86-imul cgc (x86-rbx) (x86-mem 0 (x86-rsp)))
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-label cgc label-jo)
                                     (x86-jo cgc (list-ref stub-labels 0))))
                                  (($quotient) ;; TODO : check '/0'
                                   (begin
                                          (x86-pop cgc (x86-rax))
                                          (x86-sar cgc (x86-rax) (x86-imm-int 2))
                                          (x86-cqo cgc)
                                          (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                          (x86-idiv cgc (x86-rbx))
                                          (x86-shl cgc (x86-rax) (x86-imm-int 2))
                                          (x86-push cgc (x86-rax))
                                          (x86-label cgc label-jo)
                                          (x86-jo cgc (list-ref stub-labels 0))))
                                  (($modulo) ;; TODO : check '/0'
                                   (begin
                                          (x86-pop cgc (x86-rax))
                                          (x86-sar cgc (x86-rbx) (x86-imm-int 2))
                                          (x86-sar cgc (x86-rax) (x86-imm-int 2))
                                          (x86-cqo cgc)
                                          (x86-idiv cgc (x86-rbx))
                                          (x86-mov cgc (x86-rax) (x86-rdx)) ;; (a%b) in rax, b in rbx
                                          (x86-add cgc (x86-rax) (x86-rbx)) ;; (a%b + b) in rax
                                          (x86-cqo cgc)
                                          (x86-idiv cgc (x86-rbx))
                                          (x86-shl cgc (x86-rdx) (x86-imm-int 2))
                                          (x86-push cgc (x86-rdx))))
                                  (($<)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-jl  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  (($>)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-jg  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  (($=)
                                   (let ((label-done
                                           (asm-make-label cgc (new-sym 'done))))
                                     (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                                     (x86-je  cgc label-done)
                                     (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                                     (x86-label cgc label-done)
                                     (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                                  (else
                                    (error "unknown op" op)))
                                (jump-to-version cgc
                                                 succ
                                                 (ctx-push
                                                   (ctx-pop (ctx-pop ctx))
                                                   (cond ((member op '($+ $- $* $modulo $quotient)) CTX_NUM)
                                                         ((member op '($< $> $=)) CTX_BOOL))))))))
              ;; Lazy code, tests types from ctx to jump to the correct lazy-code
              (lazy-main (make-lazy-code
                           (lambda (cgc ctx)
                             
                             (let ((left-type  (cadr (ctx-stack ctx)))
                                   (right-type (car (ctx-stack ctx)))
                                   ;; ctx with num for right operand
                                   (rctx  (ctx-push (ctx-pop ctx) CTX_NUM))
                                   ;; ctx with num for left operand
                                   (lctx  (make-ctx (cons (car (ctx-stack ctx)) (cons CTX_NUM (cddr (ctx-stack ctx)))) (ctx-env ctx) (ctx-nb-args ctx)))
                                   ;; ctx with num for left AND right operand
                                   (lrctx (make-ctx (cons CTX_NUM (cons CTX_NUM (cddr (ctx-stack ctx)))) (ctx-env ctx) (ctx-nb-args ctx))))

                               (cond ((eq? left-type CTX_NUM)
                                      (cond ((eq? right-type CTX_NUM)     (jump-to-version cgc lazy-code-op ctx))
                                            ((eq? right-type CTX_UNK) (jump-to-version cgc (gen-dyn-type-test CTX_NUM 0 rctx lazy-code-op ctx lazy-fail) ctx))
                                            (else                     (gen-error cgc (ERR_TYPE_EXPECTED CTX_NUM)))))

                                     ((eq? left-type CTX_UNK)
                                      (cond ((eq? right-type CTX_NUM)     (jump-to-version cgc (gen-dyn-type-test CTX_NUM 1 lctx lazy-code-op ctx lazy-fail) ctx))
                                            ((eq? right-type CTX_UNK) (let* ((right-test (gen-dyn-type-test CTX_NUM 1 lrctx lazy-code-op ctx lazy-fail))
                                                                              (left-test  (gen-dyn-type-test CTX_NUM 0 lctx right-test ctx lazy-fail)))
                                                                         (jump-to-version cgc left-test ctx)))
                                            (else                      (gen-error cgc (ERR_TYPE_EXPECTED CTX_NUM)))))
                                     (else (gen-error cgc (ERR_TYPE_EXPECTED CTX_NUM)))))))))
    ;; Return left operand lazy-code
    lazy-ast-left))

;;
;; Make lazy code from GENERIC OPERATOR
;;
(define (mlc-op-gen ast succ op)
  (let ((lazy-code-op (make-lazy-code
                        (lambda (cgc ctx)
                          (x86-pop cgc (x86-rbx))
                          (case op
                            (($eq?)
                             (let ((label-done
                                     (asm-make-label cgc (new-sym 'done))))
                               (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rbx))
                               (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #t)))
                               (x86-je  cgc label-done)
                               (x86-mov cgc (x86-rbx) (x86-imm-int (obj-encoding #f)))
                               (x86-label cgc label-done)
                               (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rbx))))
                            (else
                              (error "unknown op" op)))
                          (jump-to-version cgc
                                           succ
                                           (ctx-push
                                             (ctx-pop (ctx-pop ctx))
                                             (cond ((member op '($eq?)) CTX_BOOL))))))))
    (gen-ast-l (cdr ast) lazy-code-op)))

;;
;; Make lazy code from TYPE TEST
;;
(define (mlc-test ast succ)
  (let* ((op (car ast))
         (lazy-test (make-lazy-code
                      (lambda (cgc ctx)
                        (let ((label-done (asm-make-label cgc (new-sym 'label_done))))
                          (x86-pop   cgc (x86-rax))
                          (cond ;; $number?
                                ((eq? op '$number?)
                                    (x86-and   cgc (x86-rax) (x86-imm-int 3)) ;; If equal, set ZF to 1
                                    (x86-mov   cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
                                    (x86-je    cgc label-done)
                                    (x86-mov   cgc (x86-rax) (x86-imm-int (obj-encoding #f))))
                                ;; $char?
                                ((eq? op '$char?)
                                    (x86-mov cgc (x86-rbx) (x86-rax))
                                    (x86-and cgc (x86-rax) (x86-imm-int 3))
                                    (x86-cmp cgc (x86-rax) (x86-imm-int TAG_SPECIAL))
                                    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                    (x86-jne cgc label-done) ;; No tag for special, then not char
                                    ;; Tag is 'special', test value
                                    (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
                                    (x86-jl cgc label-done) ;; <0 is !char and >=0 is char
                                    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t))))
                                ;; Others
                                (else (x86-mov cgc (x86-rbx) (x86-rax))
                                      (x86-and cgc (x86-rax) (x86-imm-int 3))
                                      (x86-cmp cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
                                      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                      (x86-jne cgc label-done)
                                      ;; It's a memory allocated obj
                                      (x86-mov cgc (x86-rbx) (x86-mem (* -1 TAG_MEMOBJ) (x86-rbx)))
                                      (x86-and cgc (x86-rbx) (x86-imm-int 248))
                                      (cond ((eq? op '$procedure?)   (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_PROCEDURE)))) ;; STAG_PROCEDURE << 3
                                            ((eq? op '$pair?)        (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_PAIR))))      ;; STAG_PAIR << 3
                                            ((eq? op '$vector?)      (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_VECTOR))))    ;; STAG_VECTOR << 3
                                            ((eq? op '$string?)      (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_STRING))))    ;; STAG_STRING << 3
                                            ((eq? op '$symbol?)      (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_SYMBOL))))    ;; STAG_SYMBOL << 3
                                            ((eq? op '$input-port?)  (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_IPORT))))     ;; STAG_IPORT << 3
                                            ((eq? op '$output-port?) (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_OPORT))))     ;; STAG_OPORT << 3
                                            (else (error "NYI")))
                                      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                      (x86-jne cgc label-done)
                                      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))))
                          ;;
                          (x86-label cgc label-done)
                          (x86-push  cgc (x86-rax))
                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))))
    (gen-ast (cadr ast) lazy-test)))

;;
;; Make lazy code to create pair
;; Create pair with the too values on top of the stack
;;
(define (mlc-pair succ)
  (make-lazy-code
    (lambda (cgc ctx)
       (let ((header-word (mem-header 3 STAG_PAIR)))
         
         ;; Alloc
         (gen-allocation cgc ctx STAG_PAIR 3)
         
         ;; Write object header
         (x86-mov cgc (x86-rax) (x86-imm-int header-word))
         (x86-mov cgc (x86-mem -24 alloc-ptr) (x86-rax))
         (x86-pop cgc (x86-rbx)) ;; pop CDR
         (x86-pop cgc (x86-rax)) ;; pop CAR
         ;; Write pair
         (x86-mov cgc (x86-mem -16 alloc-ptr)  (x86-rax))
         (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rbx))
         ;; Tag,Push closure and update alloc-ptr
         (x86-mov cgc (x86-rax) alloc-ptr)
         (x86-add cgc (x86-rax) (x86-imm-int (- TAG_MEMOBJ 24)))
         (x86-push cgc (x86-rax))
         (jump-to-version cgc succ (ctx-push (ctx-pop-nb ctx 2) CTX_PAI))))))

;;-----------------------------------------------------------------------------

;; AST RELATED FUNCTIONS

;;-----------------------------------------------------------------------------

;;
;; CC TABLE
;;

;; CC Table (Closure Context Table) :
;; A closure contains a header, the CC Table addr, and all free vars
;; The CC Table contains multiple possible entry points (fixed number) for the procedure
;; Each slot contains initially the address of the procedure stub
;; As soon as a version is generated for a context, the slot is replaced by the generated address
;;
;; EX : closure at initial state
;; +----------------+---------+---------+---------+---------+---------+
;; |Header          |CC Table |Free var |Free var |   ...   |Free var |
;; |(Same as gambit)|addr     |    1    |    2    |         |    n    |
;; +----------------+----|----+---------+---------+---------+---------+
;;                       |
;;      +----------------+
;;      |
;;      v
;; +---------+---------+---------+---------+---------+
;; |Stub addr|Stub addr|Stub addr|   ...   |Stub addr|
;; |         |         |         |         |         |
;; +---------+---------+---------+---------+---------+
;;  index  0  index  1  index  2     ...    index  n
;;
;; EX closure with two existing versions
;; +----------------+---------+---------+---------+---------+---------+
;; |Header          |CC Table |Free var |Free var |   ...   |Free var |
;; |(Same as gambit)|addr     |    1    |    2    |         |    n    |
;; +----------------+----|----+---------+---------+---------+---------+
;;                       |
;;      +----------------+
;;      |
;;      v
;; +---------+---------+---------+---------+---------+
;; |Proc addr|Stub addr|Proc addr|   ...   |Stub addr|
;; |(ctx1)   |         |(ctx5)   |         |         |
;; +---------+---------+---------+---------+---------+
;;  index  0  index  1  index  2     ...    index  n

;; Gen a new cc-table at 'alloc-ptr' and write 'stub-addr' in each slot
(define (gen-cc-table cgc stub-addr offset)
  (x86-mov cgc (x86-rax) (x86-imm-int stub-addr))
  (gen-cc-table-h cgc offset global-cc-table-maxsize))

;; Gen-cc-table helper
(define (gen-cc-table-h cgc offset nb-slots)
  (if (> nb-slots 0)
      (begin (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))
             (gen-cc-table-h cgc (+ offset 8) (- nb-slots 1)))))

;;
;; VARIABLE SET
;;

;; Gen code to set a free/local
;; variable is the lookup result which contains id info
;;  ex: variable = '(n . identifier-obj)

;; Free var
(define (gen-set-freevar cgc ctx variable)
   (let ((mutable (identifier-mutable? (cdr variable))))
      (if mutable
        (begin (gen-get-freevar cgc ctx variable 'gen-reg)
               (x86-pop cgc (x86-rbx))
               (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) (x86-rbx)))
               ;; TODO replace ctx type when implemented for free vars
        (error "Compiler error : set a non mutable free var"))))

;; Local var
(define (gen-set-localvar cgc ctx variable)

   (let ((mutable (identifier-mutable? (cdr variable))))
     (if mutable
        (begin (gen-get-localvar cgc ctx variable 'gen-reg)
               (x86-pop cgc (x86-rbx))
               (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)) (x86-rbx))
               
               ;; Replace ctx type
               (let* ((fs (length (ctx-stack ctx)))
                      (idx (- fs 2 (identifier-offset (cdr variable)))))
               (set-car! (list-tail (ctx-stack ctx) idx) (car (ctx-stack ctx)))))
               
        (error "Compiler error : set a non mutable local var"))))

;; Gen code to set a global var
(define (gen-set-globalvar cgc ctx variable)
   (x86-pop cgc (x86-rax))
   (x86-mov cgc (x86-mem (* 8 (cdr variable)) (x86-r10)) (x86-rax)))

;;
;; VARIABLE GET
;;

;; Gen code to get a variable from closure/stack
;; variable is the lookup result which contains id info
;;  ex: variable = '(n . identifier-obj)
;; dest is the destination. possible values are :
;;  'stack : push value
;;  'gen-reg : general register (mov value to rax)
;; raw_value is #t if the value is copied directly from closure
;;              #f if the value is copied from memory (id variable is mutable)

;; Free variable
(define (gen-get-freevar cgc ctx variable dest #!optional (raw_value? #t))
   (let* ((offset (+ (- 16 TAG_MEMOBJ) (* 8 (identifier-offset (cdr variable)))))
          (clo-offset (* 8 (closure-pos ctx)))
          (mutable (identifier-mutable? (cdr variable))))

      ;; Get closure
      (x86-mov cgc (x86-rax) (x86-mem clo-offset (x86-rsp)))

      (if (or raw_value? (not mutable))
        ;; Raw value required (direct copy from closure)
        ;; OR variable is not mutable
        (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem offset (x86-rax))))
              ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax))))
              (else (error "Invalid destination")))
        ;; Real value required and variable is mutable
        (begin (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
               (cond ((eq? dest 'stack) (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                     ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                     (else (error "Invalid destination")))))

      CTX_UNK)) ;; TODO return free var ctx info when implemented

;; Local variable
(define (gen-get-localvar cgc ctx variable dest #!optional (raw_value? #t))
   (let* ((fs (length (ctx-stack ctx)))
          (pos (- fs 2 (identifier-offset (cdr variable))))
          (mutable (identifier-mutable? (cdr variable))))
      
      (if (or raw_value? (not mutable))
        ;; Raw value required (direct copy from stack)
        ;; OR variable is not mutable
        (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem (* pos 8) (x86-rsp))))
              ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (* pos 8) (x86-rsp))))
              (else (error "Invalid destination")))
        ;; Real value required and variable is mutable
        (begin (x86-mov cgc (x86-rax) (x86-mem (* pos 8) (x86-rsp)))
               (cond ((eq? dest 'stack)
                        (begin 
                               (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax)))))
                     ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))
                     (else (error "Invalid destination")))))
      (list-ref (ctx-stack ctx) pos)))

;; Gen code to get a global var
(define (gen-get-globalvar cgc ctx variable dest)
   (cond ((eq? dest 'stack)   (x86-push cgc (x86-mem (* 8 (cdr variable)) (x86-r10))))
         ((eq? dest 'gen-reg) (x86-mov cgc (x86-rax) (x86-mem (* 8 (cdr variable)) (x86-r10))))
         (else (error "Invalid destination")))
   CTX_UNK)

;;
;; FREE VARS
;;

;; Extends env with 'fvars' free vars starting with offset
(define (build-fenv saved-env mvars fvars offset)
  (if (null? fvars)
      '()
      (cons (cons (car fvars) (make-identifier 'free
                                               offset
                                               (let ((res (assoc (car fvars) saved-env)))
                                                  (if (identifier-mutable? (cdr res))
                                                    '(mutable)
                                                    '()))))
            (build-fenv saved-env mvars (cdr fvars) (+ offset 1)))))

;; Write free vars in closure
(define (gen-free-vars cgc vars ctx offset)
  (if (null? vars)
      '()
      (let* ((var (car vars))
             (res (assoc var (ctx-env ctx))))
         (if res
            (if (eq? (identifier-type (cdr res)) 'free)
               ;; Free var
               (gen-get-freevar  cgc ctx res 'gen-reg)
               ;; Local var
               (gen-get-localvar cgc ctx res 'gen-reg))
            (error "Can't find variable: " var))
         ;; TODO : free vars ctx information
         (x86-mov cgc (x86-mem offset alloc-ptr) (x86-rax))
         (gen-free-vars cgc (cdr vars) ctx (+ offset 8)))))

;; Return all free vars used by the list of ast knowing env 'clo-env'
(define (free-vars-l lst clo-env ctx)
  (if (null? lst)
      '()
      (set-union (free-vars (car lst) clo-env ctx) (free-vars-l (cdr lst) clo-env ctx))))

;; Return all free vars used by ast knowing env 'clo-env'
(define (free-vars ast clo-env ctx)
  (cond ;; Symbol
        ((symbol? ast)
          (if (and (assoc ast (ctx-env ctx))
                   (not (member ast clo-env)))
              (list ast)
              '()))
        ;; Literal
        ((literal? ast) '())
        ;; Pair
        ((pair? ast)
          (let ((op (car ast)))
            (cond ;; If
                  ((eq? op 'if) (set-union (free-vars (cadr ast)   clo-env ctx)   ; cond
                                           (set-union (free-vars (caddr ast)  clo-env ctx)    ; then
                                                      (free-vars (cadddr ast) clo-env ctx)))) ; else
                  ;; Quote
                  ((eq? op 'quote) '())
                  ;; Lambda
                  ((eq? op 'lambda) (free-vars (caddr ast) 
                                               (if (list? (cadr ast))
                                                  (append (cadr ast) clo-env)
                                                  (cons (cadr ast) clo-env))
                                               ctx))
                  ;; Call
                  (else (free-vars-l ast clo-env ctx)))))))

;;
;; MUTABLE VARS
;;

;; Return all mutable vars used by the list of ast
(define (mutable-vars-l lst params)
  (if (null? lst)
    '()
    (set-union (mutable-vars (car lst) params) (mutable-vars-l (cdr lst) params))))

;; Return all mutable vars used by ast
(define (mutable-vars ast params)
  (cond ;; Literal 
        ((literal? ast) '())
        ;; Pair
        ((pair? ast)
           (let ((op (car ast)))
              (cond ((eq? op 'lambda) (mutable-vars (caddr ast) (set-sub params 
                                                                         (if (list? (cadr ast))
                                                                           (cadr ast)
                                                                           (list (cadr ast)))
                                                                         '())))
                    ((eq? op 'set!)
                      (set-union (mutable-vars (caddr ast) params)
                                 (if (member (cadr ast) params)
                                   (list (cadr ast))
                                   '())))
                    (else (mutable-vars-l ast params)))))))

;;
;; OPTIMIZATIONS
;;

;; Shift stack slots for tail call optimization
;; 'nb': nb of slot to shift
;; 'init-from': position of first slot to move
;; 'init-to': destination of the first slot to move
(define (tail-shift cgc nb init-from init-to)
  (tail-shift-h cgc nb init-from init-to (- init-to init-from)))

(define (tail-shift-h cgc curr from to rsp-offset)
  (if (< curr 0)
      ;; All shifted, update RSP
      (x86-add cgc (x86-rsp) (x86-imm-int (* 8 rsp-offset)))
      ;; Shift next
      (begin (x86-mov cgc (x86-rax) (x86-mem (* from 8) (x86-rsp)))
             (x86-mov cgc (x86-mem (* to 8) (x86-rsp))  (x86-rax))
             (tail-shift-h cgc (- curr 1) (- from 1) (- to 1) rsp-offset))))

;;
;; UTILS
;;

;; Build new environment with ids starting from 'start'
;; Append this new environment to existing 'env'
;; ex : (build-env '(...) '(a b c) 8) -> ((a . 8) (b . 9) (c . 10))
;; mvars contains all mutable vars to tag created identifier objects
(define (build-env mvars ids start env)
  (if (null? ids)
    env
    (cons (cons (car ids) (make-identifier 'local
                                           start
                                           (if (member (car ids) mvars)
                                              '(mutable)
                                              '())))
          (build-env mvars (cdr ids) (+ start 1) env))))

;; Get position of current closure in stack
(define (closure-pos ctx)
  (- (length (ctx-stack ctx)) 2 (ctx-nb-args ctx))) ;; 2= 1length + 1retAddr

;; Return label associated to function name
(define (lookup-fn name)
  (let ((r (assoc name functions)))
    (if r
      (cdr r)
      (cond ((eq? name '$$putchar)  label-$$putchar)
            (else (error "NYI"))))))

;; Set subtraction with lists
;; return lsta - lstb
;; res is accu
(define (set-sub lsta lstb res)
  (if (null? lsta)
    res
    (if (member (car lsta) lstb)
      (set-sub (cdr lsta) lstb res)
      (set-sub (cdr lsta) lstb (cons (car lsta) res)))))

;; Set union with lists
;; return lsta U lstb
(define (set-union lsta lstb)
  (if (null? lsta)
    lstb
    (if (member (car lsta) lstb)
      (set-union (cdr lsta) lstb)
      (set-union (cdr lsta) (cons (car lsta) lstb)))))

;; Flatten list x
(define (flatten x)
   (cond ((null? x) '())
         ((not (pair? x)) (list x))
         (else (append (flatten (car x))
                       (flatten (cdr x))))))

;; Get formal params from list of params
;; Ex: (formal-params '(a b c)  ) -> '(a b c)
;;     (formal-params '(a b . c)) -> '(a b)
(define (formal-params l)
  (if (not (pair? l))
     '()
     (cons (car l) (formal-params (cdr l)))))

;; Gen mutable variable
;; This code is a function prelude. It transforms variable from stack (args) tagged as "mutable"
;; into memory-allocated variables.
(define (gen-mutable cgc ctx mutable)

    (if (not (null? mutable))
       
       (let* ((res (assoc (car mutable) (ctx-env ctx)))
              (header-word (mem-header 2 STAG_MOBJECT))
              (fs (length (ctx-stack ctx)))
              (offset (* (- fs 2 (identifier-offset (cdr res))) 8)))
         
        ;; Alloc
        (gen-allocation cgc ctx STAG_MOBJECT 2)
        
        ;; Create var in memory
        (gen-get-localvar cgc ctx res 'gen-reg) ;; There are only localvar here (no free vars)
        (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rax))
        (x86-mov cgc (x86-rax) (x86-imm-int header-word))
        (x86-mov cgc (x86-mem -16 alloc-ptr) (x86-rax))

        ;; Replace local
        (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 16) alloc-ptr))
        (x86-mov cgc (x86-mem offset (x86-rsp)) (x86-rax))

        ;; Gen next mutable vars
        (gen-mutable cgc ctx (cdr mutable)))))

;; Gen code to create rest list from stack in heap.
;; nb-pop: Number of values in rest list
;; sp-offset: offset from rsp of the first value in rest list
(define (gen-rest-lst cgc ctx nb-pop)
  ;; Push the last cdr
  (x86-push cgc (x86-imm-int (obj-encoding '())))
  ;; Buils rest list
  (gen-rest-lst-h cgc ctx nb-pop nb-pop 16)) ;; 24: '(), ctx, closure

;; Create a pair with top of stack in cdr
;; and argument slot (rsp + sp-offset) in car
;; then push this pair and create next.
(define (gen-rest-lst-h cgc ctx pos nb sp-offset)
  (if (= pos 0)
      ;; All pairs created, then change stack layout
      (begin ;; Mov rest list to stack
             (x86-pop cgc (x86-rax))
             (x86-mov cgc (x86-mem (* nb 8) (x86-rsp)) (x86-rax))
             ;; Update closure position
             (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
             (x86-mov cgc (x86-mem (- (* 8 nb) 8) (x86-rsp)) (x86-rax))
             ;; Update rsp
             (x86-add cgc (x86-rsp) (x86-imm-int (- (* 8 nb) 8))))
      ;; Create a pair and continue
      (begin ;; Alloc pair
             (gen-allocation cgc ctx STAG_PAIR 3)
             
             (let ((header (mem-header 3 STAG_PAIR)))
               ;; Write header in pair
               (x86-mov cgc (x86-rax) (x86-imm-int header))
               (x86-mov cgc (x86-mem -24 alloc-ptr) (x86-rax))
               ;; Get car from stack (arg slot) and write in pair
               (x86-mov cgc (x86-rax) (x86-mem sp-offset (x86-rsp)))
               (x86-mov cgc (x86-mem -16 alloc-ptr) (x86-rax))
               ;; Get cdr from stack (top of stack) and write in pair
               (x86-pop cgc (x86-rax))
               (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rax))
               ;; Tag & push
               (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 24) alloc-ptr))
               (x86-push cgc (x86-rax)))
             ;; Create next pair
             (gen-rest-lst-h cgc ctx(- pos 1) nb (+ sp-offset 8)))))

;;-----------------------------------------------------------------------------
;; Utils

;; Is the v a literal ?
(define (literal? v)
   (or (char? v) (number? v) (symbol? v) (vector? v) (string? v) (boolean? v) (null? v)))

;; Call n times the function fn with given args
(define (call-n n fn . args)
  (if (> n 0)
    (begin (apply fn args)
           (apply call-n (append (list (- n 1) fn ) args)))))