
(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; Base ctx for procedure call
(define base-ctx (list CTX_CTXID CTX_CLO))

;; TODO : RCX global ? (used by if/else stubs)

;;-----------------------------------------------------------------------------
;; AST FUNCTIONS

;; Gen lazy code from a list of exprs
(define (gen-ast-l lst succ tail)
  (cond ((null? lst) (error "Empty list"))
        ((= (length lst) 1) (gen-ast (car lst) succ #f))
        (else (gen-ast (car lst) (gen-ast-l (cdr lst) succ #f) #f))))

;; Gen lazy code from ast
(define (gen-ast ast succ tail)
  (cond ;; String
        ((string? ast)  (mlc-string ast succ #f))
        ;; Literal
        ((literal? ast) (mlc-literal ast succ))
        ;; Symbol
        ((symbol? ast)  (mlc-identifier ast succ))
        ;; Pair
        ((pair? ast)
         (let ((op (car ast)))
           (cond ;; Special with call
                 ((member op '($$putchar)) (mlc-special-c ast succ))
                 ;; Special without call
                 ((member op '($error $string-set! $make-string $symbol->string $string->symbol $string-length $string-ref $integer->char $char->integer $vector-set! $vector-ref $vector-length $make-vector $cons $set-car! $set-cdr! $car $cdr)) (mlc-special-nc ast succ))
                 ;; Quote
                 ((eq? 'quote (car ast)) (mlc-quote (cadr ast) succ))
                 ;; Set!
                 ((eq? 'set! (car ast)) (mlc-set! ast succ))
                 ;; Lambda
                 ((eq? op 'lambda) (mlc-lambda ast succ))
                 ;; Operator num
                 ((member op '($+ $- $* $quotient $modulo $< $> $=)) (mlc-op-num ast succ op))
                 ;; Operator gen
                 ((member op '($eq?)) (mlc-op-gen ast succ op))
                 ;; Tests
                 ((member op '($symbol? $string? $char? $vector? $number? $procedure? $pair?)) (mlc-test ast succ))
                 ;; If
                 ((eq? op 'if) (mlc-if ast succ))
                 ;; Define
                 ((eq? op 'define) (mlc-define ast succ))
                 ;; Call expr
                 (else (mlc-call ast succ tail)))))
        ;; *unknown*
        (else
         (error "unknown ast" ast))))

;;-----------------------------------------------------------------------------
;; MLC FUNCTIONS :
;; Make a lazy code from ast

;;
;; Make lazy code from LITERAL
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
;; (ex. 'Hello)
;;
(define (mlc-symbol ast succ)
  (mlc-string (symbol->string ast) succ #t))

;;
;; Make lazy code from vector
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
             (lazy-vector-set-gen idx)
             #f))                  
  
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
        (else (gen-ast ast succ #f))))

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

     (gen-ast (caddr ast) lazy-set #f)))

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
         (lazy-val (gen-ast (caddr ast) lazy-bind #f)))

    (make-lazy-code (lambda (cgc ctx)
                      (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
                      (x86-mov cgc (x86-mem (* 8 (length globals)) (x86-r10)) (x86-rax))
                      (set! globals (cons (cons identifier (length globals)) globals))
                      (jump-to-version cgc lazy-val ctx)
                      ))))

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
        (gen-ast-l (cdr ast) lazy-special #f)
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
                    (make-lazy-code
                      (lambda (cgc ctx)
                        (let ((ctx-type  (car (ctx-stack ctx)))
                              (lazy-fail (lambda (ERR) (make-lazy-code
                                                         (lambda (cgc ctx)
                                                           (gen-error cgc ERR))))))
                          (cond ((or (and (eq? special '$char->integer) (eq? ctx-type CTX_CHAR))
                                     (and (eq? special '$integer->char) (eq? ctx-type CTX_NUM)))
                                 (jump-to-version cgc lazy-charint ctx))
                                ((and (eq? special '$char->integer) (eq? ctx-type CTX_UNK)) ;; TODO : gen dyn commun
                                 (jump-to-version cgc
                                                  (gen-dyn-type-test CTX_CHAR
                                                                     0
                                                                     (ctx-push (ctx-pop ctx) CTX_CHAR)
                                                                     lazy-charint
                                                                     ctx
                                                                     (lazy-fail ERR_CHAR_EXPECTED))
                                                  ctx))
                                ((and (eq? special '$integer->char) (eq? ctx-type CTX_UNK))
                                 (jump-to-version cgc
                                                  (gen-dyn-type-test CTX_NUM
                                                                     0
                                                                     (ctx-push (ctx-pop ctx) CTX_NUM)
                                                                     lazy-charint
                                                                     ctx
                                                                     (lazy-fail ERR_NUM_EXPECTED))
                                                  ctx))
                                (else (if (eq? special '$char->integer)
                                          (gen-error cgc ERR_CHAR_EXPECTED)
                                          (gen-error cgc ERR_NUM_EXPECTED)))))))))
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
                        (x86-mov cgc (x86-rcx) (x86-rdx))
                        (x86-shl cgc (x86-rcx) (x86-imm-int 1))
                        (x86-neg cgc (x86-rcx))
                        (x86-add cgc (x86-rcx) alloc-ptr)
                        (x86-sub cgc (x86-rcx) (x86-imm-int 24))
                        
                        ;; Fill string
                        (x86-push cgc (x86-rbx))
                        
                        ;; RCX contient la position du vecteur
                        (x86-mov cgc (x86-rax) (x86-rcx))
                        
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
                        (x86-mov cgc (x86-mem 8 (x86-rcx)) (x86-rbx))
                        
                        ;; Write header
                        (x86-shl cgc (x86-rdx) (x86-imm-int 6))
                        (x86-add cgc (x86-rdx) (x86-imm-int header-word))
                        (x86-mov cgc (x86-mem 0 (x86-rcx)) (x86-rdx))
                        
                        ;; Push vector
                        (x86-add cgc (x86-rcx) (x86-imm-int TAG_MEMOBJ))
                        (x86-push cgc (x86-rcx))
                        
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
                        (x86-mov cgc (x86-rcx) alloc-ptr)
                        (x86-sub cgc (x86-rcx) (x86-rax))
                        
                        ;; Fill vector
                        (x86-push cgc (x86-rcx))
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
                            (x86-mov cgc (x86-mem 16 (x86-rcx)) (x86-rax))
                            ;; Update offset and remaining elements nb
                            (x86-add cgc (x86-rcx) (x86-imm-int 8))
                            (x86-sub cgc (x86-rbx) (x86-imm-int 4))
                            ;; loop
                            (x86-jmp cgc label-loop)
                        
                        ;; END:  
                        (x86-label cgc label-end)
                        (x86-pop cgc (x86-rbx))
                        (x86-pop cgc (x86-rcx)))
                        
                        ;; Write encoded length
                        (x86-mov cgc (x86-mem 8 (x86-rcx)) (x86-rbx))
                        
                        ;; Write header
                        (x86-shl cgc (x86-rbx) (x86-imm-int 6))
                        (x86-add cgc (x86-rbx) (x86-imm-int header-word))
                        (x86-mov cgc (x86-mem 0 (x86-rcx)) (x86-rbx))
                        
                        ;; Push vector
                        (x86-add cgc (x86-rcx) (x86-imm-int TAG_MEMOBJ))
                        (x86-push cgc (x86-rcx))
                        
                        ;;
                        (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_VECT))))))
                 
                 ;; STRING<->SYMBOL
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
                      (x86-mov cgc (x86-rcx) (x86-mem 8 (x86-rax)))
                      (x86-mov cgc (x86-mem 8 (x86-rbx)) (x86-rcx))
                      
                      ;; Mov header in symbol
                      (x86-mov cgc (x86-rcx) (x86-mem 0 (x86-rax)))
                      (if (eq? special '$string->symbol)
                         (x86-sub cgc (x86-rcx) (x86-imm-int (arithmetic-shift (- STAG_STRING STAG_SYMBOL) 3)))
                         (x86-add cgc (x86-rcx) (x86-imm-int (arithmetic-shift (- STAG_STRING STAG_SYMBOL) 3))))
                      (x86-mov cgc (x86-mem 0 (x86-rbx)) (x86-rcx))
                      
                      ;; Encoded length in rcx
                      (x86-shr cgc (x86-rcx) (x86-imm-int 8))
                      (x86-shl cgc (x86-rcx) (x86-imm-int 3))
                                            
                      ;; If encoded length == 16
                      ;;    jump label-fin
                      (let ((label-loop (asm-make-label cgc (new-sym 'label-loop)))
                            (label-fin  (asm-make-label cgc (new-sym 'label-fin))))
                        
                        (x86-label cgc label-loop)
                        (x86-cmp cgc (x86-rcx) (x86-imm-int 16))
                        (x86-jle cgc label-fin)
                        
                          (x86-mov cgc (x86-rdx) (x86-mem -8 (x86-rcx) (x86-rax)))
                          (x86-mov cgc (x86-mem -8 (x86-rcx) (x86-rbx)) (x86-rdx))
                          (x86-sub cgc (x86-rcx) (x86-imm-int 8))
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
          ;; $cons, $string-ref, $vector-ref, $set-car!, $set-cdr!
          ((member special '($cons $string-ref $vector-ref $set-car! $set-cdr!))
           (let ((lazy-right (gen-ast (caddr ast) lazy-special #f)))
             (gen-ast (cadr ast) lazy-right #f)))
          ;; $car, $cdr, $make-vector, $string->symbol, $symbol->string, ...
          ((member special '($symbol->string $string->symbol $string-length $integer->char $char->integer $vector-length $make-string $make-vector $car $cdr))
           (gen-ast (cadr ast) lazy-special #f))
          ;; $string-set!, $vector-set!
          ((member special '($string-set! $vector-set!))
           (let* ((lazy-value (gen-ast (cadddr ast) lazy-special #f))
                  (lazy-index (gen-ast (caddr  ast) lazy-value #f)))
             (gen-ast (cadr ast) lazy-index #f)))
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
         (lazy-ret (make-lazy-code
                     (lambda (cgc ctx)
                       ;; Here the stack is :
                       ;;         RSP
                       ;;     | ret-val |  ctx  | ret-addr | closure | arg n | ... | arg 1 |
                       ;; Or if rest :
                       ;;     | ret-val |  ctx  | ret-addr | closure | rest | arg n | ... | arg 1 |
                       (let ((retval-offset
                                (if rest-param
                                   (* 8 (+ 3 (length params)))
                                   (* 8 (+ 2 (length params))))))
                         
                         ;; Pop return value
                         (x86-pop  cgc (x86-rax))
                         ;; Swap return value (rax) and return address ([rsp+offset])
                         (x86-xchg cgc (x86-rax) (x86-mem retval-offset (x86-rsp)))
                         ;; Update SP to return value
                         (x86-add  cgc (x86-rsp) (x86-imm-int retval-offset))
                         ;; Jump to continuation
                         (x86-jmp cgc (x86-rax))))))
         ;; Lazy lambda body
         (lazy-body (gen-ast (caddr ast) lazy-ret #t))
         ;; Lazy function prologue : creates rest param if any, transforms mutable vars, ...
         (lazy-prologue (make-lazy-code
                           (lambda (cgc ctx)

                              (let* ((actual-p (- (length (ctx-stack ctx)) (length base-ctx) 1)) ;; 1 for return address
                                     (formal-p (ctx-nb-args ctx)))

                                (if (or (and (not rest-param) (not (= actual-p formal-p)))
                                        (and rest-param (< actual-p formal-p)))
                                   ;; Wrong number of arguments
                                   (gen-error cgc ERR_WRONG_NUM_ARGS)
                                   ;; Correct number of arguments
                                   (begin (cond ;; Rest param declared but not given
                                                ((and rest-param (= actual-p formal-p))
                                                    ;; Shift 2 values on stack
                                                    (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
                                                    (x86-push cgc (x86-rax))
                                                    (x86-mov cgc (x86-rax) (x86-mem 16 (x86-rsp)))
                                                    (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-rax))
                                                    ;; Mov '() in rest param slot
                                                    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
                                                    (x86-mov cgc (x86-mem 16 (x86-rsp)) (x86-rax))
                                                    ;; Add type information to ctx
                                                    (ctx-stack-set! ctx (append base-ctx (list 'pair) (list-tail (ctx-stack ctx) (length base-ctx))))
                                                    (ctx-nb-args-set! ctx (+ (ctx-nb-args ctx) 1)))
                                                ;; Rest param declared and given
                                                ((and rest-param (> actual-p formal-p))
                                                    (gen-rest-lst cgc ctx (- actual-p formal-p) (+ 16 (* 8 (- actual-p formal-p 1))) 0 (- actual-p formal-p))
                                                    (ctx-stack-set! ctx (append base-ctx '(pair) (list-tail (ctx-stack ctx) (- (length (ctx-stack ctx)) formal-p 1)))) ;; 1 for return address ;; TODO : write context one time
                                                    (ctx-nb-args-set! ctx (+ (ctx-nb-args ctx) 1))
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
                                               (let* ((env (append (build-env mvars all-params 0) (build-fenv saved-env mvars fvars 0)))
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
;; Make lazy code from IF
;;
(define (mlc-if ast succ)
  (let* ((lazy-code0
           (gen-ast (cadddr ast) succ #f))
         (lazy-code1
           (gen-ast (caddr ast) succ #f))
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
      lazy-code-test
      #f)))

;;
;; Make lazy code from CALL EXPRESSION
;;
(define (mlc-call ast succ tail)
  (let* (;; Call arguments
         (args (cdr ast))
         ;; Lazy fail
         (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_PRO_EXPECTED))))
         ;; Lazy call
         (lazy-call (make-lazy-code (lambda (cgc ctx)
                                      
                                        ;; Call ctx in rdx
                                        (let* ((call-stack    (if tail
                                                                (append base-ctx (list-head (cdr (ctx-stack ctx)) (length args)) (list CTX_RETAD))
                                                                (append base-ctx (list-head (cdr (ctx-stack ctx)) (+ (length args) 1))))) ;; CTX : list CTX_RETAD au lieu de +1 ?
                                               (call-ctx      (make-ctx call-stack '() -1))
                                               (ctx-id        (length ctx_ids))
                                               (cct-offset    (* 8 (+ 1 (get-closure-index call-ctx)))))

                                          (set! ctx_ids (cons (cons ctx-id call-ctx) ctx_ids))
                                          (x86-mov cgc (x86-rax) (x86-imm-int (* 4 ctx-id))) ;; '*4' to encode ctx
                                          (x86-push cgc (x86-rax))
                                          
                                        (if tail 
                                          (tail-shift cgc
                                                      ;; Nb slots to shift
                                                      (+ (length args) 2) ;; +1 fermeture, +1 ctx
                                                      ;; Initial from slot
                                                      (- (length (ctx-stack ctx)) 3 (ctx-nb-args ctx))
                                                      ;; Initial to slot
                                                      (- (length (ctx-stack ctx)) 1)))
                                        
                                        ;; 1 - Get cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))               ;; get closure
                                        (x86-mov cgc (x86-rax) (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))) ;; get cc-table

                                        ;; 2 - Get entry point in cc-table
                                        (x86-mov cgc (x86-rax) (x86-mem cct-offset (x86-rax)))

                                        ;; 3 - Jump to entry point
                                        (x86-jmp cgc (x86-rax))))))
         ;; Lazy main
         (lazy-main (make-lazy-code (lambda (cgc ctx)
                                      (let ((op-type (car (ctx-stack ctx))))
                                        (cond ((eq? op-type CTX_CLO) (jump-to-version cgc lazy-call ctx))
                                              ((eq? op-type CTX_UNK) (jump-to-version cgc
                                                                                      (gen-dyn-type-test CTX_CLO
                                                                                                         0
                                                                                                         (ctx-push (ctx-pop ctx) CTX_CLO)
                                                                                                         lazy-call
                                                                                                         ctx
                                                                                                         lazy-fail)
                                                                                      ctx))
                                              (else (gen-error cgc ERR_PRO_EXPECTED)))))))
         ;; Lazy callee
         (lazy-operator (gen-ast (car ast) lazy-main #f)))

    ;; Build first lazy code
    ;; This lazy code creates continuation stub and push return address
    (if tail
        (if (> (length args) 0)
          ;; If args, then compile args
          (gen-ast-l args lazy-operator #f)
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
                                                   ;; Remove lambda and args from ctx, and add retval (unknown)
                                                   (let ((ctx-continuation (ctx-push ctx CTX_UNK)))
                                                      (if (not gen-flag) ;; Continuation not yet generated, then generate and set gen-flag = continuation addr
                                                         (set! gen-flag (gen-version-continuation load-ret-label
                                                                                                  succ
                                                                                                  ctx-continuation)))
                                                       gen-flag)))))
                 ;; Return address (continuation label)
                 (x86-label cgc load-ret-label)
                 (x86-mov cgc (x86-rax) (x86-imm-int (vector-ref (list-ref stub-labels 0) 1)))
                 (x86-push cgc (x86-rax))
                  
                 (let ((succ (if (> (length args) 0)
                                ;; If args, then compile args
                                (gen-ast-l args lazy-operator #f)
                                ;; Else, compile call
                                lazy-operator)))
                            
                    (jump-to-version cgc succ (ctx-push ctx CTX_RETAD)))))))))

;;
;; Make lazy code from NUMBER OPERATOR
;;
(define (mlc-op-num ast succ op)
  (letrec (   ;; Lazy code used if type test fail
              (lazy-fail (make-lazy-code (lambda (cgc ctx) (gen-error cgc ERR_NUM_EXPECTED))))
              ;; Lazy code of left operand
              (lazy-ast-left  (gen-ast (cadr ast)  lazy-ast-right #f))
              ;; Lazy code of right operand
              (lazy-ast-right (gen-ast (caddr ast) lazy-main #f))
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
                                            (else                      (gen-error cgc ERR_NUM_EXPECTED))))

                                     ((eq? left-type CTX_UNK)
                                      (cond ((eq? right-type CTX_NUM)     (jump-to-version cgc (gen-dyn-type-test CTX_NUM 1 lctx lazy-code-op ctx lazy-fail) ctx))
                                            ((eq? right-type CTX_UNK) (let* ((right-test (gen-dyn-type-test CTX_NUM 1 lrctx lazy-code-op ctx lazy-fail))
                                                                              (left-test  (gen-dyn-type-test CTX_NUM 0 lctx right-test ctx lazy-fail)))
                                                                         (jump-to-version cgc left-test ctx)))
                                            (else                      (gen-error cgc ERR_NUM_EXPECTED))))
                                     (else (gen-error cgc ERR_NUM_EXPECTED))))))))
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
    (gen-ast-l (cdr ast) lazy-code-op #f)))

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
                                      (cond ((eq? op '$procedure?) (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_PROCEDURE)))) ;; STAG_PROCEDURE << 3
                                            ((eq? op '$pair?)      (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_PAIR))))      ;; STAG_PAIR << 3
                                            ((eq? op '$vector?)    (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_VECTOR))))    ;; STAG_VECTOR << 3
                                            ((eq? op '$string?)    (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_STRING))))    ;; STAG_STRING << 3
                                            ((eq? op '$symbol?)    (x86-cmp cgc (x86-rbx) (x86-imm-int (* 8 STAG_SYMBOL))))    ;; STAG_SYMBOL << 3
                                            (else (error "NYI")))
                                      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
                                      (x86-jne cgc label-done)
                                      (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))))
                          ;;
                          (x86-label cgc label-done)
                          (x86-push  cgc (x86-rax))
                          (jump-to-version cgc succ (ctx-push (ctx-pop ctx) CTX_BOOL)))))))
    (gen-ast (cadr ast) lazy-test #f)))

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

;; Global closure context table
(define global-cc-table '())

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
        (error "Compiler error : set a non mutable var"))))

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
               
        (error "Compiler error : set a non mutable var"))))

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
  (cond ;; Literal
        ((literal? ast) '())
        ;; Symbol
        ((symbol? ast)
          (if (and (assoc ast (ctx-env ctx))
                   (not (member ast clo-env)))
              (list ast)
              '()))
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
                  ((eq? op 'lambda) (free-vars (caddr ast) (append (cadr ast) clo-env) ctx))
                  ;; Special
                  ((member op '(set! $error $string-set! $make-string $symbol->string $string->symbol $string-length $string-ref $integer->char $char->integer $vector-set! $vector-ref $vector-length $make-vector $cons $set-car! $set-cdr! $car $cdr $$putchar $+ $- $* $quotient $modulo $< $> $= $eq? $symbol? $string? $char? $vector? $number? $procedure? $pair?)) (free-vars-l (cdr ast) clo-env ctx))
                  ;; Call
                  (else (free-vars-l ast clo-env ctx)))))))

;;
;; MUTABLE VARS
;;

;; Return all mutable vars used by the list of ast
(define (mutable-vars-l lst params)
  (if (null? lst)
    '()
    (append (mutable-vars (car lst) params) (mutable-vars-l (cdr lst) params))))

;; Return all mutable vars used by ast
(define (mutable-vars ast params)
  (cond ;; Literal & Symbol 
        ((or (null? ast) (char? ast) (string? ast) (number? ast) (boolean? ast) (symbol? ast)) '())
        ;; Pair
        ((pair? ast)
           (let ((op (car ast)))
              (cond ((eq? op 'lambda) (mutable-vars (caddr ast) (set-sub params (cadr ast) '())))
                    ((eq? op 'set!)
                      (if (member (cadr ast) params)
                        (list (cadr ast))
                        '()))
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
;; ex : (build-env '(...) '(a b c) 8) -> ((a . 8) (b . 9) (c . 10))
;; mvars contains all mutable vars to tag created identifier objects
(define (build-env mvars ids start)
  (if (null? ids)
    '()
    (cons (cons (car ids) (make-identifier 'local
                                           start
                                           (if (member (car ids) mvars)
                                              '(mutable)
                                              '())))
          (build-env mvars (cdr ids) (+ start 1)))))

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
;; TODO rewrite with GC 
(define (gen-rest-lst cgc ctx nb-pop sp-offset alloc-offset pos)
  (if (= 0 pos)
    (begin  ;; MOV REST PAIR
            (x86-mov cgc (x86-rax) alloc-ptr)
            (x86-sub cgc (x86-rax) (x86-imm-int (* 24 nb-pop)))
            (x86-add cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
            (x86-mov cgc (x86-mem (+ 8 (* 8 nb-pop)) (x86-rsp)) (x86-rax)) ;; 32
            
            ;; MOV CTXID AND CLOSURE
            (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))
            (x86-mov cgc (x86-mem (+ 8 (* 8 (- nb-pop 1))) (x86-rsp)) (x86-rax))
            (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
            (x86-mov cgc (x86-mem (+ 0 (* 8 (- nb-pop 1))) (x86-rsp)) (x86-rax))
            (x86-add cgc (x86-rsp) (x86-imm-int (* 8 (- nb-pop 1)))))

    (let ((header (mem-header 3 STAG_PAIR)))

      ;; Alloc pair
      (gen-allocation cgc ctx STAG_PAIR 3)
      
      ;; Write header and car
      (x86-mov cgc (x86-rax) (x86-imm-int header))
      (x86-mov cgc (x86-mem -24 alloc-ptr) (x86-rax))
      (x86-mov cgc (x86-rax) (x86-mem sp-offset (x86-rsp)))
      (x86-mov cgc (x86-mem -16 alloc-ptr) (x86-rax))
      
      ;; Write cdr
      (if (= 1 pos)
        (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
        (begin (x86-mov cgc (x86-rax) alloc-ptr)
               (x86-add cgc (x86-rax) (x86-imm-int (+ 0 TAG_MEMOBJ)))))
      (x86-mov cgc (x86-mem -8 alloc-ptr) (x86-rax))
      
      (gen-rest-lst cgc ctx nb-pop (- sp-offset 8) alloc-offset (- pos 1)))))    

;; Is the v a literal ?
(define (literal? v)
   (or (char? v) (number? v) (string? v) (boolean? v) (null? v)))