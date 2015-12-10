
;;-----------------------------------------------------------------------------
;; x86 Codegen utils
(define (x86-codegen-void cgc)
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

(define (x86-codegen-set-bool cgc b)
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding b)))
  (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax)))

(define (x86-codegen-push-n cgc imm n)
  (call-n n x86-push cgc (x86-imm-int (obj-encoding n))))

;;-----------------------------------------------------------------------------
;; Define
(define (x86-codegen-define-id cgc)
  (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
  (x86-mov cgc (x86-mem (* 8 (length globals)) (x86-r10)) (x86-rax)))

(define (x86-codegen-define-bind cgc pos)
  (x86-pop cgc (x86-rax))
  (x86-mov cgc (x86-mem (* 8 pos) (x86-r10)) (x86-rax))
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

;;-----------------------------------------------------------------------------
;; Special forms
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Begin
(define (x86-codegen-begin-out cgc nb-expr)
  (x86-pop  cgc (x86-rax)) ;; Pop result of last expr
  (x86-add  cgc (x86-rsp) (x86-imm-int (* 8 nb-expr))) ;; Clean stack
  (x86-push cgc (x86-rax))) ;; Push result

;;-----------------------------------------------------------------------------
;; Bindings (let, let*, letrec)
;; TODO
(define (x86-codegen-binding-clear cgc nb-slots)
  (x86-pop cgc (x86-rax))
  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 nb-slots)))
  (x86-push cgc (x86-rax)))

(define (x86-codegen-letrec-end cgc nb-slots)
  (x86-add cgc (x86-rsp) (x86-imm-int (* 8 nb-slots))))

(define (x86-codegen-letrec-bind cgc from to)
  (x86-mov cgc (x86-rax) (x86-mem (* 8 from) (x86-rsp)))
  (x86-mov cgc (x86-rbx) (x86-mem (* 8 to) (x86-rsp)))
  (x86-mov cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rbx)) (x86-rax)))

;;-----------------------------------------------------------------------------
;; Values
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; Literal
(define (x86-codegen-literal cgc lit)
  (if (and (number? lit)
           (or (>= lit (expt 2 29))   ;; 2^(32-1-2) (32bits-sign-tags)
               (<  lit (expt 2 28))))
      (begin (x86-mov  cgc (x86-rax) (x86-imm-int (obj-encoding lit)))
             (x86-push cgc (x86-rax)))
      (x86-push cgc (x86-imm-int (obj-encoding lit)))))

;;-----------------------------------------------------------------------------
;; Flonum
(define (x86-codegen-flonum cgc immediate)
  (let ((header-word (mem-header 2 STAG_FLONUM)))
    (gen-allocation cgc #f STAG_FLONUM 2) ;; TODO #f
    ;; Write header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Write number
    (x86-mov cgc (x86-rax) (x86-imm-int immediate))
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
    ;; Push flonum
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; Symbol
(define (x86-codegen-symbol cgc sym)
  (let ((qword (get-symbol-qword sym)))
    (x86-mov cgc (x86-rax) (x86-imm-int qword))
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; String
(define (x86-codegen-string cgc str)
  (let* ((len (string-length str))
         (size (arithmetic-shift (bitwise-and (+ len 8) (bitwise-not 7)) -3))
         (header-word (mem-header (+ size 2) STAG_STRING)))

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
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-push cgc (x86-rax))))

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
(define (x86-codegen-pair cgc)
  (let ((header-word (mem-header 3 STAG_PAIR)))
    ;; Alloc
    (gen-allocation cgc #f STAG_PAIR 3)
    ;; Write object header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    (x86-pop cgc (x86-rbx)) ;; pop CDR
    (x86-pop cgc (x86-rax)) ;; pop CAR
    ;; Write pair
    (x86-mov cgc (x86-mem 8 alloc-ptr)  (x86-rax))
    (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rbx))
    ;; Tag,Push closure and update alloc-ptr
    (x86-mov cgc (x86-rax) alloc-ptr)
    (x86-add cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; Functions
;;-----------------------------------------------------------------------------

;; Generate specialized function prologue with rest param and actual == formal
(define (x86-codegen-prologue-rest= cgc)
  ;; Shift closure
  (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
  (x86-push cgc (x86-rax))
  ;; Mov '() in rest param slot
  (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding '())))
  (x86-mov cgc (x86-mem 8 (x86-rsp)) (x86-rax)))

;; Generate specialized function prologue with rest param and actual > formal
(define (x86-codegen-prologue-rest> cgc restlen)

  ;; Create a pair with
  ;; car: [rsp+sp_offset]
  ;; cdr: top of stack
  ;; Then, push this pair and create next until pos == 0
  (define (gen-rest-lst cgc pos nb sp-offset)
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
               (gen-allocation cgc #f STAG_PAIR 3)
               (let ((header (mem-header 3 STAG_PAIR)))
                 ;; Write header in pair
                 (x86-mov cgc (x86-rax) (x86-imm-int header))
                 (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
                 ;; Get car from stack (arg slot) and write in pair
                 (x86-mov cgc (x86-rax) (x86-mem sp-offset (x86-rsp)))
                 (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
                 ;; Get cdr from stack (top of stack) and write in pair
                 (x86-pop cgc (x86-rax))
                 (x86-mov cgc (x86-mem 16 alloc-ptr) (x86-rax))
                 ;; Tag & push
                 (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
                 (x86-push cgc (x86-rax)))
               ;; Create next pair
               (gen-rest-lst cgc (- pos 1) nb (+ sp-offset 8)))))

  ;; Build rest argument
  (x86-push cgc (x86-imm-int (obj-encoding '())))
  ;; Gen code to create rest list from stack
  (gen-rest-lst cgc restlen restlen 16)) ;; 16 TODO

;; Generate generic function prologue
(define (x86-codegen-prologue-gen cgc rest? nb-formal err-label)
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

;; Build closure using a single entry point
(define (x86-codegen-closure-ep cgc ctx ep-loc fvars)
  (let* ((closure-size  (+ 2 (length fvars))) ;; header, entry point
         (header-word (mem-header closure-size STAG_PROCEDURE)))
    ;; 0 - Alloc closure
    (gen-allocation cgc #f STAG_PROCEDURE closure-size)
    ;; 1 - Write closure header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; 2 - Write entry point
    (x86-mov cgc (x86-rax) (x86-mem (+ 8 (- (obj-encoding ep-loc) 1))))
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
    ;; 3 - Write free vars
    (gen-free-vars cgc fvars ctx 16)
    ;; 4 - Tag and push closure
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-push cgc (x86-rax))))

;; Build closure using a cctable with multiple entry points
(define (x86-codegen-closure-cc cgc ctx cctable-loc fvars)
  (let* ((closure-size  (+ 2 (length fvars))) ;; header, cctable
         (header-word (mem-header closure-size STAG_PROCEDURE)))
    ;; 0 - Alloc closure
    (gen-allocation cgc #f STAG_PROCEDURE closure-size)
    ;; 1 - Write closure header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; 2 - Write cctable ptr
    (x86-mov cgc (x86-rax) (x86-imm-int cctable-loc))
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rax))
    ;; 3 - Write free vars
    (gen-free-vars cgc fvars ctx 16)
    ;; 4 - Tag and push closure
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-push cgc (x86-rax))))

;; Generate function return using a return address
(define (x86-codegen-return-rp cgc retaddr-offset)
  ;; Pop return value
  (x86-pop  cgc (x86-rax))
  ;; Update SP to ret addr
  (x86-add  cgc (x86-rsp) (x86-imm-int retaddr-offset))
  ;; Jump to continuation (ret)
  ;; Do not use ret instruction. This ret is not paired with a call. Using a ret would cause branch misprediction
  ;; TODO: use call/ret if no entry points and no return points used
  (x86-pop cgc (x86-rdx))
  (x86-jmp cgc (x86-rdx)))

;; Generate function return using a crtable
(define (x86-codegen-return-cr cgc retaddr-offset crtable-offset)
  ;; Pop return value
  (x86-pop  cgc (x86-rax))
  ;; Update SP to ret addr
  (x86-add  cgc (x86-rsp) (x86-imm-int retaddr-offset))
  ;; Get return point from cr table and jump to it
  (x86-pop cgc (x86-rdx))
  (x86-mov cgc (x86-rbx) (x86-mem crtable-offset (x86-rdx)))
  (x86-mov cgc (x86-r11) (x86-imm-int crtable-offset))
  (x86-jmp cgc (x86-rbx)))

;;-----------------------------------------------------------------------------
;; Operators
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; N-ary arithmetic operators
;;TODO

;;-----------------------------------------------------------------------------
;; N-ary comparison operators
;;TODO

;;-----------------------------------------------------------------------------
;; Binary operators
(define (x86-codegen-binop cgc op)
  (x86-pop cgc (x86-rbx)) ;; Pop right
  (x86-pop cgc (x86-rax)) ;; Pop left
  (x86-sar cgc (x86-rax) (x86-imm-int 2))
  (x86-sar cgc (x86-rbx) (x86-imm-int 2))
  (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
  (x86-je  cgc (get-label-error ERR_DIVIDE_ZERO)) ;; Check '/0'
  (x86-cqo cgc)
  (x86-idiv cgc (x86-rbx))
  (cond ((eq? op 'quotient)
          (x86-shl cgc (x86-rax) (x86-imm-int 2))
          (x86-push cgc (x86-rax)))
        ((eq? op 'remainder)
          (x86-shl cgc (x86-rdx) (x86-imm-int 2))
          (x86-push cgc (x86-rdx)))
        ((eq? op 'modulo)
          (x86-mov cgc (x86-rax) (x86-rdx)) ;; (a%b) in rax, b in rbx
          (x86-add cgc (x86-rax) (x86-rbx)) ;; (a%b + b) in rax
          (x86-cqo cgc)
          (x86-idiv cgc (x86-rbx))
          (x86-shl cgc (x86-rdx) (x86-imm-int 2))
          (x86-push cgc (x86-rdx)))))

;;-----------------------------------------------------------------------------
;; Primitives
;;-----------------------------------------------------------------------------

;;-----------------------------------------------------------------------------
;; SPECIAL $$print-flonum: call gambit at the moment. TODO: write print flonum asm code
(define (x86-codegen-print-flonum cgc)
  (x86-pop cgc (x86-rax))
  ;; NOTE: This uses Gambit function to print a flonum (because LC uses the same flonum encoding)
  (gen-print-msg cgc (x86-rax) #f #f)
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

;;-----------------------------------------------------------------------------
;; not
(define (x86-codegen-not cgc)
  (let ((label-done
          (asm-make-label cgc (new-sym 'done))))
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
    (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
    (x86-je  cgc label-done)
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
    (x86-label cgc label-done)
    (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))))

;;-----------------------------------------------------------------------------
;; eq?
(define (x86-codegen-eq? cgc)
  (let ((label-done (asm-make-label cgc (new-sym 'done))))
    (x86-pop cgc (x86-rax))
    (x86-cmp cgc (x86-mem 0 (x86-rsp)) (x86-rax))
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
    (x86-je  cgc label-done)
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
    (x86-label cgc label-done)
    (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))))

;;-----------------------------------------------------------------------------
;; car/cdr
(define (x86-codegen-car/cdr cgc op)
  (let ((offset
          (if (eq? op 'car)
              (-  8 TAG_MEMOBJ)
              (- 16 TAG_MEMOBJ))))
    (x86-pop cgc (x86-rax))
    (x86-mov cgc (x86-rax) (x86-mem offset (x86-rax)))
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; set-car!/set-cdr!
(define (x86-codegen-scar/scdr cgc op)
  (let ((offset
          (if (eq? op 'set-car!)
              (-  8 TAG_MEMOBJ)
              (- 16 TAG_MEMOBJ))))
    (x86-pop cgc (x86-rax)) ;; val
    (x86-pop cgc (x86-rbx)) ;; pair
    (x86-mov cgc (x86-mem offset (x86-rbx)) (x86-rax))
    (x86-push cgc (x86-imm-int ENCODING_VOID))))

;;-----------------------------------------------------------------------------
;; current-input/output-port
(define (x86-codegen-current-io-port cgc op)
  (let ((block-offset (if (eq? op 'current-output-port) 8 24)))
    (x86-mov cgc (x86-rax) (x86-imm-int (+ TAG_MEMOBJ block-offset block-addr)))
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; close-input/output-port
(define (x86-codegen-close-io-port cgc)
  (gen-syscall-close cgc)
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

;;-----------------------------------------------------------------------------
;; open-input/output-port
(define (x86-codegen-open-io-file cgc op)
  (let* ((direction   (if (eq? op 'open-output-file) 'out 'in))
         (stag        (if (eq? direction 'in) STAG_IPORT STAG_OPORT))
         (header-word (mem-header 2 stag)))
    ;; Gen 'open' syscall, file descriptor in rax
    (gen-syscall-open cgc direction)
    (x86-mov cgc (x86-rbx) (x86-rax))
    ;; Allocate port object
    (gen-allocation cgc #f stag 2)
    ;; Mov header
    (x86-mov cgc (x86-rax) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rax))
    ;; Mov descriptor
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rbx))
    ;; Tag & push
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax))))

;;-----------------------------------------------------------------------------
;; eof-object?
(define (x86-codegen-eof? cgc)
  (let ((label-end (asm-make-label #f (new-sym 'label-end))))
    (x86-pop cgc (x86-rax))
    (x86-cmp cgc (x86-rax) (x86-imm-int ENCODING_EOF))
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #f)))
    (x86-jne cgc label-end)
    (x86-mov cgc (x86-rax) (x86-imm-int (obj-encoding #t)))
    (x86-label cgc label-end)
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; read-char
(define (x86-codegen-read-char cgc)
  ;; Gen 'read' syscall (read 1 byte), encoded value (char or eof) in rax
  (gen-syscall-read-char cgc)
  ;; Push encoded result
  (x86-mov cgc (x86-mem 0 (x86-rsp)) (x86-rax)))

;;-----------------------------------------------------------------------------
;; write-char
(define (x86-codegen-write-char cgc)
  ;; Gen 'read' syscall, encoded value (char or eof) in rax
  (gen-syscall-write-char cgc)
  (x86-add cgc (x86-rsp) (x86-imm-int 16)) ;; NOTE: clean stack in gen-syscall-write-char?
  ;; Push encoded result
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

;;-----------------------------------------------------------------------------
;; char->integer/integer->char
(define (x86-codegen-ch<->int cgc op)
  (if (eq? op 'char->integer)
      (x86-xor cgc (x86-mem 0 (x86-rsp)) (x86-imm-int TAG_SPECIAL) 8)
      (x86-or  cgc (x86-mem 0 (x86-rsp)) (x86-imm-int TAG_SPECIAL) 8)))

;;-----------------------------------------------------------------------------
;; make-string
(define (x86-codegen-make-string cgc init-value?)
  (let* ((header-word (mem-header 3 STAG_STRING)))
    ;; Pop encoded length
    (if init-value?
        (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))
        (x86-pop cgc (x86-rax)))
    (x86-mov cgc (x86-rbx) (x86-rax))
    ;; Nb chars to byte size
    (x86-shr cgc (x86-rax) (x86-imm-int 2))
    (x86-and cgc (x86-rax) (x86-imm-int (bitwise-not 7)))
    (x86-shr cgc (x86-rax) (x86-imm-int 1))
    (x86-mov cgc (x86-rsi) (x86-rax))
    ;; Alloc
    (gen-allocation cgc #f STAG_STRING 3 #t)
    ;; Fill string
    (x86-push cgc (x86-rbx))
    ;;
    (x86-mov cgc (x86-rax) alloc-ptr)
    (let ((label-loop (asm-make-label cgc (new-sym 'fill-string-loop)))
          (label-end  (asm-make-label cgc (new-sym 'fill-string-end))))

      (if init-value?
          (begin (x86-mov cgc (x86-rdx) (x86-mem 8 (x86-rsp)))
                 (x86-sar cgc (x86-rdx) (x86-imm-int 2))))
      ;; LOOP:
      ;;   if (rbx == 0) jump END
      (x86-label cgc label-loop)
      (x86-cmp cgc (x86-rbx) (x86-imm-int 0))
      (x86-jle  cgc label-end)
        ;; Write init value
        (if init-value?
            (begin (x86-mov cgc (x86-mem 16 (x86-rax)) (x86-dl)) ;; Write char
                   (x86-add cgc (x86-rax) (x86-imm-int 1)) ;; Update offset
                   (x86-sub cgc (x86-rbx) (x86-imm-int 4))) ;; Remove 1 (=1*4=4) to encoded number (remaining els)
            (begin (x86-mov cgc (x86-mem 16 (x86-rax)) (x86-imm-int 0) 64) ;; Write 0 in 8 chars
                   (x86-add cgc (x86-rax) (x86-imm-int 8)) ;; Update offset
                   (x86-sub cgc (x86-rbx) (x86-imm-int 32)))) ;; Remove 8 (=8*4=32) to encoded number (remaining els)
        ;; Loop
        (x86-jmp cgc label-loop)
      ;; END:
      (x86-label cgc label-end)
      (x86-pop cgc (x86-rbx)))
    ;; Clean stack
    (if init-value?
        (x86-add cgc (x86-rsp) (x86-imm-int 16)))
    ;; Write encoded length
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rbx))
    ;; Write header
    (x86-shl cgc (x86-rsi) (x86-imm-int 6))
    (x86-add cgc (x86-rsi) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rsi))
    ;; Push string
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; make-vector
(define (x86-codegen-make-vector cgc init-value?)
  (let* ((header-word (mem-header 2 STAG_VECTOR)))
    ;; Pop encoded length
    (if init-value?
        (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))
        (x86-pop cgc (x86-rax)))
    (x86-mov cgc (x86-rbx) (x86-rax))
    ;; Alloc
    (gen-allocation cgc #f STAG_VECTOR 2 #t)
    ;; Get vector position in R15
    (x86-mov cgc (x86-r15) alloc-ptr)
    ;; Fill vector
    (x86-push cgc (x86-rbx))
    ;; Init value in RAX (0)
    (if init-value?
        (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))
        (x86-mov cgc (x86-rax) (x86-imm-int 0)))
    ;;
    (let ((label-loop (asm-make-label #f (new-sym 'fill-vector-loop)))
          (label-end  (asm-make-label #f (new-sym 'fill-vector-end))))

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
      (x86-pop cgc (x86-rbx)))
    ;; Clean stack
    (if init-value?
        (x86-add cgc (x86-rsp) (x86-imm-int 16)))
    ;; Write encoded length
    (x86-mov cgc (x86-mem 8 alloc-ptr) (x86-rbx))
    ;; Write header
    (x86-shl cgc (x86-rbx) (x86-imm-int 6))
    (x86-add cgc (x86-rbx) (x86-imm-int header-word))
    (x86-mov cgc (x86-mem 0 alloc-ptr) (x86-rbx))
    ;; Push vector
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    (x86-push cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; string->symbol
(define (x86-codegen-str->sym cgc)
  (gen-interned-symbol cgc))

;;-----------------------------------------------------------------------------
;; symbol->string
(define (x86-codegen-sym->str cgc)
  ;; Alloc
  (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
  (x86-sub cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
  (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rax)))
  (x86-shr cgc (x86-rax) (x86-imm-int 8))
  (x86-shl cgc (x86-rax) (x86-imm-int 2)) ;; Length in rax
  (gen-allocation cgc #f STAG_STRING 0 #t)
  ;; String address in rbx
  (x86-mov cgc (x86-rbx) alloc-ptr)
  ;; Symbol address in rax
  (x86-pop cgc (x86-rax))
  (x86-sub cgc (x86-rax) (x86-imm-int TAG_MEMOBJ))
  ;; Mov length in string
  (x86-mov cgc (x86-r15) (x86-mem 8 (x86-rax)))
  (x86-mov cgc (x86-mem 8 (x86-rbx)) (x86-r15))
  ;; Mov header in string
  (x86-mov cgc (x86-r15) (x86-mem 0 (x86-rax)))
  (x86-add cgc (x86-r15) (x86-imm-int (arithmetic-shift (- STAG_STRING STAG_SYMBOL) 3)))
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

    (x86-label cgc label-fin)
    (x86-add cgc (x86-rbx) (x86-imm-int TAG_MEMOBJ))
    (x86-push cgc (x86-rbx))))


;;-----------------------------------------------------------------------------
;; vector/string-length
(define (x86-codegen-vec/str-length cgc)
  (x86-pop cgc (x86-rax)) ;; Pop vector
  (x86-push cgc (x86-mem (- 8 TAG_MEMOBJ) (x86-rax))))

;;-----------------------------------------------------------------------------
;; vector-ref
(define (x86-codegen-vector-ref cgc)
  (x86-pop cgc (x86-rax)) ;; Pop index
  (x86-pop cgc (x86-rbx)) ;; Pop vector
  (x86-shl cgc (x86-rax) (x86-imm-int 1))
  (x86-add cgc (x86-rbx) (x86-rax))
  (x86-push cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx))))

;;-----------------------------------------------------------------------------
;; string-ref
(define (x86-codegen-string-ref cgc)
  (x86-pop cgc (x86-rax)) ;; Pop index
  (x86-pop cgc (x86-rbx)) ;; Pop string
  (x86-shr cgc (x86-rax) (x86-imm-int 2)) ;; Decode position
  (x86-mov cgc (x86-al) (x86-mem (- 16 TAG_MEMOBJ) (x86-rax) (x86-rbx))) ;; Get Char
  (x86-and cgc (x86-rax) (x86-imm-int 255)) ;; Clear bits before al
  (x86-shl cgc (x86-rax) (x86-imm-int 2)) ;; Encode char
  (x86-add cgc (x86-rax) (x86-imm-int TAG_SPECIAL))
  (x86-push cgc (x86-rax))) ;; Push char

;;-----------------------------------------------------------------------------
;; vector-set!
(define (x86-codegen-vector-set! cgc)
  (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))  ;; Get index
  (x86-mov cgc (x86-rbx) (x86-mem 16 (x86-rsp))) ;; Get vector
  (x86-mov cgc (x86-rdx) (x86-mem 0 (x86-rsp)))  ;; Get new value
  (x86-shl cgc (x86-rax) (x86-imm-int 1))
  (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx) (x86-rax)) (x86-rdx))
  (x86-add cgc (x86-rsp) (x86-imm-int 24))
  (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
  (x86-push cgc (x86-rax)))

;;-----------------------------------------------------------------------------
;; string-set!
(define (x86-codegen-string-set! cgc)
  (x86-mov cgc (x86-rax) (x86-mem 8 (x86-rsp)))  ;; Get index
  (x86-mov cgc (x86-rbx) (x86-mem 16 (x86-rsp))) ;; Get string
  (x86-mov cgc (x86-rdx) (x86-mem 0 (x86-rsp)))  ;; Get new value
  (x86-shr cgc (x86-rdx) (x86-imm-int 2))
  (x86-shr cgc (x86-rax) (x86-imm-int 2))
  (x86-mov cgc (x86-mem (- 16 TAG_MEMOBJ) (x86-rbx) (x86-rax)) (x86-dl))
  (x86-add cgc (x86-rsp) (x86-imm-int 24))
  (x86-mov cgc (x86-rax) (x86-imm-int ENCODING_VOID))
  (x86-push cgc (x86-rax)))
