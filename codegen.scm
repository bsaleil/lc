
;;-----------------------------------------------------------------------------
;; x86 Codegen utils

(define (x86-codegen-void cgc)
  (x86-push cgc (x86-imm-int ENCODING_VOID)))

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
;; Primitives
;;-----------------------------------------------------------------------------

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
