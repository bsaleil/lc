
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
