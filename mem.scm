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

(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; TODO : 'encoded' as parameter to avoid shr/shl of make-string

;;-----------------------------------------------------------------------------

;; Print msg if gc log is enabled
(define-macro (log-gc msg)
  `(if opt-verbose-gc
       (println ,msg)))

;; Out of memory error
(define (out-of-memory)
  (println "GC: Out of memory.")
  (exit 1))

;; When the GC copy an object to the to-space, it writes the
;; BROKEN-HEART value at [addr + 0] and write forwarding
;; pointer at [addr + 8].
(define BROKEN-HEART -1)

;; Is the object at 'addr' a special object ?
(define (is-special-object addr)
  (cond ((and (>= addr block-addr)
              (< addr (+ block-addr (* 8 global-offset))))
          #t)
        ((and (>= addr (##foreign-address sym-space))
              (< addr (+ ( ##foreign-address sym-space) sym-space-len)))
          #t)
        (else #f)))

;;-----------------------------------------------------------------------------
;; ALLOCATOR

;; Generate an heap object header
;; NOTE : 'life' is fixed as 6 for now.
;(define (memobj-header length stag life)
(define (mem-header length stag)
    ;; => Length (56 bits) | sTag (5 bits) | Life (3 bits)
    (+ (arithmetic-shift length 8) (arithmetic-shift stag 3) 6))

;; Generate allocation code
;; use-rax to #t to allocate from register (with optional imm length)
;; use-rax to #f to allocate only from immediate
(define (gen-allocation cgc ctx stag length #!optional use-rax)
  (if use-rax
      (gen-alloc-regimm cgc stag length)
      (gen-alloc-imm    cgc stag length)))

;; Allocate length(imm) bytes in heap
;; DESTROY RAX !!
(define (gen-alloc-imm cgc stag length)
  (let ((label-alloc-end (asm-make-label #f (new-sym 'alloc-end))))
    (x86-sub cgc alloc-ptr (x86-imm-int (* length 8)))
    (x86-mov cgc (x86-rax) (x86-imm-int (+ (* 5 8) block-addr)))
    (x86-cmp cgc alloc-ptr (x86-mem 0 (x86-rax)) 64)
    (x86-jge cgc label-alloc-end)
      (x86-mov cgc (x86-rax) (x86-imm-int (* length 8)))
      (x86-call cgc label-gc-trampoline)
    (x86-label cgc label-alloc-end)))

;; Allocate RAX(reg) + length(imm) bytes in heap
;; DESTROY RAX !!
;; Rax contains the encoded number of 64bits words to alloc
;; Ex. For (make-vector 3) rax contains 12
(define (gen-alloc-regimm cgc stag length)
  (let ((label-alloc-end (asm-make-label #f (new-sym 'alloc-end))))
    (x86-push cgc (x86-rax))
    ;; rax = rax*2 + length*8 (nbbytes in rax + cst nbbytes)
    (x86-lea cgc (x86-rax) (x86-mem (* length 8) (x86-rax) (x86-rax) 0))
    (x86-sub cgc alloc-ptr (x86-rax))
    (x86-mov cgc (x86-rax) (x86-imm-int (+ (* 5 8) block-addr)))
    (x86-cmp cgc alloc-ptr (x86-mem 0 (x86-rax)) 64)
    (x86-jge cgc label-alloc-end)
      (x86-mov cgc (x86-rax) (x86-mem 0 (x86-rsp)))
      (x86-lea cgc (x86-rax) (x86-mem (* length 8) (x86-rax) (x86-rax) 0))
      (x86-call cgc label-gc-trampoline)
    (x86-label cgc label-alloc-end)
    (x86-pop cgc (x86-rax))))

;;-----------------------------------------------------------------------------
;; COLLECTOR :
;;
;; This garbage collector implements Cheney's algorithm.
;; The algorithm uses 4 phases :
;;   1 - Copy all roots from stack
;;   2 - Copy all roots from global values
;;   3 - Scan copied roots to copy referenced objects
;;   4 - Update to/from-space pointers and alloc pointer
;;
;; When the GC copy an object it replaces the header [obj-addr + 0] with the BROKEN-HEART
;; and replaces the first slot [obj-addr + 8] with forwarding pointer to the new addr
;; Each time the GC tries to copy an object it first checks if the object is already copied
;; (i.e. [obj-addr + 0] == BROKEN-HEART). If already copied it only patches the reference from forwarding
;; pointer.
;;
;;
;;-----------------------------------------------------------------------------
;; COLLECTOR - Utils
;;

;; Pretty print stag
(define (pp-stag stag)
  (cond ((eq? stag STAG_PROCEDURE) (println "PROCEDURE"))
        ((eq? stag STAG_PAIR)      (println "PAIR"))
        ((eq? stag STAG_MOBJECT)   (println "MOBJECT"))
        ((eq? stag STAG_VECTOR)    (println "VECTOR"))
        ((eq? stag STAG_STRING)    (println "STRING"))
        ((eq? stag STAG_IPORT)     (println "INPUT_PORT"))
        ((eq? stag STAG_OPORT)     (println "OUTPUT_PORT"))
        ((eq? stag STAG_SYMBOL)    (println "SYMBOL"))
        ((eq? stag STAG_CCTABLE)   (println "CCTABLE"))
        ((eq? stag STAG_FLONUM)    (println "FLONUM"))
        (else
          (print "Unknown stag ")
          (println (number->string stag)))))

;; Pretty print header
;; 'header' is a list: (head stag length)
(define (pp-header header)
  (println "--Header:")
  (print   "  head   = ") (println (car header))
  (print   "  stag   = ") (pp-stag (cadr header))
  (print   "  length = ") (println (caddr header)))

;; Return tag from qword (qword & 3)
(define (get-tag qword)
  (bitwise-and qword 3))

;; Read header at address 'addr'
;; Return a list representing a formatted header :
;; ex. (head stag length)
(define (read-header addr)
  (let ((qword (get-i64 addr)))
    (list (bitwise-and qword 7)                         ;; Get head
          (arithmetic-shift (bitwise-and qword 248) -3) ;; Get stag
          (arithmetic-shift qword -8))))                ;; Get length

;;; Copy 'len' bytes from address 'from' to copy-ptr
(define (copy-bytes from copy-ptr len)

  ;; Copy len bytes from left to right
  (define (copy-bytes-h from to len)
    (if (> len 0)
        (begin (put-i64 to (get-i64 from))
               (copy-bytes-h (+ from 8) (+ to 8) (- len 1)))
        to))

  ;; 'copy-ptr' points to the header of the previously copied object
  (set! copy-ptr (- copy-ptr (* 8 len)))
  ;; Check for available space
  (if (< copy-ptr (- to-space space-len))
      (out-of-memory))
  ;; Copy from new copy-ptr position
  (copy-bytes-h from copy-ptr len)
  copy-ptr)

;;-----------------------------------------------------------------------------
;; COLLECTOR - Copy phase

;; Copy root
;; Check if the value at 'slot-addr' is a root (memory allocated object).
;; If it's a non yet copied root, then copy to to-space and patch slot
;; If it's a copied root then update slot from forwarding pointer
;; If it's not a root do nothing
;; Return new position of copy-ptr
(define (copy-root slot-addr current-copy-ptr)

   (let* ((value (get-i64 slot-addr))
          (tag   (get-tag value)))

     (assert (or (= tag TAG_MEMOBJ)
                 (= tag TAG_NUMBER)
                 (= tag TAG_SPECIAL))
             "Internal error")

     (if (= tag TAG_MEMOBJ)
         (let* (;; Object address in heap
                (obj-addr (- value tag))
                ;; Object header
                (header-qword (get-i64 obj-addr)))

           (cond ;; If it is a special object do nothing
                 ((is-special-object obj-addr) #t)
                 ;; Header is BH
                 ;; Patch memory slot
                 ((= header-qword BROKEN-HEART)
                    (let ((new-pos (get-i64 (+ 8 obj-addr))))
                      (put-i64 slot-addr new-pos)))
                 (else
                    ;; Object is not yet copied
                    ;; Copy object and get new copy-ptr pos
                    (let* (;; Object header
                          (header (read-header obj-addr))
                          ;; Object stag
                          (stag (cadr header))
                          ;; Object length
                          (length (caddr header))
                          ;;
                          (c (copy-bytes obj-addr current-copy-ptr length)))

                       ;; Write BH
                       (put-i64 obj-addr BROKEN-HEART)
                       ;; Write new position (tagged)
                       (put-i64 (+ 8 obj-addr) (+ c TAG_MEMOBJ))
                       ;; Patch slot
                       (put-i64 slot-addr (+ c TAG_MEMOBJ))
                       ;; Update copy-ptr
                       (set! current-copy-ptr c))))))

     current-copy-ptr))

;;---------------
;; STACK ROOTS
;; Copy all stack roots to to-space
;; sbegin: First stack address to scan
;; send:   Last stack address to scan
;; current-copy-ptr: current position of copy-ptr in to-space
(define (copy-stack-roots sbegin send current-copy-ptr)

  (if (< sbegin send)
      ;; All roots are copied then return new position of copy-ptr
      current-copy-ptr
      ;; Else get first stack value and copy
      (let (;; Copy slot if it's a heap obj
            (c (copy-root sbegin current-copy-ptr)))
          ;; Continue with next globals
          (copy-stack-roots (- sbegin 8) send c))))

;;---------------
;; GLOBAL ROOTS
;; Copy all global roots to to-space
;; globals: globals to read
;; current-copy-ptr: current position of copy-ptr in to-space
(define (copy-global-roots globals current-copy-ptr)
  (error "NYI globals is now a table")
  (if (null? globals)
      ;; All roots are copied then return new position of copy-ptr
      current-copy-ptr
      ;; Else get first global value and copy
      (let* (;; Get global info
             (global (car globals))
             ;; Get global address
             (global-addr (+ (* 8 global-offset) (* 8 (cdr global)) block-addr))
             ;; Copy global if it's a heap obj
             (c (copy-root global-addr current-copy-ptr)))

          ;; Continue with next globals
          (copy-global-roots (cdr globals) c))))

;;---------------
;; SCAN REFERENCES
;; Scan copied objects in to-space and update/copy referenced objects
;; copy: current copy ptr value
;; max-to-space: address of the last slot (most right in memory) of the to space
;; The algorithm used ensures that all objects are scanned in an heap growing from right to left in O(n):
;; limit = max-to-space
;; scan = copy
;; do
;; {
;;     previous-copy = copy
;;     while (scan < limit)
;;         scan,copy = scan-object(scan,copy)
;;
;;     limit = previous-copy
;;     scan = copy
;; } while (previous-copy != copy) ;; While there is at least one object copied in previous phase
(define (scan-references copy max-to-space)

  (define (scan-phase scan limit copy)
    (if (>= scan limit)
        copy
        (let ((res (scan-object scan copy)))
          (scan-phase (car res) limit (cdr res)))))

  (define (scan-references previous-copy copy)
    (if (= previous-copy copy) ;; TODO not copy moved
        copy
        (let ((new-copy (scan-phase copy previous-copy copy)))
          (scan-references copy new-copy))))

  (let ((new-copy (scan-phase copy max-to-space copy)))
    (scan-references copy new-copy)))

;; SCAN OBJECT
;; Scan copied object in to-space and update/copy referenced objects
;; scan: address of object to scan
;; copy: address of copy ptr
;; Returns a pair: (new-scan . new-copy)
(define (scan-object scan copy)
  (let* ((header (read-header scan))
         (h (car header))
         (s (cadr header))
         (l (caddr header)))

    (cond ;; Procedure
          ((= s STAG_PROCEDURE) (scan-procedure scan copy h s l))
          ;; Vector
          ((= s STAG_VECTOR) (scan-vector scan copy h s l))
          ;; String, Symbol, I/Oport, Flonum
          ((or (= s STAG_STRING) (= s STAG_SYMBOL) (= s STAG_IPORT) (= s STAG_OPORT) (= s STAG_FLONUM))
            (cons (+ (* 8 l) scan) copy))
          ;; Mobject
          ((= s STAG_MOBJECT) (scan-mobject scan copy h s l))
          ;; Pair
          ((= s STAG_PAIR) (scan-pair scan copy h s l))
          ;; Others
          (else (pp-stag s)
                (pp header)
                (error "Unknown stag while scanning references")))))

;; Scan field
;; This function scans a field of an object (vector element, pair car/cdr, ...)
;; If the field is a reference to a memory allocated object, then copy the object
;; and return new position of copy-ptr
(define (scan-field addr copy)
  (let* ((qword (get-i64 addr))   ;; Get field value
         (tag   (get-tag qword))) ;; Get field tag

    (cond ;; Number & Special
          ;; Nothing to do
          ((or (= tag TAG_NUMBER)
               (= tag TAG_SPECIAL))
              copy)
          ;; Heap object
          ((= tag TAG_MEMOBJ)
              (copy-root addr copy))
          ;; Other
          (else (error "GC - Can't scan object field")))))

;; Scan mobject
;; Scan object reference of mobject
;; Return new scan/copy-ptr position
(define (scan-mobject scan copy head stag length)
  (let ((c (scan-field (+ scan 8) copy)))
    (cons ;; New scan position
          (+ scan 16)
          ;; New copy position
          c)))

;; Scan pair
;; Scan car/cdr references
;; Return new scan/copy-ptr position
(define (scan-pair scan copy head stag length)
  (let* ((ccar (scan-field (+ scan  8) copy))
         (ccdr (scan-field (+ scan 16) ccar)))
    (cons ;; New scan position
          (+ scan 24)
          ;; New copy position
          ccdr)))

;; Scan procedure
;; Scan all free vars
;; Return new scan/copy-ptr position
(define (scan-procedure scan copy head stag length)
  ;; Scan free vars
  (let ((c (scan-freevars (+ scan 16) (- length 2) copy)))
    (cons ;; New scan position
          (+ scan (* 8 length))
          ;; New copy position
          c)))

;; Scan free vars of procedure object
(define (scan-freevars pos nb-free copy)
  (if (= nb-free 0)
      copy
      (let ((c (scan-field pos copy)))
        (scan-freevars (+ pos 8) (- nb-free 1) c))))

;; Scan vector
;; Scan all fields of vector
;; Return new scan/copy-ptr positions
(define (scan-vector scan copy head stag length)
  (let* (;; Get vector length from (vector-pos + 8)
         (length (/ (get-i64 (+ scan 8)) 4))
         ;; Scan vector and get new copy-ptr position
         (c (scan-vector-h (+ scan 16) length copy)))
      (cons ;; New scan position
            (+ scan (* 8 (+ length 2)))
            ;; New copy position
            c)))

(define (scan-vector-h pos length copy)
  (if (= length 0)
      copy
      (let ((c (scan-field pos copy)))
        (scan-vector-h (+ pos 8) (- length 1) c))))

;;---------------
;; GC MAIN
;; This is the entry point of the GC.
;; This function will execute a complete collection phase :
;; copy stack roots, copy global roots, scan objects, update pointers
;; Returns the new position of alloc-ptr

(define (run-gc sp alloc-size)

  (define copy-ptr    to-space)
  (define stack-begin (- (get-i64 block-addr) 8))
  (define stack-end   (+ sp (* 8 (length c-caller-save-regs)) 8)) ;; +8 for return address of gc trampoline

  (log-gc "GC BEGIN")

  ;; 1 - Copy roots from stack
  (log-gc "--------------")
  (log-gc "-- STACK ROOTS")
  (log-gc "--------------")
  (set! copy-ptr (copy-stack-roots stack-begin stack-end copy-ptr))

  ;; 2 - Copy roots from globals
  (log-gc "---------------")
  (log-gc "-- GLOBAL ROOTS")
  (log-gc "---------------")
  (set! copy-ptr (copy-global-roots globals copy-ptr))

  ;; 3 - Copy referenced objects until scan == copy
  (log-gc "--------------")
  (log-gc "-- REFERENCES ")
  (log-gc "--------------")
  (set! copy-ptr (scan-references copy-ptr to-space))

  (if (< (- copy-ptr alloc-size) (- to-space space-len))
      (out-of-memory))

  ;; Update from/to-spaces positions
  (let ((tmp from-space))
    (set! from-space to-space)
    (set! to-space tmp))

  ;; Update heaplimit
  (put-i64 (+ (* 8 5) block-addr) (- from-space space-len))

  (log-gc "GC END")

  ;; Return new position of alloc-ptr
  (- copy-ptr alloc-size))

;;-------------------------
;; HEAP DEV DEBUG FUNCTIONS
;;-------------------------

; ; Fill assoc list STACK_TYPES.
; ; Each stack slot is associated to a tag (or stag if mem obj)
; ; Ex ((addrSlot1 2) (addrSlot2 630) ...)
; ; Used to check that reachable objects from stack keep the same shape after GC.
; (define STACK_TYPES '())
; (define (get-stack-types sbegin send)
;   (if (>= sbegin send)
;       (let* ((slot-val (get-i64 sbegin))
;              (tag (bitwise-and slot-val 3)))
;         (if (= tag TAG_MEMOBJ)
;             (let ((header (get-i64 (- slot-val tag))))
;                (set! STACK_TYPES (cons (cons sbegin header) STACK_TYPES)))
;             (set! STACK_TYPES (cons (cons sbegin tag) STACK_TYPES)))
;         (get-stack-types (- sbegin 8) send))))

; ; Fill assoc list GLOBAL_TYPES.
; ; Each global slot is associated to a tag (or stag if mem obj)
; ; Ex ((addrSlot1 2) (addrSlot2 630) ...)
; ; Used to check that reachable objects from globals keep the same shape after GC.
; (define GLOBAL_TYPES '())
; (define (get-global-types globals)
;   (if (not (null? globals))
;       (let* ((global (car globals))
;              (global-addr (+ 8 (* 8 (cdr global)) block-addr))
;              (slot-val (get-i64 global-addr))
;              (tag (bitwise-and slot-val 3)))
;         (if (= tag TAG_MEMOBJ)
;             (let ((header (get-i64 (- slot-val tag))))
;                (set! GLOBAL_TYPES (cons (cons global header) GLOBAL_TYPES)))
;             (set! GLOBAL_TYPES (cons (cons global tag) GLOBAL_TYPES)))
;         (get-global-types (cdr globals)))))

; ; Check if addr is in to space
; (define (check-intospace addr)
;   (if (or (< addr to-space)
;           (>= addr (+ to-space space-len)))
;     (error "Referenced object out of to-space, heap check failed")
;     #t))

; ; Check if header at addr is not a BH
; (define (check-validheader addr)
;   (let ((h (get-i64 addr)))
;     (if (< h 0)
;         (error "Invalid header, heap check failed")
;         #t)))

; ; Check if the field at 'addr' is in to space and is not a BH
; (define (check-field addr)
;   (let* ((val (get-i64 addr))
;          (tag (bitwise-and val 3)))
;     (if (= tag TAG_MEMOBJ)
;        (begin ;; 1 - Object is in to space
;               (check-intospace (- val tag))
;               ;; 2 - Object header != BROKEN-HEART
;               (check-validheader (- val tag)))
;        #t)))

; ; Check all vector fields
; (define (check-vector scan l)
;   (if (= l 0)
;       #t
;       (begin (check-field scan)
;              (check-vector (+ scan 8) (- l 1)))))

; ; Check all heap objects after GC (scan to space)
; (define (check-heap scan copy)
;   (cond ((> scan copy)
;             (error "Unexpected behavior, heap check failed."))
;         ((= scan copy)
;             #t)
;         ((< scan copy)
;             (let* ((obj-header (read-header scan))
;                    (h (car   obj-header))
;                    (s (cadr  obj-header))
;                    (l (caddr obj-header)))

;               ;; stag
;               (cond ;; CCTABLE & STRING
;                     ((or (= s STAG_CCTABLE)
;                          (= s STAG_STRING)
;                          (= s STAG_IPORT)
;                          (= s STAG_OPORT)
;                          (= s STAG_SYMBOL))
;                         ;; Nothing to do
;                         (check-heap (+ scan (* 8 l)) copy))
;                     ;; PROCEDURE
;                     ((= s STAG_PROCEDURE)
;                         ;; CCtable is special (not tagged)
;                         ;; Free vars are not checked
;                         (check-intospace   (+ scan 8))
;                         ;(check-validheader (+ scan 8))
;                         (check-heap (+ scan (* 8 l)) copy))
;                     ;; MOBJECT
;                     ((= s STAG_MOBJECT)
;                         (check-field (+ scan 8))
;                         (check-heap (+ scan 16) copy))
;                     ;; VECTOR
;                     ((= s STAG_VECTOR)
;                         (check-vector scan l)
;                         (check-heap (+ scan (* 8 l)) copy))
;                     ;; PAIR
;                     ((= s STAG_PAIR)
;                         (check-field (+ scan  8))
;                         (check-field (+ scan 16))
;                         (check-heap (+ scan 24) copy))
;                     (else (pp-stag s)
;                           (error "NYI")))))))
