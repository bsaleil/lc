(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; Use & !7 for string size (make-string)
;; TODO : 'encoded' as parameter to avoid shr/shl of make-string
;; TODO : mlc-lambda offset 8 for cc-table header
;; TODO : améliorer init-rtlib (x86)
;; TODO : Fusionner copie des locales (stack) et globaless
;; TODO : renommer BH

;;-----------------------------------------------------------------------------

;; Print msg if gc log is enabled
(define-macro (log-gc msg)
  `(if verbose-gc
       (println ,msg)))

;;-----------------------------------------------------------------------------

(define (out-of-memory)
  (println "GC: Out of memory.")
  (exit 1))

;;-----------------------------------------------------------------------------
;; ALLOCATOR

;; Generate allocation code
;; use-rax to #t to allocate from register (with optional imm length)
;; use-rax to #f to allocate only from immediate
(define (gen-allocation cgc ctx stag length #!optional use-rax)
  (if use-rax
      (gen-alloc-regimm cgc stag length)
      (gen-alloc-imm    cgc stag length)))

;; Allocate length(imm) bytes in heap
(define (gen-alloc-imm cgc stag length)
  (let ((label-allok (asm-make-label cgc (new-sym 'alloc-ok))))
	  (x86-mov cgc (x86-rax) alloc-ptr)
	  (x86-add cgc (x86-rax) (x86-imm-int (* length 8)))
	  (x86-mov cgc (x86-rbx) (x86-imm-int (+ from-space space-len)))
   	  (x86-cmp cgc (x86-rax) (x86-rbx)) ;; TODO
   	  (x86-jl  cgc label-allok)
      (x86-sub cgc (x86-rax) alloc-ptr) ;; TODO PARAM RAX = allocsize
      (gen-gc-call cgc)
    ;; Can be allocated
    (x86-label cgc label-allok)
    (x86-add cgc alloc-ptr (x86-imm-int (* length 8)))))

;; Allocate RAX(reg) + length(imm) bytes in heap
;; DESTROY RAX !!
;; TODO : Rewrite alloc guard
(define (gen-alloc-regimm cgc stag length)
  (let ((label-allok (asm-make-label cgc (new-sym 'alloc-ok))))
    (x86-push cgc (x86-rbx))
    (x86-push cgc (x86-rax))
    (x86-shl cgc (x86-rax) (x86-imm-int 1))
    (x86-add cgc (x86-rax) alloc-ptr)  
    (if (> length 0)
      (x86-add cgc (x86-rax) (x86-imm-int (* 8 length))))
    (x86-mov cgc (x86-rbx) (x86-imm-int (+ from-space space-len)))
      (x86-cmp cgc (x86-rax) (x86-rbx))
      (x86-jl cgc label-allok)
      (x86-sub cgc (x86-rax) alloc-ptr) ;; TODO PARAM RAX = allocsize
      (gen-gc-call cgc)
    ;; Can be allocated
    (x86-label cgc label-allok)
    (x86-pop cgc (x86-rax))
    (x86-shl cgc (x86-rax) (x86-imm-int 1))
    (if (> length 0)
      (x86-add cgc (x86-rax) (x86-imm-int (* 8 length))))
    (x86-add cgc alloc-ptr (x86-rax))
    (x86-pop cgc (x86-rbx))))

;; Generate an heap object header
;; NOTE : 'life' is fixed as 6 for now.
;(define (memobj-header length stag life)
(define (mem-header length stag)
    ;; => Length (56 bits) | sTag (5 bits) | Life (3 bits)
    (+ (arithmetic-shift length 8) (arithmetic-shift stag 3) 6))

;;-----------------------------------------------------------------------------
;; COLLECTOR
;;

;; Pretty print stag
(define (pp-stag stag)
  (cond ((eq? stag STAG_PROCEDURE) (println "PROCEDURE"))
        ((eq? stag STAG_PAIR)      (println "PAIR"))
        ((eq? stag STAG_MOBJECT)   (println "MOBJECT"))
        ((eq? stag STAG_VECTOR)    (println "VECTOR"))
        ((eq? stag STAG_STRING)    (println "STRING"))
        ((eq? stag STAG_CCTABLE)   (println "CCTABLE"))
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

;; TODO
(define (read-header-a addr)
  (let ((qword (get-i64 addr)))
    (list (bitwise-and qword 7)                         ;; Get head
          (arithmetic-shift (bitwise-and qword 248) -3) ;; Get stag
          (arithmetic-shift qword -8))))                ;; Get length
  
;; Read header from heap and return formatted list:
;; (head stag length)
(define (read-header byte-idx)
  (let ((qword (get-i64 (+ from-space (* 8 byte-idx)))))
    (list (bitwise-and qword 7)                         ;; Get head
          (arithmetic-shift (bitwise-and qword 248) -3) ;; Get stag
          (arithmetic-shift qword -8))))                ;; Get length

;; Copy 'len' bytes from address 'from' to address 'to'
(define (copy-bytes from to len)
  (log-gc (string-append "Copy "
                         (number->string len)
                         " bytes from "
                         (number->string from)
                         " to "
                         (number->string to)))
  (let ((copy-ptr (copy-bytes-h from to len)))
    (if (>= copy-ptr (+ to-space space-len))
        (out-of-memory)
        copy-ptr)))

(define (copy-bytes-h from to len)
  (if (> len 0)
      (begin (put-i64 to (get-i64 from))
             (copy-bytes-h (+ from 8) (+ to 8) (- len 1)))
      to))

;;-------------

;; TODO SECTION

(define (copy-root slot-addr current-copy-ptr)
   (let* ((value (get-i64 slot-addr))
          (tag (bitwise-and value 3)))
     
     (if (= tag TAG_MEMOBJ)
         (let* (;; Object address in heap
                (obj-addr (- value tag))
                ;; Object header
                (header-qword (get-i64 obj-addr)))
           
           (if (= header-qword -1)
              ;; Header is BH
              ;; Patch memory slot
              (let ((new-pos (get-i64 (+ 8 obj-addr))))
                (put-i64 slot-addr new-pos))
              ;; Object is not yet copied
              ;; Copy object and get new copy-ptr pos
              (let* (;; Object header
                    (header (read-header-a obj-addr))
                    ;; Object stag
                    (stag (cadr header))
                    ;; Object length
                    (length (caddr header))
                    ;;
                    (c (copy-bytes obj-addr current-copy-ptr length)))
                
                 ;; Write BH
                 (put-i64 obj-addr -1)
                 ;; Write new position (tagged)
                 (put-i64 (+ 8 obj-addr) (+ current-copy-ptr TAG_MEMOBJ))
                 ;; Patch slot
                 (put-i64 slot-addr (+ current-copy-ptr TAG_MEMOBJ))
                 ;; Update copy-ptr
                 (set! current-copy-ptr c)))))
     
     current-copy-ptr))
              
               


;;-------------
;; STACK ROOTS
;; Frist step of the GC. Read each stack slot. If a slot is a memory object
;; then the GC copy this object in to-space and update copy-ptr

;; Copy all stack roots to to-space
;; sbegin: First stack address to scan
;; send:   Last stack address to scan
;; current-copy-ptr: current position of copy-ptr in to-space
(define (copy-stack-roots sbegin send current-copy-ptr)
  (if (< sbegin send);; TODO < ?
      ;; All roots are copied then return new position of copy-ptr
      current-copy-ptr
      ;; Else, get read first value and copy if memobj
      (let* ((stack-val (get-i64 sbegin))
             (tag (bitwise-and stack-val 3)))
        ;; This stack slot is a memobj
        (if (= tag TAG_MEMOBJ) ;; TODO read-header
           (let* (;; Object address in heap
                  (addr (- stack-val tag))
                  ;; Object header
                  (header (get-i64 addr))
                  ;; Object stag
                  (stag   (arithmetic-shift (bitwise-and header 248) -3))
                  ;; Object length
                  (length (arithmetic-shift header -8)))
          
             (if (= header -1)
                 ;; Header is BH
                 ;; Patch stack slot
                 (let ((new-pos (get-i64 (+ 8 addr))))
                   (put-i64 sbegin new-pos))
                 ;; Object is not yet copied
                 ;; Copy object and get new copy-ptr pos
                 (let ((c (copy-bytes addr current-copy-ptr length)))
                        ;; Write BH
                        (put-i64 addr -1)
                        ;; Write new position (tagged)
                        (put-i64 (+ 8 addr) (+ current-copy-ptr TAG_MEMOBJ))
                        ;; Patch stack slot
                        (put-i64 sbegin (+ current-copy-ptr TAG_MEMOBJ))
                        ;; Update copy-ptr
                        (set! current-copy-ptr c)))))
        ;; Read next stack slot       
        (copy-stack-roots (- sbegin 8) send current-copy-ptr))))

;;--------------
;; GLOBAL ROOTS
;; Second step of the GC. Read each global value. If a value is a memory object
;; then the GC copy this object in to-space and update copy-ptr

;; Copy all global roots to to-space
;; globals: globals to read
;; current-copy-ptr: current position of copy-ptr in to-space
(define (copy-global-roots globals current-copy-ptr)
  (if (null? globals)
      ;; All roots are copied then return new position of copy-ptr
      current-copy-ptr
      ;; Else get first global value and copy
      (let* (;; Get global info
             (global (car globals))
             ;; Get global address
             (global-addr (+ 8 (* 8 (cdr global)) block-addr))
             ;; Copy global if it's a heap obj
             (c (copy-root global-addr current-copy-ptr)))
          ;; Continue with next globals    
          (copy-global-roots (cdr globals) c))))

;;--------------
;; SCAN OBJECTS
;; Third and last step of the GC. Read each object in to-space starting from scan-ptr.
;; If an object contains references to memory objects the GC copy these referenced objects
;; in to-space and update copy-ptr.
;; Read all object in to-space until scan-ptr == copy-ptr

;; Copy all stack roots to to-space
;; scan: address of object to scan
;; copy: address of copy ptr
(define (scan-references scan copy)
  (cond ;; If scan > copy this is an unexpected behavior
        ((> scan copy)
            (error "Unexpected behavior"))
        ;; If scan == copy GC is over
        ((= scan copy)
            copy)
        ;; Else continue to scan
        (else
            ;; Read header of object
            (let ((qword (get-i64 scan))) ;; TODO read-header
              (let ((h (bitwise-and qword 7)) ;; Get head
                    (s (arithmetic-shift (bitwise-and qword 248) -3)) ;; Get stag
                    (l (arithmetic-shift qword -8))) ;; Get length
              
              (cond ;; Procedure object
                    ((= s STAG_PROCEDURE)
                      (let ((sc (scan-procedure scan copy h s l)))
                        (scan-references (car sc) (cdr sc))))
                    ;; Vector object
                    ((= s STAG_VECTOR)
                      (let ((sc (scan-vector scan copy h s l)))
                        (scan-references (car sc) (cdr sc))))
                    ;; Cc-table Object
                    ((= s STAG_CCTABLE)
                      ;; Nothing to do
                      (scan-references (+ scan (* 8 l)) copy))
                    ;; Unknown stag
                    (else (pp-stag s)
                          (error "Unknown stag while scanning references"))))))))

;; Scan procedure
;; Copy cc-table to to-space
;; Scan all fields of closure
;; Return new scan/copy-ptr position
(define (scan-procedure scan copy head stag length)
  (let* ((cc-table-loc (get-i64 (+ 8 scan)))
         (cc-head (get-i64 cc-table-loc)))
    ;; BH tests is useless because cc-table is not yet copied (1 cc-table <-> 1 closure)
    ;; Patch closure (cc-table location)
    (put-i64 (+ 8 scan) copy)
    ;; Copy cc-table
    (let ((c (copy-bytes cc-table-loc copy (arithmetic-shift cc-head -8))))
      ;; TODO COPY MEMORY ALLOCATED FREE VARIABLES
      (cons ;; New scan position
            (+ scan (* 8 length))
            ;; New copy position
            c))))

;; 
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

;; GC main
(define (run-gc sp alloc-size)
  
  ;; Set used values
  (define scan-ptr to-space)
  (define copy-ptr to-space)
  (define stack-begin (- (get-i64 block-addr) 8))
  (define stack-end   (+ sp (* 8 (length c-caller-save-regs))))
  
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
  (set! copy-ptr (scan-references scan-ptr copy-ptr))
  
  (if (>= (+ copy-ptr alloc-size) (+ to-space space-len))
    (out-of-memory))
  
  ;; Update from/to-spaces positions
  (let ((tmp from-space))
    (set! from-space to-space)
    (set! to-space tmp))
  
  ;; Return new position of alloc-ptr
  copy-ptr)


;; TODO SECTION
;;-------------

(define (get-tag qword)
  (bitwise-and qword 3))

;; Scan field
;; This function scan a field of an object (vector element, pair car/cdr, ...)
;; If the field is a reference to a memory allocated object, then copy the object
;; and return new copy-ptr value
(define (scan-field addr copy)
  (let* ((qword (get-i64 addr))
         (tag   (get-tag qword)))
    
    (cond ;; Number & Special
          ((or (= tag TAG_NUMBER)
               (= tag TAG_SPECIAL))
              copy)
          ;; Heap object
          ((= tag TAG_MEMOBJ)
              (error "NYI"))
          ;; Other
          (else (error "NYI")))))