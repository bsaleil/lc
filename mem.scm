(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; Use & !7 for string size (make-string)
;; TODO : 'encoded' as parameter to avoid shr/shl of make-string
;; TODO : mlc-lambda offset 8 for cc-table header
;; TODO : améliorer init-rtlib (x86)
;; TODO : Fusionner copie des locales (stack) et globaless

;;-----------------------------------------------------------------------------
;; ALLOCATOR
;;

(define (gen-allocation cgc stag length #!optional use-rax)
  (if use-rax
      (gen-alloc-regimm cgc stag length)
      (gen-alloc-imm    cgc stag length)))

;; NOTE : 'life' is fixed as 6 for now.
;(define (memobj-header length stag life)
(define (mem-header length stag)
    ;; => Length (56 bits) | sTag (5 bits) | Life (3 bits)
    (+ (arithmetic-shift length 8) (arithmetic-shift stag 3) 6))

;; Allocate length(imm) bytes in heap
(define (gen-alloc-imm cgc stag length)
  (let ((label-allok (asm-make-label cgc (new-sym 'alloc-ok))))
	  (x86-mov cgc (x86-rax) alloc-ptr)
	  (x86-add cgc (x86-rax) (x86-imm-int (* length 8)))
	  (x86-mov cgc (x86-rbx) (x86-imm-int (+ from-space space-len)))
   	  (x86-cmp cgc (x86-rax) (x86-rbx)) ;; TODO
   	  (x86-jl  cgc label-allok)
      (gen-gc-call cgc)
    ;; Can be allocated
    (x86-label cgc label-allok)
    (x86-add cgc alloc-ptr (x86-imm-int (* length 8)))))

;; Allocate RAX(reg) + length(imm) bytes in heap
;; DESTROY RAX !!
(define (gen-alloc-regimm cgc stag length)
  (let ((label-allok (asm-make-label cgc (new-sym 'alloc-ok))))
    (x86-push cgc (x86-rbx))
    (x86-shl cgc (x86-rax) (x86-imm-int 1))
    (x86-add cgc (x86-rax) alloc-ptr)  
    (if length
      (x86-add cgc (x86-rax) (x86-imm-int (* 8 length))))
    (x86-mov cgc (x86-rbx) (x86-imm-int (+ from-space space-len)))
      (x86-cmp cgc (x86-rax) (x86-rbx))
      (x86-jl cgc label-allok)
      (gen-gc-call cgc)
    ;; Can be allocated
    (x86-label cgc label-allok)
    (x86-mov cgc alloc-ptr (x86-rax))
    (x86-pop cgc (x86-rbx))))

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

;; Read header from heap and return formatted list:
;; (head stag length)
(define (read-header byte-idx)
  (let ((qword (get-i64 (+ from-space (* 8 byte-idx)))))
    (list (bitwise-and qword 7)                         ;; Get head
          (arithmetic-shift (bitwise-and qword 248) -3) ;; Get stag
          (arithmetic-shift qword -8))))                ;; Get length

(define (pp-heap-headers idx)
  (pp (* 8 idx))
  (pp space-len)
  (if (< (* idx 8) space-len)
    (let ((header (read-header idx)))
      (if (= (+ (car   header)
                (cadr  header)
                (caddr header))
             0)
        (error "GC pp error read value 0 from heap."))
      
      (pp-header header)
      (print "add offset ")
      (println (caddr header))
      (pp-heap-headers (+ idx (caddr header))))))

;;;;;;;
(define (copy-bytes from to len)
  ; (print "Copy ")
  ; (print len)
  ; (print " bytes from ")
  ; (print from)
  ; (print " to ")
  ; (println to)
  (copy-bytes-h from to len))

(define (copy-bytes-h from to len)
  (if (> len 0)
      (begin (put-i64 to (get-i64 from))
             (copy-bytes-h (+ from 8) (+ to 8) (- len 1)))
      to))

;;;;;;;;;;;;;;
;; STACK ROOTS
;;;;;;;;;;;;;;

(define (copy-stack-roots sbegin send current-copy-ptr)
  (if (<= sbegin send) ;; TODO < ?
      current-copy-ptr
      (let* ((stack-val (get-i64 sbegin))
             (tag (bitwise-and stack-val 3)))
        
        ;; This stack slot is a MEMOBJ (ROOT)
        (if (= tag TAG_MEMOBJ)
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
                        ; (print "Write -1 at ")
                        ; (println addr)
                        (put-i64 addr -1)
                        ;; Write TAGGED new position
                        (put-i64 (+ 8 addr) (+ current-copy-ptr TAG_MEMOBJ))
                        ;; Patch stack slot
                        (put-i64 sbegin (+ current-copy-ptr TAG_MEMOBJ))
                        ;; Update copy-ptr
                        (set! current-copy-ptr c)))))
        
        ;; Read next stack slot       
        (copy-stack-roots (- sbegin 8) send current-copy-ptr))))

;;;;;;;;;;;;;;;
;; GLOBAL ROOTS
;;;;;;;;;;;;;;;

(define (copy-global-roots globals current-copy-ptr)
  (if (null? globals)
      current-copy-ptr
      (let* ((global (car globals))
             (global-val (get-i64 (+ 8 (* 8 (cdr global)) block-addr))) ;; TODO global addr
             (tag (bitwise-and global-val 3)))
        
        ;; This global is a MEMOBJ (ROOT)
        (if (= tag TAG_MEMOBJ)
            (let* (;; Object address in heap
                   (addr (- global-val tag))
                   ;; Object header
                   (header (get-i64 addr))
                   ;; Object stag
                   (stag   (arithmetic-shift (bitwise-and header 248) -3))
                   ;; Object length
                   (length (arithmetic-shift header -8)))
            
              (if (= header -1)
                  ;; Header is BH
                  ;; Patch global var
                  (let ((new-pos (get-i64 (+ 8 addr))))
                   (put-i64 (+ 8 (* 8 (cdr global)) block-addr) new-pos))
                  ;; Object is not yet copied
                  ;; Copy object and get new copy-ptr pos
                  (let ((c (copy-bytes addr current-copy-ptr length)))
                        ;; Write BH
                        ; (print "Write -1 at ")
                        ; (println addr)
                        (put-i64 addr -1)
                        ;; Write TAGGED new position
                        (put-i64 (+ 8 addr) (+ current-copy-ptr TAG_MEMOBJ))
                        ;; Patch stack slot
                        (put-i64 (+ 8 (* 8 (cdr global)) block-addr) (+ current-copy-ptr TAG_MEMOBJ))
                        ;; Update copy-ptr
                        (set! current-copy-ptr c)))))

        ;; Read next stack slot       
        (copy-global-roots (cdr globals) current-copy-ptr))))

;;;;;;;;;;;;;;;
;; COPY OBJECTS
;;;;;;;;;;;;;;;

;; TODO
(define (copy-procedure scan copy head stag length)
  (let ((cc-table-loc (get-i64 (+ 8 scan))))
    (let ((cc-head (get-i64 cc-table-loc)))
      (if (= cc-head -1)
          (error "NYI already collected") ;; TODO ? optional ? (1 cc-table pour 1 fermeture)
          ;; Not yet collected
          (begin  ;; Patch closure (cc-table location)
                  (put-i64 (+ 8 scan) copy)
                  ;; Copy cc-table
                  (let ((c (copy-bytes cc-table-loc copy (arithmetic-shift cc-head -8))))
                    ;; Set BH and new loc
                      ;; TODO ? optional ? (1 cc-table pour 1 fermeture)
                    (cons ;; New scan position
                          (+ scan (* 8 length))
                          ;; New copy position
                          c)))))))

;; TODO
(define (copy-vector scan copy head stag length)
  ;; Copier chaque element su tableau
  (let ((length (/ (get-i64 (+ scan 8)) 4))) ;; Decoded length
    (let ((c (copy-vector-h (+ scan 16) length copy)))
      (cons ;; New scan position
            (+ scan (* 8 (+ length 2)))
            ;; New copy position
            c))))

(define (copy-vector-h pos length copy)
  (if (= length 0)
      copy
    (let* ((obj (get-i64 pos))
           (tag (bitwise-and obj 3)))
      ;; TODO NYI
      (cond ((= tag 0) ;; Number
                (copy-vector-h (+ pos 8) (- length 1) copy))
            (else (error "NYI other objects"))))))

(define (copy-references scan copy)
  (cond ((> scan copy)
            (error "Unexpected behavior"))
        ((= scan copy)
            copy)
        (else
            (let ((qword (get-i64 scan))) ;; TODO use read-header
              (let ((h (bitwise-and qword 7)) ;; Get head
                    (s (arithmetic-shift (bitwise-and qword 248) -3)) ;; Get stag
                    (l (arithmetic-shift qword -8))) ;; Get length
              (cond ;; PROCEDURE
                    ((= s STAG_PROCEDURE)
                      (let ((sc (copy-procedure scan copy h s l)))
                        (copy-references (car sc) (cdr sc))))
                    ;; VECTOR
                    ((= s STAG_VECTOR)
                      (let ((sc (copy-vector scan copy h s l)))
                        (copy-references (car sc) (cdr sc))))
                    ;; CC-TABLE
                    ((= s STAG_CCTABLE)
                      ;; Nothing to do
                      (copy-references (+ scan (* 8 l)) copy))
                    (else (pp-stag s)
                          (error "NYI copy-references"))))))))

;; GC main
(define (run-gc sp)
  
  ;; 0 - Préparation
  (define scan-ptr to-space)
  (define copy-ptr to-space)
   
  (define stack-begin (- (get-i64 block-addr) 8))
  (define stack-end   (+ sp (* 8 (length c-caller-save-regs))))
  
  ;(println "GC")
  
  ;(println "GC BEGIN")
  
  ;; 1 - Copier les racines de la pile
  ; (println "--------------")
  ; (println "-- STACK ROOTS")
  ; (println "--------------")
  (set! copy-ptr (copy-stack-roots stack-begin stack-end copy-ptr))
  
  ;; 2 - Copier les racines des globales
  ; (println "---------------")
  ; (println "-- GLOBAL ROOTS")
  ; (println "---------------")
  (set! copy-ptr (copy-global-roots globals copy-ptr))
      
  ;; 3 - Copie des références
  ;; Tant que scan < copy
    ;; Copier tous les objets référencés par l'objet lu
    ;; MAJ scan ptr
  ; (println "------------------")
  ; (println "-- COPY REFERENCES")
  ; (println "------------------")
  (set! copy-ptr (copy-references scan-ptr copy-ptr))
  
  
  ;; 4 - MAJ des pointeurs
  ;; to-space = from-space
  ;; from-space = to-space
  (let ((tmp from-space))
    (set! from-space to-space)
    (set! to-space tmp))
  ;; mov alloc-ptr <- copy-ptr
  
  ;(error "FIN")
  
  ;(println "GC END")
  ;(pp copy-ptr)
  copy-ptr)
  
  