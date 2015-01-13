(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; Use & !7 for string size (make-string)
;; TODO : 'encoded' as parameter to avoid shr/shl of make-string

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
(define (read-header)
  (let ((qword (get-i64 from-space)))
    (list (bitwise-and qword 7)                         ;; Get head
          (arithmetic-shift (bitwise-and qword 248) -3) ;; Get stag
          (arithmetic-shift qword -8))))                ;; Get length

;; GC main
(define (run-gc)
  (let ((header (read-header)))
    (pp-header header)))