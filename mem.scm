(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

;; Dans make-string et make-vector OR au lieu de add ?
;; utiliser la solution avec & !7 pour avoir la taille ?
;; TODO : encodée en option, pour eviter shr/shl dans make-string
;; Nettoyer todos
;; performance add vs or ? 

;;
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
	  (x86-mov cgc (x86-rbx) (x86-imm-int (+ heap-addr heap-len)))
   	  (x86-cmp cgc (x86-rax) (x86-rbx)) ;; TODO
   	  (x86-jl  cgc label-allok)
      (gen-error cgc "HEAP OVERFLOW")
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
    (x86-mov cgc (x86-rbx) (x86-imm-int (+ heap-addr heap-len)))
      (x86-cmp cgc (x86-rax) (x86-rbx))
      (x86-jl cgc label-allok)
      (gen-error cgc "HEAP OVERFLOW")
    ;; Can be allocated
    (x86-label cgc label-allok)
    (x86-mov cgc alloc-ptr (x86-rax))
    (x86-pop cgc (x86-rbx))))

;;
;; COLLECTOR
;;