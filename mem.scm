(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

(define ALLOC-DEVLOG-ENABLED #t)
(define-macro (ALLOC-DEVLOG msg)
	`(if ALLOC-DEVLOG-ENABLED
     	(println (string-append "ALLOCATOR - "
                 				,msg))))

;;
;; ALLOCATOR
;;

;; NOTE : 'life' is fixed as 6 for now.
;(define (memobj-header length stag life)
(define (mem-header length stag)
	#;(ALLOC-DEVLOG (string-append "Build mem header "
                                 "length "
                                 (number->string length)
                                 ", stag "
                                 (number->string stag)))

    ;; => Length (56 bits) | sTag (5 bits) | Life (3 bits)
    (+ (arithmetic-shift length 8) (arithmetic-shift stag 3) 6))

;; Ex : (gen-alloc STAG_PAIR 2)
(define (gen-alloc cgc stag length)
  (let ((label-allok (asm-make-label cgc (new-sym 'alloc-ok))))
	  (x86-mov cgc (x86-rax) alloc-ptr)
	  (x86-add cgc (x86-rax) (x86-imm-int (* length 8))) ;; (pair + header word)
	  (x86-mov cgc (x86-rbx) (x86-imm-int (+ heap-addr heap-len)))
   	  (x86-cmp cgc (x86-rax) (x86-rbx)) ;; TODO
   	  (x86-jl  cgc label-allok)
      (gen-error cgc "HEAP OVERFLOW")
      ;;
      (x86-label cgc label-allok)
      (x86-add cgc alloc-ptr (x86-imm-int (* length 8)))))
  
  

;;
;; COLLECTOR
;;