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

(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm")
(include "~~lib/_codegen#.scm")

;;-----------------------------------------------------------------------------

(define (assert c msg . l)
  (if (not c)
     (error msg)))

(define (x86-reg-xmm? x)
  (and (x86-reg? x) (x86-xmm? x)))

;;-----------------------------------------------------------------------------
;; IEEE754 conversion

;; Return integer representation of sign of f (0 or 1)
(define (sign-int f) (if (>= f 0) 0 1))

;; Return pair (fraction, exponent) of float number f (n is recursion value)
;; ex. (frac-expo 0.085 0) -> 1.36, -4 (0.085 / 2^-4 = 1.36)
(define (frac-expo f n)
   (if (and (>= f 1) (< f 2)) ;; done
      (cons f n)
      (if (< f 1)
         (frac-expo (* f 2) (- n 1))
         (frac-expo (/ f 2) (+ n 1)))))

;; Return approximate integer representation of fraction f (n, p are recursion values)
;; ex. (frac-int 0.36 0 -1) -> 3019899
;;     (01011100001010001111011 with 0.01011100001010001111011 the approximate binary representation of 0.36)
(define (frac-int nbits-fraction f n p)

  (cond ;; Last choice (do we take 2^-23 ?)
      ((= p (* -1 nbits-fraction))
         ;; We get the closest number of f between n and n+2^-23
         (let ((sa (abs (- f (+ n (expt 2 p)))))
               (sb (abs (- f n))))
           (if (< sa sb)
              1
              0)))
          ;; We can't take 2^p
      ((> (+ n (expt 2 p)) f)
         (frac-int nbits-fraction f n (- p 1)))
      ;; Else take 2^p and continue
      (else
         (+ (expt 2 (+ nbits-fraction p))
            (frac-int nbits-fraction f (+ n (expt 2 p)) (- p 1))))))

;; Return the integer corresponding to the 'IEEE 754' representation of float f
;; ex. (float-int 0.085) -> 1034818683 (0 01111011 01011100001010001111011 in binary, with sign, exponent and fraction)
(define (ieee754 f  #!optional (precision 'simple))

  (define bias #f)
  (define nbits-fraction #f)
  (define nbits-exp-frac #f)

  (cond ((eq? precision 'simple)
            (set! bias 127)
            (set! nbits-fraction 23)
            (set! nbits-exp-frac 31))
        ((eq? precision 'double)
            (set! bias 1023)
            (set! nbits-fraction 52)
            (set! nbits-exp-frac 63))
        (else (error "NYI precision:" precision)))

  (if (= f 0)
     0
     (let* ((frac  (frac-expo (abs f) 0))
        (nsign (sign-int f))
        (nexpo (+ bias (cdr frac)))
        (nfrac (frac-int nbits-fraction (- (car frac) 1) 0 -1)))
     (+ (* nsign (expt 2 nbits-exp-frac))
        (* nexpo (expt 2 nbits-fraction))
        nfrac))))

;;-----------------------------------------------------------------------------
;; Intel SSE2 instructions

(define (x86-movss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-mem? dst)     (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "movss" dst src #xf3 #x10))

(define (x86-movsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-mem? dst)     (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "movsd" dst src #xf2 #x10))

(define (x86-addss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "addss" dst src #xf3 #x58))

(define (x86-addsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "addsd" dst src #xf2 #x58))

(define (x86-subss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "subss" dst src #xf3 #x5c))

(define (x86-subsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "subsd" dst src #xf2 #x5c))

(define (x86-mulss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "mulss" dst src #xf3 #x59))

(define (x86-mulsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "mulsd" dst src #xf2 #x59))

(define (x86-divss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "divss" dst src #xf3 #x5e))

(define (x86-divsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "divsd" dst src #xf2 #x5e))

;; From intel manual: The COMISD instruction differs from the UCOMISD instruction in that
;; it signals a SIMD floating-point invalid operation exception (#I) when a source operand
;; is either a QNaN or SNaN.
(define (x86-comisd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "comisd" dst src #x66 #x2f))

(define (x86-comiss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "comiss" dst src #f #x2f))

(define (x86-movd/movq cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (or (x86-reg? src) (x86-mem? src)))
              (and (x86-reg-xmm? src) (or (x86-reg? dst) (x86-mem? dst))))
          "invalid operands")
  (if (x86-reg-xmm? src)
    (x86-sse-op cgc "movq" src dst #x66 #x7e)
    (x86-sse-op cgc "movq" dst src #x66 #x6e)))

(define (x86-cvtsi2sd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst)
                   (x86-reg? src)
                   (not (x86-xmm? src))
                   (or (= (x86-reg-width src) 32) (= (x86-reg-width src) 64)))
              (and (x86-reg-xmm? dst)
                   (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "cvtsi2sd" dst src #xf2 #x2a))

(define (x86-sse-op cgc mnemonic dst src opcode1 opcode2)

  (if opcode1
    (asm-8 cgc opcode1)) ;; opcode

  (if (x86-reg? dst)

    (begin (x86-opnd-prefix-reg-opnd cgc dst src)       ;; prefix
           (asm-8 cgc #x0f)                             ;; opcode 1
           (asm-8 cgc opcode2)                             ;; opcode 2
           (x86-opnd-modrm/sib-reg-opnd cgc dst src)   ;; ModR/M
           (x86-listing cgc mnemonic (x86-reg-width dst) dst src))

    (begin (x86-opnd-prefix-reg-opnd cgc src dst)       ;; prefix
           (asm-8 cgc #x0f)                             ;; opcode 1
           (asm-8 cgc (+ opcode2 1))                    ;; opcode 2
           (x86-opnd-modrm/sib-reg-opnd cgc src dst) ;; ModR/M
           (x86-listing cgc mnemonic (x86-reg-width src) dst src))))

;;;----------------------------------------------------------------------------
;; Used functions from gambit/gsc/_x86.scm

;;;============================================================================

;;; Architecture selection (either x86-32 or x86-64).

(define-macro (x86-assert-64bit-mode cgc)
  `(assert (x86-64bit-mode? ,cgc)
           "instruction only valid for x86-64"))

;;;----------------------------------------------------------------------------

;;; Instruction operands.

(define (x86-force-width? x) (and (vector? x) (fx= (vector-length x) 3)))
(define (x86-force-width-opnd x) (vector-ref x 0))
(define (x86-force-width-width x) (vector-ref x 1))

(define (x86-imm? x) (pair? x))
(define (x86-imm-int-width x) (car x))

(define (x86-imm-lbl? x) (and (pair? x) (vector? (cdr x))))
(define (x86-imm-lbl-offset x) (car x))
(define (x86-imm-lbl-label x) (cdr x))

(define (x86-imm-late? x) (and (pair? x) (procedure? (cdr x))))
(define (x86-imm-late-handler x) (cdr x))

(define (x86-mem? x) (and (vector? x) (fx= (vector-length x) 4)))
(define (x86-mem-offset x) (vector-ref x 0))
(define (x86-mem-reg1 x) (vector-ref x 1))
(define (x86-mem-reg2 x) (vector-ref x 2))
(define (x86-mem-scale x) (vector-ref x 3))

(define (x86-mem-abs? x)
  (and (not (x86-mem-reg1 x))
       (not (x86-mem-reg2 x))))

;;;----------------------------------------------------------------------------

;;; Listing generation.

(define (x86-offset->string offset)
  (cond ((fx= offset 0) "")
        ((fx< offset 0) (number->string offset))
        (else           (string-append "+" (number->string offset)))))

(define (x86-listing cgc mnemonic width . opnds)

  (define (instr-format-gnu)

    (define (opnd-format opnd)
      (cond ((x86-force-width? opnd)
             (opnd-format (x86-force-width-opnd opnd)))
            ((x86-reg? opnd)
             (list "%" (x86-register-name opnd)))
            ((x86-imm? opnd)
             (list "$"
                   (cond ((x86-imm-int? opnd)
                          (x86-imm-int-value opnd))
                         ((x86-imm-lbl? opnd)
                          (list (asm-label-name (x86-imm-lbl-label opnd))
                                (x86-offset->string (x86-imm-lbl-offset opnd))))
                         ((x86-imm-late? opnd)
                          ((x86-imm-late-handler opnd) cgc 'listing))
                         (else
                          (error "unknown immediate" opnd)))))
            #;
            ((x86-glo? opnd);;;;;;;;;;
             (let ((name (x86-glo-name opnd))
                   (offset (x86-glo-offset opnd)))
               (list name
                     (x86-offset->string offset))))
            ((x86-mem? opnd)
             (let ((reg1 (x86-mem-reg1 opnd))
                   (reg2 (x86-mem-reg2 opnd))
                   (scale (x86-mem-scale opnd))
                   (offset (x86-mem-offset opnd)))
               (if reg1
                   (let ((x
                          (cons "("
                                (cons (opnd-format reg1)
                                      (if reg2
                                          (cons ","
                                                (cons (opnd-format reg2)
                                                      (if (fx= scale 0)
                                                          '(")")
                                                          (list ","
                                                                (fxarithmetic-shift-left
                                                                 1
                                                                 scale)
                                                                ")"))))
                                          '(")"))))))
                     (if (fx= offset 0) x (cons offset x)))
                   offset)))
            (else
             opnd)))

    (let ((operands
           (asm-separated-list (map opnd-format (reverse opnds)) ",")))
      (cons #\tab
            (cons mnemonic
                  (if (fx>= width 0)
                      (cons (x86-width-suffix cgc width)
                            (if (pair? operands)
                                (cons #\tab
                                      operands)
                                '()))
                      (if (pair? operands)
                          (cons #\tab
                                (cons "*"
                                      operands))
                          '()))))))

  (define (instr-format-nasm)

    (define (data-width-qualifier width)
      (case width
        ((8)  "byte ")
        ((16) "word ")
        ((32) "dword ")
        ((64) "qword ")
        ((1)  "short ") ;; special width for short jumps
        (else "")))

    (define (opnd-format opnd)
      (cond ((x86-force-width? opnd)
             (list (data-width-qualifier (x86-force-width-width opnd))
                   (opnd-format (x86-force-width-opnd opnd))))
            ((x86-reg? opnd)
             (x86-register-name opnd))
            ((x86-imm? opnd)
             (cond ((x86-imm-int? opnd)
                    (let ((value (x86-imm-int-value opnd))
                          (opnd-width (x86-imm-int-width opnd)))
                      (if (or (fx= width 8)
                              (fx= width opnd-width)
                              (and (fx= width 64)
                                   (fx= opnd-width 32)
                                   (not (equal? mnemonic "mov"))))
                          value
                          (list (data-width-qualifier opnd-width) value))))
                   ((x86-imm-lbl? opnd)
                    (list (asm-label-name (x86-imm-lbl-label opnd))
                          (x86-offset->string (x86-imm-lbl-offset opnd))))
                   ((x86-imm-late? opnd)
                    ((x86-imm-late-handler opnd) cgc 'listing))
                   (else
                    (error "unknown immediate" opnd))))
            #;
            ((x86-glo? opnd);;;;;;;;;;
             (let ((name (x86-glo-name opnd))
                   (offset (x86-glo-offset opnd)))
               (list "["
                     name
                     (x86-offset->string offset)
                     "]")))
            ((x86-mem? opnd)
             (let ((offset (x86-mem-offset opnd))
                   (reg1 (x86-mem-reg1 opnd))
                   (reg2 (x86-mem-reg2 opnd))
                   (scale (x86-mem-scale opnd)))
               (list "["
                     (if reg1
                         (opnd-format reg1)
                         "")
                     (if reg2
                         (list "+"
                               (opnd-format reg2)
                               (if (fx= scale 0)
                                   ""
                                   (list "*"
                                         (fxarithmetic-shift-left 1 scale))))
                         "")
                     (x86-offset->string offset)
                     "]")))
            (else
             opnd)))

    (cons #\tab
          (cons mnemonic
                (if (pair? opnds)
                    (let ((width-implicit? #f))

                      (define (opnd-fmt opnd)
                        (if (x86-reg? opnd)
                            (set! width-implicit? #t))
                        (opnd-format opnd))

                      (let ((opnds-listing
                             (asm-separated-list (map opnd-fmt opnds) ",")))
                        (cons #\tab
                              (if width-implicit?
                                  opnds-listing
                                  (cons (data-width-qualifier width)
                                        opnds-listing)))))
                    '()))))

  (asm-listing
   cgc
   (case (codegen-context-listing-format cgc)
     ((gnu)
      (instr-format-gnu))
     (else ;;(nasm)
      (instr-format-nasm)))))

(define (x86-width-suffix cgc width)
  (case (codegen-context-listing-format cgc)
    ((gnu)
     (cond ((fx= width 64) "q")
           ((fx= width 32) "l")
           ((fx= width 16) "w")
           ((fx= width 8)  "b")
           ((fx= width 1)  "")
           (else           "")))
    (else ;;(nasm)
     "")))

;;;----------------------------------------------------------------------------

;;; X86 operand encoding.

(define (x86-opnd-prefix-reg-opnd cgc reg opnd)
  (let* ((width
          (x86-reg-width reg))
         (field
          (x86-reg-field reg))
         (ext-lo8-reg?
          (and (fx= width 8)
               (fx>= field 4)
               (not (x86-reg8-h? reg)))))
    (if (x86-reg? opnd)
        (begin
          (let* ((field2
                  (x86-reg-field opnd))
                 (ext-lo8-reg2?
                  (and (fx= (x86-reg-width opnd) 8)
                       (fx>= field2 4)
                       (not (x86-reg8-h? opnd))))
                 (rex?
                  (x86-opnd-prefix cgc
                                   width
                                   field
                                   opnd
                                   (or ext-lo8-reg? ext-lo8-reg2?))))
            (assert (not (and rex?
                              (or (x86-reg8-h? reg)
                                  (x86-reg8-h? opnd))))
                    "cannot use high 8 bit register here" reg opnd)
            rex?))
        (x86-opnd-prefix cgc
                         width
                         field
                         opnd
                         ext-lo8-reg?))))

(define (x86-opnd-modrm/sib-reg-opnd cgc reg opnd)
  (x86-opnd-modrm/sib cgc (x86-reg-field reg) opnd))

(define (x86-opnd-prefix cgc width field opnd force-rex?)
  (let ((rex*
         (fx+ ;; if needed emit REX.W (64 bit operand size)
              (if (and (not (fx= width 0)) ;; implicit width?
                       (or (fx= width 64)
                           (and (x86-reg? opnd) (x86-reg64? opnd))))
                  8
                  0)
              ;; if needed emit REX.R (Extension of the ModR/M reg field)
              (fxarithmetic-shift-left
               (fxarithmetic-shift-right
                field
                3)
               2)
              (cond ((x86-reg? opnd)
                     ;; if needed emit REX.B (Extension of
                     ;; the ModR/M r/m field, SIB base field,
                     ;; or Opcode reg field)
                     (fxarithmetic-shift-right
                      (x86-reg-field opnd)
                      3))
                    #;
                    ((x86-glo? opnd);;;;;;;;;;
                     0)
                    ((x86-mem? opnd)
                     (let ((reg1 (x86-mem-reg1 opnd)))
                       (if reg1
                           (begin
                             (assert (or (x86-reg32? reg1)
                                         (and (x86-reg64? reg1)
                                              (x86-64bit-mode? cgc)))
                                     "invalid width base register" reg1)
                             (fx+ ;; if needed emit REX.B (Extension of
                                  ;; the ModR/M r/m field, SIB base field,
                                  ;; or Opcode reg field)
                                  (fxarithmetic-shift-right
                                   (x86-reg-field reg1)
                                   3)
                                  (let ((reg2 (x86-mem-reg2 opnd)))
                                    (if reg2
                                        (begin
                                          (assert (if (x86-reg32? reg1)
                                                      (x86-reg32? reg2)
                                                      (x86-reg64? reg2))
                                                  "index register must have same width as base" reg2)
                                          ;; if needed emit REX.X (Extension
                                          ;; of the SIB index field)
                                          (fxarithmetic-shift-left
                                           (fxarithmetic-shift-right
                                            (x86-reg-field reg2)
                                            3)
                                           1))
                                        0))))
                           0)))
                    (else
                     (error "unknown operand" opnd))))))
    (x86-opnd-size-override-prefix cgc width)
    (x86-addr-size-override-prefix cgc opnd)
    (if (or force-rex?
            (not (fx= rex* 0)))
        (begin
          (x86-assert-64bit-mode cgc)
          (asm-8 cgc (fx+ #x40 rex*)) ;; REX
          #t)
        #f)))

(define (x86-opnd-size-override-prefix cgc width)
  (if (fx= width 16)
      (asm-8 cgc #x66))) ;; operand size override prefix

(define (x86-addr-size-override-prefix cgc opnd)
  (if (and (x86-mem? opnd)
           (let ((reg1 (x86-mem-reg1 opnd)))
             (and reg1
                  (eq? (x86-64bit-mode? cgc)
                       (not (x86-reg64? reg1))))))
      (asm-8 cgc #x67))) ;; address size override prefix

(define (x86-opnd-modrm/sib cgc field opnd)
  (let ((modrm-rf
         (fxarithmetic-shift-left (fxand 7 field) 3)))

    (define (abs-addr)
      (if (x86-64bit-mode? cgc) ;; avoid RIP relative encoding?
          (begin
            (asm-8 cgc (fx+ modrm-rf 4)) ;; ModR/M
            (asm-8 cgc #x25))            ;; SIB
          (asm-8 cgc (fx+ modrm-rf 5)))) ;; ModR/M

    (cond ((x86-reg? opnd)
           (let ((modrm*
                  (fx+ modrm-rf (fxand 7 (x86-reg-field opnd)))))
             (asm-8 cgc (fx+ #xc0 modrm*)))) ;; ModR/M

          #;
          ((x86-glo? opnd);;;;;;;;;;
           (abs-addr)
           (let ((name (x86-glo-name opnd))
                 (offset (x86-glo-offset opnd)))
             (x86-abs-addr cgc (nat-global-lookup cgc name) offset 32)))

          ((x86-mem? opnd)
           (let ((offset (x86-mem-offset opnd))
                 (reg1   (x86-mem-reg1 opnd)))

             (if reg1

                 (let* ((field1    (x86-reg-field reg1))
                        (field1-lo (fxand 7 field1))
                        (reg2      (x86-mem-reg2 opnd)))

                   (if (or reg2 ;; need a SIB when using an index
                           (fx= field1-lo 4)) ;; register or base = RSP/R12

                       ;; SIB needed

                       (let ((modrm*
                              (fx+ modrm-rf 4))
                             (sib
                              (fx+ field1-lo
                                   (if reg2
                                       (let ((field2 (x86-reg-field reg2)))
                                         (assert (not (fx= field2 4))
                                                 "SP not allowed as index" reg2)
                                         (fx+ (fxarithmetic-shift-left
                                               (fxand 7 field2)
                                               3)
                                              (fxarithmetic-shift-left
                                               (x86-mem-scale opnd)
                                               6)))
                                       #x20)))) ;; no index and no scaling

                         (if (asm-signed8? offset)
                             (if (or (not (fx= offset 0)) ;; non-null offset?
                                     (fx= field1 5))      ;; or RBP
                                 (begin ;; use 8 bit displacement
                                   (asm-8 cgc (fx+ #x40 modrm*)) ;; ModR/M
                                   (asm-8 cgc sib) ;; SIB
                                   (asm-8 cgc offset))
                                 (begin
                                   (asm-8 cgc (fx+ #x00 modrm*)) ;; ModR/M
                                   (asm-8 cgc sib))) ;; SIB
                             (begin ;; use 32 bit displacement
                               (asm-8 cgc (fx+ #x80 modrm*)) ;; ModR/M
                               (asm-8 cgc sib)               ;; SIB
                               (asm-32-le cgc offset))))

                       ;; SIB not needed

                       (let ((modrm*
                              (fx+ modrm-rf field1-lo)))
                         (if (asm-signed8? offset)
                             (if (or (not (fx= offset 0)) ;; non-null offset?
                                     (fx= field1-lo 5)) ;; or RBP/R13
                                 (begin ;; use 8 bit displacement
                                   (asm-8 cgc (fx+ #x40 modrm*)) ;; ModR/M
                                   (asm-8 cgc offset))
                                 (asm-8 cgc (fx+ #x00 modrm*))) ;; ModR/M
                             (begin ;; use 32 bit displacement
                               (asm-8 cgc (fx+ #x80 modrm*)) ;; ModR/M
                               (asm-32-le cgc offset))))))

                 (begin ;; absolute address, use disp32 ModR/M
                   (abs-addr)
                   (asm-32-le cgc offset)))))

          (else
           (error "unknown operand" opnd)))))

;;;----------------------------------------------------------------------------
