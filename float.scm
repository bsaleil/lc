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

(include "config.scm")

(include "~~lib/_asm#.scm")
(include "~~lib/_x86#.scm")
(include "~~lib/_codegen#.scm")

;;-----------------------------------------------------------------------------

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
(define (flonum->ieee754 f #!optional (precision 'simple))

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

(define (ieee754->flonum i #!optional (precision 'simple))

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

  (let* ((sign (bitwise-and i (expt 2 nbits-exp-frac)))
         (neg? (= sign (expt 2 nbits-exp-frac)))
         (exponent (arithmetic-shift (bitwise-and i (- (expt 2 nbits-exp-frac) 1)) (* -1 nbits-fraction)))
         (fraction
           (let* ((fraction (bitwise-and i (- (expt 2 nbits-fraction) 1)))
                  (binstr (number->string fraction 2))
                  (len (string-length binstr)))
             (let loop ((idx 0) (pow (- nbits-fraction len -1)) (r 0))
               (if (= idx len)
                   (exact->inexact r)
                   (let* ((bit (string-ref binstr idx))
                          (set? (char=? bit #\1)))
                    (if set?
                        (loop (+ idx 1) (+ pow 1) (+ r (expt 2 (* -1 pow))))
                        (loop (+ idx 1) (+ pow 1) r))))))))

    (* (+ 1 fraction) (expt 2 (- exponent bias)) (if neg? -1 1))))

(define (get-ieee754-imm64 f)
  (if (< f 0)
      (let* ((ieee-rep (flonum->ieee754 (abs f) 'double))
             (64-mod   (bitwise-not (- ieee-rep 1)))
             (64-modl  (bitwise-and (- (expt 2 63) 1) 64-mod)))
        (* -1 64-modl))
      (flonum->ieee754 f 'double)))

;;-----------------------------------------------------------------------------
;; Intel SSE2 instructions

(define (x86-movss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-mem? dst)     (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "movss" dst src #xf3 #x10 #f))

(define (x86-movsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-mem? dst)     (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "movsd" dst src #xf2 #x10 #f))

(define (x86-sqrtsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "sqrtsd" dst src #xf2 #x51 #f))

(define (x86-addss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "addss" dst src #xf3 #x58 #f))

(define (x86-addsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "addsd" dst src #xf2 #x58 #f))

(define (x86-subss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "subss" dst src #xf3 #x5c #f))

(define (x86-subsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "subsd" dst src #xf2 #x5c #f))

(define (x86-mulss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "mulss" dst src #xf3 #x59 #f))

(define (x86-mulsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "mulsd" dst src #xf2 #x59 #f))

(define (x86-divss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "divss" dst src #xf3 #x5e #f))

(define (x86-divsd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-mem? src))
              (and (x86-reg-xmm? dst) (x86-reg-xmm? src)))
          "invalid operands")
  (x86-sse-op cgc "divsd" dst src #xf2 #x5e #f))

;; From intel manual: The COMISD instruction differs from the UCOMISD instruction in that
;; it signals a SIMD floating-point invalid operation exception (#I) when a source operand
;; is either a QNaN or SNaN.
(define (x86-comisd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "comisd" dst src #x66 #x2f #f))

(define (x86-comiss cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "comiss" dst src #f #x2f #f))

(define (x86-pcmpeqb cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "pcmpeqb" dst src #x66 #x74 #f))

(define (x86-movd/movq cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (or (x86-reg? src) (x86-mem? src)))
              (and (x86-reg-xmm? src) (or (x86-reg? dst) (x86-mem? dst))))
          "invalid operands")
  (if (x86-reg-xmm? src)
      (x86-sse-op cgc "movq" src dst #x66 #x7e #f)
      (x86-sse-op cgc "movq" dst src #x66 #x6e #f)))

(define (x86-pxor cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "pxor" dst src #x66 #xEF #f))

(define (x86-psllw cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-imm? src) (< (x86-imm-int-value src) 255)))
          "invalid operands")
  (x86-sse-op cgc "psllw" dst src #x66 #xF3 6))

(define (x86-psrlw cgc dst src)
  (assert (or (and (x86-reg-xmm? dst) (x86-reg-xmm? src))
              (and (x86-reg-xmm? dst) (x86-imm? src) (< (x86-imm-int-value src) 255)))
          "invalid operands")
  (x86-sse-op cgc "psrlw" dst src #x66 #xD3 2))

(define (x86-cvtsi2sd cgc dst src)
  (assert (or (and (x86-reg-xmm? dst)
                   (x86-reg? src)
                   (not (x86-xmm? src))
                   (or (= (x86-reg-width src) 32) (= (x86-reg-width src) 64)))
              (and (x86-reg-xmm? dst)
                   (x86-mem? src)))
          "invalid operands")
  (x86-sse-op cgc "cvtsi2sd" dst src #xf2 #x2a #f))


(define (x86-sse-op cgc mnemonic dst src opcode1 opcode2 op)
  (if opcode1
    (asm-8 cgc opcode1)) ;; opcode

  (if (x86-reg? dst)

      (if (x86-imm? src)
          (begin (x86-opnd-prefix-opnd cgc 1 dst)
                 (asm-8 cgc #x0f)
                 (asm-8 cgc #x73)
                 (x86-opnd-modrm/sib cgc op dst)
                 (asm-8 cgc (x86-imm-int-value src))
                 (x86-listing cgc mnemonic (x86-reg-width dst) dst src))
          (begin (x86-opnd-prefix-reg-opnd cgc dst src)       ;; prefix
                 (asm-8 cgc #x0f)                             ;; opcode 1
                 (asm-8 cgc opcode2)                          ;; opcode 2
                 (x86-opnd-modrm/sib-reg-opnd cgc dst src)    ;; ModR/M
                 (x86-listing cgc mnemonic (x86-reg-width dst) dst src)))
    (begin (x86-opnd-prefix-reg-opnd cgc src dst)       ;; prefix
           (asm-8 cgc #x0f)                             ;; opcode 1
           (asm-8 cgc (+ opcode2 1))                    ;; opcode 2
           (x86-opnd-modrm/sib-reg-opnd cgc src dst) ;; ModR/M
           (x86-listing cgc mnemonic (x86-reg-width src) dst src))))

;;;----------------------------------------------------------------------------

(define x86-mem? _x86#x86-mem?)
(define x86-imm? _x86#x86-imm?)
(define x86-listing _x86#x86-listing)
(define x86-opnd-prefix-opnd _x86#x86-opnd-prefix-opnd)
(define x86-opnd-modrm/sib _x86#x86-opnd-modrm/sib)
(define x86-opnd-prefix-reg-opnd _x86#x86-opnd-prefix-reg-opnd)
(define x86-opnd-modrm/sib-reg-opnd _x86#x86-opnd-modrm/sib-reg-opnd)
