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

;;-----------------------------------------------------------------------------
;; SLOT DEBUG

(define (get-slot slot)
  (get-i64 (+ block-addr (* 8 (cdr (assoc slot debug-slots))))))

(define (gen-set-slot cgc slot imm)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-push cgc (x86-rax))
    (x86-push cgc (x86-rbx))
    (x86-mov  cgc (x86-rax) (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov  cgc (x86-rbx) (x86-imm-int imm))
    (x86-mov  cgc (x86-mem 0 (x86-rax)) (x86-rbx))
    (x86-pop cgc (x86-rbx))
    (x86-pop cgc (x86-rax))))

(define (gen-get-slot cgc slot r)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-mov cgc r (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov cgc r (x86-mem 0 r))))

(define (gen-inc-slot cgc slot)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-ppush cgc (x86-rax))
    (x86-ppush cgc (x86-rbx))
    (x86-mov cgc (x86-rax) (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov cgc (x86-rbx) (x86-mem 0 (x86-rax)))
    (x86-inc cgc (x86-rbx))
    (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-rbx))
    (x86-ppop cgc (x86-rbx))
    (x86-ppop cgc (x86-rax))))
