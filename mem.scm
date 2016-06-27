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

(define MSECTION_BIGGEST 255)

(c-declare
"
// TODO: remove signal stack when gambit accepts new flag
#include <stdlib.h> // exit
#include <signal.h>
stack_t mss;
void initmss()
{
  if ((mss.ss_sp = malloc(SIGSTKSZ)) == NULL)
      exit(0);
  mss.ss_size = SIGSTKSZ;
  mss.ss_flags = 0;
  if (sigaltstack(&mss,(stack_t *)0) < 0)
      exit(0);
}

___U64 alloc_still(___U64 stag, ___U64 bytes)
{
  ___U64 r = ___alloc_scmobj(___PSTATE,stag,bytes);
  if ((r & 3)==0)
  {
    puts(\"Error: Heap overflow\\n\");
    exit(0);
  }
  return r;
}

___U64  get___alloc_still_addr()      { return (___U64)&alloc_still; }

int callHL()
{
  int r = ___heap_limit(___PSPNC) && ___garbage_collect (___PSP 0);
  if (r != 0)
  {
    puts(\"Error: Heap overflow\");
    exit(0);
  }
  return 0;
}

___U64  get_pstate_addr()            { return (___WORD)&___PSTATE;             }
___WORD get_hp_addr()                { return (___WORD)&___PSTATE->hp;         }
___WORD get_heap_limit_addr()        { return (___WORD)&___PSTATE->heap_limit; }
___U64  get___heap_limit_addr()      { return (___U64)&callHL; }

")

;; TODO: remove signal stack when gambit accepts new flag
(define (init-mss)
  ((c-lambda () void "initmss")))

(define (get___heap_limit-addr)
  ((c-lambda () long "get___heap_limit_addr")))

(define (get___alloc_still-addr)
  ((c-lambda () long "get___alloc_still_addr")))

(define (get-heap_limit-addr)
  ((c-lambda () long "get_heap_limit_addr")))

(define (alloc-still-vector len)
  ((c-lambda (int)
             scheme-object
             "___result = ___EXT(___make_vector) (___PSTATE, ___arg1, ___FAL);")
   len))

(define (get-pstate-addr)
  ((c-lambda () long "get_pstate_addr")))

(define (get-hp-addr)
  ((c-lambda () long "get_hp_addr")))

(define (get-words-from-byte bytes)
  (let ((words (quotient bytes 8))
        (extra (modulo bytes 8)))
    (if (> extra 0)
        (+ words 1)
        words)))

;; Alloc from nbbytes in rax (and align alloc-ptr)
;; Return encoded scheme object in rax
;; sizeloc is DESTROYED !
(define (gen-allocation-rt cgc stag sizeloc)

  ;; TODO: use a dispatch to generate only still or not-still code

  (define label-alloc-beg (asm-make-label #f (new-sym 'alloc_begin_)))
  (define label-alloc-end (asm-make-label #f (new-sym 'alloc_end_)))
  (define label-alloc-ret (asm-make-label #f (new-sym 'alloc_ret_)))
  (define label-not-still (asm-make-label #f (new-sym 'alloc_not_still_)))
  (define label-alloc-still-end (asm-make-label #f (new-sym 'alloc_still_end_)))

  (x86-label cgc label-alloc-beg)

  ;; Save size (bytes)
  (x86-upush cgc sizeloc)
  ;; Align sizeloc (8 bytes)
  (x86-add cgc sizeloc (x86-imm-int 7))
  (x86-mov cgc selector-reg (x86-imm-int -8))
  (x86-and cgc sizeloc selector-reg)
  ;; Save aligned size
  (x86-upush cgc sizeloc)

  ;; see TODO
  (x86-cmp cgc sizeloc (x86-imm-int MSECTION_BIGGEST))
  (x86-jl cgc label-not-still) ;; TODO jl or jle ?
    ;; TODO: write comments, and rewrite optimized code sequence
    (x86-ppush cgc (x86-imm-int stag)) ;; stag is not encoded, push it to pstack
    (x86-pcall cgc label-alloc-still-handler)
    (x86-mov cgc (x86-mem 0 (x86-usp)) (x86-imm-int 0) 64) ;; 0
    (x86-add cgc (x86-rsp) (x86-imm-int 8)) ;; remove stag
    (x86-jmp cgc label-alloc-still-end)
  (x86-label cgc label-not-still)

  ;; Update alloc ptr
  (x86-lea cgc alloc-ptr (x86-mem 8 alloc-ptr sizeloc))

  ;; if hp <= heap_limit, alloc ok
  (x86-mov cgc (x86-rax) (x86-imm-int (+ (* 5 8) block-addr)))
  (x86-cmp cgc alloc-ptr (x86-mem 0 (x86-rax)) 64)
  (x86-jle cgc label-alloc-end)

    ;; call heap-limit
    (x86-pcall cgc label-heap-limit-handler)
    ;; rax = encoded ptr to obj
    (x86-lea cgc (x86-rax) (x86-mem TAG_MEMOBJ alloc-ptr))
    ;; Update alloc ptr
    (x86-mov cgc selector-reg (x86-mem 0 (x86-usp))) ;; get aligned size
    (x86-lea cgc alloc-ptr (x86-mem 8 alloc-ptr selector-reg))
    (x86-jmp cgc label-alloc-ret)

  (x86-label cgc label-alloc-end)
  ;; rax = encoded ptr to obj
  (x86-lea cgc (x86-rax) (x86-mem (- TAG_MEMOBJ 8) alloc-ptr))
  (x86-sub cgc (x86-rax) (x86-mem 0 (x86-usp)))

  (x86-label cgc label-alloc-ret)
  (x86-mov cgc selector-reg (x86-mem 8 (x86-usp))) ;; get saved nbytes (no aligned)
  (x86-shl cgc selector-reg (x86-imm-int 8))
  (x86-or  cgc selector-reg (x86-imm-int (mem-header 0 stag)))
  (x86-mov cgc (x86-mem (- TAG_MEMOBJ) (x86-rax)) selector-reg) ;; write header

  (x86-label cgc label-alloc-still-end)
  ;; Restore selector
  (x86-mov cgc selector-reg (x86-imm-int 0))
  ;; Remove saved values
  (x86-add cgc (x86-usp) (x86-imm-int 16)))

;; Alloc object of type stag of size nbytes + 8 (header)
;; For performance reason, unlike gen-allocation-rt,
;; this function does *not* return encoded object in rax.
;; Caller need to load address of object
(define (gen-allocation-imm cgc stag nbytes)

  (define label-alloc-beg (asm-make-label #f (new-sym 'alloc_begin_)))
  (define label-alloc-end (asm-make-label #f (new-sym 'alloc_end_)))

  (if (> nbytes MSECTION_BIGGEST)
      (error "NYI - ALLOC STILL OBJ"))

  (assert (= (modulo nbytes 4) 0) "GC internal error")

  (x86-label cgc label-alloc-beg)
  ;; hp += (nbytes + 8)
  (x86-add cgc alloc-ptr (x86-imm-int (+ nbytes 8)))

  ;; if hp <= heap_limit, alloc ok
  (x86-mov cgc (x86-rax) (x86-imm-int (+ (* 5 8) block-addr)))
  (x86-cmp cgc alloc-ptr (x86-mem 0 (x86-rax)) 64)
  (x86-jle cgc label-alloc-end)
  ;; else
    (x86-pcall cgc label-heap-limit-handler)
    (x86-add cgc alloc-ptr (x86-imm-int (+ nbytes 8)))

  ;; write header
  (x86-label cgc label-alloc-end)
  (x86-mov cgc (x86-mem (- 0 nbytes 8) alloc-ptr) (x86-imm-int (mem-header nbytes stag)) 64))

;; TODO remove when all implemented
(define (gen-allocation cgc ctx stag length rt-size?)
  (error "NYI, use gen-allocation-imm for now"))

;; Generate an heap object header
;; using layout used by Gambit.
;; NOTE : 'life' field is not used and set to 0.
(define (mem-header length stag)
    ;; => Length (56 bits) | sTag (5 bits) | Life (3 bits)
    (+ (arithmetic-shift length 8) (arithmetic-shift stag 3) 0))
