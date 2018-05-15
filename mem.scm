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

(include "~~lib/_x86#.scm")
(include "~~lib/_asm#.scm")

(define MSECTION_BIGGEST 255) ;; words
(define MSECTION_FUDGE  8192) ;; words

(define (mem-still-required? nbytes)
  (>= nbytes (* MSECTION_BIGGEST 8)))

(define (mem-can-alloc-group? nbytes)
  (< nbytes (* MSECTION_FUDGE 8)))

(c-declare
"
#include <stdio.h>  // puts
#include <stdlib.h> // exit
#include <unistd.h> // getpid
#include <math.h> // sin cos atan
#include <stdint.h>

int ___heap_limit(); // Gambit ___heap_limit

___U64 alloc_still(___U64 stag, ___U64 bytes)
{
  ___U64 r = ___alloc_scmobj(___PSTATE,stag,bytes);
  if ((r & 3)==0)
  {
    puts(\"Error: Heap overflow\\n\");
    exit(0);
  }
  ___still_obj_refcount_dec(r);
  return r;
}

___U64  get___alloc_still_addr()      { return (___U64)&alloc_still; }

int callHL(___U64 ustackptr)
{
  lc_global_ctx.lc_stack_ptr = ustackptr;
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

#include <signal.h>

void lcIntHandler(int p) {
  // Write is reentrant
  write(1, \"*** INTERRUPTED IN ASM (SIGINT)\\n\",32);
  signal(SIGINT, SIG_DFL);
  kill(getpid(), SIGINT);
}

void initc()
{
  signal(SIGINT, lcIntHandler);
}

void writeLcStack(___U64 stack)
{
    lc_global_ctx.lc_stack = stack;
}

void writeLcGlobal(___U64 global)
{
    lc_global_ctx.lc_global = global;
}

// This function detects if an xmm register can be set to a value using tricks presented here:
// https://blogs.msdn.microsoft.com/oldnewthing/20141215-00/?p=43403
// It detects if the binary ieee754 double precision representation contains a single pack of 1
// If it is the case, it returns the left (psll) and right (psrl) shifts to apply after pcmpeq
// The result is returned in a 64 bits value:
// .. 16 bits .. | .. 16 bits .. | .. 16 bits .. | .. 16 bits ..
//    unused           psrl            psll        if 1, the tricks can be used
//                                                 if 0, the tricks cannot be used
// i must be a non-zero value
uint64_t c_xmm_imm_shift(uint64_t i)
{
    int8_t highest = 63;
    int8_t lowest = 0;
    uint64_t start = pow(2,63);
    while ((i & start) == 0)
    {
        highest--;
        start = start >> 1;
    }

    lowest = highest;
    while ((i & start) && (lowest >= 0))
    {

        i = i ^ start;
        lowest--;
        start = start >> 1;
    }

    if (i != 0)
        return 0;

    uint64_t l = (lowest + 1);
    uint64_t r = (64 - highest + lowest);
    return (i == 0) | l << 16 | r << 32;
}
")

;; TODO: remove signal stack when gambit accepts new flag
(define (init-c)
  ((c-lambda () void "initc")))

(define (get___heap_limit-addr)
  ((c-lambda () long "get___heap_limit_addr")))

(define (get___alloc_still-addr)
  ((c-lambda () long "get___alloc_still_addr")))

(define (get-sin-addr)
  ((c-lambda () long "___result = &sin;")))
(define (get-cos-addr)
  ((c-lambda () long "___result = &cos;")))
(define (get-atan-addr)
  ((c-lambda () long "___result = &atan;")))

(define (get-heap_limit-addr)
  ((c-lambda () long "get_heap_limit_addr")))

(define (alloc-still-vector len)
  ((c-lambda (int)
             scheme-object
             "___result = ___EXT(___make_vector) (___PSTATE, ___arg1, ___FAL);")
   len))

(define (write_lc_stack addr)
  ((c-lambda (long) void "writeLcStack") addr))

(define (write_lc_global addr)
  ((c-lambda (long) void "writeLcGlobal") addr))

(define (write_lc_stack_ptr usp)
  ((c-lambda (long) void "lc_global_ctx.lc_stack_ptr = ___arg1;") usp))

(define (write_lc_stack_desc desc)
  ((c-lambda (long) void "lc_global_ctx.lc_stack_desc = ___arg1;") desc))

(define (write_lc_stack_usedesc usedesc)
  ((c-lambda (long) void "lc_global_ctx.lc_stack_usedesc = ___arg1;") usedesc))

(define (get_lc_stack_desc_addr)
  ((c-lambda () long "___result = &lc_global_ctx.lc_stack_desc;")))

(define (get_lc_stack_usedesc_addr)
  ((c-lambda () long "___result = &lc_global_ctx.lc_stack_usedesc;")))

(define (get_lc_stack_ptr_addr)
  ((c-lambda () long "___result = &lc_global_ctx.lc_stack_ptr;")))

(define (block_gc)
  ((c-lambda () void "lc_global_ctx.gc_lock = 1;")))

(define (unblock_gc)
  ((c-lambda () void "lc_global_ctx.gc_lock = 0;")))

(define (xmm_imm_shift imm)
  (let* ((r ((c-lambda (unsigned-long) long "c_xmm_imm_shift") imm))
         (opt? (= (bitwise-and r 65535) 1)))
    (if opt?
        (let ((psll (bitwise-and (arithmetic-shift r -16) 65535))
              (psrl (bitwise-and (arithmetic-shift r -32) 65535)))
          (cons psrl psll))
        #f)))

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
(define (gen-allocation-rt cgc stag sizeloc gc-desc)

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
  (x86-mov cgc selector-reg (x86-imm-int (to-64-value (obj-encoding 0)))) ;; we need to reset selector in case gc is triggered
  ;; Save aligned size
  (x86-upush cgc sizeloc)

  (x86-cmp cgc sizeloc (x86-imm-int (* 8 MSECTION_BIGGEST)))
  (x86-jl cgc label-not-still) ;; TODO jl or jle ?
    ;; TODO: write comments, and rewrite optimized code sequence
    (x86-ppush cgc (x86-imm-int stag)) ;; stag is not encoded, push it to pstack
    (x86-mov cgc (x86-rax) (x86-imm-int (+ gc-desc 2))) ;; TODO (NOTE): see other call to label-alloc-still-handler
    (x86-upush cgc (x86-rax))
    (x86-pcall cgc label-alloc-still-handler)
    (x86-add cgc (x86-usp) (x86-imm-int 8))
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
  (if opt-nan-boxing
      ;; change encoding from tagging to nan-boxing
      (begin (x86-mov cgc selector-reg (x86-imm-int (to-64-value (+ NB_MASK_MEM TAG_MEMOBJ))))
             (x86-xor cgc (x86-rax) selector-reg)))
  ;; Restore selector
  (x86-mov cgc selector-reg (x86-imm-int (obj-encoding 0)))
  ;; Remove saved values
  (x86-add cgc (x86-usp) (x86-imm-int 16)))

;; Alloc object of type stag of size nbytes + 8 (header)
;; For performance reason, unlike gen-allocation-rt,
;; this function does *not* return encoded object in rax.
;; Caller needs to load address of object
(define (gen-allocation-imm cgc stag nbytes gc-desc)

  (define label-alloc-beg (asm-make-label #f (new-sym 'alloc_begin_)))
  (define label-alloc-end (asm-make-label #f (new-sym 'alloc_end_)))

  (assert (= (modulo nbytes 4) 0) "GC internal error")
  (assert (not (mem-still-required? nbytes)) "Internal error")

  (if (and opt-stats (eq? stag STAG_FLONUM))
      (gen-inc-slot cgc 'flbox))

  (x86-label cgc label-alloc-beg)
  ;; hp += (nbytes + 8)
  (x86-add cgc alloc-ptr (x86-imm-int (+ nbytes 8)))

  ;; if hp <= heap_limit, alloc ok
  (x86-mov cgc (x86-rax) (x86-imm-int (+ (* 5 8) block-addr)))
  (x86-cmp cgc alloc-ptr (x86-mem 0 (x86-rax)) 64)

  ;(x86-jle cgc label-alloc-end)
  ;; else
    (x86-mov cgc (x86-rax) (x86-imm-int gc-desc))
    (x86-upush cgc (x86-rax))
    (x86-pcall cgc label-heap-limit-handler)
    (x86-add cgc (x86-usp) (x86-imm-int 8))
    (x86-add cgc alloc-ptr (x86-imm-int (+ nbytes 8)))
  ;; write header
  (x86-label cgc label-alloc-end)

  (x86-mov cgc (x86-mem (- 0 nbytes 8) alloc-ptr) (x86-imm-int (mem-header nbytes stag)) 64))

(define (gen-allocation-imm-sti cgc stag nbytes gc-desc)

  (assert (= (modulo nbytes 4) 0) "GC internal error")
  (assert (mem-still-required? nbytes) "Internal error")

  ;; Save size (bytes)
  (x86-upush cgc (x86-imm-int nbytes))
  ;; Save aligned size ;; TODO change gen-alloc-still api
  (x86-upush cgc (x86-imm-int nbytes))
  ;; Stag is not encoded, push it to pstack
  (x86-ppush cgc (x86-imm-int stag))
  ;; Alloc
  (x86-mov cgc (x86-rax) (x86-imm-int (+ gc-desc 2))) ;; TODO (NOTE): +2 for nbytes pushed before (no need to adjust fo (set it to 00))
  (x86-upush cgc (x86-rax))
  (x86-pcall cgc label-alloc-still-handler)
  (x86-add cgc (x86-usp) (x86-imm-int 8))
  ;; Clean stacks
  (x86-add cgc (x86-rsp) (x86-imm-int 8))
  (x86-add cgc (x86-usp) (x86-imm-int 16)))

;; Generate an heap object header
;; using layout used by Gambit.
(define (mem-header length stag #!optional (life LIFE_MOVE))
    ;; => Length (56 bits) | sTag (5 bits) | Life (3 bits)
    (+ (arithmetic-shift length 8) (arithmetic-shift stag 3) life))
