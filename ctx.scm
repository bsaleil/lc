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

(include "utils.scm")
(include "core.scm")

(define mem-header #f)

;;-----------------------------------------------------------------------------
;; Ctx

;; Compilation context
(define-type ctx
  stack     ;; virtual stack of types
  slot-loc  ;; alist which associates a virtual stack slot to a location
  free-regs ;; list of current free virtual registers
  env       ;; alist which associates a variable symbol to an identifier object
  nb-args   ;; number of arguments of function of the current stack frame
  fs        ;; current frame size
)

;; Generate initial free regs list
(define (ctx-init-free-regs)
  (build-list (length regalloc-regs) (lambda (i)
                                       (string->symbol
                                         (string-append "r" (number->string i))))))

;; Create an empty context
(define (ctx-init)
  (make-ctx '()
            '()
            (ctx-init-free-regs)
            '()
            -1
            0))

;; Create an empty context with given stack
(define (ctx-init-with-stack stack)
  (let ((ctx (ctx-init)))
    (make-ctx
      stack
      (ctx-slot-loc ctx)
      (ctx-free-regs ctx)
      (ctx-env ctx)
      (ctx-nb-args ctx)
      (ctx-fs ctx))))

;; Convert stack list index to slot
(define (ctx-lidx-to-slot ctx lidx)
  (- (length (ctx-stack ctx)) lidx 1))

;; Convert stack slot to list index
(define (ctx-slot-to-lidx ctx slot)
  (- (length (ctx-stack ctx)) slot 1))

;; Return type associated to identifier
(define (ctx-identifier-type ctx identifier)
  (if (eq? 'free (identifier-kind identifier))
      (identifier-stype identifier)
      (let ((slot (car (identifier-sslots identifier))))
        (list-ref (ctx-stack ctx) (ctx-slot-to-lidx ctx slot)))))

;; Return location associated to stack slot
(define (ctx-get-loc ctx slot)
  (let ((r (assoc slot (ctx-slot-loc ctx))))
    (if r
        (cdr r)
        (error "NYI error"))))

;; Return location associated to identifier
(define (ctx-identifier-loc ctx identifier #!optional (orig-loc? #f))

  ;; Return 'best' location
  ;; Return register location if identifier is available in a register only or in register and memory
  ;; Return memory location instead
  (define (get-loc slots ret)
    (if (null? slots)
        ret
        (let* ((r (assoc (car slots) (ctx-slot-loc ctx)))
               (loc (cdr r)))
          (if (ctx-loc-is-register? loc)
              loc
              (get-loc (cdr slots) loc)))))

  (if (eq? (identifier-kind identifier) 'free)
      (let ((slots (identifier-sslots identifier)))
        (if (or orig-loc? (null? slots))
            (identifier-cloc identifier)
            (get-loc slots #f)))
      (let ((slots (identifier-sslots identifier)))
        (if orig-loc?
            (ctx-get-loc ctx (list-ref slots (- (length slots) 1)))
            (get-loc slots #f)))))

;; loc is a register if loc is of the form (rx . value)
;; loc is a free variable location if loc is of the form (fx . value)
;; loc is a memory location if loc if of the form (integer . value)
;; Return #t if location is a register, else return #f
(define (ctx-loc-is-register? loc)
  (and (symbol? loc)
       (char=? (string-ref (symbol->string loc) 0) #\r)))

;; Return #t if location is a memory location, else return #f
(define (ctx-loc-is-memory? loc)
  (integer? loc))

;; Return #t if location is a free variable location, else return #f
(define (ctx-loc-is-floc? loc)
  (and (symbol? loc)
       (char=? (string-ref (symbol->string loc) 0) #\f)))

;; Is loc the original location of identifier ?
;; (floc for free identifier, or first mloc for local identifier)
(define (ctx-loc-is-orig-loc ctx identifier loc)
  (if (eq? (identifier-kind identifier) 'free)
      (eq? loc (identifier-cloc identifier))
      (let ((orig-slot (list-ref (identifier-sslots identifier)
                                 (- (length (identifier-sslots identifier)) 1))))
        (eq? loc (ctx-get-loc ctx orig-slot)))))

;; Get free variable position in closure from free location
(define (ctx-floc-to-fpos floc)
  (- (string->number
       (list->string
         (cdr (string->list
                (symbol->string floc)))))
     1))

;; Return ident pair (id . identifier) which is on slot 'slot'
(define (ctx-ident-at ctx slot)
  (define (env-ident-at env)
    (if (null? env)
        #f
        (let ((identifier (cdar env)))
          (if (member slot (identifier-sslots identifier))
              (car env)
              (env-ident-at (cdr env))))))
  (env-ident-at (ctx-env ctx)))

;;-----------------------------------------------------------------------------
;; Register allocation


;;-----------------------------------------------------------------------------
;; Ctx

;; Identifier object
(define-type identifier
  kind   ;; symbol 'free or 'local
  sslots ;; list of stack slots where the variable is
  flags  ;; list of variable
  stype  ;; ctx type (copied to virtual stack)
  cloc   ;; closure slot if free variable
)
