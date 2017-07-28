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

;; TODO DESCRIPTION
(define lazy-code-entry? #f)
(define lco_versions (make-table test: eq?))
(define lco_generic  (make-table test: eq?))

;;------------------------------------------------------------------------------
;; PUBLIC

(define (lazy-code-versions lco)
  (let ((versions (table-ref lco_versions lco #f)))
    (or versions
        (let ((versions (make-table)))
          (table-set! lco_versions lco versions)
          versions))))

(define (lazy-code-nb-versions lazy-code)
  (table-length (lazy-code-versions lazy-code)))

(define (lazy-code-nb-real-versions lazy-code)
  (count (table->list (lazy-code-versions lazy-code)) cddr))

(define (strat-get-options)
  strat-options)

;;------------------------------------------------------------------------------
;; PRIVATE

(define opt-cst-variation-limit 10)

;; Options specific to this strat
(define strat-options `(
    (--strat-cst-variation-limit
      "(strat) Set the maximum number of times a constant can be used to generate a version"
      ,(lambda (args) (set! opt-cst-variation-limit (string->number (cadr args)))
                      (cdr args)))
))

(define (lazy-code-generic lco)
  (table-ref lco_generic lco #f))

(define (lazy-code-generic-set! lco ctx version)
  (table-set! lco_generic lco (cons ctx version)))

(define (get-version lazy-code ctx)
  (let* ((key (if (lazy-code-entry? lazy-code)
                  ctx;(ctx-stack ctx)
                  ctx))
         (versions (lazy-code-versions lazy-code))
         (version  (table-ref versions key #f)))
    (and version (car version))))

(define (put-version lazy-code ctx version real-version?)
  (let ((key (if (lazy-code-entry? lazy-code)
                 ctx;(ctx-stack ctx)
                 ctx))
        (versions (lazy-code-versions lazy-code)))
    (table-set! versions key (cons version real-version?))))

(define (strat-get-version lco ctx)

  ;; CASE 1: a version exists for this ctx, use it
  (define (case-use-version version)
    (list version ctx (lambda (r) r)))

  ;; CASE 2: no version for this ctx, and limit is not reached
  ;; then generate a new version
  (define (case-gen-version nctx)
    (list #f nctx (lambda (label-merge label-version)
                    (put-version lco ctx label-merge #f)
                    (lazy-code-generic-set! lco nctx label-version))))

  ;;
  (define (case-gen-unchanged)
    (list #f ctx (lambda (label) (put-version lco ctx label #t))))

  ;; CASE 3: no version for this ctx, limit reached, generic exists
  ;; then use the generic version
  (define (case-use-generic generic)
    (let ((callback (lambda (label-merge) (put-version lco ctx label-merge #f))))
      (list (cdr generic) (car generic) callback)))

  ;; CASE 4: no version for this ctx, limit reached, generic does not exist
  ;; then generate the generic version
  (define (case-gen-generic)
    (let* ((gctx (ctx-generic ctx))
           (callback (lambda (label-merge label-version)
                       (put-version lco ctx label-merge #f)
                       (lazy-code-generic-set! lco gctx label-version))))
      (list #f gctx callback)))

  (let ((version (get-version lco ctx)))

    ;TODO reenable (if (not opt-const-vers)  (error "Internal error"))

    (if version
        ;;
        (case-use-version version)
        ;;
        (let ((ctx-mod (GET_CTX_MOD lco ctx)))
          (if (eq? ctx ctx-mod)
              ;; CASE 2.1 there is no change, ctx can be used to generate new version
              (case-gen-unchanged)
              ;; CASE 2.2 ctx changed
              (case-gen-version ctx-mod))))))


;;----- TODO
;;----- TODO

;; TODO: problème des fonctions qui doivent quand meme etre propagées

(define lco-cst-lst (make-table test: eq?))

(define (lco-cst-lst-get lco ctx)
  (let ((r (table-ref lco-cst-lst lco #f)))
    ;; TODO assert
    (if (and r
            (not (eq? (length r)
                      (length (ctx-stack ctx)))))
        (error "Internal error")
    ;;
    (or r
        (let ((lst (map (lambda (n) 0) (ctx-stack ctx))))
          (table-set! lco-cst-lst lco lst)
          lst)))))

(define (lco-cst-lst-update lco lst)
  (table-set! lco-cst-lst lco lst))

(define (RETROGRADE ctx sum-lst idx)
  (cond ((null? sum-lst)
           ctx)
        ((> (car sum-lst) opt-cst-variation-limit)
           (let* ((r (ctx-get-free-reg #f ctx #f 0)) ;; moves/reg/ctx
                  (reg (cadr r))
                  (ctx (caddr r))
                  ;;
                  (ctx (ctx-set-type ctx idx (ctx-type-nocst (list-ref (ctx-stack ctx) idx)) #t))
                  (ctx (ctx-set-loc  ctx (stack-idx-to-slot ctx idx) reg)))
             (RETROGRADE ctx (cdr sum-lst) (+ idx 1))))
        (else
           (RETROGRADE ctx (cdr sum-lst) (+ idx 1)))))

(define (GET_CTX_MOD lco ctx)
  ;; TODO: add MAX_VERSIONS
  (let* ((lco-lst (lco-cst-lst-get lco ctx))
         (cur-lst (map (lambda (n) (if (ctx-type-cst? n) 1 0)) (ctx-stack ctx)))
         (sum-lst (map + lco-lst cur-lst))
         (ok? (foldr (lambda (el r) (and (<= el opt-cst-variation-limit) r)) #t sum-lst)))
    (if ok?
        (begin (lco-cst-lst-update lco sum-lst)
               ctx)
        (begin
          ;;1. On rétrograde les cst en trop dans le ctx
          (let ((ctx (RETROGRADE ctx sum-lst 0)))
            ;; TODO test opt
            (let ((nctx
                    (let loop ((versions (table->list (lazy-code-versions lco))))
                      (cond ((null? versions) #f)
                            ((equal? (ctx-stack ctx)
                                     (ctx-stack (caar versions)))
                               (caar versions))
                            (else (loop (cdr versions)))))))

              (lco-cst-lst-update lco sum-lst)
              (or nctx ctx)))))))
