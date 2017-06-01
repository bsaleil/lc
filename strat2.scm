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

;; If the context does not contain constants, use simple strategy
;; (generate until limit is reached).
;; If the context contains constants, generate versions until
;; CST_MAX_VERSIONS is reached, then fallback to non const versions

(define CST_MAX_VERSIONS 10)

(define opt-max-versions #f)
(define lazy-code-entry? #f)

(define lco_versions (make-table test: eq?))
(define lco_cst_versions (make-table test: eq?))
(define lco_generic  (make-table test: eq?))

;;------------------------------------------------------------------------------
;; PUBLIC

(define (lazy-code-versions* versions_table)
  (lambda (lco)
    (let ((versions (table-ref versions_table lco #f)))
      (or versions
          (let ((versions (make-table)))
            (table-set! versions_table lco versions)
            versions)))))

(define lazy-code-versions     (lazy-code-versions* lco_versions))
(define lazy-code-cst-versions (lazy-code-versions* lco_cst_versions))

(define (lazy-code-nb-versions lazy-code)
  (table-length (lazy-code-versions lazy-code)))

(define (lazy-code-nb-real-versions lazy-code versions-accessor)
  (count (table->list (versions-accessor lazy-code)) cddr))

;;------------------------------------------------------------------------------
;; PRIVATE

(define (lazy-code-generic lco)
  (table-ref lco_generic lco #f))

(define (lazy-code-generic-set! lco ctx version)
  (table-set! lco_generic lco (cons ctx version)))

(define (get-version* versions-accessor)
  (lambda (lazy-code ctx)
    (let* ((key (if (lazy-code-entry? lazy-code)
                    ctx;(ctx-stack ctx)
                    ctx))
           (versions (versions-accessor lazy-code))
           (version  (table-ref versions key #f)))
      (and version (car version)))))

(define get-version     (get-version* lazy-code-versions))
(define get-version-cst (get-version* lazy-code-cst-versions))

(define (put-version* versions-accessor)
  (lambda (lazy-code ctx version real-version?)
    (let ((key (if (lazy-code-entry? lazy-code)
                   ctx;(ctx-stack ctx)
                   ctx))
          (versions (versions-accessor lazy-code)))
      (table-set! versions key (cons version real-version?)))))

(define put-version     (put-version* lazy-code-versions))
(define put-version-cst (put-version* lazy-code-cst-versions))

(define (limit-reached? lco)
  (and opt-max-versions
       (let ((nb-versions (lazy-code-nb-real-versions lco lazy-code-versions)))
         (>= nb-versions opt-max-versions))))

(define (cst-limit-reached? lco)
  (let ((nb-versions (lazy-code-nb-real-versions lco lazy-code-cst-versions)))
    (>= nb-versions CST_MAX_VERSIONS)))

(define (strat-get-version lco ctx)

  ;; CASE 1: a version exists for this ctx, use it
  (define (case-use-version version)
    (list version ctx (lambda (r) r)))

  ;; CASE 2: no version for this ctx, and limit is not reached
  ;; then generate a new version
  (define (case-gen-version with-cst?)
    (let ((put (if with-cst? put-version-cst put-version)))
      (list #f ctx (lambda (label) (put lco ctx label #t)))))

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

  (define (case-gen-noncst)
    (let* ((nctx (ctx-non-cst ctx))
           (callback (lambda (label-merge label-version)
                       (put-version-cst lco ctx  label-merge #f)
                       (put-version     lco nctx label-version #t))))
      (list #f nctx callback)))

  (let ((version (if (ctx-contains-cst ctx)
                     (get-version-cst lco ctx)
                     (get-version lco ctx))))

    (cond
      ;;
      (version (case-use-version version))
      ;;
      ((and (ctx-contains-cst ctx)
            (not (cst-limit-reached? lco)))
         (case-gen-version #t))
      ;;
      ((and (not (ctx-contains-cst ctx))
            (not (limit-reached? lco)))
         (case-gen-version #f))
      ;;
      ((and (ctx-contains-cst ctx)
            (not (limit-reached? lco)))
         (case-gen-noncst))
      ;;
      (else
         (let ((generic (lazy-code-generic lco)))
           (if generic
               (case-use-generic generic)
               (case-gen-generic)))))))

;;------------------------------------------------------------------------------

(define (ctx-contains-cst ctx)
  (foldr (lambda (type r)
           (or (ctx-type-cst? type) r))
         #f
         (ctx-stack ctx)))


(define (ctx-non-cst ctx)
  (define (ctx-non-cst-h idx ctx)
    (if (= idx (length (ctx-stack ctx)))
        ctx
        (let ((type (ctx-get-type ctx idx)))
          (if (ctx-type-cst? type)
              (let ((ntype (ctx-type-nocst type)))
                (let ((r (ctx-get-free-reg #f ctx #f 0)))
                  (let* ((ctx (ctx-set-loc (caddr r) (stack-idx-to-slot ctx idx) (cadr r)))
                         (ctx (ctx-set-type ctx idx ntype #t)))
                    (ctx-non-cst-h (+ idx 1) ctx))))
              (ctx-non-cst-h (+ idx 1) ctx)))))
  (ctx-non-cst-h 0 ctx))
