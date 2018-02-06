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

;; This is the most simple strategy
;; Generate lazy-code objects with given contexts until hard limit is reached
;; Then fallback to a generic version

(define lazy-code-entry? #f)
(define opt-static-mode #f)
(define new-sym #f)

;;-----------------------------------------------------------------------------
;; VERSIONS AND GENERIC TABLES

;; Exec mode
(define lco_versions (make-table test: eq?))
(define lco_generic  (make-table test: eq?))

;; Static mode
(define lco_static_versions (make-table test: eq?))
(define lco_static_generic (make-table test: eq?))

(define (lco-versions-table)
  (if opt-static-mode
      lco_static_versions
      lco_versions))

(define (lco-generic-table)
  (if opt-static-mode
      lco_static_generic
      lco_generic))

;;-----------------------------------------------------------------------------
;; PUBLIC

(define (lazy-code-versions lco)
  (let ((versions (table-ref (lco-versions-table) lco #f)))
    (or versions
        (let ((versions (make-table)))
          (table-set! (lco-versions-table) lco versions)
          versions))))

(define (lazy-code-versions-ctx lco)
  (let ((ctxs (map car (keep cddr (table->list (lazy-code-versions lco))))))
    ctxs))

(define (lazy-code-nb-versions lazy-code)
  (table-length (lazy-code-versions lazy-code)))

(define (lazy-code-nb-real-versions lazy-code)
  (count (table->list (lazy-code-versions lazy-code)) cddr))

(define (strat-get-options)
  strat-options)

;;-----------------------------------------------------------------------------
;; PRIVATE

(define opt-max-versions #f) ;; Limit of number of versions (#f=no limit, 0=only generic, ...)
(define opt-regalloc-vers #t)

;; Options specific to this strat
(define strat-options `(
  (--max-versions
    "Set a limit on the number of versions of lazy code objects"
    ,(lambda (args) (set! opt-max-versions (string->number (cadr args)))
                    (set! args (cdr args))
                    args))
  (--disable-regalloc-vers
    "Do not use register allocation information to specialize generated code"
    ,(lambda (args) (set! opt-regalloc-vers #f)
                    (set! get-version nregalloc-get-version)
                    (set! put-version nregalloc-put-version)
                    args))))

(define (lazy-code-generic lco)
  (table-ref (lco-generic-table) lco #f))

(define (lazy-code-generic-set! lco ctx version)
  (table-set! (lco-generic-table) lco (cons ctx version)))

(define (get-version lazy-code ctx)
  (let* ((key ctx)
         (versions (lazy-code-versions lazy-code))
         (version  (table-ref versions key #f)))
    (and version (cons #f (car version)))))

(define (put-version lazy-code ctx version real-version?)
  (let ((key ctx)
        (versions (lazy-code-versions lazy-code))
        (k (cons version real-version?)))
    (table-set! versions key k)
    k))

;;-----------------------------------------------------------------------------
;; !regalloc versioning

(define (version-key ctx)
  (list (ctx-stack ctx)
        (ctx-fs ctx)
        (ctx-ffs ctx)
        (map (lambda (ident) (list (car ident) (identifier-sslots (cdr ident)) (identifier-stype (cdr ident))))
             (ctx-env ctx))))

(define (nregalloc-get-version lazy-code ctx)
  (let* ((key (version-key ctx))
         (versions (lazy-code-versions lazy-code))
         (version  (table-ref versions key #f)))
    (if version
        (cons (car version) (cadr version))
        #f)))

(define (nregalloc-put-version lazy-code ctx version real-version?)
  (let* ((key (version-key ctx))
         (versions (lazy-code-versions lazy-code))
         (k (cons ctx (cons version real-version?))))
    (table-set! versions key k)
    (cdr k)))

;; !regalloc versioning
;;-----------------------------------------------------------------------------

(define (strat-label-from-stack lco stack)
  (let ((versions (lazy-code-versions lco)))
    (if versions
        (let loop ((versions (table->list versions)))
          (if (null? versions)
              #f
              (let ((cstack (if opt-regalloc-vers
                                (ctx-stack (caar versions))
                                (caaar versions)))
                    (label  (if opt-regalloc-vers
                                (cadar versions)
                                (caddar versions))))
                (if (equal? cstack stack)
                    label
                    (loop (cdr versions))))))
        #f)))

(define (limit-reached? lco)
  (and opt-max-versions
       (let ((nb-versions (lazy-code-nb-real-versions lco)))
         (>= nb-versions opt-max-versions))))

(define (strat-get-version lco ctx)
  (if opt-static-mode
      (strat-get-static-version  lco ctx)
      (strat-get-dynamic-version lco ctx)))

(define (strat-get-dynamic-version lco ctx)

  ;; CASE 1: a version exists for this ctx, use it
  (define (case-use-version dst-ctx version)
    (if dst-ctx
        (let ((k (put-version lco ctx #f #f)))
          (list version dst-ctx (lambda (label) (set-car! k label))))
        (list version ctx (lambda (r) r))))

  ;; CASE 2: no version for this ctx, and limit is not reached
  ;; then generate a new version
  (define (case-gen-version)
    (let ((k (put-version lco ctx #f #t)))
      (list #f ctx (lambda (label) (set-car! k label)))))

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

  (let* ((r (get-version lco ctx))
         (dst-ctx (and r (car r)))
         (version (and r (cdr r))))

    (cond ;;
          (version
             (case-use-version dst-ctx version))
          ;;
          ((not (limit-reached? lco))
             (case-gen-version))
          ;;
          ((lazy-code-generic lco) => (lambda (generic)
             (case-use-generic generic)))
          ;;
          (else
             (case-gen-generic)))))

;;-----------------------------------------------------------------------------

(define (strat-get-static-version lco ctx)

  ;; CASE 1: a version exists for this ctx, use it
  (define (case-use-version dst-ctx version)
    (if dst-ctx
        (let ((k (put-version lco ctx #f #f)))
          (list version dst-ctx (lambda (label) (set-car! k label))))
        (list version ctx (lambda (r) r))))

  (define (case-gen-version)
    (define (get-ctx ctx ctxs)
      (define stack-idx-max (length (ctx-stack ctx)))
      (let loop ((stack (ctx-stack ctx))
                 (stacks (map ctx-stack ctxs))
                 (stack-idx 0)
                 (ctx ctx))
        (if (= stack-idx stack-idx-max)
            ctx
            (let ((type (ctx-get-type ctx stack-idx)))
              (if (ctx-type-cst? type)
                  (let* ((types (map (lambda (ctx) (ctx-get-type ctx stack-idx)) ctxs))
                         (r (keep (lambda (t) (not (equal? t type))) types)))
                    (if (null? r)
                        (loop (cdr stack) (map cdr stacks) (+ stack-idx 1) ctx)
                        (let* ((type (ctx-get-type ctx stack-idx))
                               (moves/loc/ctx (ctx-get-free-reg #f ctx #f 0))
                               (moves (car moves/loc/ctx))
                               (loc   (cadr moves/loc/ctx))
                               (ctx   (caddr moves/loc/ctx))
                               (ctx   (ctx-set-loc ctx (stack-idx-to-slot ctx stack-idx) loc))
                               (ctx   (ctx-set-type ctx stack-idx (ctx-type-nocst type) #f)))
                          (loop (cdr stack) (map cdr stacks) (+ stack-idx 1) ctx))))
                  (loop (cdr stack) (map cdr stacks) (+ stack-idx 1) ctx))))))


    (let* ((ctx (get-ctx ctx (map car (table->list (lazy-code-versions lco)))))
           (k (put-version lco ctx #f #t)))
      (list #f ctx (lambda (label . a) (set-car! k label)))))


  (let* ((r (get-version lco ctx))
         (dst-ctx (and r (car r)))
         (version (and r (cdr r))))

    (cond ;;
          (version
             (case-use-version dst-ctx version))
          ;;
          (else
             (case-gen-version)))))
