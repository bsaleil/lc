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
  (count (table->list (lazy-code-versions lazy-code)) caddr))

(define (lazy-code-nb-real-cont-versions lazy-code)
  (count (table->list (lazy-code-versions lazy-code)) (lambda (e) (and (caddr e) (cadddr e)))))

(define (strat-get-options)
  strat-options)

;;------------------------------------------------------------------------------
;; PRIVATE

(define opt-max-versions #f) ;; Limit of number of versions (#f=no limit, 0=only generic, ...)
(define opt-max-cont-versions #f) ;; TODO

;; Options specific to this strat
(define strat-options `(
  (--max-versions
    "Set a limit on the number of versions of lazy code objects"
    ,(lambda (args) (set! opt-max-versions (string->number (cadr args)))
                    (set! args (cdr args))
                    args))
  (--max-cont-versions
    "Set a limit on the nujmber of versions with cst continuation"
    ,(lambda (args) (set! opt-max-cont-versions (string->number (cadr args)))
                    (set! args (cdr args))
                    args))))

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

(define (put-version lazy-code ctx version real-version? cont-cst-version?)
  (let ((key (if (lazy-code-entry? lazy-code)
                 ctx
                 ctx))
        (versions (lazy-code-versions lazy-code))
        (k (list version real-version? cont-cst-version?)))
    (table-set! versions key k)
    k))

(define (limit-reached? lco)
  (and opt-max-versions
       (let ((nb-versions (lazy-code-nb-real-versions lco)))
         (>= nb-versions opt-max-versions))))

(define (limit-cont-reached? lco)
  (and opt-max-cont-versions
       (let ((nb-versions (lazy-code-nb-real-cont-versions lco)))
         (>= nb-versions opt-max-cont-versions))))

(define (strat-get-version lco ctx)

  ;; CASE 1: a version exists for this ctx, use it
  (define (case-use-version version)
    (list version ctx (lambda (r) r)))

  ;; CASE 2: no version for this ctx, and limit is not reached
  ;; then generate a new version
  (define (case-gen-version-cont-cst)
    (let ((k (put-version lco ctx #f #t #t)))
      (list #f ctx (lambda (label) (set-car! k label)))))

  ;; TODO split in two cases
  (define (case-gen-version-cont-ncst)
    (if (ctx-const-continuation? ctx)
        (begin (set! ctx (ctx-no-const-continuation ctx))
               (let ((k (put-version lco ctx #f #t #f)))
                 (list #f ctx (lambda (label-merge label-version)
                                        (put-version lco ctx label-merge #f #f)
                                        (set-car! k label-version)))))

        (let ((k (put-version lco ctx #f #t #f)))
          (list #f ctx (lambda (label) (set-car! k label))))))


  ;; CASE 3: no version for this ctx, limit reached, generic exists
  ;; then use the generic version
  (define (case-use-generic generic)
    (let ((callback (lambda (label-merge) (put-version lco ctx label-merge #f #f))))
      (list (cdr generic) (car generic) callback)))

  ;; CASE 4: no version for this ctx, limit reached, generic does not exist
  ;; then generate the generic version
  (define (case-gen-generic)
    (let* ((gctx (ctx-generic ctx))
           (callback (lambda (label-merge label-version)
                       (put-version lco ctx label-merge #f #f)
                       (lazy-code-generic-set! lco gctx label-version))))
      (list #f gctx callback)))

  (if (< opt-max-versions opt-max-cont-versions) (error "Internal error strat1cp"))

  (let ((version (get-version lco ctx)))

    (cond (version
            (case-use-version version))
          ((and (ctx-const-continuation? ctx)
                (not (limit-cont-reached? lco)))
            (case-gen-version-cont-cst))
          ((not (limit-reached? lco))
            (case-gen-version-cont-ncst))
          ((lazy-code-generic lco) =>
            (lambda (generic)
              (case-use-generic generic)))
          (else
              (case-gen-generic)))))
