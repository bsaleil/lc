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
  (count (table->list (lazy-code-versions lazy-code)) cddr))

(define (strat-get-options)
  strat-options)

;;------------------------------------------------------------------------------
;; PRIVATE

(define opt-max-versions #f) ;; Limit of number of versions (#f=no limit, 0=only generic, ...)

;; Options specific to this strat
(define strat-options `(
  (--max-versions
    "Set a limit on the number of versions of lazy code objects"
    ,(lambda (args) (set! opt-max-versions (string->number (cadr args)))
                    (set! args (cdr args))
                    args))
  (--disable-regalloc-vers
    "Do not use register allocation information to specialize generated code"
    ,(lambda (args) (set! get-version nregalloc-get-version)
                    (set! put-version nregalloc-put-version)
                    args))
  (--max-nb-fam-var
    ""
    ,(lambda (args) (set! MAX_NB_FAM_VAR (string->number (cadr args)))
                    (cdr args)))

  (--max-nb-lvl-var
    ""
    ,(lambda (args) (set! MAX_NB_LVL_VAR (string->number (cadr args)))
                    (cdr args)))))

(define (lazy-code-generic lco)
  (table-ref lco_generic lco #f))

(define (lazy-code-generic-set! lco ctx version)
  (table-set! lco_generic lco (cons ctx version)))

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

;;------------------------------------------------------------------------------
;; !regalloc versioning

(define (version-key ctx)
  (list (ctx-stack ctx)
        (ctx-fs ctx)
        (ctx-ffs ctx)
        (map (lambda (ident) (cons (car ident) (identifier-sslots (cdr ident))))
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
;;------------------------------------------------------------------------------

(define MAX_NB_FAM_VAR 1)
(define MAX_NB_LVL_VAR 1)

(define (parent-type type)
  (cond ((ctx-type-unk? type) type)
        ((not (ctx-type-cst? type)) (make-ctx-tunk))
        ;; cst
        (else (ctx-type-nocst type))))

(define (get-nb-fam-var type types)
  (if (ctx-type-cst? type)
      (let loop ((types types))
        (if (null? types)
            0
            (if (and (ctx-type-cst? (car types))
                     (ctx-type-teq? type (car types))
                     (not (equal? (ctx-type-cst type) (ctx-type-cst (car types)))))
                (+ 1 (loop (cdr types)))
                (loop (cdr types)))))

      0))

(define (get-nb-lvl-var type types)
  (cond ((ctx-type-unk? type) 0)
        ((not (ctx-type-cst? type))
           (count types (lambda (t) (and (not (ctx-type-teq? t type))
                                         (not (ctx-type-cst? t))))))
        (else
           (count types (lambda (t) (and (not (ctx-type-teq? t type))
                                         (ctx-type-cst? t)))))))

(define (ctx-steps ctx versions-ctx)
  (let ((vstacks (map ctx-stack versions-ctx)))
    (let loop ((ctx ctx))
      (let ((nctx (ctx-step ctx vstacks)))
        (if (eq? ctx nctx)
            ctx
            (loop nctx))))))

(define (change-ctx ctx types)
  (let loop ((idx 0)
             (new-stack types)
             (old-stack (ctx-stack ctx))
             (ctx ctx))
    (if (null? new-stack)
        ctx
        (let ((new-type (car new-stack))
              (old-type (car old-stack)))
          (if (and (ctx-type-cst? old-type)
                   (not (ctx-type-cst? new-type)))
              (let* ((get-fun
                       (if (ctx-type-flo? new-type)
                           ctx-get-free-freg
                           ctx-get-free-reg))
                     (moves/reg/ctx (get-fun #f ctx #f 0))
                     (nctx (caddr moves/reg/ctx))
                     (nctx (ctx-set-type nctx idx new-type #f))
                     (nctx (ctx-set-loc nctx (stack-idx-to-slot ctx idx) (cadr moves/reg/ctx))))
                (loop (+ idx 1) (cdr new-stack) (cdr old-stack) nctx))
              (loop (+ idx 1) (cdr new-stack) (cdr old-stack) ctx))))))

(define (ctx-step ctx vstacks)
  (let* ((changed #f)
         (lvl-var #f)
         (result
           (let loop ((types (ctx-stack ctx)) (i 0))
             (if (null? types)
                 '()
                 (let ((ident (ctx-ident-at ctx i))
                       (nb-fam-var (get-nb-fam-var (car types) (map (lambda (el) (list-ref el i)) vstacks)))
                       (nb-lvl-var (get-nb-lvl-var (car types) (map (lambda (el) (list-ref el i)) vstacks))))
                   (cond ;; 0
                         ((and ident (identifier-cst (cdr ident)))
                            (cons (car types) (loop (cdr types) (+ i 1))))
                         ;; 1 check for nb fam var
                         ((>= nb-fam-var MAX_NB_FAM_VAR)
                            (set! changed #t)
                            (cons (parent-type (car types)) (loop (cdr types) (+ i 1))))
                         ;; 2 check for nb lvl var
                         ((>= nb-lvl-var MAX_NB_LVL_VAR)
                            (set! changed #t)
                            (set! lvl-var #t)
                            (cons (parent-type (car types)) (loop (cdr types) (+ i 1))))
                         ;; 3 keep this type
                         (else (cons (car types) (loop (cdr types) (+ i 1))))))))))

    (if changed
        (change-ctx ctx result)
        ctx)))


(define (limit-reached? lco)
  (and opt-max-versions
       (let ((nb-versions (lazy-code-nb-real-versions lco)))
         (>= nb-versions opt-max-versions))))

(define (strat-get-version lco ctx)

  (define (case-use-version ctx dst-ctx version)
    (if dst-ctx
        (let ((k (put-version lco ctx #f #f)))
          (list version dst-ctx (lambda (label) (set-car! k label))))
        (list version ctx (lambda (r) r))))

  (define (case-gen-no-merge ctx)
    (let ((k (put-version lco ctx #f #t)))
      (list #f ctx (lambda (label) (set-car! k label)))))

  (define (case-gen-and-merge ctx dst-ctx)
    (let* ((k1 (put-version lco ctx #f #f))
           (k2 (put-version lco dst-ctx #f #t))
           (callback (lambda (label-merge label-version)
                       (set-car! k1 label-merge)
                       (set-car! k2 label-version))))
      (list #f dst-ctx callback)))

  (define (compute-new-ctx lco ctx)
    (let* ((v (lazy-code-versions lco))
           ;; only real versions
           (versions-ctx (foldr (lambda (el r)
                                  (if (cddr el)
                                      (cons (car el) r)
                                      r))
                                '()
                                (table->list v))))
           ;; all versions
           ;(versions-ctx (map car (table->list v))))
      (ctx-steps ctx versions-ctx)))

  (let* ((r (get-version lco ctx))
         (dst-ctx (and r (car r)))
         (version (and r (cdr r))))

    (if version
        ;; c1, this version exists (real or not)
        (case-use-version ctx dst-ctx version)
        ;; c2, this version does not exist
        (let ((nctx (compute-new-ctx lco ctx)))
          ;;
          (cond ;; c2.1, we can generate a version for this context
                ((eq? ctx nctx)
                   (case-gen-no-merge ctx))
                ;; c2.2, there is a version associated to the new ctx, use it
                ((get-version lco nctx) => (lambda (r)
                   (let ((dst-ctx (car r))
                         (version (cdr r)))
                     (if dst-ctx
                         ;;
                         (let ((k (put-version lco ctx #f #f)))
                           (list version dst-ctx (lambda (label) (set-car! k label))))
                         ;;
                         (let ((k (put-version lco ctx #f #f)))
                           (list version nctx (lambda (label) (set-car! k label))))))))
                ;; c2.3, generate the version associated to the new ctx
                (else
                   (case-gen-and-merge ctx nctx)))))))
