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

;;------------------------------------------------------------------------------
;; PUBLIC

(define (strat-get-options)
  strat-options)

;;------------------------------------------------------------------------------
;; PRIVATE

;; Options specific to this strat
(define strat-options `(
  (--max-full
    ""
    ,(lambda (args) (set! MAX_NB_VERSIONS (string->number (cadr args)))
                    (set! args (cdr args))
                    args))))

;;------------------------------------------------------------------------------

(define MAX_NB_VERSIONS 5)

(define lco-versions (make-table test: eq?))

(define (version-key ctx)
  (list (ctx-stack ctx)
        (ctx-fs ctx)
        (ctx-ffs ctx)
        (map (lambda (ident) (cons (car ident) (identifier-sslots (cdr ident))))
             (ctx-env ctx))))

(define (get-version lco ctx)
  (let* ((versions (table-ref lco-versions lco #f))
         (key (version-key ctx)))
    (and versions
         (table-ref versions key #f))))

(define (get-nb-versions lco)
  (let ((versions (table-ref lco-versions lco #f)))
    (if versions
        (table-length versions)
        0)))

(define (put-version lco ctx version)
  (let ((versions (table-ref lco-versions lco #f))
        (key (version-key ctx))
        (k (cons version ctx)))
    (if (not versions)
        (let ((vers (make-table test: equal?)))
          (table-set! lco-versions lco vers)
          (set! versions vers)))
    (table-set! versions key k)
    k))

(define lco-generic (make-table test: eq?))

(define (get-curr-generic lco)
  (table-ref lco-generic lco #f))

(define (set-curr-generic! lco ctx)
  (table-set! lco-generic lco ctx))

(define (compute-gen-type types)

  (define (compute t1 t2)
    (cond ;; one is unk -> unk
          ((ctx-type-unk? t1) t1)
          ((ctx-type-unk? t2) t2)
          ;;
          ((and (ctx-type-cst? t1)
                (ctx-type-cst? t2))
             (if (equal? t1 t2)
                 t1
                 (compute (ctx-type-nocst t1)
                          (ctx-type-nocst t2))))
          ;;
          ((ctx-type-cst? t1)
             (compute (ctx-type-nocst t1) t2))
          ;;
          ((ctx-type-cst? t2)
             (compute t1 (ctx-type-nocst t2)))
          ;; type type
          (else
             (if (equal? t1 t2)
                 t1
                 (make-ctx-tunk)))))

  (let loop ((gen (car types))
             (types (cdr types)))
    (if (null? types)
        gen
        (loop (compute gen (car types))
              (cdr types)))))

(define (compute-gen-stack . stacks)
  (if (null? (car stacks))
      '()
      (cons (compute-gen-type (map car stacks))
            (apply compute-gen-stack (map cdr stacks)))))

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
              (let ((nctx (ctx-set-type ctx idx new-type #f)))
                (loop (+ idx 1) (cdr new-stack) (cdr old-stack) nctx)))))))

;; CAS_1
(define (case-use-version lco ctx version)
  (list version ctx (lambda (r) r)))

;; CAS_2
(define (case-gen-version lco ctx)
  (let ((k (put-version lco ctx #f)))
    (list #f ctx (lambda (label) (set-car! k label)))))

;; CAS_3&4
(define (case-generic-* lco ctx stacks)

  (define (return-changed nctx)
    (let ((callback (lambda (label-merge label-version)
                      (put-version lco nctx label-version))))
      (list #f nctx callback)))

  (define (return-unchanged nctx)
    (let ((callback (lambda (label-version)
                      (put-version lco nctx label-version))))
      (list #f nctx callback)))

  (let* ((gen-stack (apply compute-gen-stack stacks))
         (nctx
           ;; TODO: improve perfs: return a bool changed? from compute-gen-stack
           (if (equal? gen-stack (ctx-stack ctx))
               ctx
               (change-ctx ctx gen-stack))))


    ;; 1- on set le ctx generique, pour lesp rochaines generic
    ;; 2- on génère et ajoute la version
    (set-curr-generic! lco nctx)
    (if (eq? ctx nctx)
        (return-unchanged nctx)
        (return-changed   nctx))))

;; CAS_3
(define (case-generic-all lco ctx)
  (let* ((versions (table-ref lco-versions lco))
         (stacks (cons (ctx-stack ctx)
                       (map caar (table->list versions)))))
    (case-generic-* lco ctx stacks)))

;; CAS_4
(define (case-generic-next lco ctx)
  (let* ((gctx (get-curr-generic lco))
         (stacks (list (ctx-stack ctx)
                       (ctx-stack gctx))))
    (case-generic-* lco ctx stacks)))

;; TODO !! Attention aux appels à lazy-code-versions dans ast.scm par ex. qui doivent matcher

;; TODO: placeholder for --stats
(define (lazy-code-nb-real-versions lco)
  (let ((versions (table-ref lco-versions lco #f)))
    (if versions
        (table-length versions)
        0)))

;;; TODO: placeholder for --stats
(define (lazy-code-versions-ctx lco)
  (let ((versions (table-ref lco-versions lco #f)))
    (if versions
        (map cddr (table->list versions))
        '())))

(define (strat-label-from-stack lco stack)
  (let* ((versions (table-ref lco-versions lco #f))
         (r (and versions (table-ref versions stack #f))))
    (if r
        (error "U")
        #f)))

(define (strat-get-version lco ctx)

  (let* ((version (get-version lco ctx)))

    (cond (version
            ;(pp "c1")
            (case-use-version lco (cdr version) (car version)))
          ((< (get-nb-versions lco) MAX_NB_VERSIONS)
            ;(pp "c2")
            (case-gen-version lco ctx))
          ((not (get-curr-generic lco))
            ;(pp "c3")
            (case-generic-all lco ctx))
          (else
            ;(pp "c4")
            (case-generic-next lco ctx)))))
