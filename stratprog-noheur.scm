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

;(define (get-version lco ctx)
;  (let* ((versions (table-ref lco-versions lco #f))
;         (key (version-key ctx)))
;    (and versions
;         (table-ref versions key #f))))

(define (get-nb-versions lco)
  (let ((versions (table-ref lco-versions lco #f)))
    (if versions
        (table-length versions)
        0)))

;(define (put-version lco ctx version)
;  (let ((versions (table-ref lco-versions lco #f))
;        (key (version-key ctx))
;        (k (cons version ctx)))
;    (if (not versions)
;        (let ((vers (make-table test: equal?)))
;          (table-set! lco-versions lco vers)
;          (set! versions vers)))
;    (table-set! versions key k)
;    k))

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
          (cond ;; cst -> !cst
                ((and (ctx-type-cst? old-type)
                      (not (ctx-type-cst? new-type)))
                   (let* ((get-fun
                            (if (ctx-type-flo? new-type)
                                ctx-get-free-freg
                                ctx-get-free-reg))
                          (moves/reg/ctx (get-fun #f ctx #f 0))
                          (nctx (caddr moves/reg/ctx))
                          (nctx (ctx-set-type nctx idx new-type #f))
                          (nctx (ctx-set-loc nctx (stack-idx-to-slot ctx idx) (cadr moves/reg/ctx))))
                     (loop (+ idx 1) (cdr new-stack) (cdr old-stack) nctx)))
                ;; flo -> !flo
                ((and (ctx-type-flo? old-type)
                      (not (ctx-type-flo? new-type)))
                  (let* ((moves/reg/ctx (ctx-get-free-reg #f ctx #f 0))
                         (nctx (caddr moves/reg/ctx))
                         (nctx (ctx-set-type nctx idx new-type #f))
                         (nctx (ctx-set-loc nctx (stack-idx-to-slot ctx idx) (cadr moves/reg/ctx))))
                    (loop (+ idx 1) (cdr new-stack) (cdr old-stack) nctx)))
                ;;
                (else
                  (let ((nctx (ctx-set-type ctx idx new-type #f)))
                    (loop (+ idx 1) (cdr new-stack) (cdr old-stack) nctx))))))))

;; CAS_1
(define (case-use-version lco ctx version)
  (list version ctx (lambda (r) r)))

;; CAS_2
(define (case-gen-version-force lco ctx)
  (let ((k (put-version lco ctx #f)))
    (list #f ctx (lambda (label) (set-car! k label)))))

;; CAS_3
(define (case-gen-version lco ctx)
  ;; TODO: cas des heuristiques

  (define (return-gen)
    (let ((k (put-version lco ctx #f)))
      (list #f ctx (lambda (label) (set-car! k label)))))

  ;; -> si on a une variable avec 1 de diff plusieurs fois, on upgrade
  (define (heur1 stack stacks)

    (define (num-cst? type)
      (and (ctx-type-cst? type)
           (or (ctx-type-int? type)
               (ctx-type-flo? type))))

    (let loop ((stack stack)
               (stacks stacks))
      (cond ((null? stack)
               '()) ;; TODO changed? switch
            ((num-cst? (car stack))
               ;;
               (let* ((cst (ctx-type-cst (car stack)))
                      (types (map car stacks))
                      (type-up?
                        (let loop2 ((types types))
                          (cond ((null? types)
                                   #f)
                                ((and (num-cst? (car types))
                                      (or (= (ctx-type-cst (car types)) (+ cst 1))
                                          (= (ctx-type-cst (car types)) (- cst 1))
                                          (= (ctx-type-cst (car types)) (+ cst 2))
                                          (= (ctx-type-cst (car types)) (- cst 2))))
                                   #t)
                                (else
                                   (loop2 (cdr types)))))))
                 (cons
                   (if type-up? (ctx-type-nocst (car stack)) (car stack))
                   (loop (cdr stack) (map cdr stacks)))))
            (else
               (cons (car stack)
                     (loop (cdr stack) (map cdr stacks)))))))

  (let* ((versions (table-ref lco-versions lco #f))
         (stacks (if versions
                     (map caar (table->list versions))
                     '())))

    (let ((stack  (ctx-stack ctx))
          (nstack (heur1 (ctx-stack ctx) stacks)))
      (if (equal? stack nstack)
          (return-gen)
          (let ((nctx (change-ctx ctx nstack)))
            (let* ((k1 (put-version lco  ctx #f))
                   (k2 (put-version lco nctx #f))
                   (callback (lambda (label-merge label-version)
                               (set-car! k1 label-merge)
                               (set-car! k2 label-version))))
              (list #f nctx callback)))))))

;; CAS_4&5
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
        (map caar (map table->list (map cdr (table->list versions))))
        '())))

(define (strat-label-from-stack lco stack)
  (let* ((versions (table-ref lco-versions lco #f))
         (r (and versions (table-ref versions stack #f))))
    (if r
        (error "U")
        #f)))

(define (get-version-table lco ctx)
  (let* ((versions (table-ref lco-versions lco #f))
         (key (version-key ctx)))
    (and versions
         (table-ref versions key #f))))

(define (get-version-from-verstable verstable ctx)
  (and verstable
       (table-ref verstable ctx #f)))

(define (put-version lco ctx version)

  (define (get-verstable)
    (let ((verstable (table-ref lco-versions lco #f)))
      (if (not verstable)
          (let ((verstable (make-table test: equal?)))
            (table-set! lco-versions lco verstable)
            verstable)
          verstable)))

  (define (get-versions-table verstable)
    (let* ((key (version-key ctx))
           (versions-table (table-ref verstable key #f)))
      (if (not versions-table)
          (let ((versions-table (make-table test: equal?)))
            (table-set! verstable key versions-table)
            versions-table)
          versions-table)))

  (let* ((verstable (get-verstable))
         (versions-table (get-versions-table verstable))
         (k (cons version #f)))

    (table-set! versions-table ctx k)
    k))


(define TOT 0)
(define (strat-get-version lco ctx)
  (let ((r (##exec-stats (lambda () (strat-get-version-h lco ctx)))))
    (set! TOT (+ TOT (cdr (assoc 'user-time r))))
    (cdr (assoc 'result r))))

(define (strat-get-version-h lco ctx)

  (let* ((verstable (get-version-table lco ctx))
         (version   (get-version-from-verstable verstable ctx)))

    (cond ;; Case 1 - we have a verstable AND the version exists
          (version
            (case-use-version lco ctx (car version)))
          ;; Case 2 - we have a verstable but the version does not exist
          (verstable
            (case-gen-version-force lco ctx))
          ;; Case 3 - no verstable, limit not reached
          ((< (get-nb-versions lco) MAX_NB_VERSIONS)
            (case-gen-version-force lco ctx))
          ;; Case 4 - no verstable, limit reached and no current generic
          ((not (get-curr-generic lco))
            (case-generic-all lco ctx))
          ;; Case 5 - no verstable, limit reached and we have a current generic
          (else
            (case-generic-next lco ctx)))))
