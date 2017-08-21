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

(define (put-version lazy-code ctx version real-version?)
  (let ((key (if (lazy-code-entry? lazy-code)
                 ctx;(ctx-stack ctx)
                 ctx))
        (versions (lazy-code-versions lazy-code)))
    (table-set! versions key (cons version real-version?))))

(define (limit-reached? lco)
  (and opt-max-versions
       (let ((nb-versions (lazy-code-nb-real-versions lco)))
         (>= nb-versions opt-max-versions))))

(define (strat-get-version lco ctx)

  ;; CASE 1: a version exists for this ctx, use it
  (define (case-use-version version)
    (list version ctx (lambda (r) r)))

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

  (define (case-ctx-with-distance lco nctx)
      (let ((callback (lambda (label-merge label-version)
                        (put-version lco ctx label-merge #f))))
        (list #f nctx callback)))

  (let ((version (get-version lco ctx)))
    (cond (version
            (case-use-version version))
          ((not (limit-reached? lco))
            (case-heuristic-1 lco ctx))
          ((ctx-heuristic-2 lco ctx) => (lambda (r)
            r))
          ((lazy-code-generic lco) => (lambda (generic)
            (case-use-generic generic)))
          (else
            (case-gen-generic)))))

;; CASE 2: no version for this ctx, and limit is not reached
;; then generate a new version
(define (case-gen-version lco ctx)
  (list #f ctx (lambda (label) (put-version lco ctx label #t))))

;; TODO
(define (case-gen-other-version lco ctx nctx)
  (list #f nctx  (lambda (label-merge label-version)
                   (put-version lco ctx label-merge #f)
                   (put-version lco nctx label-version #t))))

;; TODO: Heuristic3: (global?):
;;       Si on a plus d'entrée dans la table de pt d'entrée, on peut chercher le ctx le plus proche et l'utiliser

;; TODO: H1, H2
;; Suite: H3, optimiser la compilation de strat4

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; Heuristic 1
;; La limite n'est pas atteinte.
;; On regarde si il existe un contexte qui a exactement les même types, mais avec des cst différentes.
;; Si c'est le cas, on réduit les types de ces slots au type de base, et on génère une version pour ce ctx

(define (get-indexes ctx ctx-version)
  (define (get stack stack-version idx indexes)
    (cond ((null? stack)
             indexes)
          ((not (ctx-type-teq? (car stack) (car stack-version)))
             #f)
          ((and (ctx-type-cst? (car stack))
                (ctx-type-cst? (car stack-version))
                (not (equal? (ctx-type-cst (car stack))
                             (ctx-type-cst (car stack-version)))))
             (get (cdr stack) (cdr stack-version) (+ idx 1) (cons idx indexes)))
          (else
             (get (cdr stack) (cdr stack-version) (+ idx 1) indexes))))
  (get (ctx-stack ctx) (ctx-stack ctx-version) 0 '()))

(define (remove-csts ctx indexes)
  (if (null? indexes)
      ctx
      (let* ((idx (car indexes))
             (type (ctx-get-type ctx idx))
             (r (if (ctx-type-flo? type)
                    (ctx-get-free-freg #f ctx #f 0)
                    (ctx-get-free-reg  #f ctx #f 0))) ;; moves/reg/ctx
             (reg (cadr r))
             (ctx (caddr r))
             ;;
             (ctx (ctx-set-type ctx idx (ctx-type-nocst type) #t))
             (ctx (ctx-set-loc  ctx (stack-idx-to-slot ctx idx) reg)))
        (remove-csts ctx (cdr indexes)))))

(define (case-heuristic-1 lco ctx)

  (let ((r
          (foldr (lambda (el r)
                   (let ((indexes (get-indexes ctx (car el))))
                     (if (and indexes
                              (or (not (cdr r))
                                  (< (length indexes) (length (cdr r)))))
                         (cons (car el) indexes)
                         r)))
                 (cons #f #f)
                 (table->list (lazy-code-versions lco)))))

    (if (car r)
        (let ((nctx (remove-csts (car r) (cdr r))))
          (case-gen-other-version lco ctx nctx))
        (case-gen-version lco ctx))))

;; -----------------------------------------------------------------------------
;; -----------------------------------------------------------------------------
;; Heuristic 2
;; Quand la limite est atteinte, on calcule la distance du ctx à toutes les
;; versions existantes. Si on a au moins une distance finie, on génère un merge
;; vers la plus proche.
;; Calcul de la distance:
;; -> nombre de types identiques hors cst

;; TODO: heuristic 1 & 2: check only real versions ?

(define (get-types-dist type1 type2)

  (cond ;; CAS1
        ((and (ctx-type-cst? type1)
              (ctx-type-cst? type2)
              (equal? (ctx-type-cst type1)
                      (ctx-type-cst type2)))
           0)
        ;; CAS2
        ((ctx-type-cst? type2) #f)
        ;; CAS3
        ((and (ctx-type-cst? type1)
              (ctx-type-teq? type1 type2))
           1)
        ;; CAS4
        ((and (ctx-type-cst? type1)
              (ctx-type-unk? type2))
           2)
        ;; CAS5
        ((ctx-type-cst? type1) #f)
        ;; CAS6 / CAS9
        ((ctx-type-teq? type1 type2) 0)
        ;; CAS10
        ((ctx-type-unk? type1) #f)
        ;; CAS7
        ((ctx-type-unk? type2) 1)
        ;; CAS8
        (else #f)))


  ;; CAS1: 2 cst identiques -> 0
  ;; CAS2: type2 cst -> INF
  ;; CAS3: type1 cst, type2 meme type -> 1
  ;; CAS4: type1 cst, type2 unk -> 2
  ;; CAS5: type1 cst, type2 diff type -> INF


  ;; CAS6: type1 type, type2 meme type -> 0
  ;; CAS7: type1 type, type2 unk -> 1
  ;; CAS8: type1 type, type2 autre -> INF

  ;; CAS9: type1 unk, type2 unk -> 0
  ;; CAS10: type1 unk, autre -> INF


(define (get-distance ctx ctx-version)
  (define (get stack stack-version dist)
    (if (null? stack)
        dist
        (let ((d (get-types-dist (car stack) (car stack-version))))
          (if (not d)
              #f
              (get (cdr stack) (cdr stack-version) (if dist (+ dist d) d))))))
  (get (ctx-stack ctx) (ctx-stack ctx-version) #f))

;; TODO
(define (case-use-h2 lco version ctx vctx)
  (let ((callback (lambda (label-merge) (put-version lco ctx label-merge #f))))
    (list version vctx callback)))

(define (ctx-heuristic-2 lco ctx)
  (let ((r (foldr (lambda (el r)
                    (let ((distance (get-distance ctx (car el))))
                      (if (or (not distance) ;; infinite distance
                              (and (car r) (>= distance (caddr r)))) ;; > distance
                          r
                          (list (car el) (cadr el) distance))))
                  (list #f #f #f) ;; ctx label distance (TODO move distance at pos 2 for perf?)
                  (table->list (lazy-code-versions lco)))))
    (if (car r)
        (case-use-h2 lco (cadr r) ctx (car r))
        #f)))
