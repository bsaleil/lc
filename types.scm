

(define lco_versions (make-table test: eq?))
(define lco_generic  (make-table test: eq?))

;; TODO: these functions are used in other files
;; they must *not* be use in other files when meta-strat is implemented
(define (lazy-code-generic lco)
  (table-ref lco_generic lco #f))
(define (lazy-code-generic-set! lco ctx version)
  (table-set! lco_generic lco (cons ctx version)))

(define (lazy-code-versions lco)
  (let ((versions (table-ref lco_versions lco #f)))
    (or versions
        (let ((versions (make-table)))
          (table-set! lco_versions lco versions)
          versions))))

(define (get-version lazy-code ctx)
  (let* ((key ctx)
         (versions (lazy-code-versions lazy-code))
         (version  (table-ref versions key #f)))
    (and version (car version))))

(define (put-version lazy-code ctx version real-version?)
  (let ((key ctx)
        (versions (lazy-code-versions lazy-code)))
    (table-set! versions key (cons version real-version?))))

;;------------------------------------------------------------------------------

;;;; return version, context, callback
;(define (meta-get-version lco ctx)
;  (let ((versions (lazy-code-versions lco)))
;    (let ((version (get-version lco ctx)))
;      (if version
;          ;; A version exists for this ctx
;          (list version ctx #f)
;          ;; No version for this ctx
;          (if (>= (table-length versions) MAX_VERSIONS)
;              ;; Limit is reached
;              (let ((generic (lco-get-generic lco)))
;                (if generic
;                    ;; The generic version already exists
;                    (list generic ctx #f)
;                    ;; The generic version does not exist
;                    (let ((gctx (get-generic-ctx ctx)))
;                      (list #f gctx (lambda (version) (new-version-callback version #t))))))
;              ;; Limit is not reached
;              (list #f ctx (lambda (version) (new-version-callback version #f))))))))


;(define (lazy-code-version versions ctx)
;  (table-ref versions ctx #f))
;
;(define (new-version-callback lco version generic?)
;  ()
;
;(define (get-version lco ctx)
;  (let* ((versions (lazy-code-versions lco))
;         (version  (lazy-code-version  versions ctx)))
;    (if version
;        ;; A version exists for this ctx
;        (list version #f #f)
;        ;; No version for this ctx
;        (if (>= (table-length versions) MAX_VERSIONS)
;            ;; Limit is reached
;            (error "NYI")
;            ;; Limit is not reached
;            (list #f ctx (lambda (version) (new-version-callback version #f)))))))

;(define MAX_VERSIONS 5)
;(define lco_versions (make-table test:eq?))
;(define lco_generics (make-table test:eq?))
;
;(define (lco-get-generic lco)
;  (table-ref lco_generics lco #f))
;
;(define (new-version-callback lco ctx version generic?)
;  (if generic?
;      (table-set! lco_generics lco version)
;      (let ((versions (table-ref lco_versions lco)))
;        (table-set! versions ctx version))))
;
;;; version, context, continuation
;(define (get-version lco ctx)
;  (let ((versions (lco-get-versions lco)))
;    (let ((version (lco-get-version versions ctx)))
;      (if version
;          ;; A version exists for this ctx
;          (list version ctx #f)
;          ;; No version for this ctx
;          (if (>= (table-length versions) MAX_VERSIONS)
;              ;; Limit is reached
;              (let ((generic (lco-get-generic lco)))
;                (if generic
;                    ;; The generic version already exists
;                    (list generic ctx #f)
;                    ;; The generic version does not exist
;                    (let ((gctx (get-generic-ctx ctx)))
;                      (list #f gctx (lambda (version) (new-version-callback version #t))))))
;              ;; Limit is not reached
;              (list #f ctx (lambda (version) (new-version-callback version #f))))))))
