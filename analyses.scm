
;;-----------------------------------------------------------------------------
;; Free variables
;;-----------------------------------------------------------------------------

;; Return all free vars used by ast knowing env 'clo-env'
(define (free-vars body params enc-ids)
  ;; TODO: memoize result with given input to avoid multiple calls (analyses.scm & ast.scm)
  (cond
    ;; Keyword
    ((symbol? body) '())
    ;; Atom node
    ((atom-node? body)
       (let ((val (atom-node-val body)))
         (if (and (symbol? val)
                  (member val enc-ids)
                  (not (member val params)))
             (list val)
             '())))
    ;; Pair
    ((pair? body)
      (let ((op (car body)))
        (cond ;; If
              ((eq? op 'if) (set-union (free-vars (cadr body) params enc-ids)                 ; cond
                                       (set-union (free-vars  (caddr body) params enc-ids)    ; then
                                                  (free-vars (cadddr body) params enc-ids)))) ; else
              ;; Quote
              ((eq? op 'quote) '())
              ;; Lambda
              ((eq? op 'lambda) (free-vars (caddr body)
                                           (if (list? (cadr body))
                                              (append (cadr body) params)
                                              (cons (cadr body) params))
                                           enc-ids))
              ;; Call
              (else (free-vars-l body params enc-ids)))))
    ;;
    (else (error "Unexpected expr (free-vars)"))))

;;-----------------------------------------------------------------------------
;; Globals passes
;;-----------------------------------------------------------------------------

(define (analyses-find-global-types! expr)

  (define (find-types! expr)
    (if (null? expr)
       #t
       (let ((el (car expr)))
         (if (and (pair? el)
                  (eq? (car el) 'define))
             (let* ((id (if (pair? (cadr el))
                            (caadr el)
                            (cadr el)))
                    (global (asc-globals-get id))
                    (stype (get-global-type el)))
               (if (and global
                        (not (ctx-type-teq? stype (global-stype global))))
                   (global-stype-set! global #f)
                   (asc-globals-add id stype))))
         (find-types! (cdr expr)))))

   (define (remove-mutables! expr)
     (if (or (not (pair? expr))
             (eq? (car expr) 'quote))
         #f
         (let ((op (car expr)))
           (if (eq? op 'set!)
               (let ((r (asc-globals-get (cadr expr))))
                 (if r
                     (global-stype-set! r (make-ctx-tunk))))
               (begin
                 (remove-mutables! (car expr))
                 (remove-mutables! (cdr expr)))))))

  (define (get-global-type g)
    (define val (caddr g))
    ;; TODO WIP: add cst vals here
    (cond ((symbol? (cadr g))
              (cond ((symbol?  val) (make-ctx-tunk))
                    ((integer? val) (make-ctx-tint val))
                    ((flonum?  val) (make-ctx-tflo val))
                    ((char?    val) (make-ctx-tcha val))
                    ((string?  val) (make-ctx-tstr val))
                    ((boolean? val) (make-ctx-tboo val))
                    ((eq? (caaddr g) 'lambda) (make-ctx-tclo #t val))
                    ((pair? (caddr g)) (make-ctx-tunk))
                    (else (error "NYI"))))
          ((pair? (cadr g)) (make-ctx-tclo))
          (else (error "NYI"))))

  (find-types! expr)
  (remove-mutables! expr))



;;-----------------------------------------------------------------------------
;; Liveness
;;-----------------------------------------------------------------------------

;; TODO: values with same name as existing var -> alpha conversion

(define (ast-use node locals)
  (let ((op (car node)))
    (cond ((atom-node? node)
             (let ((val (atom-node-val node)))
               (if (and (symbol? val)
                        (member val locals))
                   (list val)
                   '())))
          ((eq? op 'lambda)
             (free-vars (caddr node) (cadr node) locals))
          ((or (eq? op 'begin)
               (eq? op 'let)
               (eq? op 'define))
            '())
          (else '()))))

(define (ast-def node)
  (let ((op (car node)))
    (cond ((atom-node? node) '())
          ((or (eq? op 'begin)
               (eq? op 'lambda)
            '()))
          ((eq? op 'let)
             (map car (cadr node)))
          ((eq? op 'define)  (list (cadr node)))
          (else (error "NYI")))))

(define liveness-in  (make-table test: eq?))
(define liveness-out (make-table test: eq?))

(define (liveness-in-get node)
  (table-ref liveness-in node '()))
(define (liveness-in-set! node in)
  (let ((r (table-ref liveness-in node #f)))
    (if r
        (error "Internal error (liveness-in-set!). We are not supposed to compute two times liveness for a node.")))
  (table-set! liveness-in node in))

(define (liveness-out-get node)
  (table-ref liveness-out node '()))
(define (liveness-out-set! node out)
  (let ((r (table-ref liveness-out node #f)))
    (if r
        (error "Internal error (liveness-in-set!). We are not supposed to compute two times liveness for a node.")))
  (table-set! liveness-out node out))

(define (liveness-prog ast)
  (let loop ((exprs (reverse ast))
             (succs (atom-node-make #f)))
    (if (not (null? exprs))
        (let ((first (liveness (car exprs) '() succs)))
          (loop (cdr exprs) (list first))))))

(define (liveness-fun ast)
  (liveness (caddr ast) (cadr ast) (atom-node-make #f)))

(define (liveness ast locals succs)

  ;; Apply dataflow equations of liveness analysis
  (define (compute-in-out ast locals succs)
    (let* ((out
             (foldr (lambda (succ live)
                      (set-union
                        (liveness-in-get succ)
                        live))
                    '()
                    succs))
           (in
             (set-union
               (ast-use ast locals)
               (set-sub out (ast-def ast) '()))))
      (liveness-in-set! ast in)
      (liveness-out-set! ast out)))

  (define (liveness-lst exprs locals succs)
    (let loop ((exprs (reverse exprs))
               (succs succs))
      (if (not (null? exprs))
          (let ((first (liveness (car exprs) locals succs)))
            (loop (cdr exprs) (list first))))))

  (cond ;; ATOM node
        ((atom-node? ast)
           (compute-in-out ast locals succs)
           ast)
        ((eq? (car ast) 'lambda)
           (compute-in-out ast locals succs)
           ast)
        ((eq? (car ast) 'let)
           (let ((bodies (cddr ast)))
             (if (not (= (length bodies) 1))
                 (error "Internal error"))

             (let ((body (car bodies))
                   (bindings (map cadr (cadr ast))))
               (compute-in-out ast locals succs)
               (let* ((ids (map car (cadr ast)))
                      (firstb (liveness body (append ids locals) (list ast))))
                 (liveness-lst bindings locals (list firstb))))))
        ((eq? (car ast) 'letrec)
           (error "NYI"))
        ((or (eq? (car ast) 'define)
             (eq? (car ast) 'set!))
           (compute-in-out ast locals succs)
           (liveness (caddr ast) locals (list ast)))
        ;; IF node
        ((eq? (car ast) 'if)
           (let* ((firstt (liveness (caddr ast) locals succs))
                  (firstf (liveness (cadddr ast) locals succs))
                  (firstc (liveness (cadr ast) locals (list firstt firstf))))
             (compute-in-out ast locals succs)
             ast))
        ;; Others
        ;; Begin
        (else
           (let* ((op (car ast))
                  (lst (if (eq? op 'begin)
                           (cdr ast)
                           ast)))
             (compute-in-out ast locals succs)
             (let loop ((exprs (reverse lst))
                        (succs (list ast)))
               (cond ((null? exprs) (error "Internal error"))
                     ((= (length exprs) 1)
                        (liveness (car exprs) locals succs))
                     (else
                       (let ((first (liveness (car exprs) locals succs)))
                         (loop (cdr exprs) (list first))))))))))
