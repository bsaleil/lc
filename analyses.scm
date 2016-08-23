
;;-----------------------------------------------------------------------------
;; Liveness
;;-----------------------------------------------------------------------------

;; TODO: values with same name as existing var

(define (ast-use node)
  (let ((op (car node)))
    (cond ((atom-node? node)
             (let ((val (atom-node-val node)))
               (if (symbol? val)
                   (list val)
                   '())))
          ((or (eq? op 'begin)
               (eq? op 'let)
               (eq? op 'define))
            '())
          (else (error "NYI")))))

(define (ast-def node)
  (let ((op (car node)))
    (cond ((atom-node? node) '())
          ((eq? op 'begin)   '())
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
        (let ((first (liveness (car exprs) succs)))
          (loop (cdr exprs) (list first))))))

(define (liveness ast succs)

  ;; Apply dataflow equations of liveness analysis
  (define (compute-in-out ast succs)
    (let* ((out
             (foldr (lambda (succ live)
                      (set-union
                        (liveness-in-get succ)
                        live))
                    '()
                    succs))
           (in
             (set-union
               (ast-use ast)
               (set-sub out (ast-def ast) '()))))
      (liveness-in-set! ast in)
      (liveness-out-set! ast out)))

  (define (liveness-lst exprs succs)
    (let loop ((exprs (reverse exprs))
               (succs succs))
      (if (not (null? exprs))
          (let ((first (liveness (car exprs) succs)))
            (loop (cdr exprs) (list first))))))

  (cond ;; ATOM node
        ((atom-node? ast)
           (compute-in-out ast succs)
           ast)
        ((eq? (car ast) 'lambda)
           (error "NYI"))
        ((eq? (car ast) 'let)
           (let ((bodies (cddr ast)))
             (if (not (= (length bodies) 1))
                 (error "Internal error"))

             (let ((body (car bodies))
                   (bindings (map cadr (cadr ast))))
               (compute-in-out ast succs)
               (let ((firstb (liveness body (list ast))))
                 (liveness-lst bindings (list firstb))))))
        ((eq? (car ast) 'letrec)
           (error "NYI"))
        ((or (eq? (car ast) 'define)
             (eq? (car ast) 'set!))
           (compute-in-out ast succs)
           (liveness (caddr ast) (list ast)))
        ;; IF node
        ((eq? (car ast) 'if)
           (let* ((firstt (liveness (caddr ast) succs))
                  (firstf (liveness (cadddr ast) succs))
                  (firstc (liveness (cadr ast) (list firstt firstf))))
             (compute-in-out ast succs)
             ast))
        ;; Others
        ;; Begin
        (else
           (let* ((op (car ast))
                  (lst (if (eq? op 'begin)
                           (cdr ast)
                           ast)))
             (compute-in-out ast succs)
             (let loop ((exprs (reverse lst))
                        (succs (list ast)))
               (cond ((null? exprs) (error "Internal error"))
                     ((= (length exprs) 1)
                        (liveness (car exprs) succs))
                     (else
                       (let ((first (liveness (car exprs) succs)))
                         (loop (cdr exprs) (list first))))))))))
