
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
;; Liveness analysis
;;-----------------------------------------------------------------------------

(define (live-out? id ast)
  (member id (table-ref live-out ast '()))) ;; TODO: default '() or (list id) ???

;; in[n] = use[n] U (out[n] - def[n])
;; out[n] = U in[s] with s the successors of n

(define live-in  (make-table test: eq?))
(define live-out (make-table test: eq?))

(define (compute-live-out expr successors)
  (let ((out
          (foldr (lambda (el set)
                   (let ((r (table-ref live-in el #f)))
                     (if r
                         (set-union r set)
                         set)))
                 '()
                 successors)))
    (table-set! live-out expr out)))

(define (compute-live-in expr used #!optional (killed '()))
  (let* ((out (table-ref live-out expr #f))
         (in (if out
                 (set-union used (set-sub out killed '()))
                 used)))
    (table-set! live-in expr in)))

;; TODO: on considère à chaque noeud que in et out des successors sont calculés
(define (liveness-expr expr locals successors)
  (let ((op (car expr)))
    (cond ;; Atom node
          ((atom-node? expr)
             (let ((val (atom-node-val expr)))
               ;; Live out
               (compute-live-out expr successors)
               ;; Live in
               (if (symbol? val)
                   (compute-live-in expr (list val))
                   (compute-live-in expr '()))))
          ;; Begin
          ((eq? op 'begin)
             ;;
             (let ((r (liveness-seq (cdr expr) locals successors)))
               (compute-live-out expr r)
               (compute-live-in expr '())))
          ;; Define
          ((eq? op 'define)
             (let ((val (caddr expr)))
               ;; Val
               (liveness-expr val locals successors)
               ;; Expr
               (compute-live-out expr (list val))
               (compute-live-in  expr '())))
          ;; If
          ((eq? op 'if)
             (let ((bcond (cadr expr))
                   (bthen (caddr expr))
                   (belse (cadddr expr)))
               (liveness-expr bthen locals successors)
               (liveness-expr belse locals successors)
               (liveness-expr bcond locals (list bthen belse))
               ;;
               (compute-live-out expr (list bcond))
               (compute-live-in  expr '())))
          ;; Lambda
          ((eq? op 'lambda)
             (let* ((nids (flatten (cadr expr)))
                    (free-vars (free-vars (caddr expr) nids locals)))
               ;; Body TODO: delay to function call
               (liveness-expr (caddr expr) (set-union free-vars nids) (list (cons 'END 'END)))
               ;;
               (let ((d (cons 'USE free-vars)))
                 ;; Dummy
                 (compute-live-out d successors)
                 (compute-live-in  d free-vars)
                 ;; Curr
                 (compute-live-out expr (list d))
                 (compute-live-in  expr '()))))
          ;; Let
          ((eq? op 'let)
             (let ((ids (map car (cadr expr)))
                   (exprs (map cadr (cadr expr)))
                   (body (caddr expr)))
               ;;
               (liveness-expr body (set-union locals ids) successors)
               (liveness-seq exprs locals (list body))
               ;;
               (compute-live-out expr (list (car exprs)))
               (compute-live-in expr '())))
          ;; Letrec
          ((eq? op 'letrec)
             (let ((ids (map car (cadr expr)))
                   (exprs (map cadr (cadr expr)))
                   (body (caddr expr)))
               ;;
               (liveness-expr body (set-union locals ids) successors)
               (liveness-seq exprs (set-union locals ids) (list body))
               ;;
               (compute-live-out expr (list (car exprs)))
               (compute-live-in expr '())))
          ;; Set!
          ((eq? op 'set!)
             ;; kill
             (let* ((val (caddr expr))
                    (id  (cadr expr))
                    (d   (cons 'KILL id)))
               ;; Dummy ast node to kill identifier
               (compute-live-out d successors)
               (compute-live-in  d '() (list (cadr expr)))
               ;; Val node
               (liveness-expr val locals (list d))
               ;; Curr node
               (compute-live-out expr (list val))
               (compute-live-in expr '())))
          ;; Call
          (else
            (let ((r (liveness-seq expr locals successors)))
              (compute-live-out expr r)
              (compute-live-in expr '()))))))

(define (liveness-seq exprs locals successors)
  (if (null? exprs)
      successors
      (let* ((first (car exprs))
             (r (liveness-seq (cdr exprs) locals successors)))
        (liveness-expr first locals r)
        (list first))))

(define (compute-liveness exp-content)
  (liveness-seq exp-content '() (list (cons 'END 'END))))

;;-----------------------------------------------------------------------------
;; Alpha conversion
;;-----------------------------------------------------------------------------

(define (analyses-a-conversion! exp-content)

  ;; Expr changes TODO
  (define (change-let-syms! bindings symtable)
    (if (null? bindings)
        '()
        (let ((binding (car bindings)))
          (set-car! binding (symtable-get-symbol symtable (car binding)))
          (change-let-syms! (cdr bindings) symtable))))

  (define (change-lambda-syms! args symtable)
    (cond ((null? args) #f)
          ((and (pair? args)
                (not (pair? (cdr args))))
            (set-car! args (symtable-get-symbol symtable (car args)))
            (set-cdr! args (symtable-get-symbol symtable (cdr args))))
          (else
            (set-car! args (symtable-get-symbol symtable (car args)))
            (change-lambda-syms! (cdr args) symtable))))

  ;; Asc table
  (define (symtable-add-bindings table syms)
    (if (null? syms)
        table
        (symtable-add-bindings
            (symtable-add-binding table (car syms))
            (cdr syms))))

  (define (symtable-add-binding table sym)
    (let ((asc (assoc sym table)))
      (if asc
          (cons (cons sym (+ (cdr asc) 1))
                table)
          (cons (cons sym 0)
                table))))

  (define (symtable-get-symbol table sym)
    (let ((asc (assoc sym table)))
      (if asc
          (string->symbol
            (string-append
              (symbol->string sym)
              (number->string (cdr asc))))
          sym)))

  ;; Aconv
  (define (aconv expr symtable)
    (if (not (pair? expr))
        #f
        (let ((op (car expr)))
          (cond ;; Atom node
                ((atom-node? expr)
                   (let ((val (atom-node-val expr)))
                     (if (symbol? val)
                         (atom-node-val-set! expr (symtable-get-symbol symtable val)))))
                ;; Let
                ((eq? op 'let)
                   (let* ((new-ids (map car (cadr expr)))
                          (new-symtable (symtable-add-bindings symtable new-ids)))
                     ;; aconv on let bindings
                     (for-each (lambda (x) (aconv (cadr x) symtable))
                               (cadr expr))
                     ;; Change binding symbols
                     (change-let-syms! (cadr expr) new-symtable)
                     ;; aconv on let body
                     (aconv (caddr expr) new-symtable)))
                ;; Let*
                ((eq? op 'let*)    (error "Internal error."))
                ;; Letrec
                ((eq? op 'letrec)
                   (let* ((new-ids (map car (cadr expr)))
                          (new-symtable (symtable-add-bindings symtable new-ids)))
                     ;; aconv on letrec bindings
                     (for-each (lambda (x) (aconv (cadr x) new-symtable))
                               (cadr expr))
                     ;; Change binding symbols
                     (change-let-syms! (cadr expr) new-symtable)
                     ;; aconv on letrec body
                     (aconv (caddr expr) new-symtable)))
                ;; Lambda
                ((eq? op 'lambda)
                   (let* ((new-ids  (flatten (cadr expr)))
                          (new-symtable (symtable-add-bindings symtable new-ids)))
                     ;; if it's a (lambda arg body) form, change arg
                     ;; else, change lambda args
                     (if (symbol? (cadr expr))
                         (set-car! (cdr expr) (symtable-get-symbol new-symtable (cadr expr)))
                         (change-lambda-syms! (cadr expr) new-symtable))
                     ;; aconv on lambda body
                     (aconv (caddr expr) new-symtable)))
                ;; Others
                (else
                  (map (lambda (x) (aconv x symtable))
                       expr))))))

  (aconv exp-content '()))

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
