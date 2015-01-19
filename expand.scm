;; EXPAND
;; public (expand expr)

(define (expand expr)
  (cond
      ((or (null? expr) (symbol? expr) (number? expr) (char? expr) (string? expr) (boolean? expr)) expr)
      
      ((equal? (car expr) 'define) (expand-define expr))
      ((equal? (car expr) 'if) (expand-if expr))
      ((equal? (car expr) 'begin) (expand-begin expr))
      ((equal? (car expr) 'let) (expand-let expr))
      ((equal? (car expr) 'let*) (expand-let* expr))
      ((equal? (car expr) 'letrec) (expand-letrec expr))
      ((equal? (car expr) 'do) (expand-do expr))
      ((equal? (car expr) 'lambda) (expand-lambda expr))
      ((equal? (car expr) 'or) (expand-or expr))
      ((equal? (car expr) 'and) (expand-and expr))
      ((equal? (car expr) 'cond) (expand-cond expr))
      (else (if (list? (cdr expr))
                (map expand expr)
                (cons (expand (car expr)) (expand (cdr expr))))))) ;; (e1 . e2)

;; DEFINE
(define (expand-define expr)
  (if (pair? (cadr expr)) ;; (define (fn arg1 ... argN) ...)
    `(define ,(caadr expr) ,(expand `(lambda ,(cdadr expr) ,@(cddr expr))))         
    `(define ,(cadr expr) ,(expand (caddr expr)))))

;; IF
(define (expand-if expr)
  (if (eq? (length expr) 3)
      ;; No else
      `(if ,(expand (cadr expr)) ,(expand (caddr expr)) #f)
      ;; If then else
      `(if ,(expand (cadr expr)) ,(expand (caddr expr)) ,(expand (cadddr expr)))))

;; BEGIN
(define (expand-begin expr)
  (if (eq? (length expr) 2)
    ;; 1 expr
    (expand (cadr expr))
    ;; > 1 expr
    (let ((v (gensym)))
           (expand `(let ((,v ,(expand (cadr expr)))) ,(expand `(begin ,@(cddr expr))))))))

;; LET
(define (expand-let expr)
  (if (symbol? (cadr expr))
    (expand-letn expr)
    (let ((bindings (cadr  expr))
          (body     (cddr expr)))
      `((lambda ,(map car bindings)
                ,(if (and (list? (car body))
                          (eq? (car (car body)) 'define))
                     (expand-internal body)
                     (expand `(begin ,@body))))
                ,@(map (lambda (n) (expand (cadr n))) bindings)))))

;; LETN (named let)
(define (expand-letn expr)
   (let ((id (cadr expr))
         (bindings (caddr expr))
         (body (cdddr expr)))
   (expand `((letrec ((,id (lambda ,(map car bindings) ,@body))) ,id) ,@(map cadr bindings)))))

;; LET*
(define (expand-let* expr)
  (let ((bindings (cadr expr))
        (body     (cddr expr)))
    (cond ;; 1 body
          ((and (list? body) (= (length bindings) 1))
              (expand `(let ,bindings ,@body)))
          ;; > 1 body
          (else (expand `(let ,(list (car bindings))
                        (let* ,(cdr bindings)
                           ,@body)))))))
      
;; LETREC
;; TODO : use set! for now
(define (expand-letrec expr)
  (let ((bindings (cadr expr))
        (body     (cddr expr)))
     (expand `(let ,(map (lambda (l) (list (car l) #f)) bindings)
           ,@(map (lambda (l) (list 'set! (car l) (cadr l))) bindings)
           ,@body))))

;; DO-h
(define (do-steps ids)
  (if (null? ids)
    '()
    (let ((stepl (cddr (car ids))))
      (if (null? stepl)
        (cons (car (car ids)) (do-steps (cdr ids)))
        (cons (car stepl) (do-steps (cdr ids)))))))

;; DO
(define (expand-do expr)
   (let ((SYM (gensym))
         (ids (cadr expr))
         (stop (caddr expr))
         (commands (cdddr expr)))
      (expand `(letrec ((,SYM (lambda ,(map car ids)
                         (if ,(car stop)
                            (begin ,@(cdr stop))
                            (begin ,@commands
                                   (,SYM ,@(do-steps ids)))))))
               (,SYM ,@(map cadr ids))))))

;; Expand-internal helper
;; binds is formatted list of bindings (accu)
(define (expand-internal-h binds expr)
  (if (null? expr)
      (cons binds '())
      (let ((first (car expr)))
        (if (and (list? first)
                 (eq? (car first) 'define))
            (let ((exp (expand first)))
              (expand-internal-h (append binds (list (list (cadr exp) (expand (caddr exp)))))
                                 (cdr expr)))
            (cons binds expr)))))
        
;; Expand a body which contains internal definitions
(define (expand-internal expr)
  (let ((ret (expand-internal-h '() expr)))
    (expand `(letrec ,(car ret)
                ,(if (null? (cdr ret))
                   #f ;; TODO #f
                   (expand `(begin ,@(cdr ret))))))))

;; LAMBDA
(define (expand-lambda expr)
  (if (eq? (length expr) 3)
      ;; 1 body
      `(lambda ,(cadr expr) ,(expand (caddr expr)))
      ;; > 1 body
      (let ((first-body (caddr expr)))
        (if (and (list? first-body)
                 (eq? (car first-body) 'define))
            ;; Internal def
            `(lambda ,(cadr expr) ,(expand-internal (cddr expr)))
            ;; No internal def
            `(lambda ,(cadr expr) ,(expand `(begin ,@(cddr expr))))))))

;; OR
(define (expand-or expr)
  (cond ((eq? (length expr) 1) '#f) ;; (or)
        ((eq? (length expr) 2) (expand (cadr expr))) ;; (or e1)
        (else `(if ,(expand (cadr expr)) #t ,(expand `(or ,@(cddr expr))))))) ;; (or e1 ... en)

;; AND
(define (expand-and expr)
  (cond ((eq? (length expr) 1) '#t) ;; (and)
        ((eq? (length expr) 2) (expand (cadr expr))) ;; (and e1)
        (else `(if ,(expand (cadr expr)) ,(expand `(and ,@(cddr expr))) #f)))) ;; (and e1 ... en)

;; COND
(define (expand-cond expr)
  (cond ;; (cond)
        ((eq? (length expr) 1) (error "Ill-formed special form: cond"))
        ;; (cond (e1 e2))
        ((eq? (length expr) 2)
            (if (eq? (caadr expr) 'else) 
                ;; (cond (else ...))
                (expand (cadr (cadr expr)))
                ;; (cond (e1 e2))
                `(if ,(expand (caadr expr))
                     ,(expand (cadr (cadr expr)))
                     #f))) ;; NOTE : Should return #!void
        ;; (cond (e1 e1) ...)
        (else `(if ,(expand (caadr expr))
                   ,(expand (cadr (cadr expr)))
                   ,(expand `(cond ,@(cddr expr)))))))