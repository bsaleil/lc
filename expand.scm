
(define (expand expr)
  (cond
      ((or (null? expr) (symbol? expr) (number? expr) (char? expr) (string? expr) (boolean? expr)) expr)
      
      ((equal? (car expr) 'define) (expand-define expr))
      ((equal? (car expr) 'if) (expand-if expr))
      ((equal? (car expr) 'begin) (expand-begin expr))
      ((equal? (car expr) 'let) (expand-let expr))
      ((equal? (car expr) 'lambda) (expand-lambda expr))
      ((equal? (car expr) 'or) (expand-or expr))
      ((equal? (car expr) 'and) (expand-and expr))
      ((equal? (car expr) 'cond) (expand-cond expr))
      (else (map expand expr))))

;; DEFINE
(define (expand-define expr)
  (if (list? (cadr expr)) ;; (define (fn arg1 ... argN) ...)
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
  (let ((bindings (cadr  expr))
        (body     (cddr expr)))
    (cond ;; 2 body
          ((and (list? body) (= (length body) 2))
                `((lambda ,(map car bindings) ,(expand `(let ((,(gensym) ,(car body))) ,(cadr body)))) ,@(expand (map cadr bindings))))
          ;; > 2 body
          ((and (list? body) (> (length body) 2))
                `((lambda ,(map car bindings) ,(expand `(let ((,(gensym) ,(car body))) ,@(cdr body)))) ,@(expand (map cadr bindings))))
          ;; 1 body
          (else `((lambda ,(map car bindings) ,(expand (car body))) ,@(expand (map cadr bindings)))))))
      
;; LAMBDA
(define (expand-lambda expr)
  (if (eq? (length expr) 3)
      ;; 1 body
      `(lambda ,(cadr expr) ,(expand (caddr expr)))
      ;; > 1 body
      `(lambda ,(cadr expr) ,(expand `(begin ,@(cddr expr))))))

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