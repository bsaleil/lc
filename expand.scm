;; EXPAND

;; ERRORS
(define ILL-DEFINE "Ill-placed 'define'")
(define EMPTY-BODY "Body must contain at least one expression")

;; Expand function called from top-level (allows define)
(define (expand-tl exprs)
  (if (null? exprs)
      '()
      (let ((expr (car exprs)))
        (if (and (list? expr) (eq? (car expr) 'define))
            (cons (expand-define expr) (expand-tl (cdr exprs)))
            (cons (expand expr) (expand-tl (cdr exprs)))))))

;; Expand function called outside top-level
(define (expand expr)
  (cond
      ((or (null? expr) (vector? expr) (symbol? expr) (number? expr) (char? expr) (string? expr) (boolean? expr)) expr)
      ((equal? (car expr) 'define) (error ILL-DEFINE))
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
      ((equal? (car expr) 'quote) expr)
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
;; TODO : begin is a special form
;; compiler is able to build a lazy object chain from begin
;; WITHOUT internal defs. For now, begin with internal defs are 
;; transformed into lambda.
(define (expand-begin expr)
  (if (eq? (length expr) 2)
    ;; 1 expr
    (expand (cadr expr))
    ;; > 1 expr
    (let* ((r (get-internal-defs (cdr expr)))
           (defs (car r))
           (body (cdr r)))
      
      (if (not (null? defs))
        ;; Internal def
        (expand (build-internal-defs defs body))
        `(begin ,@(map expand (cdr expr)))))))

;; LET
(define (expand-let expr)
  (if (symbol? (cadr expr))
    ;; Named let
    (expand-letn expr)
    ;; Normal let
    (let ((ids (map car (cadr expr)))
          (values (map cadr (cadr expr)))
          (bodies (cddr expr)))
    `(let ,(map (lambda (i v)
                     (list i (expand v))) ids values)
       ,@(map expand bodies)))))


; (define (expand-let expr)
;   (if (symbol? (cadr expr))
;     ;; Named let
;     (expand-letn expr)
;     ;; Normal let
;     (let ((bindings (cadr  expr))
;           (body     (cddr expr)))
;       `((lambda ,(map car bindings)
;                 ,(expand `(begin ,@body))) ,@(map expand (map cadr bindings))))))

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
                            ,(if (null? (cdr stop))
                                #t
                                `(begin ,@(cdr stop)))
                            (begin ,@commands
                                   (,SYM ,@(do-steps ids)))))))
               (,SYM ,@(map cadr ids))))))

;; LAMBDA
(define (expand-lambda expr)
  (cond ((< (length expr) 3) (error EMPTY-BODY))
        (;; 1 body
         (eq? (length expr) 3)
          (if (and (list? (caddr expr))
                   (eq? (car (caddr expr)) 'define))
              (error EMPTY-BODY)
              `(lambda ,(cadr expr) ,(expand (caddr expr)))))   
         ;; > 1 body
         (else
           (let* ((r (get-internal-defs (cddr expr)))
                  (defs (car r))
                  (body (cdr r)))
             
             (if (not (null? defs))
                `(lambda ,(cadr expr) ,(expand (build-internal-defs defs body)))
                `(lambda ,(cadr expr) ,(expand `(begin ,@body))))))))

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
                (expand `(begin ,@(cdr (cadr expr))))
                ;; (cond (e1 e2))
                `(if ,(expand (caadr expr))
                     ,(if (null? (cdr (cadr expr)))
                          ;; (cond (e1))
                          '#t
                          ;; (cond (e1 e2 [e3 ...]))
                          (expand `(begin ,@(cdr (cadr expr)))))
                     #f))) ;; NOTE : Should return #!void
        ;; (cond (e1 e2) ...)
        (else `(if ,(expand (caadr expr))
                   ,(if (null? (cdr (cadr expr)))
                        ;; (cond (e1) ...)
                        '#t
                        ;; (cond (e1 e2) ...)
                        (expand `(begin  ,@(cdr (cadr expr)))))
                   ,(expand `(cond ,@(cddr expr)))))))

;;----------

;; Get exprs (list of lists)
;; Return '(defs body)
;; defs : list of internal definitions
;; body : list of body expressions
(define (get-internal-defs exprs)
  (get-internal-defs-h exprs '() '() #t))

(define (get-internal-defs-h exprs def body def-allowed)
  (cond ((null? exprs)
           (cons def body))
        ((and (list? (car exprs))
              (eq? (caar exprs) 'define))
           (if def-allowed
            (get-internal-defs-h (cdr exprs) (append def (list (car exprs))) body #t)
            (error ILL-DEFINE)))
        (else (get-internal-defs-h (cdr exprs)
                         def
                         (append body (list (car exprs)))
                         #f))))

;; Build bindings for letrec from list of definitions
;; Ex :
;; defs = '((define A 10) (define B 20))
;; Return : ((A 10) (B 20))
(define (build-defs-bindings defs)
  (if (null? defs)
      '()
      (let ((f (car defs)))
        ;; (define (id) ...)
        (if (list? (cadr f))
            (let ((fn `(lambda ,(cdadr f) ,@(cddr f))))
              (cons (list (car (cadr f)) fn) (build-defs-bindings (cdr defs))))
            (cons (list (cadr f) (caddr f)) (build-defs-bindings (cdr defs)))))))

;; Build letrec for internal defs from defs and body
(define (build-internal-defs defs body)
  `(letrec ,(build-defs-bindings defs)
                    ,@body))