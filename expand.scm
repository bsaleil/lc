

(define (expand expr)
  (cond
      ((or (null? expr) (symbol? expr) (number? expr) (char? expr) (string? expr) (boolean? expr)) expr)
      
      ;((equal? (car expr) 'define) (expand-define expr))
      ;((equal? (car expr) 'if) (expand-if expr))
      ;((equal? (car expr) 'begin) (expand-begin expr))
      ;((equal? (car expr) 'let) (expand-let expr))
      ;((equal? (car expr) 'lambda) (expand-lambda expr))
      (else (map expand expr))))

; ;; DEFINE
; (define (expand-define expr)
;   (if (list? (cadr expr)) ;; (define (fn arg1 ... argN) ...)
;     `(define ,(caadr expr) ,(expand `(lambda ,(cdadr expr) ,@(cddr expr))))         
;     `(define ,(cadr expr) ,(expand (caddr expr)))))

; ;; IF
; (define (expand-if expr)
;   (if (eq? (length expr) 3) ;; (if cond then)
;     `(if ,(expand (cadr expr)) ,(expand (caddr expr)) #f)
;     `(if ,(expand (cadr expr)) ,(expand (caddr expr)) ,(expand (cadddr expr)))))

; ;; BEGIN
; (define (expand-begin expr)
;   (if (eq? (length expr) 2) ;; (begin EXPR)
;     (expand (cadr expr))
;     (let ((v (gensym)))
;           `(let ((,v ,(expand (cadr expr)))) ,(expand `(begin ,@(cddr expr)))))))

; ;; LET
; (define (expand-let expr)
;   (if (> (length expr) 3)
;     `(let ,(expand (cadr expr)) ,(expand `(begin ,@(cddr expr))))
;     `((lambda ,(map expand (map car (cadr expr))) ; binding symbols
;               ,(if (list? (caddr expr))           ; body
;                    (map (expand (caddr expr)))
;                    (expand (caddr expr)))
;               )                                   ; lambda end
;               ,@(map cadr (cadr expr)))))         ; binding values

; ;; LAMBDA
; (define (expand-lambda expr)
;   (if (eq? (length expr) 3) ;; (lambda (args) body)
;     `(lambda ,(cadr expr) ,(expand (caddr expr)))
;     ;; (lambda (args) body1 body2 ... bodyN)
;     `(lambda ,(cadr expr) ,(expand `(begin ,@(cddr expr))))))