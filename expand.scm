(define (expand expr)
  (cond
      ((or (null? expr) (symbol? expr) (number? expr) (char? expr) (string? expr) (boolean? expr)) expr)
      
      ((equal? (car expr) 'define) (expand-define expr))
      ((equal? (car expr) 'if) (expand-if expr))
      ((equal? (car expr) 'lambda) (expand-lambda expr))
      (else (map expand expr))))

;; DEFINE
(define (expand-define expr)
  `(define ,(cadr expr) ,(expand (caddr expr))))

;; IF
(define (expand-if expr)
  (if (eq? (length expr) 3) ;; (if cond then)
    `(if ,(expand (cadr expr)) ,(expand (caddr expr)) #f)
    `(if ,(expand (cadr expr)) ,(expand (caddr expr)) ,(expand (cadddr expr)))))

;; LAMBDA
(define (expand-lambda expr)
  (if (eq? (length expr) 3) ;; (lambda (args) body)
    `(lambda ,(cadr expr) ,(expand (caddr expr)))
    ;; (lambda (args) body1 body2 ... bodyN)
    `(lambda ,(cadr expr) ,(expand `(begin ,@(cddr expr))))))