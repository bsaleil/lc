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

;; EXPAND

(define ILL-DEFINE "Ill-placed 'define'")
(define ILL-CASE   "Ill-formed 'case'")
(define EMPTY-BODY "Body must contain at least one expression")

(define list-accessors '(caar cadr cadar caddar cdar cddr caddr cdddr cadddr))

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
      ((equal? (car expr) 'list)  (expand-list expr))
      ((equal? (car expr) 'let) (expand-let expr))
      ((equal? (car expr) 'let*) (expand-let* expr))
      ((equal? (car expr) 'letrec) (expand-letrec expr))
      ((equal? (car expr) 'lambda) (expand-lambda expr))
      ((equal? (car expr) 'or) (expand-or expr))
      ((equal? (car expr) 'and) (expand-and expr))
      ((equal? (car expr) 'cond) (expand-cond expr))
      ((equal? (car expr) 'case) (expand-case expr))
      ((equal? (car expr) 'quote) expr)
      ((equal? (car expr) 'set!) (expand-set! expr))
      ((member (car expr) '(> >= < <= =)) (expand-cmp expr))
      ((member (car expr) list-accessors) (expand-accessor expr))
      (else (if (list? (cdr expr))
                (map expand expr)
                (cons (expand (car expr)) (expand (cdr expr))))))) ;; (e1 . e2)

(define (expand-accessor expr)
    (let ((op   (car expr))
          (opnd (expand (cdr expr))))
      (case (car expr)
         ((caar)  `(car (car ,@opnd)))
         ((cadr)  `(car (cdr ,@opnd)))
         ((cadar) `(car (cdr (car ,@opnd))))
         ((caddar) `(car (cdr (cdr (car ,@opnd)))))
         ((cdar)  `(cdr (car ,@opnd)))
         ((cddr)  `(cdr (cdr ,@opnd)))
         ((caddr) `(car (cdr (cdr ,@opnd))))
         ((cdddr) `(cdr (cdr (cdr ,@opnd))))
         ((cadddr) `(car (cdr (cdr (cdr ,@opnd))))))))

(define (expand-set! expr)
  (let ((r (assoc (cadr expr) gids)))
     (if r
        (set-cdr! r #f)))

  `(set! ,(cadr expr) ,(expand (caddr expr))))

(define (expand-cmp expr)

  (define (expand-cmp-n op expr prev)
    (cond ;;
          ((= (length expr) 1)
             (let ((s (gensym)))
               `(let ((,s ,(car expr)))
                  (,op ,prev ,s))))
          ;; prev
          (prev
             (let ((s (gensym)))
               `(let ((,s ,(car expr)))
                  (and (,op ,prev ,s)
                       ,(expand-cmp-n op (cdr expr) s)))))
          ;;
          (else
             (let ((s1 (gensym))
                   (s2 (gensym)))
               `(let ((,s1 ,(car expr)) (,s2 ,(cadr expr)))
                  (and (,op ,s1 ,s2)
                       ,(expand-cmp-n op (cddr expr) s2)))))))

  (if (<= (length (cdr expr)) 2)
      (cons (car expr) (expand (cdr expr)))
      (expand (expand-cmp-n (car expr) (cdr expr) #f))))

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

;; LIST
(define (expand-list expr)
  (cond ((null? (cdr expr))
           `(quote ()))
        ((eq? (length (cdr expr)) 1)
           (expand `(cons ,(cadr expr) '())))
        (else
           (let ((r (cddr expr)))
             (expand `(cons ,(cadr expr) (list ,@r)))))))

;; LET*
(define (expand-let* expr)
  (let ((bindings (cadr expr))
        (bodies   (cddr expr)))
    (if (<= (length bindings) 1)
        (expand `(let ,bindings ,@bodies))
        (expand `(let (,(car bindings))
                   (let* ,(cdr bindings)
                     ,@bodies))))))

;; LET
;; letn and let with internal defs are not handled by compiler (mlc-let)
(define (expand-let expr)

  ;; NAMED LET
  (define (expand-letn expr)
    (let ((id (cadr expr))
          (bindings (caddr expr))
          (body (cdddr expr)))
      (expand `((letrec ((,id (lambda ,(map car bindings) ,@body))) ,id) ,@(map cadr bindings)))))

  (if (symbol? (cadr expr))
      ;; Named let
      (expand-letn expr)
      ;; let
      `(let ,(expand (cadr expr))
         ,(expand (cons 'begin (cddr expr))))))

;; LETREC
(define (expand-letrec expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    `(letrec ,(map (lambda (n) (cons (car n) (expand (cdr n))))
                       bindings)
       ,(expand (cons 'begin body)))))

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
        (else
          (let ((sym (gensym)))
             `(let ((,sym ,(expand (cadr expr))))
                (if ,sym
                   ,sym
                   ,(expand `(or ,@(cddr expr))))))))) ;; (or e1 ... en)

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
                (expand-cond-clause expr '#f))) ;; NOTE : Should return #!void
        ;; (cond (e1 e2) ...)
        (else (expand-cond-clause expr
                                  (expand `(cond ,@(cddr expr)))))))

;; COND clause
;; expand cond clause with 'el' in else part
(define (expand-cond-clause expr el)
  (cond ((null? (cdr (cadr expr)))
            ;; (cond (e1))
            (let ((sym (gensym)))
              `(let ((,sym ,(expand (car (cadr expr)))))
                 (if ,sym
                    ,sym
                    ,el))))

        ((eq? (cadr (cadr expr)) '=>)
            ;; (cond (e1 => e2))
            (let ((sym (gensym)))
              `(let ((,sym ,(expand (car (cadr expr)))))
                  (if ,sym
                    (,(expand (caddr (cadr expr))) ,sym)
                    ,el))))
        (else
            ;; (cond (e1 ...))
            `(if ,(expand (caadr expr))
                ,(expand `(begin ,@(cdr (cadr expr))))
                ,el))))

;; CASE
(define (expand-case expr)
  (let ((key (cadr expr))
        (clauses (cddr expr))
        (sym (gensym)))
    `(let ((,sym ,key))
      ,(expand-case-clauses sym clauses))))

;; CASE (clauses)
(define (expand-case-clauses sym clauses)
  (cond ;; 0 clause, error
        ((null? clauses) (error ILL-CASE))
        ;; 1 clause
        ((= (length clauses) 1)
           (let ((clause (car clauses)))
             (if (eq? (car clause) 'else)
                (expand (cons 'begin (cdr clause)))
                `(if (memv ,sym (quote ,(car clause)))
                    ,(expand (cons 'begin (cdr clause)))
                    #f))))
        ;; >1 clauses
        (else
          (let ((clause (car clauses)))
            `(if (memv ,sym (quote ,(car clause)))
                ,(expand (cons 'begin (cdr clause)))
                ,(expand-case-clauses sym (cdr clauses)))))))

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
            (get-internal-defs-h (cdr exprs) (append def (list (expand-define (car exprs)))) body #t)
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
