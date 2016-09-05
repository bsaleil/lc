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

(define aliases
  `(;; FX ops
    (,##fx+  . ##fx+)
    (,##fx-  . ##fx-)
    (,##fx*  . ##fx*)
    ;; FX ops with overflow
    (,##fx+? . ##fx+?)
    (,##fx-? . ##fx-?)
    (,##fx*? . ##fx*?)
    ;; FL ops
    (,##fl+  . ##fl+)
    (,##fl-  . ##fl-)
    (,##fl*  . ##fl*)
    ;; Generic ops
    (,+ . +)
    (,- . -)
    (,* . *)
    (,/ . /)
    (,< . <)
    (,> . >)
    (,<= . <=)
    (,>= . >=)
    (,= . =)
    ;; Core primitives
    (,##box . ##box)
    (,##unbox . ##unbox)
    (,##set-box! . ##set-box!)
    (,##subtype . ##subtype)
    (,##subtyped? . ##subtyped?)
    (,##cons    . cons)
    (,cons      . cons)
    (,modulo    . modulo)
    (,quotient  . quotient)
    (,remainder . remainder)
    (,##eq? . eq?)
    (,eq? . eq?)
    (,##not . not)
    (,not . not)
    (,car . car)
    (,cdr . cdr)
    (,##boolean? . boolean?)
    (,boolean? . boolean?)
    (,##char? . char?)
    (,char? . char?)
    (,##fixnum? . fixnum?)
    (,fixnum? . fixnum?)
    (,##flonum? . flonum?)
    (,##integer? . integer?)
    (,integer? . integer?)
    (,##null? . null?)
    (,null? . null?)
    (,##pair? . pair?)
    (,pair? . pair?)
    (,##procedure? . procedure?)
    (,procedure? . procedure?)
    (,##string? . string?)
    (,string? . string?)
    (,##symbol? . symbol?)
    (,symbol? . symbol?)
    (,##vector? . vector?)
    (,vector? . vector?)
    (,##char<? . char<?)
    (,char<? . char<?)
    (,char<=? . char<=?)
    (,char>=? . char>=?)
    (,##char=? . char=?)
    (,char=? . char=?)
    (,##eof-object? . eof-object?)
    (,eof-object? . eof-object?)
    (,##mem-allocated? . ##mem-allocated?)
    (,##vector-set! . vector-set!)
    (,vector-set! . vector-set!)
    (,##string-set! . string-set!)
    (,string-set! . string-set!)
    (,##vector-ref . vector-ref)
    (,vector-ref . vector-ref)
    (,make-vector . make-vector)
    (,make-string . make-string)
    (,string->symbol . string->symbol)
    (,symbol->string . symbol->string)
    (,##integer->char . integer->char)
    (,integer->char . integer->char)
    (,##char->integer . char->integer)
    (,char->integer   . char->integer)
    (,##string-length . string-length)
    (,string-length . string-length)
    (,##vector-length . vector-length)
    (,vector-length . vector-length)
    (,##string-ref . string-ref)
    (,string-ref . string-ref)
    (,##fixnum->flonum . ##fixnum->flonum)
    ;; Functions
    (,abs       . abs)
    (,##fxabs . abs)
    (,##flabs   . abs)
    (,##fxabs?  . abs)
    (,odd?      . odd?)
    (,##fxodd? . odd?)
    (,##flodd?  . odd?)
    (,even?     . even?)
    (,##fxeven? . even?)
    (,##fleven? . even?)
    (,min . min)
    (,max . max)
    (,##fxmin . min)
    (,##fxmax . max)
    (,##flmax   . max)
    (,expt . expt)
    (,negative? . negative?)
    (,positive? . positive?)
    (,eqv?      . eqv?)
    (,zero? . zero?)
    (,member . member)
    (,memq . memq)
    (,memv . memv)
    (,equal? . equal?)
    (,length . length)
    (,append . append)
    (,assoc . assoc)
    (,assv . assv)
    (,assq . assq)
    (,apply . apply)
    (,reverse . reverse)
    (,call/cc . call/cc)
    (,open-input-file  . open-input-file)
    (,open-output-file . open-output-file)
    (,close-input-port . close-input-port)
    (,close-output-port . close-output-port)
    (,input-port? . input-port?)
    (,output-port? . output-port?)
    (,number? . number?)
    (,##number? . number?)
    (,list? . list?)
    (,newline . newline)
    (,string-append . string-append)
    (,list->vector . list->vector)
    (,list-ref . list-ref)
    (,substring . substring)
    (,string-copy . string-copy)
    (,string<? . string<?)
    (,list . list)
    (,string-fill! . string-fill!)
    (,vector-fill! . vector-fill!)
    (,list->string . list->string)
    (,string->list . string->list)
    (,vector->list . vector->list)
    (,number->string . number->string)
    (,string->number . string->number)
    (,string=? . string=?)
    (,##fxzero? . zero?)
    (,read . read)
    (,write . write)
    (,display . display)
    (,##char-whitespace? . char-whitespace?)
    (,char-whitespace? . char-whitespace?)
    (,##char-downcase . char-downcase)
    (,char-downcase . char-downcase)
    (,##eqv? . eqv?)
    (,vector . vector)
    (,##vector . vector)
    (,##list . list)
    (,##string . string)
    (,string . string)
    (,write-char . write-char)
    (,read-char . read-char)
    (,map . map)
    (,for-each . for-each)
    ;; Not implemented
    (,##quasi-list   . NYIquasi-list)
    (,truncate       . NYItruncate)
    (,##fltruncate   . NYItruncate)
    (,##flfinite?    . NYIfinite?)
    (,inexact->exact . NYIinexact->exact)
    (,exact->inexact . exact->inexact)
    (,exact?         . exact?)
    (,##rational?    . NYIrational?)
    (,rational?      . NYIrational?)
    (,peek-char    . NYIpeek-char)
    ;;
    ;; NYI
    ;;
    (,##fx= . =)
    (,##fx< . <)
    (,##flzero? . zero?)
    (,##fx> . >)
    (,##fl> . >)
    (,##fl>= . >=)
    (,##fl< . <)
    (,##fl<= . <)
    (,##fxnegative? . negative?)
    (,##flnegative? . negative?)
    (,##fxpositive? . positive?)
    (,##flpositive? . positive?)
    (,##fl= . =)
    (,##fl/ . /)
    (,##fx<= . <=)
    (,##fx>= . >=)
    (,##car . car)
    (,##caar . caar)
    (,caar . caar)
    (,##cadr . cadr)
    (,cadr . cadr)
    (,##cadar . cadar)
    (,cadar . cadar)
    (,##caddar . caddar)
    (,caddar . caddar)
    (,##caddr . caddr)
    (,caddr . caddr)
    (,##cadddr . cadddr)
    (,cadddr . cadddr)
    (,##cdr . cdr)
    (,##cdar . cdar)
    (,cdar . cdar)
    (,##cddr . cddr)
    (,cddr . cddr)
    (,##cdddr . cdddr)
    (,cdddr . cdddr)
    (,##fxquotient . quotient)
    (,##fxremainder . remainder)
    (,##set-car! . set-car!)
    (,set-car! . set-car!)
    (,##set-cdr! . set-cdr!)
    (,set-cdr! . set-cdr!)
    (,##fxmodulo . modulo)
    (,##char-numeric? . char-numeric?)
    (,char-numeric? . char-numeric?)
    (,##char-alphabetic? . char-alphabetic?)
    (,##char-downcase . ##char-downcase)
    ))

(define (get-alias el)
  (if (and (pair? el)
           (eq? (car el) 'quote))
      (set! el (cadr el)))
  (let ((r (assoc el aliases)))
    (if r
        (cdr r)
        el)))

(define-macro (resolve-alias op ast)
  `(if (and (pair? ,op)
            (eq? (car ,op) 'quote))
       (let ((r (assoc (cadr ,op) aliases)))
        (if r
            (begin (set! ,op (cdr r))
                   (set! ,ast (cons ,op (cdr ,ast))))))))

(define-macro (resolve-symalias ast)
  (let ((s (gensym)))
    `(if (and (pair? ,ast)
              (eq? (car ,ast) 'quote))
         (let ((,s (assoc (cadr ,ast) aliases)))
           (if ,s
               (set! ,ast (cdr ,s)))))))

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
  (resolve-symalias expr)
  (cond ((void? expr) (atom-node-make #f))
        ((or (null? expr) (vector? expr) (symbol? expr) (number? expr) (char? expr) (string? expr) (boolean? expr))
         (atom-node-make expr))
        (else
          (let ((op (car expr)))
            (resolve-alias op expr)
            (cond
              ((equal? (car expr) 'define) (error ILL-DEFINE))
              ((equal? (car expr) 'if) (expand-if expr))
              ((equal? (car expr) 'begin) (expand-begin expr))
              ((equal? (car expr) 'do) (expand-do expr))
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
              ((member (car expr) '(real? eqv?)) (expand-prim expr))
              ((member (car expr) '(> >= < <= =)) (expand-cmp expr))
              ((member (car expr) list-accessors) (expand-accessor expr))
              (else (if (list? (cdr expr))
                        (map expand expr)
                        (cons (expand (car expr)) (expand (cdr expr)))))))))) ;; (e1 . e2)

(define (expand-accessor expr)
    (define (CAR) (atom-node-make 'car))
    (define (CDR) (atom-node-make 'cdr))

    (let ((opnd (expand (cdr expr))))
      (case (car expr)
         ((caar)   `(,(CAR) (,(CAR) ,@opnd)))
         ((cadr)   `(,(CAR) (,(CDR) ,@opnd)))
         ((cadar)  `(,(CAR) (,(CDR) (,(CAR) ,@opnd))))
         ((caddar) `(,(CAR) (,(CDR) (,(CDR) (,(CAR) ,@opnd)))))
         ((cdar)   `(,(CDR) (,(CAR) ,@opnd)))
         ((cddr)   `(,(CDR) (,(CDR) ,@opnd)))
         ((caddr)  `(,(CAR) (,(CDR) (,(CDR) ,@opnd))))
         ((cdddr)  `(,(CDR) (,(CDR) (,(CDR) ,@opnd))))
         ((cadddr) `(,(CAR) (,(CDR) (,(CDR) (,(CDR) ,@opnd))))))))

(define (expand-set! expr)
  `(set! ,(cadr expr) ,(expand (caddr expr))))

(define (expand-prim expr)

  (define (get-opnds-bindings opnds bindings symbols)
    (if (null? opnds)
        (cons (reverse bindings)
              (reverse symbols))
        (let ((opnd (car opnds)))
          (if (literal? opnd)
              (get-opnds-bindings (cdr opnds) bindings (cons opnd symbols))
              (let ((sym (gensym)))
                (get-opnds-bindings
                  (cdr opnds)
                  (cons (cons sym (list opnd))
                        bindings)
                  (cons sym symbols)))))))


  (assert-p-nbargs (car expr) expr)
  (let ((op (car expr)))
    (cond ;; real?
          ((eq? op 'real?)
            `(number? ,@(cdr expr)))
          ;; eqv?
          ((eq? op 'eqv?)
            (let* ((r (get-opnds-bindings (cdr expr) '() '()))
                   (syml (cadr r))
                   (symr (caddr r)))
              (expand
                `(let ,(car r)
                   (if (number? ,syml)
                       (and (number? ,symr) (= ,syml ,symr))
                       (eq? ,syml ,symr)))))))))

(define (expand-cmp expr)

  (define op-node (atom-node-make (car expr)))

  (define (expand-cmp-n expr prev)
    (cond ;;
          ((= (length expr) 1)
             (let ((s (gensym)))
               `(let ((,s ,(car expr)))
                  (,op-node ,prev ,s))))
          ;; prev
          (prev
             (let ((s (gensym)))
               `(let ((,s ,(car expr)))
                  (and (,op-node ,prev ,s)
                       ,(expand-cmp-n (cdr expr) s)))))
          ;;
          (else
             (let ((s1 (gensym))
                   (s2 (gensym)))
               `(let ((,s1 ,(car expr)) (,s2 ,(cadr expr)))
                  (and (,op-node ,s1 ,s2)
                       ,(expand-cmp-n (cddr expr) s2)))))))

  (if (<= (length (cdr expr)) 2)
      (cons op-node (expand (cdr expr)))
      (expand (expand-cmp-n (cdr expr) #f))))

;; DEFINE
(define (expand-define expr)
  (if (pair? (cadr expr)) ;; (define (fn arg1 ... argN) ...)
    `(define ,(caadr expr) ,(expand `(lambda ,(cdadr expr) ,@(cddr expr))))
    `(define ,(cadr expr) ,(expand (caddr expr)))))

;; IF
(define (expand-if expr)
  (cond ;; (if A B) -> (if A B #f)
        ((eq? (length expr) 3)
           (expand `(if ,(cadr expr) ,(caddr expr) #f)))
        ;; (if (not A) B C) -> (if A C D)
        ((and (pair? (cadr expr))
              (eq? (get-alias (caadr expr)) 'not))
           (let ((c (cadadr expr))
                 (t (cadddr expr))
                 (f (caddr expr)))
             (expand `(if ,c ,t ,f))))
        ;;
        (else
           `(if ,(expand (cadr expr)) ,(expand (caddr expr)) ,(expand (cadddr expr))))))

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

;; DO
(define (expand-do expr)
  (let ((vars-init (map (lambda (el) (list (car el) (cadr el))) (cadr expr)))
        (steps
          (foldr (lambda (el r)
                   (if (not (null? (cddr el)))
                       (cons (caddr el) r)
                       (cons (car el) r)))
                 '()
                 (cadr expr)))
        (test (caaddr expr))
        (exprs
          (let ((e (cdaddr expr)))
            (if (null? e) (list #f) e)))
        (body (cdddr expr))
        (loopname (gensym)))
    (expand
      `(let ,loopname ,vars-init
         (if ,test
             (begin ,@exprs)
             (begin ,@body
                    (,loopname ,@steps)))))))

;; LIST
;(define (expand-list expr)
;  (cond ((null? (cdr expr))
;           `(quote ()))
;        ((eq? (length (cdr expr)) 1)
;           (expand `(cons ,(cadr expr) '())))
;        (else
;           (let ((r (cddr expr)))
;             (expand `(cons ,(cadr expr) (list ,@r)))))))

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

  (define (expand-bindings bindings)
    (map (lambda (n) (cons (car n) (expand (cdr n))))
         bindings))

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
      (if (null? (cadr expr))
          (expand `(begin ,@(cddr expr)))
          (let ((bindings (expand-bindings (cadr expr))))
            `(let ,bindings
               ,(expand (cons 'begin (cddr expr))))))))

;; LETREC
(define (expand-letrec expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    `(letrec ,(map (lambda (n) (cons (car n) (expand (cdr n))))
                       bindings)
       ,(expand (cons 'begin body)))))

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
             (expand
               `(let ((,sym ,(cadr expr)))
                  (if ,sym
                     ,sym
                     (or ,@(cddr expr))))))))) ;; (or e1 ... en)

;; AND
(define (expand-and expr)
  (cond ((eq? (length expr) 1) '#t) ;; (and)
        ((eq? (length expr) 2) (expand (cadr expr))) ;; (and e1)
        (else (expand `(if ,(cadr expr) (and ,@(cddr expr)) #f))))) ;; (and e1 ... en)

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
              (expand
                `(let ((,sym ,(car (cadr expr))))
                   (if ,sym
                      ,sym
                      ,el)))))
        ((eq? (cadr (cadr expr)) '=>)
            ;; (cond (e1 => e2))
            (let ((sym (gensym)))
              (expand
                `(let ((,sym ,(car (cadr expr))))
                    (if ,sym
                      (,(caddr (cadr expr)) ,sym)
                      ,el)))))
        (else
            ;; (cond (e1 ...))
            (expand
              `(if ,(caadr expr)
                  (begin ,@(cdr (cadr expr)))
                  ,el)))))

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
                (expand
                  `(if (memv ,sym (quote ,(car clause)))
                      ,(cons 'begin (cdr clause))
                      #f)))))
        ;; >1 clauses
        (else
          (let ((clause (car clauses)))
            (expand
              `(if (memv ,sym (quote ,(car clause)))
                  ,(expand (cons 'begin (cdr clause)))
                  ,(expand-case-clauses sym (cdr clauses))))))))

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
