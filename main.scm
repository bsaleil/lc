;; File: lazy-comp2.scm

;; June 16, 2014

;;-----------------------------------------------------------------------------

(define extremely-lazy? #f)

;;-----------------------------------------------------------------------------

(define closures-refs '())
(define closure-current 1)

(define code-vector-size 100)

(define code-vector #f)
(define code-alloc #f)
(define stub-alloc #f)
(define stub-freelist #f)

(define (init-code-allocator)
  (set! code-vector (make-vector code-vector-size '()))
  (set! code-alloc 0)
  (set! stub-alloc code-vector-size)
  (set! stub-freelist -1))

(define (code-add instr)
  (vector-set! code-vector code-alloc instr)
  (set! code-alloc (+ code-alloc 1))
  (if (> code-alloc stub-alloc)
      (error "code vector overflow while allocating code")))

(define (stub-add stub)
  (let ((loc
         (if (= stub-freelist -1)

             (let ((loc (- stub-alloc 1)))
               (set! stub-alloc loc)
               (if (> code-alloc stub-alloc)
                   (error "code vector overflow while allocating stub"))
               loc)

             (let ((loc stub-freelist))
               (set! stub-freelist (vector-ref code-vector loc))
               loc))))

    (vector-set! code-vector loc stub)
    loc))

(define (stub-reclaim loc)
  (vector-set! code-vector loc stub-freelist)
  (set! stub-freelist loc))

(define (pp-code-vector current-loc)

  (println "--------------------------------- code vector")

  (let loop ((loc 0))
    (if (< loc code-alloc)
        (begin
          (if (= loc current-loc) (print ">"))
          (print loc ": ")
          (pretty-print (vector-ref code-vector loc))
          (loop (+ loc 1)))))

  (let loop ((loc stub-alloc))
    (if (< loc (vector-length code-vector))
        (let ((stub (vector-ref code-vector loc)))
          (if (pair? stub)
              (begin
                (if (= loc current-loc) (print ">"))
                (print loc ": ")
                (pretty-print stub)))
          (loop (+ loc 1))))))

;;-----------------------------------------------------------------------------

(define stack '())

(define (init-code-executor)
  (set! stack '()))

(define (exec loc)

  (pp-code-vector loc) ;; for debugging

  (let loop ((loc loc))
    (let ((instr (vector-ref code-vector loc)))

      (case (car instr)

        ((ret)
         (car stack)) ;; return value on stack

        ((nop)
         (loop (+ loc 1)))

        ((push-lit)
         (set! stack (cons (cadr instr) stack))
         (loop (+ loc 1)))

        ((push-local)
         (set! stack (cons (list-ref stack (cadr instr)) stack))
         (loop (+ loc 1)))

        ((add)
         (set! stack (cons (+ (cadr stack) (car stack)) (cddr stack)))
         (loop (+ loc 1)))

        ((sub)
         (set! stack (cons (- (cadr stack) (car stack)) (cddr stack)))
         (loop (+ loc 1)))

        ((mul)
         (set! stack (cons (* (cadr stack) (car stack)) (cddr stack)))
         (loop (+ loc 1)))

        ((lt)
         (set! stack (cons (< (cadr stack) (car stack)) (cddr stack)))
         (loop (+ loc 1)))

        ((eq)
         (set! stack (cons (= (cadr stack) (car stack)) (cddr stack)))
         (loop (+ loc 1)))

        ((jump)
         (loop (cadr instr)))

        ((jump-if-false)
         (let ((test (car stack)))
           (set! stack (cdr stack))
           (loop (if (eq? test #f) (cadr instr) (+ loc 1)))))

        ((jump-if-not-false)
         (let ((test (car stack)))
           (set! stack (cdr stack))
           (loop (if (eq? test #f) (+ loc 1) (cadr instr)))))

        ; TODO
        ((call)
          (let ((closure (car stack)))
                   (set! stack (cdr stack)) ;; POP
                   (let* ((dest (cadr closure))
                          (res (exec dest)))
                          (loop (+ loc 1)))))

        ((gen-version)
         ;; This instruction is always in a stub.
         ;; It calls the code generator to generate a specific version
         ;; if one hasn't been generated yet.
         (stub-reclaim loc)
         (exec (gen-version (list-ref instr 1)
                            (list-ref instr 2)
                            (list-ref instr 3))))

        (else
         (error "unknown instruction" instr))))))

;;-----------------------------------------------------------------------------

(define-type lazy-code
  constructor: make-lazy-code*
  generator
  versions
)

(define (make-lazy-code generator)
  (make-lazy-code* generator (make-table)))

(define (get-version lazy-code ctx)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-ref versions ctx #f)))

(define (put-version lazy-code ctx v)
  (let ((versions (lazy-code-versions lazy-code)))
    (table-set! versions ctx v)))

(define-type ctx
  stack ;; compile time stack, containing types
)

(define (ctx-push ctx type)
  (make-ctx (cons type (ctx-stack ctx))))

(define (ctx-pop ctx)
  (make-ctx (cdr (ctx-stack ctx))))

(define (patch-jump jump-loc dest-loc)
      (if (< jump-loc code-alloc)
          (set-car! (cdr (vector-ref code-vector jump-loc)) dest-loc)
          (vector-set! code-vector jump-loc (list 'jump dest-loc))))

(define (patch-closure closure new-dest)
  (set-car! (cdr closure) new-dest))

(define (jump-to-version lazy-code ctx)
  (if extremely-lazy?

      (let ((stub-loc (gen-version-stub-add code-alloc lazy-code ctx)))
        (code-add (list 'jump stub-loc))
        ;;(code-add '(nop)) ;; uncomment to ruin the fall-through optimization
        )

      (gen-version code-alloc lazy-code ctx)))

(define (gen-version-stub-add jump-loc lazy-code ctx)
  (stub-add (list 'gen-version jump-loc lazy-code ctx)))

(define (gen-version jump-loc lazy-code ctx)
  (let ((dest-loc (get-version lazy-code ctx)))
    (if dest-loc
        (begin
          (patch-jump jump-loc dest-loc)
          (if (= jump-loc code-alloc)
              (set! code-alloc (+ code-alloc 1)))
          dest-loc)

        (begin
         
          (cond ((and (list? jump-loc) (eq? (car jump-loc) 'closure)) (patch-closure jump-loc code-alloc))
                ((and (= jump-loc (- code-alloc 1))
                      (eq? (car (vector-ref code-vector jump-loc))
                           'jump))
                 (set! code-alloc jump-loc)) ;; fall-through optimization

                ((and (= jump-loc (- code-alloc 2))
                      (eq? (car (vector-ref code-vector jump-loc))
                           'jump-if-false)
                      (eq? (car (vector-ref code-vector (+ jump-loc 1)))
                           'jump))
                 (let ((dest
                        (cadr (vector-ref code-vector (+ jump-loc 1)))))
                   ;; swap conditional jump destinations
                   ;; and point gen-version to correct jump
                   (if (eq? (car (vector-ref code-vector dest))
                            'gen-version)
                       (set-car! (cdr (vector-ref code-vector dest))
                                 jump-loc))
                   (vector-set! code-vector
                                jump-loc
                                (list 'jump-if-not-false dest))
                   (set! code-alloc (+ jump-loc 1)))) ;; fall-through optimization

                (else
                  (patch-jump jump-loc code-alloc)))

          (let ((version-loc code-alloc))
            (put-version lazy-code ctx version-loc)
            ((lazy-code-generator lazy-code) ctx)
            version-loc)))))

(define (gen-ast ast succ)

  (cond ((or (number? ast) (boolean? ast))
         (make-lazy-code
          (lambda (ctx)
            (code-add (list 'push-lit ast))
            (jump-to-version succ
                             (ctx-push ctx
                                       (cond ((number? ast) 'num)
                                             ((boolean? ast) 'bool)))))))

        ((symbol? ast) ;; a, b, c refer to the arguments of the function
         (make-lazy-code
          (lambda (ctx)
            (let* ((fs (length (ctx-stack ctx)))
                   (pos (- fs 1 (cdr (assoc ast '((a . 0) (b . 1) (c . 2)))))))
              (code-add (list 'push-local pos))
              (jump-to-version succ
                               (ctx-push ctx (list-ref (ctx-stack ctx) pos)))))))

        ((pair? ast)
         (let ((op (car ast)))

           (cond ((member op '(+ - * < =))
                  (let* ((lazy-code2
                          (make-lazy-code
                           (lambda (ctx)
                             (code-add
                              (case op
                                ((+) '(add))
                                ((-) '(sub))
                                ((*) '(mul))
                                ((<) '(lt))
                                ((=) '(eq))
                                (else (error "unknown op" op))))
                             (jump-to-version succ
                                              (ctx-push
                                               (ctx-pop (ctx-pop ctx))
                                               (cond ((member op '(+ - *)) 'num)
                                                     ((member op '(< =)) 'bool)))))))
                         (lazy-code1
                          (gen-ast
                           (caddr ast)
                           lazy-code2)))
                    (gen-ast
                     (cadr ast)
                     lazy-code1)))

                 ((eq? op 'if)
                  (let* ((lazy-code2
                          (gen-ast (cadddr ast) succ))
                         (lazy-code1
                          (gen-ast (caddr ast) succ))
                         (lazy-code0
                          (make-lazy-code
                           (lambda (ctx)
                             (let* ((ctx2
                                     (ctx-pop ctx))
                                    (stub2-loc
                                     (gen-version-stub-add
                                      code-alloc
                                      lazy-code2
                                      ctx2)))
                               (code-add (list 'jump-if-false stub2-loc))
                               (let* ((ctx1
                                       ctx2)
                                      (stub1-loc
                                       (gen-version-stub-add
                                        code-alloc
                                        lazy-code1
                                        ctx1)))
                                 (code-add (list 'jump stub1-loc))))))))
                    (gen-ast
                     (cadr ast)
                     lazy-code0)))

                 
                 ((eq? op 'lambda)

                    (let* ((lazy-code-fn (gen-ast (caddr ast) (make-lazy-code (lambda (ctx) (code-add '(ret))))))
                          (closure (make-closure -1 '()))
                          (stub (gen-version-stub-add closure lazy-code-fn (make-ctx '()))))

                        (set-car! (cdr closure) stub)

                        (make-lazy-code (lambda (ctx)
                            (code-add (list 'push-lit closure))
                            (jump-to-version succ (ctx-push ctx 'closure))))))

                 ;; TODO
                 ;; fn call
                 (else (let ((lazy-code-call ;; Call continuation
                                  (make-lazy-code 
                                      (lambda (ctx)
                                        (code-add '(call))
                                        (jump-to-version succ (ctx-push ctx 'unknown))))))

                                (gen-ast (car ast) lazy-code-call)))))) ;; Gen fn AST

        (else
         (error "unknown ast" ast))))

(define (make-closure dest free-vars)
  (cons 'closure (cons dest free-vars)))

;;-----------------------------------------------------------------------------

(define (compile-lambda params expr)

  (init-code-allocator)
  (init-code-executor)

  (let ((lazy-code
         (gen-ast expr
                  (make-lazy-code
                   (lambda (ctx)
                     (code-add '(ret)))))))
    (gen-version code-alloc
                 lazy-code
                 (make-ctx (map (lambda (x) 'unknown) params)))
    (lambda args
      (set! stack args)
      (exec 0))))

(define (test expr)
  (println "******************************************************************")
  (print "*** TESTING: ")
  (pretty-print (list 'define 'f (list 'lambda '(a b) expr)))
  (let ((f (compile-lambda '(a b) expr)))

    (define (t . args)
      (let ((result (apply f (reverse args))))
        (print "*** RESULT: ")
        (pretty-print (list (cons 'f args) '=> result))))

    (t 1 2)
    (t 2 1)))

;(test '(if #t 11 22))

;(test 1)
;(test '(1 2 3))

(test '(+ 100 ((lambda () (+ 10 20)))))
;(test '(+ 900 ((lambda () (+ 10 20)))))

;(test '(- 11 (if #t 22 33)))

;(test '(- (if (< a b) 11 22) (if (< b a) 33 44)))

;;-----------------------------------------------------------------------------
