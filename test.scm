;/
;abs
;apply
;call/cc
;exact->inexact
;peek-char
;rational?
;truncate

;; Case : tester 

;; Tests bindings let, let* et letrec avec definitions internes
;; Test apply + check-err

;; Set! dans le corps
;; Set! dans les valeurs

;; TODO : corps define begin ?

; (define (foo n m)
;   ($$putchar n)
;   ($$putchar m))

; (foo 55 56)

; (apply (lambda () (println "Test1")) '())

; (apply (lambda (x)
;          (println x))
;        '("Test2"))

; (apply (lambda (x y z)
;          (println x)
;          (println y)
;          (println z))
;        '("Test3" "Test4" "Test5"))

; (apply (lambda (x y . z)
;          (println x)
;          (println y)
;          (pp z))
;        '("Hello" "World" 1 2 3 4 5))

;; Que donne le expand de (begin expr) ?
;; + test.scm de baptHome


;; Expand multiple corps des lambda et define en begin (fait ?)

;; TEST A PASSER :
;; boyer.scm
;; conform.scm
;; earley.scm
;; lattice.scm
;; matrix.scm

;-------------------------------

;TODO : Ajouter schema ordre lazy objects du do dans le wiki
;TODO: change LETRICK name (LETRICK lazy-success lazy-fail) in ast.scm mlc-do



