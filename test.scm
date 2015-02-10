;; Heap test
;; Test definition interne
;; PB quoted vector vide et avec liste
;; Test set-car, set-cdr + Check err
;; Test bind define
;; Test appel terminal (+ ajouter if then else comme position terminale, ... ?)
;; Test eq? eqv? equal?
;; Test ERROR
;; Ajouter port au GC
;; Test open-input-file, read-char, eof-object ?
;; Test print symbol, print eof
;; Test odd even negative positive zero?
;; Test remaider
;; +++ Check-err de tout ça
;; PB Registre RCX. Pour le moment, remplacé partout par R15 (A MODIFIER)
;; ATTENTION A LA COPIE DES CTX. A revoir dans patch-closure (et voir les set car set cdr)
;;     + ajouter des fonctions de copie etc de ctx
;;     ctx-stack-set!
;;     avec segfault ctx id
;; Revoir test rest avec des ctx existants
;; Enlever les deux TODO dans ast.scm au moment de la création des deux nouveaux contextes
;; Test cond avec (e1 e2 e3 ...) et (else e1 e2 e3 ...)
;; Test TOUTES fonctions des ports + pp et print ports
;; TODO : uniformiser protocole des fonctions de native.scm (aussi enlever $$putchar pour write-char?)
;; Current in/output-port
;; TODO : revoir les tests de lib
;; Tests :
;;   macros (case)
;;   apply
;;   Float
;;   bitwise-and/not
;;   bignums
;;   call-with-current-continuation

;; Todo : pb quand fonction à un seul arg qui est le rest, et aucun donné


;=============================================================================



; (define (foo n)
;   (if (= n 0)
;     #t
;     (begin (list n 10 20 30 40)
;            (foo (- n 1)))))

; (foo 1000000)




(define a "a")
(define b "b")
(define c "ab")

(define (try p1 p2) (println (string<? p1 p2)))

(try a a)
(try a b)
(try a c)

(try b a)
(try b b)
(try b c)

(try c a)
(try c b)
(try c c)