;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err
;; Test bind define
;; Test appel terminal (+ ajouter if then else comme position terminale, ... ?)
;; Test eq? eqv? equal?
;; Test list-ref
;; Test map
;; Test reverse
;; Test for-each
;; Test ERROR
;; Ajouter port au GC
;; Test open-input-file, read-char, eof-object ?
;; Test print symbol, print eof
;; Test odd even negative positive zero?
;; Test memq, member, remaider!!!
;; +++ Check-err de tout ça
;; PB Registre RCX. Pour le moment, remplacé partout par R15 (A MODIFIER)
;; ATTENTION A LA COPIE DES CTX. A revoir dans patch-closure (et voir les set car set cdr)
;;     + ajouter des fonctions de copie etc de ctx
;;     ctx-stack-set!
;;     avec segfault ctx id
;; Revoir test rest avec des cts existants
;; Enlever les deux TODO dans ast.scm au moment de la création des deux nouveaux contextes

;; Todo : pb quand fonction à un seul arg qui est le rest, et aucun donné
;; Todo : ajouter print port quand input-port? et output-port? sont implémentées

; (define (rch port)
;   (let ((r (read-char port)))
;     (println (eof-object? r))
;     (pp (char? r))
;     (pp r)))

; (let ((f (open-input-file "in")))
;   (println f)
;   (pp (port? f))
;   (println "-----")
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f)
;   (rch f))


;=============================================================================


