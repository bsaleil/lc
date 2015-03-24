;; 1 - TODO CODE
;;   * PB quoted vector avec liste
;;   * Enlever les deux TODO dans ast.scm au moment de la création des deux nouveaux contextes
;;   * MLC qui génere une erreur pour remplacer les (make-lazy-code) avec juste une erreur
;;   * Factoriser lazy-ret de apply et call
;;   * Factoriser gen-fatal et den-dyn
;;   * Si redéfinition de globale, écraser l'ancienne ? (peut briser opti appels lib)
;;   * Bug gc a la lecture du tag 4 (sur compiler.scm)
;;   * Expand multiple corps des lambda et define en begin (fait ?)
;;   * MLC-test peut etre modifié pour éviter d'utiliser trop de lazy-objects
;;   * Test des out of bounds (string-ref "kk" 100), ...
;;   
;;   * Constantes en opérandes
;;   * Ctx info sur les variables libres? (probleme des mutables)

;; 2 - TODO TESTS UNITAIRES
;;   * Heap
;;   * apply
;;   * bind define
;;   * eq? eqv? equal?
;;   * ERROR
;;   * definitions internes
;;   * tests bindings let, let* et letrec avec definitions internes
;;   * corps define begin ?
;;   * print symbol, print eof
;;   * open-input-file, read-char, eof-object ?
;;   * TOUTES fonctions des ports + pp et print ports
;;   * revoir les tests de lib
;;   * Tests unitaires pour les nouvelles primitives inlinées (bcp d'args) (+,-*,...)


;; 3 - TODO ORGANISATION
;;   * Check-err de toutes les primitives
;;   * reorganiser tests unitaires
;;   * Le test boyer passe
;;   * Que donne le expand de (begin expr)


;-------------------------------
;; OPTI : Gérer mutation comme (set! LIB_GLO)
;; OPTI : (define integer? number?) c'est pas géré donc modifier :
    ;; fatal-error, integer?, write quand modifié
;; Workaround : Bug GC avec opti libcall, donc on stocke la fermeture non encodee (coute un inc de plus)
;; Points dans le code :
;; 0 - Associer un still vec à chaque idlib
;; 1 - Mlc-define. Si on reconnait une définition de librairie, on compile la lambda avec un flag
;; 2 - Mlc-lambda. Si on recoit le flag de mlc-define, on saivegarde l'adresse de la fermeture et de la cc table dans le still vector
;; 3 - Mlc-call (X2)
;; 4 - Mlc-set! TODO
;; 5 - GC : maj apres déplacement des fermetures, CC table
;-------------------------------

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

;; TODO : apply write-char (list #\A (open-output-port "out") ne marche pas
;;        car la fonction générée pour write-char a un seul arg. (modifier quand current-output-port implémenté)
;; TODO : pareil pour apply make-vector (modifier quand optional param implémenté, sinon, créer des fonctions alternatives qui sont retournées avec rest en attendant)
;; INTERNED STRING (symbols)
;; TODO : améliorer la detection des mutables : par exemple voir le test ut/Symbols.scm avec lc et gsi !

;; TODO : augmenter l'info de ctx avec eq? (ex. eq? a #t) ... et pour d'autres primitives si on a un id
;; TODO : Voir pourquoi KO avec opt-libcall faux
; (define (foo n)
;   (pp (length n)))

;; Revoir allocation str/sym, taille dans header

; (foo '(1 2 3))

; (define (length n)
;   10)

; (foo '(1 2 3))

; (define (length l)
;   (if ($eq? l '())
;     55
;     (length (cdr l))))

; (define (foo) 10)


;; TODO : pas de gc pour les symboles. Pour que les symboles soient GC :
;; 1 - dans core.scm partie interned symbol, les allouer dans le tas au lieu d'un endroit spécial
;; 2 - dans mem.scm, enlever simplement les symboles de is-special ? (Et vérifier que ok dans scan-field, scan-ref et copy-root)


;(define B 'bonjour)

; (define C "bonjour")
; (define D (string->symbol C))

; (pp A)
; (pp B)
; (pp C)
; (pp D)

; (pp (eq? A B))
; (pp (eq? A D))
; (pp (eq? B D))

; (define interned-symbols (make-table test: eq?))

; (define addr 43524528) ;; TODO

; (define (alloc-symbol sym)
;   ;; TODO (put-i64, ...)
;   (let ((r (+ addr 1))) ;; Encoded
;     (set! addr (+ addr 256))
;     r))
  

; (define (get-symbol-qword sym)
;   (let ((r (table-ref interned-symbols sym #f)))
;     (if r
;        ;; Symbol exists
;        r
;        ;; Symbol does not exist
;        (let ((c (alloc-symbol sym)))
;          (table-set! interned-symbols sym c)
;          c))))

; (pp (get-symbol-qword 'aa))
; (pp (get-symbol-qword 'bb))
; (pp (get-symbol-qword (string->symbol "aa")))

;; TODO : renommer saved-env
;; TODO : enlever les variables mutables du letrec

;; TODO : pourquoi make-string  et d'autres passent par mlc-primitives avec uniquement (car #f)

; (make-vector 10)

; (car #f)
;(write-char #\A (current-output-port))

;; TODO : PROBLEME DE MUTABLE

;;-----
;; MARDI : 
;;   Nombre de pt entree max pour chaque test
;;   Performances contre gsc avec options pour chaque test
;;   Nombre d'appels pour chaque primitive

;; Nettoyer optimisation variables globales

(println (modulo 9 1))