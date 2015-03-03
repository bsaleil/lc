;; 1 - TODO CODE
;;   * Ajouter port au GC
;;   * PB quoted vector vide et avec liste
;;   * Enlever les deux TODO dans ast.scm au moment de la création des deux nouveaux contextes
;;   * PB Registre RCX. Pour le moment, remplacé partout par R15 (A MODIFIER)
;;   * CTX : enlever les ctx-stack-set! et autres
;;           + ajouter des fonctions de copie etc de ctx
;;           + segfault ctx id (pb résolu?)
;;           + set-car set-cdr sur les ctx (dans patch-closure?)
;;   * pb quand fonction à un seul arg qui est le rest, et aucun donné
;;   * Uniformiser protocole des fonctions de native.scm (que faire avec $$putchar?)
;;   * Current in/output-port
;;   * Inliner les primitives manquantes
;;   * MLC qui génere une erreur pour remplacer les (make-lazy-code) avec juste une erreur
;;   * Factoriser lazy-ret de apply et call
;;   * Verfier que si (+ a b a), le deuxieme a n'a pas de test de type
;;   * Les tests de type sont toujours faits dans les prédicats (number?, ...) ?
;;   * Si redéfinition de globale, écraser l'ancienne ? (peut briser opti appels lib)
;;   * Bug gc a la lecture du tag 4  (sur compiler.scm)
;;   * Expand multiple corps des lambda et define en begin (fait ?)

;;   * Constantes en opérandes
;;   * Correspondance id <-> stack slot
;;   * Ctx info sur les variables libres? (probleme des mutables)

;; 2 - TODO TESTS UNITAIRES
;;   * Heap
;;   * case
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


(let ((a ((lambda () 10))))
  (+ a 1))
  ;(+ a 2))