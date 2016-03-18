;;reverse
;;nqueens



(define (foo x y z)
  (lambda (a b c d e f g) (- a b x y z)))

(pp ((foo 30 3 4) 2 3 4 5 6 7 8))



;(define (foo a b c d e f g h)
;  (let ((i 1) (j 2) (k 3) (l 4))
;    (+ i j k l)))
;
;(foo 1 2 3 4 5 6 7 8)

;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 1))
;         (fib (- n 2)))))
;
;(fib 20)


;; TODO WIP:

;* quand on créé rctx, il faut aussi élimier les locs supplémentaires et laisser que les orig loc.

;; TODO
;; Comparer le contexte avec le contexte associé à la version générique.
;; Si le contexte diffère seulement sur l'allocation de registre,
;; Faire un merge, et utiliser la version générique existante.
;; Sinon,
;; Comme avant, générer une nouvelle version
;; -> Au moment de générer une version, il faut enregistrer le contexte
;;    associé à la version générique.

;; TODO: vérifier que pour chaque lco, toutes les versions ont une pile de la meme taille (sauf pt entrée)

;; TODO:
;; Pour limiter le nombre de version, il manque le cas ou l'allocation de registre provoque
;; un changement de version. dans ce cas, on doit générer un merge

;; TODO: pour le tri topologique:
;; commencer par la aretes qui ont une destination libre.
;; Puis, terminer par les autres
;; -> permet d'éviter des mouvements inutiles
;; Exemple avec:

;(define (foo n m)
;  (if (< n 0)
;      1
;      (let ((a 1) (b 2) (c 3) (d 4) (e 5) (f 6) (g 7) (h 8))
;        2))
;  (+ n m))
;
;(pp (foo -10 3))
;(pp (foo 100 3))

;; -----------------------------

;; Optimisations:
;; Ne pas utiliser de booléen dans les conditions du if
;;   * Ajouter la fonctionalité pour les (if (number?) ...)
;;   * Reécrire pour les opérateurs de comparaison avec le nouveau code mlc-op-n
;; Détecter les constantes en opérandes des opérateurs
;;   * DONE!
;; Enlever rbp
;;   * DONE!
;; Récupérer directement la valeur de retour dans rax (pareil pour les arguments?)
;;   * TODO
;; Tail call: écraser les valeurs des registres d'argument
;;   * ?

;; Avec les arguments par registres, il manque:
;; Attention, est-ce que les registres sont bien sauvegardés dans mlc-apply ?
;; Tail call

;; FAIT?
;; Au site d'appel, sauvegarder sur la pile les registres associés à des valeurs,
;;   mais PAS ceux des arguments

;(declare (standard-bindings) (block))
;(declare (fixnum) (not safe))

;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 1)) (fib (- n 2)))))
;
;(println (fib 40))

;(print-pos 10)
;(define (foo a b . c)
;  c)
;
;(foo 1 2 3)


;; TODO:
;; REGALLOC: décider si on utilise des registres des opérandes ou non. L'avantage est qu'on a moins de spill.
;;           l'inconvénient est que le code générer de peut pas utiliser le registre dest comme temporaire (ce qu'on fait en ce moment)
;;           donc bcp de code à changer
;; get-free : retourner les mouvements à faire (spill), puis le faire dans le générateur de code
;; voir ou est utilisé (ctx-identifier-loc ...) peut-etre faut-il ajouter #t pour avoir la orig-loc
;;  ((AU SPILL, CONSERVER LES DEUX LOCS))
;;  * meme travail pour gen-get-freevar que gen-get-localvar:
;;    Il faut faire attenton au orig-loc
;;  * au gen set local/free, changer le type
;;  ATTENTION: au set, changer la valeur DANS ORIG POS et enlever les autres locs qui ne sont plus bonnes


;; Graph ok:
;;  -- propagation des points d'entrée + points de retour (no opts)
;;  -- propagation des points d'entrée uniquement (--disable-return-points)


;; TODO: ALLOC DE REGISTRE:
;; - Fonction pour init les registres libres (utilisé 3 ou 4 fois)
;; - Vérifier que le flag ret est bien propagé
;; - Conventio d'appel : Retaddr, Op, arg1, ..., argn
;; - SUPPRIMER la primitive 'list' car macro. Mais, conserver la création de fermeture si mlc-identifier list
;; - RBP inutile? puisqu'on connait le fs...
;; - Vérifier les headers mémoire des vecteurs, et strings + taille au moment de l'allocation
;; - Un registre est assigné à UN slot mémoire
;; - Et vice versa
;; - Par contre, un identifier DOIT pouvoir etre assigné à plusieurs stack slots
;;   (pour le moment assigné à un loc, mais DOIT changer)

;; TODO: AVANT ALLOC DE REGISTRE:
;;  - Vérifier si --max-versions 3 et --max-versions 0 ne dépassent pas le nb de versions
;;    (avec et sans entry/return points)

;; 0 - TODO UTILISATIONS
;;   * (length l)   -> si on sait que l est une liste, on peut simplement PUSH 0
;;   * (equal? a b) -> si on a une info de type sur a et b, on peut spécialiser par eq? voir meme push direct si a et b sont null par ex
;;   * append avec une des listes NULL -> simplement ignorer la liste

;; 1 - TODO CODE
;;   * PB quoted vector avec liste
;;   * Enlever les deux TODO dans ast.scm au moment de la création des deux nouveaux contextes
;;   * MLC qui génere une erreur pour remplacer les (make-lazy-code) avec juste une erreur
;;   * Factoriser lazy-ret de apply et call
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

;; TODO : faire une fonction log-jit comme log-gc (voir mem.scm)
;; TODO : sortir toutes les fonctions utils dans un utils.scm

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

;; TODO : convention pour les noms de fonctions :
;;   lazy-fn représente un block
;;   get-lazy-fn, fonction qui créé un block
;;   gen-... pour la génération du code machine

;; TODO : ATTENTION, pour l'instant l'optimisation du type de retour fait que par ex si
;; la fonction 'length' est appellée, la valeur de retour est un nombre. MAIS, l'opti
;; est bien désactivée si la globale est modifiée (set! length ...) mais pas si elle est
;; redéfinie. ex (define length ...) dans le programme.
;; Il faut donc passer à faux ces redéfinitions dans gids

;; TODO: unit tests fp operations (se baser sur le test comparison.scm)

;(pp (+ 1 (length '(1 2 3))))

; (define (lam-gen z)
;   (lambda (a) (not a)))

; (lam-gen 1)
; (lam-gen #t)
;(lam-gen 2)
;(lam-gen #f)

; (println 10)

; (define (lam a)
; 	(lambda ()
; 		(+ a 10)))

; ((lam 1))
; ((lam 10))

; (define (ppp)
;   (print "#t"))

;(ppp)
;(char=? #\A #\B))

;; TODO : prendre en compte le slot pour le generic addr dans le GC
;; Partager la table de méthodes, mais aussi le stub de lambda ?
;; TODO : ajouter les flonums au GC et autres points (number? real? flonum?)
;; TODO : on peut passer pas mal de fonctions dans lib/num.scm en primitive pour etre inlinees

; (define (fact n)
;    (if (or (= n 0) (= n 1))
;        1
;        (* n (fact (- n 1)))))

; (println (fact 10))

; (define (fibo n)
;    (if (or (= n 0) (= n 1))
;       n
;       (+ (fibo (- n 1)) (fibo (- n 2)))))

; (println (fibo 31))

;(println (number? 10))

;; CAS:
   ;; On a deux lambdas qui suivent la meme table (donc spécialisées avec les meme types unknown)
   ;; La découverte du type de la variable libre est valide pour l'une mais pas pour l'autre

;; Spécialiser les lambda en fonction du type des locales ?


; UNK INT INT INT CHAR STRING INT UNK UNK RET
;
; a:1,2,9
;
; change-type 9 -> INT
;
; 1-retrouver identifier depuis pos -> a
; 2-A chaque pos: mettre le type

;; On découvre le type de l dans (cddr l)
;; Mais on a créé la fermeture avec l:unknown, donc on doit retester le type avec (car l)

;; Allocation sinking:
;; A la création de la fermeture:
;;   - Si aucune variable libre: on propage simplement l'identité, on alloue pas de fermeture
;;   - Si au moins une variable libre mutable: on créé normalement la fermeture
;;   - Si variables libres et aucunes mutables, on alloue en mémoire header, espace cctable, copie des variables libres, mais on n'utilise pas de cc table
;;      -> Tout ca est stocké en mémoire avec un nouveau type closure "non initialisée" T
;;
;;   Cas d'allocation de la fermeture:
;;    - 1: lors d'un appel, un argument est de type T, ou on a que l'identité, on doit initialiser la fermeture
;;    - 2: une variable de type T (ou une id de fonction) devient variable libre
;;    - 3: On perd l'information sur le type d'une variable de type T, ou un id de fonction.


;; Solution pourrie:
;;   On garde l'information: une variable v est utilisée comme variable libre dans la fonction numéro #fn
;;   Lorsqu'on découvre le type de v, on sait qu'elle est utilisée pour #fn.
;;   Alors on récupère la table de #fn pour laquelle f est du type découvert (sinon on la créé)
;;   On patche la fermeture si elle est présente sur la pile avec l'adresse de la nouvelle table

;; Meilleur solution:
;; Allocation sinking, la fermeture n'est pas allouée
;; On va découvrir le type de la variable, la fermeture est allouée que si elle sort du scope
;; Dans l'exemple de recursive-div2-cps, la fermeture sera créée apr!s avoir évalué (cddr l) donc on connaitra le type de l
