

(define (foo n)
  (gambit$$pp n))

(foo 1)
(foo 42)









;(pp (##lc-exec-stats (lambda () (fib 40))))


;(let ((r (##lc-time (lambda () (fib 40)))))
;  (pp "R IS")
;  (pp r))



;; CONST VERS HEURISTIQUES:
  ;; * Nb versions cst/non-cst
  ;; * Certains entiers petits / grands
  ;; * Cas spéciaux p.e. map, etc...
  ;; * Versioner si la cst est dans l'appel

;; On peut versionner avec le type null.
;;   -> à la construction d'une liste, la constante est retournée.
;;   -> à chaque cons, on peut conserver dans le contexte le type: liste/length/type (0/1/2)
;; Ex. * fonctionne avec une fonction récursive qui créé une liste, mais une version par appel
;;     * fonctionne avec (list ...), on a l'information tout de suite.
;;     * fonctionne avec (make-list .. ..)
;; * Permet de conserver le type au car
;; * Permet de conserver le type au cdr
;; * Permet d'optimiser (length l)

;; -> On peut faire pareil avec le type vector: conserver le type: vector/length/type
;; ex. avec (make-vector 10 #\R) -> vector/10/char
;; Permet de conserver le type au vector-ref
;; Permet d'éventuellement conserver le type au vector-set
;; Permet d'optimiser (vector-length v)
;; Permet déviter l'utilisation des vecteurs homogènes -> comportement dynamique

;* Full interp versions 5 8 10 15
;* EP only 5 8 10 15
;* RP only 5 8 10 15
;
;--max-versions 5
;--max-versions 5 --enable-const-vers --const-vers-types boo --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types cha --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types clo --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types str --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types vec --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types nul --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types pai --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types int --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types flo --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types voi --enable-cxoverflow-fallback
;--max-versions 5 --enable-const-vers --const-vers-types sym --enable-cxoverflow-fallback


;(define (fib n)
;  (if (< n 2)
;      1
;      (+ (fib (- n 1))
;         (fib (- n 2)))))
;
;(gambit$$pp (fib 30))



;; Versions: ctx -> label
;; EPTable:  stack -> label



;; Vérifier tous les opt-const-vers:

    ;; ast.scm 2
    ;; ctx.scm 9
    ;; OK core.scm 1
    ;; OK main.scm 1
    ;; OK codegen  1


;; Si on atteint la limite du nb de versions:
;; Si c'est un entry un tail, on doit forcément avoir un fallback.
;; 1. Déplacer les arguments dans les bons registres. (rien à faire si pas de cst ni flo)
;; 2. Récupérer un ctx générique, ou le ctx generique du fallback
;; 3. Set le ctx générique (si n'existe pas)
;; 4. Récupérer la version générique du fallback, ou la générer si elle n'existe pas


;; mazefun
;; compiler

;; out.scm 1333

;; 1. Au retour, on regarde le nb de params, et si c'est des csts
;;    Si c'est pas le cas, rien à faire.
;;    Si c'est le cas, on ajoute une entrés dans la table d'assoc fib: cst -> res
;;      avec res la cst si c'est une cst, ou la valeur du registre sinon

;; 2. A chaque compilation d'un appel à fib, si l'association existe,
;;    -> on push simplement la cst

;; WIP:
;; -> Quand on génère un E.P. générique, il faut patcher le pt générique + la fermeture a l'index

;; ->
;; * Merge regalloc
;; * Merge max versions
;; * add bound tests


;; NEXT:
;; * check cc-key
;; * utiliser un systeme pour les globales non mutables compatible avec le nouvel cst vers.
;; * return value (type cr)

;; TODO: quand on récupère l'emplacement d'une variable, regarder les slots pour trouver la meilleure loc (cst > reg > mem)
;; TODO: #<ctx-tclo #3 sym: closure mem-allocated?: #t is-cst: (lambda () ($$atom 1)) cst: #f fn-num: 0>
;;       pourquoi l'ast dans is-cst?
;; TODO: merge de regalloc
;; TODO: merge de version
;; TODO: jitter le alloc-rt pour ne pas générer de code si la taille ne nécessite pas un still
;; TODO: ajouter le support des constantes dans les globales non mutables

;; Liveness: terminer le travail
;; Letrec: attention aux lates !function
;; Liveness: pb sur '() ?
;; Liveness: cas spécial, set-box! est un kill
;; Liveness: alpha conversion
;; Regalloc: pb movs en trop (fib.s)

;; TODO: optimization: pour un pt entrée:
;;       * si on génère un pt entrée dont la 1ere instruction est un jump,
;;       * on peut patcher le pt d'entrée pour sauter directement au bon endroit
