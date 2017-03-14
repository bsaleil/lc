

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



; Given a graph, a partial permutation vector, the next input and the next
; output, return 'less, 'equal or 'more depending on the lexicographic
; comparison between the permuted and un-permuted graph.
(define cmp-next-vertex
    (lambda (graph perm x perm-x)
        (let ((from-x
                    (string-ref "KKKKK" x))
                (from-perm-x
                    (string-ref "KKKKK" x)))
            (let _-*-
                ((y
                        0))
                (if (= x y)
                    'equal
                    (let ((uuu (error "TTT"))
                          (x->y?
                                (vector-ref from-x y))
                            (perm-y
                                (vector-ref perm y)))
                        ;;
                        ;;
                        ;;

                        (cond ((eq? x->y?
                                    (vector-ref from-perm-x perm-y))
                                (let ((y->x?
                                            (vector-ref (vector-ref graph y)
                                                x)))
                                    (cond ((eq? y->x?
                                                (vector-ref (vector-ref graph perm-y)
                                                    perm-x))
                                            (_-*- (+ y 1)))
                                        (y->x?
                                            'less)
                                        (else
                                            'more))))
                            (x->y?
                                'less)
                            (else
                                'more))))))))

(define fold-over-rdg
    (lambda ()
      (gambit$$pp (cmp-next-vertex '#(#(#f)) '#() 0 0))))

  (fold-over-rdg)
















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
