

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



(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))



(define list-read
  (lambda (lst i)
    (if (= i 0)
        (car lst)
        (list-read (cdr lst) (- i 1)))))

(define list-write
  (lambda (lst i val)
    (if (= i 0)
        (cons val (cdr lst))
        (cons (car lst) (list-write (cdr lst) (- i 1) val)))))

(define matrix-read
  (lambda (mat i j)
    (list-read (list-read mat i) j)))

(define matrix-write
  (lambda (mat i j val)
    (list-write mat i (list-write (list-read mat i) j val))))

(define matrix-size
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(define change-cavity
  (lambda (cave pos new-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (change-cavity-aux cave pos new-cavity-id (matrix-read cave i j)))))

(define change-cavity-aux
  (lambda (cave pos new-cavity-id old-cavity-id)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((cavity-id (matrix-read cave i j)))
        (if (equal? cavity-id old-cavity-id)
            (foldl (lambda (c nc)
                     (change-cavity-aux c nc new-cavity-id old-cavity-id))
                   (matrix-write cave i j new-cavity-id)
                   (neighboring-cavities pos cave))
            cave)))))

(define neighboring-cavities
  (lambda (pos cave)
    (let ((size (matrix-size cave)))
      (let ((n (car size)) (m (cdr size)))
        (let ((i (car pos)) (j (cdr pos)))
          (append (if (and (> i 0) (matrix-read cave (- i 1) j))
                      (list (cons (- i 1) j))
                      '())
                  (if (and (< i (- n 1)) (matrix-read cave (+ i 1) j))
                      (list (cons (+ i 1) j))
                      '())
                  (if (and (> j 0) (matrix-read cave i (- j 1)))
                      (list (cons i (- j 1)))
                      '())
                  (if (and (< j (- m 1)) (matrix-read cave i (+ j 1)))
                      (list (cons i (+ j 1)))
                      '())))))))


  (let* ((pos  '(3 . 2))
         (ncs  '((2 . 2) (4 . 2)))
         (cave '(((0 . 0) #f (1 . 2) #f (0 . 4))
                   (#f #f (1 . 2) #f #f)
                   ((4 . 1) #f (1 . 2) #f (2 . 4))
                   ((4 . 1) #f #f #f #f)
                   ((4 . 1) (4 . 1) (4 . 1) #f (4 . 4)))))
   (foldl (lambda (c nc) (change-cavity c nc pos))
          cave
          ncs)
   (error "eK"))








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
