;;; MAZEFUN -- Constructs a maze in a purely functional way,
;;; written by Marc Feeley.

(define foldr
  (lambda (f base lst)

    (define foldr-aux
      (lambda (lst)
        (if (null? lst)
            base
            (f (car lst) (foldr-aux (cdr lst))))))

    (foldr-aux lst)))

(define foldl
  (lambda (f base lst)

    (define foldl-aux
      (lambda (base lst)
        (if (null? lst)
          base
          (foldl-aux (f base (car lst)) (cdr lst)))))

    (foldl-aux base lst)))

(define for
  (lambda (lo hi f)

    (define for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            '())))

    (for-aux lo)))

(define concat
  (lambda (lists)
    (foldr append '() lists)))

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

(define list-remove-pos
  (lambda (lst i)
    (if (= i 0)
        (cdr lst)
        (cons (car lst) (list-remove-pos (cdr lst) (- i 1))))))

(define duplicates?
  (lambda (lst)
    (if (null? lst)
        #f
        (or (member (car lst) (cdr lst))
            (duplicates? (cdr lst))))))

(define make-matrix
  (lambda (n m init)
    (for 0 n (lambda (i) (for 0 m (lambda (j) (init i j)))))))

(define matrix-read
  (lambda (mat i j)
    (list-read (list-read mat i) j)))

(define matrix-write
  (lambda (mat i j val)
    (list-write mat i (list-write (list-read mat i) j val))))

(define matrix-size
  (lambda (mat)
    (cons (length mat) (length (car mat)))))

(define matrix-map
  (lambda (f mat)
    (map (lambda (lst) (map f lst)) mat)))

(define initial-random 0)

(define next-random
  (lambda (current-random)
    (remainder (+ (* current-random 3581) 12751) 131072)))

(define shuffle
  (lambda (lst)
    (shuffle-aux lst initial-random)))

(define shuffle-aux
  (lambda (lst current-random)
    (if (null? lst)
        '()
        (let ((new-random (next-random current-random)))
          (let ((i (modulo new-random (length lst))))
            (cons (list-read lst i)
                  (shuffle-aux (list-remove-pos lst i)
                               new-random)))))))

(define make-maze
  (lambda (n m) ; n and m must be odd
    (if (not (and (odd? n) (odd? m)))
        'error
        (let ((cave
               (make-matrix n m (lambda (i j)
                                  (if (and (even? i) (even? j))
                                      (cons i j)
                                      #f))))
              (possible-holes
               (concat
                (for 0 n (lambda (i)
                           (concat
                            (for 0 m (lambda (j)
                                       (if (equal? (even? i) (even? j))
                                           '()
                                           (list (cons i j)))))))))))
          (pp cave)
          (pp possible-holes)
          (pp (pierce-randomly (shuffle possible-holes) cave))
          (error "K")))))

(define cave-to-maze
  (lambda (cave)
    (matrix-map (lambda (x) (if x '_ '*)) cave)))

(define pierce
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (matrix-write cave i j pos))))

(define pierce-randomly
  (lambda (possible-holes cave)
    (print "*** ") (pp cave)
    (if (null? possible-holes)
        cave
        (let ((hole (car possible-holes)))
          (print "****** ") (pp hole)
          (pierce-randomly (cdr possible-holes)
                           (try-to-pierce hole cave))))))

(define try-to-pierce
  (lambda (pos cave)
    (let ((i (car pos)) (j (cdr pos)))
      (let ((ncs (neighboring-cavities pos cave)))
        (if (duplicates?
             (map (lambda (nc) (matrix-read cave (car nc) (cdr nc))) ncs))
            cave
            (pierce pos
                    (foldl (lambda (c nc) (change-cavity c nc pos))
                           cave
                           ncs)))))))

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

(pp (make-maze 5 5))


;; TODO wip:
;; 1. Ajouter n types à la pile juste avant clo/retaddr
;; 2. Changer le paramètre de départ pour init-env-local
;; 3. Changer le paramètre de départ pour init-slot-loc-local
;; 4. Modifier slot-loc-base
;; 5. Mettre à jour la pile générique quand stack est #f

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
