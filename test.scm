(define (equal? x y)
  (cond ((pair? x)
         (and (pair? y) (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
        ((string? x) (and (string? y) (string=? x y)))
        ((vector? x)
         (and (vector? y)
              (eq? (vector-length x) (vector-length y))
              (let loop ((idx (- (vector-length x) 1)))
                (if (< idx 0)
                    #t
                    (and (equal? (vector-ref x idx) (vector-ref y idx))
                         (loop (- idx 1)))))))
        (else (eqv? x y))))

(define (string->list s)
  (let ((len (string-length s)))
    (let loop ((i (- len 1)) (lst '()))
      (if (< i 0) lst (loop (- i 1) (cons (string-ref s i) lst))))))

(define (number->string num)
(define (digit->string d) (make-string 1 (integer->char (+ d 48))))
(define (number->string-h num)
(if (= num 0)
""
(string-append
 (number->string-h (quotient num 10))
 (digit->string (modulo num 10)))))
(cond ((= num 0) "0")
((< num 0) (string-append "-" (number->string-h (* num -1))))
(else (number->string-h num))))

(define (##append-two lst1 lst2)
  (if (null? lst1)
      lst2
      (let ((result (cons (car lst1) '())))
        (let loop ((sentinel result) (src (cdr lst1)))
          (if (null? src)
              (begin (set-cdr! sentinel lst2) result)
              (begin
                (set-cdr! sentinel (cons (car src) '()))
                (loop (cdr sentinel) (cdr src))))))))

;;; CONFORM -- Type checker, written by Jim Miller.

;;; Functional and unstable

(define (sort-list obj pred)

  (define (loop l)
    (if (and (pair? l) (pair? (cdr l)))
        (split-list l '() '())
        l))

  (define (split-list l one two)
    (if (pair? l)
        (split-list (cdr l) two (cons (car l) one))
        (merge (loop one) (loop two))))

  (define (merge one two)
    (cond ((null? one) two)
          ((pred (car two) (car one))
           (cons (car two)
                 (merge (cdr two) one)))
          (else
           (cons (car one)
                 (merge (cdr one) two)))))

  (loop obj))

;; SET OPERATIONS
; (representation as lists with distinct elements)

(define (adjoin element set)
  (if (memq element set) set (cons element set)))

(define (eliminate element set)
  (cond ((null? set) set)
        ((eq? element (car set)) (cdr set))
        (else (cons (car set) (eliminate element (cdr set))))))

(define (intersect list1 list2)
  (let loop ((l list1))
    (cond ((null? l) '())
          ((memq (car l) list2) (cons (car l) (loop (cdr l))))
          (else (loop (cdr l))))))

(define (union list1 list2)
  (if (null? list1)
      list2
      (union (cdr list1)
             (adjoin (car list1) list2))))

;; GRAPH NODES

(define make-internal-node vector)
(define (internal-node-name node) (vector-ref node 0))
(define (internal-node-green-edges node) (vector-ref node 1))
(define (internal-node-red-edges node) (vector-ref node 2))
(define (internal-node-blue-edges node) (vector-ref node 3))
(define (set-internal-node-name! node name) (vector-set! node 0 name))
(define (set-internal-node-green-edges! node edges) (vector-set! node 1 edges))
(define (set-internal-node-red-edges! node edges) (vector-set! node 2 edges))
(define (set-internal-node-blue-edges! node edges) (vector-set! node 3 edges))

(define (make-node name . blue-edges)   ; User's constructor
  (let ((name (if (symbol? name) (symbol->string name) name))
        (blue-edges (if (null? blue-edges) 'NOT-A-NODE-YET (car blue-edges))))
    (make-internal-node name '() '() blue-edges)))

(define (copy-node node)
  (make-internal-node (name node) '() '() (blue-edges node)))

; Selectors

(define name internal-node-name)
(define (make-edge-getter selector)
  (lambda (node)
    (if (or (none-node? node) (any-node? node))
        (fatal-error "Can't get edges from the ANY or NONE nodes")
        (selector node))))
(define red-edges (make-edge-getter internal-node-red-edges))
(define green-edges (make-edge-getter internal-node-green-edges))
(define blue-edges (make-edge-getter internal-node-blue-edges))

; Mutators

(define (make-edge-setter mutator!)
  (lambda (node value)
    (cond ((any-node? node) (fatal-error "Can't set edges from the ANY node"))
          ((none-node? node) 'OK)
          (else (mutator! node value)))))
(define set-red-edges! (make-edge-setter set-internal-node-red-edges!))
(define set-green-edges! (make-edge-setter set-internal-node-green-edges!))
(define set-blue-edges! (make-edge-setter set-internal-node-blue-edges!))

;; BLUE EDGES

(define make-blue-edge vector)
(define (blue-edge-operation edge) (vector-ref edge 0))
(define (blue-edge-arg-node edge) (vector-ref edge 1))
(define (blue-edge-res-node edge) (vector-ref edge 2))
(define (set-blue-edge-operation! edge value) (vector-set! edge 0 value))
(define (set-blue-edge-arg-node! edge value) (vector-set! edge 1 value))
(define (set-blue-edge-res-node! edge value) (vector-set! edge 2 value))

; Selectors
(define operation blue-edge-operation)
(define arg-node blue-edge-arg-node)
(define res-node blue-edge-res-node)

; Mutators
(define set-arg-node! set-blue-edge-arg-node!)
(define set-res-node! set-blue-edge-res-node!)

; Higher level operations on blue edges

(define (lookup-op op node)
  (let loop ((edges (blue-edges node)))
    (cond ((null? edges) '())
          ((eq? op (operation (car edges))) (car edges))
          (else (loop (cdr edges))))))

(define (has-op? op node)
  (not (null? (lookup-op op node))))

;; GRAPHS

(define make-internal-graph vector)
(define (internal-graph-nodes graph) (vector-ref graph 0))
(define (internal-graph-already-met graph) (vector-ref graph 1))
(define (internal-graph-already-joined graph) (vector-ref graph 2))
(define (set-internal-graph-nodes! graph nodes) (vector-set! graph 0 nodes))

; Constructor

(define (make-graph . nodes)
  (make-internal-graph nodes (make-empty-table) (make-empty-table)))

; Selectors

(define graph-nodes internal-graph-nodes)
(define already-met internal-graph-already-met)
(define already-joined internal-graph-already-joined)

; Higher level functions on graphs

(define (add-graph-nodes! graph nodes)
  (set-internal-graph-nodes! graph (cons nodes (graph-nodes graph))))

(define (copy-graph g)
  (define (copy-list l) (vector->list (list->vector l)))
  (make-internal-graph
   (copy-list (graph-nodes g))
   (already-met g)
   (already-joined g)))

(define (clean-graph g)
  (define (clean-node node)
    (if (not (or (any-node? node) (none-node? node)))
        (begin
          (set-green-edges! node '())
          (set-red-edges! node '()))))
  (for-each clean-node (graph-nodes g))
  g)

(define (canonicalize-graph graph classes)
  (define (fix node)
    (define (fix-set object selector mutator)
      (mutator object
               (map (lambda (node)
                      (find-canonical-representative node classes))
                    (selector object))))
    (if (not (or (none-node? node) (any-node? node)))
        (begin
          (fix-set node green-edges set-green-edges!)
          (fix-set node red-edges set-red-edges!)
          (for-each
           (lambda (blue-edge)
             (set-arg-node! blue-edge
                            (find-canonical-representative (arg-node blue-edge) classes))
             (set-res-node! blue-edge
                            (find-canonical-representative (res-node blue-edge) classes)))
           (blue-edges node))))
    node)
  (define (fix-table table)
    (define (canonical? node) (eq? node (find-canonical-representative node classes)))
    (define (filter-and-fix predicate-fn update-fn list)
      (let loop ((list list))
        (cond ((null? list) '())
              ((predicate-fn (car list))
               (cons (update-fn (car list)) (loop (cdr list))))
              (else (loop (cdr list))))))
    (define (fix-line line)
      (filter-and-fix
       (lambda (entry) (canonical? (car entry)))
       (lambda (entry) (cons (car entry)
                             (find-canonical-representative (cdr entry) classes)))
       line))
    (if (null? table)
        '()
        (cons (car table)
              (filter-and-fix
               (lambda (entry) (canonical? (car entry)))
               (lambda (entry) (cons (car entry) (fix-line (cdr entry))))
               (cdr table)))))
  (make-internal-graph
   (map (lambda (class) (fix (car class))) classes)
   (fix-table (already-met graph))
   (fix-table (already-joined graph))))

;; USEFUL NODES

(define none-node (make-node 'none #t))
(define (none-node? node) (eq? node none-node))

(define any-node (make-node 'any '()))
(define (any-node? node) (eq? node any-node))

;; COLORED EDGE TESTS

(define (green-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
        ((none-node? from-node) #t)
        ((memq to-node (green-edges from-node)) #t)
        (else #f)))

(define (red-edge? from-node to-node)
  (cond ((any-node? from-node) #f)
        ((none-node? from-node) #t)
        ((memq to-node (red-edges from-node)) #t)
        (else #f)))

;; SIGNATURE

; Return signature (i.e. <arg, res>) given an operation and a node

(define sig
  (let ((none-comma-any (cons none-node any-node)))
    (lambda (op node)                   ; Returns (arg, res)
      (let ((the-edge (lookup-op op node)))
        (if (not (null? the-edge))
            (cons (arg-node the-edge) (res-node the-edge))
            none-comma-any)))))

; Selectors from signature

(define (arg pair) (car pair))
(define (res pair) (cdr pair))

;; CONFORMITY

(define (conforms? t1 t2)
  (define nodes-with-red-edges-out '())
  (define (add-red-edge! from-node to-node)
    (set-red-edges! from-node (adjoin to-node (red-edges from-node)))
    (set! nodes-with-red-edges-out
          (adjoin from-node nodes-with-red-edges-out)))
  (define (greenify-red-edges! from-node)
    (set-green-edges! from-node
                      (append (red-edges from-node) (green-edges from-node)))
    (set-red-edges! from-node '()))
  (define (delete-red-edges! from-node)
    (set-red-edges! from-node '()))
  (define (does-conform t1 t2)
    (cond ((or (none-node? t1) (any-node? t2)) #t)
          ((or (any-node? t1) (none-node? t2)) #f)
          ((green-edge? t1 t2) #t)
          ((red-edge? t1 t2) #t)
          (else
           (add-red-edge! t1 t2)
           (let loop ((blues (blue-edges t2)))
             (if (null? blues)
                 #t
                 (let* ((current-edge (car blues))
                        (phi (operation current-edge)))
                   (and (has-op? phi t1)
                        (does-conform
                         (res (sig phi t1))
                         (res (sig phi t2)))
                        (does-conform
                         (arg (sig phi t2))
                         (arg (sig phi t1)))
                        (loop (cdr blues)))))))))
  (let ((result (does-conform t1 t2)))
    (for-each (if result greenify-red-edges! delete-red-edges!)
              nodes-with-red-edges-out)
    result))

(define (equivalent? a b)
  (and (conforms? a b) (conforms? b a)))

;; EQUIVALENCE CLASSIFICATION
; Given a list of nodes, return a list of equivalence classes

(define (classify nodes)
  (let node-loop ((classes '())
                  (nodes nodes))
    (if (null? nodes)
        (map (lambda (class)
               (sort-list class
                          (lambda (node1 node2)
                            (< (string-length (name node1))
                               (string-length (name node2))))))
             classes)
        (let ((this-node (car nodes)))
          (define (add-node classes)
            (cond ((null? classes) (list (list this-node)))
                  ((equivalent? this-node (caar classes))
                   (cons (cons this-node (car classes))
                         (cdr classes)))
                  (else (cons (car classes)
                              (add-node (cdr classes))))))
          (node-loop (add-node classes)
                     (cdr nodes))))))

; Given a node N and a classified set of nodes,
; find the canonical member corresponding to N

(define (find-canonical-representative element classification)
  (let loop ((classes classification))
    (cond ((null? classes) (fatal-error "Can't classify" element))
          ((memq element (car classes)) (car (car classes)))
          (else (loop (cdr classes))))))

; Reduce a graph by taking only one member of each equivalence
; class and canonicalizing all outbound pointers

(define (reduce graph)
  (let ((classes (classify (graph-nodes graph))))
    (canonicalize-graph graph classes)))

;; TWO DIMENSIONAL TABLES

(define (make-empty-table) (list 'TABLE))
(define (lookup table x y)
  (let ((one (assq x (cdr table))))
    (if one
        (let ((two (assq y (cdr one))))
          (if two (cdr two) #f))
        #f)))
(define (insert! table x y value)
  (define (make-singleton-table x y)
    (list (cons x y)))
  (let ((one (assq x (cdr table))))
    (if one
        (set-cdr! one (cons (cons y value) (cdr one)))
        (set-cdr! table (cons (cons x (make-singleton-table y value))
                              (cdr table))))))

;; MEET/JOIN
; These update the graph when computing the node for node1*node2

(define (blue-edge-operate arg-fn res-fn graph op sig1 sig2)
  (make-blue-edge op
                  (arg-fn graph (arg sig1) (arg sig2))
                  (res-fn graph (res sig1) (res sig2))))

(define (meet graph node1 node2)
  (cond ((eq? node1 node2) node1)
        ((or (any-node? node1) (any-node? node2)) any-node) ; canonicalize
        ((none-node? node1) node2)
        ((none-node? node2) node1)
        ((lookup (already-met graph) node1 node2)) ; return it if found
        ((conforms? node1 node2) node2)
        ((conforms? node2 node1) node1)
        (else
         (let ((result
                (make-node (string-append "(" (name node1) " ^ " (name node2) ")"))))
           (add-graph-nodes! graph result)
           (insert! (already-met graph) node1 node2 result)
           (set-blue-edges! result
             (map
              (lambda (op)
                (blue-edge-operate join meet graph op (sig op node1) (sig op node2)))
              (intersect (map operation (blue-edges node1))
                         (map operation (blue-edges node2)))))
           result))))

(define (join graph node1 node2)
  (cond ((eq? node1 node2) node1)
        ((any-node? node1) node2)
        ((any-node? node2) node1)
        ((or (none-node? node1) (none-node? node2)) none-node) ; canonicalize
        ((lookup (already-joined graph) node1 node2)) ; return it if found
        ((conforms? node1 node2) node1)
        ((conforms? node2 node1) node2)
        (else
         (let ((result
                (make-node (string-append "(" (name node1) " v " (name node2) ")"))))
           (add-graph-nodes! graph result)
           (insert! (already-joined graph) node1 node2 result)
           (set-blue-edges! result
             (map
              (lambda (op)
                (blue-edge-operate meet join graph op (sig op node1) (sig op node2)))
              (union (map operation (blue-edges node1))
                     (map operation (blue-edges node2)))))
           result))))

;; MAKE A LATTICE FROM A GRAPH

(define (make-lattice g print?)
  (define (step g)
    (let* ((copy (copy-graph g))
           (nodes (graph-nodes copy)))
      (for-each (lambda (first)
                  (for-each (lambda (second)
                              (meet copy first second) (join copy first second))
                            nodes))
                nodes)
      copy))
  (define (loop g count)
    (if print? (display count))
    (let ((lattice (step g)))
      (if print? (begin (display " -> ") (display (length (graph-nodes lattice)))))
      (let* ((new-g (reduce lattice))
             (new-count (length (graph-nodes new-g))))
        (if (= new-count count)
            (begin
              (if print? (newline))
              new-g)
            (begin
              (if print? (begin (display " -> ") (display new-count) (newline)))
              (loop new-g new-count))))))
  (pp "APPLY")

  (let* ((args (adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))
         (rrrr (pp "NA"))
         (graph
         (apply make-graph
                args)))
    (pp "PAPPLY")
    (loop graph (length (graph-nodes graph)))))

;; DEBUG and TEST

(define a '())
(define b '())
(define c '())
(define d '())

(define (setup)
  (set! a (make-node 'a))
  (set! b (make-node 'b))
  (set-blue-edges! a (list (make-blue-edge 'phi any-node b)))
  (set-blue-edges! b (list (make-blue-edge 'phi any-node a)
                           (make-blue-edge 'theta any-node b)))
  (set! c (make-node "c"))
  (set! d (make-node "d"))
  (set-blue-edges! c (list (make-blue-edge 'theta any-node b)))
  (set-blue-edges! d (list (make-blue-edge 'phi any-node c)
                           (make-blue-edge 'theta any-node d)))
  )

(setup)
(make-lattice (make-graph a b c d any-node none-node) #f)


;(define (make-adder n)
;  (lambda (m) (+ n m)))
;
;(define add10 (make-adder 10))
;
;(pp (add10 11))


;; WIP:
;; 1. Propagate continuation when call is inlined
;; 2. optimization when --disable-return-points ?
;;
;; a. continuation jmp optimization with return site patching (generic + specialized)
;; b. continuation inlining

;; c. do *not* allocate slot in cc/cr-table is call/return is inlined


;
;(define (map2 f l1 l2)
;  (if (null? l1)
;      l1
;      (list (cons 'a 5))))
;
;(define (peval proc args)
;    (let ((parms (cadr proc))  ; get the parameter list
;          (body  (caddr proc))) ; get the body of the procedure
;
;      (list 'lambda
;            (beta-subst ; in the body, replace variable refs to the constant
;              body      ; parameters by the corresponding constant
;              (map2 (lambda (x y) (cons x y))
;                    parms
;                    args)))))
;
;(define (assq el lst)
;  (cond ((null? lst) #f)
;        ((eq? el (car (car lst))) (car lst))
;        (else (assq el (cdr lst)))))
;
;(define (beta-subst exp env) ; return a modified 'exp' where each var named in
;  (define (bs exp)           ; 'env' is replaced by the corresponding expr (it
;    (cond
;          ((symbol? exp)
;             (gambit$$pp env)
;             (gambit$$pp exp)
;             (gambit$$pp (assq exp env))
;             (error "K"))
;
;
;          (else
;           (map bs exp))))
;  (if (fixnum? exp)
;      exp
;      (bs (caddr exp))))
;
;
;(define example5
;    '(lambda (a) 11))
;
;(define example6
;  '(lambda ()
;     (let 11
;       fib)))
;
;(peval example5 (list 5))
;(peval example6 '())

;-----

;;------------------------------------------------------------------------------

;(define (foo xp yp x y)
;  (let loop ((c #f) (i 0) (j 0))
;
;    (if (< i 0)
;      0
;      (if (or (> (vector-ref yp 0) y)
;              (>= x (vector-ref xp i)))
;        (loop c (- i 1) i)
;        (loop c (- i 1) i)))))
;
;(let ((xp (vector  1.0))
;      (yp (vector  2.0)))
;  (gambit$$pp (foo xp yp .5 .5)))




;loop sans const:
;main:
;    mov r1, i
;    mov r2, sum
;    cmp r1, r2
;    jge belse
;    mov rr, sum
;    ret
;belse:
;    mov r1, i
;    sub r1, 1
;    mov r2, i
;    add r2, sum
;    call main

;(define (create-n n)
;  (do ((n n (- n 1))
;       (a '() (cons '() a)))
;      ((= n 0) a)))
;
;(define *ll* (create-n 200))
;
;(define (recursive-div2 l)
;  (cond ((null? l) '())
;        (else (cons (car l) (recursive-div2 (cddr l))))))
;
;(pp (length (recursive-div2 (create-n 1000))))



;;-------------------------

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
