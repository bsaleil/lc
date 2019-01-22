;;------------------------------------------------------------------------------
;; Macros

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

(def-macro (FLOATvector-const . lst)   `',(list->vector lst))
(def-macro (FLOATvector? x)            `(vector? ,x))
(def-macro (FLOATvector . lst)         `(vector ,@lst))
(def-macro (FLOATmake-vector n . init) `(make-vector ,n ,@init))
(def-macro (FLOATvector-ref v i)       `(vector-ref ,v ,i))
(def-macro (FLOATvector-set! v i x)    `(vector-set! ,v ,i ,x))
(def-macro (FLOATvector-length v)      `(vector-length ,v))

(def-macro (nuc-const . lst)
`',(list->vector lst))

(def-macro (FLOAT+ . lst) `(+ ,@lst))
(def-macro (FLOAT- . lst) `(- ,@lst))
(def-macro (FLOAT* . lst) `(* ,@lst))
(def-macro (FLOAT/ . lst) `(/ ,@lst))
(def-macro (FLOAT= . lst)  `(= ,@lst))
(def-macro (FLOAT< . lst)  `(< ,@lst))
(def-macro (FLOAT<= . lst) `(<= ,@lst))
(def-macro (FLOAT> . lst)  `(> ,@lst))
(def-macro (FLOAT>= . lst) `(>= ,@lst))
(def-macro (FLOATnegative? . lst) `(negative? ,@lst))
(def-macro (FLOATpositive? . lst) `(positive? ,@lst))
(def-macro (FLOATzero? . lst)     `(zero? ,@lst))
(def-macro (FLOATabs . lst) `(abs ,@lst))
(def-macro (FLOATsin . lst) `(sin ,@lst))
(def-macro (FLOATcos . lst) `(cos ,@lst))
(def-macro (FLOATatan . lst) `(atan ,@lst))
(def-macro (FLOATsqrt . lst) `(sqrt ,@lst))
(def-macro (FLOATmin . lst) `(min ,@lst))
(def-macro (FLOATmax . lst) `(max ,@lst))
(def-macro (FLOATround . lst) `(round ,@lst))
(def-macro (FLOATinexact->exact . lst) `(inexact->exact ,@lst))

(def-macro (GENERIC+ . lst) `(+ ,@lst))
(def-macro (GENERIC- . lst) `(- ,@lst))
(def-macro (GENERIC* . lst) `(* ,@lst))
(def-macro (GENERIC/ . lst) `(/ ,@lst))
(def-macro (GENERICquotient . lst)  `(quotient ,@lst))
(def-macro (GENERICremainder . lst) `(remainder ,@lst))
(def-macro (GENERICmodulo . lst)    `(modulo ,@lst))
(def-macro (GENERIC= . lst)  `(= ,@lst))
(def-macro (GENERIC< . lst)  `(< ,@lst))
(def-macro (GENERIC<= . lst) `(<= ,@lst))
(def-macro (GENERIC> . lst)  `(> ,@lst))
(def-macro (GENERIC>= . lst) `(>= ,@lst))
(def-macro (GENERICexpt . lst) `(expt ,@lst))

;;------------------------------------------------------------------------------
;; Functions used by LC to get time info

(def-macro (##lc-time expr)
  (let ((sym (gensym)))
    `(let ((r (##lc-exec-stats (lambda () ,expr))))
       (##print-perm-string "CPU time: ")
       (##print-double (+ (cdr (assoc "User time" (cdr r)))
                          (cdr (assoc "Sys time" (cdr r)))))
       (##print-perm-string "\n")
       (##print-perm-string "GC CPU time: ")
       (##print-double (+ (cdr (assoc "GC user time" (cdr r)))
                          (cdr (assoc "GC sys time" (cdr r)))))
       (##print-perm-string "\n")

       (map (lambda (el)
              (##print-perm-string (car el))
              (##print-perm-string ": ")
              (##print-double (cdr el))
              (##print-perm-string "\n"))
            (cdr r))
       r)))

(define (##lc-exec-stats thunk)
  (let* ((at-start (##process-statistics))
         (result (thunk))
         (at-end (##process-statistics)))
    (define (get-info msg idx)
      (cons msg
            (- (f64vector-ref at-end idx)
               (f64vector-ref at-start idx))))
    (list
      result
      (get-info "User time" 0)
      (get-info "Sys time" 1)
      (get-info "Real time" 2)
      (get-info "GC user time" 3)
      (get-info "GC sys time" 4)
      (get-info "GC real time" 5)
      (get-info "Nb gcs" 6))))

;;------------------------------------------------------------------------------

(define (run-bench name count ok? run)
  (let loop ((i count) (result '(undefined)))
    (if (< 0 i)
      (loop (- i 1) (run))
      result)))

(define (run-benchmark name count ok? run-maker . args)
  (let ((run (apply run-maker args)))
    (let ((result (car (##lc-time (run-bench name count ok? run)))))
      (if (not (ok? result))
        (begin
          (display "*** wrong result ***")
          (newline)
          (display "*** got: ")
          (write result)
          (newline))))))

; Gabriel benchmarks
(define boyer-iters        20)
(define browse-iters      600)
(define cpstak-iters     1000)
(define ctak-iters        100)
(define dderiv-iters  2000000)
(define deriv-iters   2000000)
(define destruc-iters     500)
(define diviter-iters 1000000)
(define divrec-iters  1000000)
(define puzzle-iters      100)
(define tak-iters        2000)
(define takl-iters        300)
(define trav1-iters       100)
(define trav2-iters        20)
(define triangl-iters      10)

; Kernighan and Van Wyk benchmarks
(define ack-iters          10)
(define array1-iters        1)
(define cat-iters           1)
(define string-iters       10)
(define sum1-iters         10)
(define sumloop-iters      10)
(define tail-iters          1)
(define wc-iters            1)

; C benchmarks
(define fft-iters        2000)
(define fib-iters           5)
(define fibfp-iters         2)
(define mbrot-iters       100)
(define nucleic-iters       5)
(define pnpoly-iters   100000)
(define sum-iters       20000)
(define sumfp-iters     20000)
(define tfib-iters         20)

; Other benchmarks
(define conform-iters      40)
(define dynamic-iters      20)
(define earley-iters      200)
(define fibc-iters        500)
(define graphs-iters      300)
(define lattice-iters       1)
(define matrix-iters      400)
(define maze-iters       4000)
(define mazefun-iters    1000)
(define nqueens-iters    2000)
(define paraffins-iters  1000)
(define peval-iters       200)
(define pi-iters            2)
(define primes-iters   100000)
(define ray-iters           5)
(define scheme-iters    20000)
(define simplex-iters  100000)
(define slatex-iters       20)
(define perm9-iters        10)
(define nboyer-iters      100)
(define sboyer-iters      100)
(define gcbench-iters       1)
(define compiler-iters    300)
(define nbody-iters         1)
(define fftrad4-iters       4)

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
  (let ((graph
         (apply make-graph
                (adjoin any-node (adjoin none-node (graph-nodes (clean-graph g)))))))
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
  '(made a b c d))

(define (test)
  (setup)
  (map name
       (graph-nodes (make-lattice (make-graph a b c d any-node none-node) #f))))

(define (main . args)
  (run-benchmark
    "conform"
    conform-iters
    (lambda (result)
      (equal? (map (lambda (s)
                     (list->string (map char-downcase (string->list s))))
                   result)
              '("(((b v d) ^ a) v c)"
                "(c ^ d)"
                "(b v (a ^ d))"
                "((a v d) ^ b)"
                "(b v d)"
                "(b ^ (a v c))"
                "(a v (c ^ d))"
                "((b v d) ^ a)"
                "(c v (a v d))"
                "(a v c)"
                "(d v (b ^ (a v c)))"
                "(d ^ (a v c))"
                "((a ^ d) v c)"
                "((a ^ b) v d)"
                "(((a v d) ^ b) v (a ^ d))"
                "(b ^ d)"
                "(b v (a v d))"
                "(a ^ c)"
                "(b ^ (c v d))"
                "(a ^ b)"
                "(a v b)"
                "((a ^ d) ^ b)"
                "(a ^ d)"
                "(a v d)"
                "d"
                "(c v d)"
                "a"
                "b"
                "c"
                "any"
                "none")))
    (lambda () (lambda () (test)))))

(main)
