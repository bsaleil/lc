;;; GRAPHS -- Obtained from Andrew Wright.

;;; ==== util.ss ====


; Fold over list elements, associating to the left.
(define fold
    (lambda (lst folder state)
;        (assert (list? lst)
;            lst)
;        (assert (procedure? folder)
;            folder)
        (do ((lst lst
                    (cdr lst))
                (state state
                    (folder (car lst)
                        state)))
            ((null? lst)
                state))))

; Given the size of a vector and a procedure which
; sends indicies to desired vector elements, create
; and return the vector.
(define proc->vector
  (lambda (size f)
;    (assert (and (integer? size)
;                 (exact? size)
;                 (>= size 0))
;      size)
;    (assert (procedure? f)
;      f)
    (if (zero? size)
        (vector)
        (let ((x (make-vector size (f 0))))
          (let loop ((i 1))
            (if (< i size)
              (begin
                (vector-set! x i (f i))
                (loop (+ i 1)))))
          x))))

(define vector-fold
    (lambda (vec folder state)
;        (assert (vector? vec)
;            vec)
;        (assert (procedure? folder)
;            folder)
        (let ((len
                    (vector-length vec)))
            (do ((i 0
                        (+ i 1))
                    (state state
                        (folder (vector-ref vec i)
                            state)))
                ((= i len)
                    state)))))

(define vector-map
    (lambda (vec proc)
        (proc->vector (vector-length vec)
            (lambda (i)
                (proc (vector-ref vec i))))))

; Given limit, return the list 0, 1, ..., limit-1.
(define giota
    (lambda (limit)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
        (let _-*-
            ((limit
                    limit)
                (res
                    '()))
            (if (zero? limit)
                res
                (let ((limit
                            (- limit 1)))
                    (_-*- limit
                        (cons limit res)))))))

; Fold over the integers [0, limit).
(define gnatural-fold
    (lambda (limit folder state)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? folder)
;            folder)
        (do ((i 0
                    (+ i 1))
                (state state
                    (folder i state)))
            ((= i limit)
                state))))

; Iterate over the integers [0, limit).
(define gnatural-for-each
    (lambda (limit proc!)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? proc!)
;            proc!)
        (do ((i 0
                    (+ i 1)))
            ((= i limit))
            (proc! i))))

(define natural-for-all?
    (lambda (limit ok?)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? ok?)
;            ok?)
        (let _-*-
            ((i 0))
            (or (= i limit)
                (and (ok? i)
                    (_-*- (+ i 1)))))))

(define natural-there-exists?
    (lambda (limit ok?)
;        (assert (and (integer? limit)
;                (exact? limit)
;                (>= limit 0))
;            limit)
;        (assert (procedure? ok?)
;            ok?)
        (let _-*-
            ((i 0))
            (and (not (= i limit))
                (or (ok? i)
                    (_-*- (+ i 1)))))))

(define there-exists?
    (lambda (lst ok?)
;        (assert (list? lst)
;            lst)
;        (assert (procedure? ok?)
;            ok?)
        (let _-*-
            ((lst lst))
            (and (not (null? lst))
                (or (ok? (car lst))
                    (_-*- (cdr lst)))))))


;;; ==== ptfold.ss ====


; Fold over the tree of permutations of a universe.
; Each branch (from the root) is a permutation of universe.
; Each node at depth d corresponds to all permutations which pick the
; elements spelled out on the branch from the root to that node as
; the first d elements.
; Their are two components to the state:
;       The b-state is only a function of the branch from the root.
;       The t-state is a function of all nodes seen so far.
; At each node, b-folder is called via
;       (b-folder elem b-state t-state deeper accross)
; where elem is the next element of the universe picked.
; If b-folder can determine the result of the total tree fold at this stage,
; it should simply return the result.
; If b-folder can determine the result of folding over the sub-tree
; rooted at the resulting node, it should call accross via
;       (accross new-t-state)
; where new-t-state is that result.
; Otherwise, b-folder should call deeper via
;       (deeper new-b-state new-t-state)
; where new-b-state is the b-state for the new node and new-t-state is
; the new folded t-state.
; At the leaves of the tree, t-folder is called via
;       (t-folder b-state t-state accross)
; If t-folder can determine the result of the total tree fold at this stage,
; it should simply return that result.
; If not, it should call accross via
;       (accross new-t-state)
; Note, fold-over-perm-tree always calls b-folder in depth-first order.
; I.e., when b-folder is called at depth d, the branch leading to that
; node is the most recent calls to b-folder at all the depths less than d.
; This is a gross efficiency hack so that b-folder can use mutation to
; keep the current branch.
(define fold-over-perm-tree
    (lambda (universe b-folder b-state t-folder t-state)
;        (assert (list? universe)
;            universe)
;        (assert (procedure? b-folder)
;            b-folder)
;        (assert (procedure? t-folder)
;            t-folder)
        (let _-*-
            ((universe
                    universe)
                (b-state
                    b-state)
                (t-state
                    t-state)
                (accross
                    (lambda (final-t-state)
                        final-t-state)))
            (if (null? universe)
                (t-folder b-state t-state accross)
                (let _-**-
                    ((in
                            universe)
                        (out
                            '())
                        (t-state
                            t-state))
                    (let* ((first
                                (car in))
                            (rest
                                (cdr in))
                            (accross
                                (if (null? rest)
                                    accross
                                    (lambda (new-t-state)
                                        (_-**- rest
                                            (cons first out)
                                            new-t-state)))))
                        (b-folder first
                            b-state
                            t-state
                            (lambda (new-b-state new-t-state)
                                (_-*- (fold out cons rest)
                                    new-b-state
                                    new-t-state
                                    accross))
                            accross)))))))


;;; ==== minimal.ss ====


; A directed graph is stored as a connection matrix (vector-of-vectors)
; where the first index is the `from' vertex and the second is the `to'
; vertex.  Each entry is a bool indicating if the edge exists.
; The diagonal of the matrix is never examined.
; Make-minimal? returns a procedure which tests if a labelling
; of the verticies is such that the matrix is minimal.
; If it is, then the procedure returns the result of folding over
; the elements of the automoriphism group.  If not, it returns #f.
; The folding is done by calling folder via
;       (folder perm state accross)
; If the folder wants to continue, it should call accross via
;       (accross new-state)
; If it just wants the entire minimal? procedure to return something,
; it should return that.
; The ordering used is lexicographic (with #t > #f) and entries
; are examined in the following order:
;       1->0, 0->1
;
;       2->0, 0->2
;       2->1, 1->2
;
;       3->0, 0->3
;       3->1, 1->3
;       3->2, 2->3
;       ...
(define make-minimal?
    (lambda (max-size)
;        (assert (and (integer? max-size)
;                (exact? max-size)
;                (>= max-size 0))
;            max-size)
        (let ((iotas
                    (proc->vector (+ max-size 1)
                        giota))
                (perm
                    (make-vector max-size 0)))
            (lambda (size graph folder state)
;                (assert (and (integer? size)
;                        (exact? size)
;                        (<= 0 size max-size))
;                    size
;                    max-size)
;                (assert (vector? graph)
;                    graph)
;                (assert (procedure? folder)
;                    folder)
                (fold-over-perm-tree (vector-ref iotas size)
                    (lambda (perm-x x state deeper accross)
                        (case (cmp-next-vertex graph perm x perm-x)
                            ((less)
                                #f)
                            ((equal)
                                (vector-set! perm x perm-x)
                                (deeper (+ x 1)
                                    state))
                            ((more)
                                (accross state))
                            (else
;                                (assert #f)
                                (fatal-error "???"))))
                    0
                    (lambda (leaf-depth state accross)
;                        (assert (eqv? leaf-depth size)
;                            leaf-depth
;                            size)
                        (folder perm state accross))
                    state)))))

; Given a graph, a partial permutation vector, the next input and the next
; output, return 'less, 'equal or 'more depending on the lexicographic
; comparison between the permuted and un-permuted graph.
(define cmp-next-vertex
    (lambda (graph perm x perm-x)
        (let ((from-x
                    (vector-ref graph x))
                (from-perm-x
                    (vector-ref graph perm-x)))
            (let _-*-
                ((y
                        0))
                (if (= x y)
                    'equal
                    (let ((x->y?
                                (vector-ref from-x y))
                            (perm-y
                                (vector-ref perm y)))
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


;;; ==== rdg.ss ====


; Fold over rooted directed graphs with bounded out-degree.
; Size is the number of verticies (including the root).  Max-out is the
; maximum out-degree for any vertex.  Folder is called via
;       (folder edges state)
; where edges is a list of length size.  The ith element of the list is
; a list of the verticies j for which there is an edge from i to j.
; The last vertex is the root.
(define fold-over-rdg
    (lambda (size max-out folder state)
;        (assert (and (exact? size)
;                (integer? size)
;                (> size 0))
;            size)
;        (assert (and (exact? max-out)
;                (integer? max-out)
;                (>= max-out 0))
;            max-out)
;        (assert (procedure? folder)
;            folder)
        (let* ((root
                    (- size 1))
                (edge?
                    (proc->vector size
                        (lambda (from)
                            (make-vector size #f))))
                (edges
                    (make-vector size '()))
                (out-degrees
                    (make-vector size 0))
                (minimal-folder
                    (make-minimal? root))
                (non-root-minimal?
                    (let ((cont
                                (lambda (perm state accross)
;                                    (assert (eq? state #t)
;                                        state)
                                    (accross #t))))
                        (lambda (size)
                            (minimal-folder size
                                edge?
                                cont
                                #t))))
                (root-minimal?
                    (let ((cont
                                (lambda (perm state accross)
;                                    (assert (eq? state #t)
;                                        state)
                                    (case (cmp-next-vertex edge? perm root root)
                                        ((less)
                                            #f)
                                        ((equal more)
                                            (accross #t))
                                        (else
;                                            (assert #f)
                                            (fatal-error "???"))))))
                        (lambda ()
                            (minimal-folder root
                                edge?
                                cont
                                #t)))))
            (let _-*-
                ((vertex
                        0)
                    (state
                        state))
                (cond ((not (non-root-minimal? vertex))
                        state)
                    ((= vertex root)
;                        (assert
;                            (begin
;                                (gnatural-for-each root
;                                    (lambda (v)
;                                        (assert (= (vector-ref out-degrees v)
;                                                (length (vector-ref edges v)))
;                                            v
;                                            (vector-ref out-degrees v)
;                                            (vector-ref edges v))))
;                                #t))
                        (let ((reach?
                                    (make-reach? root edges))
                                (from-root
                                    (vector-ref edge? root)))
                            (let _-*-
                                ((v
                                        0)
                                    (outs
                                        0)
                                    (efr
                                        '())
                                    (efrr
                                        '())
                                    (state
                                        state))
                                (cond ((not (or (= v root)
                                                (= outs max-out)))
                                        (vector-set! from-root v #t)
                                        (let ((state
                                                    (_-*- (+ v 1)
                                                        (+ outs 1)
                                                        (cons v efr)
                                                        (cons (vector-ref reach? v)
                                                            efrr)
                                                        state)))
                                            (vector-set! from-root v #f)
                                            (_-*- (+ v 1)
                                                outs
                                                efr
                                                efrr
                                                state)))
                                    ((and (natural-for-all? root
                                                (lambda (v)
                                                    (there-exists? efrr
                                                        (lambda (r)
                                                            (vector-ref r v)))))
                                            (root-minimal?))
                                        (vector-set! edges root efr)
                                        (folder
                                            (proc->vector size
                                                (lambda (i)
                                                    (vector-ref edges i)))
                                            state))
                                    (else
                                        state)))))
                    (else
                        (let ((from-vertex
                                    (vector-ref edge? vertex)))
                            (let _-**-
                                ((sv
                                        0)
                                    (outs
                                        0)
                                    (state
                                        state))
                                (if (= sv vertex)
                                    (begin
                                        (vector-set! out-degrees vertex outs)
                                        (_-*- (+ vertex 1)
                                            state))
                                    (let* ((state
                                                ; no sv->vertex, no vertex->sv
                                                (_-**- (+ sv 1)
                                                    outs
                                                    state))
                                            (from-sv
                                                (vector-ref edge? sv))
                                            (sv-out
                                                (vector-ref out-degrees sv))
                                            (state
                                                (if (= sv-out max-out)
                                                    state
                                                    (begin
                                                        (vector-set! edges
                                                            sv
                                                            (cons vertex
                                                                (vector-ref edges sv)))
                                                        (vector-set! from-sv vertex #t)
                                                        (vector-set! out-degrees sv (+ sv-out 1))
                                                        (let* ((state
                                                                    ; sv->vertex, no vertex->sv
                                                                    (_-**- (+ sv 1)
                                                                        outs
                                                                        state))
                                                                (state
                                                                    (if (= outs max-out)
                                                                        state
                                                                        (begin
                                                                            (vector-set! from-vertex sv #t)
                                                                            (vector-set! edges
                                                                                vertex
                                                                                (cons sv
                                                                                    (vector-ref edges vertex)))
                                                                            (let ((state
                                                                                        ; sv->vertex, vertex->sv
                                                                                        (_-**- (+ sv 1)
                                                                                            (+ outs 1)
                                                                                            state)))
                                                                                (vector-set! edges
                                                                                    vertex
                                                                                    (cdr (vector-ref edges vertex)))
                                                                                (vector-set! from-vertex sv #f)
                                                                                state)))))
                                                            (vector-set! out-degrees sv sv-out)
                                                            (vector-set! from-sv vertex #f)
                                                            (vector-set! edges
                                                                sv
                                                                (cdr (vector-ref edges sv)))
                                                            state)))))
                                        (if (= outs max-out)
                                            state
                                            (begin
                                                (vector-set! edges
                                                    vertex
                                                    (cons sv
                                                        (vector-ref edges vertex)))
                                                (vector-set! from-vertex sv #t)
                                                (let ((state
                                                            ; no sv->vertex, vertex->sv
                                                            (_-**- (+ sv 1)
                                                                (+ outs 1)
                                                                state)))
                                                    (vector-set! from-vertex sv #f)
                                                    (vector-set! edges
                                                        vertex
                                                        (cdr (vector-ref edges vertex)))
                                                    state)))))))))))))

; Given a vector which maps vertex to out-going-edge list,
; return a vector  which gives reachability.
(define make-reach?
    (lambda (size vertex->out)
        (let ((res
                    (proc->vector size
                        (lambda (v)
                            (let ((from-v
                                        (make-vector size #f)))
                                (vector-set! from-v v #t)
                                (for-each
                                    (lambda (x)
                                        (vector-set! from-v x #t))
                                    (vector-ref vertex->out v))
                                from-v)))))
            (gnatural-for-each size
                (lambda (m)
                    (let ((from-m
                                (vector-ref res m)))
                        (gnatural-for-each size
                            (lambda (f)
                                (let ((from-f
                                            (vector-ref res f)))
                                    (if (vector-ref from-f m)
                                        (gnatural-for-each size
                                            (lambda (t)
                                                (if (vector-ref from-m t)
                                                    (vector-set! from-f t #t)))))))))))
            res)))


;;; ==== test input ====

; Produces all directed graphs with N verticies, distinguished root,
; and out-degree bounded by 2, upto isomorphism.

(define (run n)
  (fold-over-rdg n
    2
    cons
    '()))

;-----

(let ((r (run 5)))
  (pp (length r))
  (println r))

;596
;313210203313210203231321020212132312032132312022132312032213231201213231203121323120212132312002132312030213231202021323120102132310321323102213231032213231021213231002132310302132310202132310321323102213231032213231012132310312132310212132310021323103021323102021323101021322032132203221322031213232032132320221323203221323201213232031213232021213232002132320302132320202132320102132303213230221323032213230121323031213230212132300213230302132302021323010323210103323210103232321010312311032311022311032231103123110212311002311030231102023110102331103233110223311032233110123311031233110212331100233110302331102023311010231103231102231103223110123110312311021231100231103023110202311010233120323312032233120123312031233120212331201023310323310223310322331012331031233102123310023310302331020233101023103231032231031323110332311023231103232311031323110213231100323110303231102032311010323311033233110232331103232331101323311031323311021323311003233110303233110203233110103231103323110232311032323110132311031323110213231100323110303231102032311010323120332312023231203232312013231203132312021323120032312030323120203231201032331203323312023233120323233120132331203132331202132331200323312030323312020323312010323310332331023233103232331013233103132331021323310032331030323310203233101032312103231213032312110323110323113032311203231110323121032312130323121203231211032331210323312130323312120323312110323311032331130323311203233111032311032311303231120323111022311032231102223110322231101223110312231102122311002231103022311020223110102323110323231102232311032232311012323110312323110212323110023231103023231102023231101032231103322311023223110323223110132231103132231102132231100322311030322311020322311010322110332211032322110313221103032323110332323110232323110323232311013232311031323231102132323110032323110303232311020323231101032321103323211023232110323232110132321103132321102132321100323211030323211020323211010323231203323231203232323120132323120313232312021323231201032323103323231023232310323232310132323103132323102132323100323231030323231020323231010323210332321032323210132321031323210213232101032323121032323121303232312110323231103232311303232311203232311103232110323211303232111022310322310222310322231012231031223102122310102210322103222103123231032323102232310322323101232310312323102123231002323103023231020232310102321032321032232101232103123210212321030232101032323103323231023232310323232310132323103132323102132323101032321033232103232321013232103132321021323210103222103222130322212032221103223210322321303223212032232110322310322313032231203223110322103221303221203221103232210323221303232212032322110323232103232321303232321203232321103232310323231303232312032323110323210323213032321203232110323232103232310323210321203321202321203232120132120313212021321203032120203212010323120332312023231203232312013231203132312021323120303231202032312010323102323103232310213231020332120333212023321203233212013321203133212021332120033212030332120203321201033231203332312023323120323323120133231203133231202133231200332312030332312020332312010332310233231032332310213323102032312033231203232312013231203132312021323120103231023231032323101323103132310213231020323101032103232121032121303212120323121032312130323121203231211033231210332312130332312120332120332123033212203321210332312033231230332312203323121033231203231213023103231022310322310123103123102123103023102023101032310332310232310323231013231031323102132310303231020323101032103321032321031321021322013220313220213220103232013232031323202132320103230132303132302132301032013203132021320103231033231023231032323101323103132310213231003231030323102032310103210332103232103132100321030321020321010332310333231023323103233231013323103133231021332310033231030332310203323101033210333210323321013321031332102133210033210303321020332101033220133220313322021332201033232013323203133232021332320103323013323031332302133230103320133203133202133201032203132201032320132320313232021323201032301323031323021323010320313221032213032212032211032321032321303232120323211032310323130323120323110321032130321203211033221033221303322120332211033232103323213033232120332321103323103323130332312033231103321033213033212033211033221033232103323103321032321032310321023213023212023211023130322130323213032321203232110323210323103210321303212033210332130332120332110333210333213033321203321033321021303213032120332130332120321033210
