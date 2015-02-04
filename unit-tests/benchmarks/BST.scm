;; BST implementation

;----------------
; Tree definition
;----------------

(define (make-node k l r)
  (list k l r))

(define (node.data n)
  (car n))

(define (node.left n)
  (cadr n))

(define (node.right n)
  (caddr n))

;----------
; Insertion
;----------

(define (insert tree k)
  (insert-node tree (make-node k '() '())))

(define (insert-node tree n)
  (if (null? tree)
      n
      (if (< (node.data n) (node.data tree))
        (make-node (node.data tree) (insert-node (node.left tree) n) (node.right tree))
        (make-node (node.data tree) (node.left tree) (insert-node (node.right tree) n)))))

(define (insert-list tree l)
  (if (null? l)
      tree
      (insert-list (insert tree (car l))
                   (cdr l))))

;----------
; Checking
;----------

(define (is-bst tree min max)
  (cond ((null? tree)
           #t)
        ((or (< (node.data tree) min)
             (> (node.data tree) max))
           #f)
        (else (and (is-bst (node.left  tree)  min (node.data  tree))
                   (is-bst (node.right tree)  (node.data  tree) max)))))

;----------
; Searching
;----------

(define (search tree k)
  (cond ((null? tree) #f)
        ((= (node.data tree) k)
           tree)
        ((< k (node.data tree))
           (search (node.left tree) k))
        (else
           (search (node.right tree) k))))

(define (find-min tree)
  (cond ((null? tree) (println "ERR"))
        ((null? (node.left tree)) (node.data tree))
        (else (find-min (node.left tree)))))

(define (find-max tree)
  (cond ((null? tree) (println "ERR"))
        ((null? (node.right tree)) (node.data tree))
        (else (find-max (node.right tree)))))

;----------
; Traversal
;----------

(define (pre-order tree fn)
  (if (not (null? tree))
     (begin (fn (node.data tree))
            (pre-order (node.left  tree) fn)
            (pre-order (node.right tree) fn))))

(define (post-order tree fn)
  (if (not (null? tree))
     (begin (post-order (node.left  tree) fn)
            (post-order (node.right tree) fn)
            (fn (node.data tree)))))

(define (in-order tree fn)
  (if (not (null? tree))
     (begin (in-order (node.left  tree) fn)
            (fn (node.data tree))
            (in-order (node.right tree) fn))))
 
;---------
; Deletion
;---------

(define (delete tree k)
  (cond ((null? tree) '())
        ((= k (node.data tree))
           (cond ((and (null? (node.left tree)) (null? (node.right tree)))
                     '())
                 ((null? (node.left tree))
                     (node.right tree))
                 ((null? (node.right tree))
                     (node.left tree))
                 (else
                     (let ((min (find-min (node.right tree))))
                       (make-node min (node.left tree) (delete (node.right tree) min))))))
        ((< k (node.data tree))
           (make-node (node.data tree) (delete (node.left tree) k) (node.right tree)))
           
        (else
           (make-node (node.data tree) (node.left tree) (delete (node.right tree) k)))))

;---------
; Run
;---------

(define tree '())
(define keys '(10 8 24 13 4 9 45 12 25 109 87 3 42 58 222 57 23 14))

;; Insertion
(println "Insertion")
(set! tree (insert-list tree keys))
(pp tree)

;; Checking
(println "Checking")
(pp (is-bst tree 0 99999999999))

;; Searching
(println "Searching")
(pp (search tree 12))
(pp (search tree 15))

;; Deletion
(println "Deletion")
(pp (delete tree 24))
(pp (is-bst (delete tree 24) 0 99999999999))
(pp (delete tree 444))
(pp (is-bst (delete tree 444) 0 99999999999))

;; Traversal
(println "Traversal")

;; Pre-order traversal
(println "pre:")
(pre-order tree
           (lambda (n)
             (print n)
             (print " ")))
(newline)

;; Post-order traversal
(println "post:")
(post-order tree
            (lambda (n)
              (print n)
              (print " ")))

(newline)

;; In-order traversal
(println "in:")
(in-order tree
          (lambda (n)
            (print n)
            (print " ")))

(newline)

;Insertion
;(10 (8 (4 (3 () ()) ()) (9 () ())) (24 (13 (12 () ()) (23 (14 () ()) ())) (45 (25 () (42 () ())) (109 (87 (58 (57 () ()) ()) ()) (222 () ())))))
;Checking
;#t
;Searching
;(12 () ())
;#f
;Deletion
;(10 (8 (4 (3 () ()) ()) (9 () ())) (25 (13 (12 () ()) (23 (14 () ()) ())) (45 (42 () ()) (109 (87 (58 (57 () ()) ()) ()) (222 () ())))))
;#t
;(10 (8 (4 (3 () ()) ()) (9 () ())) (24 (13 (12 () ()) (23 (14 () ()) ())) (45 (25 () (42 () ())) (109 (87 (58 (57 () ()) ()) ()) (222 () ())))))
;#t
;Traversal
;pre:
;10 8 4 3 9 24 13 12 23 14 45 25 42 109 87 58 57 222 
;post:
;3 4 9 8 12 14 23 13 42 25 57 58 87 222 109 45 24 10 
;in:
;3 4 8 9 10 12 13 14 23 24 25 42 45 57 58 87 109 222 
