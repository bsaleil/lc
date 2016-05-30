;;---------------------------------------------------------------------------
;;
;;  Copyright (c) 2015, Baptiste Saleil. All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;   3. The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior written
;;      permission.
;;
;;  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
;;  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;---------------------------------------------------------------------------

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
(println tree)

;; Checking
(println "Checking")
(pp (is-bst tree 0 99999999999))

;; Searching
(println "Searching")
(println (search tree 12))
(println (search tree 15))

;; Deletion
(println "Deletion")
(println (delete tree 24))
(pp (is-bst (delete tree 24) 0 99999999999))
(println (delete tree 444))
(pp (is-bst (delete tree 444) 0 99999999999))

;; Traversal
(println "Traversal")

;; Pre-order traversal
(println "pre:")
(pre-order tree
           print)
(newline)

;; Post-order traversal
(println "post:")
(post-order tree
            print)

(newline)

;; In-order traversal
(println "in:")
(in-order tree
          print)

(newline)

;Insertion
;1084392413122314452542109875857222
;Checking
;#t
;Searching
;12
;#f
;Deletion
;10843925131223144542109875857222
;#t
;1084392413122314452542109875857222
;#t
;Traversal
;pre:
;1084392413122314452542109875857222
;post:
;3498121423134225575887222109452410
;in:
;3489101213142324254245575887109222
