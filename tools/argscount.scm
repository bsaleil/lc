#!/usr/bin/env gsi
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

;;
;;	./argscount.scm path1 [path2 ...]
;;
;;  1 - Get the number of arguments of each declared functions for each .scm file
;;     recursively found in path1, path2, ... pathn
;;  2 - Merge results obtained from all files
;;  3 - Print csv representation of merged results
;;

(define SCRIPT "argscount.scm")

;;---------------------------------------------------------------------------
;; Scheme utils

;; Foldr
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))

(define (find-files file-or-dir filter)
  (if (eq? (file-type file-or-dir) 'directory)

      (apply
       append
       (map
        (lambda (f)
          (find-files (path-expand f file-or-dir) filter))
        (directory-files file-or-dir)))

      (if (filter file-or-dir)
          (list file-or-dir)
          (list))))

;; Recursively find .scm files from paths in 'args'
(define (list-of-scm-files args)
  (apply
   append
   (map
    (lambda (f)
      (find-files f
                  (lambda (filename)
                    (equal? (path-extension filename) ".scm"))))
    args)))

;;---------------------------------------------------------------------------
;; Script utils

;; Merge two results
(define (merge r1 r2)
   (if (null? r1)
   	  r2
   	  (let* ((first (car r1))
   	  	     (r2n   (assoc (car first) r2)))
   	    (if r2n
   	    	;; Exists in r2
   	    	(set-cdr! r2n (+ (cdr r2n) (cdr first)))
   	    	;; Does not exist in r2
   	    	(set! r2 (cons first r2)))
   	    (merge (cdr r1) r2))))

;; Print csv representation of res
(define (print-csv res)

  (define (print-csv-h res)
     (if (not (null? res))
        (let ((f (car res)))
      	   (if (number? (car f))
      	      (println (car f) ";" (cdr f))
      	      (println (car (car f)) "+rest;" (cdr f)))
      	   (print-csv-h (cdr res)))))

  (println "#args;#functions")
  (print-csv-h res))

;;---------------------------------------------------------------------------

(define (get-fn args)

	(define (get-improper l)
		(if (not (pair? l))
		   0
		   (+ 1 (get-improper (cdr l)))))

	(cond ((and (list? args) (or (member #!optional args)
		                         (member #!rest args))
	          (error "NYI")))
	      ;; List
	      ((list? args) (list (cons (length args) 1)))
	      ;; Pair
	      ((pair? args) (list (cons (cons (get-improper args) 'rest) 1)))
	      ;; Symbol
	      ((symbol? args) (list (cons (cons 0 'rest) 1)))
	      ;; Others
	      (else (error "Unsupported form"))))

(define (run ast)
   (cond ;; Lambda expr
   	     ((and (pair? ast) (eq? (car ast) 'lambda))
   	     	(merge (get-fn (cadr ast))
   	     	       ;(list (cons (length (cadr ast)) 1))
   	     		   (run (cddr ast))))
   	     ;; Define lambda
   	     ((and (pair? ast) (eq? (car ast) 'define) (pair? (cadr ast)))
   	     	(merge (get-fn (cdr (cadr ast)))
   	     	       ;(list (cons (- (length (cadr ast)) 1) 1))
   	     		   (run (cddr ast))))
   	     ;; List
   	     ((list? ast)
   	     	(foldr (lambda (el r)
   	     		      (merge (run el) r))
   	     	       '()
   	     	       ast))
   	     ;; Others
   	     (else '())))

;;---------------------------------------------------------------------------

(define (main . args)

	;; Run for all files
	(define (run-all files)
	   (if (null? files)
	   	  '()
	   	  (cons (cons (car files) (run (read-all (open-input-file (car files)))))
	   	  	    (run-all (cdr files)))))

	;; Merge result obtained from all files
	(define (merge-all in out)
		(if (null? in)
		   out
		   (merge-all (cdr in)
		   	          (merge (cdar in) out))))

	(if (null? args)
	   (begin (println "usage: "
	   	               "./"
	   	               SCRIPT
	   	               " path1 [path2 ...]")
	          (exit 0)))

	(let ((r (run-all (list-of-scm-files args))))
		;; (pp r)
		(print-csv (merge-all r '()))))

;;---------------------------------------------------------------------------
