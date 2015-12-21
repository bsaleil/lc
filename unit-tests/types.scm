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

(println "Number")

(println (number?  1))
(println (number? -1))
(println (number?  0))
(println (number? #t))
(println (number? #f))
(println (number? '()))
(println (number? (cons 1 2)))
(println (number? (cons 1 (cons 2 '()))))
(println (number? (lambda (a b c) (+ a c))))
(println (number? (make-vector 10 #t)))
(println (number? #\newline))
(println (number? #\a))
(println (number? "Hello World"))
(println (number? (make-string 4 #\A)))
(println (number? 'Hello))
(println (number? (string->symbol (string-append "Hi World"))))

(println "Boolean")

(println (boolean?  1))
(println (boolean? -1))
(println (boolean?  0))
(println (boolean? #t))
(println (boolean? #f))
(println (boolean? '()))
(println (boolean? (cons 1 2)))
(println (boolean? (cons 1 (cons 2 '()))))
(println (boolean? (lambda (a b c) (+ a c))))
(println (boolean? (make-vector 10 #t)))
(println (boolean? #\newline))
(println (boolean? #\a))
(println (boolean? "Hello World"))
(println (boolean? (make-string 4 #\A)))
(println (boolean? 'Hello))
(println (boolean? (string->symbol (string-append "Hi World"))))

(println "Null")

(println (null?  1))
(println (null? -1))
(println (null?  0))
(println (null? #t))
(println (null? #f))
(println (null? '()))
(println (null? (cons 1 2)))
(println (null? (cons 1 (cons 2 '()))))
(println (null? (lambda (a b c) (+ a c))))
(println (null? (make-vector 10 #t)))
(println (null? #\newline))
(println (null? #\a))
(println (null? "Hello World"))
(println (null? (make-string 4 #\A)))
(println (null? 'Hello))
(println (null? (string->symbol (string-append "Hi World"))))

(println "Pair")

(println (pair?  1))
(println (pair? -1))
(println (pair?  0))
(println (pair? #t))
(println (pair? #f))
(println (pair? '()))
(println (pair? (cons 1 2)))
(println (pair? (cons 1 (cons 2 '()))))
(println (pair? (lambda (a b c) (+ a c))))
(println (pair? (make-vector 10 #t)))
(println (pair? #\newline))
(println (pair? #\a))
(println (pair? "Hello World"))
(println (pair? (make-string 4 #\A)))
(println (pair? 'Hello))
(println (pair? (string->symbol (string-append "Hi World"))))

(println "Procedure")

(println (procedure?  1))
(println (procedure? -1))
(println (procedure?  0))
(println (procedure? #t))
(println (procedure? #f))
(println (procedure? '()))
(println (procedure? (cons 1 2)))
(println (procedure? (cons 1 (cons 2 '()))))
(println (procedure? (lambda (a b c) (+ a c))))
(println (procedure? (make-vector 10 #t)))
(println (procedure? #\newline))
(println (procedure? #\a))
(println (procedure? "Hello World"))
(println (procedure? (make-string 4 #\A)))
(println (procedure? 'Hello))
(println (procedure? (string->symbol (string-append "Hi World"))))

(println "List")

(println (list?  1))
(println (list? -1))
(println (list?  0))
(println (list? #t))
(println (list? #f))
(println (list? '()))
(println (list? (cons 1 2)))
(println (list? (cons 1 (cons 2 '()))))
(println (list? (lambda (a b c) (+ a c))))
(println (list? (make-vector 10 #t)))
(println (list? #\newline))
(println (list? #\a))
(println (list? "Hello World"))
(println (list? (make-string 4 #\A)))
(println (list? 'Hello))
(println (list? (string->symbol (string-append "Hi World"))))

(println "Vector")

(println (vector?  1))
(println (vector? -1))
(println (vector?  0))
(println (vector? #t))
(println (vector? #f))
(println (vector? '()))
(println (vector? (cons 1 2)))
(println (vector? (cons 1 (cons 2 '()))))
(println (vector? (lambda (a b c) (+ a c))))
(println (vector? (make-vector 10 #t)))
(println (vector? #\newline))
(println (vector? #\a))
(println (vector? "Hello World"))
(println (vector? (make-string 4 #\A)))
(println (vector? 'Hello))
(println (vector? (string->symbol (string-append "Hi World"))))

(println "Char")

(println (char?  1))
(println (char? -1))
(println (char?  0))
(println (char? #t))
(println (char? #f))
(println (char? '()))
(println (char? (cons 1 2)))
(println (char? (cons 1 (cons 2 '()))))
(println (char? (lambda (a b c) (+ a c))))
(println (char? (make-vector 10 #t)))
(println (char? #\newline))
(println (char? #\a))
(println (char? "Hello World"))
(println (char? (make-string 4 #\A)))
(println (char? 'Hello))
(println (char? (string->symbol (string-append "Hi World"))))

(println "String")

(println (string?  1))
(println (string? -1))
(println (string?  0))
(println (string? #t))
(println (string? #f))
(println (string? '()))
(println (string? (cons 1 2)))
(println (string? (cons 1 (cons 2 '()))))
(println (string? (lambda (a b c) (+ a c))))
(println (string? (make-vector 10 #t)))
(println (string? #\newline))
(println (string? #\a))
(println (string? "Hello World"))
(println (string? (make-string 4 #\A)))
(println (string? 'Hello))
(println (string? (string->symbol (string-append "Hi World"))))

(println 'Symbol)

(println (symbol?  1))
(println (symbol? -1))
(println (symbol?  0))
(println (symbol? #t))
(println (symbol? #f))
(println (symbol? '()))
(println (symbol? (cons 1 2)))
(println (symbol? (cons 1 (cons 2 '()))))
(println (symbol? (lambda (a b c) (+ a c))))
(println (symbol? (make-vector 10 #t)))
(println (symbol? #\newline))
(println (symbol? #\a))
(println (symbol? "Hello World"))
(println (symbol? (make-string 4 #\A)))
(println (symbol? 'Hello))
(println (symbol? (string->symbol (string-append "Hi World"))))

;Number
;#t
;#t
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;Boolean
;#f
;#f
;#f
;#t
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;Null
;#f
;#f
;#f
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;Pair
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;Procedure
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;List
;#f
;#f
;#f
;#f
;#f
;#t
;#f
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;Vector
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;Char
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#f
;#f
;#f
;#f
;String
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#f
;#f
;Symbol
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
