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

(println (cons 1 2))
(println (length (cons 1 '())))
(println (length (cons (cons 1 2) '())))
(println (length (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))))
(println (length (cons 1 (cons #f (cons '() (cons (lambda (x) x) '()))))))

(define a (list))
(define b (list 1))
(define c (list 1 2))
(define d (list 1 2 3 4 5))

(pp (pair? a))
(pp (list? a))
(pp (null? a))
(pp (length a))
(pp a)

(pp (pair? b))
(pp (list? b))
(pp (null? b))
(pp (length b))
(pp b)

(pp (pair? c))
(pp (list? c))
(pp (null? c))
(pp (length c))
(pp c)

(pp (pair? d))
(pp (list? d))
(pp (null? d))
(pp (length d))
(pp d)

(println "LIB")

(println "Car/Cdr/...")

(define l '(1 #t #\T "Hello"))
(pp (car l))
(pp (cdr l))
(pp (cddr l))
(pp (cadr l))
(pp (caddr l))
(define ll (list 1 #t #\T "Hello"))
(set-car! ll #f)
(pp ll)
(set-cdr! ll '(#f #f #f))
(pp ll)

(println "Set-car!/Set-cdr!")

(define l2 (list 1 #t (list 1 2 3) #\T "Hello"))
(set-car! (caddr l2) #t)
(pp l2)
(set-cdr! (caddr l2) 10)
(pp l2)
(set-car! l2 'Hello)
(pp l2)
(set-cdr! l2 (list 'World))
(pp l2)

(println "Append")

(pp (append))
(pp (append '()))
(pp (append '(1 2 3)))
(pp (append '(1 2 3) '(4 5 6)))
(pp (append '(#f #\P 100) '("Hello" World)))

(println "List-ref")

(define l3 '(1 #t #\T "Hello"))

(pp (list-ref l3 0))
(pp (list-ref l3 1))
(pp (list-ref l3 2))
(pp (list-ref l3 3))

(println "Reverse")

(pp (reverse '()))
(pp (reverse '(#t)))
(pp (reverse '(1 2 3 4 5)))

(println "For-each")

(for-each (lambda (n) (print n)) '())
(newline)
(for-each (lambda (n) (print n)) '(1 2 3))
(newline)
(for-each (lambda (n) (print (+ n 100))) '(1 2 3))
(newline)

(println "Member")

(println (member 10 '(1 2 3 4 5 6)))
(println (member 4  '(1 2 3 4 5 6)))
(println (member "test" '("just" "a" "test")))
(println (member 'test  '(just a test)))
(println (member 10 '()))
(println (member '(5 6) '((1 2) (3 4) (5 6) (7 8))))

(println "Memq")

(println (memq 10 '(1 2 3 4 5 6)))
(println (memq 4  '(1 2 3 4 5 6)))
(println (memq "test" '("just" "a" "test")))
(println (memq 'test  '(just a test)))
(println (memq 10 '()))
(println (memq '(5 6) '((1 2) (3 4) (5 6) (7 8))))

(println "Assq")

(define lst '((2 . "two") (3 . "three") (5 . "five") (8 . "eight")
              (1 . "one") (7 . "seven") (4 . "four") (6 . "six")))

(pp (assq 5 lst))
(pp (assq 8 lst))

(define lst '(("one" . 1) ("two" . 2)))

(pp (assq "one" lst))

(println "Map")

(map println '(1 2 3))
(pp (map (lambda (n) (+ n 100)) '(1 2 3)))
(pp (map (lambda (n) (+ n 100)) '()))

;12
;1
;1
;5
;4
;#f
;#t
;#t
;0
;()
;#t
;#t
;#f
;1
;(1)
;#t
;#t
;#f
;2
;(1 2)
;#t
;#t
;#f
;5
;(1 2 3 4 5)
;LIB
;Car/Cdr/...
;1
;(#t #\T "Hello")
;(#\T "Hello")
;#t
;#\T
;(#f #t #\T "Hello")
;(#f #f #f #f)
;Set-car!/Set-cdr!
;(1 #t (#t 2 3) #\T "Hello")
;(1 #t (#t . 10) #\T "Hello")
;(Hello #t (#t . 10) #\T "Hello")
;(Hello World)
;Append
;()
;()
;(1 2 3)
;(1 2 3 4 5 6)
;(#f #\P 100 "Hello" World)
;List-ref
;1
;#t
;#\T
;"Hello"
;Reverse
;()
;(#t)
;(5 4 3 2 1)
;For-each
;
;123
;101102103
;Member
;#f
;456
;test
;test
;#f
;5678
;Memq
;#f
;456
;#f
;test
;#f
;#f
;Assq
;(5 . "five")
;(8 . "eight")
;#f
;Map
;1
;2
;3
;(101 102 103)
;()
