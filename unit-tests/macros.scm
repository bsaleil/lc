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

(println "AND")

(println (and))
(println (and #t))
(println (and #f))
(println (and #f #t))
(println (and #f #f #t))
(println (and #f #f #f))
(println (and #t 10 '()))
(println (and 1 2 3))
(println (and (and #t #t) (and #t #t)))

(println "OR")

(println (or))
(println (or #t))
(println (or #f))
(println (or #f #t))
(println (or #f #f #t))
(println (or #f #f #f))
(println (or #f #f 10))
(println (or (or #f #f) (or #f #t)))

(println "COND")

(println (cond (else 99)))
(println (cond (#t 50)))
(println (cond (#f 51)))
(println (cond ((= 0 1) 0)
               ((= 0 2) 1)
               ((= 0 3) 2)))
(println (cond ((> 30 20) 0)
               ((< 30 20) 1)
               ((= 30 20) 2)))
(println (cond ((> 10 20) 0)
               ((< 10 20) 1)
               ((= 10 20) 2)))
(println (cond ((> 20 20) 0)
               ((< 20 20) 1)
               ((= 20 20) 2)))
(println (cond ((= 30 40) 0)
               ((= 30 50) 1)
               (else #f)))
(println (cond (else (or #t #f))))
(println (cond ((= 1 2) #t)
               ((= 2 2) (and #t #f))
               (else 10)))
(println (cond ((= 1 2) #t)
               ((= 1 1) (print "=") (print "1") (println "2") #t)
               (else #f)))
(println (cond ((= 1 2) #t)
               ((= 2 1) #f)
               (else (print 'e) (print 'l) (print 's) (println 'e) #t)))

(println "CASE")

(let ((a 5)
      (b 1))

  (println (case (+ a b)
             (else #f)))

  (println (case (+ a b)
             ((0 1 2 3 4) "<5")
             ((5 6 7 8 9) ">4,<10")
             (else ">9")))

  (println (case (car '(a b c))
             ((d a r k)   "I'm your father")
             ((v a d o r) "I'm your father again"))))

(println (case 'a
           ((f o o) (print "foo") 1)
           ((b a r) (print "bar") 2)))

(println "BEGIN")

(begin (println 0))
(begin (println 1)
  	   (println 2))
(begin (println (or #t #f #t)))

(define (foo1 n)
  (begin (println n)
         (println n)
         (println n)))

(define (foo2 n)
  (begin (println n)
         (println 10)
         (println n)))

(define (foo3 n)
  (begin (println 10)
         (println 10)
         (println n)))

(foo1 100)
(foo2 200)
(foo3 300)

(println "IF")

(if #t (println 100))
(if #f (println 200))
(if #t (println 300) (println 400))
(if #f (println 500) (println 600))
(if (or #t #f) (println (and #t #t)) (println (or #t #t)))
(if (and #t #f) (println (and #t #t)) (println (or #t #t)))

;AND
;#t
;#t
;#f
;#f
;#f
;#f
;
;3
;#t
;OR
;#f
;#t
;#f
;#t
;#t
;#f
;10
;#t
;COND
;99
;50
;#f
;#f
;0
;1
;2
;#f
;#t
;#f
;=12
;#t
;else
;#t
;CASE
;#f
;>4,<10
;I'm your father
;bar2
;BEGIN
;0
;1
;2
;#t
;100
;100
;100
;200
;10
;200
;10
;10
;300
;IF
;100
;300
;600
;#t
;#t
