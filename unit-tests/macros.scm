
(println (and))
(println (and #t))
(println (and #f))
(println (and #f #t))
(println (and #f #f #t))
(println (and #f #f #f))
(println (and #t 10 '()))
(println (and 1 2 3))
(println (and (and #t #t) (and #t #t)))

(println 123456789)

(println (or))
(println (or #t))
(println (or #f))
(println (or #f #t))
(println (or #f #f #t))
(println (or #f #f #f))
(println (or #f #f 10))
(println (or (or #f #f) (or #f #t)))

(println 123456789)

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

(println 123456789)

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

(println 123456789)

(if #t (println 100))
(if #f (println 200))
(if #t (println 300) (println 400))
(if #f (println 500) (println 600))
(if (or #t #f) (println (and #t #t)) (println (or #t #t)))
(if (and #t #f) (println (and #t #t)) (println (or #t #t)))

;#t
;#t
;#f
;#f
;#f
;#f
;
;3
;#t
;123456789
;#f
;#t
;#f
;#t
;#t
;#f
;10
;#t
;123456789
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
;123456789
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
;123456789
;100
;300
;600
;#t
;#t
