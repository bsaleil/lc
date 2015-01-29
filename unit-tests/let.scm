
(let ((x 1))
  (println x))

(let ((x 1)
      (y 2))
  (println x))

(let ((x 1)
      (y 2))
  (println y))

(let ((x 1)
      (y 2)
      (z 3))
  (println x)
  (println y)
  (println z)
  z)

(define lx 10)
(let ((lx 100)
      (ly (+ lx 200)))
  (println lx)
  (println ly))

(println 123456789)

(let* ((x 1)
      (y 2))
  (println x))

(let* ((x 1)
      (y 2))
  (println y))

(let* ((x 1)
      (y 2)
      (z 3))
  (println x)
  (println y)
  (println z)
  z)

(define la 10)
(let* ((lb (+ la 200))
       (la 100)
       (lc (+ la 200)))
  (println la)
  (println lb)
  (println lc))

(letrec ((f (lambda (n)
              (if (= 0 n)
                ($$putchar 80) ;; P
                (g (- n 1)))))
         (g (lambda (n)
              (if (= 0 n)
                ($$putchar 73) ;; I
                (f (- n 1))))))
   (f 11)
   (f 12)
   (f 13)
   (f 14)
   (f 15)
   (f 16)
   ($$putchar 10))

(println 

(let fact ((n 4))
   (println 10)
   (if (= n 0)
      1
      (* n (fact (- n 1)))))

)

;1
;1
;2
;1
;2
;3
;100
;210
;123456789
;1
;2
;1
;2
;3
;100
;210
;300
;IPIPIP
;10
;10
;10
;10
;10
;24
