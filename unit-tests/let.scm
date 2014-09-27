
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