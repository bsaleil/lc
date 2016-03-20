
;; car/cdr primitive
(let ((a (cons 1 #\o)))
  (print (car a))
  (write-char (cdr a))
  (set! a (cons 2 #\t))
  (print (car a))
  (write-char (cdr a))
  (newline))

;; not primitive
(let ((a 10))
  (print (not a))
  (set! a #f)
  (print (not a))
  (newline))

;; vector-length primitive
(let ((a (make-vector 10 #\A)))
  (println (vector-length a))
  (set! a (make-vector 15 #\B))
  (println (vector-length a)))

;; string-length primitive
(let ((a "Hello World"))
  (println (string-length a))
  (set! a "Goodbye World")
  (println (string-length a)))

;; write-char primitive
(let ((a #\A))
  (write-char a)
  (set! a #\B)
  (write-char a)
  (newline))


;1o2t
;#f#t
;AB
