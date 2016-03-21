
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
  (print (vector-length a))
  (set! a (make-vector 15 #\B))
  (print (vector-length a))
  (newline))

;; char->integer primitive
(let ((a #\*))
  (print (char->integer a))
  (set! a #\0)
  (print (+ (char->integer a) 2))
  (newline))

;; integr->char primitive
(let ((a 68))
  (print (integer->char a))
  (set! a 86)
  (print (integer->char a))
  (newline))

;; string-length primitive
(let ((a "Hello World"))
  (print (string-length a))
  (set! a "Goodbye World")
  (print (string-length a))
  (newline))

;; write-char primitive
(let ((a #\A))
  (write-char a)
  (set! a #\B)
  (write-char a)
  (newline))


;1o2t
;#f#t
;1015
;4250
;DV
;1113
;AB
