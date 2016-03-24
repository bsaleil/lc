
;; car/cdr primitive
(let ((a (cons 1 #\o)))
  (print (car a))
  (write-char (cdr a))
  (set! a (cons 2 #\t))
  (print (car a))
  (write-char (cdr a))
  (newline))

;; eq? primitive
(let ((a 10)
      (b 20)
      (c 21))
  (print (eq? a b))
  (set! a 21)
  (print (eq? a b))
  (print (eq? a c))
  (print (eq? c b))
  (set! b 21)
  (println (eq? a b)))

;; not primitive
(let ((a 10))
  (print (not a))
  (set! a #f)
  (print (not a))
  (newline))

;; set-car, set-cdr primitives
(let ((a '(1 2)))
  (print a) (print #\,)
  (set! a '(3 4))
  (print a) (print #\,)
  (set-car! a 10)
  (print a) (print #\,)
  (set-cdr! a 20)
  (println a))

;; cons primitive
(let ((a 10) (b 20) (c 30))
  (print (cons a b)) (print #\,)
  (set! a 10)
  (print (cons a c)) (print #\,)
  (set! b 20)
  (println (cons c b)))

;; vector-length primitive
(let ((a (make-vector 10 #\A)))
  (print (vector-length a))
  (set! a (make-vector 15 #\B))
  (print (vector-length a))
  (newline))

;; vector-ref primitive
(let ((a '#(10 20 30 40))
      (idx 2))
  (print (vector-ref a idx)) (print '#\,)
  (set! a '#(100 200 300 400))
  (print (vector-ref a idx)) (print '#\,)
  (set! idx 3)
  (println (vector-ref a idx)))

;; char->integer primitive
(let ((a #\*))
  (print (char->integer a))
  (set! a #\0)
  (print (+ (char->integer a) 2))
  (newline))

;; integer->char primitive
(let ((a 68))
  (print (integer->char a))
  (set! a 86)
  (print (integer->char a))
  (newline))

;; string-ref primitive
(let ((str "hello")
      (idx 2))
  (print (string-ref str idx)) (print #\,)
  (set! str "World")
  (print (string-ref str idx)) (print #\,)
  (set! idx 4)
  (println (string-ref str idx)))

;; string->symbol primitive
(let ((a "world"))
  (pp a)
  (set! a (string->symbol a))
  (pp a))

;; symbol->string primitive
(let ((a 'hello))
  (pp a)
  (set! a (symbol->string a))
  (pp a))

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
;#f#f#t#f#t
;#f#t
;12,34,104,1020
;1020,1030,3020
;1015
;30,300,400
;4250
;DV
;l,r,d
;"world"
;world
;hello
;"hello"
;1113
;AB
