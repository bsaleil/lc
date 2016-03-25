
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

;; close-input/output-port, write-char, read-char primitives
(let ((file "./unit-tests/mutable-out")
      (port #f))
  (set! port (open-output-file file))
  (write-char #\X port)
  (write-char #\Y port)
  (close-output-port port)
  (set! port (open-input-file file))
  (print (read-char port)) (print #\,)
  (print (read-char port))
  (println #\.))

;; open-input/output-file primitives
(let ((file "./unit-tests/mutable"))
  (set! file "./unit-tests/mutable-out")
  (let ((out (open-output-file file)))
    (write-char #\A out)
    (write-char #\B out)
    (close-output-port out))
  (let ((in (open-input-file file)))
    (print (read-char in)) (print #\,)
    (println (read-char in))))

;; string-set! primitive
(let ((str "Hello World")
      (idx 2)
      (chr #\L)
      (idxs 4)
      (chrs #\B))
  (string-set! str idx #\P)
  (set! idx 4)
  (string-set! str idx chr)
  (set! chr #\O)
  (string-set! str idx chr)
  (print str) (print #\,)
  (set! str "Other test")
  (string-set! str idx chr)
  (print str) (print #\,)
  (string-set! str idxs chr)
  (string-set! str idxs chrs)
  (pp str))

;; string-length primitive
(let ((a "Hello World"))
  (print (string-length a))
  (set! a "Goodbye World")
  (print (string-length a))
  (newline))

;; make-vector primitive
(let ((len 9)
      (init #\V))
  (print (make-vector len init)) (print #\,)
  (set! len 12)
  (set! init #\M)
  (println (make-vector len init)))

;; make-string primitive
(let ((len 10)
      (init #\L))
  (print (make-string len init)) (print #\,)
  (set! len 5)
  (set! init #\M)
  (println (make-string len init)))

;; eof-object? primitive
(let ((out (open-output-file "./unit-tests/mutable-out")))
  (write-char #\O out)
  (close-output-port out)
  (let* ((in (open-input-file "./unit-tests/mutable-out"))
         (ch (read-char in)))
    (print (eof-object? ch))
    (set! ch (read-char in))
    (println (eof-object? ch))))

;; write-char primitive
(let ((a #\A))
  (write-char a)
  (set! a #\B)
  (write-char a)
  (newline))

;;
;;
;;

;; integer numeric operator
(let ((a 100)
      (b 200)
      (c 5)
      (d 6))
   (set! a 48)
   (set! b 4)
   (print (+ a b)) (print #\,)
   (print (* a c)) (print #\,)
   (println (- d a)))

; integer comparison operator
(let ((a 100)
      (b 200)
      (c 5)
      (d 6))
   (set! a 5)
   (set! b 4)
   (print (< a b)) (print #\,)
   (print (= a c)) (print #\,)
   (print (> a b)) (print #\,)
   (print (<= c a)) (print #\,)
   (print (< a d)) (print #\,)
   (print (> d a)) (print #\,)
   ;; inlined condition
   (if (< a b)
       (print #\a)
       (print #\b))
   (print #\,)
   (if (>= d c)
       (println #\c)
       (println #\d)))

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
;X,Y.
;A,B
;HePlO World,OtheO test,"OtheB test"
;1113
;VVVVVVVVV,MMMMMMMMMMMM
;LLLLLLLLLL,MMMMM
;#f#t
;AB
;52,240,-42
;#f,#t,#t,#t,#t,#t,b,c
