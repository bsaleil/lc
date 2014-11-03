
;; OPERATORS

(define (+ x y)        ($+ x y))
(define (- x y)        ($- x y))
(define (* x y)        ($* x y))
(define (quotient x y) ($quotient x y))
(define (modulo x y)   ($modulo x y))
(define (< x y)        ($< x y))
(define (> x y)        ($> x y))
(define (= x y)        ($= x y))
(define (eq? x y)      ($eq? x y))

(define (not x)
  (eq? x #f))

(define (<= x y)
  (not (> x y)))

(define (>= x y)
  (not (< x y)))

;; TYPE TESTS

(define (number? n)
  ($number? n))

(define (char? n)
  ($char? n))

(define (procedure? n)
  ($procedure? n))

(define (pair? n)
  ($pair? n))

(define (vector? n)
  ($vector? n))

(define (string? s)
  ($string? s))

(define (boolean? n)
  (cond ((eq? n #t) #t)
        ((eq? n #f) #t)
        (else #f)))

(define (null? n)
	(eq? n '()))

;; LISTS

(define (cons a b)
  ($cons a b))

(define (car l)
  ($car l))

(define (cdr l)
  ($cdr l))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define (list . l) l)

;; TODO : type test 
(define (list? n)
  (cond ((null? n) #t)
        ((pair? n) (list? (cdr n)))
        (else #f)))

;; VECTORS

(define (vector-length v)
   ($vector-length v))

(define (vector-ref v i)
  ($vector-ref v i))

(define (vector-set! v i val)
  ($vector-set! v i val))

(define (vector-fill!-h v pos val len)
  (if (= pos len)
    #f
    (begin (vector-set! v pos val)
           (vector-fill!-h v (+ pos 1) val len))))

(define (vector-fill! v val)
  (vector-fill!-h v 0 val (vector-length v)))

(define (make-vector size . init)
  (let ((v ($make-vector size)))
    (cond ((null? init)
            (begin (vector-fill! v 0) v))
        ((= (length init) 1)
            (begin (vector-fill! v (car init)) v))
        (else #f)))) ;; TODO ERR

;; STRINGS

(define (string-length s)
  ($string-length s))

(define (string-ref s i)
  ($string-ref s i))

(define (make-string s)
  ($make-string s))

;; TYPES CONVERSION

(define (vector->list-h vector idx length)
   (if (= idx length)
      '()
      (cons (vector-ref vector idx) (vector->list-h vector (+ idx 1) length))))

(define (vector->list v)
   (vector->list-h v 0 (vector-length v)))

(define (list->vector-h lst vec pos len)
  (if (null? lst)
    vec
    (begin (vector-set! vec pos (car lst))
           (list->vector-h (cdr lst) vec (+ pos 1) len))))

(define (list->vector l)
  (let ((v (make-vector (length l))))
     (list->vector-h l v 0 (length l))))

(define (char->integer c)
  ($char->integer c))

(define (integer->char n)
  ($integer->char n))

;; TODO wrong place
(define (vector . l) (list->vector l))

;; CHAR

(define (char=? c1 c2)
   (= (char->integer c1) (char->integer c2)))

(define (char<? c1 c2)
   (< (char->integer c1) (char->integer c2)))

(define (char>? c1 c2)
   (> (char->integer c1) (char->integer c2)))

(define (char<=? c1 c2)
   (<= (char->integer c1) (char->integer c2)))

(define (char>=? c1 c2)
   (>= (char->integer c1) (char->integer c2)))

(define (char-alphabetic? c)
  (let ((c (char->integer c)))
    (or (and (> c 64) (< c 91))
      (and (> c 96) (< c 123)))))

(define (char-numeric? c)
  (let ((c (char->integer c)))
    (and (> c 47) (< c 58))))

(define (char-whitespace? c)
  (let ((c (char->integer c)))
    (or (= c 32) (= c 9) (= c 10) (= c 12) (= c 13))))

(define (char-upper-case? c)
  (let ((c (char->integer c)))
    (and (> c 64) (< c 91))))

(define (char-lower-case? c)
  (let ((c (char->integer c)))
    (and (> c 96) (< c 123))))

(define (char-upcase c)
   (let ((v (char->integer c)))
     (if (and (> v 96) (< v 123))
        (integer->char (- v 32))
        c)))

(define (char-downcase c)
   (let ((v (char->integer c)))
     (if (and (> v 64) (< v 91))
        (integer->char (+ v 32))
        c)))

(define (char-ci=? c1 c2)
   (= (char->integer (char-downcase c1))
      (char->integer (char-downcase c2))))

(define (char-ci<? c1 c2)
   (< (char->integer (char-downcase c1))
      (char->integer (char-downcase c2))))

(define (char-ci>? c1 c2)
   (> (char->integer (char-downcase c1))
      (char->integer (char-downcase c2))))

(define (char-ci<=? c1 c2)
   (<= (char->integer (char-downcase c1))
       (char->integer (char-downcase c2))))

(define (char-ci>=? c1 c2)
   (>= (char->integer (char-downcase c1))
       (char->integer (char-downcase c2))))

;; PRINT
(define print #f)

(define (print-pos-nz n)
    (if (> n 0)
        (begin (print-pos-nz (quotient n 10))
               ($$putchar (+ (modulo n 10) 48)))))

(define (print-pos n)
    (if (= n 0)
        ($$putchar 48)
        (print-pos-nz n)))

(define (print-nb n)
    (if (< n 0)
        (begin ($$putchar 45)
               (print-pos (* -1 n)))
        (print-pos n)))

(define (print-char n)
  ($$putchar (char->integer n)))

(define (print-bool n)
  (if ($eq? n #t)
      (begin ($$putchar  35)
             ($$putchar 116))
      (begin ($$putchar  35)
             ($$putchar 102))))

(define (print-procedure n)
  ($$putchar  35)
  ($$putchar  60)
  ($$putchar 112)
  ($$putchar 114)
  ($$putchar 111)
  ($$putchar  99)
  ($$putchar 101)
  ($$putchar 100)
  ($$putchar 117)
  ($$putchar 114)
  ($$putchar 101)
  ($$putchar  62))

(define (print-vector vector idx length)
  (if (< idx length)
    (begin (print (vector-ref vector idx))
           (print-vector vector (+ idx 1) length))))

(set! print
  (lambda (n)
    (cond ((null? n) #f)
          ((number? n) (print-nb n))
          ((char? n) (print-char n))
          ((procedure? n) (print-procedure n))
          ((pair? n) (begin (print (car n))
                            (print (cdr n))))
          ((vector? n) (print-vector n 0 (vector-length n)))
          (else (print-bool n)))))
  
(define (println n)
  (print n)
  ($$putchar 10))

;; PP
(define pp-h #f)

(define (pp-pair-h n)
  (cond ;; (e1)
        ((and (list? n) (= (length n) 1)) (pp-h (car n)))
        ;; (e1 . e2)
        ((not (pair? (cdr n)))
            (begin (pp-h (car n))
                   ($$putchar 32)
                   ($$putchar 46)
                   ($$putchar 32)
                   (pp-h (cdr n))))
        ;; (e1 ...)
        (else (begin (pp-h (car n))
                     ($$putchar 32)
                     (pp-pair-h (cdr n))))))

(define (pp-pair n)
  ($$putchar 40)
  (pp-pair-h n)
  ($$putchar 41))

(define (pp-char n)
  ($$putchar 35)
  ($$putchar 92)
  (let ((v (char->integer n)))
    (if (> v 32)
       (print-char n)
       (begin ($$putchar 84) ($$putchar 79) ($$putchar 68) ($$putchar 79))))) ;; TODO when strings implemented

(define (pp-vector-h vector idx length)
  (cond ((= idx (- length 1))
            (pp-h (vector-ref vector idx)))
        ((< idx length)
            (begin (pp-h (vector-ref vector idx))
                   ($$putchar 32)
                   (pp-vector-h vector (+ idx 1) length)))))

(define (pp-vector vector)
  ($$putchar 35)
  ($$putchar 40)
  (pp-vector-h vector 0 (vector-length vector))
  ($$putchar 41))

(set! pp-h (lambda (n)
  (cond ((null? n) (begin ($$putchar 40) ($$putchar 41))) ;; ()
        ((number? n) (print-nb n))
        ((char? n) (pp-char n))
        ((procedure? n) (print-procedure n))
        ((pair? n) (pp-pair n))
        ((vector? n) (pp-vector n))
        (else (print-bool n)))))

(define (pp n)
  (pp-h n)
  ($$putchar 10))

;; Others utils not in unit tests

(define (newline)
   ($$putchar 10))

(define (expt n e)
  (if (= e 0)
    1
    (* n (expt n (- e 1)))))

(define (cddr l)
  (cdr (cdr l)))