
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

(define (symbol? s)
  ($symbol? s))

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

(define (cddr l)
  (cdr (cdr l)))

(define (cadr l)
  (car (cdr l)))

(define (length l)
  (if (null? l)
      0
      (+ 1 (length (cdr l)))))

(define list (lambda x x))

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
    (if (not (null? init))
        (vector-fill! v (car init)))
    v))

;; CHAR

(define (char->integer c)
  ($char->integer c))

(define (integer->char n)
  ($integer->char n))

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

;; STRINGS

(define (string-length s)
  ($string-length s))

(define (string-ref s i)
  ($string-ref s i))

(define (string-set! s i v)
  ($string-set! s i v))

(define (string->list-h s pos)
  (if (= (string-length s) pos)
      '()
      (cons (string-ref s pos) (string->list-h s (+ pos 1)))))

(define (string->list s)
  (string->list-h s 0))

(define (string-fill!-h str char pos len)
  (if (< pos len)
    (begin (string-set! str pos char)
         (string-fill!-h str char (+ pos 1) len))
    str))

(define (string-fill! str char)
  (string-fill!-h str char 0 (string-length str)))

(define (make-string size . init)
  (let ((s ($make-string size)))
    (if (not (null? init))
        (string-fill! s (car init)))
    s))

(define (list->string-h l str pos) ;; TODO : char list
  (if (null? l)
     str
     (begin (string-set! str pos (car l))
            (list->string-h (cdr l) str (+ pos 1)))))

(define (list->string l)
  (let ((str (make-string (length l))))
     (list->string-h l str 0)))

(define (string-h str chars pos)
  (if (null? chars)
     str
     (begin (string-set! str pos (car chars))
            (string-h str (cdr chars) (+ pos 1)))))

(define (string . chars)
   (if (null? chars)
      ""
      (let ((str (make-string (length chars))))
         (string-h str chars 0))))

(define (substring-h to from posf post end)
  (if (= posf end)
     to
     (begin (string-set! to post (string-ref from posf))
            (substring-h to from (+ posf 1) (+ post 1) end))))

(define (substring string start end)
   (if (or (< start 0) (> end (string-length string)) (< end start))
      "" ;; TODO error
      (let ((new-str (make-string (- end start))))
        (substring-h new-str string start 0 end))))

(define (string-append-two str str2)
  (let* ((l1 (string-length str))
       (l2 (string-length str2))
       (new-str (make-string (+ l1 l2))))
    (do ((pos 0 (+ pos 1)))
      ((= pos (+ l1 l2)) new-str)
      (if (< pos l1)
         (string-set! new-str pos (string-ref str pos))
         (string-set! new-str pos (string-ref str2 (- pos l1)))))))

(define (string-append-h strings)
   (cond ((null? strings) "")
         ((null? (cdr strings)) (car strings))
         (else (string-append-h (cons (string-append-two (car strings) (cadr strings))
                                      (cddr strings))))))

(define (string-append . strings)
   (string-append-h strings))

(define (string-copy str)
   (do ((new-str (make-string (string-length str)))
        (pos 0 (+ pos 1)))
       ((= pos (string-length str)) new-str)
       (string-set! new-str pos (string-ref str pos))))

(define (string=?-h str1 str2 pos)
   (cond ((= pos (string-length str1)) (= pos (string-length str2)))
         ((= pos (string-length str2)) #f)
         (else (if (char=? (string-ref str1 pos) (string-ref str2 pos))
                  (string=?-h str1 str2 (+ pos 1))
                  #f))))

(define (string=? str1 str2)
  (string=?-h str1 str2 0))

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

(define (string->symbol str)
  ($string->symbol str))

(define (symbol->string sym)
  ($symbol->string sym))

;; TODO wrong place
(define (vector . l) (list->vector l))

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

(define (print-string str pos len)
  (if (< pos len)
     (begin (print (string-ref str pos))
            (print-string str (+ pos 1) len))))

(set! print
  (lambda (n)
    (cond ((null? n) #f)
          ((number? n) (print-nb n))
          ((char? n) (print-char n))
          ((procedure? n) (print-procedure n))
          ((pair? n) (begin (print (car n))
                            (print (cdr n))))
          ((vector? n) (print-vector n 0 (vector-length n)))
          ((string? n) (print-string n 0 (string-length n)))
          ((symbol? n)
             (let ((str (symbol->string n)))
               (print-string str 0 (string-length str))))
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
       (cond ((eq? v  0) (print "nul"))
             ((<   v  7) (begin (print "x0") (print v)))
             ((eq? v  7) (print "alarm"))
             ((eq? v  8) (print "backspace"))
             ((eq? v  9) (print "tab"))
             ((eq? v 10) (print "newline"))
             ((eq? v 11) (print "vtab"))
             ((eq? v 12) (print "page"))
             ((eq? v 13) (print "return"))
             ((eq? v 14) (print "x0e"))
             ((eq? v 15) (print "x0f"))
             ((<   v 26) (begin (print "x") (print (- v 6))))
             ((eq? v 26) (print "x1a"))
             ((eq? v 27) (print "esc"))
             ((eq? v 28) (print "x1c"))
             ((eq? v 29) (print "x1d"))
             ((eq? v 30) (print "x1e"))
             ((eq? v 31) (print "x1f"))
             ((eq? v 32) (print "space"))
             (else (print "TODO"))))))

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

(define (pp-string-h string idx length)
  (if (< idx length)
     (begin (print (string-ref string idx))
            (pp-string-h string (+ idx 1) length))))

(define (pp-string string)
  ($$putchar 34)
  (pp-string-h string 0 (string-length string))
  ($$putchar 34))

(set! pp-h (lambda (n)
  (cond ((null? n) (begin ($$putchar 40) ($$putchar 41))) ;; ()
        ((number? n) (print-nb n))
        ((char? n) (pp-char n))
        ((procedure? n) (print-procedure n))
        ((pair? n) (pp-pair n))
        ((vector? n) (pp-vector n))
        ((string? n) (pp-string n))
        ((symbol? n)
           (let ((str (symbol->string n)))
             (print-string str 0 (string-length str))))
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

