
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
