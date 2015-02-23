
;; Predicates

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

(define (port? p)
  ($port? p))

(define (boolean? n)
  (cond ((eq? n #t) #t)
        ((eq? n #f) #t)
        (else #f)))

(define (null? n)
	(eq? n '()))


;; Conversion


(define (vector->list v)
   (vector->list-h v 0 (vector-length v)))

(define (vector->list-h vector idx length)
   (if (= idx length)
      '()
      (cons (vector-ref vector idx) (vector->list-h vector (+ idx 1) length))))

(define (list->vector l)
  (let ((v (make-vector (length l))))
     (list->vector-h l v 0 (length l))))

(define (list->vector-h lst vec pos len)
  (if (null? lst)
    vec
    (begin (vector-set! vec pos (car lst))
           (list->vector-h (cdr lst) vec (+ pos 1) len))))

(define (string->symbol str)
  ($string->symbol str))

(define (symbol->string sym)
  ($symbol->string sym))

(define (number->string num)
  (define (digit->string d)
    (make-string 1 (integer->char (+ d 48))))
  (define (number->string-h num)
    (if (= num 0)
        ""
        (string-append (number->string-h (quotient num 10))
                       (digit->string    (modulo   num 10)))))
  (cond ((= num 0) "0")
        ((< num 0) (string-append "-" (number->string-h (* num -1))))
        (else (number->string-h num))))

(define (string->number str . l)
  (define (s->n str pos)
    (if (= pos (string-length str))
      ""
      (let ((c (string-ref str pos)))
        (if (char-numeric? c)
          ;;
          (let ((r (s->n str (+ pos 1))))
            (if r
               (string-append (make-string 1 c) r)
               #f))
          ;;
          #f))))
  (if (= (string-length str) 0)
     #f
     (s->n str 0)))