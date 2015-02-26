
(define (string-length s)
  ($string-length s))

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

;; Rewrite stirng<?
(define (string<? str1 str2)
  (define (string<?-h str1 str2 pos)
    (cond ((= pos (- (string-length str2) 1))
             (char<? (string-ref str1 pos)
                     (string-ref str2 pos)))
          ((= pos (- (string-length str1) 1))
             (char<=? (string-ref str1 pos)
                     (string-ref str2 pos)))
          (else (and (char<=? (string-ref str1 pos)
                             (string-ref str2 pos))
                     (string<?-h str1 str2 (+ pos 1))))))
  
  (cond ((= (string-length str1) 0)
           (> (string-length str2) 0))
        ((= (string-length str2) 0)
           #f)
        (else (string<?-h str1 str2 0))))
