;; TODO : integer->char pour pouvoir faire
;;        char-upcase, char-downcase, et *ci?

(define (integer->char n)
  ($integer->char n))

(define (print-str-h str pos len)
  (if (< pos len)
     (begin (print ($string-ref str pos))
            (print-str-h str (+ pos 1) len))))

(define (print-str str)
  (print-str-h str 0 ($string-length str))
  ($$putchar 10))
  
(print-str "erer€oioi")