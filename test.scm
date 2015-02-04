;; Heap test
;; Test (lambda x x)
;; Test definition interne
;; Test quoted vector ex. '#(1 2 "Hello" ...)
;; Test set-car, set-cdr + Check err
;; Test bind define
;; Test appel terminal
;; Faire egalites conformes (= eq eqv equal)



; (pp ((lambda x x) 1 2 3 4 5))

; ;(pp '#())
; (pp '#(1 2 3 4 5))
; (pp '#("Hello" " " "World" " " "!"))
; (pp (vector-length '#(1 "Hello" #f " " 'test "World" '(1 2) " " #\P "!"))) ;; Pb si liste non quotee

; (let ((l '(1 2 3 4)))
;   (pp l)
;   (set-car! l "Hello")
;   (pp l)
;   (set-cdr! l '(10 20 30))
;   (pp l))

; (define (print-pyramid base)
  
;   (define (line n)
;      (let ((sup (quotient (- base n) 2)))
;        (println (string-append (make-string sup #\space)
;                                (make-string n   #\A)
;                                (make-string sup #\space)))))
  
;   (if (= (modulo base 2) 0)
;       (println "B")
;       (do ((i 1 (+ i 2)))
;           ((= i base) #t)
;           (line i))))

; (print-pyramid 123)
  

