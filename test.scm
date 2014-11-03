;; STRINGS. Tester :
;; string?
;; string-length
;; string-ref
;; make-string (+ ajouter avec param rest)

;($make-string 10)
; (pp (string-length "test"))

; (pp (string? "Hello"))

; (println (string-ref (make-string 3) 0))
; (println (string-ref (make-string 3) 1))
; (println (string-ref (make-string 3) 2))

(define (string->list-h s pos)
   (pp (string-length s))
   (pp (cons (string-ref s 0) '())))

(define (string->list s)
   (string->list-h s 0))

(pp (string->list-h "bonjour" 0))