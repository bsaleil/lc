;; STRINGS. Tester :
;; string?
;; string-length
;; string-ref
;; make-string (+ ajouter avec param rest)
;; string->list
;; println string
;; list->string
;; string-set!
; TODO : pp-string

(define s "BONJour")

(println s)
(println s)
(println (string? s))
(pp (string->list s))
;(pp s)

(string-set! s 0 #\a)
(println s)

(pp (string? (string->list s)))
(pp (list? (string->list s)))
(pp (string->list s))