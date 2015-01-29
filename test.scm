;; Heap test
;; Symbol test
;; Ajoute dans check-err les fonctions liées aux symboles
;; TESTS : symbol?, symbol<->string

; (let ((s 'montest))
;   (pp (symbol? s))
;   (pp s))

(pp (symbol->string (string->symbol (string-append "Bonjour" "Monde"))))


; (make-vector 200 100)
; (make-vector 200 100)
; (make-vector 200 100)
; (make-vector 200 100)


; (let ((v ($make-string 28)))
;   (pp (string-length v))
;   (pp v))