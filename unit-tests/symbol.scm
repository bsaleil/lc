(define sym 'TEST)

(println (symbol? sym))
(println sym)
(pp sym)

(let* ((sym 'OTHER)
       (str (symbol->string sym)))
  (string-set! str 0 #\A)
  (pp sym)
  (pp str))

(pp (string->symbol "Hello"))
(pp (symbol->string 'World))

;#t
;TEST
;TEST
;OTHER
;"ATHER"
;Hello
;"World"
