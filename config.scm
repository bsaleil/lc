
(define-macro (assert c err)
  `(if (not ,c)
       (begin
        (println "!!! ERROR : " ,err)
        (exit 1))))
