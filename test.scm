
(##apply (lambda () 1) '())

;; Pourquoi on a besoin de encoding->obj d'une procédure ? (voir values.scm)
;; -> il faut génrer les variables libres. On doit lire une variable depuis la fermeture encodée en nan boxing
