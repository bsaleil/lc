
(define (fact n)
  (if (<= n 0)
      1
      (fact (- n 1))))

(gambit$$pp (fact 10.0))


;; Pourquoi on a besoin de encoding->obj d'une procédure ? (voir values.scm)
;; -> il faut génrer les variables libres. On doit lire une variable depuis la fermeture encodée en nan boxing
