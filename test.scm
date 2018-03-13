
;; fibfp
;; sumfp
;; mbrot
;; pnpoly
;; simplex

;; nucleic (???)

(define (massoc lst)
  (car lst))

(let ((r '(0.1)))
   (gambit$$pp (+ (massoc r)
                  (massoc r))))


;; Pourquoi on a besoin de encoding->obj d'une procédure ? (voir values.scm)
;; -> il faut génrer les variables libres. On doit lire une variable depuis la fermeture encodée en nan boxing
