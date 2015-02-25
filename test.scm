;; Verfier que si (+ a b a), le deuxieme a n'a pas de test de type
;; TODO : tests unitaires pour les nouvelles primitives inlinées
;;        (+,-*,...)
;; TODO : les tests de type sont toujours faits dans les prédicats (number?, ...) ?
;; TODO : test remainder
;-------------------------------

(pp (map println (or '((1 2)) '((3 4)))))


;(map (lambda (el) (car el)) (or '((1 2)) '((3 4))))

;; (pp (map (lambda (l) (car l)) '((1 2) (3 4) (5 6))))