;; Verfier que si (+ a b a), le deuxieme a n'a pas de test de type
;; TODO : tests unitaires pour les nouvelles primitives inlinées
;;        (+,-*,...)
;; TODO : les tests de type sont toujours faits dans les prédicats (number?, ...) ?
;-------------------------------

(pp (>= 0 0 0 0 0))
(pp (>= 1 2 3 4 5))
(pp (>= 5 4 3 2 1))
(pp (>= 1 2 3 4 0))
(pp (>= 5 4 3 2 6))
(pp (>= 3 2 3 4 5))
(pp (>= 3 5 4 3 2))
(pp (>= 10 10))
(pp (>= 10 20))
(pp (>= 20 10))
(pp (>= 10))
(pp (>= -10))
(pp (>=))