(let ((x 11))
  (let ((x (+ x x)))
    (let ((y (let ((x 2200))
               (+ x x))))
      (println (+ x y)))))

;4422
