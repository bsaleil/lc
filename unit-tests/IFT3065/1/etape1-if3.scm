(println
 (let ((x 11))
   (let ((y 33))
     (if (let ((y (* x 1)))
           (= x y))
         (+ x y)
         (* x y)))))

;44
