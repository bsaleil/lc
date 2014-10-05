
(letrec ((f (lambda (n)
              (if (= 0 n)
              	($$putchar 80) ;; P
              	(g (- n 1)))))
         (g (lambda (n)
              (if (= 0 n)
              	($$putchar 73) ;; I
              	(f (- n 1))))))
   (f 0)
   (f 1)
   (f 2)
   (f 3)
   (f 4)
   (f 5)
   ($$putchar 10))

