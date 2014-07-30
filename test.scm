(define fact
  (lambda (n)
    (if (= n 0)
	  1
	  (if (= n 1)
	    1
		(* n (fact (- n 1)))))))

(fact 4)

(define fibo
  (lambda (n)
    (if (= n 0)
      0
      (if (= n 1)
        1
        (+ (fibo (- n 1)) (fibo (- n 2)))))))

(fibo 40)