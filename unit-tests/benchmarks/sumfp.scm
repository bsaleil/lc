;;; SUMFP -- Compute sum of integers from 0 to 10000 using floating point

(##define-macro (def-macro form . body)
  `(##define-macro ,form (let () ,@body)))

(def-macro (FLOAT> . lst)    `(> ,@lst))
(def-macro (FLOAT>= . lst)    `(>= ,@lst))
(def-macro (FLOAT< . lst)    `(< ,@lst))
(def-macro (FLOAT<= . lst)    `(<= ,@lst))
(def-macro (FLOAT+ . lst)    `(+ ,@lst))
(def-macro (FLOAT- . lst)    `(- ,@lst))
(def-macro (FLOAT* . lst)    `(* ,@lst))
(def-macro (FLOAT/ . lst)    `(/ ,@lst))

(define (run n)
  (let loop ((i n) (sum 0.))
    (if (FLOAT< i 0.)
      sum
      (loop (FLOAT- i 1.) (FLOAT+ i sum)))))

(let ((result (run 10000.)))
  (println (= result 50005000.)))

;#t
