

(define for
  (lambda (lo hi f)

    (define for-aux
      (lambda (lo)
        (if (< lo hi)
            (cons (f lo) (for-aux (+ lo 1)))
            '())))

    (for-aux lo)))

(define make-maze
  (lambda ()
    (let ((possible-holes
            (for 0 3 (lambda (i)
                        (for 0 3 (lambda (j)
                                   (gambit$$pp i)))))))

      #f)))

(make-maze)
