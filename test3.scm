

(define (foo)

  (let ((b 500)
        (c 400)
        (d 300)
        (e 200)
        (f 100))

    (define (bar y a b c d) 1)

    (bar 1 b b b d)
    b))

(foo)
