(define (fib n)
  (if (< n 2)
      (cons n '())
      (cons (+ (car (fib (- n 1))) (car (fib (- n 2)))) '())))

(println (car (fib 30)))
(println (car (fib 30)))
(println (car (fib 30)))
(println (car (fib 30)))
(println (car (fib 30)))

;832040
;832040
;832040
;832040
;832040


; (define aaaa 10)

; (define (f) (println 10010))

; (define (stress-heap n)
;   (if ($> n 0)
;       (begin ($make-vector 10000)
;              (stress-heap ($- n 1)))))

; (stress-heap 30)
; (f)


; ; ; ;; LENGTH 8 TAG pair(1)
; ; ; (define p (cons 1 2))

; ; ; ;; LENGTH 1 TAG mobject(2)
; ; ; ;;

; ; ; ;; LENGTH 12 TAG vector(0)
; ; ; ;; TODO (voir code)

; ; ; ;; LENGTH 20 TAG string(19)
; ; ; (define s "Test")

; ; ; ;; LENGTH 1 TAG procedure(14)
; ; ; (define (f a) (println a))

; ; ; ;; LENGTH 1 TAG procedure(14)
; ; ; ;; LENGTH 2 TAG procedure(14)
; ; ; (define (g a)
; ; ;   (lambda () (+ 1 a)))

; ; ; (g 10)