(define (heap-stress n)
  (if (> n 0)
      (begin ($make-vector 100)
             (heap-stress (- n 1)))))

(heap-stress 1000)


; ; ;; LENGTH 8 TAG pair(1)
; ; (define p (cons 1 2))

; ; ;; LENGTH 1 TAG mobject(2)
; ; ;;

; ; ;; LENGTH 12 TAG vector(0)
; ; ;; TODO (voir code)

; ; ;; LENGTH 20 TAG string(19)
; ; (define s "Test")

; ; ;; LENGTH 1 TAG procedure(14)
; ; (define (f a) (println a))

; ; ;; LENGTH 1 TAG procedure(14)
; ; ;; LENGTH 2 TAG procedure(14)
; ; (define (g a)
; ;   (lambda () (+ 1 a)))

; ; (g 10)