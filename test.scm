(define (f a b)
  (pp a)
  (set! a 10)
  (pp a))

(f 100 200)


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