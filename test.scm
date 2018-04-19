
; (define-macro (shl a b) `(fxarithmetic-shift-left  ,a ,b))
; (define-macro (shr a b) `(fxarithmetic-shift-right ,a ,b))

(define (shl n i)
  (* n (expt 2 i)))

(define (shr n i)
  (if (and (< n 0) (odd? n))
      (error "NYI case shr")
      (quotient n (expt 2 i))))

(define (foo n m)
  (pp (shl n 2))
  (pp (shr n m)))

(foo 10 2)
;(gambit$$pp (foo 3.14))

; (define (foo v idx val)
;   (f64vector-set! v 0 5.0)
;   (f64vector-set! v 1 val)
;   (f64vector-set! v idx 7.0)
;   (f64vector-set! v (+ idx 1) (+ val 2)))
;
; (define v (make-f64vector 5 1.0))
;
; (pp (f64vector-length v))
; (pp (vector? v))
; (pp (f64vector? v))
;
; (pp (f64vector-ref v 0))
;
; (pp v)
; (foo v 2 6.0)
; (pp v)

; (define (bar n)
;   (f64vector 42.0 n (+ n 1) (+ n 2) (+ n 3) (+ n 4) (+ n 5)))
;
; (pp (bar 43.0))

;
; ;; imm f64vector !still
; (define (imm a)
;   (list (make-f64vector 10 1.1)
;         (make-f64vector 10 a)))
;
; (let ((r (imm 2.2)))
;   (pp (car r))
;   (pp (cadr r)))
;
; ;; imm f64vector still
; (define (imm-still a)
;   (list (make-f64vector 350 1.1)
;         (make-f64vector 350 a)))
;
; (let ((r (imm-still 2.2)))
;   (pp (car r))
;   (pp (cadr r)))
;
; ;; opn f64vector !still
; (define (opn size a)
;   (list (make-f64vector size 1.1)
;         (make-f64vector size a)))
;
; (let ((r (opn 5 2.2)))
;   (pp (car r))
;   (pp (cadr r)))
;
; ;; opn f64vector still
; (define (opn-still size a)
;   (list (make-f64vector size 1.1)
;         (make-f64vector size a)))
;
; (let ((r (opn-still 350 2.2)))
;   (pp (car r))
;   (pp (cadr r)))
