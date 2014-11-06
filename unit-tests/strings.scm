;; make-string
(define s1 (make-string 10))
(define s2 (make-string 10 #\a))

(println s1)
(println s2)

;; string-fill!
(string-fill! s1 #\b)

(println s1)
(println s2)

;; string-set!
(string-set! s2 0 #\?)
(string-set! s2 (- (string-length s2) 1) #\?)

(println s1)
(println s2)

;; string<->list
(define s3 (list->string (list #\H #\i #\.)))
(define s4 (list->string '()))
(define l1 (string->list "Hello"))
(define l2 (string->list (make-string 4 #\a)))
(define l3 (string->list ""))

(pp s3)
(pp s4)
(pp l1)
(pp l2)
(pp l3)

;; string
(define s5 (string #\H #\e #\l #\l #\o #\!))
(define s6 (string))

(pp s5)
(pp s6)

;; string-append
(define s7  (string-append "Dark " (list->string (list #\V #\a #\d #\o #\r))))
(define s8  (string-append "A" ""))
(define s9  (string-append "" "B"))
(define s10 (string-append))
(define s11 (string-append "Hi"))

(pp s7)
(pp s8)
(pp s9)
(pp s10)
(pp s11)

;;