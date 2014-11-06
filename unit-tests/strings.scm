;; make-string
;;;(define s1 (make-string 10))
(define s1 (make-string 10 #\0))
(define s2 (make-string 10 #\a))

(pp s1)
(pp s2)

;; string-fill!
(string-fill! s1 #\b)

(pp s1)
(pp s2)

;; string-set!
(string-set! s2 0 #\?)
(string-set! s2 (- (string-length s2) 1) #\?)

(pp s1)
(pp s2)

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

;; string-length
(define s12 "Hello")
(define s13 (string-append "Dark " (list->string (list #\V #\a #\d #\o #\r))))
(define s14 (list->string '()))

(pp (string-length s12))
(pp (string-length s13))
(pp (string-length s14))

;; substring
(define s15 (string-append "Dark " (list->string (list #\V #\a #\d #\o #\r))))

(pp (substring s15 0 0))
(pp (substring s15 0 (string-length s15)))
(pp (substring s15 3 7))

;; string-ref
(define s16 "Hello")
(define s17 (string-append "Dark " (list->string (list #\V #\a #\d #\o #\r))))

(pp (string-ref s16 0))
(pp (string-ref s16 3))
(pp (string-ref s17 4))
(pp (string-ref s17 9))

;; string-copy
(define s18 (make-string 5 #\Z))
(define s19 (string-copy s18))

(pp s18)
(pp s19)

(string-set! s18 4 #\U)

(pp s18)
(pp s19)

;; string=?
(define s20 "Hello")
(define s21 "Hello World")
(define s22 " Hello")
(define s23 (list->string (list #\H #\e #\l #\l #\o)))
(define s24 "")
(define s25 (make-string 0))

(pp (string=? s20 s20))
(pp (string=? s20 s21))
(pp (string=? s20 s22))
(pp (string=? s20 s23))
(pp (string=? s20 s24))
(pp (string=? s24 s24))
(pp (string=? s24 s23))
(pp (string=? s24 s25))
(pp (string=? s21 s22))

;"0000000000"
;"aaaaaaaaaa"
;"bbbbbbbbbb"
;"aaaaaaaaaa"
;"bbbbbbbbbb"
;"?aaaaaaaa?"
;"Hi."
;""
;(#\H #\e #\l #\l #\o)
;(#\a #\a #\a #\a)
;()
;"Hello!"
;""
;"Dark Vador"
;"A"
;"B"
;""
;"Hi"
;5
;10
;0
;""
;"Dark Vador"
;"k Va"
;#\H
;#\l
;#\space
;#\r
;"ZZZZZ"
;"ZZZZZ"
;"ZZZZU"
;"ZZZZZ"
;#t
;#f
;#f
;#t
;#f
;#t
;#f
;#t
;#f
