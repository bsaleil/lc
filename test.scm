;; TODO : integer->char pour pouvoir faire
;;        char-upcase, char-downcase, et *ci?

; (define (integer->char n)
;   ($integer->char n))






(pp (char-upcase #\a))
(pp (char-upcase #\A))
(pp (char-upcase #\?))
(pp (char-upcase #\}))
(pp (char-upcase #\0))
(pp (char-upcase #\z))
(pp (char-upcase #\Z))
(pp (char-upcase #\r))
(pp (char-upcase #\R))

(pp 1234567890)

(pp (char-downcase #\a))
(pp (char-downcase #\A))
(pp (char-downcase #\?))
(pp (char-downcase #\}))
(pp (char-downcase #\0))
(pp (char-downcase #\z))
(pp (char-downcase #\Z))
(pp (char-downcase #\r))
(pp (char-downcase #\R))