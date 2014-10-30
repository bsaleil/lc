
(pp (char=? #\A #\B))
(pp (char=? #\A #\A))
(pp (char=? #\B #\A))
(pp (char<? #\A #\B))
(pp (char<? #\A #\A))
(pp (char<? #\B #\A))
(pp (char>? #\A #\B))
(pp (char>? #\A #\A))
(pp (char>? #\B #\A))
(pp (char<=? #\A #\B))
(pp (char<=? #\A #\A))
(pp (char<=? #\B #\A))
(pp (char>=? #\A #\B))
(pp (char>=? #\A #\A))
(pp (char>=? #\B #\A))

(pp 1234567890)

(pp (char=? #\a #\b))
(pp (char=? #\a #\a))
(pp (char=? #\b #\a))
(pp (char<? #\a #\b))
(pp (char<? #\a #\a))
(pp (char<? #\b #\a))
(pp (char>? #\a #\b))
(pp (char>? #\a #\a))
(pp (char>? #\b #\a))
(pp (char<=? #\a #\b))
(pp (char<=? #\a #\a))
(pp (char<=? #\b #\a))
(pp (char>=? #\a #\b))
(pp (char>=? #\a #\a))
(pp (char>=? #\b #\a))

(pp 1234567890)

(pp (char<? #\0 #\9))
(pp (char<? #\5 #\5))
(pp (char<? #\9 #\0))
(pp (char>? #\0 #\9))
(pp (char>? #\5 #\5))
(pp (char>? #\9 #\0))
(pp (char<=? #\0 #\9))
(pp (char<=? #\5 #\5))
(pp (char<=? #\9 #\0))
(pp (char>=? #\0 #\9))
(pp (char>=? #\5 #\5))
(pp (char>=? #\9 #\0))

(pp 1234567890)

(pp (char=? #\a #\1))
(pp (char=? #\A #\1))
(pp (char=? #\1 #\a))
(pp (char=? #\1 #\A))
(pp (char=? #\a #\A))
(pp (char=? #\A #\a))
(pp (char<? #\a #\1))
(pp (char<? #\A #\1))
(pp (char<? #\1 #\a))
(pp (char<? #\1 #\A))
(pp (char<? #\a #\A))
(pp (char<? #\A #\a))
(pp (char>? #\a #\1))
(pp (char>? #\A #\1))
(pp (char>? #\1 #\a))
(pp (char>? #\1 #\A))
(pp (char>? #\a #\A))
(pp (char>? #\A #\a))
(pp (char<=? #\a #\1))
(pp (char<=? #\A #\1))
(pp (char<=? #\1 #\a))
(pp (char<=? #\1 #\A))
(pp (char<=? #\a #\A))
(pp (char<=? #\A #\a))
(pp (char>=? #\a #\1))
(pp (char>=? #\A #\1))
(pp (char>=? #\1 #\a))
(pp (char>=? #\1 #\A))
(pp (char>=? #\a #\A))
(pp (char>=? #\A #\a))

(pp 1234567890)

(pp (char-alphabetic? #\`))
(pp (char-alphabetic? #\a))
(pp (char-alphabetic? #\s))
(pp (char-alphabetic? #\z))
(pp (char-alphabetic? #\{))
(pp (char-alphabetic? #\@))
(pp (char-alphabetic? #\A))
(pp (char-alphabetic? #\S))
(pp (char-alphabetic? #\Z))
(pp (char-alphabetic? #\[))
(pp (char-alphabetic? #\_))
(pp (char-alphabetic? #\5))

(pp 1234567890)

(pp (char-numeric? #\/))
(pp (char-numeric? #\0))
(pp (char-numeric? #\5))
(pp (char-numeric? #\9))
(pp (char-numeric? #\:))
(pp (char-numeric? #\e))
(pp (char-numeric? #\E))
(pp (char-numeric? #\+))

(pp 1234567890)

(pp (char-whitespace? #\a))
(pp (char-whitespace? #\6))
(pp (char-whitespace? #\A))
(pp (char-whitespace? #\+))
(pp (char-whitespace? #\{))
(pp (char-whitespace? #\space))
(pp (char-whitespace? #\tab))
(pp (char-whitespace? #\newline))
(pp (char-whitespace? #\page))
(pp (char-whitespace? #\return))

(pp 1234567890)

(pp (char-upper-case? #\a))
(pp (char-upper-case? #\6))
(pp (char-upper-case? #\{))
(pp (char-upper-case? #\*))
(pp (char-upper-case? #\return))
(pp (char-upper-case? #\A))
(pp (char-upper-case? #\S))
(pp (char-upper-case? #\Z))
(pp (char-upper-case? #\@))
(pp (char-upper-case? #\[))

(pp 1234567890)

(pp (char-lower-case? #\A))
(pp (char-lower-case? #\6))
(pp (char-lower-case? #\{))
(pp (char-lower-case? #\*))
(pp (char-lower-case? #\return))
(pp (char-lower-case? #\a))
(pp (char-lower-case? #\s))
(pp (char-lower-case? #\z))
(pp (char-lower-case? #\`))
(pp (char-lower-case? #\{))

(pp 1234567890)
                                          
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

(pp 1234567890)

(pp (char-ci=? #\A #\B))
(pp (char-ci=? #\A #\A))
(pp (char-ci=? #\B #\A))
(pp (char-ci<? #\A #\B))
(pp (char-ci<? #\A #\A))
(pp (char-ci<? #\B #\A))
(pp (char-ci>? #\A #\B))
(pp (char-ci>? #\A #\A))
(pp (char-ci>? #\B #\A))
(pp (char-ci<=? #\A #\B))
(pp (char-ci<=? #\A #\A))
(pp (char-ci<=? #\B #\A))
(pp (char-ci>=? #\A #\B))
(pp (char-ci>=? #\A #\A))
(pp (char-ci>=? #\B #\A))

(pp 1234567890)

(pp (char-ci=? #\a #\b))
(pp (char-ci=? #\a #\a))
(pp (char-ci=? #\b #\a))
(pp (char-ci<? #\a #\b))
(pp (char-ci<? #\a #\a))
(pp (char-ci<? #\b #\a))
(pp (char-ci>? #\a #\b))
(pp (char-ci>? #\a #\a))
(pp (char-ci>? #\b #\a))
(pp (char-ci<=? #\a #\b))
(pp (char-ci<=? #\a #\a))
(pp (char-ci<=? #\b #\a))
(pp (char-ci>=? #\a #\b))
(pp (char-ci>=? #\a #\a))
(pp (char-ci>=? #\b #\a))

(pp 1234567890)

(pp (char-ci<? #\0 #\9))
(pp (char-ci<? #\5 #\5))
(pp (char-ci<? #\9 #\0))
(pp (char-ci>? #\0 #\9))
(pp (char-ci>? #\5 #\5))
(pp (char-ci>? #\9 #\0))
(pp (char-ci<=? #\0 #\9))
(pp (char-ci<=? #\5 #\5))
(pp (char-ci<=? #\9 #\0))
(pp (char-ci>=? #\0 #\9))
(pp (char-ci>=? #\5 #\5))
(pp (char-ci>=? #\9 #\0))

(pp 1234567890)

(pp (char-ci=? #\a #\1))
(pp (char-ci=? #\A #\1))
(pp (char-ci=? #\1 #\a))
(pp (char-ci=? #\1 #\A))
(pp (char-ci=? #\a #\A))
(pp (char-ci=? #\A #\a))
(pp (char-ci<? #\a #\1))
(pp (char-ci<? #\A #\1))
(pp (char-ci<? #\1 #\a))
(pp (char-ci<? #\1 #\A))
(pp (char-ci<? #\a #\A))
(pp (char-ci<? #\A #\a))
(pp (char-ci>? #\a #\1))
(pp (char-ci>? #\A #\1))
(pp (char-ci>? #\1 #\a))
(pp (char-ci>? #\1 #\A))
(pp (char-ci>? #\a #\A))
(pp (char-ci>? #\A #\a))
(pp (char-ci<=? #\a #\1))
(pp (char-ci<=? #\A #\1))
(pp (char-ci<=? #\1 #\a))
(pp (char-ci<=? #\1 #\A))
(pp (char-ci<=? #\a #\A))
(pp (char-ci<=? #\A #\a))
(pp (char-ci>=? #\a #\1))
(pp (char-ci>=? #\A #\1))
(pp (char-ci>=? #\1 #\a))
(pp (char-ci>=? #\1 #\A))
(pp (char-ci>=? #\a #\A))
(pp (char-ci>=? #\A #\a))

;#f
;#t
;#f
;#t
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;1234567890
;#f
;#t
;#f
;#t
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;1234567890
;#t
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;1234567890
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#f
;#f
;#f
;#t
;#t
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#f
;1234567890
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#f
;1234567890
;#f
;#t
;#t
;#t
;#f
;#f
;#f
;#f
;1234567890
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#t
;#t
;1234567890
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;1234567890
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;1234567890
;#\A
;#\A
;#\?
;#\}
;#\0
;#\Z
;#\Z
;#\R
;#\R
;1234567890
;#\a
;#\a
;#\?
;#\}
;#\0
;#\z
;#\z
;#\r
;#\r
;1234567890
;#f
;#t
;#f
;#t
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;1234567890
;#f
;#t
;#f
;#t
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;1234567890
;#t
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#f
;#f
;#t
;#t
;1234567890
;#f
;#f
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;#t
;#f
;#f
;#f
;#f
;#f
;#f
;#t
;#t
;#t
;#t
;#t
;#t
;#f
;#f
;#t
;#t
