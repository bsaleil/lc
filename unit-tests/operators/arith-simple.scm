;;---------------------------------------------------------------------------
;;
;;  Copyright (c) 2015, Baptiste Saleil. All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;   1. Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;   2. Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;   3. The name of the author may not be used to endorse or promote
;;      products derived from this software without specific prior written
;;      permission.
;;
;;  THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED
;;  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
;;  NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;  NOT LIMITED TO PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;
;;---------------------------------------------------------------------------

(println (+ 0 0))
(println (+ 0 -0))
(println (+ 10 0))
(println (+ 0 10))
(println (+ 0 -10))
(println (+ 10 10))
(println (+ -10 10))
(println (+ (+ 2 1) (+ 2 2)))

(println 123456789)

(println (- 0 0))
(println (- 0 -0))
(println (- 10 0))
(println (- 0 10))
(println (- 0 -10))
(println (- 10 10))
(println (- -10 10))
(println (- (+ -10 15) (- 10 12)))

(println 123456789)

(println (* 0 0))
(println (* 0 -0))
(println (* 10 0))
(println (* 0 10))
(println (* 0 -10))
(println (* 10 10))
(println (* -10 10))
(println (- (+ -10 15) (- 10 (* 2 6))))

(println 123456789)

(println (quotient 0 10))
(println (quotient 9 10))
(println (quotient 10 10))
(println (quotient 11 10))
(println (quotient -10 10))
(println (quotient 10 -10))
(println (quotient -10 -10))
(println (- (+ -10 (quotient 156 10)) (- 10 (* 2 6))))

(println 123456789)

(println (modulo 0 10))
(println (modulo 9 10))
(println (modulo 9 -10))
(println (modulo -9 10))
(println (modulo -9 -10))
(println (modulo 10 10))
(println (modulo 11 10))
(println (modulo -10 10))
(println (modulo 10 -10))
(println (modulo -10 -10))
(println (- (+ -10 (quotient 156 10)) (- 10 (* 2 (modulo 48 7)))))

;0
;0
;10
;10
;-10
;20
;0
;7
;123456789
;0
;0
;10
;-10
;10
;0
;-20
;7
;123456789
;0
;0
;0
;0
;0
;100
;-100
;7
;123456789
;0
;0
;1
;1
;-1
;-1
;1
;7
;123456789
;0
;9
;-1
;1
;-9
;0
;1
;0
;0
;0
;7
