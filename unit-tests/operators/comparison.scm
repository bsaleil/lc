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

(println "=")

(println (=))
(println (=  10))
(println (=  10.2))
(println (=  10  10))
(println (=  10 -10))
(println (= -10  10))
(println (= -10 -10))
(println (=  10   9))
(println (=   9  10))
(println (= (- 20 10) (+ 5 5)))
(println (= (- 21 10) (+ 5 5)))
(println (= (- 20 10) (+ 5 6)))
(println (= 1.2 1.2))
(println (= 1.1 1.2))
(println (= 1.2 1.1))
(println (= 10 1.2))
(println (= 1.2 10))
(println (= 10 10.0))
(println (= 10.0 10))
(println (= 10 1.2))
(println (= 1 1.2 4 3.4))
(println (= 1 1.0 1.0 1))
(println (= 1 1.2 2 2.3 3 3.4))
(println (= 8.8 8 7 5.6 3 1.1))
(println (= 1.3 -1.3))
(println (= -3.2 -3.2))

(println "<")

(println (<))
(println (<  20))
(println (<  20.2))
(println (<  10  10))
(println (<  10 -10))
(println (< -10  10))
(println (< -10 -10))
(println (<  10   9))
(println (<   9  10))
(println (< (- 20 10) (+ 5 5)))
(println (< (- 21 10) (+ 5 5)))
(println (< (- 20 10) (+ 5 6)))
(println (< 1.2 1.2))
(println (< 1.1 1.2))
(println (< 1.2 1.1))
(println (< 10 1.2))
(println (< 1.2 10))
(println (< 10 10.0))
(println (< 10.0 10))
(println (< 10 1.2))
(println (< 1 1.2 4 3.4))
(println (< 1 1.0 1.0 1))
(println (< 1 1.2 2 2.3 3 3.4))
(println (< 8.8 8 7 5.6 3 1.1))
(println (< 1.3 -1.3))
(println (< -3.2 -3.2))

(println ">")

(println (>))
(println (>  40))
(println (>  40.2))
(println (>  10  10))
(println (>  10 -10))
(println (> -10  10))
(println (> -10 -10))
(println (>  10   9))
(println (>   9  10))
(println (> (- 20 10) (+ 5 5)))
(println (> (- 21 10) (+ 5 5)))
(println (> (- 20 10) (+ 5 6)))
(println (> 1.2 1.2))
(println (> 1.1 1.2))
(println (> 1.2 1.1))
(println (> 10 1.2))
(println (> 1.2 10))
(println (> 10 10.0))
(println (> 10.0 10))
(println (> 10 1.2))
(println (> 1 1.2 4 3.4))
(println (> 1 1.0 1.0 1))
(println (> 1 1.2 2 2.3 3 3.4))
(println (> 8.8 8 7 5.6 3 1.1))
(println (> 1.3 -1.3))
(println (> -3.2 -3.2))

(println "<=")

(println (<=))
(println (<= 30))
(println (<= 30.2))
(println (<=  10  10))
(println (<=  10 -10))
(println (<= -10  10))
(println (<= -10 -10))
(println (<=  10   9))
(println (<=   9  10))
(println (<= (- 20 10) (+ 5 5)))
(println (<= (- 21 10) (+ 5 5)))
(println (<= (- 20 10) (+ 5 6)))
(println (<= 1.2 1.2))
(println (<= 1.1 1.2))
(println (<= 1.2 1.1))
(println (<= 10 1.2))
(println (<= 1.2 10))
(println (<= 10 10.0))
(println (<= 10.0 10))
(println (<= 10 1.2))
(println (<= 1 1.2 4 3.4))
(println (<= 1 1.0 1.0 1))
(println (<= 1 1.2 2 2.3 3 3.4))
(println (<= 8.8 8 7 5.6 3 1.1))
(println (<= 1.3 -1.3))
(println (<= -3.2 -3.2))

(println ">=")

(println (>=))
(println (>= 50))
(println (>= 50.2))
(println (>=  10  10))
(println (>=  10 -10))
(println (>= -10  10))
(println (>= -10 -10))
(println (>=  10   9))
(println (>=   9  10))
(println (>= (- 20 10) (+ 5 5)))
(println (>= (- 21 10) (+ 5 5)))
(println (>= (- 20 10) (+ 5 6)))
(println (>= 1.2 1.2))
(println (>= 1.1 1.2))
(println (>= 1.2 1.1))
(println (>= 10 1.2))
(println (>= 1.2 10))
(println (>= 10 10.0))
(println (>= 10.0 10))
(println (>= 10 1.2))
(println (>= 1 1.2 4 3.4))
(println (>= 1 1.0 1.0 1))
(println (>= 1 1.2 2 2.3 3 3.4))
(println (>= 8.8 8 7 5.6 3 1.1))
(println (>= 1.3 -1.3))
(println (>= -3.2 -3.2))

;=
;#t
;#t
;#t
;#t
;#f
;#f
;#t
;#f
;#f
;#t
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;#f
;#f
;#f
;#t
;<
;#t
;#t
;#t
;#f
;#f
;#t
;#f
;#f
;#t
;#f
;#f
;#t
;#f
;#t
;#f
;#f
;#t
;#f
;#f
;#f
;#f
;#f
;#t
;#f
;#f
;#f
;>
;#t
;#t
;#t
;#f
;#t
;#f
;#f
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
;#f
;#f
;#t
;#f
;#f
;#f
;#t
;#t
;#f
;<=
;#t
;#t
;#t
;#t
;#f
;#t
;#t
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
;#t
;#t
;#f
;#f
;#t
;#t
;#f
;#f
;#t
;>=
;#t
;#t
;#t
;#t
;#t
;#f
;#t
;#t
;#f
;#t
;#t
;#f
;#t
;#f
;#t
;#t
;#f
;#t
;#t
;#t
;#f
;#t
;#f
;#t
;#t
;#t
