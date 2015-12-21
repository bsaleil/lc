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

(define (read)

  ;; Port to read
  (define port (current-input-port))

  ;; Syntax error
  (define (syntax-error msg)
    (error msg))

  (define (objread-char o)
    (car o))
  (define (objread-line o)
    (cadr o))
  (define (objread-col o)
    (caddr o))
  (define (objread-next o)
    (if (char=? (objread-char o) #\newline)
        (list (read-char port) (+ (objread-line o) 1) 1)
        (list (read-char port) (objread-line o) (+ (objread-col o) 1))))
  (define (objret-new o val)
    (list val (objread-line o) (objread-col o)))
  (define (objret-val o)
    (car o))

  (define (read-num o num)
    (if (and (char>=? (objread-char o) #\0)
             (char<=? (objread-char o) #\9))
        (if (not num)
            (read-num (objread-next o) (objret-new o (- (char->integer (objread-char o)) 48)))
            (read-num (objread-next o) (objret-new num (+ (* (objret-val num) 10) (char->integer (objread-char o)) -48))))
        num))

  (let ((r (read-num (list (read-char port) 1 1) #f)))
    (objret-val r)))
