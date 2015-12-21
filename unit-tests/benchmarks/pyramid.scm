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

;;; Pyramid

(define (print-pyramid base)

  (define (str-triangles n)

    (define (fill-str-triangles str pos)
    (if (>= pos (string-length str))
      str
      (begin (string-set! str (+ pos 0) (integer->char 226))
             (string-set! str (+ pos 1) (integer->char 150))
             (string-set! str (+ pos 2) (integer->char 179))
             (fill-str-triangles str (+ pos 3)))))

    (let ((str (make-string (* n 3))))
      (fill-str-triangles str 0)))

  (define (line n)
     (let ((sup (quotient (- base n) 2)))
       (println (string-append (make-string sup #\space)
                               (str-triangles n)
                               (make-string sup #\space)))))

  (if (= (modulo base 2) 0)
      (println "ERR")
      (do ((i 1 (+ i 2)))
          ((= i (+ base 2)) #t)
          (line i))))

(print-pyramid 51)

;                         △                         
;                        △△△                        
;                       △△△△△                       
;                      △△△△△△△                      
;                     △△△△△△△△△                     
;                    △△△△△△△△△△△                    
;                   △△△△△△△△△△△△△                   
;                  △△△△△△△△△△△△△△△                  
;                 △△△△△△△△△△△△△△△△△                 
;                △△△△△△△△△△△△△△△△△△△                
;               △△△△△△△△△△△△△△△△△△△△△               
;              △△△△△△△△△△△△△△△△△△△△△△△              
;             △△△△△△△△△△△△△△△△△△△△△△△△△             
;            △△△△△△△△△△△△△△△△△△△△△△△△△△△            
;           △△△△△△△△△△△△△△△△△△△△△△△△△△△△△           
;          △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△          
;         △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△         
;        △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△        
;       △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△       
;      △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△      
;     △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△     
;    △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△    
;   △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△   
;  △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△  
; △△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△ 
;△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△△
