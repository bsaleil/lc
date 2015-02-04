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
