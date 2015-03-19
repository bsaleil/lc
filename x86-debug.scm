
(define label-breakpoint #f)
(define label-dump-regs  #f)
(define label-dump-stack #f)

;;-----------------------------------------------------------------------------
;; BREAKPOINT

;; Gen call to breakpoint label
(define (gen-breakpoint cgc)
  
  (push-pop-regs
     cgc
     all-regs
     (lambda (cgc)
       (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
       (x86-and  cgc (x86-rsp) (x86-imm-int -16))
       (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
       (x86-push cgc (x86-rdi))
       (x86-call cgc label-breakpoint) ;; call C function
       (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
       )))

;; Give a cmd to user to print registers or stack
(c-define (break_point sp) (long) void "break_point" ""
  
  (let ((reg-names '("R15" "R14" "R13" "R12" "R11" "R10" " R9" " R8"
                     "RDI" "RSI" "RBP" "RDX" "RCX" "RBX" "RAX" "RSP")))
  
  (define (get-regs regs offset)
    (if (null? regs)
      '()
      (cons (cons (car regs) (get-i64 (+ sp offset)))
            (get-regs (cdr regs) (+ offset 8)))))
  
  (define (println-slot slot)
    (let ((str (number->string slot)))
      (print slot (make-string (- 19 (string-length str)) #\space)))
    (print "|  ")
    (let ((tag (bitwise-and slot 3)))
      (cond ((= tag 0) (print (quotient slot 4)))
            ((= tag 1) (print "Mem object"))
            ((= slot ENCODING_VOID) (print "void"))
            ((= slot ENCODING_EOF)  (print "eof"))
            ((= tag 2) (print (encoding-obj slot)))))
    (newline))
  
  (define (print-stack n regs)
    (cond ((= n 0))
          ((= n 1)
             (println "       +--------------------+--------------------+")
             (print   "RSP -> | ") (println-slot (get-i64 (+ (* 8 (- n 1)) (cdr (assoc "RSP" regs)))))
             (println "       +--------------------+--------------------+"))
          (else
             (print-stack (- n 1) regs)
             (print   "       | ") (println-slot (get-i64 (+ (* 8 (- n 1)) (cdr (assoc "RSP" regs)))))
             (println "       +--------------------+--------------------+"))))
  
  (define (print-regs regs)
    (if (not (null? regs))
      (begin (print-regs (cdr regs))
             (print-reg  (car regs)))))
  
  (define (print-reg reg)
    (println (car reg) " = " (cdr reg)))
    
  (let ((regs (get-regs reg-names 0)))
    
    (define (run)
      (print ">")
      (let ((r (read-line)))
        (set! r (string-upcase r))
        (let ((reg (assoc r regs)))
          (cond ;; Print one reg
                ((assoc r regs) => (lambda (r) (print-reg r)))
                ;; Print all regs
                ((string=? r "REGS")  (print-regs regs))
                ;; Print stack (5 slots)
                ((string=? r "STACK") (print-stack 5 regs))
                ;; Print stack (n slots)
                ((and (> (string-length r) 6)
                      (string=? (substring r 0 5) "STACK")
                      (char=? (string-ref r 5) #\space)
                      (string->number (substring r 6 (string-length r))))
                  (let ((num (string->number (substring r 6 (string-length r)))))
                    (if (and (integer? num)
                             (> num 0))
                        (print-stack num regs)
                        (println "Unknown command"))))
                ;; Continue
                ((string=? r "NEXT"))
                ;; Unknown
                (else (println "Unknown command")))
          (if (not (string=? r "NEXT"))
            (run)))))
    
    (println "\033[1;31m⚫ Breakpoint\033[0m")
    (run))))


;;-----------------------------------------------------------------------------
;; REGISTERS

;; generate code to call the "dump registers" function
(define (gen-dump-regs cgc)
  (push-pop-regs
     cgc
     all-regs
     (lambda (cgc)
       (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
       (x86-and  cgc (x86-rsp) (x86-imm-int -16))
       (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
       (x86-push cgc (x86-rdi))
       (x86-call cgc label-dump-regs) ;; call C function
       (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
       )))

;; Get all regs vals from 'regs-pos' list    
(define (get-regs-val regs-pos sp)
  (if (null? regs-pos)
      '()
      (cons (get-i64 (+ sp (* (car regs-pos) 8))) (get-regs-val (cdr regs-pos) sp))))

;; Pretty print all couples reg name <-> reg val
(define (pp-regs regs-name regs-val)
  (if (null? regs-name)
      '()
      (begin (println "   " (string-append (car regs-name) " : " (number->string (car regs-val))))
             (pp-regs (cdr regs-name) (cdr regs-val)))))
        
;; Get registers values from stack and pretty print
;; NOTE : hardcoded registers positions in stack to match 'all-regs' list
(c-define (dump-regs sp) (long) void "dump_regs" ""
   (let* ((regs-pos '(15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0))
         (regs-name '("RSP" "RAX" "RBX" "RCX" "RDX" "RBP" "RSI" "RDI"
                      "R8 "  "R9 "  "R10" "R11" "R12" "R13" "R14" "R15"))
         (regs-val (get-regs-val regs-pos sp)))
     
    (pp-regs regs-name regs-val)))

;;-----------------------------------------------------------------------------
;; STACK

;; generate code to call the "dump stack" function
(define (gen-dump-stack cgc)
  (push-pop-regs
     cgc
     all-regs
     (lambda (cgc)
       (x86-mov  cgc (x86-rdi) (x86-rsp)) ;; align stack-pointer for C call
       (x86-and  cgc (x86-rsp) (x86-imm-int -16))
       (x86-sub  cgc (x86-rsp) (x86-imm-int 8))
       (x86-push cgc (x86-rdi))
       (x86-call cgc label-dump-stack) ;; call C function
       (x86-pop  cgc (x86-rsp)) ;; restore unaligned stack-pointer
       )))

;; Pretty print stack top values
(c-define (dump-stack sp) (long) void "dump_stack" ""
     (let ((offset (length all-regs))
           (slots-nb 10)) ;; TODO arg ?
       (println "-------------------------")
       (println "DUMP STACK :")
       (println "-------------------------")
       (println "RSP -> " (get-i64 (+ sp (* offset 8)))) ;; TODO 16 start
       (println "-------------------------")
       (print-stack-slots sp (+ offset 1) slots-nb)))

;; Print n stack slots from sp+pos*8
(define (print-stack-slots sp pos n)
  (if (= n 0)
      '()
      (let ((val (get-i64 (+ sp (* pos 8)))))
        (println "       " val)
        (println "-------------------------")
        (print-stack-slots sp (+ pos 1) (- n 1)))))

;;-----------------------------------------------------------------------------
;; SLOT DEBUG

(define (gen-set-slot cgc slot imm)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-push cgc (x86-rax))
    (x86-push cgc (x86-rbx))
    (x86-mov  cgc (x86-rax) (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov  cgc (x86-rbx) (x86-imm-int imm))
    (x86-mov  cgc (x86-mem 0 (x86-rax)) (x86-rbx))
    (x86-pop cgc (x86-rbx))
    (x86-pop cgc (x86-rax))))

(define (gen-get-slot cgc slot r)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-mov cgc r (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov cgc r (x86-mem 0 r))))

(define (gen-inc-slot cgc slot)
  (let ((res (assoc slot debug-slots)))
    (if (not res) (error "Compiler error"))
    (x86-push cgc (x86-rax))
    (x86-push cgc (x86-rbx))
    (x86-mov cgc (x86-rax) (x86-imm-int (+ block-addr (* (cdr res) 8))))
    (x86-mov cgc (x86-rbx) (x86-mem 0 (x86-rax)))
    (x86-inc cgc (x86-rbx))
    (x86-mov cgc (x86-mem 0 (x86-rax)) (x86-rbx))
    (x86-pop cgc (x86-rbx))
    (x86-pop cgc (x86-rax))))