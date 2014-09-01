;; TODO dump, hardcoded

;;
;; REGISTERS DUMP
;;

;; dump registers label
(define label-dump-regs #f)

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
         (regs-name '("RAX" "RBX" "RCX" "RDX" "RSP" "RBP" "RSI" "RDI"
                      "R8 "  "R9 "  "R10" "R11" "R12" "R13" "R14" "R15"))
         (regs-val (get-regs-val regs-pos sp)))
     
    (pp-regs regs-name regs-val)))

;;
;; STACK DUMP
;;

;; dump stack label
(define label-dump-stack #f)

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