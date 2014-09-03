#lang racket
(require srfi/13)
(require srfi/14)

(define (readfile filename)     ;
  (call-with-input-file filename
    (λ (port)
      (read port))))

;(print (current-command-line-arguments)
; vector

;(vector-ref (current-command-line-arguments) 0)))

(define inputtest (readfile "25tests/test2.L1"))

(define AsmOutputFilename "prog.S")

(define (writefile filename output)
  (call-with-output-file filename
    (lambda (out)
      (fprintf out output))
    #:exists 'truncate))

(define (writefile-append filename output)
  (call-with-output-file filename
    (lambda (out)
      (fprintf out output))
    #:exists 'append))

; Code for generating labels
(define counter 0)
(define (new-lab)
  (set! counter (add1 counter))
  (string-append "_deflab" (number->string counter)))

; Main Prefix
(writefile AsmOutputFilename ".text
	.globl go
	.type	go, @function
go:

# save caller's base pointer 
pushl   %ebp
movl    %esp, %ebp

# save callee-saved registers
pushl   %ebx
pushl   %esi
pushl   %edi
pushl   %ebp

# body begins with base and
# stack pointers equal
movl    %esp, %ebp\n")


(define (instrtransl instruc)   ; instruction translation function
  (cond
    [(not (list? instruc)) (writefile-append AsmOutputFilename (string-append "\n_" (substring (symbol->string instruc) 1) ":"))]   ; Handles labels
    [(equal? (first instruc) `return) (writefile-append AsmOutputFilename (string-append "\nmovl %ebp, %esp\npopl %ebp\nret"))] ; Handles return
    [(equal? (first instruc) `call) (let* ([label (new-lab)])         ; Handles call
                                      (writefile-append AsmOutputFilename (string-append "\npushl $" label ""
                                                                                "\npushl %ebp"
                                                                                "\nmovl %esp, %ebp"
                                                                                "\njmp " (if (equal? #f
                                                                                                     (string-index (symbol->string (second instruc)) (string->char-set ":") 0 (string-length (symbol->string (second instruc)))))
                                                                                             (symbol->string (second instruc)) ; if not a label
                                                                                             (string-append "_" (substring (symbol->string (second instruc)) 1))
                                                                                             ) "\n"
                                                                                               label ":" )))]
    [(equal? (first instruc) `tail-call)  (writefile-append AsmOutputFilename (string-append "\nmovl %ebp, %esp" ; Handles tail-call
                                                                                    "\njmp " (if (equal? #f
                                                                                                         (string-index (symbol->string (second instruc)) (string->char-set ":") 0 (string-length (symbol->string (second instruc)))))
                                                                                                 (if (number? (second instruc)) (string-append "$" (number->string (second instruc))) (string-append "%" (symbol->string (second instruc))))  ; if not a label then could be a number or register
                                                                                                 (string-append "_" (substring (symbol->string (second instruc)) 1)) ;or a label ; handles labels
                                                                                                 )))]
    [(equal? (first instruc) `goto) (writefile-append AsmOutputFilename     ; Handles goto   ; done with length 2 instructions
                                                      (string-append "\njmp " (string-append "_" (substring (symbol->string instruc) 1) "")))]
    [(list? (first instruc)) ; Handles MEM write    ; only the mem write has a list for it's first instruc ; ((mem x n4) <- s)
     (let* ([offset (number->string (third (first instruc)))])
       (writefile-append AsmOutputFilename (string-append "\nmovl "   
                                                 (if (number? (third instruc))
                                                     (string-append "$" (number->string (third instruc)))
                                                     (if (equal? #f             ; test if s if a number, label, or register 
                                                                 (string-index (symbol->string (third instruc)) (string->char-set ":") 0 (string-length (symbol->string (third instruc)))))
                                                         (string-append "%" (symbol->string (third instruc)))     ; if it is not a label
                                                         (string-append "_" (substring (symbol->string (third instruc)) 1)) ;QQQQQ!!!!! then is a label
                                                         ))
                                                 (string-append ", " offset "(%" (symbol->string (second (first instruc))) ")")) ; or a register
                         ))]
    [(and (list? (third instruc)) (equal? (first (third instruc)) 'mem)) ; Handles MEM read (x <- (mem x n4))
     (let* ([offset (number->string (third (third instruc)))])
       (writefile-append AsmOutputFilename (string-append "\nmovl " offset "(" (string-append "%" (symbol->string (second (third instruc))) ")")
                                                 (string-append ", %" (symbol->string (first instruc))))
                         ) )]
    ; remaining is conditional jump, assignment, runtime funcs, and comparison   ;;; pretty confident that above code is correct
    [(and (list? (third instruc)) (equal? (first (third instruc)) 'print)) ; Handles PRINT runtime call ;  (eax <- (print t))
     (writefile-append AsmOutputFilename (string-append "\npushl " (if (number? (second (third instruc))) 
                                                              (string-append "$" (number->string (second (third instruc)))) 
                                                              (string-append "%" (symbol->string (second (third instruc))))) "\n"
                                                                                                                             "call print\n"
                                                                                                                             "addl $4, %esp"))]
    [(and (list? (third instruc)) (equal? (first (third instruc)) 'allocate)) ; Handles ALLOCATE runtime call ; (eax <- (allocate t t))
     (writefile-append AsmOutputFilename  (string-append "pushl " (if (number? (third (third instruc))) ; compile b
                                                             (string-append "$" (number->string (third (third instruc)))) 
                                                             (string-append "%" (symbol->string (third (third instruc))))) "\n"
                                                                                                                           "pushl " (if (number? (second (third instruc))) ; compile a 
                                                                                                                                        (string-append "$" (number->string (second (third instruc)))) 
                                                                                                                                        (string-append "%" (symbol->string (second (third instruc))))) "\n"
                                                                                                                                                                                                       "call allocate\n"
                                                                                                                                                                                                       "addl $8, %esp"))]
    [(and (list? (third instruc)) (equal? (first (third instruc)) 'array-error)) ; Handles ARRAY-ERR runtime call (eax <- (array-error t t))
     (writefile-append AsmOutputFilename  (string-append "pushl " (if (number? (third (third instruc))) ; compile b
                                                             (string-append "$" (number->string (third (third instruc)))) 
                                                             (string-append "%" (symbol->string (third (third instruc))))) "\n"
                                                                                                                           "pushl " (if (number? (second (third instruc))) ; compile a 
                                                                                                                                        (string-append "$" (number->string (second (third instruc)))) 
                                                                                                                                        (string-append "%" (symbol->string (second (third instruc))))) "\n"
                                                                                                                                                                                                       "call print_error\n"
                                                                                                                                                                                                       "addl $8, %esp"))]                                                                                                                                                                                         
    [(equal? (second instruc) '+=) (if (number? (third instruc)) (writefile-append AsmOutputFilename ; Handles plus
                                                                                   (string-append "\naddl $" (number->string (third instruc)) ", %" (symbol->string (first instruc)) ))
                                       (writefile-append AsmOutputFilename 
                                                         (string-append "\naddl %" (symbol->string (third instruc)) ", %" (symbol->string (first instruc)))))]
    [(equal? (second instruc) '-=) (if (number? (third instruc)) (writefile-append AsmOutputFilename ; Handles minus
                                                                                   (string-append "\nsubl $" (number->string (third instruc)) ", %" (symbol->string (first instruc)) ))
                                       (writefile-append AsmOutputFilename 
                                                         (string-append "\nsubl %" (symbol->string (third instruc)) ", %" (symbol->string (first instruc)))))]
    [(equal? (second instruc) '*=) (if (number? (third instruc)) (writefile-append AsmOutputFilename ; Handles multiply
                                                                                   (string-append "\nimull $" (number->string (third instruc)) ", %" (symbol->string (first instruc)) ))
                                       (writefile-append AsmOutputFilename 
                                                         (string-append "\nimull %" (symbol->string (third instruc)) ", %" (symbol->string (first instruc)))))]
    [(equal? (second instruc) '&=) (if (number? (third instruc)) (writefile-append AsmOutputFilename ; Handles bitwise&
                                                                                   (string-append "\nandl $" (number->string (third instruc)) ", %" (symbol->string (first instruc)) ))
                                       (writefile-append AsmOutputFilename 
                                                         (string-append "\nandl %" (symbol->string (third instruc)) ", %" (symbol->string (first instruc)))))]     
    [(equal? (second instruc) '<<=) (if (number? (third instruc)) (writefile-append AsmOutputFilename  ; Handles leftshift
                                                                                    (string-append "\nsall $" (number->string (third instruc)) ", %" (symbol->string (first instruc)) ))
                                        (writefile-append AsmOutputFilename 
                                                          (string-append "\nsall %" (symbol->string (third instruc)) ", %" (symbol->string (first instruc)))))]
    [(equal? (second instruc) '>>=) (if (number? (third instruc)) (writefile-append AsmOutputFilename ; Handles righttshift
                                                                                    (string-append "\nsarl $" (number->string (third instruc)) ", %" (symbol->string (first instruc)) ))
                                        (writefile-append AsmOutputFilename 
                                                          (string-append "\nsarl %" (symbol->string (third instruc)) ", %ecx")))]
    ; remaining is conditional jump, assignment, and comparison
    [(and (equal? (second instruc) `<-) (equal? (length instruc) 3))   ; Handles Assignment Statement   (x <- s)   
     (writefile-append AsmOutputFilename (string-append "\nmovl " (if (number? (third instruc))
                                                             (string-append "$" (number->string (third instruc)))
                                                             (if (equal? #f             ; test if s if a number, label, or register 
                                                                         (string-index (symbol->string (third instruc)) (string->char-set ":") 0 (string-length (symbol->string (third instruc)))))
                                                                 (string-append "%" (symbol->string (third instruc)))     ; if it is not a label
                                                                 (string-append "_" (substring (symbol->string (third instruc)) 1)) ;QQQQQ!!!!!
                                                                 ))  ", %" (string-append (symbol->string (first instruc)) ))
                       )]
    
    ;CJUMP
    [(and (equal? (first instruc) `cjump) (or (equal? (third instruc) `<) (equal? (third instruc) `<=) (equal? (third instruc) `=)))     ; Handles cjump Statement with < as cmp operator
     (cond 
       [(and (number? (second instruc)) (number? (fourth instruc)))  ; if both args to cmp are immediates
        (writefile-append AsmOutputFilename (if (< (second instruc) (fourth instruc)) (string-append "\njmp "
                                                                                            (string-append "_" (substring (symbol->string (fifth instruc)) 1))) ;;; QQQ333!!!
                                       (string-append "\njmp " (string-append "_" (substring (symbol->string (sixth instruc)) 1)))))]
       [(and (symbol? (second instruc)) (symbol? (fourth instruc))) (writefile-append AsmOutputFilename
                                                                                       (string-append "\ncmpl %" (symbol->string (fourth instruc))  ", %" (symbol->string (second instruc))   ;note reversal of argument order here.                                                                                                                 
                                                                                     (cond [(equal? (third instruc) `<) (string-append "\njl ")]
                                                                [(equal? (third instruc) `<=) (string-append "\njle ")]
                                                                [(equal? (third instruc) `=) (string-append "\nje ")]                                                                                 
                                                                                             )                                                           
                                                                                        (string-append "_" (substring (symbol->string (fifth instruc)) 1)) ; jl for "jump less", rewrite labels to prefix w/ underscores
                                                                                      "\njmp " (string-append "_" (substring (symbol->string (sixth instruc)) 1)) ) )]
       [(and (number? (second instruc)) (symbol? (fourth instruc))) ; this one gets flipped
        (writefile-append AsmOutputFilename (string-append "\ncmpl $" (number->string (second instruc))", %" (symbol->string (fourth instruc))
                                                   (cond [(equal? (third instruc) `<) (string-append "\njg ")]
                                                                [(equal? (third instruc) `<=) (string-append "\njge ")]
                                                                [(equal? (third instruc) `=) (string-append "\njne ")]                                                                                 
                                                                                            ) 
                                                  
                                                  (string-append "_" (substring (symbol->string (fifth instruc)) 1)) 
                                                  "\njmp " (string-append "_" (substring (symbol->string (sixth instruc)) 1))))]
       [(and (symbol? (second instruc)) (number? (fourth instruc)))
        (writefile-append AsmOutputFilename (string-append "\ncmpl $" (number->string (fourth instruc))", %" (symbol->string (second instruc))
                                                      (cond [(equal? (third instruc) `<) (string-append "\njl ")]
                                                                [(equal? (third instruc) `<=) (string-append "\njle ")]
                                                                [(equal? (third instruc) `=) (string-append "\nje ")]                                                                                 
                                                                                             )
                                                  
                                                  (string-append "_" (substring (symbol->string (fifth instruc)) 1)) 
                                                  "\njmp " (string-append "_" (substring (symbol->string (sixth instruc)) 1))))
        ])]  
    ; begins final CMP translation ;(cx <- t cmp t)
    [(and (equal? (second instruc) `<-) (or (equal? (fourth instruc) `<) (equal? (fourth instruc) `<=) (equal? (fourth instruc) `=)))   ; Handles comparison Statement with < as cmp op ie: (cx <- t cmp t)
       (cond
       [(and (number? (third instruc)) (number? (fifth instruc))) 
        (writefile-append AsmOutputFilename (if (< (third instruc) (fifth instruc))
                                       (string-append "\nmov $1, %" (symbol->string (first instruc)))
                                       (string-append "\nmov $0, %" (symbol->string (first instruc)))))
        ]
       [(and (symbol? (third instruc)) (symbol? (fifth instruc)))   ;; QQQQ???
        (writefile-append AsmOutputFilename (string-append "\ncmpl %" (symbol->string (fifth instruc)) ", %" (symbol->string (third instruc)) "\n"
                                                 (cond
                                                  [(equal? (fourth instruc) `<) (string-append "\nsl ")]
                                                 [(equal? (fourth instruc) `<=) (string-append "\nsle ")]
                                                 [(equal? (fourth instruc) `=) (string-append "\nse ")] )
                                                                " %" (cond
                                                               [(equal? (first instruc) `eax) "al"]
                                                               [(equal? (first instruc) `ebx) "bl"]
                                                               [(equal? (first instruc) `ecx) "cl"]
                                                               [(equal? (first instruc) `edx) "dl"]
                                                               )
                                                  "\nmovzbl %" (cond
                                                                [(equal? (first instruc) `eax) "al"]
                                                                [(equal? (first instruc) `ebx) "bl"]
                                                                [(equal? (first instruc) `ecx) "cl"]
                                                                [(equal? (first instruc) `edx) "dl"]
                                                                ) ", %" (symbol->string (first instruc))))
        ]
       [(and (symbol? (third instruc)) (number? (fifth instruc)))
        (writefile-append AsmOutputFilename (string-append "\ncmpl $" (number->string (fifth instruc)) ", %" (symbol->string (third instruc)) "\n"
                                                  (cond
                                                  [(equal? (fourth instruc) `<) (string-append "\nsl ")]
                                                 [(equal? (fourth instruc) `<=) (string-append "\nsle ")]
                                                 [(equal? (fourth instruc) `=) (string-append "\nse ")] )
                                                  " %" (cond
                                                               [(equal? (first instruc) `eax) "al"]
                                                               [(equal? (first instruc) `ebx) "bl"]
                                                               [(equal? (first instruc) `ecx) "cl"]
                                                               [(equal? (first instruc) `edx) "dl"]
                                                               )
                                                  "\nmovzbl %" (cond
                                                                [(equal? (first instruc) `eax) "al"]
                                                                [(equal? (first instruc) `ebx) "bl"]
                                                                [(equal? (first instruc) `ecx) "cl"]
                                                                [(equal? (first instruc) `edx) "dl"]
                                                                ) ", %" (symbol->string (first instruc))))
        ]
       [(and (number? (third instruc)) (symbol? (fifth instruc))) ; this one gets flipped
        (writefile-append AsmOutputFilename (string-append "\ncmpl $" (number->string (third instruc)) ", %" (symbol->string (fifth instruc)) "\n"
                                                  (cond
                                                  [(equal? (fourth instruc) `<) (string-append "\nsg ")]
                                                 [(equal? (fourth instruc) `<=) (string-append "\nsge ")]
                                                 [(equal? (fourth instruc) `=) (string-append "\nsne ")] )
                                                  " %" (cond
                                                               [(equal? (first instruc) `eax) "al"]
                                                               [(equal? (first instruc) `ebx) "bl"]
                                                               [(equal? (first instruc) `ecx) "cl"]
                                                               [(equal? (first instruc) `edx) "dl"]
                                                               )
                                                  "\nmovzbl " (cond
                                                                [(equal? (first instruc) `eax) "al"]
                                                                [(equal? (first instruc) `ebx) "bl"]
                                                                [(equal? (first instruc) `ecx) "cl"]
                                                                [(equal? (first instruc) `edx) "dl"]
                                                                ) ", %" (symbol->string (first instruc))))
        ]
       )]
     
                                                                                               
    [else (print "\nError: Cond Cases are not Exhaustive or incorrect L1 input\n")]
    )
  )

(define (L1tox86Asmstring mainandfunctions) (if (equal? mainandfunctions `()) 5 
                                                (if (equal? (rest mainandfunctions) `()) (instrtransl (first mainandfunctions)) 
                                                    (begin (instrtransl (first mainandfunctions)) (L1tox86Asmstring (rest mainandfunctions)))
                                                    )))

(define (parse inputprogram) 
  (if (equal? inputprogram `()) "" (if (equal? (rest inputprogram) `()) (L1tox86Asmstring (first inputprogram))
                                       (begin (L1tox86Asmstring (first inputprogram)) (parse (rest inputprogram))))))



(parse inputtest)

(writefile-append AsmOutputFilename "\n# restore callee-saved registers
popl   %ebp
popl   %edi
popl   %esi
popl   %ebx

        # restore caller's base pointer
leave
ret
.size	go, .-go
	.section	.note.GNU-stack,\"\",@progbits\n")