#lang racket
(provide spill)

(define ResultProgramofSpill '())

;(displayln ResultProgramofSpill)

;(set! ResultProgramofSpill (list ResultProgramofSpill (append (list 'eax))))

;(displayln ResultProgramofSpill)


(define (spill input variable offset prefix)

;   (displayln 55)
;  (displayln input)
  
(set! ResultProgramofSpill '()) 
  
;; Global counter for naming unique variables
(define (writefile-append filename output)
  (call-with-output-file filename
    (lambda (out)
      (fprintf out output))
    #:exists 'append))

(define (writefile filename output)
  (call-with-output-file filename
    (lambda (out)
      (fprintf out output))
    #:exists 'truncate))
 
(define counter -1)
(define a -1)
(define (add_to)
  (set! counter (add1 counter))
  counter)

(define (dec_to)
  (set! counter (sub1 counter))
  counter)

(define (substitute prefix)
  (string->symbol (string-append
                   (symbol->string prefix)
                   (number->string (add_to)))))

(define (transform sexp find-var repl-var)
  (cond
    [(equal? find-var sexp) repl-var]
    [(pair? sexp) (cons (transform (car sexp) find-var repl-var)
                        (transform (cdr sexp) find-var repl-var))]
    [else sexp]))

(define (load-from-mem dst offset)
  (list dst '<- (list 'mem 'ebp offset)))

(define (store-into-mem src offset)
  (list (list 'mem 'ebp offset) '<- src))

;
;(define OutputFilename "L2spillresult.L2")

;(writefile OutputFilename "")   ; to erase the already written-to file on each run 

(define (instrtransl instruc var off pre)  
 
  ;(set! ResultProgramofSpill (append ResultProgramofSpill (list 'esp -= off)))
  
  
  ; (displayln instruc)
  (cond   
    [(not (list? instruc)) (begin (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc))) ;(displayln instruc)
                                  )]
    [(equal? (first instruc) 'return) (begin (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc))) ;(display instruc)
                                             )]
    [(or (equal? (first instruc) 'call)           ; this case handles call, tail, call, cjump, goto and MEM write
         (equal? (first instruc) 'tail-call)
         (equal? (first instruc) 'cjump)
         (equal? (first instruc) 'goto)
         ;(and (equal? (first instruc) 'eax) (or (equal? (first (third instruc)) 'print) (equal? (first (third instruc)) 'allocate) (equal? (first (third instruc)) 'array-error))); the C functions need their own case
         (list? (first instruc))) 
     
     (begin
    ;   (displayln 5)
     (let* ([new-var (substitute pre)]
            [new-sexp (transform instruc var new-var)])
       (if (equal? instruc new-sexp)
           (begin (dec_to) (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc))) ;(displayln instruc)
                  )
           (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))
                  (set! ResultProgramofSpill (append ResultProgramofSpill (append (list new-sexp)))) ;(displayln new-sexp)
                  
                  ))) )]
    [(and (list? (third instruc)) (equal? (first (third instruc)) 'mem))     ;#3 this case handles MEM read    ; [(list a '<- (list 'mem b _))
     (begin
     ;  (displayln 6)
     (let* ([new-var (substitute pre)]
            [new-sexp (transform instruc var new-var)])
       (cond
         [(equal? instruc new-sexp)
          (begin (dec_to) (set! ResultProgramofSpill (append ResultProgramofSpill (append (list instruc)))) ;(displayln instruc)
                 )]
         [(and (equal? (first instruc) var)
               (equal? (second (third instruc)) var))
          (begin  (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))
                  (set! ResultProgramofSpill (append ResultProgramofSpill (list new-sexp))) ;(displayln new-sexp)
                  (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (store-into-mem new-var off))))) ;(displayln (store-into-mem new-var off))
                  )]
         [(equal? (first instruc) var)
          (begin (set! ResultProgramofSpill (append ResultProgramofSpill (list (list new-sexp)))) ;(displayln new-sexp)
                 (set! ResultProgramofSpill (append ResultProgramofSpill (list (list (store-into-mem new-var off))))) ;(displayln (store-into-mem new-var off))
          )]
         [else
          (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))  
                 (set! ResultProgramofSpill (append ResultProgramofSpill (list new-sexp))) ;(displayln new-sexp)
                 )])))]
    [(and (equal? 3 (length instruc)) (equal? (second instruc) '<-) (not (list? (third instruc))))  ; #4 this case handles assignment statements   (list a '<- b)
     (begin
     (cond
       [(and (equal? (first instruc) (third instruc))
             (equal? (first instruc) var))
        (set! a 1)]
       [(equal? (third instruc) var)
        (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem (first instruc) off))))) ;(displayln (load-from-mem (first instruc) off))
               )]
       [(equal? (first instruc) var)
        (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (store-into-mem (third instruc) off))))) ;(displayln (store-into-mem (third instruc) off))
               )]
       [else  (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc)))  ;  (displayln instruc)
             ]))]
    [(or (equal? (second instruc) '+=)
        (equal? (second instruc) '-=)
        (equal? (second instruc) '*=)
        (equal? (second instruc) '&=)
        (equal? (second instruc) '<<=)
        (equal? (second instruc) '>>=))
     (begin
     (let* ([new-var (substitute pre)]
            [new-sexp (transform instruc var new-var)])
       (cond
         [(equal? (first instruc) var)
          (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))
                 (set! ResultProgramofSpill (append ResultProgramofSpill (list new-sexp))) ;(displayln new-sexp)
                 (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (store-into-mem new-var off))))) ;(displayln (store-into-mem new-var off))
                 )]
         [(equal? (third instruc) var)
          (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))
                 (set! ResultProgramofSpill (append ResultProgramofSpill (list new-sexp))) ;(displayln new-sexp)
                 )]
         [else (begin (dec_to) 
                      (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc))) ;(displayln instruc)
                      )])))]

    [(and (list? (third instruc)) (or (equal? (first (third instruc)) 'print) (equal? (first (third instruc)) 'allocate) (equal? (first (third instruc)) 'array-error)))   ; handles runtime call print, allocate, and array-error  
     (begin
      ; (displayln 9)
     (let* ([new-var (substitute pre)]
            [new-sexp (transform instruc var new-var)])
       (if (equal? instruc new-sexp)
           (begin (dec_to) (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc))) ;(displayln instruc)
                  )
           (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))  
                  (set! ResultProgramofSpill (append ResultProgramofSpill (list new-sexp))) ;(displayln new-sexp)
                  ))))]
    [(or (equal? (fourth instruc) '<) (equal? (fourth instruc) '=) (equal? (fourth instruc) '<=))             ;cx<-t cmp t
     (begin
      ; (displayln 10)
       (let* ([new-var (substitute pre)]
            [new-sexp (transform instruc var new-var)])
       (cond
         [(and (equal? (first instruc) var)
               (or (equal? (third instruc) var)
                   (equal? (fifth instruc) var)))
          (begin (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))
                 (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (store-into-mem new-sexp))))) ; (displayln new-sexp)
                 (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (store-into-mem new-var off))))) ;(displayln (store-into-mem new-var off))
                 )]
         [(equal? (first instruc) var)
          (begin (set! ResultProgramofSpill (append ResultProgramofSpill (list new-sexp))) ;(displayln new-sexp)
                 (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (store-into-mem new-var off))))) ;(displayln (store-into-mem new-var off))
                 )]
         [(or (equal? (third instruc) var)
              (equal? (fifth instruc) var))
          (if (equal? instruc new-sexp)
              (begin (dec_to) 
                     (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc))) ;(displayln instruc)
                     )
              (begin 
                 (set! ResultProgramofSpill (append ResultProgramofSpill (append (list (load-from-mem new-var off))))) ;(displayln (load-from-mem new-var off))
                 (set! ResultProgramofSpill (append ResultProgramofSpill (list new-sexp))) ;(displayln new-sexp)
                 ))]
         [else (begin (dec_to)
                      (set! ResultProgramofSpill (append ResultProgramofSpill (list instruc))) ;(displayln instruc)
                      )])))]
    [else "Instruction Translation Function is Not Exhaustive"]
    
    ))


;(define input (call-with-input-file (vector-ref (current-command-line-arguments) 0)
 ; (lambda (x) (list (read x) (read x) (read x) (read x))) ))
    
    
   



;(define input (call-with-input-file "test11.L2f" (lambda (x) (list (read x) (read x) (read x) (read x)))))


  
;(define (L2spill mainandfunctions) (if (equal? mainandfunctions `())  
;                                                (if (equal? (rest mainandfunctions) `()) (instrtransl (first mainandfunctions)) 
;                                                    (begin (instrtransl (first mainandfunctions)) (L2spill (rest mainandfunctions)))
;                                                    )))
;(display "(")
(define (translate inputprogram) 
  (if (equal? inputprogram `()) (null) (if (equal? (rest inputprogram) `()) (instrtransl (first inputprogram) variable offset prefix)     ; transformment var , offset, prefix
                                       (begin (instrtransl (first inputprogram) variable offset prefix) (translate (rest inputprogram))))))


(translate  input)
;(display ")")
  
;(set! ResultProgramofSpill (list (append (list ResultProgramofSpill))))
 
  
   ResultProgramofSpill
  
  
  )

;(define qinput `(((a <- 3) ((mem ebp -4) <- 7) (c <- 35) (d <- 17) (e <- 501) (f <- 3) (g <- 22) (L23719810 <- (mem ebp -4)) 
 ;         (L23719810 += a) ((mem ebp -4) <- L23719810) (L23719811 <- (mem ebp -4)) (c += L23719811) (d += c) (e += d) 
  ;        (f += e) (g += f) (a += g) (eax <- (print a)))))

;(spill qinput 'd -8 'LQ) 
