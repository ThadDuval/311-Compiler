;        [(equal? (first i) 'aref) (begin  
;                                    (displayln (list WhereResultIsToBeStored '<- (encode (third i))))
;                                    (displayln (list WhereResultIsToBeStored '>>= 1))
;                                    (add_toV)
;                                    (displayln (list (FreshNewVar counterV) '<- (list 'mem (second i) 0)))
;                                    (let ([bounds-pass-label (begin (add_toL) (FreshNewLabel counterL))] [bounds-fail-label (begin (add_toL) (FreshNewLabel counterL))])
;                                      (displayln (list 'cjump WhereResultIsToBeStored '< (FreshNewVar counterV) bounds-pass-label bounds-fail-label))
;                                      (displayln bounds-fail-label)
;                                      (displayln (list 'eax '<- (list 'array-error (second i) (encode (third i)))))
;                                      (displayln bounds-pass-label)
;                                      (displayln (list WhereResultIsToBeStored '*= 4))
;                                      (displayln (list WhereResultIsToBeStored '+= (second i)))
;                                      (displayln (list WhereResultIsToBeStored '<- (list 'mem WhereResultIsToBeStored 4)))
;                                      ))]   ; (aref v v) -- first argument is the array and the second argument is the position in the array
;        [(equal? (first i) 'aset) (begin
;                                    (displayln (list WhereResultIsToBeStored '<- (encode (third i))))
;                                    (displayln (list WhereResultIsToBeStored '>>= 1))
;                                    (add_toV)
;                                    (displayln (list (FreshNewVar counterV) '<- (list 'mem (second i) 0)))
;                                    (let ([bounds-pass-label (begin (add_toL) (FreshNewLabel counterL))] [bounds-fail-label (begin (add_toL) (FreshNewLabel counterL))])
;                                      (displayln (list 'cjump WhereResultIsToBeStored '< (FreshNewVar counterV) bounds-pass-label bounds-fail-label))
;                                      (displayln bounds-fail-label)
;                                      (displayln (list 'eax '<- (list 'array-error (second i) (encode (third i)))))
;                                      (displayln bounds-pass-label)
;                                      (displayln (list WhereResultIsToBeStored '*= 4))
;                                      (displayln (list WhereResultIsToBeStored '+= (second i)))
;                                      (displayln (list (list 'mem WhereResultIsToBeStored 4) '<- (encode (fourth i))))
;                                      (displayln (list WhereResultIsToBeStored '<- 1))
;                                      ))] 

#lang racket
(require srfi/13)
(require srfi/14)

;(define inputProgram (call-with-input-file "AlltestsuptoL3/tests/39/3-test/test6.L3" (lambda (x) (read x))))
(define inputProgram (call-with-input-file (vector-ref (current-command-line-arguments) 0) read))


;------Helper Functions for Variable and Label Generation ----;
(define counterV 1)
(define (add_toV)
  (set! counterV (+ counterV 1))
  )
(define (FreshNewVar counter)
  (string->symbol (string-append
                   (symbol->string 'tmp)
                   (number->string counter))))

(define counterL 1)
(define (add_toL)
  (set! counterL (+ counterL 1))
  )
(define (FreshNewLabel counter)
  (string->symbol (string-append
                   (symbol->string ':L_)
                   (number->string counter))))
;^^------------------------------------------------^^;
;------Helper Function for Encoding------------------;
(define (encode n)
  (if (number? n) (+ 1 (* 2 n)) n)
  )
;^^------------------------------------------------^^;
;------Helper Function for Translating Arithmetical and Comparison Instructions------------------;
(define (convertArithInstruc i WhereResultIsToBeStored)
  (cond [(equal? (first i) '+)  (begin (add_toV)
                                       (displayln (list (FreshNewVar counterV) '<- (encode (second i))))
                                       (displayln (list (FreshNewVar counterV) '+= (encode (third i))))
                                       (displayln (list (FreshNewVar counterV) '-= 1))
                                       (displayln (list WhereResultIsToBeStored '<- (FreshNewVar counterV))))]
        
        [(equal? (first i) '*)  (begin (displayln (list (FreshNewVar counterV) '<- (encode (second i))))
                                       
                                       (displayln (list (FreshNewVar counterV) '>>= 1))
                                       (displayln (list WhereResultIsToBeStored '<- (encode (third i))))
                                       (displayln (list WhereResultIsToBeStored '>>= 1))
                                       (displayln (list WhereResultIsToBeStored '*= (FreshNewVar counterV)))
                                       (displayln (list WhereResultIsToBeStored '*= 2))
                                       (displayln (list WhereResultIsToBeStored '+= 1))
                                       (set! counterV (add1 counterV)))]  ; to have a New Fresh Var next time you need one
        
        [(equal? (first i) '-)  (begin (add_toV)
                                       (displayln (list (FreshNewVar counterV) '<- (encode (second i))))
                                       (displayln (list (FreshNewVar counterV) '-= (encode (third i))))
                                       (displayln (list (FreshNewVar counterV) '+= 1))
                                       (displayln (list WhereResultIsToBeStored '<- (FreshNewVar counterV))))]
        [else "Error: trying to Convert an instruction that is not Arithmetical"]
        ))

(define (convertComparisonInstruc i WhereResultIsToBeStored)
  (cond [(equal? (first i) '<)  (begin (displayln (list WhereResultIsToBeStored '<- (second i) '< (third i)))
                                       (displayln (list WhereResultIsToBeStored '<<= 1))
                                       (displayln (list WhereResultIsToBeStored '+= 1)))]
        
        [(equal? (first i) '<=)  (begin (displayln (list WhereResultIsToBeStored '<- (second i) '<= (third i)))
                                        (displayln (list WhereResultIsToBeStored '<<= 1))
                                        (displayln (list WhereResultIsToBeStored '+= 1)))] 
        
        [(equal? (first i) '=)  (begin (displayln (list WhereResultIsToBeStored '<- (second i) '= (third i)))
                                       (displayln (list WhereResultIsToBeStored '<<= 1))
                                       (displayln (list WhereResultIsToBeStored '+= 1)))]
        [else "Error: trying to Convert an instruction that is not Arithmetical"]
        ))

(define (convertNewTuple i WhereResultIsToBeStored)
  (let ([len (- (length i) 1)])
    (displayln (list 'eax '<- (list 'allocate (encode len) 3)))
    (for ([j (in-range 1 (+ len 1))])
      ;      (displayln j)
      (displayln (list (list 'mem 'eax (* 4 j)) '<- (encode (list-ref i j))))
      )
    (displayln (list WhereResultIsToBeStored '<- 'eax))
    )
  )
;^^------------------------------------------------^^;

(define (compileFunc i)
  
  (if (and (<= 2 (length i)) (list? (second i)))
      (cond [(equal? (length (second i)) 3) (begin ; it's a function definition
                                              (displayln (first i))
                                              (displayln (list (first (second i)) '<- 'ecx))
                                              (displayln (list (second (second i)) '<- 'edx))
                                              (displayln (list (third (second i)) '<- 'eax))
                                              (compileE (third i))
                                              (displayln '(return))
                                              )]
            [(equal? (length (second i)) 2) (begin ; it's a function definition
                                              (displayln (first i))
                                              (displayln (list (first (second i)) '<- 'ecx))
                                              (displayln (list (second (second i)) '<- 'edx))
                                              ;(displayln (list (third (second i)) '<- 'eax))
                                              (compileE (third i))
                                              (displayln '(return))
                                              )]
            [(equal? (length (second i)) 1) (begin ; it's a function definition
                                              (displayln (first i))
                                              (displayln (list (first (second i)) '<- 'ecx))
                                              ;(displayln (list (second (second i)) '<- 'edx))
                                              ;(displayln (list (third (second i)) '<- 'eax))
                                              (compileE (third i))
                                              (displayln '(return))
                                              )]
            [(equal? (length (second i)) 0) (begin ; it's a function definition
                                              (displayln (first i))
                                              ;(displayln (list (first (second i)) '<- 'ecx))
                                              ;(displayln (list (second (second i)) '<- 'edx))
                                              ;(displayln (list (third (second i)) '<- 'eax))
                                              (compileE (third i))
                                              (displayln '(return))
                                              )])
      (begin ; else it's a function call
        (cond [(equal? (length i) 4) (begin (displayln (list 'ecx '<- (encode (second i))))
                                            (displayln (list 'edx '<- (encode (third i))))
                                            (displayln (list 'eax '<- (encode (fourth i))))
                                            (displayln (list 'call (first i)))
                                            )]
              [(equal? (length i) 3) (begin (displayln (list 'ecx '<- (encode (second i))))
                                            (displayln (list 'edx '<- (encode (third i))))
                                            ;(displayln (list 'eax '<- (encode (fourth i))))
                                            (displayln (list 'call (first i)))
                                            )]
              [(equal? (length i) 2) (begin (displayln (list 'ecx '<- (encode (second i))))
                                            ;(displayln (list 'edx '<- (encode (third i))))
                                            ;(displayln (list 'eax '<- (encode (fourth i))))
                                            (displayln (list 'call (first i)))
                                            )]
              [(equal? (length i) 1) (begin ;(displayln (list 'ecx '<- (encode (second i))))
                                       ;(displayln (list 'edx '<- (encode (third i))))
                                       ;(displayln (list 'eax '<- (encode (fourth i))))
                                       (displayln (list 'call (first i)))
                                       )]
              
              ))
      )
  )

(define (CompileFuncfromLet i WhereResultIsToBeStored)
  (cond [(equal? (length i) 4) (begin (displayln (list 'ecx '<- (encode (second i))))
                                      (displayln (list 'edx '<- (encode (third i))))
                                      (displayln (list 'eax '<- (encode (fourth i))))
                                      (displayln (list 'call (first i)))
                                      (displayln (list WhereResultIsToBeStored '<- 'eax))
                                      )]
        [(equal? (length i) 3) (begin (displayln (list 'ecx '<- (encode (second i))))
                                      (displayln (list 'edx '<- (encode (third i))))
                                      ;(displayln (list 'eax '<- (encode (fourth i))))
                                      (displayln (list 'call (first i)))
                                      (displayln (list WhereResultIsToBeStored '<- 'eax))
                                      )]
        [(equal? (length i) 2) (begin (displayln (list 'ecx '<- (encode (second i))))
                                      ;(displayln (list 'edx '<- (encode (third i))))
                                      ;(displayln (list 'eax '<- (encode (fourth i))))
                                      (displayln (list 'call (first i)))
                                      (displayln (list WhereResultIsToBeStored '<- 'eax))
                                      )]
        [(equal? (length i) 1) (begin ;(displayln (list 'ecx '<- (encode (second i))))
                                 ;(displayln (list 'edx '<- (encode (third i))))
                                 ;(displayln (list 'eax '<- (encode (fourth i))))
                                 (displayln (list 'call (first i)))
                                 (displayln (list WhereResultIsToBeStored '<- 'eax))
                                 )]
        
        )
  )

(define (compileLET i)
  (compileD (second (first (second i))) (first (first (second i))))
  (compileE (third i))
  )

(define (compileIF i)   
  (let ([then (begin (add_toL) (FreshNewLabel counterL))] [else (begin (add_toL) (FreshNewLabel counterL))] [afterif (begin (add_toL) (FreshNewLabel counterL))])
    (displayln (list 'cjump (encode (second i)) '= 1 else then))
    (displayln then)
    (compileE (third i))
    (displayln (list 'goto afterif))
    (displayln else)
    (compileE (fourth i))
    (displayln afterif)
    )
  )


(define (compileD i WhereResultIsToBeStored)
  (cond [(not (list? i)) (displayln (list WhereResultIsToBeStored '<- (encode i)))]
        [(or (equal? (first i) '+) (equal? (first i) '-) (equal? (first i) '*)) (convertArithInstruc i WhereResultIsToBeStored)] ; binary operation
        [(or (equal? (first i) '<) (equal? (first i) '<=) (equal? (first i) '=))  (convertComparisonInstruc i WhereResultIsToBeStored)]
        [(equal? (first i) 'a?) (begin
                                  (displayln (list WhereResultIsToBeStored '<- (encode (second i))))
                                  (displayln (list WhereResultIsToBeStored '&= 1))
                                  (displayln (list WhereResultIsToBeStored '*= -2))
                                  (displayln (list WhereResultIsToBeStored '+= 3))
                                  )]
        [(equal? (first i) 'number?) (begin
                                       (displayln (list WhereResultIsToBeStored '<- (encode (second i))))
                                       (displayln (list WhereResultIsToBeStored '&= 1))
                                       (displayln (list WhereResultIsToBeStored '*= 2))
                                       (displayln (list WhereResultIsToBeStored '+= 1))          
                                       ;(displayln (list WhereResultIsToBeStored '*= -2))
                                       ;(displayln (list WhereResultIsToBeStored '+= 3))
                                       )]
        [(equal? (first i) 'new-array) (begin (displayln (list 'eax '<- (list 'allocate (encode (second i)) (encode (third i)))))
                                              (displayln (list WhereResultIsToBeStored '<- 'eax)))]
        [(equal? (first i) 'new-tuple) (convertNewTuple i WhereResultIsToBeStored)]
        [(equal? (first i) 'aref) (begin  
                                    (let ([bounds-pass-label (begin (add_toL) (FreshNewLabel counterL))] [bounds-fail-label (begin (add_toL) (FreshNewLabel counterL))])
                                      (displayln (list WhereResultIsToBeStored '<- (encode (third i))))
                                      (displayln (list WhereResultIsToBeStored '>>= 1))
                                      (add_toV)
                                      (displayln (list (FreshNewVar counterV) '<- (list 'mem (second i) 0)))
                                      (displayln (list 'cjump WhereResultIsToBeStored '< (FreshNewVar counterV) bounds-pass-label bounds-fail-label))
                                      (displayln bounds-fail-label)
                                      (displayln (list 'eax '<- (list 'array-error (second i) (encode (third i)))))
                                      (displayln bounds-pass-label)
                                      (displayln (list WhereResultIsToBeStored '*= 4))
                                      (displayln (list WhereResultIsToBeStored '+= (second i)))
                                      (displayln (list WhereResultIsToBeStored '<- (list 'mem WhereResultIsToBeStored 4)))
                                      ))]   ; (aref v v) -- first argument is the array and the second argument is the position in the array
        [(equal? (first i) 'aset) (begin
                                    (displayln (list WhereResultIsToBeStored '<- (encode (third i))))
                                    (displayln (list WhereResultIsToBeStored '>>= 1))
                                    (displayln (list WhereResultIsToBeStored '*= 4))
                                    (displayln (list WhereResultIsToBeStored '+= (second i)))
                                    (displayln (list (list 'mem WhereResultIsToBeStored 4) '<- (encode (fourth i))))
                                    (displayln (list WhereResultIsToBeStored '<- 1))
                                    )] ; (aset v v v) -- aset: the first argument is the array, the second is the position, and the third is a new value for that position in the array
        [(equal? (first i) 'alen)  (begin
                                     (displayln (list WhereResultIsToBeStored '<- (list 'mem (second i) 0)))
                                     (displayln (list WhereResultIsToBeStored '<<= 1))
                                     (displayln (list WhereResultIsToBeStored '+= 1)))] ; (alen v)
        [(equal? (first i) 'print) (displayln (list 'eax '<- (list 'print (encode (second i))))) ] ; (print v)
        [(equal? (first i) 'make-closure) (begin
                                            (displayln (list 'eax '<- (list 'allocate 5 3)))
                                            (displayln (list (list 'mem 'eax 4) '<- (encode (second i))))
                                            (displayln (list (list 'mem 'eax 8) '<- (encode (third i))))
                                            (displayln (list WhereResultIsToBeStored '<- 'eax))
                                            )] ; (make-closure label v)
        [(equal? (first i) 'closure-proc) (begin
                                            (displayln (list WhereResultIsToBeStored '<- (list 'mem  (second i) 4)))
                                            ; (displayln (list (list 'mem 'eax 4) '<- (second i)))
                                            )] ; (closure-proc v)
        [(equal? (first i) 'closure-vars) (displayln (list WhereResultIsToBeStored '<- (list 'mem  (second i) 8)))] ; (closure-vars v)
        
        [else (CompileFuncfromLet i WhereResultIsToBeStored)]
        
        ))

(define (compileE i)                                 ; from the top level each part of the program could be a
  (cond [ (not (list? i)) (begin
                            (add_toV)
                            (compileD i (FreshNewVar counterV) )) ]
        [ (equal? (first i) 'let) (compileLET i) ] ; let expression,
        [ (equal? (first i) 'if) (compileIF i) ]  ; an if statement,  
        [ (string-index (symbol->string (first i)) (string->char-set ":") 0 (string-length (symbol->string (first i)))) (compileFunc i)]  ; or a function
        [else (compileD i 'eax)]))

(define (compileMain i)
  (displayln "(")
  (displayln (list 'call (FreshNewLabel counterL)))
  (display ")")
  (newline)
  (display "(")
  (displayln (FreshNewLabel counterL))
  (compileE i)
  (displayln (list 'return))
  (display ")")
  )
;^^------------------------------------------------^^;

;_________MAIN_____________;
(display "(")
(define iCounter 1)
(for ([i inputProgram])
  (when (equal? iCounter 1)
    (begin
      (compileMain i)
      )
    )
  (when (not (equal? iCounter 1))
    (begin
      (newline)
      (display "(")
      (compileE i)
      (displayln ")")
      ))
  (set! iCounter (+ 1 iCounter))
  )
(display ")")
;^^______________________^^;

;The L3 grammar.
;
;p ::= (e
;       (label arglist e)
;       ...)
;arglist ::= () | (var) | (var var) | (var var var)
;;; note that functions have arguments now
;;; but still at most three arguments
;
;e ::= (let ([var d]) e)
;check?    | (if v e e)
;    | d
;
;d ::= (biop v v)
;check?      (pred v)
;check?      (v)       ;; fn call with 0 arg
;check?      (v v)     ;; fn call with 1 arg
;check?      (v v v)   ;; fn call with 2 args
;check?      (v v v v) ;; fn call with 3 args
;check?      (new-array v v)
;check?      (new-tuple v ...)
;check?      (aref v v)
;check?      (aset v v v)
;check?      (alen v)
;check?      (print v)
;check?      (make-closure label v)
;check?      (closure-proc v)
;check?      (closure-vars v)
;check?      v
;
;v :: = var | label | num
;
;check?   biop ::= + | - | *
;check? | < | <= | =
;check?     pred ::= number? | a?