#lang racket
(require racket/include)
(require srfi/13)
(require srfi/14)
(define input (call-with-input-file (vector-ref (current-command-line-arguments) 0) read))
;(define input (call-with-input-file "tests/robby/2-test/tests/42.L2" read)) ; input test

(require "liveness4.rkt")
(require "GraphColoring.rkt")
(require "spill3.rkt")

;"LOGIC STARTS HERE" - #1
(define finalL1program '())
(define FINALfinalL1program '())
(define didFunctionNeedVarReplacement 0) ; flag for whether a function had any instruc that was "replaced"
(define WasStackOFFSETNeeded 0)    ; flag for if spill was called
(define functionCounter 1)
(define Buildingfunctiontranslation '())
(define tmpforAddingStackinfotofunction '())
(define spillResult '())
(define tmpNewInput '())
(define spillCount 0)
(define looper 0)
(define mainEnderCounter 0)
(define StackOffsetEnderCounter 0)

(define (addStackOffsetInfotoFunction offset mainorNot)
  (if (equal? mainorNot 1)
      (begin  
        (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list (list 'esp '-= (* -1 offset)))))
        (for ([k Buildingfunctiontranslation])
          (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list k))))
        (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list (list 'esp '+= (* -1 offset))))))
      
      (begin  
        (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list (list-ref Buildingfunctiontranslation 0))))
        (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list (list 'esp '-= (* -1 offset))))) 
        (for ([k (in-range 1 (- (length Buildingfunctiontranslation) 1))])   
            (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list (list-ref Buildingfunctiontranslation k)))))
            (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list (list 'esp '+= (* -1 offset)))))
             (set! tmpforAddingStackinfotofunction (append tmpforAddingStackinfotofunction (list (list 'return))))
        )))

(define (ReplaceVarsWithTheirColor func ColorVarPairs)    
  ; **** MAJOR WORK OCCURS BELOW HERE (BETWEEN)  
  (for ([i ColorVarPairs])    ; for each variable    
    (for ([j func])             ; loop through each instruction of the function replacing instances of the variable with it's appropriate color
      (if (member (first i) (flatten j)) ; if variable exists in a given instruction, then build it's correct translation and append the translation to Buildingfunctiontranslation     
          (begin (let ([tmpinstruc '()])
                   (for ([k j])      ; for each component of an instruction
                     (when (not (list? k))   ; when component is not a list
                       (if (equal? k (first i))
                           (begin (set! tmpinstruc (append tmpinstruc (list (second i))))  (set! didFunctionNeedVarReplacement 1))  ; color the variable
                           (set! tmpinstruc (append tmpinstruc (list k)))      ; else just pass it through
                           ))
                     (when (list? k)   ; when component IS a list
                       (let ([sublist '()])
                         (for ([l k])
                           ;  (displayln l)
                           (if (equal? l (first i))
                               (begin (set! sublist (append sublist (list (second i)))) (set! didFunctionNeedVarReplacement 1))     ; color the variable
                               (set! sublist (append sublist (list l)))                    ; else just pass it through 
                               )
                           )
                         (set! tmpinstruc (append tmpinstruc (list sublist)))
                         ))
                     )
                   (set! Buildingfunctiontranslation (append Buildingfunctiontranslation (list tmpinstruc)))       
                   ))  
          (begin 
            (set! Buildingfunctiontranslation (append Buildingfunctiontranslation (list j))))
          )
      )
    (set! func Buildingfunctiontranslation)
    (set! Buildingfunctiontranslation '())  
    )
  (set! Buildingfunctiontranslation func)  
  ; **** MAJOR WORK OCCURS ABOVE HERE (BETWEEN) 
  )

;-----; LOGIC PROCEDES HERE 2ND --- #2
;--********-----MAIN----------************-----;
(define (L2toL1 input)
  
  (set! looper 0)
  (for ([i input])            ; must set up a loop that loops through each function in a program
 
   (set! looper (add1 looper))
    
   (when (equal? functionCounter looper) 
   ; (newline)
   ; (displayln "Showing Each Function") 
   ; (displayln i)
   ; (displayln functionCounter)
   ; (displayln input)
   ; (newline)
    
    (define GraphColoringResults '())
    
    (if (equal? i '()) (set! GraphColoringResults '('() '()))    ; check for the empty program 
        (set! GraphColoringResults (RunGraphColoring i)))          
    
   ; (displayln GraphColoringResults)
    
    (if (equal? (first GraphColoringResults) #f)                                       ; IF - if the result of the Coloring algorithm is a failure 
        (begin  
        ;  (displayln "Graph Coloring Failed")
          (when (< spillCount 6)
            (begin 
              
              (set! spillCount (add1 spillCount))
              
              (define prefix 'L2)
              
              
              ; choosing a spill variable -----------
              (define vartoSpill (second GraphColoringResults))
              (define finalvartoSpillList '())
              (set! vartoSpill (remove 'eax vartoSpill))
              (set! vartoSpill (remove 'ebx vartoSpill))
              (set! vartoSpill (remove 'ecx vartoSpill))
              (set! vartoSpill (remove 'edx vartoSpill))
              (set! vartoSpill (remove 'esi vartoSpill))
              (set! vartoSpill (remove 'edi vartoSpill))
              ; (displayln vartoSpill)
              
              (for ([k vartoSpill]) 
                (when (equal? #f (string-index (symbol->string k) (string->char-set "L2") 0 (string-length (symbol->string k))))  
                  (set! finalvartoSpillList (append finalvartoSpillList (list k)))
                  )
                )
              
              
          ;    (displayln finalvartoSpillList)
              ; choosing a spill variable -----------
          ;    (displayln "InputFunction right before call to spill: ")
            ;  (displayln input)
           ;   (newline)
            ;  (display functionCounter)
            ;  (newline)
            ;  (displayln (list-ref input (- functionCounter 1)))
            ;  (newline)
              ;(displayln (list-ref input 1))
              ;(newline)
              (set! spillResult (spill (list-ref input (- functionCounter 1)) (first finalvartoSpillList) (* spillCount -4) prefix) ) 
       ;       (displayln "SPILL RES")
       ;       (displayln spillResult)
              
          ;    (displayln "TMP NEW INPUT BEFORE RECURING")
          ;     (displayln tmpNewInput)
               
              (for ([c (in-range (length input) )]) 
          ;      (newline)
          ;      (displayln "Copying Program")
          ;      (displayln (- functionCounter 1))
          ;      (displayln c)
          ;      (displayln (list-ref input c))
          ;      (newline)
                
               (if (equal? c (- functionCounter 1))         
                    (set! tmpNewInput (append tmpNewInput (list spillResult)))    
                    (set! tmpNewInput (append tmpNewInput (list (list-ref input c)))))
               )
              
              
      ;       (displayln "TMP NEW INPUT **")
      ;         (displayln tmpNewInput)
               
     ;          (displayln functionCounter)
              
              (set! functionCounter (- functionCounter 0))            
              (set! input tmpNewInput) 
              (set! WasStackOFFSETNeeded 0)
              (set! didFunctionNeedVarReplacement 0)
              (set!  Buildingfunctiontranslation '())
              
              (set! spillResult '())
              (set!  tmpNewInput '())
              (set!  tmpforAddingStackinfotofunction '())
              (set! finalL1program '())
              
              
        ;     (displayln "Call to L2toL1 tail-call")
              (L2toL1 input)
              ))
          
          
          (when (> spillCount 6)   
            
            (when (equal? functionCounter 1)  ; error message incase of spilling main
              (displayln "could not register allocate main")
              (displayln "must call spill"))
            (when (not (equal? functionCounter 1)) ; error message incase of spill another function  
              (display "could not register allocate ")
              (displayln (first i))  
              (displayln "must call spill"))
            
            
            )
          )                                       
        
        (begin (when (not (equal? (second GraphColoringResults) '())) ; ELSE -- when successful result from Graph Coloring, loop through program and color vars 
                 (set! didFunctionNeedVarReplacement 1)
                 (begin    
                   ;       (displayln "CALLING REPLACE VARS WITH COLORS")     
                   (ReplaceVarsWithTheirColor i (second GraphColoringResults))   ;RVw/theirCOLOR uses Buildingfunctiontranslation to build its output
                   
                   ))
               
     (when (equal? StackOffsetEnderCounter 0)
      (when  (<= 1 spillCount)
        (set! WasStackOFFSETNeeded 1)
       ; (displayln 55)
       ; (displayln spillCount)
        )
    (set! StackOffsetEnderCounter (add1 StackOffsetEnderCounter)))
     
    (when (equal? mainEnderCounter 0)
    (when (and (equal? didFunctionNeedVarReplacement 1) (equal? WasStackOFFSETNeeded 0)) 
      (set! finalL1program (append (list Buildingfunctiontranslation))))
    (when (equal? didFunctionNeedVarReplacement 0)
      (set! finalL1program (append (list i))))
    (when (or (equal? WasStackOFFSETNeeded 1) (>= spillCount 1)) 
      (addStackOffsetInfotoFunction (* -4 spillCount) functionCounter)
      (set! finalL1program (append (list tmpforAddingStackinfotofunction))))
    )
               
               
               
               
               ))                      ; with their appropriate color
    
    
   
    
  ;  (displayln didFunctionNeedVarReplacement)
  ;  (displayln WasStackOFFSETNeeded)
 ;  (newline)
   
 ;   (newline)
 ;  (displayln finalL1program)
     
     (set! FINALfinalL1program (append FINALfinalL1program finalL1program))
     
  ;  (displayln Buildingfunctiontranslation)
 ;   (displayln "functionCounter before processing next function")
 ;   (displayln functionCounter)
 ;   (newline)
    
    (set! WasStackOFFSETNeeded 0)
    (set! didFunctionNeedVarReplacement 0)
    (set!  Buildingfunctiontranslation '())
    (set!  tmpforAddingStackinfotofunction '())
    (set! spillCount 0)
    (set! functionCounter 
          (add1 functionCounter)
          )
     
 )
    
    
    
    )   ; end of looping over functions
  
  (when (equal? mainEnderCounter 0)
 ;   (displayln "******FINAL PROGRAM: ")
    (displayln FINALfinalL1program)
    (set! mainEnderCounter (add1 mainEnderCounter))
    )
  )

(L2toL1 input)

;-----END OF MAIN-----;

