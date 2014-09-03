#lang racket
(provide RunLivenessAnalysis)
(provide KillListforeachInstruction)
(require srfi/1)
(require srfi/13)
(require srfi/14)
(require unstable/list)

(define (KillListforeachInstruction input)
  (define (findKILLS instruc)
    (match instruc
      [(list a <- (list 'mem b c)) (list (first instruc))]    ; MEM READ  
      [(list (list 'mem a b) <- c) (list)]     ; MEM WRITE
      [(list a '+= b) (list (first instruc) )]     ; plus
      [(list a '-= b) (list (first instruc) )]     ; minus
      [(list a '*- b) (list (first instruc) )]    ; multiply
      [(list a '&- b) (list (first instruc) )]     ; logicalAND
      [(list a '<<= b) (list (first instruc))]     ; shiftLEFT
      [(list a '>>= b) (list (first instruc) )]     ; shiftRIGHT
      [(list 'goto x) (list '())]          ; goto
      [(list 'call x) (list 'ecx 'edx 'eax 'ebx)]         ; call
      [(list 'tail-call x)  (list)] 
      [(list 'return) (list)]
      [(list 'cjump a '< b c d) (list)]   ; CJUMP
      [(list 'cjump a '<= b c d) (list)]   ; CJUMP
      [(list 'cjump a '= b c d) (list)]    ; CJUMP
      [(list a '<- b '< c)  (list (first instruc) )]
      [(list a '<- b '<= c) (list (first instruc) )]
      [(list a '<- b '= c)  (list (first instruc) )]
      [(list 'eax '<- (list 'print a))  (list 'eax 'ecx 'edx )]
      [(list 'eax '<- (list 'allocate a b))  (list 'eax 'ecx 'edx )]
      [(list 'eax '<- (list 'array-error a b)) (list)]
      [(list a <- b) (list (first instruc))] ; Handles Assignments
      [a  (list )]   ; Handles Labels
      [else "Instruction Matching is not functioning properly"])
    )
  
  
  
  
  (define killList (map findKILLS input))
  killList
  )

(define (numORlabel? arg)
    (if (number? arg) #t (not (equal? #f (string-index (symbol->string arg) (string->char-set ":") 0 (string-length (symbol->string arg)))))))
  (define (label? arg)
    (not (equal? #f (string-index (symbol->string arg) (string->char-set ":") 0 (string-length (symbol->string arg))))))

(define (RunLivenessAnalysis input)
  ; HELPER functions
  
  (define (findGENS instruc)          ; FindGENS definition
    (match instruc
      [(list a <- (list 'mem b c)) (list (second (third instruc)))]   ; MEM READ
      [(list (list 'mem a b) <- c) (if (and (not (numORlabel? (third instruc))) (not (equal? (second (first instruc)) (third instruc)))) (list (second (first instruc)) (third instruc)) (list (second (first instruc))))]   ; MEM WRITE
      
      [(list a '+= b) (if (and (not (number? (third instruc))) (not (equal? (first instruc) (third instruc)))) (list (first instruc) (third instruc)) (list (first instruc)))]     ; plus
      [(list a '-= b) (if (and (not (number? (third instruc))) (not (equal? (first instruc) (third instruc)))) (list (first instruc) (third instruc)) (list (first instruc)))]     ; minus
      [(list a '*= b) (if (and (not (number? (third instruc))) (not (equal? (first instruc) (third instruc)))) (list (first instruc) (third instruc)) (list (first instruc)))]    ; multiply
      [(list a '&= b) (if (and (not (number? (third instruc))) (not (equal? (first instruc) (third instruc)))) (list (first instruc) (third instruc)) (list (first instruc)))]     ; logicalAND
      [(list a '<<= b) (if (and (not (number? (third instruc))) (not (equal? (first instruc) (third instruc)))) (list (first instruc) (third instruc)) (list (first instruc)))]     ; shiftLEFT
      [(list a '>>= b) (if (and (not (number? (third instruc))) (not (equal? (first instruc) (third instruc)))) (list (first instruc) (third instruc)) (list (first instruc)))]     ; shiftRIGHT
      [(list 'goto x) null]          ; goto
      [(list 'call x) (if (not (label? (second instruc))) (list 'eax 'ecx 'edx (second instruc)) (list 'eax 'ecx 'edx))]         ; call
      [(list 'tail-call x) (if (not (label? (second instruc))) (list 'eax 'ecx 'edx 'esi 'edi (second instruc)) (list 'eax 'ecx 'edx 'esi 'edi))] 
      [(list 'return) (list 'eax 'esi 'edi)]
      [(list 'cjump a _ b c d) 
       (cond ((and (not (number? (second instruc))) (not (number? (fourth instruc))) (not (equal? (second instruc) (fourth instruc)))) (list (second instruc) (fourth instruc)))
             ((and (not (number? (second instruc))) (number? (fourth instruc))) (list (second instruc)))
             ((and (number? (second instruc)) (not (number? (fourth instruc)))) (list (fourth instruc)))
             (else null)) ]   ; CJUMP
      [(list a '<- b _ c)  
       (cond ((and (not (number? (third instruc))) (not (number? (fifth instruc))) (not (equal? (third instruc) (fifth instruc)))) (list (third instruc) (fifth instruc)))
             ((and (not (number? (third instruc))) (number? (fifth instruc))) (list (third instruc)))
             ((and (number? (third instruc)) (not (number? (fifth instruc)))) (list (fifth instruc)))
             (else null))]
      [(list 'eax '<- (list 'print a)) (begin (if (not (number? (second (third instruc)))) (list (second (third instruc))) null) ) ]
      [(list 'eax '<- (list 'allocate a b))  
       (cond ((and (not (number? (second (third instruc)))) (not (number? (third (third instruc)))) (not (equal? (second (third instruc)) (third (third instruc))))) (list (second (third instruc)) (third (third instruc))))
             ((and (not (number? (second (third instruc)))) (number? (third (third instruc)))) (list (second (third instruc))))
             ((and (number? (second (third instruc))) (not (number? (third (third instruc))))) (list (third (third instruc))))
             (else null))]
      [(list 'eax '<- (list 'array-error a b))  
       (cond ((and (not (number? (second (third instruc)))) (not (number? (third (third instruc)))) (not (equal? (second (third instruc)) (third (third instruc))))) (list (second (third instruc)) (third (third instruc))))
             ((and (not (number? (second (third instruc)))) (number? (third (third instruc)))) (list (second (third instruc))))
             ((and (number? (second (third instruc))) (not (number? (third (third instruc))))) (list (third (third instruc))))
             (else null))]
      [(list a <- b) (if (not (numORlabel? (third instruc))) (list (third instruc)) null)] ; Handles Assignments
      [a  null]   ; Handles Labels
      [else "Instruction Matching is not functioning properly"])
    )
  (define (findKILLS instruc)
    (match instruc
      [(list a <- (list 'mem b c)) (list (first instruc))]    ; MEM READ  
      [(list (list 'mem a b) <- c) (list)]     ; MEM WRITE
      [(list a '+= b) (list (first instruc) )]     ; plus
      [(list a '-= b) (list (first instruc) )]     ; minus
      [(list a '*- b) (list (first instruc) )]    ; multiply
      [(list a '&- b) (list (first instruc) )]     ; logicalAND
      [(list a '<<= b) (list (first instruc))]     ; shiftLEFT
      [(list a '>>= b) (list (first instruc) )]     ; shiftRIGHT
      [(list 'goto x) (list '())]          ; goto
      [(list 'call x) (list 'ecx 'edx 'eax 'ebx)]         ; call
      [(list 'tail-call x)  (list 'err)] 
      [(list 'return) (list 'err)]
      [(list 'cjump a '< b c d) (list 'err 'cjump)]   ; CJUMP
      [(list 'cjump a '<= b c d) (list 'err 'cjump)]   ; CJUMP
      [(list 'cjump a '= b c d) (list 'err 'cjump)]    ; CJUMP
      [(list a '<- b '< c)  (list (first instruc) )]
      [(list a '<- b '<= c) (list (first instruc) )]
      [(list a '<- b '= c)  (list (first instruc) )]
      [(list 'eax '<- (list 'print a))  (list 'eax 'ecx 'edx )]
      [(list 'eax '<- (list 'allocate a b))  (list 'eax 'ecx 'edx )]
      [(list 'eax '<- (list 'array-error a b)) (list 'err)]
      [(list a <- b) (list (first instruc) )] ; Handles Assignments
      [a  (list )]   ; Handles Labels
      [else "Instruction Matching is not functioning properly"])
    )
  
  
;  (display formattedIN) 
 ;  (display formattedOUT) 
  
  (define (sortandformatLivenessResults)
    (set! formattedIN (append '(in) formattedIN))
    (for ([i (in-range 1 (+ lengthoffunction 1))])
      (set! formattedIN (append formattedIN (list (remove-duplicates (sort (hash-ref IN i) symbol<?)))))
      )
    (set! formattedOUT (append '(out) formattedOUT))
    (for ([i (in-range 1 (+ lengthoffunction 1))])
      (set! formattedOUT (append formattedOUT (list (remove-duplicates (sort (hash-ref OUT i) symbol<?))))) 
      )  
    
   ;  (displayln IN) 
  ; (displayln OUT) 
    
    (set! finalOUTPUT (append (list formattedIN) (list formattedOUT))) 
    )
  
  ;  LOGIC STARTS HERE
  ; Import the input function and find it length to avoid out of Bounds indexing      ; STEP 1
  ;(define input (call-with-input-file (vector-ref (current-command-line-arguments) 0) read))
  ;(define input (call-with-input-file "test9.L2f" read))
  
  
  (define lengthoffunction (length input))
  (define formattedIN (list))
  (define formattedOUT (list))
  (define finalOUTPUT (list))
  
  ; hash each instruction for easy reference   ; STEP 2
  (define instructionsList (make-hash))
  (define labelsHASHtable (make-hash))       ; labels HASH table for CJUMP
  (define counter 1)
  (define (add1toCounter)
    (set! counter (add1 counter)))
  (for ([i input])      ; OKAY, instructions have been hashed
    (when (not (list? i)) (hash-set! labelsHASHtable i counter))
    (hash-set! instructionsList counter i)
    (add1toCounter)
    )
  
  ; go through each instruction finding the gens and hash the KILL list           ; STEP 3
  (define genList (map findGENS input))      ; GENS for instruction in genList
  (define killList (map findKILLS input))
  
;  (display genList)
  
  
  ; filter out ebp and esp
  (define Stackvarsremoval '())
  ;Stackvarsremoval
  (for ([i (in-range 0 lengthoffunction)])
    (set! Stackvarsremoval (append Stackvarsremoval (list (remove* '(ebp esp) (list-ref genList i)))))
    )
  (set! genList Stackvarsremoval)
  ;genList
  ;   (remove* v-lst lst [proc]) → list?
  
  
  (define hashedKILLlist (make-hash))    ; hash the KILL list for convenience
  (set! counter 1)            ; reset counter to 1
  (for ([i killList])  
    (hash-set! hashedKILLlist counter i)
    (add1toCounter)
    )   ; OKAY kills have been hashed
  
  ; STEP 4                           ; initialize IN with genList          
  (define IN (make-hash))     ; used to produce the final inlist  -- IN
  (define OUT (make-hash))     ; used to produce the final outList -- OUT
  (set! counter 1)            ; reset counter to 1
  (for ([i genList])  
    (hash-set! IN counter i)
    (add1toCounter)
    )
  
  
  ;IN
  ;OUT
  ;labelsHASHtable
  
  ;(newline)
  ;hashedKILLlist
  
  ; LIVENESS ANALYSIS LOOP ; STEP 5
  (define (LivenessAnalysis in out)
    (begin
      (define changed 0)  ; changed flag
      (for ([i (in-range 1 lengthoffunction)])            ;  Move the in-set of the (n+1)th instruction
        
        (if (not (equal? (hash-ref hashedKILLlist i) '(err))) (hash-set! out i (hash-ref in (+ i 1))) (hash-set! out i '()))          ;  to the out set of the nth instruction
        ;(list 'cjump a '< b c d) 
        (when (and (list? (hash-ref instructionsList i)) (equal? (first (hash-ref instructionsList i)) 'cjump))   ; Handling CJUMP
          (begin 
            (let ([insoffirstlabel (hash-ref in (hash-ref labelsHASHtable (fifth (hash-ref instructionsList i))))]
                  [insofsecondlabel (hash-ref in (hash-ref labelsHASHtable (sixth (hash-ref instructionsList i))))]
                  )
              (hash-set! out i (remove-duplicates (append insoffirstlabel insofsecondlabel)))
              )))
        (when (and (list? (hash-ref instructionsList i)) (equal? (first (hash-ref instructionsList i)) 'goto))   ; Handling CJUMP
          (begin 
            (let ([insoffirstlabel (hash-ref in (hash-ref labelsHASHtable (second (hash-ref instructionsList i))))]
                  )
              (hash-set! out i (remove-duplicates insoffirstlabel)))
            ))
        )                                                   ;
      
      (if (not (or (and (list? (hash-ref instructionsList lengthoffunction)) (equal? (first (hash-ref instructionsList lengthoffunction)) 'cjump))    ; handle the final instruction
                   (and (list? (hash-ref instructionsList lengthoffunction)) (equal? (first (hash-ref instructionsList lengthoffunction)) 'goto))
                   ))   (hash-set! out lengthoffunction '())   ;  the out-set for the final instruction is always the empty set except if its goto or cjump
                        (begin
                          (when (and (list? (hash-ref instructionsList lengthoffunction)) (equal? (first (hash-ref instructionsList lengthoffunction)) 'cjump))   ; Handling CJUMP
                            (begin 
                              (let ([insoffirstlabel (hash-ref in (hash-ref labelsHASHtable (fifth (hash-ref instructionsList lengthoffunction))))]
                                    [insofsecondlabel (hash-ref in (hash-ref labelsHASHtable (sixth (hash-ref instructionsList lengthoffunction))))]
                                    )
                                (hash-set! out lengthoffunction (remove-duplicates (append insoffirstlabel insofsecondlabel)))
                                )))
                          (when (and (list? (hash-ref instructionsList lengthoffunction)) (equal? (first (hash-ref instructionsList lengthoffunction)) 'goto))   ; Handling CJUMP
                            (begin 
                              (let ([insoffirstlabel (hash-ref in (hash-ref labelsHASHtable (second (hash-ref instructionsList lengthoffunction))))]
                                    )
                                (hash-set! out lengthoffunction (remove-duplicates insoffirstlabel)))
                              ))
                          )
                        )
      ;(displayln IN)
      ;(displayln OUT)
      ; if a member of the out set for an instruction is not killed 
      ; then add it to the in-set for that instruction and set the CHANGED flag to ensure next interation of Liveness Analysis
      
      (for ([i (in-range 1 (+ lengthoffunction 1))])      ; + 1 to loop over every instruc in the function        
        (let ([tmplist '()])
          (begin
            (set! tmplist (hash-ref OUT i))
            (for ([j (in-range 1 (+ (length tmplist) 1))])        ; for each symbol in the outset 
              (when (and (not (set-member? (hash-ref in i) (list-ref tmplist (- j 1))))
                         (equal? #f (set-member? (hash-ref hashedKILLlist i) (list-ref tmplist (- j 1)))) 
                         (not (equal? (hash-ref hashedKILLlist i) '(err)) )
                         )  ; see if it is in the kills of the instruc
                (begin                            
                  (set! changed 1)                  ; if symbol is not in kill list then set changed flag               
                  (hash-set! in i (append (hash-ref in i) (list (list-ref tmplist (- j 1)))))  ; and add it to the in set of the instruction you are considering  
                  ))))))
      
      (when (equal? changed 1) (begin (set! changed 0) (LivenessAnalysis in out)))
      ))
  ; end of Liveness Analysis function
  
  
  (LivenessAnalysis IN OUT)
  (sortandformatLivenessResults)  ; Format IN and OUT to print to standard out or be given to graph-coloring algorithm ; FINAL STEP
  
  ;(displayln formattedIN)
  ;(displayln formattedOUT)
  ;(displayln finalOUTPUT)
  
  finalOUTPUT
  
  
  ; TESTING AREA
  ; move IN's of nth instruction to out of (n-1)th instruction
  ; do a union of the in and out of each instruction and store in IN
  ; filter the kills out of each IN
  ; test to see if IN list has changed (YES: Repeat Liveness Analysis NO: Format IN and OUT to print to standard out
  ; Definition of live at the same time means 
  ; you a variable/or register appears in the same IN set (for the first instruc)
  ; or the same OUT set (for each subsequent instruction)
  ; First thing graph-coloring algorithm does is call Liveness Analysis on the input program (written in L2)
  ; the with this IN/OUT information
  ; Livenes  ananysis
  ; Big Functions at Bottom ; they need prototypes or Contracts at top of file
  
  
  )

;(define input (call-with-input-file "tests/22/liveness-test/06.L2f" read)) ; input test

;(define LivenessResults (RunLivenessAnalysis input))
;LivenessResults
