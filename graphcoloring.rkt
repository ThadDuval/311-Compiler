#lang racket
(require racket/include)
(require srfi/1)

(provide RunGraphColoring)

(require "liveness4.rkt")

;(require "testinginclude.rkt")
;(define input (call-with-input-file "GraphTestsRob/55.L2f" read)) ; input test
;(define input (call-with-input-file "GraphTestsRob/53.L2f" read)) ; input test
;(define input (call-with-input-file "GraphTestsRob/40.L2f" read)) ; input test
;(define input (call-with-input-file "tests/22/2-test/17.L2" read)) ; input test

;(define input (call-with-input-file "tests/22/liveness-test/09.L2f" read)) ; input test
;(define input (call-with-input-file (vector-ref (current-command-line-arguments) 0) read))


(define (RunGraphColoring input)

(define FlagforwhetherGraphColoringFails 0)
(define RegistersthatvarswasAllocatedtoAllocatedto '())
;input



(define LivenessResults (RunLivenessAnalysis input))
(define killLists (KillListforeachInstruction input))

  
;(displayln LivenessResults)

(define reg (list 'eax 'ebx 'ecx 'edi 'edx 'esi))
;LivenessResults
(define in_set (first LivenessResults))   
(define out_set (second LivenessResults))

(define out_setoffirstinstruc (first (rest out_set)))
  
;(newline)
;(displayln out_setoffirstinstruc)
;(displayln out_set)
  
(set! in_set (append (second in_set)  out_setoffirstinstruc ))  ; IN of first instruc
(set! out_set (rest (rest out_set)))   ; OUT's of Remaining instruc's
(set! out_set (append (list in_set) out_set))  ;append them together

;(displayln out_set)  
  
(define NodesandEdges '())       ; builds a list of all the unique variable names and registers used in the program
(define GraphColoringResults '())   ; to be used later

(define (mainLoop arg)      ; Loop over the out_set list - which is just the IN of instruc #1 appended to the out of each subsequent instruction
  (when (not (equal? (rest arg) '()))
      (begin
        (set! NodesandEdges (append NodesandEdges (first arg)))
        (mainLoop (rest arg)))))
;(mainLoop out_set)      ; Flatten out_set // result is stored in NodesandEdges and out_set stays the same as it will be used later in algorithm \


; Flatten KillLists and append to NodesandEdges
(set! NodesandEdges (append NodesandEdges (flatten out_set)))
(set! NodesandEdges (append NodesandEdges (flatten (rest (first LivenessResults)))))
(set! NodesandEdges (append NodesandEdges (flatten killLists)))

(set! NodesandEdges (append NodesandEdges reg))

(set! NodesandEdges (remove-duplicates NodesandEdges)) ; remove duplicates
(set! NodesandEdges (sort NodesandEdges symbol<?)) ; sort NodesandEdges and now you have a list of all the unique variables used in the program in alphabetical order

;(displayln NodesandEdges)
  
;(newline)
;out_set
;(newline)

(define tmp '())
; #5 If Two variables appear together in the first instructions' IN set or any other
; #5 instructions OUT set then they need an edge
; #5 But if two variables appear in the same OUT set but one of them is in the kill set for the instruction they DON'T get an edge becuz of it
; #5 If two variables interfere with one another and they instruction is an assignment statement the DON'T get an edge becuz of it
(define (FindAdjacencies specificSymbol)   ; for each unique variable in the program
  (let ([x '()]   ; build an adjacency list
        [count 0]) ; used to index into killLists
   ; (displayln specificSymbol)  
    (begin
      (for ([i out_set])      ; look through each instruction's OUT set (or the first instruc's IN)
      
        (unless (and (equal? 0 count) 
                     ;(member specificSymbol in_set)
                     )                ; exceptions
            (set! i (append i (list-ref killLists count)))
          )
        
        (when (and (equal? 0 count) (member specificSymbol in_set))   ; exceptions
            (set! x (append x in_set)))
       
        (when (and (equal? 0 count) (member specificSymbol out_setoffirstinstruc))   ; exceptions -- when a variable is in an outset it interferes with the kills
            (set! x (append x (list-ref killLists count))))
        
    ;    (let ([tmp ])
        (when (and (equal? 0 count) (member specificSymbol (list-ref killLists count))) ; reciprocate above - when a variable is in an 
            (set! x (append x out_setoffirstinstruc)))
     ;   )
       
        ; reciprocate cx constraints
        (when (and (equal? 0 count) (list? (list-ref input count)) (equal? (length (list-ref input count)) 5) (equal? (first (list-ref input count)) specificSymbol))
                    (set! x (append x (list 'edi 'esi)))               
                   )
        
        ;(when (and (list? (list-ref input count)) (equal? (length (list-ref input count)) 5)) (display "cmp"))
       
        ; CONSTRAINED OPERATOR
        (when (and (or (equal? specificSymbol 'edi) (equal? specificSymbol 'esi)) (list? (list-ref input count)) (equal? (length (list-ref input count)) 5))
               (set! x (append x (first (list-ref input count))))
                  )
                
        
        (when (and (list? (list-ref input count)) (equal? (length (list-ref input count)) 3) (equal? (third (list-ref input count)) specificSymbol) (or (equal? (second (list-ref input count)) '<<=) (equal? (second (list-ref input count)) '>>=)))
              (set! x (append x (list 'eax 'ebx 'edx 'edi 'esi))) )     ; makes sure shifted by ecx
        
        ; reciprocate the interference
        (when (and (list? (list-ref input count)) (equal? (length (list-ref input count)) 3) (not (number? (third (list-ref input count)))) (not (equal? 'ecx specificSymbol)) (member specificSymbol reg) (or (equal? (second (list-ref input count)) '<<=) (equal? (second (list-ref input count)) '>>=)))
              (set! x (append x (third (list-ref input count) ))))     ; makes sure shifted by ecx
        
       (set! x (flatten x))
        
        (begin
        ; (display "Instruc Number:") (display count ) (displayln (list-ref killLists count))
          (when (member specificSymbol i)
                   ; (member specificSymbol (list-ref killLists count))
                     ; if a variable appears in an instrucs OUT set (or first instruc's IN) it interferes with other variables in the IN/OUT list
          (begin                                                         ; EXCEPT IF
 ;           (fprintf (current-output-port)
 ;          "~a was found in OUT set of instruction: ~a  ~a.\n"
 ;          specificSymbol
 ;          count
 ;          i)
            (set! tmp (list-ref input count))    ; get the corresponding instruction
           
           
           
         
           ; (display "Merged killList and OUT set")(displayln i)
            (match tmp              
             
              [(or (list a '<- b '< c) (list a '<- b '<= c) (list a '<- b '= c)) (begin (when (equal? (first tmp) specificSymbol) (set! x (append x (list 'edi 'esi))))
                                                                                   (set! x (append x i)))]
             
              [`(,a <- ,b)              ; for each symbol in the OUT set
                        (for ([j i])
                      ;    (display "tmp:") (displayln tmp)
    ;                      (displayln specificSymbol) (displayln j)
                          (unless (or (and         
                                   (equal? specificSymbol (first tmp))
                                   (equal? j (third tmp)))
                                      (and         
                                   (equal? specificSymbol (third tmp))
                                   (equal? j (first tmp)))
                                      )          
                            (set! x (append x (list j)))
                            ; (begin (displayln specificSymbol) (displayln i) (displayln (list-ref killLists count))  (set! x (append x (list (first tmp)))))    ; If the Node & Symbol participate in the assign statement do nothing,
                            ))]     ; else add the Symbol as an Adjacent Node
                    
           
             
              [else (set! x (append x i))
                                ])     
      )) (set! count (+  count 1)))
  
      (set! x (flatten x))
  ;  (display x)  
        (when (member specificSymbol reg)
            (set! x (append x reg)))
      (set! x (remove-duplicates x))             ; remove multiple edges between nodes
      (set! x (remove* (list specificSymbol) x)) ; a node does not need to adjacent to itself
     
      (set! x (sort x symbol<?))                 ; sort all the adjacent nodes alphabetically
      (set! x (append (list specificSymbol) x))  ; for each unique variable you get a list of the form : (node adjacentnode1 adjacentnode2 adjacentnode3 . . .)
   ; (displayln x)
                                                ; return the list a let map do the work of putting the lists for a variable together in one data structure
) x )))


; Build the Graph Coloring Results
(define adjacencyList (map FindAdjacencies NodesandEdges))

;(displayln adjacencyList)

;(newline)
;  (newline)
;(displayln NodesandEdges)
;(newline)
;adjacencyList

; find all variables that are NOT eax ebx ecx edx esi edi

(define variablesStack (box '()))
(define (push x a-list)
  (set-box! a-list (cons x (unbox a-list))))

(define (pop a-list)
  (let ((result (first (unbox a-list))))
    (set-box! a-list (rest (unbox a-list)))
    result))

; Graph Coloring Algorithm

(for ([i NodesandEdges])           ; push non-Reg variables first
  (when (not (member i reg))
      (push i variablesStack)))
(for ([i reg])          ; then push Reg variables
   (push i variablesStack))

 
; pop each Node off the variables stack
; of the Nodes that are in the graph, see which ones are in the correponding adjacency list
; if the count goes above 5, then return false
; for the vars, return the first available register that it can be allocated to
(define GraphColoring
  (let ([NodesthathavebeenAddedtoGraph '()])
      (begin
       (for ([i NodesandEdges])               ; for the amount of variables found in function (this is the amount of time you will call pop)
          (let ([NodetryingtoColor (pop variablesStack)] [edgeCount 0])  ; pop a variable of the variablesStack and set the edgeCount to 0
            (for/first ([j adjacencyList] #:when (equal? NodetryingtoColor (first j)))  ; locate the adjacency list for the Node you are trying to color, then                                        
                  (begin                                                       ; for each variable that interferes with the Node you are trying to color that
                   (for ([k NodesthathavebeenAddedtoGraph])                   ; is already in the graph
                     (when (member k j) (set! edgeCount (+ 1 edgeCount))))     ; increment the edgeCount
                
                   (when (> edgeCount 5) (begin (set! FlagforwhetherGraphColoringFails 1)))   ; if the edgeCount is Greater then 5 for a Node then signal failure
                   (when (equal? FlagforwhetherGraphColoringFails 0)                  ; before officially adding the Node to the graph, you must figure out what color the Node will be colored with
                      (begin                                                     
                      ;  (displayln j)
                        
                       
                       ; testing if previously colored vars interfere with current var you are trying to color
                        ; if so you must add an edge between that color and the var you are trying to Color
                     (let ([count 0] [tmpAllocatedtoAllocatedto (flatten RegistersthatvarswasAllocatedtoAllocatedto)]) 
                       (for ([m tmpAllocatedtoAllocatedto])   
                        (when (even? count)
                          (when (member (list-ref tmpAllocatedtoAllocatedto count) j)
                            
                            (set! j (append j (list-ref tmpAllocatedtoAllocatedto (+ count 1))))
                            (set! j (flatten j))
                           )                              
                          ;   (displayln tmpAllocatedtoAllocatedto) 
                         ;(set! j (append j (flatten RegistersthatvarswasAllocatedtoAllocatedto)))
   
                          )
                         (set! count (+ count 1))
                         ))
                       ; (displayln j)
                        (set! j (flatten j))
                        (for/first ([l reg] #:when (not (member l j)))  ; so loop through the reg list and find the first register that is not in the adjacency list of the Node                            
                               (set! RegistersthatvarswasAllocatedtoAllocatedto             ; this will be the register that the var will be allocated to
                                   (cons (list NodetryingtoColor l) RegistersthatvarswasAllocatedtoAllocatedto                 ; add this (register var) info to the RegistersthatvarswasAllocatedtoAllocatedto dataStructure
                                        ))
                          
                        )
                        (set! NodesthathavebeenAddedtoGraph (append NodesthathavebeenAddedtoGraph (list NodetryingtoColor))) ; then official mark the Node as added to the Graph
                        ))
                   ))
            )      
       
            ))
  ;  (display "Nodes that have been re-added: ") (displayln NodesthathavebeenAddedtoGraph)
  
))

;(for/first ([i '(1 2 3 "x")]
;              #:when (even? i))

;GraphColoring

;(displayln FlagforwhetherGraphColoringFails)
;RegistersthatvarswasAllocatedtoAllocatedto


; reciprocate ecx shifting exclusiveness


;
;(if (equal? 0 FlagforwhetherGraphColoringFails)
;    (begin (for ([i adjacencyList]) (displayln i))
;           (display RegistersthatvarswasAllocatedtoAllocatedto))
;    (display #f))
;
;  
;;  (displayln (flatten RegistersthatvarswasAllocatedtoAllocatedto))
;(newline)
;(newline)

(if (equal? 0 FlagforwhetherGraphColoringFails)
    (begin (list
            adjacencyList
           RegistersthatvarswasAllocatedtoAllocatedto))
    (begin (list #f NodesandEdges)))
)


;(RunGraphColoring input)



;((in (eax ecx edi edx esi) (eax ecx edi edx esi) (eax edi esi)) (out (eax ecx edi edx esi) (eax edi esi) ()))
; graph coloring
; input: a L2 function
; output : adjacency list for each node in graph ; plus the "color" that was assigned to your variables as the second list
; each node corresponds to a variable in the program
; graph coloring algorithm
; #1 Given an L2 function - first do a liveness analysis on the function
; #2 Then append the IN of the first instruc to the OUT of the remaining instructions
;
; #3 Loop over this list finding every unique variable symbol
; #4 Sort these variable into alphabetical order as a lists of lists
; #5 Loop through the Liveness results to build the adjacency lists for each variable
; #5 If Two variables appear together in the first instructions' IN set or any other
; #5 instructions OUT set then they need an edge
; #5 But if two variables appear in the same OUT set but one of them is in the kill set for the instruction they DON'T get an edge becuz of it
; #5 If two variables interfere with one another and they instruction is an assignment statement the DON'T get an edge becuz of it
; #6 Sort each adjacency list
; #7 If any adjacency list has more than 5 members return false  
; #7 Else color the graph by removing the registers first, followed by the variables ; each variable removed should be pushed onto a stack
; #7 Assign the variables the "smallest possible color" ie: sort the main regs by alphabetical order
; #7 When reinserting the removed nodes, use adjacency lists to determined the color for variable
