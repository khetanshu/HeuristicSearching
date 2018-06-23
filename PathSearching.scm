; -------------------------------------------------------------------------
; ASSIGNMENT# 2 SEARCHING
; -------------------------------------------------------------------------
;**************************************************************************
;************************** Program Contents ******************************
; 1. A* Algorithm       (WILDCARD Entry supported)
; 2. id-dfs Algorithm   (WILDCARD Entry supported)
;
;@Developer : Khetanshu Chauhan 
;@Date April-24-2018

;**************************************************************************
;******************DESCRIPTION OF THE HEURISTIC ***************************

; A* SCORE(for any state) = (CURRENT COST  + ESTIMATED COST to the reach the GOAL-STATE)
; 
; Also, 
; ESIMATED COST = (MAXIMUM POSSIBLE SWAPS - ADJACENCY SCORE of the current state)
; 
; where,
; MAXIMUM POSSIBLE SWAPS  would = N-1 (N be the # of cities into consideration)
; ADJACENCY SCORE would be # of consequetive neighbour adjacent to each other.
;
; The above hueristic is admisible, it would never over estimate the cost to reach the GOAL state. Below is the explation, as in why,
;
; For example, consider a scenarios where we have a GOAL STATE (A B C D E), here N = 5
; This means that following pairs ( AB, BC, CD, DE )  are adjacent to each other i.e. total N-1 pairs.
; Which means that in the worst case if none of the cities for the above example are in the right position,
; then it would take no more than N-1 swaps to form the correct path A->B->C->D->E or E->D->C->B->A 
;
; say, for the above example, we have two intermediate states 
; STATE_A = ((A B C E D) ((1 3))  
; STATE_B = ((B A C D E) ()) 
;
; Therefore for,
; STATE_A: 	
; 	CURRENT COST = 1 {# swaps so far}
;	ADJACENCY SCORE = 3 {as (AB, BC, ED) are adjacent to each other}
;	so,
;	A* SCORE = 1 + ((N-1) - 3) = 1 + (4 - 3) = 2 {i.e. to reach the goal state it would require total of 2 swaps}
; Similarly for,
; STATE_B:
; 	CURRENT COST = 0 
; 	ADJACENCY SCORE =3  {as (BA, CD, DE) are adjacent to each other}
; 	so,
; 	A* SCORE = 0 + (4 - 3) = 1  {i.e. to reach the goal state it would require total of 1 swap}
;
; Here, A* SCORE for STATE B < A* SCORE of STATE A. Therefore the optimal STATE would be STATE_B.	
;
; ***Notice that if we don't consider the CURRENT COST then for the above example the algorithm could have picked STATE A as
; an optimal state, in that case in for the above example chosing A would have been a wrong decison as it would take total of
; 2 swaps however A would take only 1.
;
; This shows that the above heuristic would alway find an optimal solution and is admisible.
; 
; Therefore, STATE selection using A* algorithm would be as follows, 
; 1> Compute the A* scores 
; 2> Scan for STATE which has LEAST A* score (which mean that which requires least total swaps to reach the goal state), and pick
; 3> Now append all of the FOUND-STATE's childern into the FRONTIER pool, 
; 4> Repeat until the GOAL STATE is found OR EXIT if no GOAL state is found. 



;**************************************************************************
;;******************************** API ************************************

;#INPUT
;@intialLocations : should be in the form of "(Kentucky Kentucky Tennessee)" OR (Wildcard Oregon Arizona)
;#RETURNS
;Either 'a goal state' - if path exists or  '() - if already ordered path or #f - if path doesn't exists
(define (A* intialLocations)
  (find-goal-state intialLocations (length intialLocations) 'A*)
) 

;#INPUT
;@intialLocations : should be in the form of "(Kentucky Kentucky Tennessee)" OR (Wildcard Oregon Arizona)
;#RETURNS
;Either 'a goal state' - if path exists or  '() - if already ordered path or #f - if path doesn't exists
(define (id-dfs intialLocations)
  (find-goal-state intialLocations 0 'id-dfs)
)  

;Note : A* and id-dfs logic is diverted from depth-limited search



;**************************************************************************
;************************** Unit Test Cases ******************************* 

;%% Unordered - path scenarios (Positive) ******  Expected valid path with swap sequence

; (id-dfs '(Tennessee Iowa Kentucky North-Carolina Missouri))
; (A* '(Tennessee Iowa Kentucky North-Carolina Missouri))

; (id-dfs '(Missouri Kentucky Ohio Tennessee))
; (A* '(Missouri Kentucky Ohio Tennessee))

;%% Unordered - Path scenarios with repeated cities (to-fro scenario) (Positive)
 
; (id-dfs '(Kentucky Kentucky Tennessee))
; (A* '(Kentucky Kentucky Tennessee))



;%% Already existing path scenarios (Negative) ****** Expected ((locations..) ())

; (id-dfs '(California))
; (A* '(California))

;%% No path scenario  (Negative) ****** Expected #f

; (id-dfs '(Arizona Alaska))
; (A* '(Arizona Alaska))

; (id-dfs '( Oregon Arizona  Utah))
; (A* '( Oregon Arizona  Utah))


;%% Empty case: ****** Expected #f

; (id-dfs '())
; (A* '())


;%%%%  Wildcard sceanrios	 ****** 

; (id-dfs '(Oregon Arizona Wildcard Utah))
; (A* '(Oregon Arizona Wildcard Utah))

; (id-dfs '(Wildcard Oregon Arizona))
; (A* '(Wildcard Oregon Arizona))

; (id-dfs '(Tennessee Wildcard Iowa Kentucky North-Carolina Missouri))
; ; (A* '(Tennessee Wildcard Iowa Kentucky North-Carolina Missouri))



;%% Special Case (Computation Power test) : This case shows the noticeable difference in the computation time. 
; A* shown to be fast in comparision to id-dfs, other above examples doesn't show much difference, 
; both's result look same.

; (id-dfs '(Arizona Washington Montana California Idaho Oregon Nevada))
; ; (A* '(Arizona Washington Montana California Idaho Oregon Nevada))




;**************************************************************************
;******************************* MODULES **********************************

; -------------------------------------------------------------------------
;A* and ID-DFS Logic - Start $$


;This is the wrapper module to "depth-limited-search" module  
;#INPUT
;@intialLocations : should be in the form of "(Kentucky Kentucky Tennessee)" OR (Wildcard Oregon Arizona)
;@i = default to 0
;@approachMode = A* or id-dfs
;#RETURNS
;Either 'state' or  '() or #f
(define (find-goal-state intialLocations i approachMode)
    (cond
      ((null? intialLocations) #f)
      ((> i (length intialLocations)) #f)
      (#t 
          (let*
            ( 
              (states  (list (cons intialLocations (list '()))) )
              (foundState (depth-limited-search states i approachMode))
              
            )
            (if (null? foundState)
                ;if goal state is not found then go deeper in the tree - more iteration needed
               (find-goal-state intialLocations (+ i 1) approachMode)                       
               (let* (
                      (foundPath (car foundState))
                      (swapSequence (cadr foundState))
                      (refinedSwapSequence (remove-duplicates swapSequence '()))
                    )
                  
                    (append (list foundPath) (list refinedSwapSequence))
                    ;if A* value need to be append then enable below line and disable above line
                    ; (append (list foundPath) (list refinedSwapSequence) (cddr foundState))  
               )                
            )  
          )  
      )
   )
)


;#INPUT
;@path : should be in the form of "(Kentucky Kentucky Tennessee)" OR (Wildcard Oregon Arizona)
;@limit = number representing the allow iteration depth (Note for A* limit has to be passed as the total # of cities as its getting used while getting the State with an Optimal Score i.e. the least A* value)
;@approachMode = A* or id-dfs
;#RETURNS
;Either Raw verison of ('state' or  '())
(define (depth-limited-search states limit approachMode)
  (begin
      ; (newline)
      ; (display approachMode)
      ; (display "   |  ")
      ; (display limit)
      ; (display "   |  ")
      ; (display states)
      ; (newline)
      (cond
        ((null? states) states)
        (#t
          ;logic diversion for A* and ID-DFS
          (let* 
              (
                (updatedStates (if (equal? approachMode 'A* )
                                          (updateWithA*Score states)
                                          states
                                    )
                )
                (frontState   (if (equal? approachMode 'A* ) 
                                          (getStateWithOptimalScore updatedStates  '() limit)
                                          (car updatedStates)
                                  )
                )
                (newStates    (if (equal? approachMode 'A* ) 
                                          ; (if (null? frontState)
                                          ;     (delete-item updatedStates frontState)
                                          ;     (append (list frontState) (delete-item updatedStates frontState))
                                          ; )
                                          (append (list frontState) (delete-item updatedStates frontState))
                                           updatedStates
                                  )    
                )
                
                (depth 
                       (begin
                        ; (display "updatedStates   |   ")
                        ; (display updatedStates)
                        ; (newline)
                        ; (display "newStates  |  ")
                        ; (display newStates)
                        
                        ; (newline)
                        ; (display "Selected Front State  |  ")
                        ; (display frontState)
                        ; (newline)
                        ; (display "*************")
                        ; (newline)
                        (if (null? frontState) 
                          limit
                          (length (cadr frontState))
                        )
                       )
                       
                )
              ) 
              (if (null? frontState) 
                  frontState
              ;This is diversion point for normal case vs Wildcard case, if the input has wildcardValue then first 
              ;condition below would be executed otherwise the 'else' part
                  (if (list-contains? wildcardValue (car frontState))
                      ;this is Wildcard case
                      (begin 
                            ; (display "Wildcard case")
                            ; (newline)
                             
                      (let 
                          (
                            (WildcardOutput (is-goal-state-wildcard?  frontState))
                          )
                          (begin
                            ; (display "WildcardOutput -> ")
                            ; (display frontState)
                            ; (newline)
                            ; (display (list (replace-wildcard-entry (car frontState) WildcardOutput) (cdr frontState)))
                          (if (not (null? WildcardOutput))
                              (list (replace-wildcard-entry (car frontState) WildcardOutput) (cadr frontState))
                              
                              (if (= depth limit)
                                  ;then return back and check the previous depth items
                                  (depth-limited-search (cdr newStates) limit approachMode)
                                  ;otherwise just add the childern in the fronttier list (newStates)
                                  (let* 
                                    (
                                      (newChildern (get-children frontState))
                                      (newState  
							(append newChildern (cdr newStates)))
				      
                                    )
                                    (depth-limited-search newState limit approachMode)
                                  )
                                )
                          ))
                      )
                      )
                      ;this is normal case
                      (if (is-goal-state?  frontState)
                       ;if i==dept then check if you have reached a goal state
                       (begin
                          ; (display "frontState -> ")
                          ; (display frontState)
                          ; (newline)
                         frontState
                      )
                    
                      (begin
                        ; (newline)
                        ; (display "(if (= depth limit) module")
                        ; (display "depth")
                        ; (display depth)
                          (if (= depth limit)
                               ;then return back and check the previous depth items
                              (depth-limited-search (cdr newStates) limit approachMode)
                              ;otherwise just add the childern in the fronttier list (newStates)
                              (let* 
                                      (
                                         (newChildern (get-children frontState))
                                         (newState    
								(append newChildern (cdr newStates)))
                                      )
                                      (depth-limited-search newState limit approachMode)
                            )
                          )
                      )
                      )
                  )
              )  
          )
        )
      )
  )
)


;#INPUT
;@alist = a list
;#RETURNS
;Same list without duplicate values
(define (remove-duplicates alist returnList)
  (cond
    ((null? alist) returnList)
    (#t (remove-duplicates 
                           (cdr alist) 
                           (append  returnList 
                                    (if (list-contains? (car alist) returnList)
                                        '()
                                         (list (car alist))
                                    )
                            )        
      )
    )
  )
)


;A* and ID-DFS Logic - End $$
;-------------------------------------------------------------------------
;A* algorithm supporting modules - Starts ##



;#INPUT
;@states = likely states with update Heuristic scores
;@foundState = default to '() i.e. empty list
;@optimialScore = default to 0
;#RETURNS
;a likely state that seems to be optimial as the present moment having maximum A* score
(define (getStateWithOptimalScore states foundState optimialScore)
  (begin
    ; (display states)
    ; (newline)
    (cond
      ((null? states) foundState)
      ; (#t (if (>= (car (caddar states)) optimialScore)
      (#t (if (<= (car (caddar states)) optimialScore)        
              (getStateWithOptimalScore (cdr states) (car states) (car (caddar states)))
              (getStateWithOptimalScore (cdr states) foundState optimialScore)
          )
      )
    )
  )
)  

;This is wrapper function to "(addA*Score state)"
;#INPUT
;@states = likely states
;#RETURNS
;states with update A* scores
(define (updateWithA*Score states)
  (cond
    ((null? states) states)
    (#t (append (list (addA*Score (car states))) (updateWithA*Score (cdr states))))
  )  
)

;#INPUT
;@states = likely state
;#RETURNS
;an updated likely state with update A* scores
;Total Cost to reach the goalState (which is the A* Score) = Cost so far i.e. current # swaps + the estimateHeuristicValue
(define (addA*Score state)
  (begin
    ; (newline)
    ; (display "*****")
    ; (newline)
    ; (display "state")
    ; (newline)
    ; (display state)
    ; (newline)
    ; (display "((length(cadr state)))")
    ; (display (length(cadr state)))
    ; (newline)
    (append 
            (list (car state)) 
            (list (cadr state)) 
            (list (list 
                       (+ 
                          (length(cadr state))
                          ; (if (null? (cadr state)) 0 (length(cadr state)))
                          (estimateHeuristicValue (car state))
                          )
                  )
            )
    )
)
)

;Heuristic Cost to reach the goalstate = least # swap to reach the goalstate 
;= maximum # swap (Which would never be more than n-1, where n is the number of citiies considering) - the adjacent factor i.e how adjacent is consecutive pair are through the path 
(define (estimateHeuristicValue cities)
 (- (- (length cities) 1) (calculateAdjacencyScore cities))
)

;#INPUT
;@states = list of cities
;#RETURNS
;calculated value which shows how adjacent is consecutive pair are through the path 
;For example if cities are (A B C D E ) -> then if AB and CD are the only adjacent neghbours 
;then the score would be 1+0+1+0 = 2 (which shows that alteast 2 swaps would be required to reach the goalstate)
(define (calculateAdjacencyScore cities)
 (begin
        ; (display cities)
        ; (newline)
 (cond
    ((<= (length cities) 1) 0)
    (#t
      (if (is-adjacent? (car cities) (cadr cities))
          (+ 1 (calculateAdjacencyScore (cdr cities) ))
          (+ 0 (calculateAdjacencyScore (cdr cities) ))
      )

    )

  )
)
)

;A* algorithm supporting modules - End $$
; -------------------------------------------------------------------------
;Wildcard Logic - Start ##


;#INPUT
;@states = likely states
;#RETURNS
;#t of #f
(define (is-goal-state-wildcard?  states)
  (let 
       (
         (path (car states))
       )
       (findWildCardEntryPossibility path)
  )
)


;#INPUT
;@path = path example (Kentucky Wildcard Tennessee)
;#RETURNS
;list of all possible options for the wildcard entry
(define (findWildCardEntryPossibility path)
  (let*
    (
      (validCities (delete-item path wildcardValue))
      (wildcardCombinations (prepareWildcardCombinations path wildcardValue allLocations))
    )
    (validateWildCardEntries  wildcardCombinations)
  )
)

;This function validates each possible path for the wildcard options and  uses the global variable name 'wildcardValue'  and 'allLocations'
;#INPUT
;@wildcardCombinations = list of all possible options for the wildcard entry
;#RETURNS
;Returns a valid combination of the paths, if it exists
(define (validateWildCardEntries  wildcardCombinations)
  (cond
    ((null? wildcardCombinations) '())
    (#t 
        (let*
          (
            (path (car wildcardCombinations))
            (treeList (find-treeList path path map))
          )
          (if (path-exist? treeList path)
              path
              (validateWildCardEntries (cdr wildcardCombinations))
          )
        )
    )
  )
)


;#INPUT
;@originalList = list of cities
;@alist = same list of cities (i.e. originalList)
;@map = use the global variable named adjency-map
;#RETURNS
;a adjency-map containing only depencies of the list of cities those were passed in the input
(define (find-complete-treeList originalList alist map)
        (cond
                ((null? alist) alist)
                (#t (append 
                            (list  (find-location-sequence (car alist) map) )
                            (find-complete-treeList originalList (cdr alist) map)
                    )
                )
        )
)

; (define treeList (find-treeList path path map))

;#INPUT
;@path : list of cities
;@wildcardValue : default wildcardValue (Wildcard) - already defined here as globally
;@allLocations : all 50 US states
;#RETURNS
;a list containining all 50 possible combination 
(define (prepareWildcardCombinations path wildcardValue allLocations)
  (cond
    ((null? allLocations) allLocations)
    (#t 
      (append  
              (if (list-contains? (car allLocations) path)
               '()
               (list (replace-item path wildcardValue (car allLocations)))
              )
              (prepareWildcardCombinations path wildcardValue (cdr allLocations))
      )
    )
    
  )
)

;#INPUT
;@alist : a list to be updated
;@oldValue : value to be replaced
;@newValue : a value that oldValue needs to be replaced with
;#RETURNS
;@updated input list
(define (replace-item alist oldValue newValue)
        (cond
                ((null? alist) alist)
                ((equal? (car alist) oldValue) (append (list newValue) (cdr alist)))
                (#t (append (list (car alist)) (replace-item  (cdr alist) oldValue newValue)))
        )
)

;#INPUT
;@alist : a list to be updated
;@oldValue : value to be deleted
;#RETURNS
;@updated input list
(define (delete-item alist oldValue)
        (cond
                ((null? alist) alist)
                ((equal? (car alist) oldValue) (cdr alist))
                (#t (append (list (car alist)) (delete-item  (cdr alist) oldValue)))
        )
)

;#INPUT
;@alist : a list containining the wildcardValue. example (Oregon Wildcard Arizona)
;@blist : a list that isn't containining the wildcardValue. example (Oregon California Arizona)
;#RETURNS
;updated alist with valid entry in place of wildcardValue
(define (replace-wildcard-entry alist blist)
  (replace-item alist wildcardValue (get-wildcard-entry alist blist))
)

;#INPUT
;@alist : a list containining the wildcardValue. example (Oregon Wildcard Arizona)
;@blist : a list that isn't containining the wildcardValue. example (Oregon California Arizona)
;#RETURNS
;a value that blist that was present in the same position where wildcardValue was there in the alist. example here California
(define (get-wildcard-entry alist blist)
        (cond
                ((null? alist) alist)
                ((not (equal? (car alist) (car blist)))  (list wildcardValue (car blist)))
                (#t (get-wildcard-entry (cdr alist) (cdr blist)))
        )
)



;Wildcard Logic - End $$
;-------------------------------------------------------------------------
;is-goal-state? Logic - Start ##



;#INPUT
;@states : likely goal State
;#RETURNS
;#t or #
(define (is-goal-state?  state)
  (let* 
       (
         (path (car state))
         (treeList (find-treeList path path map))
       )
       (path-exist? treeList path)
  )
)  


;#INPUT
;@states : limited map with values having depencies with the cities present in the path
;#RETURNS
;#t or #
(define (path-exist? treeList path)
  (cond
    ((null? path) #t)
    ((null? (cdr path)) #t)
    (#t (and 
           (if
             (list-contains? (car (cdr path)) (getChildern (car path) treeList))
             #t
             #f
            )  
            (path-exist? treeList (cdr path))
        )
    )
  )
)

;#INPUT
;@originalList = list of cities
;@alist = same list of cities (i.e. originalList)
;@map = use the global variable named adjency-map
;#RETURNS
;a adjency-map containing only depencies of the list of cities those were passed in the input (With only cities that is present in the path)
(define (find-treeList originalList alist map)
        (cond
                ((null? alist) alist)
                (#t (append 
                            (list (find-intersection-list (find-location-sequence (car alist) map) originalList))
                            (find-treeList originalList (cdr alist) map)
                    )
                )
        )
)



;is-goal-state? Logic - End $$
;-------------------------------------------------------------------------
;get-children Logic - Start ##


;#INPUT
;@childernList : list of childern. Format should be like ((Alabama Arizona Alaska) ()))
;#RETURNS
;a list containing all the childern
(define (get-children childernList)
    (let*
      (
        (alist (car childernList))
        (n (length alist))
        (swap-combinations (get-swap-combination n 1 2 '()))
      )
      (add-swap-combinations childernList swap-combinations)
    )
)

;#INPUT
;@childernList = format should be like ((Alabama Arizona Alaska) ()))
;@swap-combinations = format should be like ((1 2) (1 3) (2 3))
;#RETURNS
;a list containing the paths with their swap combinations appended
(define (add-swap-combinations childernList swap-combinations)
  (begin 
    ; (display childernList)     
    ; (newline)
    ; (display  swap-combinations)
    ; (newline)
    (cond
            ((null? swap-combinations) '())
            (#t 
               (let* 
                  (
                    (existingChildern (car childernList))
                    (existingSwapSequence (cadr childernList))
                    (newSequence (car swap-combinations))
                    (updatedSequence (if  (null? existingSwapSequence) (list newSequence) (append existingSwapSequence (list newSequence))))
                    (i (car newSequence))
                    (j (cadr newSequence))
                    (n (length (car childernList)))
                    (updatedChildern (swap existingChildern n i j))
                  )
                  (append  
                          (list (list updatedChildern updatedSequence ) )
                          (add-swap-combinations childernList (cdr swap-combinations)) 
                  )
                ) 
            )
          )
  )    
)



;#INPUT
;@ n = Length
;@ i =1 
;@ j =2
;@alist = '()
;#RETURNS
;list of possible combinations 
(define (get-swap-combination n i j alist)
  (cond
    ((= i n) alist)
    ((< j n) (append alist (list (list i j)) (get-swap-combination n i (+ j 1) alist) ))
    (#t (append alist (list (list i j)) (get-swap-combination n (+ i 1) (+ i 2) alist)))
  )
)

;#INPUT
;@alist : a list that needs to be updated
;@ n = Length
;@ i = first position
;@ j = second position
;#RETURNS
;the list with swapped values
(define (swap alist n i j)
  (cond
    ((null? alist) alist)
    ((or (< i 1) (> j n))alist)
    ((= i j) alist)
    (#t 
        (let (
               (blist-i (list (nth-item i alist)))
               (blist-j (list (nth-item j alist)))
             )
             (begin
                 (replace-nth-item (replace-nth-item alist blist-i j) blist-j i)
             )
        )
    )
  )
) 


;get-children Logic - End $$
;-------------------------------------------------------------------------
;is-adjacent? Logic - Start ##

;#INPUT
;@aCity = city 1
;@bCity = city 2 
;#RETURNS
;#t or #f
(define (is-adjacent? aCity bCity)
  (let*
    (
      (cityList (list aCity))
      (aCityChildern (find-location-sequence aCity map))
    )
    (list-contains? bCity aCityChildern)
  ) 
)
 



;#INPUT
;@parentName 'parentName ex. 'Mississippi <Not a list here>
;@list would be of form  treeList = ((parent1, child1, child2.. childn)(parent2,child1, child2...)...) 
;#RETURNS
;a list having all childern
(define (getChildern parentName treeList)
        (cond
            ((null? treeList) treeList)
            ((equal? (car(car treeList)) parentName) (cdr (car treeList)))
            (#t (getChildern parentName (cdr treeList)))
        )
) 

;#INPUT
;@nodeName : city name
;@map : adjency-map
;#RETURNS
;a sequnce of location
(define (find-location-sequence nodeName map)
        (cond
                ((null? map) '())
                (#t (if (equal? (car (car map)) nodeName)
                        (car map)
                        (find-location-sequence nodeName (cdr map)))
                )
        )
)

;#INPUT
;alist = list 1 example (1 2 3)
;blist = list 2 example (x 2 y)
;#RETURNS
; list contain intersection of two list example here (2)
(define (find-intersection-list alist blist)
        (cond
                ((null? alist) alist)
                (#t (append (if (list-contains? (car alist) blist) (list (car alist)) '())
                                (find-intersection-list (cdr alist) blist))
                )
        )
)

;#INPUT
;@value : value that needs to be checked
;@alist : a list
;#RETURNS
; #t of #f depending if value is present in alist
(define (list-contains? value alist)
        (cond
                ((null? alist) #f)
                (#t (if (equal? (car alist) value) #t (list-contains? value (cdr alist))))
        )
)

;is-adjacent? Logic - End $$
;-------------------------------------------------------------------------
;Swap-elements Logic - Start ##

;#INPUT
;@i = first position
;@j = second position
;@alist = a list
;#RETURNS
;swapped value list
(define (swap-elements i j alist)
  (cond
    ((null? alist) alist)
    ((or (< i 1) (> j (length alist)))alist)
    ((= i j) alist)
    (#t 
        (let (
               (blist-i (list (nth-item i alist)))
               (blist-j (list (nth-item j alist)))
             )
             (begin
                 (replace-nth-item (replace-nth-item alist blist-i j) blist-j i)
             )
        )
    )
  )
)

;#INPUT
;@n = position
;@alist = a list
;#RETURNS
;nth-item
(define (nth-item n alist)
        (cond
                ((null? alist) alist)
                (#t (if (= n 1) (car alist)
                        (nth-item (- n 1) (cdr alist))))
))


;#INPUT
;@n = position
;@alist = a list
;@alist = a list that need to need added in placed of alist 
;#RETURNS
;replaced nth item list
(define (replace-nth-item alist blist n)
        (cond
                ((null? alist) alist)
                ((= n 1) (append blist (cdr alist)))
                (#t (append (list (car alist)) (replace-nth-item  (cdr alist) blist (- n 1))))
))

;Swap-elements Logic - End $$
;-------------------------------------------------------------------------
;*************************** GLOBAL VARIABLES ****************************

;#INPUT
;@map : adjency-map
;#RETURNS
;50 unique US states as per adjency-map
(define (getAllLocations map)
  (cond
    ((null? map) map)
    (#t (append
            (list (caar map))
            (getAllLocations (cdr map))
        )
    )
  )
)

(define map '(
  (Alabama Mississippi Tennessee Georgia Florida)
  (Alaska)
  (Arkansas Texas Oklahoma Missouri Tennessee Mississippi Louisiana)
  (Arizona California Nevada Utah New-Mexico)
  (California Arizona Nevada Oregon)
  (Colorado New-Mexico Utah Wyoming Nebraska Kansas Oklahoma)
  (Connecticut New-York Massachusetts Rhode-Island)
  (Delaware Maryland Pennsylvania New-Jersey)
  (Florida Alabama Georgia)
  (Georgia Florida Alabama Tennessee North-Carolina South-Carolina)
  (Hawaii)
  (Idaho Oregon Washington Montana Wyoming Utah Nevada)
  (Indiana Illinois Michigan Ohio Kentucky)
  (Illinois Missouri Iowa Wisconsin Indiana Kentucky)
  (Iowa Missouri Illinois Wisconsin Minnesota South-Dakota Nebraska)
  (Kansas Colorado Nebraska Missouri Oklahoma)
  (Kentucky Missouri Illinois Indiana Ohio West-Virginia Virginia Tennessee)
  (Louisiana Texas Arkansas Mississippi)
  (Maine New-Hampshire)
  (Maryland Virginia West-Virginia Pennsylvania Delaware)
  (Massachusetts Rhode-Island Connecticut New-York Vermont New-Hampshire)
  (Michigan Wisconsin Indiana Ohio)
  (Minnesota North-Dakota South-Dakota Iowa Wisconsin)
  (Mississippi Louisiana Arkansas Tennessee Alabama)
  (Missouri Oklahoma Kansas Nebraska Iowa Illinois Kentucky Tennessee Arkansas)
  (Montana Idaho Wyoming South-Dakota North-Dakota)
  (Nebraska Colorado Kansas Missouri Iowa South-Dakota Wyoming)
  (Nevada California Arizona Utah Idaho Oregon)
  (New-Hampshire Maine Vermont Massachusetts)
  (New-Jersey Delaware Pennsylvania New-York)
  (New-Mexico Texas Oklahoma Colorado Arizona)
  (New-York Pennsylvania New-Jersey Connecticut Massachusetts Vermont)
  (North-Carolina South-Carolina Georgia Tennessee Virginia)
  (North-Dakota Montana South-Dakota Minnesota)
  (Ohio Michigan Indiana Kentucky West-Virginia Pennsylvania)
  (Oklahoma Texas New-Mexico Colorado Kansas Missouri Arkansas)
  (Oregon Washington Idaho Nevada California)
  (Pennsylvania Ohio West-Virginia Maryland Delaware New-Jersey New-York)
  (Rhode-Island Connecticut Massachusetts)
  (South-Carolina Georgia North-Carolina)
  (South-Dakota Nebraska Iowa Minnesota North-Dakota Montana Wyoming)
  (Tennessee Arkansas Missouri Kentucky Virginia North-Carolina Georgia Alabama Mississippi)
  (Texas New-Mexico Oklahoma Arkansas Louisiana)
  (Utah Nevada Idaho Wyoming Colorado Arizona)
  (Vermont New-York Massachusetts New-Hampshire)
  (Virginia North-Carolina Tennessee Kentucky West-Virginia Maryland)
  (Washington Oregon Idaho)
  (West-Virginia Virginia Kentucky Ohio Pennsylvania Maryland)
  (Wisconsin Minnesota Iowa Illinois Michigan)
  (Wyoming Idaho Montana South-Dakota Nebraska Colorado Utah)
))

(define allLocations (getAllLocations map)) 
(define wildcardValue 'Wildcard)
