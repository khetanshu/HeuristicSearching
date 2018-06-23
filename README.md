# HeuristicSearching
An implementation  searching algorithms that will sort a list of locations on a map so that they form a continuous path through adjacent locations.  

The program contains:
 1. A* Algorithm       (WILDCARD Entry supported)
 2. id-dfs Algorithm   (WILDCARD Entry supported)

******************DESCRIPTION OF THE HEURISTIC ***************************

 A* SCORE(for any state) = (CURRENT COST  + ESTIMATED COST to the reach the GOAL-STATE)
 
 Also, 
 ESIMATED COST = (MAXIMUM POSSIBLE SWAPS - ADJACENCY SCORE of the current state)
 
 where,
 MAXIMUM POSSIBLE SWAPS  would = N-1 (N be the # of cities into consideration)
 ADJACENCY SCORE would be # of consequetive neighbour adjacent to each other.

 The above hueristic is admisible, it would never over estimate the cost to reach the GOAL state. Below is the explation, as in why,

 For example, consider a scenarios where we have a GOAL STATE (A B C D E), here N = 5
 This means that following pairs ( AB, BC, CD, DE )  are adjacent to each other i.e. total N-1 pairs.
 Which means that in the worst case if none of the cities for the above example are in the right position,
 then it would take no more than N-1 swaps to form the correct path A->B->C->D->E or E->D->C->B->A 

 say, for the above example, we have two intermediate states 
 STATE_A = ((A B C E D) ((1 3))  
 STATE_B = ((B A C D E) ()) 

 Therefore for,
 STATE_A: 	
 	CURRENT COST = 1 {# swaps so far}
	ADJACENCY SCORE = 3 {as (AB, BC, ED) are adjacent to each other}
	so,
	A* SCORE = 1 + ((N-1) - 3) = 1 + (4 - 3) = 2 {i.e. to reach the goal state it would require total of 2 swaps}
 Similarly for,
 STATE_B:
 	CURRENT COST = 0 
 	ADJACENCY SCORE =3  {as (BA, CD, DE) are adjacent to each other}
 	so,
 	A* SCORE = 0 + (4 - 3) = 1  {i.e. to reach the goal state it would require total of 1 swap}

 Here, A* SCORE for STATE B < A* SCORE of STATE A. Therefore the optimal STATE would be STATE_B.	

 ***Notice that if we don't consider the CURRENT COST then for the above example the algorithm could have picked STATE A as
 an optimal state, in that case in for the above example chosing A would have been a wrong decison as it would take total of
 2 swaps however A would take only 1.

 This shows that the above heuristic would alway find an optimal solution and is admisible.
 
 Therefore, STATE selection using A* algorithm would be as follows, 
 1> Compute the A* scores 
 2> Scan for STATE which has LEAST A* score (which mean that which requires least total swaps to reach the goal state), and pick
 3> Now append all of the FOUND-STATE's childern into the FRONTIER pool, 
 4> Repeat until the GOAL STATE is found OR EXIT if no GOAL state is found. 


