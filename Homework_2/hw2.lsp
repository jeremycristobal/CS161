;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; BFS takes a single list argument FRINGE, which represents a search tree,
; and returns a top-level list of leaf nodes in the order they are visted by a 
; left-to-right breadth-first search.
(defun BFS (FRINGE)
    (
    cond ((atom FRINGE) FRINGE)
	      ; If FRINGE is an atom, simply return it
         ((null FRINGE) nil)
	      ; If FRINGE is an empty list, return nil
         ((atom (car FRINGE)) (cons (car FRINGE) (BFS (cdr FRINGE))))
	      ; If the first element in FRINGE is an atom, add it to the top-level
	      ; return list and call BFS on the rest of the list
         (t (BFS (append (cdr FRINGE) (car FRINGE))))
	      ; Else (the first element in FRINGE is a list), append every element
	      ; of the embedded list to the end of FRINGE and call BFS
    )
)

; Appending the elements of a list to the end of FRINGE is analagous to expanding
; a node and adding the children to the end of the frontier.  Because of this 
; first-in first-out approach, this is accurate of breadth-first search.

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call
; (DFS '(NIL NIL NIL NIL) NIL)
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (equal S '(T T T T))
         ; Here, we let "equal" do all the work for us
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer
; with dog, and p for homer with poison).
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
   (
   cond ((equal A 'h)
	             ; Only Homer will be moved
            (
	    if (and (equal (first S) (second S)) (not (equal (third S) (fourth S))))
		     ; If Homer and the baby are on the same side, they will not be
		     ; after the move, and if the dog and poison are not on the same
		     ; side, then the baby will be left alone with either the dog or
		     ; the poison.
	     	nil
	             ; So, our above conditions are met, we return nil
	        (list (cons (not (car S)) (cdr S)))
		     ; Otherwise, we negate the value of Homer in our list
	    )
	 )

	((equal A 'b)
	             ; Homer and the baby will be moved
	    (
	    if (not (equal (first S) (second S)))
		     ; The only condition of failure is Homer and the baby being on
		     ; different sides to begin with.
		nil
	             ; If they are on different sides, return nil
	        (list (list (not (first S)) (not (second S)) (third S) (fourth S)))
		     ; Otherwise, negate the values of Homer and the baby in our list
	    )
	)

	((equal A 'd)
	             ; Homer and the dog will be moved
	    (
	    if (or (not (equal (first S) (third S))) (equal (second S) (fourth S)))
		     ; If Homer and the dog are on different sides then this action
		     ; is illegal.  If the baby and the poison are on the same side,
		     ; then that means after the move, they will be alone together.
		nil
	             ; So, if either of these are true, we return nil
	        (list (list (not (first S)) (second S) (not (third S)) (fourth S)))
	             ; Otherwise, negate the values of Homer and the dog in our list
	    )
	)

	((equal A 'p)
	             ; Homer and the poison will be moved
	    (
	    if (or (not (equal (first S) (fourth S))) (equal (second S) (third S)))
		     ; Exact same logic as the dog statement: the dog and the poison
		     ; are identical in their properties with respect to the problem
		nil
	             ; So, if either condition is met we return nil
	        (list (list (not (first S)) (second S) (third S) (not (fourth S))))
		     ; Otherwise, we negate the values of Homer and the poison
	    )
	)

	(t nil)      ; This case should never happen
   )
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
   (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
                     ; We call NEXT-STATE with each of the four possible arguments
                     ; and put them in the list to be returned
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
   (
   cond ((null STATES) nil)
	             ; If STATES is empty, return nil
	((equal S (car STATES)) t)
	             ; If the top element of STATES is equal to S, return t
	(t (ON-PATH S (cdr STATES)))
	             ; Else, recursively call ON-PATH on the rest of STATES
   )
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
   (
   if (null STATES)
       nil            ; If STATES is empty, return nil
       (
       let ((result (DFS (car STATES) PATH)))
	              ; Else, create local variable 'result' and store the
	              ; value DFS gives on the fisrst element of STATES.
	              ; We store the value so we don't have to recompute it
	   (
	   if (null result)
	       (MULT-DFS (cdr STATES) PATH)
	              ; If the result is nil, recursively call MULT-DFS on
	              ; the remaining elements of STATES
	       result
	              ; Else, we have found a PATH so we return it
	   )
       )
   )
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
   (
   cond ((FINAL-STATE S) (append PATH (list S)))
	              ; If S is the final state, append it to the end of our
	              ; PATH and return it
	((ON-PATH S PATH) nil)
	              ; If S is already in the PATH, return nil
	(t (MULT-DFS (SUCC-FN S) (append PATH (list S))))
	              ; Else, we take the given node and expand it by calling
	              ; our helper function SUCC-FN, appending S to our PATH,
	              ; and calling MULT-DFS on them.
   )
)
