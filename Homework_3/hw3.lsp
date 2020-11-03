;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	              col
	          (getKeeperColumn (cdr r) (+ col 1))
		       );end if
	      );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	          (if x
		       ;keeper is in this row
		       (list row x)
		     ;otherwise move on
		     (getKeeperPosition (cdr s) (+ row 1))
		      );end if
		         );end let
	    );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		  (res (cleanUpList (cdr L)))
		   )
	          (if cur 
		       (cons cur res)
		      res
		       )
		       );end let
	      );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; This function takes a single state (s) as its argument
; It checks every row recursively and checks every element within
; each row recursively.  If it detects a box, the goal state has
; not yet been reached and it returns nil.  If no box has been
; detected, then we are left with (true or true) several times
; over, so it returns true.
(defun goal-test (s)
    (cond ((null s) t)
          ((atom s)
             (
             if (isBox s)
                 nil
                 t
             )
          )
          (t (and (goal-test (car s)) (goal-test (cdr s))))
    )
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

; get-square takes three arguments: s is a state, r is the row number we 
; want to find, and c is the column number we want to find.  If either
; go outside of its boundary (negative number or larger than state) then
; it returns 1, as if it were a wall.  Otherwise, it returns the value
; found at (r, c), assuming that we start from (0, 0).  I switched the
; values of r and c that functions such as getKeeperPosition had because
; it made more sense to me.
(defun get-square (s r c)
    (
    if (or (< r 0) (< c 0))
        1
        (
        let ((res (nth c (nth r s))))
            (
            if (null res)
                1
                res
            )
        )
    )
)

; set-square-helper is a helper function for set-square that takes three
; arguments: row is the target row number, index is the target column
; number, and value is the value we want to give the spot occupied by
; (row, index)
(defun set-square-helper (row index value)
    (
    cond ((null row) nil)
         ((> index 0) (cons (car row) (set-square-helper (cdr row) (- index 1) value)))
         (t (cons value (cdr row)))
    )
)

; set-square is a function that takes four arguments: s is state, r is
; the row number we want to set, c is the column number we want to set,
; and v is the value we want to give to (r, c).
(defun set-square (s r c v)
    (
    cond ((null s) nil)
         ((> r 0) (cons (car s) (set-square (cdr s) (- r 1) c v)))
         (t (cons (set-square-helper (car s) c v) (cdr s)))
    )
)

; try-move is a function that attempts to move the keeper in a specified
; direction and returns the result if successful, and returns nil if 
; unsuccessful.  It takes four arguments: s is a state, d is the direction,
; r is the row number, and c is the column number.
(defun try-move (s d r c)
    (
    let ((changeold
        (
        if (isKeeperStar (get-square s r c))
            (set-square s r c star)
            (set-square s r c blank)
            ; We set the old occupied square early, as to avoid having to repeat this statement
        )))
        (
        let* ((next_row
                (
                cond ((equal d "UP") (- r 1))
                    ((equal d "DOWN") (+ r 1))
                    (t r)
                    ; If direction is up or down, we change it for next_row
                ))
             (next_col
                (
                cond ((equal d "LEFT") (- c 1))
                    ((equal d "RIGHT") (+ c 1))
                    (t c)
                    ; Similarly, if direction is left or right we change it for next_col
                ))
             (afterbox_row
                (
                cond ((equal d "UP") (- r 2))
                    ((equal d "DOWN") (+ r 2))
                    (t r)
                    ; Repeat the process, but for two spaces to accomodate for boxes
                ))
             (afterbox_col
                (
                cond ((equal d "LEFT") (- c 2))
                    ((equal d "RIGHT") (+ c 2))
                    (t c)
                ))
             (next (get-square s next_row next_col))
             (afterbox (get-square s afterbox_row afterbox_col))
            )
            (
            cond ((isWall next) nil)
                    ; If it's a wall, we cannot move there
                 ((isBlank next) (set-square changeold next_row next_col keeper))
                    ; If it's blank, moving is easy
                 ((isStar next) (set-square changeold next_row next_col keeperstar))
                    ; If it's a star, moving is still easy
                 (t
                    ; This means that a box or boxstar is next
                    (
                    cond ((isStar afterbox) 
                        (
                        if (isBox next)
                            (set-square (set-square changeold next_row next_col keeper) afterbox_row afterbox_col boxstar)
                                ; If Keeper then Box then Star
                            (set-square (set-square changeold next_row next_col keeperstar) afterbox_row afterbox_col boxstar)
                                ; If Keeper then BoxStar then Star
                        ))
                         ((isBlank afterbox)
                        (
                        if (isBox next)
                            (set-square (set-square changeold next_row next_col keeper) afterbox_row afterbox_col box)
                                ; If Keeper then Box then Blank
                            (set-square (set-square changeold next_row next_col keeperstar) afterbox_row afterbox_col box)
                                ; If Keeper then BoxStar then Blank
                        ))
                        (t nil)
                    )
                 )
            ) 
        )   
    )
)

; This function takes a single argument s, which is representative
; of a state, and returns all possible values for the next state.
; The result is filtered to remove nils, so indeed only possible
; states are returned.
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	  (x (car pos))
	   (y (cadr pos))
	    ;x and y are now the coordinate of the keeper in s.
	    (result (list (try-move s "UP" x y) (try-move s "DOWN" x y) (try-move s "LEFT" x y) (try-move s "RIGHT" x y )))
	     )
    (cleanUpList result);end
   );end let
);

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
; This function takes a single argument s, which is representative
; of a state, and always returns 0
(defun h0 (s)
    0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
; This function takes a single argument s, and computes how
; many boxes there are that are not on a star.  This implementation
; is nearly identical to goal-test, as essentially it is
; accomplishing the same idea but with the added bonus of counting.
(defun h1 (s)
    (cond ((null s) 0)
          ((atom s)
             (
             if (isBox s)
                 1
                 0
             )
          )
          (t (+ (h1 (car s)) (h1 (cdr s))))
    )
)

; box-coords is a helper function for my hueristic.  It takes
; three arguments, s is a state, r is the current row, and c is
; the current column.  If a box is found, it returns the coordinates
; in a list (r c) and returns a list of these coordinates.  It checks
; through the state recursively.
(defun box-coords (s r c)
    (
    cond ((null s) nil)
         ((atom s) 
            (
            if (isBox s)
                (list r c)
                nil
            )
         )
         (t 
            (
            if (atom (car s))
                (cons (box-coords (car s) r c) (box-coords (cdr s) r (+ c 1)))
                (append (box-coords (car s) r c) (box-coords (cdr s) (+ r 1) c))
            )
         )
    )
)

; star-coords is a helper function for my hueristic.  It takes
; three arguments, s is a state, r is the current row, and c is
; the current column.  If a star is found, it returns the coordinates
; in a list (r c) and returns a list of these coordinates.  It checks
; through the state recursively.
(defun star-coords (s r c)
    (
    cond ((null s) nil)
         ((atom s) 
            (
            if (or (or (isStar s) (isKeeperStar s)) (isBoxStar s))
                (list r c)
                nil
            )
         )
         (t 
            (
            if (atom (car s))
                (cons (star-coords (car s) r c) (star-coords (cdr s) r (+ c 1)))
                (append (star-coords (car s) r c) (star-coords (cdr s) (+ r 1) c))
            )
         )
    )
)

; box-val is a helper function that returns the manhattan distance
; minus 1 of the keeper from a given box.  We subtract one because
; the keeper can never be physically on top of the box.  It takes 
; two arguments: box_pos holds the coordinates of the given box in
; a list (r c), and keep holds the coordinates of the keeper in a
; list (r c).
(defun box-val (box_pos keep)
    (- (+ (abs (- (first keep) (first box_pos))) (abs (- (second keep) (second box_pos)))) 1)
)

; star-val is a helper function that is almost identical in nature
; to box-val.  It takes two arguments: star_pos holds the coordinates
; of a given star and box_pos holds the coordinates of a given box.
; star-val does not subtract one because boxes and stars can overlap.
(defun star-val (star_pos box_pos)
    (+ (abs (- (first box_pos) (first star_pos))) (abs (- (second box_pos) (second star_pos))))
)

; adjust-val is a helper function that makes my hueristic unique.
; depending on the position of the keeper on the state, it may have
; to move without the box in order to get the box properly to its place.
; This function counts those movements and adds it to the total.  It
; takes six arguments: k_row and k_col are the values of the keeper's
; location, b_row and b_col are the values of the box's location, and
; s_row and s_col are the values of the star's location.  The logic for
; each of the numbers added were calculated by hand, but generally for
; each time the box must change direction, the keeper must move twice.
(defun adjust-val (k_row k_col b_row b_col s_row s_col)
    (
    let ((sequential 
        (
        if (or (> k_col b_col s_col) (< k_col b_col s_col))
                ;This value came up often, so I assigned it to a variable
            t
            nil
        )))
        (
        cond ((= s_row b_row)
                (
                if sequential
                    0
                    (
                    if (= k_row b_row)
                        4
                        2
                    )
                )
             )
             ((or (= k_row b_row) (= k_row s_row))
                (
                if (or sequential (= b_col s_col))
                    2
                    4
                )
             )
             ((or (< k_row b_row s_row) (> k_row b_row s_row))
                (
                if (= b_col s_col)
                    0
                    2
                )
             )
             (t
                (
                if (or sequential (and (= s_col b_col) (not (= k_col b_col))))
                    2
                    4
                )
             )
        )
    )
)

; choose-star is a helper function that decides which star will take
; the least amount of moves for a given box to reach.  This factors in
; the adjustment value previously discussed.  It takes three arguments:
; stars is the list of stars in state, keep is the location of the keeper,
; and close_box is the box currently under investigation.  
(defun choose-star (stars keep close_box)
    (
    cond ((null stars) 1000)
            ; 1000 is a trivial number that serves as a fail
            ; it is large because this function searches for smaller numbers
         ((atom (car stars))
            (+ (star-val stars close_box) (adjust-val (first keep) (second keep) (first close_box) (second close_box) (first stars) (second stars)))
                ; add the manhattan distance from the box to the star to the adjust value
         )
         (t
            (
            let ((top (choose-star (car stars) keep close_box))
                 (rest (choose-star (cdr stars) keep close_box)))
                (
                if (< top rest)
                    top
                    rest
                )
            )
         )
    )
)

; closest-star is a helper function that takes two arguments: stars is the list
; of stars in the state and box is the box currently under investigation.  This
; function finds and returns the manhattan distance of the closest star to the 
; given box
(defun closest-star (stars box)
    (
    cond ((null stars) 1000)
         ((atom (car stars)) (star-val stars box))
         (t
            (
            let ((top (closest-star (car stars) box))
                 (rest (closest-star (cdr stars) box)))
                 (
                 cond ((< top rest) top)
                      (t rest)
                 )
            )
         )
    )
)

; best-adj-starbox-val is a helper function that takes four arguments:
; stars is the list of stars in the state, keep is the position of the keeper
; in the state, boxes is the list of boxes in the state, and place keeps track
; of the index of the boxes list so that we know which box is best.  The
; purpose of this function is to determine which box to choose as 'best' and
; get the adjust value + the manhattan distance of the keeper to it.  The return
; value is a list of two that contains (index value)
(defun best-adj-starbox-val (stars keep boxes place)
    (
    cond ((null boxes) (list 1000 1000))
         ((atom (car boxes))
            (list place (+ (choose-star stars keep boxes) (box-val boxes keep)))
         )
         (t
            (
            let ((top (best-adj-starbox-val stars keep (car boxes) place)) 
                 (rest (best-adj-starbox-val stars keep (cdr boxes) (+ place 1))))
                 (
                 cond ((< (second top) (second rest)) top)
                        ; The lower second value (which is the value, not the place)
                        ; is the one that is returned
                      (t rest)
                 )
            )
         )
    )
)

; filter_bestbox is a helper function that takes three arguments: boxes
; is the list of boxes in the state, pos is the index value we want to remove,
; and count is how many we've already looked through.  This function takes the
; box we determined to be 'best' and removes it from our list of boxes.
(defun filter_bestbox (boxes pos count)
    (
    cond ((null boxes) nil)
         ((= pos count) (cdr boxes))
         (t
            (append (list (car boxes)) (filter_bestbox (cdr boxes) pos (+ count 1)))
         )
    )
)

; rest_box_stars_dist is a helper function that takes two arguments: boxes
; is the list of boxes in the state, and stars is the list of stars in the
; state.  This function recursively iterates through the list of boxes and
; finds the star that has the smallest manhattan distance from it.  The 
; return value is the total manhattan distance of the remaining boxes to
; their closest star.
(defun rest_box_stars_dist (boxes stars)
    (
    cond ((null boxes) 0)
         ((atom (car boxes))
            (closest-star stars boxes)
         )
         (t
            (+ (rest_box_stars_dist (car boxes) stars) (rest_box_stars_dist (cdr boxes) stars))
         )
    )
)

; check_corners is a function that takes two arguments: the state s and
; the list of boxes boxes.  If a box is found in a corner, then we return
; true.  We check this because we know that if a box is in a corner, then
; it will be impossible to ever get it out, so we should fail it as soon
; as possible.
(defun check_corners (s boxes)
    (
    cond ((null boxes) nil)
         ((atom (car boxes))
            (
            let ((above (get-square s (- (first boxes) 1) (second boxes)))
                 (below (get-square s (+ (first boxes) 1) (second boxes)))
                 (left (get-square s (first boxes) (- (second boxes) 1)))
                 (right (get-square s (first boxes) (+ (second boxes) 1))))
                 (
                 if (and (or (isWall above) (isWall below)) (or (isWall left) (isWall right)))
                    t
                    nil
                 )
            )
         )
         (t
            (or (check_corners s (car boxes)) (check_corners s (cdr boxes)))
         )
    )
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
; h604982952 is my hueristic function.  The idea is to find a 'best box'
; that is determined by the manhattan distance to the keeper, the manhattan
; distance of the box to its best star, and the necessary adjustments needed
; by the keeper to get the box to its star.  Then, we find the manhattan
; distance of every other box to its closest star and add these two values
; together.  This function is admissible.
(defun h604982952 (s)
    (
    let* ((boxes (cleanUpList (box-coords s 0 0)))
            ; get list of boxes in state
          (stars (cleanUpList (star-coords s 0 0)))
            ; get list of stars in state
          (pos (getKeeperPosition s 0))
            ; get position of keeper in state
          (best_start_box (best-adj-starbox-val stars pos boxes 0))
            ; get the best_box and its adjusted value
          (rem_boxes (cleanUpList (filter_bestbox boxes (first best_start_box) 0)))
            ; filter the box list to remove the best box
          (star_dist (rest_box_stars_dist rem_boxes stars))
            ; get the summation of the remaining manhattan distances of boxes
            ; to their stars
          (total (+ (second best_start_box) star_dist))
            ; add the best box value to the other box values
          )
          (
          cond ((null boxes) 0)
                ; if boxes is empty, we reached the goal so we end
               ((check_corners s boxes) 1000)
                ; if a box is in the corner, we give an impossibly
                ; high value so we never check further
               (t total)
          )
    )
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))
   

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))
   
;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))


;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
            (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
            (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	  (k2 (getKeeperPosition s2 0))
	   (deltaX (- (car k2) (car k1)))
	    (deltaY (- (cadr k2) (cadr k1)))
	     )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	    (t (if (> deltaX 0) 'RIGHT 'LEFT))
	      );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
