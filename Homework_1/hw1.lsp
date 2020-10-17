; Question 1
; PAD takes a single integer argument N and returns the Nth Padovan Number
(defun PAD (N)
    (
    if (< N 3)
        1    ; The base case (N = 0, 1, or 2) will always give a value of 1
        (+ (PAD (- N 3)) (PAD (- N 2)) (PAD (- N 1)))
    )        ; Else, use recursion to calculate the values needed to use
)            ; the definition of the Padovan Sequence
  
; Question 2
; SUMS takes a single integer argument N and returns the number of required
; additions necessary for PAD to compute the Nth Padovan Number
(defun SUMS (N)
    (
    if (< N 3)
	0    ; If N < 3 the value of PAD is given, so no addition is necessary
        ( + 2 (SUMS (- N 3)) (SUMS (- N 2)) (SUMS (- N 1)))
    )        ; Else, we add 2 because the Padovan Sequence requires two +
)            ; operations, then use recursion to collect the SUMS of its parts

; Question 3
; ANON takes a single list or atom argument TREE, replaces all atoms of TREE
; with 0, then returns the modified TREE
(defun ANON (TREE)
    (
    cond ((not TREE) nil) ; If TREE is empty list, return nil
	 ((atom TREE) 0)  ; If TREE is an atom, return 0 to replace TREE value
	 (t (cons (ANON (car TREE)) (ANON (cdr TREE))))
    )            ; Else, recursively call ANON separately on the first element
)                ; and other elements of TREE then recombine the parts
