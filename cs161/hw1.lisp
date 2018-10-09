;;;; Name:  Feilan Wang
;;;; UID:   104796844
;;;; Class: CS 161
;;;; HW:    1



;;; 1. Write a single Boolean LISP function, called TREE-CONTAINS, 
;;;    which takes two arguments N and TREE, 
;;;    and checks whether number N appears in the ordered tree TREE. 


(defun TREE-CONTAINS (N TREE)											;Define function, takes in N and TREE as arguments.
	(cond ((numberp TREE) (= N TREE))									;If TREE is a number (not list), return value of TREE == N.
		  (t (or (TREE-CONTAINS N (car TREE)) 							;Else, return true if TREE-CONTAINS of left-child return true
		  		 (TREE-CONTAINS N (caddr TREE))))))						;or TREE-CONTAINS of right-child return true.



;;; 2. Write a single LISP function, called TREE-MIN,
;;;    which takes one argument TREE,
;;;    and return the minimum number appearing in the ordered tree TREE.


(defun TREE-MIN (TREE)													;Define function, takes in TREE as argument.
	(cond ((numberp TREE) TREE)											;If TREE is a number (not list), return TREE.
		  (t (TREE-MIN (car TREE)))))									;Else recursively call the TREE-MIN function on the L list of TREE.



;;; 3. Write a single LISP function, called TREE-ORDER, 
;;;    which takes one argument TREE, 
;;;    and return an pre-ordered list of the numbers appearing in the ordered tree TREE.


(defun TREE-ORDER (TREE)												;Define function, takes in TREE as argument.
	(cond ((numberp TREE) (cons TREE '()))								;If TREE is a number (not list), return a list containing TREE's value.
		  (t (cons (cadr TREE) 											;Else return a constructed list begin with TREE root's value, 
		  	       (append (TREE-ORDER (car TREE)) 						;followed by the TREE-ORDER of L,
		  	       (TREE-ORDER (caddr TREE)))))))						;and the TREE-ORDER of R.



;;; 4. Write a single LISP function, called SUB-LIST, 
;;;    that takes a list L and two non-negative integers START and LEN,
;;;    and return the sub-list of L starting at position START and having length LEN.
;;;    Assume that the first element of L has position 0.


(defun SUB-LIST (L START LEN)											;Define function, takes in List L, non-neg int START and LEN.
	(cond ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))				;If START > 0, recursively call SUB-LIST each time with START - 1 and L removing the first element, until START is 0.
		  (t (cond ((= LEN 0) nil)										;Else, if LEN is 0, return empty list.
		  	       (t (cons (car L) 									;	   else if LEN is not 0, return a constructed list begin with first element of L,
		  	       	        (SUB-LIST (cdr L) START (- LEN 1))))))))	;	   followed by the SUB-LIST of the rest of L and LEN - 1.



;;; 5. Write a single LISP function, called SPLIT-LIST, 
;;;    that takes a list L, 
;;;    and returns a list of two lists L1 and L2, in that order, such that
;;;			- L is the result of appending L1 and L2;
;;;			- Length of L1 minus length of L2 is 0 or 1.


(defun SPLIT-LIST (L)													;Define function, takes in List L as an argument.
	(list (SUB-LIST L 0 (/ 												;Return a newly created list made up of two SUB-LIST,
							(cond ((evenp (length L)) (length L))		;where the first SUB-LIST starts at 0,
	  							   (t (+ (length L) 1))) 				;and LEN is (int) LEN / 2.
	  					    2)
		  )
		  (SUB-LIST L (/ 												;While the second SUB-LIST starts at (int) LEN / 2,
		  					(cond ((evenp (length L)) (length L))
	  							   (t (+ (length L) 1)))
	  						2) 
		  			  (- 												;and LEN is (length of L) - (int) LEN / 2
		  					(length L) 
		  					(/ (cond ((evenp (length L)) (length L))
	  							      (t (+ (length L) 1))) 2))) ))



;;; 6. Write a single LISP function, called BTREE-HEIGHT, 
;;;    which takes a binary tree TREE, 
;;;    and returns the height of TREE. 
;;;    Note that the height of a binary tree is defined as the length of the longest path from the root node to the farthest leaf node. 


(defun BTREE-HEIGHT (TREE)												;Define function, takes in TREE as argument.
	(cond ((null TREE) 0)												;If TREE root is null, return 0.
		  ((numberp TREE) 0)											;Else if TREE root has no children, return 0.
		  ((>= (BTREE-HEIGHT (car TREE)) (BTREE-HEIGHT (cadr TREE))) 	;Else if BTREE-HEIGHT of left child >= BTREE-HEIGHT of right child,
		  		(+ 1 (BTREE-HEIGHT (car TREE))))						;return BTREE-HEIGHT of left child + 1.
		  (t (+ 1 (BTREE-HEIGHT (cadr TREE))))))						;Else, return BTREE-HEIGHT of right child + 1.



;;; 7. Write a single LISP function, called LIST2BTREE,
;;;    that takes a non-empty list of atoms LEAVES, 
;;;    and returns a binary tree such that
;;;			- The tree leaves are the elements of LEAVES;
;;;			- For any internal (non-leaf) node in the tree, the number of leaves in its left branch minus the number of leaves in its right branch is 0 or 1.


(defun LIST2BTREE (LEAVES)												;Define function, takes in list LEAVES as an argument.
	(cond ((= (length LEAVES) 1) (car LEAVES))							;If LEAVES has 1 element, return it as an atom.
		  ((= (length LEAVES) 2) LEAVES)								;Else if LEAVES has 2 elements, return it as a LIST.
		  (t (list (LIST2BTREE (car (SPLIT-LIST LEAVES))) 				;ELSE, SPLIT-LIST into two list L and R, call LIST2BTREE on L and R,
		  	       (LIST2BTREE (cadr (SPLIT-LIST LEAVES))) ))))			;and combine the results together into a new LIST for return. 



;;; 8. Write a single LISP function, called BTREE2LIST, 
;;;    that takes a binary tree TREE as input, 
;;;    and returns a list of atoms (assume TREE follows the constraints we defined earlier).
;;;			- As the input is a binary tree, each node has at most 2 children;
;;;			- This function is the inverse of LIST2BTREE. That is, (BTREE2LIST (LIST2BTREE X)) = X for all lists of atoms X.


(defun BTREE2LIST (TREE)												;Define function, takes in TREE as argument.
	(cond ((null TREE) nil)												;If TREE root is null, return empty list.
		  ((numberp TREE) (cons TREE '() ))								;Else if TREE root has no children, return a list with one element - TREE's val.
		  (t (append (BTREE2LIST (car TREE)) 							;Else (TREE is a list with two members), append the BTREE2LIST of first member
		  			 (BTREE2LIST (cadr TREE)) ))))						;to the BTREE2LIST of the second member. 



;;; 9. Write a single Boolean LISP function, called IS-SAME, 
;;;    that takes two LISP expressions E1 and E2 whose atoms are all numbers, 
;;;    and checks whether the expressions are identical. 
;;;    In this question, you can only use ‘=‘ to test equality (you cannot use ‘equal’). 
;;;    Recall that a LISP expression is either an atom or a list of LISP expressions.


(defun IS-SAME (E1 E2)													;Define function, takes in E1 and E2 as arguments.
	(cond ((and (null E1) (null E2)) t)									;If both E1 and E2 are empty list, return true.
		  ((and (numberp E1) (numberp E2))								;Else if both E1 and E2 are number (not list), 
		    	(= E1 E2))												;return if E1 and E2 are same number.
		  ((or (numberp E1) (numberp E2)) 								;Else if one is number and the other is list,
		  		nil)													;return false.
		  (t 															;Else, return first item of E1 and E2 IS-SAME and rest item of E1 and E2 IS-SAME.
		  		(and (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))))