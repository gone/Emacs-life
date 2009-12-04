;;conways game of life
;;RULES:
;Any life cell with fewer then two live neighbours dies, as if caused by underpopulation
;any live cell with more then three live neighbours dies, as if by overcrowding
;any live cell with two or three live neighbours lives on to the next generation
;any dead cell with three live neighbours becomes a live cell



(defun make-grid-hash ()
  "the gird is going to be a hashtable" 
  (let* ((totalsize (* size size))
		 (grid (make-hash-table :size totalsize :rehash-threshold 1.0)))
	(dotimes (x totalsize)
	  (puthash x 'nil grid))
	grid))

(defun make-grid-bv ()
  "the grid is going to be a bool-vector arrary"
  (make-bool-vector (* size size) 'nil))

(defun compute-life-of-cell (cell neigh)
  (let ((alive (count t neigh)))
	(if cell
		(cond ((< alive 2) ; fewer then two, die 
				 nil)
			  ((and (> alive 1) ;more then one, less then 4
					(> 4 alive)) ;stay alive
			   't)
			  ((> alive 3) ; overcrowed
			   nil))
	  (if (= alive 3)
		  't
	  nil))))

(defvar history ()
  "a history of the grid")

(defvar size 4
  "a single line in the grid")

(defvar thegrid (make-grid)
  "the current grid")
				 
(defvar make-grid make-bv-grid
  "the function to use to make a grid")

(defun make-grid ()
  (eval make-grid))

(defvar move-board move-board-bv
  "the function to use to make a grid")

(defun move-board ()
  (eval move-board))
(move-board)

(defun move-board-bv ()
  (let ((totalsize (* size size))
		(oldgrid (or thegrid
					 (make-grid-bv)))
		(thegrid (make-grid-bv)))
	(dotimes (x totalsize)
	  (aset thegrid x (compute-life-of-cell (aref oldgrid x)
										 (get-middle-neighbor-codes x))))
	(push thegrid history)
  thegrid))

(defun move-board-hash ()
  (let ((totalsize (* size size))
		(oldgrid thegrid)
		(thegrid (make-grid)))
	(dotimes (x totalsize)
	  (puthash x 
			   (compute-life-of-cell (gethash x oldgrid)
									 (mapcar (lambda (x) (gethash x oldgrid)) (get-middle-neighbor-codes x)))
			   thegrid))
	(push thegrid history)
  thegrid))

(defun take-turn ()
  (update-display (move-board)))

(defun update-display (grid)
  (message "the board is %s" grid)
  grid)
	
(defun get-middle-neighbor-codes (current)
  (let (neighbors)
	(push (1+ current) neighbors)  ;right
	(push (1- current) neighbors) ;left
	(push (+ size current) neighbors) ;below
	(push (- size current) neighbors) ;above
	neighbors))

(get-middle-neighbor-codes 4)
			  
(defun life-game ()
  "play the life game!"
  (while (or (> 2 (length history))
		   (equal (car history) (cadr history)))
	(setq thegrid (take-turn)))
  (message "the game is stable! %s went %s turns" thegrid (length history)))

(life-game)
	
