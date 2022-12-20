(defstruct pos
  (x 0 :type integer)
  (y 0 :type integer)
)

(defun dir-to-vec (direction amt)
	(ecase direction
		(:up (make-pos :x 0 :y amt))
		(:down (make-pos :x 0 :y (* -1 amt)))
		(:left (make-pos :x ( * -1 amt) :y 0))
		(:right (make-pos :x amt :y 0))
	)
)

(defun unbundle-moves (move)
	
)

(defun add-pos (fst snd)
	(make-pos :x (+ (pos-x fst) (pos-x snd)) :y (+ (pos-y fst) (pos-y snd)))
)

(defun read-direction (dir)
	(ecase dir
		(#\U :up)
		(#\D :down)
		(#\R :right)
		(#\L :left)
	)
)

(defun read-move (line)
	(let ((vals (uiop:split-string line :separator " ")))
		(dir-to-vec (read-direction (char (first vals) 0)) (parse-integer (second vals)))
	)
)

(defun read-moves ()
	(with-open-file (stream "input")
		(loop for line = (read-line stream nil)
			while line
			collect (read-move line)
		)
	)
)

; (defun make-moves (moves)
; 	(let (
; 			(head-pos (make-pos :x 0 :y 0))
; 			(tail-pos (make-pos :x 0 :y 0))
; 		)
; 		(list head-pos tail-pos)
; 	)
; )

(defun make-moves (moves)
	(let (
			(head-pos (make-pos :x 0 :y 0))
			(tail-pos (make-pos :x 0 :y 0))
		)
		(mapcar (lambda (move)
			(let ((updated (update-head-and-tail head-pos tail-pos move)))
				(progn
					(setf head-pos (first updated))
					(setf tail-pos (second updated))
				)
		)) moves)
	)
)

; return (head-pos tail-pos) 
(defun update-head-and-tail (head-pos tail-pos move)
	(let ((x-move
		(cond
			((<= (abs (- (pos-x head-pos) (pos-x tail-pos))) 1) 0)
			((> (pos-x head-pos) (pos-x tail-pos)) 1)
		 	(t -1))
	) (y-move
		(cond
			((<= (abs (- (pos-y head-pos) (pos-y tail-pos))) 1) 0)
			((> (pos-y head-pos) (pos-y tail-pos)) 1)
		 	(t -1))
	))
		(list (add-pos head-pos move) (add-pos tail-pos (make-pos :x x-move :y y-move)))
	)
)

; part 1
(length (remove-duplicates (make-moves (read-moves))))