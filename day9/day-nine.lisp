(defstruct pos
  (x 0 :type integer)
  (y 0 :type integer)
)

(defun dir-to-moves (direction amt)
	(ecase direction
		(:up (make-list amt :initial-element (make-pos :x 0 :y 1)))
		(:down (make-list amt :initial-element (make-pos :x 0 :y -1)))
		(:left (make-list amt :initial-element (make-pos :x -1 :y 0)))
		(:right (make-list amt :initial-element (make-pos :x 1 :y 0)))
	)
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
		(dir-to-moves (read-direction (char (first vals) 0)) (parse-integer (second vals)))
	)
)

(defun read-moves ()
	(with-open-file (stream "input")
		(loop for line = (read-line stream nil)
			while line
			append (read-move line)
		)
	)
)

(defun square (num)
  (* num num)
)

(defun dist (fst-pos snd-pos)
  (sqrt (+
             (square (- (pos-x fst-pos) (pos-x snd-pos)))
             (square (- (pos-y fst-pos) (pos-y snd-pos)))
  ))
)

(defun is-touching (head-pos tail-pos)
  (< (dist head-pos tail-pos) 2)
)

(defun update-tail (head-pos tail-pos)
  (let (
        (x-move
		(if (not (is-touching head-pos tail-pos))
                    (if (/= (pos-x head-pos) (pos-x tail-pos))
                          (if (> (pos-x head-pos) (pos-x tail-pos)) 1 -1)
                          0
                    )
                    0
                ))
	(y-move
		(if (not (is-touching head-pos tail-pos))
                    (if (/= (pos-y head-pos) (pos-y tail-pos))
                          (if (> (pos-y head-pos) (pos-y tail-pos)) 1 -1)
                          0
                    )
                    0
                )
          )
    )
    (add-pos tail-pos (make-pos :x x-move :y y-move))
  )
)

(defun make-moves (moves num-knots)
	(let (
                    (rope (make-list num-knots :initial-element (make-pos :x 0 :y 0)))
		)
		(mapcar (lambda (move)
                        ; first, move head of the rope
                        (progn (setf (first rope) (add-pos (first rope) move))
                            (loop for i from 1 below (length rope)
                                  do
                                      (let* ((head-pos (nth (- i 1) rope))
                                             (tail-pos (nth i rope))
                                             (updated (update-tail head-pos tail-pos)))
			              	    (setf (nth i rope) updated)
                                      )
                            )
                            (make-pos :x (pos-x (car (last rope))) :y (pos-y (car (last rope))))
                        ))
                moves)
	)
)

; part 1
(length (remove-duplicates (make-moves (read-moves) 2) :test 'equalp))

; part 2
(length (remove-duplicates (make-moves (read-moves) 10) :test 'equalp))
