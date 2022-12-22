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
                                        ; (list head-pos tail-pos)
				)
		)) moves)
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

; return (head-pos tail-pos)
(defun update-head-and-tail (head-pos tail-pos move)
	(let* ((head-new (add-pos head-pos move))
        (x-move
		(if (not (is-touching head-new tail-pos))
                    (if (/= (pos-x head-new) (pos-x tail-pos))
                          (if (> (pos-x head-new) (pos-x tail-pos)) 1 -1)
                          0
                    )
                    0
                ))
	(y-move
		(if (not (is-touching head-new tail-pos))
                    (if (/= (pos-y head-new) (pos-y tail-pos))
                          (if (> (pos-y head-new) (pos-y tail-pos)) 1 -1)
                          0
                    )
                    0
                )
          ))
		(list head-new (add-pos tail-pos (make-pos :x x-move :y y-move)))
        )
)

; part 1
(length (remove-duplicates (make-moves (read-moves)) :test 'equalp))
