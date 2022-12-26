; prevent endless loops when printing squares
(setf *print-circle* t)

(defstruct square
	(path-cost -1 :type integer)
	(row 0 :type integer)
	(col 0 :type integer)
	(height 0 :type integer)
	(end nil :type boolean)
	(above nil :type (or square null))
	(below nil :type (or square null))
	(left nil :type (or square null))
	(right nil :type (or square null))
)

(defun neighbors (grid square)
	(let ((neighbors '())
		(i (square-row square))
		(j (square-col square))
	)
		; above
		(if (> i 0)
			(setf neighbors (cons (aref grid (- i 1) j) neighbors))
		)
		; right
		(if (< i (- (array-dimension grid 1) 1))
			(setf neighbors (cons (aref grid i (+ j 1)) neighbors))
		)
		; left
		(if (> j 0)
			(setf neighbors (cons (aref grid i (- j 1)) neighbors))
		)
		; below
		(if (< i (- (array-dimension grid 0) 1))
			(setf neighbors (cons (aref grid (+ i 1) j) neighbors))
		)
		neighbors
	)
)

(defparameter grid (make-array '(41 162) :initial-element (make-square)))
(defparameter starting-point '())

(defun build-grid (input)
	(let ((row 0))
		(progn (loop for line = (read-line input nil)
				while line
				do (progn
						(loop for col from 0 below (length line)
							do (
								let (
									(end (equal (char line col) #\E))
									(height 
										(case (char line col)
											(#\E (char-code #\z))
											(#\S (char-code #\a))
											(otherwise (char-code (char line col)))
										)
									)
								)
									(setf (aref grid row col) (make-square :row row :col col :end end :height height))
							)
						)
					(incf row)
				)
			)
		)
	)
)

(defun read-grid ()
	(with-open-file (stream "input")
		(build-grid stream)
	)
)

(defun traverse-grid (square grid curr-score)
	(mapcar
		(lambda (next)
			(format t "traversing row ~d col ~d score ~d next-score ~d~%" (square-row square) (square-col square) curr-score (square-path-cost next))
			(if (<= (- (square-height square) (square-height next)) 1)
				(if (or (eq (square-path-cost next) -1) (< curr-score (square-path-cost next)))
					(if (not (square-end square))
						(progn
							(setf (square-path-cost square) curr-score)
							(setf (aref grid (square-row square) (square-col square)) square)
							(traverse-grid next grid (+ curr-score 1))
						)
						(progn
							(format t "ended with score of ~d~%~%" curr-score)
							curr-score
						)
					)
					(format t "done~%")
				)
				(format t "height diff was ~d~%" (- (square-height square) (square-height next)))
			)
		)
		(neighbors grid square))
)


(read-grid)
(traverse-grid (aref grid 20 0) grid 0)

; end
; (aref grid 20 139)