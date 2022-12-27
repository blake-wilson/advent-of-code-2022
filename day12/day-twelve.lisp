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
		(if (< j (- (array-dimension grid 1) 1))
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

(defun table-key (grid square)
  (+ (* (square-row square) (array-dimension grid 1)) (square-col square))
)

(defun unvisited-neighbors (grid square unvisited-table)
  (let ((neighbors (neighbors grid square)))
    (remove-if-not (lambda (sq) (gethash (table-key grid sq) unvisited-table)) neighbors)
  )
)

(defun build-grid (input)
	(let ((row 0)
             (grid (make-array '(41 161) :initial-element (make-square))))
		(progn (loop for line = (read-line input nil)
				while line
				do (progn
						(loop for col from 0 below (length line)
							do (
								let* (
									(end (equal (char line col) #\E))
									(height
										(case (char line col)
											(#\E (char-code #\z))
											(#\S (char-code #\a))
											(otherwise (char-code (char line col)))
										)
									)
								)
									(setf (aref grid row col) (make-square :row row :col col :end end :height height :path-cost -1))
							)
						)
					(incf row)
				)
			)
		)
                grid
	)
)

(defun read-grid ()
	(with-open-file (stream "input")
		(build-grid stream)
	)
)

(defun populate-unvisited (grid unvisited-table)
  (loop for i from 0 below (array-dimension grid 0)
        do (loop for j from 0 below (array-dimension grid 1)
            do (
                    setf (gethash (table-key grid (aref grid i j)) unvisited-table) (aref grid i j)
            )
        )
  )
  unvisited-table
)

(defun least-dist-unvisited (unvisited-table)
  (let ((least-dist 10000)
        (square nil))
    (loop for val being each hash-value of unvisited-table do (
        if (and (/= (square-path-cost val) -1) (> least-dist (square-path-cost val)))
            (progn
                (setf least-dist (square-path-cost val))
                (setf square val)
            )
    ))
    square
  )
)

(defun count-unvisited (unvisited-table)
  (let ((count 0))
        (loop for val being each hash-value of unvisited-table do
              (if (/= -1 (square-path-cost val)) (incf count 1)))
        count
  )
)

(defun dijkstra (grid start end)
  (let (
        (curr start)
        (unvisited (make-hash-table :test #'equal))
    )
    (populate-unvisited grid unvisited)
    (setf (square-path-cost (aref grid (square-row start) (square-col start))) 0)
    (loop
      while (> (count-unvisited unvisited) 0)
      do (
        progn
            ; (format t "unvisited count is ~d~%" (count-unvisited unvisited))
            ; (format t "neighbors are ~S~%" (unvisited-neighbors grid curr unvisited))
            (mapcar (lambda (next)
                (if (and (<= (- (square-height next) (square-height curr)) 1))
	           (if (or (eq (square-path-cost next) -1) (< (square-path-cost curr) (- (square-path-cost next) 1)))
	           		(progn
	           			(setf (square-path-cost next) (+ (square-path-cost curr) 1))
	           			(setf (aref grid (square-row next) (square-col next)) next)
	           			(setf (gethash (table-key grid next) unvisited) next)
                                        ; (format t "set unvisited to ~S~%" next)
	           		)
	           		(progn
	           			; (format t "ended with score of ~d~%~%" (square-path-cost curr))
	           			(setf (aref grid (square-row next) (square-col next)) next)
	           		)
	           	; (format t "done; next cost is ~d~%" (square-path-cost next))
	           )
	           ; (format t "height diff was ~d~%" (- (square-height square) (square-height next)))
            )) (unvisited-neighbors grid curr unvisited))
            (remhash (table-key grid curr) unvisited)
            (let ((next (least-dist-unvisited unvisited)))
              (if next
                (setf curr next)
              )
            )
        )
      )
    (square-path-cost end)
  )
)

; part 1
(let ((grid (read-grid)))
    (dijkstra grid (aref grid 20 0) (aref grid 20 139))
)

; part 2
(defun get-a-elev-points (grid)
  (loop for i from 0 below (array-dimension grid 0)
        append (loop for j from 0 below (array-dimension grid 1)
            append (
                     if (= (square-height (aref grid i j)) (char-code #\a))
                        (list (aref grid i j))
            )
        )
  )
)

; sol. takes a while to execute
(let ((min-dist -1)
      (point (list 0 0)))
    (mapcar (lambda (sq) (
                         let* ((grid (read-grid))
                            (sq-dist (dijkstra grid (aref grid (square-row sq) (square-col sq)) (aref grid 20 139))))
                            (if (or (= min-dist -1) (and (/= -1 sq-dist) (< sq-dist min-dist)))
                                    (progn (< sq-dist min-dist) (format t "setting min dist to ~d~%" sq-dist)
                                        (setf min-dist sq-dist) (setf point (list (square-row sq) (square-col sq))))
                            ))
                        )(get-a-elev-points (read-grid)))
  (list min-dist point)
)
