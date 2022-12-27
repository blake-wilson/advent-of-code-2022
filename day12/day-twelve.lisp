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

(defun table-key (square)
  (+ (* (square-row square) (array-dimension grid 1)) (square-col square))
)

(defun unvisited-neighbors (grid square unvisited-table)
  (let ((neighbors (neighbors grid square)))
    (remove-if-not (lambda (sq) (gethash (table-key sq) unvisited-table)) neighbors)
  )
)

(defparameter grid (make-array '(41 161) :initial-element (make-square)))
(defparameter starting-point '())

(defun build-grid (input)
	(let ((row 0))
		(progn (loop for line = (read-line input nil)
				while line
				do (progn
						(loop for col from 0 below (length line)
							do (
								let* (
									(end (equal (char line col) #\E))
                                                                        (start (equal (char line col) #\S))
									(height
										(case (char line col)
											(#\E (char-code #\z))
											(#\S (char-code #\a))
											(otherwise (char-code (char line col)))
										)
									)
                                                                        (cost (if start 0 -1))
								)
									(setf (aref grid row col) (make-square :row row :col col :end end :height height :path-cost cost))
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

; (defun traverse-grid (square grid curr-score depth max-depth)
; 	(mapcar
; 		(lambda (next)
;                         ; (if (and (eq (square-row square) 20) (eq (square-col square) 139))
;                         ;     (format t "traversing row ~d col ~d score ~d next-score ~d~%" (square-row square) (square-col square) curr-score (square-path-cost next))
;                         ; )
; 			(if (and (< depth max-depth) (<= (- (square-height next) (square-height square)) 1))
; 				(if (or (eq (square-path-cost next) -1) (< curr-score (- (square-path-cost next) 1)))
; 					(if (not (square-end square))
; 						(progn
; 							(setf (square-path-cost square) curr-score)
; 							(setf (aref grid (square-row square) (square-col square)) square)
; 							(traverse-grid next grid (+ curr-score 1) (+ depth 1) max-depth)
; 						)
; 						(progn
; 							;(format t "ended with score of ~d~%~%" curr-score)
; 							curr-score
; 						)
; 					)
; 					; (format t "done~%")
; 				)
; 				; (format t "height diff was ~d~%" (- (square-height square) (square-height next)))
; 			)
; 		)
; 		(neighbors grid square))
; )

(defun populate-unvisited (unvisited-table)
  (loop for i from 0 below (array-dimension grid 0)
        do (loop for j from 0 below (array-dimension grid 1)
            do (
                    setf (gethash (table-key (aref grid i j)) unvisited-table) (aref grid i j)
            )
        )
  )
  unvisited-table
)

(defun least-dist-unvisited (unvisited-table)
  (let ((least-dist 10000)
        (square nil))
    (maphash (lambda (key val) (
        if (and (/= (square-path-cost val) -1) (> least-dist (square-path-cost val)))
            (progn
                (setf least-dist (square-path-cost val))
                (setf square val)
            )
    )) unvisited-table)
    square
  )
)

(defun count-unvisited (unvisited-table)
  (let ((count 0))
    (maphash (lambda (key val) (if (/= -1 (square-path-cost val)) (incf count 1))) unvisited-table)
    count
  )
)


(defun dijkstra (grid start iters)
  (let (
        (curr start)
        (unvisited (make-hash-table :test #'equal))
    )
    (populate-unvisited unvisited)
    (loop
      ; for i from 0 below iters
      while (> (count-unvisited unvisited) 0)
      do (
        progn
            ; (format t "unvisited count is ~d~%" (count-unvisited unvisited))
            ; (format t "neighbors are ~S~%" (unvisited-neighbors grid curr unvisited))
            (mapcar (lambda (next)
                (if (and (<= (- (square-height next) (square-height curr)) 1))
	           (if (or (eq (square-path-cost next) -1) (< (square-path-cost curr) (- (square-path-cost next) 1)))
	           	(if (not (square-end next))
	           		(progn
	           			(setf (square-path-cost next) (+ (square-path-cost curr) 1))
	           			(setf (aref grid (square-row next) (square-col next)) next)
	           			(setf (gethash (table-key next) unvisited) next)
                                        ; (format t "set unvisited to ~S~%" next)
	           		)
	           		(progn
	           			(format t "ended with score of ~d~%~%" (square-path-cost curr))
	           		)
	           	)
	           	; (format t "done; next cost is ~d~%" (square-path-cost next))
	           )
	           ; (format t "height diff was ~d~%" (- (square-height square) (square-height next)))
            )) (unvisited-neighbors grid curr unvisited))
            (remhash (table-key curr) unvisited)
            (let ((next (least-dist-unvisited unvisited)))
              (if next
                (setf curr next)
                (progn (format t "no next neighbor~%") (return (square-path-cost curr)))
              )
            )
        )
      )
  )
)


(read-grid)
; (traverse-grid (aref grid 20 0) grid 0 0 10000)
(dijkstra grid (aref grid 20 0) 2)

; end
; (aref grid 20 139)


; too high
; 1478
