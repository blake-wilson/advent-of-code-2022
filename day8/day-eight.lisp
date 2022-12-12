(defun read-input ()
  (with-open-file (stream "input")
    (let ((heights '()))
        (loop for line = (read-line stream nil)
             while line do (setf heights (append  heights (mapcar 'digit-char-p(coerce line 'list))))
        )
      heights
    )
  )
)

(defparameter width (with-open-file (stream "input") (let (( line (read-line stream nil))) (length line))))

(defun populate-arr (arr chars width)
  (loop for i from 0 below (length chars)
        do (setf (row-major-aref arr i) (nth i chars))
  )
)

(defparameter grid (make-array (list width width)))

(populate-arr grid (read-input) width)

(defun find-visible (grid ref-func)
  (let ((visible '()))
    (loop for i from 0 below width
          do (let ((max-height -1))
              (loop for j from 0 below width
                    do
                        (let* ((refs (funcall ref-func i j))
                              (fst (first refs))
                              (snd (second refs)))
                            (if (> (aref grid fst snd) max-height)
                               (progn
                                 (setf max-height (aref grid fst snd))
                                 (setf visible (cons (list fst snd) visible))
                               )
                            )
                        )
               )
          )
    )
    visible
  )
)

(defun find-visible-l-to-r (grid)
  (find-visible grid (lambda (i j) (list j i)))
)

(defun find-visible-r-to-l (grid)
  (find-visible grid (lambda (i j) (list (- 98 j) i)))
)

(defun find-visible-t-to-b (grid)
  (find-visible grid (lambda (i j) (list i j)))
)

(defun find-visible-b-to-t (grid)
  (find-visible grid (lambda (i j) (list i (- 98 j) )))
)

(defun find-visible-all (grid)
  (remove-duplicates (append (find-visible-l-to-r grid)
          (append (find-visible-r-to-l grid)
                  (append (find-visible-t-to-b grid)
                          (append (find-visible-b-to-t grid)))))
                     :test (lambda (a b) (and (equal (first a) (first b)) (equal (second a) (second b)))))
)

; part 1
(length (find-visible-all grid))

; begin part 2
(defun get-view (grid x y)
  (let ((height (aref grid y x))
    (score-components (make-array 4)))
    (loop for i from (+ x 1) below width
          do (setf (aref score-components 0) (+ 1 (aref score-components 0)))
          if (<= height (aref grid y i)) return '()
     )
    (loop for i from (- x 1) downto 0
          do (setf (aref score-components 1) (+ 1 (aref score-components 1)))
          if (<= height (aref grid y i)) return '()
     )
    (loop for j from (+ y 1) below width
          do (setf (aref score-components 2) (+ 1 (aref score-components 2)))
          if (<= height (aref grid j x)) return '()
     )
    (loop for j from (- y 1) downto 0
          do (setf (aref score-components 3) (+ 1 (aref score-components 3)))
          if (<= height (aref grid j x)) return '()
     )
    score-components
  )
)

(defun find-view-scores (grid)
  (loop for i from 1 below (- width 1)
    append (loop for j from 1 below (- width 1)
       collect (list (list i j) (get-view grid i j))
    )
  )
)


(defun max-score (scores)
  (apply 'max (mapcar
                (lambda (comps)
                    (apply '* (coerce (second comps) 'list)))
                    scores))
)

; part 2
(max-score (find-view-scores grid))
