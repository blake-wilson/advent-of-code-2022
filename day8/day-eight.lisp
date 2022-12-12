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
