(defun get-input ()
  (let ((stacks ()) (moves ()))
    (with-open-file (stream "input")
        ; first 8 lines are the stacks
        (setf stacks (loop for i from 1 to 8
          ; do (print (read-line stream nil))
          collect (read-line stream nil)
        ))
        ; two lines before moves begin
        (read-line stream nil) (read-line stream nil)
        (setf moves (loop for line = (read-line stream nil)
          while line
          collect (read-move line)
        ))
    )
    (list stacks moves)
  )
)

(defun read-move (line)
  ; first, third, and fifth words are ignorable
  (let ((nums (uiop:split-string line :separator " ")))
    (list (parse-integer (second nums)) (parse-integer (nth 3 nums)) (parse-integer (nth 5 nums)))
  )
)

(defun make-stacks (lines)
  (let ((buckets (make-array 9 :initial-element '())))
    ; buckets are defined by 3 chars [X] with one space between them
    (loop for line in lines
      do (loop for i from 0 to (- (length line) 4) by 4
         do
            (if (char/= (char line (+ i 1)) #\space)
                  (setf (aref buckets (/ i 4))
                    (nconc
                      (aref buckets (/ i 4))
                      (list (char line (+ i 1)))
                    )
                  )
            )
        )
    )
    (coerce buckets 'list)
  )
)

(defun move (stacks move)
  (let ((count (first move)) (src (- (second move) 1)) (dst (- (third move) 1)))
    (loop for i from 1 to count
      do (setf (nth dst stacks) (cons (car (nth src stacks)) (nth dst stacks)))
      do (setf (nth src stacks) (cdr (nth src stacks)))
    )
    stacks
  )
)


; part one result
(let* ((input (get-input))
       (stacks (make-stacks (first input)))
       (moves (second input))
  )
  (loop for move in moves
    do (setq stacks (move stacks move))
  )
  (mapcar 'first stacks)
)