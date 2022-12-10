(defun read-move (line)
  ; first, third, and fifth words are ignorable
  (let ((nums (uiop:split-string line :separator " ")))
    (list (parse-integer (second nums)) (parse-integer (nth 3 nums)) (parse-integer (nth 5 nums)))
  )
)
