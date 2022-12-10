(defun get-input ()
  (uiop:read-file-string "input")
)

(defun find-unique-n (message num-unique)
  (loop for i from 0 below (length message)
    when
        (let ((window (subseq message i (+ i num-unique))))
          (= (length (remove-duplicates window)) num-unique)
        )
    return i
  )
)

; part one
(+ (find-unique-n (get-input) 4) 4)

; part two
(+ (find-unique-n (get-input) 14) 14)
