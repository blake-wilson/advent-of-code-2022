(defun get-score (opp self)
  (+
      (cdr (assoc self '(("X" . 1) ("Y" . 2) ("Z" . 3)) :test #'string=))
      (wld opp self)
  )
)

(defun wld (opp self)
  (if (member (list opp self) '(("A" "Z") ("B" "X") ("C" "Y")) :test #'equal) ; loss conds
      0
      (if (member (list opp self) '(("A" "Y") ("B" "Z") ("C" "X")) :test #'equal) ; win conds
          6
          3 ; draw conds
      )
  )
)


(defun read-scores ()
  (with-open-file (stream "guide.txt")
    (loop for line = (read-line stream nil)
          while line
          collect (uiop:split-string line :separator " ")
    )
  )
)

(defun tally-scores ()
  (let ((scores (read-scores)))
    (apply '+
      (mapcar (lambda (vals)
                (get-score (first vals) (second vals))) scores)
    )
  )
)

;; part two
(defun get-play (opp result)
  (if (equal result "X")
      (cdr (assoc opp '(("A" . "Z") ("B" . "X") ("C" . "Y")) :test #'string=))
      (if (equal result "Y")
          (cdr (assoc opp '(("A" . "X") ("B" . "Y") ("C" . "Z")) :test #'string=))
          (cdr (assoc opp '(("A" . "Y") ("B" . "Z") ("C" . "X")) :test #'string=))
      )
  )
)

(defun tally-scores-snd ()
  (let ((scores (read-scores)))
    (apply '+
      (mapcar (lambda (vals)
                (get-score (first vals) (get-play (first vals) (second vals)))) scores
      )
    )
  )
)
