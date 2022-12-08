(defun load-rucksacks ()
  (with-open-file (stream "input")
    (loop for line = (read-line stream nil)
          while line
          collect (list (subseq line 0 (/ (length line) 2)) (subseq line (/ (length line) 2) (length line)))
    )
  )
)

(defun find-matching-item (fst snd)
  (loop named outer for fst-char across fst
        do (loop for snd-char across snd
            do (if (equal fst-char snd-char)
                   (return-from outer (values fst-char))
            )
        )
  )
)

(defun get-matches ()
  (let ((rucksacks (load-rucksacks)))
    (loop for rucksack-pair in rucksacks
        collect (find-matching-item (first rucksack-pair) (second rucksack-pair))
    )
  )
)

(defun tally-matches ()
  (let ((matches (get-matches)))
   (apply '+
        (loop for match in matches
            collect (if (equal match (char-upcase match))
                        (- (char-code match) 38)
                        (- (char-code match) 96)
                    )
        )
    )
  )
)
