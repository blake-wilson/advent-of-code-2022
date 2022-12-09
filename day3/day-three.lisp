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



(defun tally-matches (vals)
   (apply '+
        (loop for val in vals
            collect (if (equal val (char-upcase val))
                        (- (char-code val) 38)
                        (- (char-code val) 96)
                    )
        )
    )
)

(tally-matches (get-matches))

; part two
(defun get-matching-chars (fst snd)
  (let ((seen ()))
  (loop for fst-char across fst
        append (loop for snd-char across snd
            if (and (equal fst-char snd-char) (not (member fst-char seen)))
                collect
		(progn
		    (setf seen (cons fst-char seen))
                    fst-char
                )
        )
    )
  )
)


(defun concat-pair (pair)
  (concatenate 'string (first pair) (second pair))
)

(defun get-badges ()
  (let ((rucksacks (load-rucksacks))
	(matches ""))
    (loop for (fst snd thd) on rucksacks by #'cdddr
	do (setf matches (get-matching-chars (concat-pair fst) (concat-pair snd)))
	  append (get-matching-chars (concatenate 'string matches) (concat-pair thd))
	)
    )
)

(tally-matches (get-badges))
