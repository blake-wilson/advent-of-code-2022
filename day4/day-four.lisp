(defun get-ranges ()
	(with-open-file (stream "input")
		(loop for line = (read-line stream nil)
			while line
			append (progn
				(let* ((members (uiop:split-string line :separator ","))
						(fst (uiop:split-string (first members) :separator "-"))
						(snd (uiop:split-string (second members) :separator "-"))
					)
					(list 
						(list (parse-integer (first fst)) (parse-integer (second fst)))
						(list (parse-integer (first snd)) (parse-integer (second snd)))
					)
				)
			)
		)
	)
)

(defun cmp-pt-one (fst-start fst-end snd-start snd-end)
	(or
		(and (<= fst-start snd-start) (>= fst-end snd-end))
		(and (<= snd-start fst-start) (>= snd-end fst-end))
	)
)

(defun get-overlap (ranges cmp)
	(loop for (fst snd) on ranges by #'cddr
		for res = 
			(let ( 
			  (fst-start (first fst))
			  (fst-end (second fst))
			  (snd-start (first snd))
			  (snd-end (second snd))
			 )
				(if (funcall cmp fst-start fst-end snd-start snd-end)
					(list fst snd)
					()
				)
		) when res collect res
	)
)

(length (get-overlap (get-ranges) #'cmp-pt-one))

; part 2

(defun cmp-pt-two (fst-start fst-end snd-start snd-end)
	(not
		(or (> fst-start snd-end) (< fst-end snd-start))
	)
)

(length (get-overlap (get-ranges) #'cmp-pt-two))