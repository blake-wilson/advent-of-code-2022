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

(defun get-overlap (ranges)
	(loop for (fst snd) on ranges by #'cddr
		for res = 
			(let ( 
			  (fst-start (first fst))
			  (fst-end (second fst))
			  (snd-start (first snd))
			  (snd-end (second snd))
			 )
				(if (or
						(and (<= fst-start snd-start) (>= fst-end snd-end))
						(and (<= snd-start fst-start) (>= snd-end fst-end))
					)
					(list fst snd)
					()
				)
		) when res collect res
	)
)

(length (get-overlap (get-ranges)))