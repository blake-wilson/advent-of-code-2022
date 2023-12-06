(defun read-scratchcards ()
  (with-open-file (stream "input")
    (loop for line = (read-line stream nil)
          while line
          collect (read-card line)
    )
  )
)

(defun num-wins (card)
  (let ((num-wins 0))
    (loop for w in (card-winning-numbers card)
        do (loop for s in (card-self-numbers card)
                 do (if (= s w) (incf num-wins))
        )
    )
    num-wins
  )
)

(defun score-wins (win-count)
  (let ((score 0))
    (loop for i from 0 to (- win-count 1)
          do (setf score (expt 2 i))
    )
    score
  )
)

(defstruct card
    num
    winning-numbers
    self-numbers
  )

(defun read-card (line)
  (let ((card (make-card)))
    (setf line (expect-str line "Card"))
    (let ((read-res (read-num (string-trim '(#\Space) line))))
        (setf line (first read-res))
        (setf (card-num card) (second read-res))
    )
    ; consume the #\:
    (setf line (consume-char line))
    (loop for result = (read-num line)
            then (read-num (first result))
            while (second result)
            do
            (progn
                (setf (card-winning-numbers card)
                      (append (card-winning-numbers card) (list (second result)))
                )
                (setf line (first result))
            )
    )
    (setf line (consume-char line))
    (setf line (consume-char line))
    (loop for result = (read-num line)
            then (read-num (first result))
            while (second result)
            do
            (progn
            (setf (card-self-numbers card)
                  (append (card-self-numbers card) (list (second result)))
            ))
    )
    card
  )
)

(defun consume-char (line)
   (subseq line 1)
  )

(defun expect-str (line expected)
  (if (string= (subseq line 0 (length expected)) expected)
      (subseq line (length expected))
  ))

(defun read-num (line)
   (let* ((trimmed-line (string-trim '(#\Space) line))
           (num-chars (loop for c across trimmed-line
                         while (digit-char-p c )
                         collect c)))
     (if (> (length num-chars) 0)
        (progn  (list (subseq trimmed-line (length num-chars)) (parse-integer (coerce num-chars 'string)))
         )
        (list trimmed-line nil)
     )
  )
)

; part 1
(let ((total 0))
  (loop for card in (read-scratchcards)
    do (incf total (score-wins (num-wins card)))
  )
  (format t "total is ~d~C" total #\Newline)
  total
)

(defun read-scratchcards-2 ()
  (let ((cards '())
        (count-map (make-hash-table)))
  (with-open-file (stream "input")
    (loop for line = (read-line stream nil)
          and i = 1
          then (incf i 1)
          while line
          do (let* ((card (read-card line))
                (numcards (gethash i count-map 1))
                (win-count (num-wins card)))
                (setf (gethash i count-map) numcards)
                (dotimes (n win-count) (incf (gethash (+ i (+ 1 n)) count-map 1) numcards))
             )
    )
    )
    (loop for value being the hash-values of count-map
          sum value
    )
  )
)
