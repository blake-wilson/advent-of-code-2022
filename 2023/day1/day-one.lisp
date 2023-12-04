(defun get-calib-sum ()
    (with-open-file (stream "input")
      (let ((sum 0) (first-digit nil) (last-digit nil))
         (loop for line = (read-line stream nil)
                   while line
                   do (progn
                       (setf first-digit nil)
                       (loop for idx from 0 to (length line)
                             while (< idx (length line))
                       do
                        (print (subseq line idx))
                           (multiple-value-bind (digit len)
                             (digit-from-text (subseq line idx))
                                (if digit (progn
                                        (if (not first-digit)
                                            (setf first-digit digit)
                                        )
                                        (setf last-digit digit)
                                    )
                                )
                       )
                    )
                   (incf sum (parse-integer (format nil "~d~d~C" first-digit last-digit #\linefeed)))
                   (setf first-digit nil)
                 )
    )
    sum
)))

(defun digit-from-text (text)
  (let ((numeric-digit (digit-char-p (char text 0))))
    (if numeric-digit
        (values numeric-digit 1)
        (let ((word (digit-from-word text)))
          (values (cdr (assoc word wordtonum :test #'string=)) (length word))
        )
    )
  ))

(defparameter wordtonum '(("one" . 1)
                           ("two" . 2)
                           ("three" . 3)
                           ("four" . 4)
                           ("five" . 5)
                           ("six" . 6)
                           ("seven" . 7)
                           ("eight" . 8)
                           ("nine" . 9)
                           ))

; return the digit from the start of the string, if it exists
(defun digit-from-word (text)
    (reduce #'(lambda (acc elem)
              (if acc acc (if (and
                                (>= (length text) (length elem))
                                (string=
                                    elem (subseq text 0 (length elem)))
                              )
                              elem
                              )))
            '(nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine")))
