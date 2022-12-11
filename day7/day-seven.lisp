(defun get-input ()
  (uiop:read-file-lines "input")
)

(defun read-lines ()
  (with-open-file (stream "input")
      (loop for line = (read-line stream nil)
        while line
        if (equal (char line 0) #\$)
            collect (list (uiop:split-string (subseq line 2) :separator " ") (read-command-output stream)) into vals
        if (not (peek-char nil stream nil))
            return (values vals)
       )
  )
)

(defun read-command-output (stream)
  (loop for char = (peek-char nil stream nil)
            if (and char (char/= char #\$))
                collect (read-line stream nil) into res
            else
                return (values res)
  )
)

(defun run-cmds (cmds)
  (loop for cmd in cmds
        (let ((command (first cmd))
              (arg (second cmd)
            )
         )
  )
  (if (equa

)
