; prevent endless loops when printing dirs
(setf *print-circle* t)

(defstruct command
  (name "" :type string)
  (arguments '() :type list)
)

(defstruct file
  (name "" :type string)
  (size 0 :type integer)
)

(defstruct dir
  (name "" :type string)
  (dirs '() :type list)
  (files '() :type list)
  (parent nil :type (or dir null))
)

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
                collect (uiop:split-string (read-line stream nil) :separator " ")into res
            else
                return (values res)
  )
)

(defun add-file (dir file)
  (setf (dir-files dir) (cons file (dir-files dir)))
)

(defun add-dir (dir to-add)
  (setf (dir-dirs dir) (acons (dir-name to-add) to-add (dir-dirs dir)))
  dir
)

(defun cd (dir dir-name)
  (let ((found (assoc dir-name (dir-dirs dir) :test #'equal)))
    (if (equal dir-name "..")
        (dir-parent dir)
        (if found
            (cdr found)
            (let ((new-dir (make-dir :name dir-name :parent dir)))
                (progn
                    (add-dir dir new-dir)
                    new-dir
                )
            )
        )
    )
  )
)


; (defun read-result (items)
;
; )

(defparameter root-dir)

(defun run-cmds (cmds)
  (let ((dir (make-dir)))
    (loop for cmd in cmds
          do (let (
                   (command (first (first cmd)))
                   (command-args (rest (first cmd)))
                   (results (second cmd))
                )
                (if (equal command "cd")
                    (setf dir (cd dir (first command-args)))
                    (if (equal command "ls")
                        (loop for file-pair in results
                              do (if (equal (first file-pair) "dir")
                                  (add-dir dir (make-dir :parent dir :name (second file-pair)))
                                  ; else is file
                                  (let ((file (make-file :size (parse-integer (first file-pair)) :name (second file-pair))))
                                     (add-file dir file)
                                  )
                              )
                        )
                    )
                )
                ; (arg (second cmd)
          )
     )
    dir
   )
)


