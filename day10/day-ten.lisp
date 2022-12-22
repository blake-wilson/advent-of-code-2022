(defparameter clock 0)
(defparameter register 1)

; clock-lambdas: list of lambdas to be called with the current
; clock cycle as their only parameter
(defparameter clock-lambdas '())

(defun sprite-val (cc reg-val)
  (let ((col (mod cc 40)))
    (if (<= (abs (- reg-val col)) 1) #\# #\.)
  )
)

(defun register-clock-lambda (f)
  (setf clock-lambdas (cons f clock-lambdas))
)

(defparameter screen (make-array 240 :initial-element #\space))
(defun print-symbol (cc)
    (setf (row-major-aref screen cc) (sprite-val cc register))
)

(defun inc-clock ()
  (print-symbol clock)
  (incf clock 1)
  (dolist (f clock-lambdas) (funcall f clock))
)

(defun exec-instr (instr &rest args)
  (if (equal instr "addx")
      (progn
        (inc-clock)
        (inc-clock)
        (setf register (+ register (parse-integer (car (first args)))))
      )
      (if (equal instr "noop")
         (inc-clock))
  )
)

(defun get-input ()
  (with-open-file (stream "input")
    (loop for line = (read-line stream nil)
          while line
          collect (uiop:split-string line :separator " ")
    )
  )
)

(defun run-input ()
  (mapcar (lambda (vals) (exec-instr (first vals) (rest vals))) (get-input))
)

(defparameter interesting-sig-strengths '())

(register-clock-lambda (lambda (cc) (
      if (member cc '(20 60 100 140 180 220))
      (progn
      (setf interesting-sig-strengths
            (cons (* register cc) interesting-sig-strengths)))))
)

; part 1
(run-input)
(apply '+ interesting-sig-strengths)

; part 2
; screen 40 wide 6 tall
(defun format-screen (screen col-width)
  (loop for i from 0 below (/ (length screen) col-width)
    do (format t "~{~A~}~%" (loop for j from 0 below col-width
        collect (if (equal (aref screen (+ (* i col-width) j)) #\#) #\# #\space)
    ))
  )
)

(format-screen screen 40)
