(defun get-elf-cals ()
    (with-open-file (stream "dayonedata")
      (let ((totals ()) (eof nil))
        (loop
          while (not eof)
          do (setf totals (cons (loop for line = (read-line stream nil)
              do (if (not line) (setq eof t))
              while (not (member line '(nil "" ) :test #'string=))
              collect (parse-integer line)
          ) totals))
        )
        totals
      )
    )
)

;; sum up all the lists to get total per elf
(defvar elf-cals (mapcar (lambda (lst) (reduce '+ lst)) (get-elf-cals)))

;; get max of all elves
(apply 'max elf-cals)

;; get first 3 highest values from elves and sum them together
(apply '+ (subseq (sort (copy-seq elf-cals) #'>) 0 3))
