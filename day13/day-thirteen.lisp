(ql:quickload "trivia")

; returns t if fst and snd are in the correct order, nil if they are in the
; incorrect order, and :cont if further comparisons are needed
(defun cmp (fst snd)
  (trivia::match (list fst snd)
                 ; first is empty list always matches
                 ((list (eq nil) (eq nil)) :cont)
                 ((list (eq nil) _) t)

                 ; second item nil only matches other nil items
                 ((list _ (eq nil)) nil)

                 ; neither a list
                 ((list (and (type integer) a) (and (type integer) b))
                 (if (< a b)
                    t
                    (if (= a b)
                        :cont
                        nil
                    )
                  )
                 )

                 ; second a list
                 ((list (and (type integer) a) (list* b))
                    (if (= (length b) 0)
                        nil
                        (let ((cmp (cmp a (car b))))
                          (if (eq cmp :cont)
                              (cmp nil (cdr b))
                              cmp
                          )
                        )
                    )
                 )

                 ; first a list
                 ((list (list* a) (and (type integer) b))
                    (if (= (length a) 0)
                        t
                        (eq t (cmp (car a) b)))
                )

                 ; both a list
                 ((list (list* a) (list* b))
                  (let ((car-cmp (cmp (car a) (car b))))
                    (if (eq car-cmp :cont)
                        (cmp (cdr a) (cdr b))
                        car-cmp
                    )
                  ))
  )
)

(defun cmp-until (fst snd)
  (let ((comparison :cont))
    (loop for i from 0 below (length (car fst))
      while (eq :cont (setf comparison (cmp (nth i (car fst)) (nth i (car snd)))))
    )
    (not (eq nil comparison))
  )
)


(defun parse-number (str ptr)
  (values (coerce (loop for c across (subseq str ptr)
      while (digit-char-p c)
          collect (progn
                    (incf ptr 1)
                     c
              )
   )'string) ptr)
)

(defun read-list (stack ptr)
  (let ((lst nil))
    (loop
        for val = (nth ptr stack)
        while (< ptr (length stack))
        do (if (eq val #\[)
                (multiple-value-bind (new-lst new-ptr) (read-list stack (+ ptr 1))
                  (setf ptr new-ptr)
                  (setf lst (append lst (list new-lst)))
                )
            (if (eq val #\])
                 (progn
                    (incf ptr 1)
                    (return lst)
                 )
                 (progn
                      (incf ptr 1)
                      (setf lst (append lst (list val)))
                  )
            )
        )
    )
    (values lst ptr)
  )
)

(defun parse-packet (str)
  (let ((tokens '())
        (comma-delimited (uiop:split-string str :separator ",")
    ))
    (loop for i from 0 below (length comma-delimited)
          for val = (nth i comma-delimited)
        do (
            let ((ptr 0))
            (loop
               while (< ptr (length val))
               do (if (digit-char-p (char val ptr))
                   (multiple-value-bind (num new-ptr) (parse-number val ptr)
                     (setf ptr new-ptr)
                     (setf tokens (append tokens (list (parse-integer num))))
                   )
                ; otherwise is bracket char
                (progn
                  (setf tokens (append tokens (list (char val ptr))))
                  (incf ptr 1)
                )
              )
            )
        )
    )
    (read-list tokens 0)
  )
)

(defun read-input ()
  (with-open-file (stream "input")
    (loop for cmp1 = (read-line stream nil)
          while cmp1
          collect (
               let ((cmp (cmp-until (parse-packet cmp1)
                    (parse-packet (read-line stream nil)))
                    ))
                    (read-line stream nil)
                    cmp
            )
    )
  )
)

; func for part 1
(defun get-right-order-index-sum ()
  (let* ((cmps (read-input))
         (idx 1)
         (num-in-order 0)
         (idx-sum 0))
        (mapcar (lambda (in-order)
                (if in-order
                  (progn
                    (incf idx-sum idx)
                    (incf num-in-order)
                  )
                ) (incf idx)) cmps)
    idx-sum
  )
)

; part 1 solution
(get-right-order-index-sum)

; part 2 stuff
(defun packet-sort-func (packet1 packet2)
  (cmp packet1 packet2)
)

(defun read-packet-from-stream (stream)
  (let ((line (read-line stream nil)))
    (if (> (length line) 0)
        (parse-packet line)
    )
  )
)

(defun read-all-packets ()
  (with-open-file (stream "input")
    (loop
        while (peek-char nil stream nil)
        append (read-packet-from-stream stream)
    )
  )
)

; part 2 solution
(let ((sorted
       (sort (append (read-all-packets)
                      (parse-packet "[[2]]")
                      (parse-packet "[[6]]"))
              'packet-sort-func)
  ))
  (apply '* (loop for i from 0 below (length sorted)
        for val = (nth i sorted)
        append (if (or (equal '((2)) val) (equal '((6)) val))
                    (list (+ i 1))
                )
  ))
)
