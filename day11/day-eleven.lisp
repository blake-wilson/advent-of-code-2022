(defstruct monkey
  (items '() :type list)
  (test (lambda (worry-level) worry-level)) ; should return t or nil
  (test-success-monkey nil :type integer) ; used when test returns t
  (test-fail-monkey nil :type integer) ; used when test returns nil
  (operation (lambda (worry-level) worry-level))
  (num-items-inspected 0 :type integer)
)

(defun add-item-to-monkey (monkey item)
  (setf (monkey-items monkey) (cons item (monkey-items monkey)))
)

(defun init-monkeys()
    (list
      (make-monkey
        :items '(80)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 2)) t))
        :test-success-monkey 4
        :test-fail-monkey 3
        :operation (lambda (worry-level) (* worry-level 5))
      )
      (make-monkey
        :items '(75 83 74)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 7)) t))
        :test-success-monkey 5
        :test-fail-monkey 6
        :operation (lambda (worry-level) (+ worry-level 7))
      )
      (make-monkey
        :items '(86 67 61 96 52 63 73)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 3)) t))
        :test-success-monkey 7
        :test-fail-monkey 0
        :operation (lambda (worry-level) (+ worry-level 5))
      )
      (make-monkey
        :items '(85 83 55 85 57 70 85 52)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 17)) t))
        :test-success-monkey 1
        :test-fail-monkey 5
        :operation (lambda (worry-level) (+ worry-level 8))
      )
      (make-monkey
        :items '(67 75 91 72 89)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 11)) t))
        :test-success-monkey 3
        :test-fail-monkey 1
        :operation (lambda (worry-level) (+ worry-level 4))
      )
      (make-monkey
        :items '(66 64 68 92 68 77)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 19)) t))
        :test-success-monkey 6
        :test-fail-monkey 2
        :operation (lambda (worry-level) (* worry-level 2))
      )
      (make-monkey
        :items '(97 94 79 88)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 5)) t))
        :test-success-monkey 2
        :test-fail-monkey 7
        :operation (lambda (worry-level) (* worry-level worry-level))
      )
      (make-monkey
        :items '(77 85)
        :test (lambda (worry-level) (if (eq 0 (mod worry-level 13)) t))
        :test-success-monkey 4
        :test-fail-monkey 0
        :operation (lambda (worry-level) (+ worry-level 6))
      )
    )
)

(defun do-round (monkeys)
  (mapcar (lambda (monkey) (do-monkey-round monkey monkeys)) monkeys)
)

(defun delete-nth (sequence n)
    (let ((g (gensym)))
        (setf (elt sequence n) g)
        (delete g sequence)
    )
)

(defun do-monkey-round (monkey all-monkeys)
  (let ((dst-monkey-item-success (nth (monkey-test-success-monkey monkey) all-monkeys))
        (dst-monkey-item-fail (nth (monkey-test-fail-monkey monkey) all-monkeys)))
    (loop for item in (monkey-items monkey)
          do (progn
               (setf item (funcall (monkey-operation monkey) item))
               (setf item (floor item 3)) ; monkey gets bored with item
               (incf (monkey-num-items-inspected monkey) 1)
               (if (funcall (monkey-test monkey) item)
                        (add-item-to-monkey (nth (monkey-test-success-monkey monkey) monkeys) item)
                        (add-item-to-monkey (nth (monkey-test-fail-monkey monkey) monkeys) item)
                   )
          )
     )
    (setf (monkey-items monkey) '())
  )
)

(defparameter monkeys (init-monkeys))

(loop for i from 0 below 20 do (do-round monkeys))

(sort monkeys (lambda (m1 m2) (> (monkey-num-items-inspected m1) (monkey-num-items-inspected m2))))

; part one sol.
(apply '* (mapcar 'monkey-num-items-inspected (subseq monkeys 0 2)))
