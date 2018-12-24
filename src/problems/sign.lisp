(in-package :rosalind)

(defparameter *input-sign* "2")

(defparameter *output-sign* "8
-1 -2
-1 2
1 -2
1 2
-2 -1
-2 1
2 -1
2 1")


(defun sign-permutations (list)
  (if (null list)
    (list '()) ;; there is exactly one permutation of the empty list: ()
    (destructuring-bind (n . more) list
      (append (mapcar (curry #'cons n) (sign-permutations more))
              (mapcar (curry #'cons (- n)) (sign-permutations more))))))

(define-problem sign (data string)
    *input-sign*
    *output-sign*
  (let* ((n (parse-integer data))
         (count (* (expt 2 n)
                   (factorial n)))
         (perms (mapcan #'sign-permutations
                        (permutations (alexandria:iota n :start 1)))))
    ;; sort to ensure consistent output for the unit test
    (format nil "~D~%~{~A~^~%~}"
            count
            (sort (mapcar (curry #'str:join " ") perms) #'string<))))

;; (problem-sign "2")
;; (solve sign)
