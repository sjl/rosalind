(in-package :rosalind)

(defparameter *input-perm* "3")

(defparameter *output-perm* "6
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1")


(define-problem perm (data string)
    *input-perm*
    *output-perm*
  (let* ((n (parse-integer data))
         (count (factorial n))
         (perms (permutations (alexandria:iota n :start 1))))
    (format nil "~D~%~{~A~^~%~}"
            count
            ;; sort to ensure consistent output for the unit test
            (sort (mapcar (curry #'str:join " ") perms) #'string<))))

;; (problem-perm "3")
;; (solve perm)
