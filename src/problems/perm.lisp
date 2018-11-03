(in-package :rosalind)

;; A permutation of length n is an ordering of the positive integers {1,2,…,n}.
;; For example, π=(5,3,2,1,4) is a permutation of length 5.
;;
;; Given: A positive integer n≤7
;;
;; Return: The total number of permutations of length n, followed by a list of
;; all such permutations (in any order).

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
