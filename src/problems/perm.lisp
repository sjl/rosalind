(defpackage :rosalind/perm (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/perm)

(defparameter *input* "3")

(defparameter *output* "6
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1")


(define-problem perm (data string) *input* *output*
  (let* ((n (parse-integer data))
         (count (u:factorial n))
         (perms (u:permutations (alexandria:iota n :start 1))))
    (format nil "~D~%~{~A~^~%~}"
            count
            ;; sort to ensure consistent output for the unit test
            (sort (mapcar (u:curry #'u:strjoin " ") perms) #'string<))))
