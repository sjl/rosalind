(defpackage :rosalind/sign (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/sign)

(defparameter *input* "2")

(defparameter *output* "8
-1 -2
-1 2
-2 -1
-2 1
1 -2
1 2
2 -1
2 1")


(defun sign-permutations (list)
  (if (null list)
    (list '()) ;; there is exactly one permutation of the empty list: ()
    (destructuring-bind (n . more) list
      (append (mapcar (u:curry #'cons n) (sign-permutations more))
              (mapcar (u:curry #'cons (- n)) (sign-permutations more))))))

(define-problem sign (data string) *input* *output*
  (let* ((n (parse-integer data))
         (count (* (expt 2 n)
                   (u:factorial n)))
         (perms (mapcan #'sign-permutations
                        (u:permutations (alexandria:iota n :start 1)))))
    ;; sort to ensure consistent output for the unit test
    (format nil "~D~%~{~A~^~%~}"
            count
            (sort (mapcar (u:curry #'str:join " ") perms) #'string<))))
