(defpackage :rosalind/seto (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/seto)

(defparameter *input*
  "10
{1, 2, 3, 4, 5}
{2, 8, 5, 10}")

(defparameter *output*
  "{1, 2, 3, 4, 5, 8, 10}
{2, 5}
{1, 3, 4}
{8, 10}
{6, 7, 8, 9, 10}
{1, 3, 4, 6, 7, 9}")

(defun set-string (set)
  ;; Sort for consistent unit test output.
  (format nil "{~{~D~^, ~}}" (sort (copy-seq set) #'<)))

(defun parse-set (string)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" string)))

(define-problem seto (data stream)
    *input*
    *output*
  (let ((u (alexandria:iota (read data) :start 1))
        (a (parse-set (read-line data)))
        (b (parse-set (read-line data))))
    (_ (list (union a b) ; to hell with it, we'll just use CL's built-in stuff
             (intersection a b)
             (set-difference a b)
             (set-difference b a)
             (set-difference u a)
             (set-difference u b))
      (mapcar #'set-string _)
      (str:join (string #\newline) _))))


#; Scratch --------------------------------------------------------------------
