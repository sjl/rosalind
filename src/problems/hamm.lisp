(defpackage :rosalind/hamm (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/hamm)

(defparameter *input* "GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT")

(defun hamming (sequence1 sequence2 &key (test #'eql))
  "Return the Hamming distance between `sequence1` and `sequence2`."
  ;; todo assert length=?
  (let ((result 0))
    (map nil (lambda (x y)
               (unless (funcall test x y)
                 (incf result)))
         sequence1
         sequence2)
    result))

(define-problem hamm (data stream)
    *input*
    "7"
  (hamming (read-line data) (read-line data) :test #'char=))
