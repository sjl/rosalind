(defpackage :rosalind/hamm (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/hamm)

(defparameter *input* "GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT")

(define-problem hamm (data stream)
    *input*
    "7"
  (u:hamming (read-line data) (read-line data) :test #'char=))
