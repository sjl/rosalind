(defpackage :rosalind/prob (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/prob)

(defparameter *input*
  "ACGATACAA
0.129 0.287 0.423 0.476 0.641 0.742 0.783")

(defparameter *output*
  "-5.737 -5.217 -5.263 -5.360 -5.958 -6.628 -7.009")


(define-problem prob (data stream) *input* *output*
  (let ((dna (read-line data))
        (gc-contents (read-all-from-string (read-line data))))
    (flet ((prob (gc-content)
             (iterate
               (for base :in-string dna)
               (summing (log (u:base-probability gc-content base) 10)))))
      (u:float-string (mapcar #'prob gc-contents)))))
