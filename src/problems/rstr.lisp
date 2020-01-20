(defpackage :rosalind/rstr (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/rstr)

(defparameter *input* "90000 0.6
ATAGCCGA")

(defparameter *output* "0.689")


(define-problem rstr (data stream) *input* *output*
  (let* ((n (read data))
         (gc (coerce (read data) 'double-float))
         (dna (read-line data))
         (prob (u:sequence-probability gc dna)))
    (u:float-string (- 1 (expt (- 1 prob) n)))))
