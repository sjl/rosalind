(defpackage :rosalind/subs (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/subs)

(defparameter *input* "GATATATGCATATACTT
ATAT")

(defparameter *output* "2 4 10")


(define-problem subs (data stream) *input* *output*
  (let ((haystack (read-line data))
        (needle (read-line data)))
    (iterate
      (for pos :seed -1 :then (search needle haystack :start2 (1+ pos)))
      (while pos)
      (collect (1+ pos) :into result)
      (finally (return (u:strjoin " " result))))))

