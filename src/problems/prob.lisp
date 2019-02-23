(in-package :rosalind)

(defparameter *input-prob*
  "ACGATACAA
0.129 0.287 0.423 0.476 0.641 0.742 0.783")

(defparameter *output-prob*
  "-5.737 -5.217 -5.263 -5.360 -5.958 -6.628 -7.009")


(define-problem prob (data stream)
    *input-prob*
    *output-prob*
  (let ((dna (read-line data))
        (gc-contents (read-all-from-string (read-line data))))
    (flet
        ((prob (gc-content)
           (iterate
             (for base :in-string dna)
             (summing (log (base-probability gc-content base) 10)))))
      (format nil "~{~,3F~^ ~}" (mapcar #'prob gc-contents)))))
