(in-package :rosalind)

(defparameter *input-hamm* "GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT")

(define-problem hamm (data stream)
    *input-hamm*
    "7"
  (hamming (read-line data) (read-line data) :test #'char=))

;; (problem-hamm *input-hamm*)
;; (solve hamm)
