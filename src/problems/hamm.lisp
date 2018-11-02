(in-package :rosalind)

;; Given two strings s and t of equal length, the Hamming distance between s and
;; t, denoted dH(s,t), is the number of corresponding symbols that differ in
;; s and t.
;;
;; Given: Two DNA strings s and t of equal length (not exceeding 1 kbp).
;;
;; Return: The Hamming distance dH(s,t)

(defparameter *input-hamm* "GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT")

(define-problem hamm (data stream)
    *input-hamm*
    "7"
  (hamming (read-line data) (read-line data) :test #'char=))

;; (problem-hamm *input-hamm*)
;; (solve hamm)
