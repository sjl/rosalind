(defpackage :rosalind/tran (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/tran)

(defparameter *input* ">Rosalind_0209
GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGA
AGTACGGGCATCAACCCAGTT
>Rosalind_2200
TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGC
GGTACGAGTGTTCCTTTGGGT")

(defparameter *output* "1.21428571429")

(defun rings (base)
  "Return the number of rings in the structure of `base`.

  Pyrimidines (cytosine, thymine, and uracil) have a single-ring structure.

  Purines (adenine and guanine) have a double-ring structure.

  "
  (ecase base
    ((#\A #\G) 2)
    ((#\C #\T #\U) 1)))

(defun transitionp (x y)
  (and (char/= x y)
       (= (rings x) (rings y))))

(defun transversionp (x y)
  (and (char/= x y)
       (/= (rings x) (rings y))))

(define-problem tran (data stream) *input* *output*
  (destructuring-bind (x y)
      (mapcar #'cdr (u:read-fasta-into-alist data))
    (format nil "~,11F" (coerce (/ (u:mapcount #'transitionp x y)
                                   (u:mapcount #'transversionp x y))
                                'double-float))))
