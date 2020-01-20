(defpackage :rosalind/prtm (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/prtm)

(defconstant +monoisotopic-mass-of-water+ 18.01056d0
  "The monoisotopic mass of a single water molecule, in Daltons.")

(defun monoisotopic-mass (residue)
  ;; todo is a hash table faster here?  we could also do an array
  ;; starting at (char-code #\A) if we really wanted
  (ecase residue
    ;; These have to be doubles or we get too much rounding error.  It's fine.
    (#\A  71.03711d0)
    (#\C 103.00919d0)
    (#\D 115.02694d0)
    (#\E 129.04259d0)
    (#\F 147.06841d0)
    (#\G  57.02146d0)
    (#\H 137.05891d0)
    (#\I 113.08406d0)
    (#\K 128.09496d0)
    (#\L 113.08406d0)
    (#\M 131.04049d0)
    (#\N 114.04293d0)
    (#\P  97.05276d0)
    (#\Q 128.05858d0)
    (#\R 156.10111d0)
    (#\S  87.03203d0)
    (#\T 101.04768d0)
    (#\V  99.06841d0)
    (#\W 186.07931d0)
    (#\Y 163.06333d0)))


(define-problem prtm (data string)
    "SKADYEK"
    "821.392"
  (_ data
    (delete #\newline _)
    (summation _ :key #'monoisotopic-mass)
    u:float-string))


