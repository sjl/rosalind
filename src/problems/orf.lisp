(defpackage :rosalind/orf (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/orf)

(defparameter *input*
  ">Rosalind_99
AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG")

(defparameter *output*
  "M
MTPRLGLESLLE
MGMTPRLGLESLLE
MLLGSFRLIPKETLIQVAGSSPCNLS")


(defun translate-all (rna)
  "Return all possible proteins that can be translated from `rna`."
  (iterate
    (for start :first 0 :then (1+ protein-start))
    (for (values protein protein-start) = (u:translate rna :start start))
    (while protein)
    (collect protein)))

(define-problem orf (data stream) *input* *output*
  (let* ((dna (cdr (first (u:read-fasta-into-alist data))))
         (rna1 (u:transcribe dna))
         (rna2 (u:transcribe (u:reverse-complement dna))))
    (-<> (append (translate-all rna1)
                 (translate-all rna2))
      (remove-duplicates <> :test #'string=)
      (sort <> #'< :key #'length)
      (format nil "~{~A~^~%~}" <>))))


