(in-package :rosalind)

(defparameter *input-orf*
  ">Rosalind_99
AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG")

(defparameter *output-orf*
  "M
MTPRLGLESLLE
MGMTPRLGLESLLE
MLLGSFRLIPKETLIQVAGSSPCNLS")


(defun translate-all (rna)
  "Return all possible proteins that can be translated from `rna`."
  (iterate
    (for start :first 0 :then (1+ protein-start))
    (for (values protein protein-start) = (translate rna :start start))
    (while protein)
    (collect protein)))

(define-problem orf (data stream)
    *input-orf*
    *output-orf*
  (let* ((dna (cdr (first (read-fasta-into-alist data))))
         (rna1 (transcribe dna))
         (rna2 (transcribe (reverse-complement dna))))
    (-<> (append (translate-all rna1)
                 (translate-all rna2))
      (remove-duplicates <> :test #'string=)
      (sort <> #'< :key #'length)
      (format nil "~{~A~^~%~}" <>))))


