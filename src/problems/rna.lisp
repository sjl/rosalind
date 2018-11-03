(in-package :rosalind)

(define-problem rna (data string)
    "GATGGAACTTGACTACGTAAATT"
    "GAUGGAACUUGACUACGUAAAUU"
  (substitute #\U #\T data))

