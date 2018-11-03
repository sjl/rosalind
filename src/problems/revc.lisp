(in-package :rosalind)

(define-problem revc (data string)
    "AAAACCCGGT"
    "ACCGGGTTTT"
  (flet ((dna-complement (base)
           (case base
             (#\A #\T)
             (#\T #\A)
             (#\G #\C)
             (#\C #\G)
             (t base)))) ; newline etc
    (map-into data #'dna-complement data)
    (nreverse data)))

