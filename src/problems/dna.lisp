(in-package :rosalind)

(define-problem dna (data string)
    "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    "20 12 17 21"
  (let ((results (frequencies data)))
    (format nil "~D ~D ~D ~D"
            (gethash #\A results 0)
            (gethash #\C results 0)
            (gethash #\G results 0)
            (gethash #\T results 0))))

;; (problem-dna "AT")
