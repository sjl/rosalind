(in-package :rosalind)

;; A string is simply an ordered collection of symbols selected from some
;; alphabet and formed into a word; the length of a string is the number of
;; symbols that it contains.
;;
;; An example of a length 21 DNA string (whose alphabet contains the symbols
;; 'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."
;;
;; Given: A DNA string s of length at most 1000 nt.  Return: Four integers
;; (separated by spaces) counting the respective number of times that the
;; symbols 'A', 'C', 'G', and 'T' occur in s.

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
