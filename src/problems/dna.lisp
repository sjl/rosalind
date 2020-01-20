(defpackage :rosalind/dna (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/dna)

;; Nucleic acids are polymers, which means they're long, repeating chains of
;; smaller molecules called monomers.
;;
;; A single monomer of a nucleic acid is called a nucleotide (nt) and has three
;; parts:
;;
;; * A sugar molecule
;; * A negative ion called a phosphate
;; * A compound called a nucleobase (base)
;;
;; The sugar of one nucleotide binds to the phosphate of the next, forming long
;; backbones for the bases:
;;
;;    …sP-+-sP-+-sP-+-sP-+-sP…
;;        |    |    |    |
;;        C    A    T    G          s = sugar molecule
;;       G    T    A    C           p = phosphate anion
;;       |    |    |    |
;;   …sP-+-sP-+-sP-+-sP-+-sP…
;;
;; In DNA the sugar is deoxyribose, and there are four possible bases:
;;
;; * A: Adenine
;; * C: Cytosine
;; * G: Guanine
;; * T: Thymine

(define-problem dna (data string)
    "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    "20 12 17 21"
  "Count the number of each base in `data`."
  (let ((results (frequencies data)))
    (format nil "~D ~D ~D ~D"
            (gethash #\A results 0)
            (gethash #\C results 0)
            (gethash #\G results 0)
            (gethash #\T results 0))))


#; Scratch --------------------------------------------------------------------

(problem-dna "AT")
