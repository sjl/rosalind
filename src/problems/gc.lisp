(in-package :rosalind)

(defparameter *input-gc* ">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT")
(defparameter *output-gc* "Rosalind_0808
60.919540")


(define-problem gc (data stream)
    *input-gc*
    *output-gc*
  (labels ((gcp (base)
             (or (char= #\G base)
                 (char= #\C base)))
           (gc-content (string)
             (/ (count-if #'gcp string)
                (length string))))
    (iterate
      (for (label dna) :in-fasta data)
      (for gc = (gc-content dna))
      (finding (format nil "~A~%~,6F" label (* 100 gc))
               :maximizing gc))))


;; (problem-gc *input-gc*)
;; (solve gc)
