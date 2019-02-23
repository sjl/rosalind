(in-package :rosalind)

(defparameter *input-kmer* ">Rosalind_6431
CTTCGAAAGTTTGGGCCGAGTCTTACAGTCGGTCTTGAAGCAAAGTAACGAACTCCACGG
CCCTGACTACCGAACCAGTTGTGAGTACTCAACTGGGTGAGAGTGCAGTCCCTATTGAGT
TTCCGAGACTCACCGGGATTTTCGATCCAGCCTCAGTCCAGTCTTGTGGCCAACTCACCA
AATGACGTTGGAATATCCCTGTCTAGCTCACGCAGTACTTAGTAAGAGGTCGCTGCAGCG
GGGCAAGGAGATCGGAAAATGTGCTCTATATGCGACTAAAGCTCCTAACTTACACGTAGA
CTTGCCCGTGTTAAAAACTCGGCTCACATGCTGTCTGCGGCTGGCTGTATACAGTATCTA
CCTAATACCCTTCAGTTCGCCGCACAAAAGCTGGGAGTTACCGCGGAAATCACAG")

(defparameter *output-kmer* "4 1 4 3 0 1 1 5 1 3 1 2 2 1 2 0 1 1 3 1 2 1 3 1 1 1 1 2 2 5 1 3 0 2 2 1 1 1 1 3 1 0 0 1 5 5 1 5 0 2 0 2 1 2 1 1 1 2 0 1 0 0 1 1 3 2 1 0 3 2 3 0 0 2 0 8 0 0 1 0 2 1 3 0 0 0 1 4 3 2 1 1 3 1 2 1 3 1 2 1 2 1 1 1 2 3 2 1 1 0 1 1 3 2 1 2 6 2 1 1 1 2 3 3 3 2 3 0 3 2 1 1 0 0 1 4 3 0 1 5 0 2 0 1 2 1 3 0 1 2 2 1 1 0 3 0 0 4 5 0 3 0 2 1 1 3 0 3 2 2 1 1 0 2 1 0 2 2 1 2 0 2 2 5 2 2 1 1 2 1 2 2 2 2 1 1 3 4 0 2 1 1 0 1 2 2 1 1 1 5 2 0 3 2 1 1 2 2 3 0 3 0 1 3 1 2 3 0 2 1 2 2 1 2 3 0 1 2 3 1 1 3 1 0 1 1 3 0 2 1 2 2 0 2 1 1")


(defun mapc-kmers (function n &key (copy t))
  (let ((kmer (make-array n :element-type 'character)))
    (recursively ((i 0))
      (if (= i n)
        (funcall function (if copy
                            (copy-seq kmer)
                            kmer))
        (flet ((branch (base)
                 (setf (aref kmer i) base)
                 (recur (1+ i))))
          (map nil #'branch "ACGT"))))))

(defun map-kmers (function n)
  (gathering (mapc-kmers (compose #'gather function) n)))

(defun kmers (n)
  (map-kmers #'identity n))


(define-problem kmer (data stream)
    *input-kmer*
    *output-kmer*
  (iterate
    (with n = 4)
    (with seq = (nth-value 1 (read-fasta data)))
    (with counts = (make-hash-table :test #'equal))
    (for i :from 0 :to (- (length seq) n))
    (for kmer = (subseq seq i (+ i n)))
    (incf (gethash kmer counts 0))
    (finally
      (return
        (format nil "~{~D~^ ~}"
                (map-kmers (lambda (kmer) (gethash kmer counts 0)) n))))))
