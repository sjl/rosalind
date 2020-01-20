(defpackage :rosalind/revp (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/revp)

;; The problem explanation provided a clever trick: you can cut the comparison
;; size in half by comparing the first half of the string to the reverse
;; complement of the second half, instead of comparing the entire thing.
;;
;; AAC GTT
;;     CAA complement
;;     AAC reverse
;; AAC=AAC palindrome!

(defparameter *input*
  ">Rosalind_24
TCAATGCATGCGGGTCTATATGCAT")

(defparameter *output*
  "4 6
5 4
6 6
7 4
17 4
18 4
20 6
21 4
")


(defun reverse-palindrome-p (dna start length)
  (let ((mid (+ start (truncate length 2)))
        (end (+ start length)))
    (unless (> end (length dna))
      (string= dna
               (u:reverse-complement (subseq dna mid end))
               :start1 start
               :end1 mid))))

(defun reverse-palindrome-length (dna start)
  (iterate (for i :from 12 :downto 4 :by 2)
           (finding i :such-that (reverse-palindrome-p dna start i))))

(define-problem revp (data stream) *input* *output*
  (with-output-to-string (s)
    (iterate
      (with dna = (cdr (first (u:read-fasta-into-alist data))))
      (for i :index-of-vector dna)
      (when-let ((l (reverse-palindrome-length dna i)))
        (format s "~D ~D~%" (1+ i) l)))))


