(defpackage :rosalind/splc (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/splc)

(defparameter *input* ">Rosalind_10
ATGGTCTACATAGCTGACAAACAGCACGTAGCAATCGGTCGAATCTCGAGAGGCATATGGTCACATGATCGGTCGAGCGTGTTTCAAAGTTTGCGCCTAG
>Rosalind_12
ATCGGTCGAA
>Rosalind_15
ATCGGTCGAGCGTGT")

(defparameter *output* "MVYIADKQHVASREAYGHMFKVCA")


(defun prefixp (prefix vector &key (start 0) (test #'eql))
  (and (<= (length prefix)
           (+ start (length vector)))
       (iterate
         (for x :in-vector prefix)
         (for y :in-vector vector :from start)
         (always (funcall test x y)))))

(defun string=* (string1 string2 &key (start1 0) end1 (start2 0) end2)
  (let ((l1 (length string1))
        (l2 (length string2)))
    (string= string1 string2
             :start1 (min (max 0 start1) l1)
             :start2 (min (max 0 start2) l2)
             :end1 (min (max 0 end1) l1)
             :end2 (min (max 0 end2) l2))))

(defun intron-matches-p (intron dna &key (start 0))
  (prefixp intron dna :start start))

(defun find-intron (introns dna &key (start 0))
  (find-if (u:rcurry #'intron-matches-p dna :start start) introns))

(defun remove-introns (dna introns)
  (iterate
    (for base :in-vector dna :with-index i)
    (if-let ((intron (find-intron introns dna :start i)))
      (incf i (1- (length intron)))
      (collect base :result-type string))))



(define-problem splc (data stream)
    *input*
    *output*
  (destructuring-bind (dna . introns)
      (mapcar #'cdr (u:read-fasta-into-alist data))
    (_ dna
      (remove-introns _ introns)
      u:transcribe
      u:translate)))
