(defpackage :rosalind/motz (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/motz)

(defparameter *input*
  ">Rosalind_57
AUAU")

(defparameter *output*
  "7")


;;;; Problem ------------------------------------------------------------------
(defun rna-bases-pair-p (base1 base2)
  (char= base1 (ecase base2
                 (#\A #\U)
                 (#\U #\A)
                 (#\G #\C)
                 (#\C #\G))))

(defun rna-matches (rna &aux (cache (make-hash-table)))
  (recursively ((start 0)
                (end (length rna)))
    (if (= start end)
      1
      (alexandria:ensure-gethash (complex start end) cache
        (+ (recur (1+ start) end)
           (iterate
             ;; Arbitrarily pick char 0 as one of the ends of the match to avoid
             ;; needing to cross the end of the RNA sequence when recurring.
             (with b1 = (char rna start))
             ;; Unlike CAT, for MOTZ we allow odd-length subsequences.
             (for b2 :in-string rna :from (1+ start) :below end :with-index i)
             (when (rna-bases-pair-p b1 b2)
               (summing (* (recur (1+ start) i)
                           (recur (1+ i) end))))))))))

(define-problem motz (data stream) *input* *output*
  (mod (rna-matches (nth-value 1 (u:read-fasta data))) 1000000))


#; Scratch --------------------------------------------------------------------


(problem-motz)

(problem-motz "UAGCGUGAUCAC")

(time (solve motz))

