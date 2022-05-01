(defpackage :rosalind/rnas (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/rnas)

(defparameter *input*
  "AUGCUAGUACGGAGCGAGUCUAGCGAGCGAUGUCGUGAGUACUAUAUAUGCGCAUAAGCCACGU")

(defparameter *output*
  "284850219977421")


;;;; Problem ------------------------------------------------------------------
(defun rna-bases-pair-p (base1 base2)
  (member base1 (ecase base2
                  (#\A '(#\U))
                  (#\U '(#\A #\G))
                  (#\G '(#\C #\U))
                  (#\C '(#\G)))))

(defun rna-matches (rna &aux (cache (make-hash-table)))
  (recursively ((start 0)
                (end (length rna)))
    (if (= start end)
      1
      (alexandria:ensure-gethash (complex start end) cache
        (+ (recur (1+ start) end)
           (iterate
             ;; This is getting tedious.
             (with b1 = (char rna start))
             ;; Start further away than for MOTZ.
             (for b2 :in-string rna :from (+ start 4) :below end :with-index i)
             (when (rna-bases-pair-p b1 b2)
               (summing (* (recur (1+ start) i)
                           (recur (1+ i) end))))))))))

(define-problem rnas (data stream) *input* *output*
  (rna-matches (nth-value 1 (u:read-fasta data))))


#; Scratch --------------------------------------------------------------------


(problem-rnas)

(problem-rnas "UAGCGUGAUCAC")

(time (solve rnas))

