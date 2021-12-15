(defpackage :rosalind/corr (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/corr)

(defparameter *input*
  ">Rosalind_52
TCATC
>Rosalind_44
TTCAT
>Rosalind_68
TCATC
>Rosalind_28
TGAAA
>Rosalind_95
GAGGA
>Rosalind_66
TTTCA
>Rosalind_33
ATCAA
>Rosalind_21
TTGAT
>Rosalind_18
TTTCC
")

(defparameter *output*
  "GAGGA->GATGA
TTCAT->TTGAT
TTTCC->TTTCA
")


;;;; Problem ------------------------------------------------------------------
(defun read-frequencies (reads)
  (iterate
    (with result = (make-hash-table :test #'equal))
    (for read :in reads)
    (incf (gethash read result 0))
    (for read-rc = (u:reverse-complement read))
    (unless (string= read read-rc)
      (incf (gethash read-rc result 0)))
    (returning (remhash-if-value (curry #'= 1) result))))

(defun find-correction (read freqs)
  (iterate (for (good nil) :in-hashtable freqs)
           (finding good :such-that (= 1 (u:hamming good read)))))

(define-problem corr (data stream) *input* *output*
  (iterate
    (with reads = (mapcar #'cdr (u:read-fasta-into-alist data)))
    (with freqs = (read-frequencies reads))
    (for r :in reads)
    (unless (gethash r freqs)
      (collect (format nil "~A->~A" r (find-correction r freqs)) :into result))
    (returning (str:join #\newline (sort result #'string<))))) ; sort for test


#; Scratch --------------------------------------------------------------------

(problem-corr)

(time (solve corr))
