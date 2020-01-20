(defpackage :rosalind/long (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/long)

(defparameter *input*
  ">Rosalind_56
ATTAGACCTG
>Rosalind_57
CCTGCCGGAA
>Rosalind_58
AGACCTGCCG
>Rosalind_59
GCCGGAATAC")

(defparameter *output*
  "ATTAGACCTGCCGGAATAC")


(defun overlap (left right)
  "Return the number of overlapping characters in `left` and `right`."
  (iterate
    (for size :from (min (length left) (length right)) :downto 0)
    (for i = (- (length left) size))
    (finding size :such-that (string= left right :start1 i :end2 size))))

(defun join-overlapping (left right)
  "Join `left` and `right` together, overlapping as much as possible."
  (concatenate 'string left (subseq right (overlap left right))))


(define-problem long (data stream) *input* *output*
  (let* ((dna (mapcar #'cdr (u:read-fasta-into-alist data)))
         (graph (digraph:make-digraph :initial-vertices dna :test #'equal)))
    (dolist (left dna)
      (dolist (right dna)
        (when (and (not (equal left right))
                   (> (overlap left right) (floor (length left) 2)))
          (digraph:insert-edge graph right left)))) ; reversed edges for toposort
    (reduce #'join-overlapping (digraph:topological-sort graph))))
