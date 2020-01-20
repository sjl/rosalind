(defpackage :rosalind/pdst (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/pdst)

(defparameter *input* ">Rosalind_9499
TTTCCATTTA
>Rosalind_0942
GATTCATTTC
>Rosalind_6568
TTTCCATTTT
>Rosalind_1833
GTTCCATTTA")

(defparameter *output* "0.00000 0.40000 0.10000 0.10000
0.40000 0.00000 0.40000 0.30000
0.10000 0.40000 0.00000 0.20000
0.10000 0.30000 0.20000 0.00000")

(defun p-distance (a b)
  (assert (= (length a) (length b)))
  (/ (u:hamming a b) (length a)))

(defun make-p-distance-matrix (seqs)
  (let* ((n (length seqs))
         (matrix (make-array (list n n) :initial-element 0)))
    (iterate
      (for row :from 0 :below n)
      (for start :from 1) ;; Don't bother comparing seqs with themselves.
      (for (a . remaining) :on seqs)
      (iterate
        (for b :in remaining)
        (for col :from start :below n)
        (for pd = (p-distance a b))
        (setf (aref matrix row col) pd ;; The matrix is symmetric along the diagonal.
              (aref matrix col row) pd)))
    matrix))

(define-problem pdst (data stream) *input* *output*
  (_ data
    u:read-fasta-into-alist
    (mapcar #'cdr _)
    make-p-distance-matrix
    (u:float-string _ 5)))
