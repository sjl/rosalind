(defpackage :rosalind/grph (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/grph)

(defparameter *input*
  ">Rosalind_0498
AAATAAA
>Rosalind_2391
AAATTTT
>Rosalind_2323
TTTTCCC
>Rosalind_0442
AAATCCC
>Rosalind_5013
GGGTGGG")

(defparameter *output*
  "Rosalind_0498 Rosalind_0442
Rosalind_0498 Rosalind_2391
Rosalind_2391 Rosalind_2323
")


(defun strings-overlap-p (k left right)
  "Return whether `left` and `right` overlap (in order) by exactly `k` characters.

    (strings-overlap-p 3 \"abcdef\"
                            \"defhi\") ; => T

    (strings-overlap-p 2 \"abcdef\"
                             \"defhi\") ; => NIL

  "
  (string= left right
           :start1 (- (length left) k)
           :end2 k))


(define-problem grph (data stream) *input* *output*
  (let* ((data (u:read-fasta-into-hash-table data))
         (graph (digraph:make-digraph
                  :test #'equal
                  :initial-vertices (alexandria:hash-table-keys data))))
    (maphash (lambda (lk lv)
               (maphash (lambda (rk rv)
                          (unless (string= lk rk)
                            (when (strings-overlap-p 3 lv rv)
                              (digraph:insert-edge graph lk rk))))
                        data))
             data)
    ;; (ql:quickload :cl-digraph.dot)
    ;; (digraph.dot:draw graph)
    (_ (iterate (for (l . r) :in (digraph:edges graph))
                  (collect (format nil "~A ~A~%" l r)))
      (sort _ #'string<)
      (str:join "" _))))



