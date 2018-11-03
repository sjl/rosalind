(in-package :rosalind)

(defparameter *input-grph* ">Rosalind_0498
AAATAAA
>Rosalind_2391
AAATTTT
>Rosalind_2323
TTTTCCC
>Rosalind_0442
AAATCCC
>Rosalind_5013
GGGTGGG")

(defparameter *output-grph* "Rosalind_0498 Rosalind_2391
Rosalind_0498 Rosalind_0442
Rosalind_2391 Rosalind_2323
")


(define-problem grph (data stream)
    *input-grph*
    *output-grph*
  (let* ((data (read-fasta-into-hash-table data))
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
    (with-output-to-string (s)
      (iterate (for (l . r) :in (digraph:edges graph))
               (format s "~A ~A~%" l r)))))



