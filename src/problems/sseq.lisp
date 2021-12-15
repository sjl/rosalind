(defpackage :rosalind/sseq (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/sseq)

(defparameter *input* ">Rosalind_14
ACGTACGTGACG
>Rosalind_18
GTA
")

(defparameter *output* "3 4 5")

;; todo: make this more efficient for lists
(defun subsequence-positions (needle haystack &key
                              (test #'eql)
                              (start-needle 0)
                              (end-needle (length needle))
                              (start-haystack 0)
                              (end-haystack (length haystack)))
  (iterate
    (with ni = start-needle)
    (with n = (elt needle ni))
    (for h :in-vector haystack :with-index hi :from start-haystack :below end-haystack)
    (when (funcall test n h)
      (collect hi :into result)
      (incf ni)
      (if (= ni end-needle)
        (return result)
        (setf n (elt needle ni))))))

(define-problem sseq (data stream) *input* *output*
  (let* ((haystack (nth-value 1 (u:read-fasta data)))
         (needle (nth-value 1 (u:read-fasta data))))
    (_ (subsequence-positions needle haystack :test #'char=)
      (mapcar #'1+ _)
      (format nil "~{~D~^ ~}" _))))
