(defpackage :rosalind/aspc (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/aspc)


(define-problem aspc (data stream) "6 3" "42"
  (let ((n (read data))
        (m (read data)))
    (iterate
      (for k :from m :to n)
      (u:summing* (u:binomial-coefficient n k) :modulo 1000000))))


#; Scratch --------------------------------------------------------------------
