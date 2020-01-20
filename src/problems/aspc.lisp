(defpackage :rosalind/aspc (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/aspc)


(define-problem aspc (data stream) "6 3" "42"
  (let ((n (read data))
        (m (read data)))
    (u:Σ (k m n :modulo 1000000)
      (u:binomial-coefficient n k))))


#; Scratch --------------------------------------------------------------------
