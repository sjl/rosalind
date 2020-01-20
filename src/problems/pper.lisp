(defpackage :rosalind/pper (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/pper)

(defparameter *input*
  "21 7")

(defparameter *output*
  "51200")


(define-problem pper (data stream) *input* *output*
  (let ((total (read data))
        (size (read data)))
    ;; The number of combinations of k out of n elements is:
    ;;
    ;;     (n choose k) = n! / k!(n-k)!
    ;;
    ;; To get the number of permutations, we multiply by the number of different
    ;; ways to order the k elements and it ends up simplifying nicely:
    ;;
    ;;     k!(n choose k) = k!n! / k!(n-k)!
    ;;                    = n! / (n-k)!
    ;;                    = (n)(n-1)(n-2)…(n-(k-1))
    ;;
    (flet ((count-permutations (size total)
             (iterate (for i :downfrom total)
                      (repeat size)
                      (multiplying i))))
      (mod (count-permutations size total)
           1000000))))
