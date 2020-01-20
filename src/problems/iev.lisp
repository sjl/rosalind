(defpackage :rosalind/iev (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/iev)

(define-problem iev (data stream)
    "1 0 0 1 0 1"
    "3.5000"
  (let* ((dd (read data))
         (dh (read data))
         (dr (read data))
         (hh (read data))
         (hr (read data))
         (rr (read data)))
    (u:float-string
      ;; It's just a weighted average…
      (* 2 (+ (* dd 1)
              (* dh 1)
              (* dr 1)
              (* hh 3/4)
              (* hr 1/2)
              (* rr 0)))
      4)))

