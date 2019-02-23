(in-package :rosalind)

(defparameter *input-rstr* "90000 0.6
ATAGCCGA")

(defparameter *output-rstr* "0.689")


(define-problem rstr (data stream)
    *input-rstr*
    *output-rstr*
  (let* ((n (read data))
         (gc (coerce (read data) 'double-float))
         (dna (read-line data))
         (prob (product dna :key (curry #'base-probability gc))))
    (format nil "~,3F" (- 1 (expt (- 1 prob) n)))))
