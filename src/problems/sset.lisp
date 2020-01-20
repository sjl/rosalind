(defpackage :rosalind/sset (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/sset)

;; The cardinality of a power set is 2ⁿ, because you can represent an individual
;; set as a binary string where 1 means the element is included and 0 is not,
;; and there are 2ⁿ possible binary strings of length n.

(define-problem sset (data stream) "3" "8"
  (mod (expt 2 (read data)) 1000000))


#; Scratch --------------------------------------------------------------------
