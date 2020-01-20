(defpackage :rosalind/pmch (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/pmch)

(defparameter *input* ">Rosalind_23
AGCUAGUCAU")

(defparameter *output* "12")

;; We can make a few observations to make things easier (well, trivial).
;;
;; First: the adjacency edges are a red herring.  Ignore them.
;;
;; Next: because adenine only interacts with uracil and guanine only interacts
;; with cytosine, we can split the problem apart into two separate graphs.  We
;; can compute the number of perfect matchings of each graph separately and then
;; multiply them together at the end to find the total.
;;
;; For each sub graph, every node has edges to all nodes of the complementary
;; base.  The problem description also guarantees that the number of
;; complementary bases are equal.
;;
;; Say we're looking at the A/U graph, and there are N adenine bases and
;; N uracil bases.  For each adenine, we need to pick a uracil to match it with.
;; For the first adenine we have N uracil's to choose from.  For the second
;; adenine we have N-1 uracils.  And so on down to the final pair.  So the total
;; number of choices we have for each graph is N(N-1)(N-2)…(1) = N!

(define-problem pmch (data stream) *input* *output*
  (let ((bases (nth-value 1 (u:read-fasta data))))
    (* (u:factorial (count #\A bases))
       (u:factorial (count #\G bases)))))
