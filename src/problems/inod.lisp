(defpackage :rosalind/inod (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/inod)

;; This one is trivial once you know the closed-form solution of N-2.  The
;; intuition for that can come in two parts.
;;
;; First, a rooted binary tree has N-1 internal nodes.  This is because at any
;; given point as you're building the tree, you select 2 of the remaining nodes
;; and join them together with an internal node, which reduces the total
;; remaining by 1.  You end when there is only one remaining node (the root) and
;; so you did N-1 subtractions.
;;
;; To convert this to an unrooted tree, you replace the root node with an edge,
;; which subtracts one more internal node from the graph.  So you're left with
;; N-2 internal nodes.

(define-problem inod (data stream)
    "4"
    "2"
  (- (read data) 2))


#; Scratch --------------------------------------------------------------------

(problem-inod)
