(defpackage :rosalind/mmch (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/mmch)

(defparameter *input* ">Rosalind_92
AUGCUUC")

(defparameter *output* "6")

;; We can attack this similarly to PMCH.
;;
;; Consider the A/U graph, and assume without loss of generality that the number
;; of As is less than or equal to the number of Us.  For each A, we choose
;; a U to pair it with like in PMCM, but this time we don't have enough A's to
;; get the full factorial.  Instead we get U(U-1)(U-2)…(U-(A-1)) which we can
;; just multiply out.
;;
;; … why did I get a dynamic programming achievement on Rosalind for solving
;; this problem?

(defun pairings (a b)
  (when (< b a)
    (rotatef a b))
  (iterate (repeat a)
           (for n :downfrom b)
           (multiplying n)))

(define-problem mmch (data stream) *input* *output*
  (let* ((bases (nth-value 1 (u:read-fasta data)))
         (freqs (frequencies bases)))
    (* (pairings (gethash #\A freqs)
                 (gethash #\U freqs))
       (pairings (gethash #\C freqs)
                 (gethash #\G freqs)))))

#; Scratch --------------------------------------------------------------------
(problem-mmch)
