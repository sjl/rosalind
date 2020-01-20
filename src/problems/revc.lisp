(defpackage :rosalind/revc (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/revc)

;; DNA is made up of two strands running in opposite directions, usually twisted
;; into a double helix, with the bases bonded:
;;
;;    …sP-+-sP-+-sP-+-sP-+-sP…
;;        |    |    |    |
;;        C    A    T    G          s = sugar molecule
;;       G    T    A    C           p = phosphate anion
;;       |    |    |    |
;;   …sP-+-sP-+-sP-+-sP-+-sP…
;;
;; Each base will only bond with one specific other base:
;;
;; * Adenine/Thymine
;; * Cytosine/Guanine
;;
;; The "complement" of a base is the other base it will bond to.
;;
;; Two bonded bases are called a base pair (bp).  Generally DNA lengths are
;; specified in numbers of base pairs.
;;
;; If we know the order of the bases in one strand, we can figure out the other
;; strand by taking the reverse complement.
;;
;; The problem summaries don't really make it clear what "running in opposite
;; directions" means.  I think I remember there being something about DNA having
;; polarized ends, with one end being called 3′ and the other being 5′, but I'm
;; not 100% sure.

(define-problem revc (data string)
    "AAAACCCGGT"
    "ACCGGGTTTT"
  (u:nreverse-complement (delete #\newline data)))

