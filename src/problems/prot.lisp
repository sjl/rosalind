(defpackage :rosalind/prot (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/prot)

(define-problem prot (data string)
    "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    "MAMAPRTEINSTRING"
  (u:translate data))
