(defpackage :rosalind/rna (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/rna)

;; RNA is a another nucleic acid that is similar to DNA, with the following
;; differences:
;;
;; * It uses ribose for its suger molecules (instead of deoxyribose)
;; * It contains the base Uracil (U) instead of Thymine
;; * It's single-helixed instead of double-helixed (I think?)
;;
;; RNA is produced from DNA in a multi-step process called "transcription" that
;; happens in the nucleus (at least in eukaryotes):
;;
;; 1. pre-mRNA is produced from DNA
;; 2. mRNA is produced from pre-mRNA
;;
;; After that the mRNA exists the nucleus.  Then proteins are produced from the
;; mRNA by ribosomes.  That process is called "translation".

(define-problem rna (data string)
    "GATGGAACTTGACTACGTAAATT"
    "GAUGGAACUUGACUACGUAAAUU"
  "Transcribe `data` from DNA into RNA."
  (u:ntranscribe data))

