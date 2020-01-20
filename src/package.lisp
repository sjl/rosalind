(defpackage :rosalind/utils
  (:nicknames :u)
  (:use :cl :iterate :losh)
  (:import-from :alexandria
    :curry :rcurry :compose
    :ensure-gethash
    :with-gensyms :once-only :symbolicate)
  (:export
    :curry :rcurry :compose
    :ensure-gethash
    :with-gensyms :once-only :symbolicate

    :define-problem :solve

    :factorial

    :permutations

    :dna-complement :reverse-complement :nreverse-complement
    :transcribe :ntranscribe
    :translate

    :gcp :base-probability :sequence-probability

    :mapcount

    :hamming

    :string-empty-p
    :first-char

    :Σ :Π :binomial-coefficient

    :returning-final

    :read-lines
    :read-fasta
    :read-fasta-into-hash-table
    :read-fasta-into-alist

    :buffering

    :uniprot

    :float-string

    :run-tests :solve))

(defpackage :rosalind
  (:import-from :rosalind/utils :run-tests :define-problem :solve)
  (:export :run-tests :define-problem :solve))
