(defpackage :rosalind/mrna (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/mrna)

;; We're using a real programming language, so we have actual numbers and don't
;; need to bother with modular arithmetic for the tiny inputs they're giving us
;; as data sets:
;;
;; (solve mrna) =>
;; 18223466647209680564519396994425171292117842339309783544950259938739364856964963
;; 66953808268144949737039356476412047824299213884930571696940173622342387041181012
;; 59342299576631057415056504154013752799852275638571559263796551872383306751629837
;; 52919433077998281001987083484254030689771952436244061958890571552274110433782197
;; 92667561733403741083516672972846644834108916554466004897769300303373575679132517
;; 064863091594635637947593916416
;;
;; But let's humor them and do it anyway, for fun.  I added it into my utils
;; library.

(defun acid-to-codons (acid)
  "Return a list of the codons that could have encoded this amino acid.

  Neither the list nor the strings in it will be fresh.
  Use `(mapcar #'copy-seq (acid-to-codon …))` if you need fresh copies.

  "
  (ecase acid
    (#\I   '("AUA" "AUC" "AUU"))
    (#\R   '("AGG" "CGG" "AGA" "CGA" "CGC" "CGU"))
    (#\D   '("GAC" "GAU"))
    (#\V   '("GUG" "GUA" "GUC" "GUU"))
    (#\P   '("CCG" "CCA" "CCC" "CCU"))
    (#\T   '("ACG" "ACA" "ACC" "ACU"))
    (#\S   '("AGC" "AGU" "UCG" "UCA" "UCC" "UCU"))
    (#\F   '("UUC" "UUU"))
    (#\Y   '("UAC" "UAU"))
    (#\L   '("CUG" "UUG" "CUA" "UUA" "CUC" "CUU"))
    (#\Q   '("CAG" "CAA"))
    (#\H   '("CAC " "CAU"))
    (#\C   '("UGC" "UGU"))
    (#\K   '("AAG" "AAA"))
    (#\N   '("AAC" "AAU"))
    (#\M   '("AUG"))
    (#\W   '("UGG"))
    (#\G   '("GGG" "GGA" "GGC" "GGU"))
    (#\E   '("GAG" "GAA"))
    (#\A   '("GCG" "GCA" "GCC" "GCU"))
    ((nil) '("UGA" "UAG" "UAA"))))

(defun acid-codon-count (acid)
  (length (acid-to-codons acid)))


(define-problem mrna (data string) "MA" "12"
  (product (delete #\newline data)
           :modulo 1000000
           :key #'acid-codon-count
           :initial-value (acid-codon-count nil)))


