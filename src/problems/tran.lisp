(in-package :rosalind)

(defparameter *input-tran* ">Rosalind_0209
GCAACGCACAACGAAAACCCTTAGGGACTGGATTATTTCGTGATCGTTGTAGTTATTGGA
AGTACGGGCATCAACCCAGTT
>Rosalind_2200
TTATCTGACAAAGAAAGCCGTCAACGGCTGGATAATTTCGCGATCGTGCTGGTTACTGGC
GGTACGAGTGTTCCTTTGGGT")

(defparameter *output-tran* "1.21428571429")

(defun transitionp (x y)
  (and (char/= x y)
       (= (rings x) (rings y))))

(defun transversionp (x y)
  (and (char/= x y)
       (/= (rings x) (rings y))))

(define-problem tran (data stream)
    *input-tran*
    *output-tran*
  (destructuring-bind (x y)
      (mapcar #'cdr (read-fasta-into-alist data))
    (format nil "~,11F" (coerce (/ (mapcount #'transitionp x y)
                                   (mapcount #'transversionp x y))
                                'double-float))))

;; (problem-tran "2")
;; (solve tran)
