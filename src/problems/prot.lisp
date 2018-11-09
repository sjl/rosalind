(in-package :rosalind)

(defmacro codon-case ((vector index) &rest clauses)
  ;; Compiles a giant list of clauses into a tree of ECASEs.
  ;;
  ;; Each codon will have at most 3 ECASEs to pass through.  Each ECASE has at
  ;; most four options, so in the worst case we end up with 3 * 4 = 12
  ;; comparisons instead of 64.
  ;;
  ;; If we ever convert bases to vectors of (unsigned-byte 2)s we could
  ;; potentially use a lookup table here, e.g.:
  ;;
  ;;     (aref +amino-acids+ (+ x (ash y 2) (ash z 4)))
  (alexandria:once-only (vector index)
    (alexandria:with-gensyms (x y z)
      `(let ((,x (aref ,vector ,index))
             (,y (aref ,vector (+ ,index 1)))
             (,z (aref ,vector (+ ,index 2))))
         ,(labels ((strip (clauses)
                     (if (= 1 (length (caar clauses)))
                       (cadar clauses)
                       (iterate (for (head body) :in clauses)
                                (collect (list (subseq head 1) body)))))
                   (split (clauses)
                     (-<> clauses
                       (group-by (rcurry #'aref 0) <> :key #'first)
                       (iterate (for (k v) :in-hashtable <>)
                                (collect (list k (strip v)))))))
            (recursively ((clauses (split clauses))
                          (codons (list x y z))
                          (i 0))
              `(ecase ,(first codons)
                 ,@(iterate (for (k remaining) :in clauses)
                            (collect `(,k ,(if (atom remaining)
                                             remaining
                                             (recur (split remaining)
                                                    (rest codons)
                                                    (1+ i)))))))))))))

(defun codon-to-protein (vector index)
  "Return the amino acid encoded by the codon in `vector` at `index`."
  (codon-case (vector index)
    ("UUU" #\F) ("CUU" #\L) ("AUU" #\I) ("GUU" #\V)
    ("UUC" #\F) ("CUC" #\L) ("AUC" #\I) ("GUC" #\V)
    ("UUA" #\L) ("CUA" #\L) ("AUA" #\I) ("GUA" #\V)
    ("UUG" #\L) ("CUG" #\L) ("AUG" #\M) ("GUG" #\V)
    ("UCU" #\S) ("CCU" #\P) ("ACU" #\T) ("GCU" #\A)
    ("UCC" #\S) ("CCC" #\P) ("ACC" #\T) ("GCC" #\A)
    ("UCA" #\S) ("CCA" #\P) ("ACA" #\T) ("GCA" #\A)
    ("UCG" #\S) ("CCG" #\P) ("ACG" #\T) ("GCG" #\A)
    ("UAU" #\Y) ("CAU" #\H) ("AAU" #\N) ("GAU" #\D)
    ("UAC" #\Y) ("CAC" #\H) ("AAC" #\N) ("GAC" #\D)
    ("UAA" nil) ("CAA" #\Q) ("AAA" #\K) ("GAA" #\E)
    ("UAG" nil) ("CAG" #\Q) ("AAG" #\K) ("GAG" #\E)
    ("UGU" #\C) ("CGU" #\R) ("AGU" #\S) ("GGU" #\G)
    ("UGC" #\C) ("CGC" #\R) ("AGC" #\S) ("GGC" #\G)
    ("UGA" nil) ("CGA" #\R) ("AGA" #\R) ("GGA" #\G)
    ("UGG" #\W) ("CGG" #\R) ("AGG" #\R) ("GGG" #\G)))

(defun translate (rna &key (start 0))
  "Translate a string of RNA bases into a protein string of amino acids.

  `rna` will be searched (beginning at `start`) for a start codon and
  translation will proceed from there.  If no start codon occurs after `start`
  then `nil` will be returned.

  Once a start codon has been found, translation proceeds to the next stop
  codon.  If no stop codon is present, `nil` will be returned.

  Otherwise two values are returned: the protein string and the index into `rna`
  where it started.

  "
  (when-let ((start (search "AUG" rna :start2 start)))
    (values
      (iterate (with limit = (- (length rna) 3))
               (for i :from start :by 3)
               (when (> i limit)
                 (return-from translate (values nil nil)))
               (for protein = (codon-to-protein rna i))
               (while protein)
               (collect protein :result-type 'string))
      start)))


(define-problem prot (data string)
    "AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA"
    "MAMAPRTEINSTRING"
  (translate data))

;; (solve prot)
