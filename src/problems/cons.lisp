(in-package :rosalind)

(defparameter *input-cons* ">Rosalind_1
ATCCAGCT
>Rosalind_2
GGGCAACT
>Rosalind_3
ATGGATCT
>Rosalind_4
AAGCAACC
>Rosalind_5
TTGGAACT
>Rosalind_6
ATGCCATT
>Rosalind_7
ATGGCACT")

(defparameter *output-cons* "ATGCAACT
A: 5 1 0 0 5 5 0 0
C: 0 0 1 4 2 0 6 1
G: 1 1 6 3 0 1 0 0
T: 1 5 0 0 0 1 1 6
")


(defun make-profile-matrix% (length)
  "Allocate a zero-filled profile matrix with 4 rows and `length` columns."
  (make-array (list 4 length) :initial-element 0))

(defmacro pmref (profile-matrix base pos)
  `(aref ,profile-matrix
     (ecase ,base
       (#\A 0)
       (#\C 1)
       (#\G 2)
       (#\T 3))
     ,pos))

(defun pmincf (profile-matrix dna)
  "Increment the profile matrix counts for the given `dna` string."
  (iterate (for base :in-vector dna :with-index pos)
           (incf (pmref profile-matrix base pos))))


(defun profile-matrix-from-fasta (source)
  "Read FASTA data from `source` and return its profile matrix.

  All DNA strings in the data must have the same length.

  A profile matrix is a 4xN matrix containing the total counts of A, C, G, and
  T bases at each position in the input strings.  For example, for the input
  strings:

    AAAA
    AGTC
    GGGG
    TAAC

  the profile matrix would be:

    pos: 0 1 2 3
    ------------
      A: 2 2 2 1
      C: 0 0 0 2
      G: 1 2 1 1
      T: 1 0 1 0

  "
  (iterate
    (with result)
    (with length)
    (for (nil dna) :in-fasta source)
    (for n :from 0)

    (if-first-time
      (setf length (length dna)
            result (make-profile-matrix% length))
      (assert (= length (length dna)) ()
        "The ~:R DNA string in the supplied FASTA data has ~D bases (expected ~D)."
        n (length dna) length))

    (pmincf result dna)

    (finally (return result))))


(defun consensus-string (profile-matrix)
  "Return a consensus string from the given `profile-matrix`.

  A consensus string is a string where the base at each position is the most
  common base at that position among all of the input strings (ties are broken
  arbitrarily).  For example, for the input strings:

    AAAA
    AGTC
    GGGG
    TAAC

  the consensus string would be \"AGAC\".

  "
  (iterate
    (with (nil length) = (array-dimensions profile-matrix))
    (for pos :below length)
    (for as = (pmref profile-matrix #\A pos))
    (for cs = (pmref profile-matrix #\C pos))
    (for gs = (pmref profile-matrix #\G pos))
    (for ts = (pmref profile-matrix #\T pos))
    (for m = (max as cs gs ts))
    (for winner = (cond ((= m as) #\A)
                        ((= m cs) #\C)
                        ((= m gs) #\G)
                        ((= m ts) #\T)))
    (collecting winner :result-type 'string)))


(define-problem cons (data stream)
    *input-cons*
    *output-cons*
  (let* ((profile-matrix (profile-matrix-from-fasta data))
         (consensus-string (consensus-string profile-matrix))
         (length (length consensus-string)))
    (with-output-to-string (s)
      (format s "~A~%" consensus-string)
      (dolist (base '(#\A #\C #\G #\T))
        (format s "~C:" base)
        (dotimes (i length)
          (format s " ~D" (pmref profile-matrix base i)))
        (terpri s)))))


;; (problem-cons)
;; (solve cons)
