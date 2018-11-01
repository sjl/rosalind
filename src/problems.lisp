(in-package :rosalind)

;;;; Testing ------------------------------------------------------------------
(defmacro define-test (problem input output &optional (test 'string=))
  `(test ,(symb 'test- problem)
     (is (,test ,output (,problem ,input)))))

(defun run-tests ()
  (1am:run))


;;;; Problems -----------------------------------------------------------------
(defmacro define-problem
    ((number name) args sample-input sample-output &body body)
  (let ((symbol (symb 'problem- number)))
    `(progn
       (defun ,symbol ,args ,@body)
       (setf (get ',symbol 'rosalind-name) ,(string-downcase name))
       (define-test ,symbol ,sample-input ,sample-output)
       ',symbol)))


(define-problem (1 dna) (data)
    "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
    "20 12 17 21"
  ;; A string is simply an ordered collection of symbols selected from some
  ;; alphabet and formed into a word; the length of a string is the number of
  ;; symbols that it contains.
  ;;
  ;; An example of a length 21 DNA string (whose alphabet contains the symbols
  ;; 'A', 'C', 'G', and 'T') is "ATGCTTCAGAAAGGTCTTACG."
  ;;
  ;; Given: A DNA string s of length at most 1000 nt.
  ;; Return: Four integers (separated by spaces) counting the respective number
  ;; of times that the symbols 'A', 'C', 'G', and 'T' occur in s.
  (let ((results (frequencies data)))
    (format nil "~D ~D ~D ~D"
            (gethash #\A results)
            (gethash #\C results)
            (gethash #\G results)
            (gethash #\T results))))

(define-problem (2 rna) (data)
    "GATGGAACTTGACTACGTAAATT"
    "GAUGGAACUUGACUACGUAAAUU"
  ;; An RNA string is a string formed from the alphabet containing 'A', 'C',
  ;; 'G', and 'U'.
  ;;
  ;; Given a DNA string t corresponding to a coding strand, its transcribed RNA
  ;; string u is formed by replacing all occurrences of 'T' in t with 'U' in u.
  ;;
  ;; Given: A DNA string t having length at most 1000 nt.
  ;;
  ;; Return: The transcribed RNA string of t.
  (substitute #\U #\T data))

(define-problem (3 revc) (data)
    "AAAACCCGGT"
    "ACCGGGTTTT"
  ;; In DNA strings, symbols 'A' and 'T' are complements of each other, as are
  ;; 'C' and 'G'.
  ;;
  ;; The reverse complement of a DNA string s is the string sc formed by
  ;; reversing the symbols of s, then taking the complement of each symbol
  ;; (e.g., the reverse complement of "GTCA" is "TGAC").
  ;;
  ;; Given: A DNA string s of length at most 1000 bp.
  ;;
  ;; Return: The reverse complement sc of s.
  (copyf data)
  (flet ((dna-complement (base)
           (case base
             (#\A #\T)
             (#\T #\A)
             (#\G #\C)
             (#\C #\G)
             (t base)))) ; newline etc
    (map-into data #'dna-complement data)
    (nreverse data)))


;;;; Solutions ----------------------------------------------------------------
(defun read-problem-data (problem)
  (-<> (get problem 'rosalind-name)
    (format nil "~~/Downloads/rosalind_~A.txt" <>)
    read-file-into-string))

(defun solve% (problem)
  (pbcopy (funcall problem (read-problem-data problem))))

(defmacro solve (problem)
  `(solve% ',problem))


;; (problem-3 "AAAACCCGGT")

;; (solve problem-3)


