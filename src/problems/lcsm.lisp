(in-package :rosalind)

;; This one sucked.  We should use suffix trees some day but I just hacked this
;; together for now.

(defparameter *input-lcsm* ">Rosalind_1
GATTACA
>Rosalind_2
TAGACCA
>Rosalind_3
ATACA")

(defun compute-substring-table% (string1 string2)
  ;; Compute the table of substring lengths.
  ;;
  ;;   a b c d e c
  ;; x 0 0 0 0 0 0
  ;; a 1 0 0 0 0 0
  ;; b 0 2 0 0 0 0
  ;; c 0 0 3 0 0 1
  ;; e 0 0 0 0 0 1
  (check-type string1 string)
  (check-type string2 string)
  (let ((table (make-array (list (length string1) (length string2)))))
    (dotimes (i1 (length string1))
      (dotimes (i2 (length string2))
        (setf (aref table i1 i2)
              (if (char= (aref string1 i1) (aref string2 i2))
                (if (or (zerop i1) (zerop i2))
                  1
                  (1+ (aref table (1- i1) (1- i2))))
                0))))
    table))

(defun-inline substring-at% (string index length)
  ;; Given a matching string/table-index and a length, return the substring.
  ;; The table effectively stores (start, end] but subseq wants [start, end).
  (let ((end (1+ index)))
    (subseq string (- end length) end)))

(defun-inline find-substrings-of-length% (table string1 length)
  ;; Find all the substrings in the table with the given length.
  (iterate (for (l i1 nil) :in-array table)
           (when (= length l)
             (collect (substring-at% string1 i1 length)))))

(defun-inline find-maximum-length% (table)
  ;; Find the highest length in the table.
  (iterate (for l :across-flat-array table)
           (maximizing l)))

(defun longest-common-substrings (string1 string2)
  "Return a list of the longest common substrings of `string1` and `string2`."
  (let ((table (compute-substring-table% string1 string2)))
    (find-substrings-of-length% table string1 (find-maximum-length% table))))

(defun longest (strings)
  "Return a list of the longest strings in `strings`."
  (remove-if-not
    (curry #'= (alexandria:extremum (mapcar #'length strings) #'>))
    strings :key #'length))

(defun longest-common-substrings-of-any (substrings string)
  "Return the longest common substrings of `string` and any one of `substrings`."
  (-<> (iterate
         (for substring :in substrings)
         (appending (longest-common-substrings substring string)))
    longest
    (remove-duplicates <> :test #'string=)))

(define-problem lcsm (data stream)
    *input-lcsm*
    "AC"
  (let ((lines (mapcar #'cdr (read-fasta-into-alist data))))
    (-<> (reduce #'longest-common-substrings-of-any (rest lines)
                 :initial-value (list (first lines)))
      (sort <> #'string<) ; tests
      first)))
