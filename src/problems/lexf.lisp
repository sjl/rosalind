(in-package :rosalind)

(defparameter *input-lexf*
  "A C G T
2")

(defparameter *output-lexf*
  "AA
AC
AG
AT
CA
CC
CG
CT
GA
GC
GG
GT
TA
TC
TG
TT
")


(define-problem lexf (data stream)
    *input-lexf*
    *output-lexf*
  (let* ((alphabet (sort (remove #\space (read-line data)) #'char<))
         (n (read data))
         (string (make-string n)))
    (with-output-to-string (s)
      (recursively ((n n)
                    (i 0))
        (if (zerop n)
          (progn (write-string string s)
                 (terpri s))
          (map nil (lambda (ch)
                     (setf (aref string i) ch)
                     (recur (1- n) (1+ i)))
               alphabet))))))
