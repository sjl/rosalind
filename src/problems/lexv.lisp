(in-package :rosalind)

(defparameter *input-lexv*
  "D N A
3")

(defparameter *output-lexv*
  "D
DD
DDD
DDN
DDA
DN
DND
DNN
DNA
DA
DAD
DAN
DAA
N
ND
NDD
NDN
NDA
NN
NND
NNN
NNA
NA
NAD
NAN
NAA
A
AD
ADD
ADN
ADA
AN
AND
ANN
ANA
AA
AAD
AAN
AAA
")


(define-problem lexv (data stream)
    *input-lexv*
    *output-lexv*
  (let* ((alphabet (remove #\space (read-line data)))
         (n (read data))
         (string (make-string n)))
    (with-output-to-string (s)
      (recursively ((n n)
                    (i 0))
        (unless (zerop i)
          ;; The empty string *is* first, lexicographically, but I don't think
          ;; they accept it in the answer for some reason.
          (write-string string s :end i)
          (terpri s))
        (unless (zerop n)
          (map nil (lambda (ch)
                     (setf (aref string i) ch)
                     (recur (1- n) (1+ i)))
               alphabet))))))

