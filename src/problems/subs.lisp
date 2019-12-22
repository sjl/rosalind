(in-package :rosalind)

(defparameter *input-subs* "GATATATGCATATACTT
ATAT")

(defparameter *output-subs* "2 4 10")


(define-problem subs (data stream)
    *input-subs*
    *output-subs*
  (let ((haystack (read-line data))
        (needle (read-line data)))
    (iterate
      (for pos :seed -1 :then (search needle haystack :start2 (1+ pos)))
      (while pos)
      (collect (1+ pos) :into result)
      (finally (return (str:join " " result))))))

