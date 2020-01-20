(defpackage :rosalind/mprt (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/mprt)

;; This was pretty simple, except for discovering that cl-ppcre's all-matches
;; function skips overlapping matches.  Otherwise we just convert the motif to
;; a regex and handle grabbing the data from Uniprot (which is straightforward
;; but can be slow).

(defparameter *input-mprt*
  "A2Z669
B5ZC00
P07204_TRBM_HUMAN
P20840_SAG1_YEAST")

(defparameter *output-mprt*
  "B5ZC00
85 118 142 306 395
P07204_TRBM_HUMAN
47 115 116 382 409
P20840_SAG1_YEAST
79 109 135 248 306 348 364 402 485 501 614
")

(defparameter *motif-n-glycosylation* "N{P}[ST]{P}")

(defun motif-to-regex (motif)
  "Turn a protein motif shorthand into a PPCRE scanner."
  (-<> motif
    ;; All we have to do is turn {X} into [^X] and compile.
    (ppcre:regex-replace-all "[{]" <> "[^")
    (substitute #\] #\} <>)
    ppcre:create-scanner))

(defun all-matches-dammit (regex target-string)
  "Return a list of start and end positions of all matches of `regex` on `target-string`.

  Unlike `ppcre:all-matches` this will return ALL matches, even if they're
  overlapping.  Example:

    (all-matches-dammit \"a..\" \"aabc\")
    ; =>
    ; (0 3 1 4)

    (ppcre:all-matches \"a..\" \"aabc\")
    ; =>
    ; (0 3) ; dammit

  "
  ;; cl-ppcre
  (iterate
    (with i = 0)
    (for (values start end) = (ppcre:scan regex target-string :start i))
    (while start)
    (collect start)
    (collect end)
    (setf i (1+ start))))


(define-problem mprt (data stream)
    *input-mprt*
    *output-mprt*
  (with-output-to-string (s)
    (iterate
      (with n-glycosylation = (motif-to-regex *motif-n-glycosylation*))
      (for id :in-stream data :using #'read-line)
      (for (nil . protein) = (u:uniprot id))
      (for matches = (all-matches-dammit n-glycosylation protein))
      (when matches
        (format s "~A~%~{~D~*~^ ~}~%" id (mapcar #'1+ matches))))))
