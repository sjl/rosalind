(defpackage :rosalind/fibd (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/fibd)

(define-problem fibd (data stream)
    "6 3"
    "4"
  (iterate
    (with months = (read data))
    (with lifespan = (read data))
    (for month :from 2 :to months)
    (labels ((ref (array index)
               (if (plusp index)
                 (aref array index)
                 0))
             (breeding (month)
               (- (ref population (- month 1))
                  (deaths month)))
             (births (month)
               (breeding (- month 1)))
             (deaths (month)
               (ref births (- month lifespan)))
             (population (month)
               (+ (breeding month)
                  (births month))))
      ;; We initialize the buffers with NIL in index 0 for the 1-based months,
      ;; and 1 in index 0 for the initial pair of rabbits.
      (u:buffering (u:returning-final (population month))
                   :into population
                   :initial-contents '(nil 1))
      (u:buffering (births month)
                   :into births
                   :initial-contents '(nil 1)))))


#; Scratch --------------------------------------------------------------------
(problem-fibd "45 6")
;; (solve fibd)
