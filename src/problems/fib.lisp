(defpackage :rosalind/fib (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/fib)

(define-problem fib (data stream)
    "5 3"
    "19"
  (let ((months (read data))
        (litter-size (read data)))
    ;; The problem description is written incorrectly.  They say "total number
    ;; of rabbit pairs … after n months", but if we list out the values for
    ;; their sample parameters, we can see their answer is wrong:
    ;;
    ;;     MONTHS  | BREEDING | TOTAL
    ;;     ELAPSED | PAIRS    | PAIRS
    ;;     ---------------------------
    ;;           0 | 0        | 1
    ;;           1 | 1        | 1
    ;;           2 | 1        | 4
    ;;           3 | 4        | 7
    ;;           4 | 7        | 19      <-- their answer
    ;;           5 | 19       | 40      <-- actual answer
    ;;
    ;; Their problem is they're treating the Fibonacci numbers Fₙ as "number
    ;; of rabbits after n months", but really Fₙ means "number of rabbits at
    ;; the beginning of the nth (ordinal) month".  This can even be seen in
    ;; their own diagram at the top of the page -- months 1 and 2 both have
    ;; 1 pair each.  If we start in month 1, and let 2 months elapse, we end up
    ;; at F₃, not F₂.
    ;;
    ;; So we'll just decrement months by one.  Sigh.
    (iterate
      (with breeding = 0)
      (with total = 1)
      (repeat (1- months))
      (psetf breeding total
             total (+ total (* breeding litter-size)))
      (finally (return total)))))

;; (problem-fib "5 3")
;; (solve fib)
