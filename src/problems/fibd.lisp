(in-package :rosalind)

;; Recall the definition of the Fibonacci numbers from “Rabbits and Recurrence
;; Relations”, which followed the recurrence relation Fn=Fn−1+Fn−2 and assumed
;; that each pair of rabbits reaches maturity in one month and produces
;; a single pair of offspring (one male, one female) each subsequent month.
;;
;; Our aim is to somehow modify this recurrence relation to achieve a dynamic
;; programming solution in the case that all rabbits die out after a fixed
;; number of months. See Figure 4 for a depiction of a rabbit tree in which
;; rabbits live for three months (meaning that they reproduce only twice before
;; dying).
;;
;; Given: Positive integers n≤100 and m≤20.
;;
;; Return: The total number of pairs of rabbits that will remain after the n-th
;; month if all rabbits live for m months.

(define-problem fibd (data stream)
    "6 3"
    "4"
  (iter
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
      (buffering (returning-final (population month))
                 :into population
                 :initial-contents '(nil 1))
      (buffering (births month)
                 :into births
                 :initial-contents '(nil 1)))))

(problem-fibd "45 6")
;; (solve fibd)
