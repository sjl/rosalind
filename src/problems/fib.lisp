(in-package :rosalind)

;; A sequence is an ordered collection of objects (usually numbers), which are
;; allowed to repeat. Sequences can be finite or infinite. Two examples are the
;; finite sequence (π,−2–√,0,π) and the infinite sequence of odd numbers
;; (1,3,5,7,9,…). We use the notation an to represent the n-th term of
;; a sequence.
;;
;; A recurrence relation is a way of defining the terms of a sequence with
;; respect to the values of previous terms. In the case of Fibonacci's rabbits
;; from the introduction, any given month will contain the rabbits that were
;; alive the previous month, plus any new offspring. A key observation is that
;; the number of offspring in any month is equal to the number of rabbits that
;; were alive two months prior. As a result, if Fn represents the number of
;; rabbit pairs alive after the n-th month, then we obtain the Fibonacci
;; sequence having terms Fn that are defined by the recurrence relation
;; Fn=Fn−1+Fn−2 (with F1=F2=1 to initiate the sequence). Although the sequence
;; bears Fibonacci's name, it was known to Indian mathematicians over two
;; millennia ago.
;;
;; When finding the n-th term of a sequence defined by a recurrence relation, we
;; can simply use the recurrence relation to generate terms for progressively
;; larger values of n. This problem introduces us to the computational technique
;; of dynamic programming, which successively builds up solutions by using the
;; answers to smaller cases.
;;
;; Given: Positive integers n≤40 and k≤5
;;
;; Return: The total number of rabbit pairs that will be present after n months,
;; if we begin with 1 pair and in each generation, every pair of
;; reproduction-age rabbits produces a litter of k rabbit pairs (instead of only
;; 1 pair).

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
