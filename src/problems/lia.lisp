(in-package :rosalind)

;; When a heterozygous organism mates, its offspring have a 50% chance to be
;; heterozygous themselves, *regardless of what the other mate happens to be*:
;;
;;         A  a       A  a       A  a
;;      A AA Aa    A AA Aa    a aA aa
;;      A AA Aa    a Aa aa    a aA aa
;;
;; Because everyone breeds with an Aa Bb mate, we don't need to worry about
;; tracking populations along the way.  Every child has a 1/2 chance of being
;; Aa, and likewise a 1/2 chance of being Bb.
;;
;; Because A's and B's are independent, this means there's a 1/2 * 1/2 = 1/4
;; chance of any given child being Aa Bb.

(defmacro do-sum ((var from to) &body body)
  "Sum `body` with `var` iterating over `[from, to]`.

  It's just Σ:

      to
      ===
      \
       >   body
      /
      ===
      n = from

  "
  (once-only (to)
    (with-gensyms (result)
      `(do ((,var ,from (1+ ,var))
            (,result 0))
         ((> ,var ,to) ,result)
         (incf ,result (progn ,@body))))))

(defmacro do-product ((var from to) &body body)
  "Multiply `body` with `var` iterating over `[from, to]`.

  It's just Π:

       to
      =====
       | |
       | |  body
       | |
      n = from

  "
  (once-only (to)
    (with-gensyms (result)
      `(do ((,var ,from (1+ ,var))
            (,result 1))
         ((> ,var ,to) ,result)
         (setf ,result (* ,result (progn ,@body)))))))


(defmacro Σ (bindings &body body) ;; lol
  `(do-sum ,bindings ,@body))

(defmacro Π (bindings &body body) ;; lol
  `(do-product ,bindings ,@body))


(defun binomial-coefficient (n k)
  "Return `n` choose `k`."
  ;; https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  (Π (i 1 k)
    (/ (- (1+ n) i) i)))

(defun bernoulli-exactly (successes trials success-probability)
  "Return the probability of exactly `successes` in `trials` Bernoulli trials.

  Returns the probability of getting exactly `n` successes out of `trials`
  Bernoulli trials with `success-probability`.

  "
  ;; For a group of N trials with success/failure probabilities s/f, any given
  ;; ordering of exactly S successes and F failures will have probability:
  ;;
  ;;     s * s * s * … * f * f * f
  ;;
  ;; There are N choose S possible orderings (or N choose F, it doesn't matter),
  ;; so we just sum up the probabilities of all of them.
  (let ((failures (- trials successes))
        (failure-probability (- 1 success-probability)))
    (* (binomial-coefficient trials successes)
       (expt success-probability successes)
       (expt failure-probability failures))))

(defun bernoulli-at-least (successes trials success-probability)
  "Return the probability of at least `successes` in `trials` Bernoulli trials.

  Returns the probability of getting at least `n` successes out of `trials`
  Bernoulli trials with `success-probability`.

  "
  ;; P(≥S) = P(=S) + P(=S+1) + … + P(N)
  (Σ (n successes trials)
    (bernoulli-exactly n trials success-probability)))

(define-problem lia (data stream)
    "2 1"
    "0.684"
  (let* ((generations (read data))
         (target (read data))
         (population (expt 2 generations)))
    (format nil "~,3F" (bernoulli-at-least target population 1/4))))
