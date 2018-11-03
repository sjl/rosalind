(in-package :rosalind)

(define-problem iprb (data stream)
    "2 2 2"
    "0.78333"
  (let* ((d (read data))
         (h (read data))
         (r (read data))
         (n (+ d h r)))
    ;; We could expand this all into a giant equation and cancel it out, but
    ;; let's just let the computer do the busywork.
    (flet ((p-same (x also-x)
             (declare (ignore also-x))
             ;; P({X X}) = X/N * (X-1)/(N-1)
             ;;          = X(X-1) / N(N-1)
             ;;          = X²-X / N²-N
             (/ (- (* x x) x)
                (- (* n n) n)))
           (p-diff (x y)
             ;; P({X Y}) = P(X, Y)     + P(Y, X)
             ;;          = X/N * Y/N-1 + Y/N * X/N-1
             ;;          = XY/N(N-1)   + YX/N(N-1)
             ;;          = 2XY/N(N-1)
             ;;          = 2XY/N²-N
             (/ (* 2 x y)
                (- (* n n) n))))
      (format nil "~,5F"
              (+ (* (p-same d d) 1)      ;; AA AA
                 (* (p-diff d h) 1)      ;; AA Aa
                 (* (p-diff d r) 1)      ;; AA aa
                 (* (p-same h h) 3/4)    ;; Aa Aa
                 (* (p-diff h r) 1/2)    ;; Aa aa
                 (* (p-same r r) 0)))))) ;; aa aa

;; (problem-iprb)
;; (solve iprb)
