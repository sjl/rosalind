(in-package :rosalind)

(define-problem iev (data stream)
    "1 0 0 1 0 1"
    "3.5000"
  (let* ((dd (read data))
         (dh (read data))
         (dr (read data))
         (hh (read data))
         (hr (read data))
         (rr (read data)))
    (format nil "~,4F"
            ;; It's just a weighted average…
            (* 2 (+ (* dd 1)
                    (* dh 1)
                    (* dr 1)
                    (* hh 3/4)
                    (* hr 1/2)
                    (* rr 0))))))

;; (problem-iev)
;; (solve iev)
