(in-package :rosalind)

(defparameter *input-eval* "10
AG
0.25 0.5 0.75")

(defparameter *output-eval* "0.422 0.563 0.422")

(define-problem eval (data stream)
    *input-eval*
    *output-eval*
  (let* ((string-length (read data))
         (substring (read-line data))
         (gc-contents (read-all data))
         (chances (- string-length (1- (length substring)))))
    (float-string (mapcar (lambda (gc-content)
                            (* chances (sequence-probability
                                         (coerce gc-content 'double-float)
                                         substring)))
                          gc-contents))))
