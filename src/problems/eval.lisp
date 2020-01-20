(defpackage :rosalind/eval (:use :cl :rosalind :losh :iterate))
(in-package :rosalind/eval)

(defparameter *input* "10
AG
0.25 0.5 0.75")

(defparameter *output* "0.422 0.563 0.422")

(define-problem eval (data stream) *input* *output*
  (let* ((string-length (read data))
         (substring (read-line data))
         (gc-contents (read-all data))
         (chances (- string-length (1- (length substring)))))
    (u:float-string (mapcar (lambda (gc-content)
                              (* chances (u:sequence-probability
                                           (coerce gc-content 'double-float)
                                           substring)))
                            gc-contents))))
