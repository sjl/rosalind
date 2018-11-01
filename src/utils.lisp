(in-package :rosalind)


(defun sh (command input)
  (declare (ignorable command input))
  #+sbcl
  (sb-ext:run-program (first command) (rest command)
                      :search t
                      :input (make-string-input-stream input))
  #+ccl
  (ccl:run-program (first command) (rest command)
                   :input (make-string-input-stream input))
  #+abcl
  (let ((p (system:run-program (first command) (rest command)
                               :input :stream
                               :output t
                               :wait nil)))
    (write-string input (system:process-input p))
    (close (system:process-input p)))
  #-(or sbcl ccl abcl)
  (error "Not implemented for this Lisp implementation, sorry"))

(defun pbcopy (string)
  (values string (sh '("pbcopy") string)))

(defmacro copyf (sequence)
  `(setf ,sequence (copy-seq ,sequence)))
