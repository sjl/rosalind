(in-package :rosalind)


;;;; Misc ---------------------------------------------------------------------
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

(defun ensure-stream (input)
  (ctypecase input
    (stream input)
    (string (make-string-input-stream input))))

(defun nconcatenate (v1 v2)
  (let* ((l1 (length v1))
         (l2 (length v2))
         (needed (+ l1 l2)))
    (when (< (array-total-size v1) needed)
      (adjust-array v1 (max needed (* l1 2))))
    (setf (fill-pointer v1) needed)
    (replace v1 v2 :start1 l1)
    (values)))

(defun make-buffer (&optional (capacity 64))
  (make-array capacity :element-type 'character :adjustable t :fill-pointer 0))

(defun round-to (number precision)
  "Round `number` to the given `precision`.

  Examples:

    (round-to 13 10)      ; => 10
    (round-to 15 10)      ; => 20
    (round-to 44 25)      ; => 50
    (round-to 457/87 1/2) ; => 11/2

  "
  (* precision (round number precision)))


;;;; Testing ------------------------------------------------------------------
(defmacro define-test (problem input output &optional (test 'string=))
  `(test ,(symb 'test- problem)
     (is (,test ,output (,problem ,input)))))

(defun run-tests ()
  (1am:run))


;;;; File Formats -------------------------------------------------------------

(defmacro-driver (FOR vars IN-FASTA source)
  (nest
    (destructuring-bind (label line) vars)
    (with-gensyms (stream l))
    (let ((kwd (if generate 'generate 'for))))
    `(progn
       (with ,label = nil)
       (with ,stream = (ensure-stream ,source))
       (,kwd ,line :do-next
        (labels ((labelp (line)
                   (char= #\> (aref line 0)))
                 (parse-next ()
                   (let ((,l (read-line ,stream nil nil nil)))
                     (cond
                       ((null ,l) (terminate))
                       ((zerop (length ,l)) (parse-next))
                       ((labelp ,l) (progn (setf ,label (subseq ,l 1)
                                                 ,line (make-buffer))
                                           (parse-next)))
                       (t (progn (nconcatenate ,line ,l)
                                 (unless (char= #\> (peek-char nil ,stream nil #\>)) ; yuck
                                   (parse-next))))))))
          (parse-next))))))



;;;; Problems -----------------------------------------------------------------
(defmacro define-problem (name args sample-input sample-output &body body)
  (let ((symbol (symb 'problem- name)))
    `(progn
       (defun ,symbol ,args ,@body)
       (setf (get ',symbol 'rosalind-name) ,(string-downcase name))
       (define-test ,symbol ,sample-input ,sample-output)
       ',symbol)))

(defun read-problem-data (problem)
  (-<> (get problem 'rosalind-name)
    (format nil "~~/Downloads/rosalind_~A.txt" <>)
    read-file-into-string))

(defun solve% (problem)
  (pbcopy (funcall problem (read-problem-data problem))))

(defmacro solve (problem)
  `(solve% ',problem))

