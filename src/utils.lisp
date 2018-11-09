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


(defun ensure-stream (input)
  (ctypecase input
    (stream input)
    (string (make-string-input-stream input))))

(defun ensure-string (input)
  (ctypecase input
    (stream (alexandria:read-stream-content-into-string input))
    (string (copy-seq input))))


(defun hamming (sequence1 sequence2 &key (test #'eql))
  "Return the Hamming distance between `sequence1` and `sequence2`."
  ;; todo assert length=?
  (let ((result 0))
    (map nil (lambda (x y)
               (unless (funcall test x y)
                 (incf result)))
         sequence1
         sequence2)
    result))


(defun factorial (x)
  (check-type x (integer 0))
  (iterate (for i :from 1 :to x)
           (multiplying i)))

(defun permutations (items)
  (gathering (alexandria:map-permutations #'gather items)))


;;;; Iterate ------------------------------------------------------------------
(defmacro-driver (FOR var SEED seed THEN then)
  "Bind `var` to `seed` initially, then to `then` on every iteration.

  This differs from `(FOR … FIRST … THEN …)` and `(FOR … INITIALLY … THEN …)`
  because `then` is evaluated on every iteration, *including* the first.

  Example:

    (iterate
      (repeat 3)
      (for x :first     0 :then (1+ x))
      (for y :initially 0 :then (1+ y))
      (for z :seed      0 :then (1+ z))
      (collect (list x y z)))
    ; =>
    ((0 0 1)
     (1 1 2)
     (2 2 3))

  "
  (let ((kwd (if generate 'generate 'for)))
    `(progn
       (,kwd ,var :next ,then)
       (initially (setf ,var ,seed)))))

(defmacro-clause (RETURNING-FINAL form)
  "Evaluate `form` each iteration and return its final value from the `iterate`.

  Example:

    (iterate
      (for i :from 1 :to 4)
      (collect (returning-final i) :into l)
      (print l))
    ; =>
    (1)
    (1 2)
    (1 2 3)
    (1 2 3 4)
    4

  "
  (with-gensyms (result)
    `(progn
       (with ,result)
       (finally (return ,result))
       (setf ,result ,form))))


;;;; Buffers ------------------------------------------------------------------
(defun make-buffer (&key initial-contents
                         (element-type t)
                         (initial-capacity (max 64 (length initial-contents))))
  (let ((buffer (make-array initial-capacity
                  :element-type element-type
                  :adjustable t
                  :fill-pointer (length initial-contents))))
    (when initial-contents
      (replace buffer initial-contents))
    buffer))

(defun make-string-buffer
    (&key initial-contents
          (initial-capacity (max 64 (length initial-contents))))
  (make-buffer :initial-contents initial-contents
               :initial-capacity initial-capacity
               :element-type 'character))

(defun buffer-push (buffer element)
  (vector-push-extend element buffer)
  element)

(defun buffer-append (buffer sequence)
  (let* ((l1 (length buffer))
         (l2 (length sequence))
         (needed (+ l1 l2)))
    (when (< (array-total-size buffer) needed)
      (adjust-array buffer (max needed (* l1 2))))
    (setf (fill-pointer buffer) needed)
    (replace buffer sequence :start1 l1)
    sequence))

(defmacro-clause (BUFFERING expr &optional
                  APPEND (append nil)
                  INTO (var iterate::*result-var*)
                  INITIAL-CONTENTS (initial-contents '())
                  ELEMENT-TYPE (element-type t))
  `(progn
     (with ,var = (make-buffer :initial-contents ,initial-contents
                               :element-type ,element-type))
     (,(if append 'buffer-append 'buffer-push) ,var ,expr)))


;;;; File Formats -------------------------------------------------------------
(defun read-fasta (stream)
  "Read and return the next FASTA label/data pair from `stream`.

  `(values label data)` will be returned for each label/data pair.  All the
  lines of FASTA data for a given label will be concatenated and returned as
  a single buffer.

  `(values nil nil)` will be returned if there is no remaining data.

  "
  (iterate
    (with label = nil)
    (case (peek-char nil stream nil :eof)
      (:eof (finish))
      (#\Newline (read-char stream))
      (#\> (if label
             (finish)
             (setf label (subseq (read-line stream) 1))))
      (t (buffering (read-line stream) :into data
                    :append t
                    :element-type 'character)))
    (finally (return (values label data)))))

(defmacro-driver (FOR vars IN-FASTA source)
  "Iterate over label/data pairs from the FASTA data in `source`.

  `vars` must be a list of two symbols that will be bound to the label and data,
  respectively, on each iteration.

  `stream` can be either a string or a character input stream.

  `generate` is supported.

  Example:

    (iterate
      (with data = (remove #\\space \">foo
                                    CATG
                                    GGAA
                                    >bar
                                    CCCTTG
                                    >baz
                                    >frob\"))
      (for (label dna) :in-fasta data)
      (collect (list label dna)))
    ; =>
    ((\"foo\" \"CATGGGAA\")
     (\"bar\" \"CCCTTG\")
     (\"baz\" \"\")
     (\"frob\" \"\"))

  "
  (destructuring-bind (label data) vars
    (with-gensyms (stream)
      (let ((kwd (if generate 'generate 'for)))
        `(progn
           (with ,stream = (ensure-stream ,source))
           (,kwd (values ,label ,data) :next (multiple-value-bind (l d)
                                                 (read-fasta ,stream)
                                               (if l
                                                 (values l d)
                                                 (terminate)))))))))

(defun read-fasta-into-hash-table (source)
  "Return everything in the FASTA `source` as a hash table of labels to data."
  (iterate (for (label data) :in-fasta source)
           (collect-hash (label data) :test #'equal)))

(defun read-fasta-into-alist (source)
  "Return everything in the FASTA `source` as an alist of labels to data."
  (iterate (for (label data) :in-fasta source)
           (collect (cons label data))))


;;;; Uniprot ------------------------------------------------------------------
(defvar *uniprot-cache* (make-hash-table :test #'equal))

(defmacro get-cached (key cache expr)
  (once-only (key cache)
    (with-gensyms (value)
      `(if-found (,value (gethash ,key ,cache))
         ,value
         (setf (gethash ,key ,cache) ,expr)))))

(defun uniprot-url (id)
  (format nil "http://www.uniprot.org/uniprot/~A.fasta" id))

(defun uniprot (id)
  (get-cached id *uniprot-cache*
              (-<> (uniprot-url id)
                drakma:http-request
                read-fasta-into-alist
                first)))


;;;; Testing ------------------------------------------------------------------
(defmacro define-test (problem input output &optional (test 'string=))
  `(test ,(symb 'test- problem)
     (is (,test ,output (aesthetic-string (,problem ,input))))))

(defun run-tests ()
  (1am:run))


;;;; Problems -----------------------------------------------------------------
(defmacro define-problem (name (arg type) sample-input sample-output &body body)
  (multiple-value-bind (body declarations docstring)
      (alexandria:parse-body body :documentation t)
    (let ((symbol (symb 'problem- name)))
      `(progn
         (defun ,symbol (&optional (,arg ,sample-input))
           ,@(when docstring (list docstring))
           ,@declarations
           (setf ,arg ,(ecase type
                         (string `(ensure-string ,arg))
                         (stream `(ensure-stream ,arg))))
           (progn ,@body))
         (setf (get ',symbol 'rosalind-name) ,(string-downcase name))
         (define-test ,symbol ,sample-input ,sample-output)
         ',symbol))))

(defun problem-data-path (problem)
  (format nil "~~/Downloads/rosalind_~A.txt" (get problem 'rosalind-name)))

(defun solve% (problem)
  (with-open-file (input (problem-data-path problem))
    (pbcopy (aesthetic-string (funcall problem input)))))

(defmacro solve (name)
  `(solve% ',(symb 'problem- name)))

