(in-package :rosalind/utils)

;;;; Misc ---------------------------------------------------------------------
(defun ensure-stream (input)
  (ctypecase input
    (stream input)
    (string (make-string-input-stream input))))

(defun ensure-string (input)
  (ctypecase input
    (stream (alexandria:read-stream-content-into-string input))
    (string (copy-seq input))))


(defun permutations (items)
  (gathering (alexandria:map-permutations #'gather items)))


(defun-inline gcp (base)
  "Return whether `base` is G or C."
  (or (char= #\G base)
      (char= #\C base)))

(defun-inline base-probability (gc-content base)
  "Return the probability of `base` in DNA with the given `gc-content`."
  (if (gcp base)
    (/ gc-content 2)
    (/ (- 1 gc-content) 2)))

(defun sequence-probability (gc-content sequence)
  "Return the probability of seeing `sequence` when generating a random sequence with the given `gc-content`."
  (product sequence :key (curry #'base-probability gc-content)))


(defun mapcount (predicate sequence &rest more-sequences)
  "Map `predicate` across sequences, counting satisfactory applications."
  (let ((result 0))
    (apply #'map nil
           (lambda (&rest args)
             (when (apply predicate args)
               (incf result)))
           sequence more-sequences)
    result))


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


;;;; Dogma --------------------------------------------------------------------
(defun dna-complement (base)
  (ecase base
    (#\A #\T)
    (#\T #\A)
    (#\G #\C)
    (#\C #\G)))

(defun nreverse-complement (dna)
  (map-into dna #'dna-complement dna)
  (nreverse dna))

(defun reverse-complement (dna)
  (nreverse-complement (copy-seq dna)))

(defun transcribe (dna)
  "Transcribe a fresh RNA string from `DNA`."
  (substitute #\U #\T dna))

(defun ntranscribe (dna)
  "Destructively transcribe `DNA` to RNA in-place."
  (nsubstitute #\U #\T dna))

(defmacro codon-case ((vector index) &rest clauses)
  ;; Compiles a giant list of clauses into a tree of ECASEs.
  ;;
  ;; Each codon will have at most 3 ECASEs to pass through.  Each ECASE has at
  ;; most four options, so in the worst case we end up with 3 * 4 = 12
  ;; comparisons instead of 64.
  ;;
  ;; If we ever convert bases to vectors of (unsigned-byte 2)s we could
  ;; potentially use a lookup table here, e.g.:
  ;;
  ;;     (aref +amino-acids+ (+ x (ash y 2) (ash z 4)))
  (alexandria:once-only (vector index)
    (alexandria:with-gensyms (x y z)
      `(let ((,x (aref ,vector ,index))
             (,y (aref ,vector (+ ,index 1)))
             (,z (aref ,vector (+ ,index 2))))
         ,(labels ((strip (clauses)
                     (if (= 1 (length (caar clauses)))
                       (cadar clauses)
                       (iterate (for (head body) :in clauses)
                                (collect (list (subseq head 1) body)))))
                   (split (clauses)
                     (_ clauses
                       (group-by (rcurry #'aref 0) _ :key #'first)
                       (iterate (for (k v) :in-hashtable _)
                                (collect (list k (strip v)))))))
            (recursively ((clauses (split clauses))
                          (codons (list x y z))
                          (i 0))
              `(ecase ,(first codons)
                 ,@(iterate (for (k remaining) :in clauses)
                            (collect `(,k ,(if (atom remaining)
                                             remaining
                                             (recur (split remaining)
                                                    (rest codons)
                                                    (1+ i)))))))))))))

(defun codon-to-protein (vector index)
  "Return the amino acid encoded by the codon in `vector` at `index`."
  (codon-case (vector index)
    ("UUU" #\F) ("CUU" #\L) ("AUU" #\I) ("GUU" #\V)
    ("UUC" #\F) ("CUC" #\L) ("AUC" #\I) ("GUC" #\V)
    ("UUA" #\L) ("CUA" #\L) ("AUA" #\I) ("GUA" #\V)
    ("UUG" #\L) ("CUG" #\L) ("AUG" #\M) ("GUG" #\V)
    ("UCU" #\S) ("CCU" #\P) ("ACU" #\T) ("GCU" #\A)
    ("UCC" #\S) ("CCC" #\P) ("ACC" #\T) ("GCC" #\A)
    ("UCA" #\S) ("CCA" #\P) ("ACA" #\T) ("GCA" #\A)
    ("UCG" #\S) ("CCG" #\P) ("ACG" #\T) ("GCG" #\A)
    ("UAU" #\Y) ("CAU" #\H) ("AAU" #\N) ("GAU" #\D)
    ("UAC" #\Y) ("CAC" #\H) ("AAC" #\N) ("GAC" #\D)
    ("UAA" nil) ("CAA" #\Q) ("AAA" #\K) ("GAA" #\E)
    ("UAG" nil) ("CAG" #\Q) ("AAG" #\K) ("GAG" #\E)
    ("UGU" #\C) ("CGU" #\R) ("AGU" #\S) ("GGU" #\G)
    ("UGC" #\C) ("CGC" #\R) ("AGC" #\S) ("GGC" #\G)
    ("UGA" nil) ("CGA" #\R) ("AGA" #\R) ("GGA" #\G)
    ("UGG" #\W) ("CGG" #\R) ("AGG" #\R) ("GGG" #\G)))

(defun translate (rna &key (start 0))
  "Translate a string of RNA bases into a protein string of amino acids.

  `rna` will be searched (beginning at `start`) for a start codon and
  translation will proceed from there.  If no start codon occurs after `start`
  then `nil` will be returned.

  Once a start codon has been found, translation proceeds to the next stop
  codon.  If no stop codon is present, `nil` will be returned.

  Otherwise two values are returned: the protein string and the index into `rna`
  where it started.

  "
  (when-let ((start (search "AUG" rna :start2 start)))
    (values
      (iterate (with limit = (- (length rna) 3))
               (for i :from start :by 3)
               (when (> i limit)
                 (return-from translate (values nil nil)))
               (for protein = (codon-to-protein rna i))
               (while protein)
               (collect protein :result-type 'string))
      start)))


;;;; Strings ------------------------------------------------------------------
(defun string-empty-p (string)
  (zerop (length string)))

(defun first-char (string)
  (if (string-empty-p string)
    nil
    (char string 0)))

(defun strjoin (sep seq)
  (iterate (for el :in-whatever seq)
           (concatenating (if (stringp el) el (aesthetic-string el))
                          :separator sep)))


;;;; Math ---------------------------------------------------------------------
(defun factorial (x)
  (check-type x (integer 0))
  (iterate (for i :from 1 :to x)
           (multiplying i)))


(defmacro do-sum ((var from to &key (initial-value 0) modulo) &body body)
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
    (with-gensyms (result mod)
      `(do ((,var ,from (1+ ,var))
            ,@(when modulo `((,mod ,modulo)))
            (,result ,initial-value))
         ((> ,var ,to) ,result)
         ,(if modulo
            `(setf ,result (mod (+ ,result (progn ,@body)) ,mod))
            `(incf ,result (progn ,@body)))))))

(defmacro do-product ((var from to &key (initial-value 1) modulo) &body body)
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
    (with-gensyms (result mod)
      `(do ((,var ,from (1+ ,var))
            ,@(when modulo `((,mod ,modulo)))
            (,result ,initial-value))
         ((> ,var ,to) ,result)
         (setf ,result
               ,(if modulo
                  `(mod (* ,result (progn ,@body)) ,mod)
                  `(* ,result (progn ,@body))))))))


(defmacro Σ (bindings &body body) ;; lol
  `(do-sum ,bindings ,@body))

(defmacro Π (bindings &body body) ;; lol
  `(do-product ,bindings ,@body))


(defun binomial-coefficient (n k)
  "Return `n` choose `k`."
  ;; https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
  (Π (i 1 k)
    (/ (- (1+ n) i) i)))


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

(defmacro-clause (SUMMING* form &optional INTO var MODULO divisor INITIAL-VALUE n)
  "Like vanilla `summing`, but with more options."
  (let ((result (or var iterate::*result-var*)))
    (with-gensyms (mod)
      `(progn
         (with ,result = ,(or n 0))
         ,@(if divisor
             `((with ,mod = ,divisor)
               (setf ,result (mod (+ ,result ,form) ,mod)))
             `((incf ,result ,form)))))))

(defmacro-clause (MULTIPLYING* form &optional INTO var MODULO divisor INITIAL-VALUE n)
  "Like vanilla `multiplying`, but using `*` and with more options."
  (let ((result (or var iterate::*result-var*)))
    (with-gensyms (mod)
      `(progn
         (with ,result = ,(or n 1))
         ,@(if divisor
             `((with ,mod = ,divisor)
               (setf ,result (mod (* ,result ,form) ,mod)))
             `((setf ,result (* ,result ,form))))))))


;;;; Readers ------------------------------------------------------------------
(defun read-lines (stream)
  "Read all lines from `stream` and return them as a fresh list of strings."
  (iterate (for line :in-stream stream :using #'read-line)
           (collect line)))


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

  `source` can be either a string or a character input stream.

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
(defparameter *cache-dir*
  (_ (or (uiop:getenv "XDG_CACHE_HOME")
         (format nil "~A/.cache" (uiop:getenv "HOME")))
    (string-right-trim "/" _)
    (format nil "~A/sjl-rosalind/" _)))

(defun cache-path (key)
  (concatenate 'string *cache-dir* key))

(defun cachedp (key)
  (probe-file (cache-path key)))

(defun getcache (key)
  (with-open-file (s (cache-path key))
    (read s)))

(defun setcache (key value)
  (ensure-directories-exist *cache-dir*)
  (let ((*print-readably* t))
    (with-open-file (s (cache-path key) :direction :output)
      (print value s)))
  value)

(defmacro ensure-cached (key expr)
  (once-only (key)
    `(if (cachedp ,key)
       (getcache ,key)
       (setcache ,key ,expr))))

(defun uniprot-url (id)
  (format nil "http://www.uniprot.org/uniprot/~A.fasta" id))

(defun uniprot (id)
  (ensure-cached id (_ (uniprot-url id)
                      drakma:http-request
                      read-fasta-into-alist
                      first)))


;;;; Output -------------------------------------------------------------------
(defun float-string (float-or-floats &optional (places 3))
  "Return a string of `float-or-floats` in the format Rosalind wants.

  `float-or-floats` can be a number, list, vector, or 2D array.

  Each float will be printed with `places` digits after the decimal point.

  If a list or vector is given, the floats will be separated by a space.

  If a two-dimensional array is given, the rows will be separated by newlines.

  "
  (let ((precision (expt 10.0d0 places)))
    (with-output-to-string (s)
      ;; Rosalind's examples use half-up style rounding.  I think their answer
      ;; checking will tolerate half-even, but in order to get consistent test
      ;; output we need consistent rounding.  Unfortunately format's ~F allows
      ;; an implementation-specific rounding strategy, so we have to hack this
      ;; in ourselves, and we may as well match Rosalind.  Sigh.
      (labels ((round-half-up (float)
                 (_ float
                   (* _ precision)
                   (+ _ 0.5d0)
                   floor
                   (/ _ precision)))
               (p (float &optional space)
                 (format s "~,VF~:[~; ~]" places (round-half-up float) space)))
        (etypecase float-or-floats
          ((or number list)
           (loop :for (float . more) :on (alexandria:ensure-list float-or-floats)
                 :do (p float more)))
          ((array * (*))
           (loop :with last = (1- (length float-or-floats))
                 :for i :from 0
                 :for f :across float-or-floats
                 :do (p f (< i last))))
          ((array * (* *))
           (destructuring-bind (rows cols) (array-dimensions float-or-floats)
             (dotimes (r rows)
               (dotimes (c cols)
                 (p (aref float-or-floats r c) (< c (1- cols))))
               (when (< r (1- rows))
                 (terpri s))))))))))


;;;; Testing ------------------------------------------------------------------
(defmacro define-test (problem input output &optional (test 'string=))
  `(1am:test ,(symbolicate 'test- problem)
     (1am:is (,test ,output (aesthetic-string (,problem ,input))))))

(defun run-tests ()
  (1am:run))


;;;; Problems -----------------------------------------------------------------
(defmacro define-problem (name (arg type) sample-input sample-output &body body)
  (multiple-value-bind (body declarations docstring)
      (alexandria:parse-body body :documentation t)
    (let ((symbol (symbolicate 'problem- name)))
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
    (let ((result (aesthetic-string (funcall problem input))))
      (pbcopy result)
      (format t "Copied the following to clipboard:~%~A~%" result)
      (values))))

(defmacro solve (name)
  (assert (symbolp name) ()
    "Usage: (solve foo)~%foo should not be quoted.")
  `(solve% ',(symbolicate 'problem- name)))

