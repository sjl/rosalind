(in-package :rosalind)

(defparameter *input-lgis*
  "5
5 1 4 2 3")

(defparameter *output-lgis*
  "1 2 3
5 4 3")


;; There's an nlog(n) algorithm described at Wikipedia:
;; https://en.wikipedia.org/wiki/Longest_increasing_subsequence#Efficient_algorithms
;;
;; Unfortunately it's pretty painful to read because they insist on using some
;; strange programming language that uses indentation for control flow but
;; doesn't have .length on arrays, doesn't have adjustable vectors, etc.  What
;; even is this language?  And of course they use one-letter names for
;; everything, because fuck you.  At least they describe the contents of each
;; auxiliary array precisely.  That's nice.
;;
;; Before we start: let's define the term "tail" to mean the final element of
;; a subsequence.
;;
;; Let's also pretend we're dealing with the < predicate instead of an arbitrary
;; one for this description, just to make things a little easier to talk about.
;;
;; We set up two arrays before we start:
;;
;; * TAIL-INDEXES: An array that contains indexes of tails.  To be more precise:
;;   TAIL-INDEXES[n] contains the index of the *minimum* tail for a subsequence
;;   of length N.
;; * PREDECESSORS: PREDECESSORS[i] stores the index of the predecessor of
;;   SEQ[i] in the resulting subsequence.
;;
;; As an example (entries marked with _ are irrelevant, and we'll use characters
;; as values for clarity):
;;
;;     SEQUENCE     [a, d, c, b]
;;       (indexes)   0  1  2  3
;;
;;     TAIL-INDEXES [_, 0, 2]
;;     PREDECESSORS [NIL, _, _, 0]
;;     RESULT       [a, b]
;;
;; A few things to notice:
;;
;; * TAIL-INDEXES[0] is garbage, because a subsequence of length zero doesn't
;;   *have* a tail.  We could do 1- everywhere to save a word but come on.
;; * TAIL-INDEXES[1] is 0, the index of the tail of the subsequence of length 1 is 0 (a).
;; * TAIL-INDEXES[2] is 3, the index of the tail of the subsequence of length 2 is 3 (b).
;; * TAIL-INDEXES only has elements 0 (garbage), 1, and 2, because the longest
;;   increasing subsequence is only 2 elements long.  We have extendable vectors
;;   in Lisp, let's use them instead of doing all the bookkeeping by hand.
;; * PREDECESSORS[3] is   0, because in the final result the predecessor of SEQ[3] (b) is SEQ[0] (a).
;; * PREDECESSORS[0] is NIL, because in the final result SEQ[0] (a) is the first element.
;;
;; Essentially the algorithm goes like this:
;;
;; * Handle the first element by hand.
;; * Iterate over the sequence:
;;   * Bisect TAIL-INDEXES at each iteration to find where the next element fits.
;;   * Update or extend TAIL-INDEXES with each successive element.
;;   * Extend PREDECESSORS to record the tail index before to the one we just used.
;; * Once we've filled in the arrays, we can walk through PREDECESSORS, starting
;;   at the final tail index.

(defun longest-monotonic-subsequence
    (sequence predicate &key (result-type 'list))
  "Return the longest monotonic subsequence of `sequence`.

  `predicate` must be a comparison predicate like `<` or `>=`.

  If there are multiple longest sequences, an arbitrary one is returned.

  Examples:

    (longest-monotonic-subsequence '()        #'<)            ; => ()
    (longest-monotonic-subsequence '(1)       #'<)            ; => (1)
    (longest-monotonic-subsequence '(2 1)     #'<)            ; => (1) or (2)
    (longest-monotonic-subsequence '(1 2)     #'<)            ; => (1 2)
    (longest-monotonic-subsequence '(2 1)     #'>)            ; => (2 1)
    (longest-monotonic-subsequence '(3 1 1 2) #'<)            ; => (1 2)
    (longest-monotonic-subsequence '(3 1 1 2) #'<=)           ; => (1 1 2)
    (longest-monotonic-subsequence '(3 1 1 2) #'>=)           ; => (3 1 1)
    (longest-monotonic-subsequence \"hello, world!\" #'char<
                                   :result-type 'string)
    ; => \"helor\"

  "
  (let* ((sequence (coerce sequence 'vector))
         (n (length sequence))
         (tail-indexes (make-array (1+ n) :fill-pointer 1 :initial-element nil))
         (predecessors (make-array n :fill-pointer 0)))
    (coerce
      (if (zerop n)
        (list) ; just bail early on this edge case
        (progn
          ;; Element 0 is always the first tail.
          (vector-push-extend 0 tail-indexes)
          (vector-push-extend nil predecessors)
          (iterate
            (for value :in-vector sequence :with-index i :from 1)
            (for (values nil tail-index) =
                 (bisect-right predicate tail-indexes value
                               :start 1 ; ignore the garbage
                               :key (curry #'aref sequence))) ; deref when bisecting
            (if tail-index
              (progn
                ;; Found a more minimal tail for existing subseq
                (setf (aref tail-indexes tail-index) i)
                (vector-push-extend (aref tail-indexes (1- tail-index))
                                    predecessors))
              (progn
                ;; Found the largest tail so far, extend our subseqs
                (vector-push-extend (vector-last tail-indexes) predecessors)
                (vector-push-extend i tail-indexes))))
          (iterate
            (for i :first (vector-last tail-indexes) :then (aref predecessors i))
            (while i)
            (collect (aref sequence i) :at :beginning))))
      result-type)))


(define-problem lgis (data stream)
    *input-lgis*
    *output-lgis*
  (let* ((size (read data))
         (elements (gimme size (read data))))
    (with-output-to-string (s)
      (format s "~{~D~^ ~}~%" (longest-monotonic-subsequence elements #'<))
      (format s "~{~D~^ ~}" (longest-monotonic-subsequence elements #'>)))))
