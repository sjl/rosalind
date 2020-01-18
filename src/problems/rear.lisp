(in-package :rosalind)

(defparameter *input-rear* "1 2 3 4 5 6 7 8 9 10
3 1 5 2 7 4 9 6 10 8

3 10 8 2 5 4 7 1 6 9
5 2 3 1 7 4 10 8 6 9

8 6 7 9 4 1 3 10 2 5
8 2 7 6 9 1 5 3 10 4

3 9 10 4 1 8 6 7 5 2
2 9 8 5 1 7 3 4 6 10

1 2 3 4 5 6 7 8 9 10
1 2 3 4 5 6 7 8 9 10")

(defparameter *output-rear* "9 4 5 7 0")

(defun bounds-list (start end &key (min-length 0))
  "Return a list of all subseq bounds between `start` and `end`."
  (iterate outer
    (for s :from start :below end)
    (iterate (for e :from (+ s min-length) :to end)
      (in outer (collect (cons s e))))))

(defun nreverse-vector (vector &key start end)
  (iterate (for i :from start)
           (for j :downfrom (1- end))
           (repeat (floor (- end start) 2))
           (rotatef (aref vector i)
                    (aref vector j)))
  vector)

(defun reverse-vector (vector &key start end)
  (nreverse-vector (copy-seq vector) :start start :end end))

(defun reversals (vector &key start end)
  (iterate (for (s . e) :in (bounds-list start end :min-length 2))
           (collect (reverse-vector vector :start s :end e))))

(defun reversals-required (from to)
  (iterate
    (with remaining = (make-queue)) ; queue of (score . state)
    (initially (enqueue (cons 0 from) remaining))
    (for (n . v) = (dequeue remaining))
    (for start = (mismatch v to))
    (for end = (mismatch v to :from-end t))
    (when (null start)
      (return n))
    (incf n)
    (dolist (r (reversals v :start start :end end))
      (enqueue (cons n r) remaining))))

;; todo: finish this one
;; (define-problem rear (data string)
;;     *input-rear*
;;     *output-rear*
;;   (let ((pairs (-<> data
;;                  (str:split (format nil "~2%") <>)
;;                  (mapcar (curry #'str:split #\newline) <>)
;;                  (mapcar (curry #'mapcar #'read-all-from-string) <>)
;;                  (mapcar (curry #'mapcar (rcurry #'coerce 'vector)) <>))))
;;     (iterate (for (from to) :in pairs)
;;              (collect (time (reversals-required from to))))))


