(in-package :rosalind)

(defparameter *input-trie*
  "ATAGA
ATC
GAT")

(defparameter *output-trie*
  "1 2 A
2 3 T
3 4 A
4 5 G
5 6 A
3 7 C
1 8 G
8 9 A
9 10 T")


;;;; Data Structure -----------------------------------------------------------
(defstruct trie-node terminal children)

(defun make-trie (strings)
  (recursively ((strings strings))
    (let ((terminal (find-if #'string-empty-p strings))
          (strings (remove-if #'string-empty-p strings)))
      (make-trie-node
        :terminal (if terminal t nil)
        :children (iterate
                    (for (ch kids) :in-hashtable (group-by #'first-char strings))
                    (collect-hash (ch (recur (mapcar (lambda (s) (subseq s 1))
                                                     kids)))))))))

(defun trie-child (trie character)
  (gethash character (trie-node-children trie)))

(defun trie-contains-p (trie string)
  (iterate
    (for ch :in-string string)
    (setf trie (trie-child trie ch))
    (when (null trie)
      (return nil))
    (finally (return (trie-node-terminal trie)))))


;;;; Graphviz -----------------------------------------------------------------
(defmethod cl-dot:graph-object-node ((graph (eql 'trie)) (node trie-node))
  (make-instance 'cl-dot:node
    :attributes (if (trie-node-terminal node)
                  '(:shape :star :width 0.3 :height 0.3 :label "" :style :filled :fillcolor "#FF66CC")
                  '(:shape :circle :width 0.2 :height 0.2 :label ""))))

(defmethod cl-dot:graph-object-points-to ((graph (eql 'trie)) (node trie-node))
  (iterate
    (for (ch child) :in-hashtable (trie-node-children node))
    (collect (make-instance 'cl-dot:attributed
               :object child
               :attributes `(:label ,(format nil " ~C " ch))))))

(defun dot-graph (graph-type root &key (rankdir :lr))
  (cl-dot:dot-graph
    (cl-dot:generate-graph-from-roots
      graph-type (list root)
      `(:rankdir ,(string-upcase rankdir)))
    "out.png" :format :png))


;;;; Problem ------------------------------------------------------------------
(defun trie-adjacency-list (root)
  (gathering
    (let ((i 0)
          (numbers (make-hash-table)))
      (flet ((n (node)
               (alexandria:ensure-gethash node numbers (incf i))))
        (recursively ((node root))
          (iterate
            (for (ch child) :in-hashtable (trie-node-children node))
            (gather (list (n node) (n child) ch))
            (recur child)))))))

(define-problem trie (data stream)
    *input-trie*
    *output-trie*
  (let* ((strings (read-lines data))
         (trie (make-trie strings)))
    ;; (dot-graph 'trie trie :rankdir :tb)
    (format nil "~{~{~A~^ ~}~^~%~}" (trie-adjacency-list trie))))


#; Scratch --------------------------------------------------------------------

(problem-trie)

(problem-trie
  "apple
apropos
banana
bandana
orange")

(problem-trie
  "art
artificial
artistic")
