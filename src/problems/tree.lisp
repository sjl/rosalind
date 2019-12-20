(in-package :rosalind)

;; For every edge we add we can link up two previously-unconnected trees.  If we
;; have N separate trees, we just need N-1 edges to connect them.  So the
;; problem reduces itself down to just counting the distinct subgraphs of the
;; given graph.  Some day I really need to add that as a utility function to
;; cl-digraph.

(defparameter *input-tree*
  "10
1 2
2 8
4 10
5 9
6 10
7 9")

(defparameter *output-tree*
  "3")

(defun subgraph-vertices (graph start)
  (gathering (digraph:map-depth-first #'gather graph start)))

(defun subgraphs (graph)
  "Return a list of lists of vertices of the distinct subgraphs of `graph`."
  (let ((graph (digraph:copy-digraph graph)))
    (iterate
      (for (values vertex found) = (digraph:arbitrary-vertex graph))
      (while found)
      (for subgraph = (subgraph-vertices graph vertex))
      (collect subgraph)
      (map nil (curry #'digraph:remove-vertex graph) subgraph))))

(define-problem tree (data stream)
    *input-tree*
    *output-tree*
  (let ((graph (digraph:make-digraph
                 :initial-vertices (alexandria:iota (read data) :start 1))))
    (iterate
      (for line :in-stream data :using #'read-line)
      (for (a b) = (read-all-from-string line))
      (digraph:insert-edge graph a b)
      (digraph:insert-edge graph b a))
    (1- (length (subgraphs graph)))))


#; Scratch --------------------------------------------------------------------

(problem-tree)
