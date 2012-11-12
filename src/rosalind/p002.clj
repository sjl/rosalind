(ns rosalind.p002
  (:refer-clojure :exclude [replace])
  (:require [clojure.string :refer [replace]]))


(defn solve [s]
  (replace s "T" "U"))

(solve "GATGGAACTTGACTACGTAAATT")

(solve (slurp "/Users/sjl/Downloads/rosalind_rna.txt"))
