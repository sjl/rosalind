(ns rosalind.p001
  (:require [clojure.string :refer [join]]))

(defn solve [s]
  (println (join " " (map (frequencies s) "ACGT"))))

(solve "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC")

(solve (slurp "/Users/sjl/Downloads/rosalind_dna.txt"))
