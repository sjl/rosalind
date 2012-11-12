(ns rosalind.p003
  (:require [clojure.string :refer [join]]))

(def nucleotide-complement
  {\A \T
   \C \G
   \T \A
   \G \C})


(defn solve [s]
  (join "" (map nucleotide-complement (reverse s))))

(solve "AAAACCCGGT")

(print (solve (slurp "/Users/sjl/Downloads/rosalind_revc.txt"))) 
