(ns rosalind.p005
  (:require [clojure.string :refer [split-lines trim]]))

(defn hamming [s t]
  (count (filter identity (map not= s t))))

(def sample
"GAGCCTACTAACGGGAT
CATCGTAATGACGGCCT")

(defn solve [s]
  (println (apply hamming
                  (-> s trim split-lines))))

(solve sample)

(print (solve (slurp "/Users/sjl/Downloads/rosalind_hamm.txt"))) 
