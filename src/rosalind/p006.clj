(ns rosalind.p006
  (:require [clojure.math.combinatorics :refer [permutations]]
            [clojure.string :refer [join trim]]))

(def sample "3")

(defn solve [s]
  (let [ps (permutations (range 1 (inc (Long/parseLong (trim s)))))]
    (println (count ps))
    (dorun (map #(println (join " " %)) ps))))

(solve sample)

(solve (slurp "/Users/sjl/Downloads/rosalind_perm.txt")) 
