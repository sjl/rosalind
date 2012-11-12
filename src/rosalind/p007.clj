(ns rosalind.p007
  (:require [clojure.string :as s]))

(def sample "0.23 0.31 0.75")

(defn content-probs [gcc]
  (let [gc (/ gcc 2)
        at (/ (- 1 gcc) 2)]
    {:g gc
     :c gc
     :a at
     :t at}))

(defn chance-of-twice [prob]
  (* prob prob))

(defn solve [s]
  (let [gccs (map #(Float/parseFloat %)
                  (s/split (s/trim s) #"\s+"))]
  (println (s/join " "
                   (for [gcc gccs]
                     (reduce + (map chance-of-twice
                                    (vals (content-probs gcc)))))))))

(solve sample)

(solve (slurp "/Users/sjl/Downloads/rosalind_prob.txt")) 
