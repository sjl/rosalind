(ns rosalind.p004
  (:refer-clojure :exclude [char])
  (:require [the.parsatron :refer [defparser many1 always run string digit char
                                   eof let->> token between]]))

(defparser number []
  (let->> [ds (many1 (digit))]
    (always (apply str ds))))

(defparser dna-line []
  (let->> [nts (many1 (token #{\G \T \C \A}))
           _ (char \newline)]
    (always (apply str nts))))

(defparser dna []
  (let->> [ntls (many1 (dna-line))]
    (always (apply str ntls))))

(defparser header []
  (between (string ">Rosalind_") (char \newline)
           (number)))

(defparser chunk []
  (let->> [id (header)
           content (dna)]
    (always [id content])))

(defparser file []
  (let->> [chunks (many1 (chunk))
           _ (eof)
           ]
    (always chunks)))

(defn parse [s]
  (run (file) s))

(def sample
">Rosalind_6404
CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
TCCCACTAATAATTCTGAGG
>Rosalind_5959
CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
ATATCCATTTGTCAGCAGACACGC
>Rosalind_0808
CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
TGGGAACCTGCGGGCAGTAGGTGGAAT
"
)

(defn gc-content [dna]
  (float (/ (count (filter #{\G \C} dna))
            (count dna))))

(defn solve [s]
  (let [[id gcc] (last (sort-by second
                                 (map (juxt first (comp gc-content second))
                                      (parse s))))]
    (println (str "Rosalind_" id))
    (println (str (* 100 gcc) "%"))))

(solve sample)

(solve (slurp "/Users/sjl/Downloads/rosalind_gc.txt"))
