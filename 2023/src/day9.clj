(ns day9
  (:require [clojure.string :as str]
            [instaparse.core :as insta]))

(def parse
  (insta/parser 
   "<S> = line*
    line = (d <hspace*>)+ <vspace+>
    d = '-'?#'[0-9]+'
    hspace = (' '|'\t')
    vspace = ('\n'|'\r')"))

(def cst->ast
  (letfn [(lf [l] (map vf (rest l)))
          (vf [v] (->> v rest str/join Integer/parseInt))]
    (partial map lf)))

(comment
  (->> (slurp "data/day9.txt")
       parse
       cst->ast))

(defn fwd-diff [xs]
  (->> xs
       (partition 2 1)
       (mapv (fn [[a b]] (- b a)))))

(defn tableau
  "create a tableau of nonzero forward differences"
  [xs]
  (->> xs
       (iterate fwd-diff)
       (take-while (every-pred not-empty (partial not-every? #{0})))))

(defn extrapolate-fwd 
  "extrapolate next in sequence via polynomial interpolation, O(n^2)"
  [xs]
  (->> xs
       tableau
       (map last)
       (reduce + 0)))

(defn extrapolate-rev 
  "extrapolate previous in sequence via polynomial interpolation, O(n^2)"
  [xs]
  (->> xs
       tableau
       (map first)
       (map * (cycle [1 -1]))
       (reduce + 0)))

(defn -main [input-path]
  (let [ast (-> input-path slurp parse cst->ast)]
    (println (->> ast (map extrapolate-fwd) (reduce + 0)))
    (println (->> ast (map extrapolate-rev) (reduce + 0)))))

(comment
  (-main "data/day9.txt")
  #_())
