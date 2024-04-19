(ns day11
  (:require [clojure.string :as str]))

(defn indices-of2 [pred xss]
  (apply concat
         (keep-indexed
          (fn [ri row]
            (-> #(when (pred %2) %1)
                (keep-indexed row)
                (->> (map (partial vector ri)))))
          xss)))

(def galaxy-chars #{\#})

(def transpose (partial apply (partial map vector)))

(defn total-dists-with-factor [fac coords]
  (letfn [(rf [[dist ct ddist y] x freq]
            ; sum of distance, galaxies seen, last gap-cost, last position
            (let [gap (inc (* fac (dec (- x y))))
                  connect-cost (+ ddist (* ct gap))]
              [(+ dist (* freq connect-cost)) (+ ct freq) connect-cost x]))]
    (->> coords
         transpose
         (map #(->> %
                    frequencies
                    (into (sorted-map))
                    (reduce-kv rf [0 0 0 0])
                    first))
         (reduce + 0))))

(defn -main [input-path]
  (let [grid (->> input-path slurp str/split-lines (mapv vec))
        galaxy-coords (indices-of2 galaxy-chars grid)]
    (println (total-dists-with-factor 2 galaxy-coords))
    (println (total-dists-with-factor 1000000 galaxy-coords))))

(comment
  (-main "data/day11.txt")
  #_())
