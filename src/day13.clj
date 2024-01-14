(ns day13
  (:require [clojure.string :as str]))

(def transpose (partial apply (partial map vector)))

(def sep :|) ; could use gensym instead, as long as it doesn't show up in grids 

(def palindromes ; Manacher's algorithm, returns vec of 2(count xs)+1 of palindrome lengths
  (letfn
   [(preprocess [%] (conj (->> % (interpose sep) (cons sep) vec) sep))
    (inner-loop [i r rs i0 r0]
      (if (<= i (+ i0 r0))
        (let [mi (- (* 2 i0) i)
              max-mr (- (+ i0 r0) i)]
          (cond
            (< (get rs mi) max-mr) (recur (inc i) r (assoc! rs i (get rs mi)) i0 r0)
            (> (get rs mi) max-mr) (recur (inc i) r (assoc! rs i max-mr) i0 r0)
            :else [i max-mr rs]))
        [i r rs]))]
    (fn [xs]
      (let [xs' (preprocess xs)]
        (loop [i 0 ; center of palindrome
               r 0 ; radius of palindrome, raw length is 2r, actual is r
               rs (-> xs' count (repeat 0) vec transient)]
          (if (< i (count rs))
            (let [r' (->> (range r (- (count rs) i))
                          (take-while #(= (get xs' (- i %)) (get xs' (+ i %))))
                          last)
                  [i' r'' rs'] (inner-loop (inc i) 0 (assoc! rs i r') i r')]
              (recur i' r'' rs'))
            (persistent! rs)))))))

(defn solve-rows [rows]
  (->> rows
       palindromes
       (take-nth 2)
       (map-indexed vector)
       butlast
       (filter (fn [[i w]] ; check whether against either edge
                 (let [r (quot w 2)]
                   (or (= i r)
                       (= i (- (count rows) r))))))
       (apply max-key second)
       first))

(defn solve-grid [grid]
  (+ (* 100 (solve-rows grid)) (solve-rows (transpose grid))))

(defn solve [input-path]
  (let [grids (->> input-path
                   slurp
                   str/split-lines
                   (partition-by empty?)
                   (take-nth 2))]
    (println (->> grids (pmap solve-grid) (reduce + 0) time))))

(defn -main [& args]
  (apply solve args)
  (shutdown-agents))

(comment
  (solve "data/day13.txt")
  #_())
