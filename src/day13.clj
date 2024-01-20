(ns day13
  (:require [clojure.string :as str]))

(def transpose (partial apply (partial map vector)))

(defn transient2 [xss] (transient (mapv transient xss)))

(defn persistent2! [xss] (mapv persistent! (persistent! xss)))

(def sep :|) ; could use gensym instead, as long as it doesn't show up in grids 

(defn words-equiv [u v]
  (empty? (drop 1 (filter not (map = u v)))))

(comment
  (words-equiv "afff" "asdf")
  #_())

; Manacher's algorithm, xs => vec of 2(count xs)+1 of palindrome lengths
(defn palindromes [xs]
  (let [xs (conj (->> xs (interpose sep) (cons sep) vec) sep)]
    (loop [rs (-> xs count (repeat 0) vec transient)
           i 0 ; center of palindrome
           r 0] ; radius of palindrome, raw length is 2r, actual is r
      (if (< i (count rs))
        (let [r (->> (range r (- (count rs) i))
                    ;;  (reduce
                    ;;   (fn [[l c] r]
                    ;;     [(conj l r)
                    ;;      (if (= (get xs (- i r)) (get xs (+ i r))) c (inc c))])
                    ;;   [[] 0])
                    ;;  (take-while #(<= (second %) 1))
                     (take-while #(= (get xs (- i %)) (get xs (+ i %))))
                     last)
              [rs i r]
              (loop [rs (assoc! rs i r) i' (inc i) r' 0]
                (if (<= i' (+ i r))
                  (let [mi (- (* 2 i) i')
                        max-mr (- (+ i r) i')]
                    (cond
                      (< (get rs mi) max-mr) (recur (assoc! rs i' (get rs mi)) (inc i') r')
                      (> (get rs mi) max-mr) (recur (assoc! rs i' max-mr) (inc i') r')
                      :else [rs i' max-mr]))
                  [rs i' r']))]
          (recur rs i r))
        (persistent! rs)))))

(comment
  (->> "data/day13.txt"
       slurp
       str/split-lines
       (partition-by empty?)
       (take-nth 2)
       (map palindromes))
  #_())

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

;; (defn some-differ-by-one [words]
;;   (if (>= (count words) 2)
;;     (let [first-half (update-vals (group-by #(subvec % 0 (quot (count %) 2)) words) set)
;;           second-half (update-vals (group-by #(subvec % (quot (count %) 2)) words) set)]
;;       (map first-half))
;;     nil))

(comment
  (solve "data/day13.txt")
  ;; build a suffix tree
  (let [grids (->> "data/day13.txt"
                   slurp
                   str/split-lines
                   (partition-by empty?)
                   (take-nth 2))

        ;; suffix-tree
        ;; (fn [tree x]
        ;;   )
        ;; palindromes-with-mistakes
        ;; (fn [mistake-limit xs]
        ;;   (let [xs (conj (->> xs (interpose sep) (cons sep) vec) sep)]
        ;;     (loop [rss (transient2 (vec (repeat (count xs) (vec (repeat (inc mistake-limit) 0)))))
        ;;            i 0 ; center of palindrome
        ;;            r (transient (vec (repeat (count xs) 0)))] ; radius of palindrome
        ;;       (if (< i (count rss))
        ;;         (let [r (->> (range r (- (count rss) i))
        ;;                      (take-while #(= (get xs (- i %)) (get xs (+ i %))))
        ;;                      last)
        ;;               [rss i rs]
        ;;               (loop [rss (assoc! rss i r) i' (inc i) rs' 0]
        ;;                 (if (<= i' (+ i r))
        ;;                   (let [mi (- (* 2 i) i')
        ;;                         max-mr (- (+ i r) i')]
        ;;                     (cond
        ;;                       (< (get rss mi) max-mr) (recur (assoc! rss i' (get rss mi)) (inc i') rs')
        ;;                       (> (get rss mi) max-mr) (recur (assoc! rss i' max-mr) (inc i') rs')
        ;;                       :else [rss i' max-mr]))
        ;;                   [rss i' rs']))]
        ;;           (recur rss i rs))
        ;;         (persistent! rss)))))
        ;; solve-rows
        ;; (fn [rows]
          ;; (->> rows
          ;;      (palindromes-with-mistakes 1)
          ;;      (take-nth 2)
          ;;      (map-indexed vector)
          ;;      butlast
          ;;      (filter (fn [[i w]] ; check whether against either edge
          ;;                (let [r (quot w 2)]
          ;;                  (or (= i r)
          ;;                      (= (- (count rows) r) i)))))
          ;;      (apply max-key second)
          ;;      first))
        ]
    ;; (->> grids (reduce suffix-tree []))
    ;; (->> grids (map palindromes-with-mistakes))
    (->> grids (map palindromes))
    #_())
  #_())
