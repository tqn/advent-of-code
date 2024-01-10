(ns day10
  (:require [clojure.set :as set]
            [clojure.string :as str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA STRUCTURE UTILITY FUNCTIONS

; adapated from clojure.core, just assoc-in for recursive transients
(defn assoc-in! [m [k & ks] v]
  (if ks
    (assoc! m k (assoc-in! (get m k) ks v))
    (assoc! m k v)))

(defn transient2 [xss] (transient (mapv transient xss)))

(defn persistent2! [xss] (mapv persistent! (persistent! xss)))

(defn assoc2 [xss coords c]
  (-> xss transient2 (assoc-in! coords c) persistent2!))

(defn indices-of2 [pred xss]
  (apply concat
         (keep-indexed
          (fn [ri row]
            (-> #(when (pred %2) %1)
                (keep-indexed row)
                (->> (map (partial vector ri)))))
          xss)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PIPE, GRID, AND DIRECTION HELPERS

(def start-chars #{\S})

(def dir-flip
  "maps a cardinal direction to its opposite"
  (let [m {:n :s, :e :w}]
    (merge m (set/map-invert m))))

(def dir-offset
  "maps cardinal direction to (row, column) offset"
  {:n [-1 0]
   :s [1 0]
   :e [0 1]
   :w [0 -1]})

(defn walk-dir [coords dir] (map + coords (dir-offset dir)))

(def pipe-dirs
  {\| #{:n :s}
   \- #{:e :w}
   \L #{:n :e}
   \J #{:n :w}
   \7 #{:s :w}
   \F #{:s :e}})

(def dir-pipe (set/map-invert pipe-dirs))

(defn pipe-continue
  "Having come along a bearing and reaching a pipe, get new bearing"
  [bearing pipe]
  (first (disj (pipe-dirs pipe) (dir-flip bearing))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUSINESS LOGIC

(defn connecting-neighbors [grid from-coords]
  (->> from-coords
       (partial map +) ; picking an affine point (origin)
       (update-vals dir-offset) ; neighbor offsets to points
       (filter (fn [[bearing coords]]
                 (contains? (pipe-dirs (get-in grid coords)) (dir-flip bearing))))))

(defn prepare-grid [grid]
  (when-some [start-coords (first (indices-of2 start-chars grid))]
    [(->> start-coords
          (connecting-neighbors grid)
          (map first)
          set
          dir-pipe
          (assoc2 grid start-coords))
     start-coords]))

(defn step-with [grid]
  (fn [[bearing coords]] ; bearing entering
    (let [bearing' (pipe-continue bearing (get-in grid coords))
          coords' (walk-dir coords bearing')]
      [bearing' coords'])))

(defn walk-loop [grid start-coords]
  (let [steps (iterate (step-with grid) [nil start-coords])]
    (cons (first steps)
          (take-while (fn [[_ coords]] (not (= start-coords coords)))
                      (rest steps)))))

;; A pipe can be thought of as a size 2 subset of the four cardinal directions,
;; visualized as two line segments connecting the midpoint of a square
;; to the midpoints of the adjacent edges. pipes-n consists of those pipes
;; that connect to the north. Due to topological reasons, we can
;; draw a horizontal line through the north half of the squares in a row
;; and count intersections to determine whether we are inside or outside
;; the loop when we are in a cell without a pipe. Thus, we mark pipes
;; connecting to the north with `:v` (vertical) and mark the other pipes in our
;; loop with `:p` (pipe) --- the former to find whether we are inside or not,
;; the latter to know which cells to skip when counting space.
(def count-interior
  (let [pipes-n (set (map key (filter (comp :n val) pipe-dirs)))]
    (letfn [(mask [grid loop-coords]
              (persistent2!
               (reduce (fn [g c]
                         (assoc-in! g c (if (pipes-n (get-in g c)) :v :p)))
                       (transient2 grid)
                       loop-coords)))
            (row-rf [[ct ins :as rd] c]
              (case c
                :v [ct (not ins)]
                :p rd
                (if ins [(inc ct) ins] rd)))
            (row-mf [row] (->> row (reduce row-rf [0 false]) first))]
      (fn [grid loop-coords]
        (->> (mask grid loop-coords) (map row-mf) (reduce + 0))))))

(defn -main [input-path]
  (let [[grid start-coords] (->> input-path slurp str/split-lines (mapv vec) prepare-grid)
        start-loop (walk-loop grid start-coords)]
    (println (quot (count start-loop) 2))
    (println (count-interior grid (map second start-loop)))))

(comment
  (-main "data/day10.txt")
  #_())
