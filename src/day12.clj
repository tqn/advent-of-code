(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [instaparse.core :as insta]))

(def parse
  (insta/parser
   "<S> = line*
    line = recs <hspace*> ds <vspace+>
    recs = #'[\\?#\\.]*'
    ds = d (<','> d)*
    d = '-'?#'[0-9]+'
    hspace = (' '|'\t')
    vspace = ('\n'|'\r')"))

(def cst->ast
  (letfn [(lf [[_ rs ds]] [(vec (last rs)) (mapv vf (rest ds))])
          (vf [v] (-> v rest str/join Integer/parseInt))]
    (partial map lf)))

(def wildcard #{\?})
(def damaged #{\#})
(def damaged-wk (set/union damaged wildcard))
(def undamaged #{\.})
(def undamaged-wk (set/union undamaged wildcard))

;; not sure if this is asymptotically optimal
;; essentially runs an NFA defined by `runs` on `rec`
;; and counts instances that finish
(def brute-count
  (let [go
        (memoize
         (fn [mgo recs runs]
           (or (if-some [run (first runs)]
                 (when-first [rec recs]
                   (condp contains? rec
                     damaged
                     (when (and (<= run (count recs))
                                (->> recs (take run) (every? damaged-wk))
                                (not (damaged (nth recs run nil))))
                       (mgo mgo (drop (inc run) recs) (rest runs)))
                     undamaged
                     (mgo mgo (rest recs) runs)
                     wildcard
                     (->>
                      (map #(mgo mgo (cons % (rest recs)) runs)
                           [(first damaged)
                            (first undamaged)])
                      (reduce + 0))))
                 (when (every? undamaged-wk recs) 1))
               0)))]
    (partial go go)))

(defn -main [input-path]
  (let [ast (-> input-path slurp parse cst->ast)
        fac 5
        recs-expand #(apply concat (interpose [(first wildcard)] (repeat fac %)))
        runs-expand #(apply concat (repeat fac %))
        expand-mf (fn [[recs runs]]
                    (brute-count (recs-expand recs) (runs-expand runs)))]
    (println
     (->> ast
          (pmap (partial apply brute-count))
          (reduce + 0)
          time))
    (println
     (->> ast
          (pmap expand-mf)
          (reduce + 0)
          time))))

(comment
  (-main "data/day12.txt")
  #_())
