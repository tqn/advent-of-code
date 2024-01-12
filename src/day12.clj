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

;; essentially runs an NFA defined by `runs` on `rec`
;; and counts instances that finish
;; probably asymptotically optimal because running an NFA and
;; counting solutions is like running a regular expression, except
;; in the latter case we are interested if there merely exists a match.
(defn brute-count [recs runs]
  (let [step-damaged
        (fn [mgo recsi runsi run]
          (let [recsi+run (+ recsi run)]
            (if (and (<= recsi+run (count recs))
                     (->> (subvec recs recsi recsi+run) (every? damaged-wk))
                     (not (damaged (get recs recsi+run))))
              (mgo mgo (min (inc recsi+run) (count recs)) (inc runsi))
              0)))
        step-undamaged
        (fn [mgo recsi runs]
          (mgo mgo (inc recsi) runs))
        go
        (memoize
         (fn [mgo recsi runsi]
           (or (if-some [run (get runs runsi)]
                 (when-some [rec (get recs recsi)]
                   (condp contains? rec
                     damaged (step-damaged mgo recsi runsi run)
                     undamaged (step-undamaged mgo recsi runsi)
                     ;;  wildcard
                     (+ (step-damaged mgo (inc recsi) runsi (dec run))
                        (step-undamaged mgo recsi runsi))))
                 (when (every? undamaged-wk (subvec recs recsi)) 1))
               0)))]
    (go go 0 0)))

(defn solve [input-path]
  (let [ast (-> input-path slurp parse cst->ast)
        fac 5
        recs-expand #(->> % (repeat fac) (interpose [(first wildcard)]) (apply concat) vec)
        runs-expand #(->> % (repeat fac) (apply concat) vec)
        expand-mf (fn [[recs runs]] (brute-count (recs-expand recs) (runs-expand runs)))]
    (println
     (->> ast
          (pmap (partial apply brute-count))
          (reduce + 0)))
    (println
     (->> ast
          (pmap expand-mf)
          (reduce + 0)))))

(defn -main [& args]
  (apply solve args)
  (shutdown-agents))

(comment
  (solve "data/day12.txt")
  #_())
