(ns tggreene.oeuvre.benchmark
  (:require [tggreene.oeuvre :as o]
            [criterium.core :as criterium]))

(comment
  (criterium/quick-bench (into [1] [2 3 4]))
  (criterium/quick-bench (apply vector 1 [ 2 3 4])))
