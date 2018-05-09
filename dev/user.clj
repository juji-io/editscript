(ns user
  (:require
    [cljs.repl :as repl]
    [cljs.repl.nashorn :as nashorn]))

(defn nashorn-repl
  []
  (repl/repl* (nashorn/repl-env)
              {:output-dir     "out"
               :optimizations  :none
               :source-map     true
               :cache-analysis true
               :checked-arrays :warn
               :watch          "src"
               :parallel-build true}))
