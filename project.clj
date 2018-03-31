(defproject juji/editscript "0.2.0-SNAPSHOT"
  :description "A differ for Clojure data structure"
  :url "https://github.com/juji-io/editscript"
  :lein-release {:deploy-via :clojars}
  :deploy-repositories [["releases" :clojars]]
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/test.check "0.9.0"]
                 [org.clojure/data.priority-map "0.0.7"]
                 [diffit "1.0.0"]
                 [criterium "0.4.4"]
                 ])
