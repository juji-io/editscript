(defproject bench "0.2.0"
  :description "Compare diff library alternatives"
  :url "https://github.com/juji-io/editscript"
  :lein-release {:deploy-via :clojars}
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [org.clojure/test.check "1.1.0"]
                 [org.clojure/data.csv "1.0.0"]
                 [criterium "0.4.6"]
                 [com.taoensso/nippy "3.1.1"]
                 [differ "0.3.3"]
                 [lambdaisland/deep-diff2 "2.0.108"]
                 [juji/editscript "0.5.4"]])
