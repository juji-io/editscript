(defproject juji/editscript "0.3.0"
  :description "A diffing library for Clojure data structures"
  :url "https://github.com/juji-io/editscript"
  :lein-release {:deploy-via :clojars}
  :deploy-repositories [["releases" :clojars]]
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]]
  :plugins [[lein-cljsbuild "1.1.7"]]
  :aliases {"nashorn-repl" ["run" "-m" "user/nashorn-repl"]}
  :cljsbuild {:builds 
              {:dev 
               {:source-paths ["src" "test" "dev"]
                :compiler     {:output-to "editscript.js"
                               :output-dir "out"
                               :optimizations  :none
                               :main "editscript.core"
                               :source-map     true
                               :cache-analysis true
                               :checked-arrays :warn
                               :parallel-build true}}
               :release
               {:source-paths ["src"]
                :compiler     {:output-to "editscript.js" 
                               :output-dir "target"
                               :source-map "target/editscript.js.map"
                               :optimizations  :advanced
                               :checked-arrays :warn
                               :pretty-print   false
                               :parallel-build true}}}}
  :profiles {:dev
             {:dependencies [[criterium "0.4.4"]
                             [org.clojure/test.check "0.9.0"]
                             [cider/piggieback "0.3.2"]]
              :source-paths ["src" "test" "dev"]
              :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}})
