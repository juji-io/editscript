(defproject juji/editscript "0.4.1"
  :description "A diffing library for Clojure data structures"
  :url "https://github.com/juji-io/editscript"
  :lein-release {:deploy-via :clojars}
  :deploy-repositories [["releases" :clojars]]
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :plugins [[lein-cljsbuild "1.1.7"]
            [lein-doo "0.1.10"]]
  :doo {:build "node"
        :paths {:karma "./node_modules/karma/bin/karma"}
        :karma {:config {"browserDisconnectTimeout" 30000
                         "browserNoActivityTimeout" 90000}}}
  :clean-targets  ^{:protect false} [:target-path "out" "target"]
  :cljsbuild {:builds
              {:dev
               {:source-paths ["src" "test" "dev"]
                :compiler     {:output-to      "target/editscript.js"
                               :output-dir     "target"
                               :optimizations  :none
                               :source-map     true
                               :cache-analysis true
                               :checked-arrays :warn
                               :parallel-build true}}
               :node
               {:source-paths ["src" "test"]
                :compiler     {:output-to      "out/node/editscript.js"
                               :output-dir     "out/node"
                               :optimizations  :advanced
                               :main           "editscript.test"
                               :source-map     "out/node/editscript.js.map"
                               :target         :nodejs
                               :cache-analysis true
                               :checked-arrays :warn
                               :parallel-build true}}
               :browser
               {:source-paths ["src" "test"]
                :compiler     {:output-to      "out/browser/editscript.js"
                               :output-dir     "out/browser"
                               :optimizations  :advanced
                               :main           "editscript.test"
                               :source-map     "out/browser/editscript.js.map"
                               :cache-analysis true
                               :checked-arrays :warn
                               :parallel-build true}}}}
  :profiles {:deploy
             {:aot      [#"editscript\.*"]
              :jvm-opts ["-Dclojure.compiler.direct-linking=true"] }
             :dev
             {:dependencies [[org.clojure/clojurescript "1.10.520"
                              :exclusions [org.clojure/core.rrb-vector]]
                             ;;see https://github.com/emezeske/lein-cljsbuild/issues/469
                             [quantum/org.clojure.core.rrb-vector "0.0.12"]
                             [criterium "0.4.5"]
                             [doo "0.1.11"]
                             [org.clojure/test.check "0.10.0"]
                             [cider/piggieback "0.4.1"]]
              :source-paths ["src" "test" "dev"]
              :repl-options {:nrepl-middleware [cider.piggieback/wrap-cljs-repl]}}})
