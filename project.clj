(defproject kickscraper "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [clj-http "2.3.0"]
                 [clj-time "0.13.0"]
                 [cheshire "5.7.0"]
                 [com.hypirion/clj-xchart "0.2.0"]
                 ;;manifold
                 [net.mikera/core.matrix "0.31.1"]
                 [net.mikera/vectorz-clj "0.26.1"]
                 [org.clojure/data.csv "0.1.2"]]
  :main ^:skip-aot kickscraper.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
