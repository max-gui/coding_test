(defproject hello-world "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/math.numeric-tower "0.0.4"]]
  :plugins [[cider/cider-nrepl "0.15.0-snapshot"]
            [lein-ring "0.9.7"]]
  :ring {:handler hello-world.handler/app}
  :profiles
  {:dev {:dependencies [[cider/cider-nrepl "0.15.0-snapshot"]
                        [javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
