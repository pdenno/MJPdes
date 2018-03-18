(defproject gov.nist/MJPdes "0.3.0"
  :description "A discrete event simulation engine for Multi-Job Production"
  :url "https://github.com/usnistgov/MJPdes"
  :license {:name "See the github site for details"
            :url "https://github.com/usnistgov/MJPdes"}
  :profiles {:uberjar {:aot :all}}
  :plugins [[lein-binplus "0.6.4"]]
  :dependencies [[org.clojure/clojure        "1.9.0"]
                 [org.clojure/core.async   "0.4.474"]
                 [incanter/incanter-core     "1.5.7"]]
  :bin {:name "MJPdes"
        :bootclasspath false 
        :jvm-opts ["-server" "-Dfile.encoding=utf-8" "$JVM_OPTS" ]}
  :main gov.nist.MJPdes.main)
