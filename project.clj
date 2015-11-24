(defproject lambda-ski-iota "0.1.0-SNAPSHOT"
  :description "Express/convert lambda functions using  SKI combinators and Iota combinator"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                [org.clojure/tools.trace "0.7.8"]
                [backtick "0.3.3"]]
  :jvm-opts ["-Xmx1g" "-XX:-OmitStackTraceInFastThrow" "-XX:MaxJavaStackTraceDepth=-1" "-Xss1600k"] ; "-Xss300k"
  ;:main ^:skip-aot lambda-ski-iota.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
