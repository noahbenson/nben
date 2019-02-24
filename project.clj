(defproject nben "0.1.0-SNAPSHOT"
  :description "The nben library is a numeric and scientific toolkit for the JVM."
  :url "http://github.com/NoahBenson/nben"
  :license {:name "GNU General Public License"
            :url "http://www.gnu.org/licenses/"}
  ;; dependencies
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.incubator "0.1.4"]
                 [criterium "0.4.3"] ;; for benchmarking (may remove in future)
                 [org.clojure/data.priority-map "0.0.7"]
                 [org.clojure/data.finger-tree "0.0.2"]
                 [org.jordanlewis/data.union-find "0.1.0"]
                 [org.clojure/data.int-map "0.2.4"]
                 [me.raynes/fs "1.4.6"]
                 [clj-time "0.13.0"]
                 [smee/binary "0.5.1"]
                 [instaparse "1.4.8"]
                 [com.cerner/clara-rules "0.19.0"]
                 ;[net.mikera/core.matrix "0.59.0"]
                 [potemkin "0.4.5"]]
  ;; location of source codes
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure"]
  ;; target for aot compilation
  :target-path "target/"
  :compile-path "target/classes"
  ;; and the namespaces to aot compile
  :aot [nben.math.graph
        nben.util.error
        nben.util.typedef
        nben.util.set
        nben.util.iterator
        nben.util.structured
        nben.util.misc
        nben.util.data
        nben.util
        nben.sys]
  ;; targets that get cleaned...
  :clean-targets [:target-path :compile-path]
  ;; jar file options...
  :jar-name "nben.jar"
  ;; uberjar...
  :uberjar-name "nben-standalone.jar"
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/"]
  ;; And some options for the REPL...
  :repl-options {:init (do (refer-clojure :exclude '[* - + == / < <= > >= not= = min max])
                           (use '[nben util sys]))
                 :prompt (fn [_] ":> ")})
