(defproject nben "0.1.0-SNAPSHOT"
  :description (str "The nben library is a numberic and scientific toolkit for the JVM.")
  :url "http://github.com/NoahBenson/nben"
  :license {:name "GNU General Public License"
            :url "http://www.gnu.org/licenses/"}
  ;; dependencies
  :dependencies [[org.clojure/clojure "1.7.0"]]
  ;; location of source codes
  :source-paths ["src/main/clojure"]
  :java-source-paths ["src/main/java"]
  :test-paths ["src/test/clojure"]
  ;; target for aot compilation
  :target-path "target/"
  :compile-path "target/classes"
  ;; and the namespaces to aot compile
  :aot [nben.math.graph]
  ;; targets that get cleaned...
  :clean-targets [:target-path :compile-path]
  ;; jar file options...
  :jar-name "nben.jar"
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/"]
  ;; And some options for the REPL...
  :repl-options {:init (use '(nben.math graph))})
