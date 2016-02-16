(defproject nben "0.1.0-SNAPSHOT"
  :description (str "The nben library is a numberic and math library for clojure. I employ it in my"
                    " research and encourage others to use it as well.")
  :url "http://github.com/NoahBenson/nben"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
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
  :aot [nben.math.graph
        ;nben.math.derivative
        ;nben.math
        ;nben.min.gradient-descent
        ;nben.min
        ]
  ;; targets that get cleaned...
  :clean-targets [:target-path :compile-path]
  ;; jar file options...
  :jar-name "nben.jar"
  :omit-source false
  :jar-exclusions [#"(?:^|/).svn/"]
  ;; And some options for the REPL...
  :repl-options {:init (use '(nben.math graph))})
