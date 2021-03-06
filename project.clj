(defproject tggreene/oeuvre "0.5.4"
  :description "Some useful stuff to complement clojure.core"
  :url "https://github.com/tggreene/oeuvre"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"
            :distribution :repo
            :comments "same as Clojure"}
  :dependencies []
  :plugins [[metosin/bat-test "0.4.4"]]

  :bat-test {:report [:pretty
                      {:type :junit :output-to "target/junit.xml"}]}

  :source-paths ["src" "target/generated/src"]
  :test-paths ["test" "target/generated/test"]

  :profiles {:dev {:plugins [[jonase/eastwood "0.3.12"]]
                   :dependencies [[org.clojure/clojure "1.9.0"]
                                  [criterium "0.4.6"]
                                  [org.clojure/clojurescript "1.10.520"]]}
             :1.7 {:dependencies [[org.clojure/clojure "1.7.0"]
                                  ;; Test aren't run with old cljs - but new cljs would bring in
                                  ;; new tools.reader which doesn't work with old Clojure.
                                  [org.clojure/clojurescript "1.7.228"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.10 {:dependencies  [[org.clojure/clojure "1.10.0"]]}}
  :deploy-repositories [["releases" :clojars]]
  :aliases {"all" ["with-profile" "dev:dev,1.7:dev,1.8:dev,1.10"]
            "test-clj"  ["all" "do" ["bat-test"] ["check"]]})
