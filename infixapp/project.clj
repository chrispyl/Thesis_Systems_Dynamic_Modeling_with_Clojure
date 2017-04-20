(defproject infixapp "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
				 [rm-hull/infix "0.2.10"]
				 [org.clojure/core.async "0.2.395"]
				 [criterium "0.4.4"]
				 [compojure "1.5.1"]
				 [ring/ring-core "1.5.0"]
				 [ring/ring-jetty-adapter "1.3.0"]
				 [cheshire "5.6.3"]
				 [spiral "0.1.1"]]
  :main infixapp.core)