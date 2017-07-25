(ns infixapp.core
	(:require [jasentaa.parser :refer [parse-all]]
			  [infix.core :refer [base-env]]
			  [infix.grammar :refer [expression]]
			  [infix.macros :refer [from-string infix]]
			  [clojure.string :as str]
			  [clojure.set :as clset]
			  [criterium.core]
			  [clojure.core.async :as async]
			  [compojure.core :refer :all]
			  [compojure.route :as route]
		      [compojure.handler]
			  [clojure.java.io :as io]
			  [cheshire.core :refer :all]
			  [spiral.core]
			  [spiral.adapters.jetty :as jetty])
	(load "infix_equation_handling")
	(load "euler_integration")
	(load "teamming")
	(load "test_systems")
	(:gen-class))	

(defn create-result-map [function-names vectors]
	(zipmap (map name function-names) vectors))	;care that create-result makes the keys strings (until then, they are keywords)	

(defn benchmark [start end step strings fileValues]
	(let [system-map (create-system-map strings fileValues)
		  team-map (create-team-map system-map fileValues)
		  subsystems-map (create-subsystem-map team-map system-map)]
		 (criterium.core/bench (serial-integration start end step system-map fileValues))
		 (criterium.core/bench (partition-labour start end step subsystems-map system-map fileValues))
		 nil))	

;the dependent team is like e.g (:i :h), but we want it to be like e.g ((:i :k)) in order to be grouped together when shown on the web app		 
(defn prepare-dependent-team-for-visualization [team-map]
	(assoc team-map :dependent (list (team-map :dependent))))		 
		 
(defn process [start end step strings fileValues]
	(let [system-map (create-system-map strings fileValues)
		  team-map (create-team-map system-map fileValues)
		  subsystems-map (create-subsystem-map team-map system-map)
		  result (cond 
					(empty? (:independent subsystems-map)) (serial-integration start end step system-map fileValues)
					(and (empty? (:dependent subsystems-map)) (= (count (:independent subsystems-map)) 1)) (serial-integration start end step system-map fileValues)
					:else (partition-labour start end step subsystems-map system-map fileValues))
		 result-map (create-result-map (keys result) (vals result))]
		(hash-map :results result-map :all-teams (prepare-dependent-team-for-visualization team-map))
		))	
	
(defn process-req [req]
	(let [raw-post (slurp (:body req))
		  m (parse-string raw-post) ;cheshire function
		  fileValues (zipmap (map keyword (keys (m "fileValues"))) (vals (m "fileValues"))) ;convert string keys to keywords
		  iterations (quot (- (m "end" ) (m "start")) (m "step"))
		  fileValues (zipmap (take (inc iterations) (keys fileValues)) (map #(vec (take (inc iterations) %)) (vals fileValues))) ;keep only the values that will be used in the integration + the initial values, we use vec because later we want it to be homogenous with others
		  result (process (m "start") (m "end" ) (m "step") (m "strings") fileValues)]
		;(println (generate-string result))  
		(generate-string result))) ;cheshire function to create json from clojure data structures			

;Defines a Ring handler function from a sequence of routes
(defroutes app-routes
	(GET "/" [] (slurp (io/resource "public/index.html")))
	(POST "/" [] (fn [req] (process-req req)))
	(route/resources "/")
	(route/not-found "HTTP Error 404"))
	
;return server to stop it if we like	
(defn start-server [port concurrent-num buffer]
	(let [handler  (spiral.core/sync->async-adapter app-routes {:parallelism concurrent-num :buffer-size buffer})
		  server (jetty/run-jetty-async (jetty/to-jetty handler) {:port port :join? false})]
		 server))			
  
(defn -main []
	(start-server 3000 (* (.availableProcessors (Runtime/getRuntime)) 2) 1000))
	
	
	