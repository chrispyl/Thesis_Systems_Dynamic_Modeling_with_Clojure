
(defn create-result-map [function-names vectors]
	(zipmap (map name function-names) vectors))	;care that create-result makes the keys strings (until then, they are keywords)

(defn create-init-vals-map [system-map]
	(zipmap (keys system-map) (map :init-val (vals system-map))))

(defn create-value-map [system-map init-vals-map]
	(transient (zipmap (keys system-map) (map #(transient [%]) (vals init-vals-map)))))

;returns a vector of vectors, each one containing the parameters of a function
(defn create-params-vector [system-map]
	(mapv :params (vals system-map)))	
	
;returns a vector of of functions
(defn create-fn-vector [system-map]
	(mapv :func (vals system-map)))				
	
(defn calc-func [iter func-name value-map params-subvector func]
	(func (zipmap params-subvector (map #((value-map %) iter) params-subvector)))) 	
	
(defn euler-method [iter step value-map func-name params-subvector func]
	(+ ((value-map func-name) iter) (* step (calc-func iter func-name value-map params-subvector func))))	

(defn serial-integration [start end step system-map]
	(let  [init-vals-map (create-init-vals-map system-map)
		   value-map (create-value-map system-map init-vals-map) ;this is a TRANSIENT map with TRANSIENT values
		   params-vector (create-params-vector system-map)
		   fn-vector (create-fn-vector system-map)
		   system-map-keys (keys system-map)
		   iterations (long (quot (- end start) step))]
		   (loop [iter 0]
				(when (< iter iterations)
					(dorun (map #(assoc! value-map % (conj! (value-map %) (euler-method iter step value-map % %2 %3))) system-map-keys params-vector fn-vector))
					(recur (inc iter))))
		   (let [pers-value-map (persistent! value-map)
				 pers-vectors (map #(persistent! %) (vals pers-value-map))]
				 (create-result-map system-map-keys  pers-vectors))))	
	
(defn go-integration [channel start end step system-map]
	(async/go
		(let  [init-vals-map (create-init-vals-map system-map)
			   value-map (create-value-map system-map init-vals-map) ;this is a TRANSIENT map with TRANSIENT values
			   params-vector (create-params-vector system-map)
			   fn-vector (create-fn-vector system-map)
			   system-map-keys (keys system-map)
			   iterations (long (quot (- end start) step))]
			   (loop [iter 0]
					(when (< iter iterations)
						(let [new-values (map #(euler-method iter step value-map % %2 %3) system-map-keys params-vector fn-vector)]
							(dorun (map #(assoc! value-map % (conj! (value-map %) %2)) system-map-keys new-values))
							(async/>! channel (zipmap system-map-keys new-values)))
						(recur (inc iter))))
			   (let [pers-value-map (persistent! value-map)
					 pers-vectors (map #(persistent! %) (vals pers-value-map))]
					(create-result-map system-map-keys pers-vectors)))))
					
(defn thread-integration [channel start end step subsystem-map original-system-map]
	(async/thread
		(let  [system-map-keys (into #{} (apply conj (flatten (map :params (vals subsystem-map))) (keys subsystem-map))) ;in order to gave outer dependencies on our system-map
			   system-map (zipmap system-map-keys (map #(original-system-map %) system-map-keys))
			   init-vals-map (create-init-vals-map system-map)
			   value-map (create-value-map system-map init-vals-map) ;this is a TRANSIENT map with TRANSIENT values
			   params-vector (create-params-vector subsystem-map)
			   fn-vector (create-fn-vector subsystem-map)
			   iterations (long (quot (- end start) step))]
			   (loop [msg (async/<!! channel) iter 0]
					(when (< iter iterations)
						(dorun (map #(assoc! value-map % (conj! (value-map %) (msg %))) (filter #(system-map %)(keys msg))))
						(if (true? (every? #(>= (count %) (inc iter)) (map #(value-map %) system-map-keys))) 
							(do
								(dorun (map #(assoc! value-map % (conj! (value-map %) (euler-method iter step value-map % %2 %3))) (keys subsystem-map) params-vector fn-vector))
								(recur (async/<!! channel) (inc iter)))
							(recur (async/<!! channel) iter))))
			   (let [pers-value-map (persistent! value-map)
					 pers-vectors (map #(persistent! %) (vals pers-value-map))]
					(create-result-map system-map-keys pers-vectors)))))					
				 
(defn partition-labour [start end step subsystems-map system-map]
	(let [channel-count (count (:independent subsystems-map))
		  buffer-size (long (quot (- end start) step))
		  channels (take channel-count (repeatedly #(async/chan buffer-size)))
		  merged-channel (async/merge channels (* buffer-size channel-count))
		  gos (doall ;without doall only 1 go starts
				(map #(go-integration % start end step %2) channels (:independent subsystems-map)))
		  thread (if (not (empty? (:dependent subsystems-map))) 
						(thread-integration merged-channel start end step (:dependent subsystems-map) system-map) 
						())]
		  (if (= thread ()) 
			(apply merge (map #(async/<!! %) gos)) 
			(apply merge (map #(async/<!! %) (conj gos thread))))))				 
		  