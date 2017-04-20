
(defn remove-elements [differential? system-map]
	(loop [ks (keys system-map) m system-map]
		(if ks
			(recur (next ks) (if (= (:differential ((first ks) m)) differential?)
								(dissoc m (first ks))
								m))
			m)))	
	
(defn create-init-vals-map [system-map]
	(zipmap (keys system-map) (map :init-val (vals system-map))))
(defn create-value-map [system-map init-vals-map]
	(transient (zipmap (keys system-map) (map #(transient [%]) (vals init-vals-map))))
		(transient (zipmap (keys system-map) (map #(transient [%]) (vals init-vals-map)))))
;returns a vector of vectors, each one containing the parameters of a function
(defn create-params-vector [system-map]
	(mapv :params (vals system-map)))	
	
;returns a vector of of functions
(defn create-fn-vector [system-map]
	(mapv :func (vals system-map)))				
	
(defn calc-func 
	([iter func-name value-map params-subvector func]
		(func (zipmap params-subvector (map #((% value-map) iter) params-subvector))))
	([iter func-name value-map outer-dependencies-produced-values-map params-subvector func]
		(func (zipmap params-subvector (map 
											#(if-let [v (% value-map)]
												(v iter)
												((% outer-dependencies-produced-values-map) iter))
										params-subvector))))) 
										
(defn euler-method 
	([iter step value-map func-name params-subvector func]
		(+ ((func-name value-map) iter) (* step (calc-func iter func-name value-map params-subvector func))))
	([iter step value-map outer-dependencies-produced-values-map func-name params-subvector func]
		(+ ((func-name value-map) iter) (* step (calc-func iter func-name value-map outer-dependencies-produced-values-map params-subvector func)))))	
		
(defn serial-integration [start end step system-map]
	(let  [whole-system-keys (keys system-map)
		   init-vals-map (create-init-vals-map system-map)
		   value-map (create-value-map system-map init-vals-map) ;this is a TRANSIENT map with TRANSIENT values
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   simple-eqs-map (remove-elements true system-map)
		   system-map (remove-elements false system-map)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   params-vector (create-params-vector system-map)
		   simple-eqs-params-vector (create-params-vector simple-eqs-map)
		   fn-vector (create-fn-vector system-map)
		   simple-eqs-fn-vector (create-fn-vector simple-eqs-map)
		   system-map-keys (keys system-map)
		   simple-eqs-map-keys (keys simple-eqs-map)
		   iterations (long (quot (- end start) step))
		   final-value-map (loop [iter 0 vm value-map]
								(if (< iter iterations)
									(let [value-map-after-diffs-assoced (loop [ks system-map-keys m vm pv params-vector fv fn-vector]
																			(if ks
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (euler-method iter step m (first ks) (first pv) (first fv)))) (next pv) (next fv))
																				m))
										  value-map-after-eqs-assoced (loop [ks simple-eqs-map-keys m value-map-after-diffs-assoced params-vec simple-eqs-params-vector fn-vec simple-eqs-fn-vector]
																			(if ks
																				(let [vec-size (count ((first ks) m))
																					  internal-parameters (filter #(% m) (first params-vec))] ;internal to dependent, not from independent teams
																					(if (every? #(= % (inc vec-size)) (map #(count (% m)) internal-parameters))										
																						(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (calc-func (inc iter) (first ks) m (first params-vec) (first fn-vec)))) (next params-vec) (next fn-vec))
																						(recur (conj (vec (next ks)) (first ks)) m (conj (vec (next params-vec)) (first params-vec)) (conj (vec (next fn-vec)) (first fn-vec)))))
																				m))]
									(recur (inc iter) value-map-after-eqs-assoced))
									vm))
			pers-value-map (persistent! final-value-map)
			pers-vectors (map #(persistent! %) (vals pers-value-map))]
		(zipmap whole-system-keys pers-vectors)))					
		
(defn independent-integration [start end step system-map channel send-at]
	(async/go
		(let  [whole-system-keys (keys system-map)
			   init-vals-map (create-init-vals-map system-map)
			   value-map (create-value-map system-map init-vals-map) ;this is a TRANSIENT map with TRANSIENT values
			   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			   simple-eqs-map (remove-elements true system-map)
			   system-map (remove-elements false system-map)
			   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
			   params-vector (create-params-vector system-map)
			   simple-eqs-params-vector (create-params-vector simple-eqs-map)
			   fn-vector (create-fn-vector system-map)
			   simple-eqs-fn-vector (create-fn-vector simple-eqs-map)
			   system-map-keys (keys system-map)
			   simple-eqs-map-keys (keys simple-eqs-map)
			   iterations (long (quot (- end start) step))
			   final-value-map (loop [iter 0 vm value-map remaining-sends send-at current-send (first send-at)]
									(let [[vm remaining-sends] (if (not= iter current-send)
																		[vm remaining-sends]
																		(let [persist-map (persistent! vm)
																			  persist-vecs (map #(persistent! %) (vals persist-map))
																			  map-to-be-sent (zipmap (keys persist-map) (map transient persist-vecs))]
																			(async/>! channel map-to-be-sent)
																			[(transient (zipmap (keys persist-map) (map transient persist-vecs))) (rest remaining-sends)]))] ;we dont put here 'map-to-be-sent' because we want to keep the transient vectors transient
										(if (< iter iterations)
											(let [value-map-after-diffs-assoced (loop [ks system-map-keys m vm pv params-vector fv fn-vector]
																					(if ks
																						(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (euler-method iter step m (first ks) (first pv) (first fv)))) (next pv) (next fv))
																						m))
												  value-map-after-eqs-assoced (loop [ks simple-eqs-map-keys m value-map-after-diffs-assoced params-vec simple-eqs-params-vector fn-vec simple-eqs-fn-vector]
																					(if ks
																						(let [vec-size (count ((first ks) m))
																							  internal-parameters (filter #(% m) (first params-vec))] ;internal to dependent, not from independent teams
																							(if (every? #(= % (inc vec-size)) (map #(count (% m)) internal-parameters))										
																								(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (calc-func (inc iter) (first ks) m (first params-vec) (first fn-vec)))) (next params-vec) (next fn-vec))
																								(recur (conj (vec (next ks)) (first ks)) m (conj (vec (next params-vec)) (first params-vec)) (conj (vec (next fn-vec)) (first fn-vec)))))
																						m))]						
											(recur (inc iter) value-map-after-eqs-assoced remaining-sends (first remaining-sends)))
										vm)))
			   pers-value-map (persistent! final-value-map)
			   pers-vectors (map #(persistent! %) (vals pers-value-map))]
			(zipmap (keys pers-value-map) pers-vectors))))
	
(defn dependent-integration [start end step system-map channel receive-at]
	(let  [dependencies (set (flatten (map #(:params (% system-map)) (keys system-map))))	
		   outer-dependencies (clset/difference dependencies (set (keys system-map)))
		   whole-system-keys (keys system-map)
		   init-vals-map (create-init-vals-map system-map)
		   value-map (create-value-map system-map init-vals-map) ;this is a TRANSIENT map with TRANSIENT values
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   simple-eqs-map (remove-elements true system-map)
		   system-map (remove-elements false system-map)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   params-vector (create-params-vector system-map)
		   simple-eqs-params-vector (create-params-vector simple-eqs-map)
		   fn-vector (create-fn-vector system-map)
		   simple-eqs-fn-vector (create-fn-vector simple-eqs-map)
		   system-map-keys (keys system-map)
		   simple-eqs-map-keys (keys simple-eqs-map)
		   iterations (long (quot (- end start) step))
		   missing-elements-map (zipmap outer-dependencies (repeatedly #(transient [])))
		   value-map (reduce conj! value-map missing-elements-map)
		   final-value-map (loop [iter 0 vm value-map remaining-receives receive-at current-receive (first receive-at)]
								(if (< iter iterations)									
									(let [[vm remaining-receives] (if (not= iter current-receive) 				
																	[vm remaining-receives]
																	(loop [m vm]
																		(if (every? #(> (count (% m)) (inc iter)) outer-dependencies)
																			[m (rest remaining-receives)]
																			(recur (conj! m (async/<!! channel))))))
										  value-map-after-diffs-assoced (loop [ks system-map-keys m vm pv params-vector fv fn-vector]
																			(if ks
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (euler-method iter step m (first ks) (first pv) (first fv)))) (next pv) (next fv))
																				m))
										  value-map-after-eqs-assoced (loop [ks simple-eqs-map-keys m value-map-after-diffs-assoced params-vec simple-eqs-params-vector fn-vec simple-eqs-fn-vector]
																			(if ks
																				(let [vec-size (count ((first ks) m))
																					  internal-parameters (filter #(% m) (first params-vec))] ;internal to dependent, not from independent teams
																					(if (every? #(> % vec-size) (map #(count (% m)) internal-parameters))								
																						(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (calc-func (inc iter) (first ks) m (first params-vec) (first fn-vec)))) (next params-vec) (next fn-vec))
																						(recur (conj (vec (next ks)) (first ks)) m (conj (vec (next params-vec)) (first params-vec)) (conj (vec (next fn-vec)) (first fn-vec)))))
																				m))]
									(recur (inc iter) value-map-after-eqs-assoced remaining-receives (first remaining-receives)))
									vm))
			pers-value-map (persistent! final-value-map)
			pers-vectors (map #(persistent! %) (vals pers-value-map))]
		(zipmap (keys pers-value-map) pers-vectors)))			
				 
(defn when-to-send [start end step num-of-sends]
	(let [iterations (long (quot (- end start) step))
		  chunk-size (quot iterations num-of-sends)]
		(take num-of-sends (iterate #(+ % chunk-size) chunk-size))))
(defn when-to-receive [start end step num-of-sends]
	(let [iterations (long (quot (- end start) step))
		  chunk-size (quot iterations num-of-sends)]
		(take num-of-sends (iterate #(+ % chunk-size) start))))		
		
(defn partition-labour [start end step subsystems-map system-map]
	(let [num-of-sends 10
		  send-at (when-to-send start end step num-of-sends)
		  receive-at (when-to-receive start end step num-of-sends)
		  channel-buffer num-of-sends
		  merged-buffer (* (count (:independent subsystems-map)) num-of-sends)
		  channels (take (count (:independent subsystems-map)) (repeatedly #(async/chan channel-buffer)))
		  merged-channel (async/merge channels merged-buffer)
		  gos (doall ;without doall only 1 go starts
					(map #(independent-integration start end step % %2 send-at) (:independent subsystems-map) channels))
		  result (if (not (empty? (:dependent subsystems-map))) 
						(dependent-integration start end step (:dependent subsystems-map) merged-channel receive-at)
						(apply merge (map #(async/<!! %) gos)))]
		result))				 
		  