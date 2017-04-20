
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
		
(defn work-sharing [tasks cores]
  (let [core-num cores
        tasks-num (count tasks)]
    (cond 
      (<= tasks-num core-num) (partition 1 tasks)
      (> tasks-num core-num) (if (> (mod tasks-num core-num) 0)
                               (let [initial-division (partition-all (quot tasks-num core-num) tasks)]
                                 (concat
                                   (drop (mod tasks-num core-num) (take core-num initial-division))
                                   (map conj initial-division (take-last (mod tasks-num core-num) tasks))))
                               (partition-all (quot tasks-num core-num) tasks)))))
		
(defn calc-euler-chunk 
	([iter step value-map outer-dependencies-produced-values-map system-map-keys-chunk params-vector-chunk fn-vector-chunk]
		(doall (map #(euler-method iter step value-map outer-dependencies-produced-values-map % %2 %3) system-map-keys-chunk params-vector-chunk fn-vector-chunk)))
	([iter step value-map system-map-keys-chunk params-vector-chunk fn-vector-chunk]
		(doall (map #(euler-method iter step value-map % %2 %3) system-map-keys-chunk params-vector-chunk fn-vector-chunk))))
		
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
		   
		   distributed-system-map-keys (work-sharing system-map-keys (.availableProcessors (Runtime/getRuntime)))
		   distributed-params-vector (work-sharing params-vector (.availableProcessors (Runtime/getRuntime)))
		   distributed-fn-vector (work-sharing fn-vector (.availableProcessors (Runtime/getRuntime)))
		   
		   final-value-map (loop [iter 0 vm value-map]
								(if (< iter iterations)
									(let [send-values-to-other-threads (doall (map #(async/go (calc-euler-chunk iter step vm % %2 %3)) distributed-system-map-keys distributed-params-vector distributed-fn-vector))
										  new-euler-method-values (flatten (doall (map #(async/<!! %) send-values-to-other-threads))) ;without flattten they are example ((5)(5)(5))
										  value-map-after-diffs-assoced (loop [ks system-map-keys m vm euler-method-values new-euler-method-values]
																			(if ks
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (first euler-method-values))) (rest euler-method-values))
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
	
	
(defn dependent-integration [start end step system-map produced-values-map]
	(let  [outer-dependencies (filter #(nil? (system-map %)) (set (flatten (map :params (vals system-map)))))
		   outer-dependencies-produced-values-map (zipmap outer-dependencies (map #(produced-values-map %) outer-dependencies))
		   whole-system-keys (keys system-map)
		   init-vals-map (create-init-vals-map system-map)
		   value-map (create-value-map system-map init-vals-map)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   simple-eqs-map (remove-elements true system-map)
		   system-map (remove-elements false system-map)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   params-vector (create-params-vector system-map)
		   simple-eqs-params-vector (create-params-vector simple-eqs-map)
		   fn-vector (create-fn-vector system-map)
		   simple-eqs-fn-vector (create-fn-vector simple-eqs-map)
		   system-map-keys (keys system-map)
		   simple-eqs-map-keys (keys simple-eqs-map)
		   iterations (long (quot (- end start) step))
		   
		   distributed-system-map-keys (work-sharing system-map-keys (.availableProcessors (Runtime/getRuntime)))
		   distributed-params-vector (work-sharing params-vector (.availableProcessors (Runtime/getRuntime)))
		   distributed-fn-vector (work-sharing fn-vector (.availableProcessors (Runtime/getRuntime)))
		   
		   final-value-map (loop [iter 0 vm value-map]
								(if (< iter iterations)
									(let [send-values-to-other-threads (doall (map #(async/go (calc-euler-chunk iter step vm outer-dependencies-produced-values-map % %2 %3)) distributed-system-map-keys distributed-params-vector distributed-fn-vector))
										  new-euler-method-values (flatten (doall (map #(async/<!! %) send-values-to-other-threads))) ;without flattten they are example ((5)(5)(5))
										  value-map-after-diffs-assoced (loop [ks system-map-keys m vm euler-method-values new-euler-method-values]
																			(if ks
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (first euler-method-values))) (rest euler-method-values))
																				m))
										  value-map-after-eqs-assoced (loop [ks simple-eqs-map-keys m value-map-after-diffs-assoced params-vec simple-eqs-params-vector fn-vec simple-eqs-fn-vector]
																			(if ks
																				(let [vec-size (count ((first ks) m))
																					  internal-parameters (filter #(% m) (first params-vec))] ;internal to dependent, not from independent teams
																					(if (every? #(= % (inc vec-size)) (map #(count (% m)) internal-parameters))										
																						(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (calc-func (inc iter) (first ks) m outer-dependencies-produced-values-map (first params-vec) (first fn-vec)))) (next params-vec) (next fn-vec))
																						(recur (conj (vec (next ks)) (first ks)) m (conj (vec (next params-vec)) (first params-vec)) (conj (vec (next fn-vec)) (first fn-vec)))))
																				m))]
									(recur (inc iter) value-map-after-eqs-assoced))
									vm))
			pers-value-map (persistent! final-value-map)
			pers-vectors (map #(persistent! %) (vals pers-value-map))]
		(zipmap whole-system-keys pers-vectors)))			
				 
	
(defn partition-labour [start end step subsystems-map system-map]
	(let [gos (doall ;without doall only 1 go starts
					(map #(async/go (serial-integration start end step %)) (:independent subsystems-map)))
		  independent-result (apply merge (map #(async/<!! %) gos))
		  dependent-result (dependent-integration start end step (:dependent subsystems-map) independent-result)
		  merged-results (merge independent-result dependent-result)]
		  (zipmap (keys merged-results) (vals merged-results))))	