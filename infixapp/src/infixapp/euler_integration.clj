;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;IF THIS METHOD IS USED DONT DECREMENT THE CORE BY 1 IN TEAMMING.CLG CREATE-SUBSYSTEM-MAP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-ns 'infixapp.core)

;returns set
(defn find-no-incoming-edges [simple-eqs-map]
	(let [keys-as-set (set (keys simple-eqs-map))]
		(loop [ks (keys simple-eqs-map) k (first ks) params (set (:params (simple-eqs-map k))) params-only-from-simple-eqs (clset/intersection params keys-as-set) nodes-no-incoming-edges #{}]
			(if (empty? ks)				
				nodes-no-incoming-edges			
				(if (empty? params-only-from-simple-eqs)			
					(if (empty? (rest ks)) ;without this if (first (rest ks)) will return nil in the last loop and we will have problem in (nil :params)
						(recur (rest ks) nil nil nil (conj nodes-no-incoming-edges k)) ;nils to show that we dont are about their values anymore
						(recur (rest ks) (first (rest ks)) (set ((simple-eqs-map (first (rest ks))) :params)) (clset/intersection (set ((simple-eqs-map (first (rest ks))) :params)) keys-as-set) (conj nodes-no-incoming-edges k)))				
					(if (empty? (rest ks))
						(recur (rest ks) nil nil nil nodes-no-incoming-edges) 
						(recur (rest ks) (first (rest ks)) (set ((simple-eqs-map (first (rest ks))) :params)) (clset/intersection (set ((simple-eqs-map (first (rest ks))) :params)) keys-as-set) nodes-no-incoming-edges)))))))

(defn remove-edges [simple-eqs-map node]
	(apply merge
		(for [vec-pair simple-eqs-map]
			(if (contains? (set (:params (second vec-pair))) node)
				{(first vec-pair) (assoc-in (second vec-pair) [:params] (vec (disj (set (:params (second vec-pair))) node)))}
				{(first vec-pair) (second vec-pair)}))))					
					
(defn topol-sort [simple-eqs-map]
	(let [S (find-no-incoming-edges simple-eqs-map)]
		(loop [nodes S node (first nodes) L [] m simple-eqs-map all-edge-free-nodes #{node}]
			(if (empty? nodes)
				L
				(let [new-map (remove-edges m node)
					  new-edge-free-nodes (clset/difference (find-no-incoming-edges new-map) all-edge-free-nodes)]
					(if (empty? new-edge-free-nodes)
						(recur (disj nodes node) (first (disj nodes node)) (conj L node) new-map all-edge-free-nodes)
						(recur (apply conj (disj nodes node) new-edge-free-nodes) (first (apply conj (disj nodes node) new-edge-free-nodes)) (conj L node) new-map (apply conj all-edge-free-nodes new-edge-free-nodes))))))))

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
(defn create-params-vector 
	([system-map]
		(mapv :params (vals system-map)))
	([system-map ordered-simple-eqs]
		(mapv :params (map #(% system-map) ordered-simple-eqs))))	
	
;returns a vector of of functions
(defn create-fn-vector 
	([system-map]
		(mapv :func (vals system-map)))
	([system-map ordered-simple-eqs]
		(mapv :func (map #(% system-map) ordered-simple-eqs))))				
	
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

		
(defn serial-integration [start end step system-map fileValues]
	(let  [whole-system-keys (keys system-map)
		   init-vals-map (create-init-vals-map system-map)
		   value-map (create-value-map system-map init-vals-map) ;this is a TRANSIENT map with TRANSIENT values
		   value-map (reduce #(assoc! % (first %2) (transient (second %2))) value-map fileValues)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   simple-eqs-map (remove-elements true system-map)
		   ordered-simple-eqs (topol-sort simple-eqs-map)
		   system-map (remove-elements false system-map)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   params-vector (create-params-vector system-map)
		   simple-eqs-params-vector (create-params-vector simple-eqs-map ordered-simple-eqs)
		   fn-vector (create-fn-vector system-map)
		   simple-eqs-fn-vector (create-fn-vector simple-eqs-map ordered-simple-eqs)
		   system-map-keys (keys system-map)
		   simple-eqs-map-keys ordered-simple-eqs
		   iterations (long (quot (- end start) step))
		   final-value-map (loop [iter 0 vm value-map]
								(if (< iter iterations)
									(let [value-map-after-diffs-assoced (loop [ks system-map-keys m vm pv params-vector fv fn-vector]
																			(if (empty? ks)
																				m
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (euler-method iter step m (first ks) (first pv) (first fv)))) (next pv) (next fv))))									
										  value-map-after-eqs-assoced (loop [ks ordered-simple-eqs m value-map-after-diffs-assoced params-vec simple-eqs-params-vector fn-vec simple-eqs-fn-vector]
																			(if (empty? ks)
																				m
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (calc-func (inc iter) (first ks) m (first params-vec) (first fn-vec)))) (next params-vec) (next fn-vec))))]
									(recur (inc iter) value-map-after-eqs-assoced))
									vm))
			pers-value-map (persistent! final-value-map)
			pers-vectors (map #(persistent! %) (vals pers-value-map))]
		(zipmap (keys pers-value-map) pers-vectors)))			
	
	
(defn dependent-integration [start end step system-map produced-values-map fileValues]
	(let  [outer-dependencies (filter #(nil? (system-map %)) (set (flatten (map :params (vals system-map)))))
		   outer-dependencies-produced-values-map (zipmap outer-dependencies (map #(produced-values-map %) outer-dependencies))
		   whole-system-keys (keys system-map)
		   init-vals-map (create-init-vals-map system-map)
		   value-map (create-value-map system-map init-vals-map)
		   value-map (reduce #(assoc! % (first %2) (transient (second %2))) value-map fileValues)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   simple-eqs-map (remove-elements true system-map)
		   ordered-simple-eqs (topol-sort simple-eqs-map)
		   system-map (remove-elements false system-map)
		   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
		   params-vector (create-params-vector system-map)
		   simple-eqs-params-vector (create-params-vector simple-eqs-map ordered-simple-eqs)
		   fn-vector (create-fn-vector system-map)
		   simple-eqs-fn-vector (create-fn-vector simple-eqs-map ordered-simple-eqs)
		   system-map-keys (keys system-map)
		   simple-eqs-map-keys ordered-simple-eqs
		   iterations (long (quot (- end start) step))
		   final-value-map (loop [iter 0 vm value-map]
								(if (< iter iterations)
									(let [value-map-after-diffs-assoced (loop [ks system-map-keys m vm pv params-vector fv fn-vector]
																			(if (empty? ks)
																				m
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (euler-method iter step m outer-dependencies-produced-values-map (first ks) (first pv) (first fv)))) (next pv) (next fv))))
										  value-map-after-eqs-assoced (loop [ks ordered-simple-eqs m value-map-after-diffs-assoced params-vec simple-eqs-params-vector fn-vec simple-eqs-fn-vector]
																			(if (empty? ks)
																				m
																				(recur (next ks) (assoc! m (first ks) (conj! ((first ks) m) (calc-func (inc iter) (first ks) m outer-dependencies-produced-values-map (first params-vec) (first fn-vec)))) (next params-vec) (next fn-vec))))]
									(recur (inc iter) value-map-after-eqs-assoced))
									vm))
			pers-value-map (persistent! final-value-map)
			pers-vectors (map #(persistent! %) (vals pers-value-map))]
		(zipmap (keys pers-value-map) pers-vectors)))			
				 
	
(defn partition-labour [start end step subsystems-map system-map fileValues]
	(let [gos (doall ;without doall only 1 go starts
					(map #(async/go (serial-integration start end step % fileValues)) (:independent subsystems-map)))
		  independent-result (apply merge (map #(async/<!! %) gos))
		  dependent-result (dependent-integration start end step (:dependent subsystems-map) independent-result fileValues)
		  merged-results (merge independent-result dependent-result)]
		  (zipmap (keys merged-results) (vals merged-results))))				 
		  
