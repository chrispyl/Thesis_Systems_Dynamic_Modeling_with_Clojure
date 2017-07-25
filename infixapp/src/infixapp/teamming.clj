(in-ns 'infixapp.core)

;checks if vector contains an element
(defn vec-contains? [v target] (some #(= target %) v))

;returns a map of all the ingoing edges of all nodes. output in the form {:x #{:x :y} :y #{}}
;system-map - map
(defn ingoing [system-map]
	(zipmap (keys system-map) (map #(set (:params %)) (vals system-map))))

;returns a map of all the outgoing edges of all nodes. output in the form {:x #{:x :y} :y #{}}
;system-map - map
(defn outgoing [system-map]
	(let [outs (map (fn [k]
					(filter #(vec-contains? ((second %) :params)  k) system-map)) 
						(keys system-map))				
		  partitioned (for [k outs]
						(set 
							(for [vector k]
							 (first vector))))]
		(zipmap (keys system-map) partitioned)))

;helper function for 'dfs' function, decide to pop or conj the stack
;stack - vector
;node-neighbours - set
;visited - set		
(defn pop-or-conj [stack node-neighbours visited]
	(if (clset/subset? node-neighbours visited) 
		(pop stack)
		(conj stack (first (clset/difference node-neighbours visited)))))		

;returns where a single node can reach
;init-node - keyword
;graph - map. Later named as 'out-map' eg {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}																									
(defn dfs [init-node graph]
  (loop [stack [init-node] visited #{} reachability #{}]
    (if (empty? stack)
      reachability
      (recur (pop-or-conj stack (graph (peek stack)) visited) (conj visited (peek stack)) (if (contains? visited (peek stack))
                                                                                            reachability
                                                                                            (reduce conj reachability (graph (peek stack))))))))

;outputs ({:x #{:c :v}}...{}{}{})
;nodes - list of keywords
;graph - map. Later named as 'out-map' eg {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}																							
(defn create-reachability-map [nodes graph]
	(map #(hash-map % (dfs % graph)) nodes))																							

;outputs the functions based on reachabilities only
;output in the form ((:a :b :c #{:c :v})(:x :y #{:a :b})...) where sets are the reachablilities
;reachabilities - map eg ({:x #{:c :v}}...{}{}{})
(defn find-teams [reachabilities]
	(let [sets (map #(first (vals %)) reachabilities) ;first to take the one and only value out of the list vals give
		  no-duplicates (set sets) 
		  before-merging (map 
							(fn [the-set](filter #(= the-set (first (vals %))) reachabilities))
								no-duplicates)] ;creates a list of lists with each list having the same reachabilities
		  (for [team before-merging]
			(conj
				(for [m team]
					(first (keys m))) (first (vals (first team)))))))	;first to take the one and only value out of the list keys give, same for the second first

;returns the independent teams
;teams - list of lists. Each sublist has as first element the reachability of the team
;in-map - map	eg. {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}					
(defn find-independent-teams [teams in-map]
	(let [independent (filter      
						(fn [team](every? true? (map #(clset/subset? (in-map %) (first team)) (drop 1 team)))) ;drop because the forst element is the reachability of the team
							teams)]
		independent))				

;returns the dependent teams
;teams - list of lists. Each sublist has as first element the reachability of the team
;independent-teams - list of lists. Each sublist has as first element the reachability of the team
(defn find-dependent-teams [independent-teams teams]
	(into () ;because independent is also a list
		(clset/difference (set teams) (set independent-teams))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		FOLLOWS TEAM EXPANSION
;returns the independent teams without the removed node
;node - keyword
;dependent-teams - list of lists  	
(defn remove-from-previous-dep-team [node dependent-teams]
	(for [team dependent-teams]
		(let [team (set team)] ;convert to set so we can search idiomatically
			(if (node team)
				(into () (disj team node))
				(into () team)))))			

;returns the dependent teams without the removed team
;dependent-team - list 				
;dependent-teams - list of lists
(defn remove-dep-team-from-dep-teams [dependent-team dependent-teams] ;to ena einai (:aw :c) kai to allo (:c :aw) gia auto den ginetai to disjoint
	(let [dependent-team-as-set (set dependent-team) ;convert to set to help the disj. If not the disj could try to e.g (disj #{(:aw :c)} (:c :aw)) which wouldn't work because the elements in the lists are not in the correct order 
		 dependent-teams-as-sets (map set dependent-teams)
		 dep-team-removed (disj (set dependent-teams-as-sets) dependent-team-as-set)
		 dep-team-removed-back-into-lists (map #(into () %) dep-team-removed)] ;revert the contained teams back to lists, which were sets before
		dep-team-removed-back-into-lists))

;returns boolean
;checks if a node depends from nodes outside of the independent team examined
;node - keyword
;team - list
;in-map - map	eg. {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}
(defn node-has-other-dependencies? [node team in-map]
	(not (clset/subset? (node in-map) (set team))))				

;returns the dependent team a node belongs to
;node - keyword
;dependent-teams - list of lists	
(defn dependent-team-of-node [node dependent-teams]
	(into () (first (filter #(node %) (map set dependent-teams)))))	

;returns boolean
;checks if a dependent team depends from nodes outside of the independent team examined	
(defn dependent-team-has-other-dependencies? [dependent-team team in-map]
	(not (every? true? (map #(clset/subset? (% in-map) (set (concat team dependent-team))) dependent-team))))	;the logic is to look at the independent team with the dependent team combined and check if this can stand as an indie

;returns a vector [recruited after-removal-dep-teams] where recruited can be a node in list, or a dependent team and after-removal-dep-teams is a list with the dependent teams updated after the removed nodes
;node - keyword
;in-map - map	eg. {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}
;out-map - map	eg. {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}
;dependent-teams - list of lists
;new-team - list
;Here 'node' is a member of the independet team. We check to see if its neighbours can be taken into the indie team. 'new-team' is the indie team examined with the recruits added as the process goes.	
;If a recruit is found we stop and return it because there situations where the order of check matters for them to come in team
;a'=a+1
;b'=a
;c'=a+b
;They can form a team but only if b is checked before c. If c is checked first it will be shown that it also depends from another dep team and wont go in team. That's why we restart and check all again.
(defn recruit-node-or-dep-team [node in-map out-map dependent-teams new-team]
	(loop [node-neighbours (node out-map) node-neighbour (first node-neighbours) recruited () after-removal-dep-teams dependent-teams]
		(if (empty? node-neighbours) ;if there aren't any neighbours left return what you found
			[recruited after-removal-dep-teams]
			(if (node-neighbour (set new-team)) ;if it belongs to the independent team skip and check rest neighbours, we dont check if it belongs to other indie teams because this case is impossible
				(recur (rest node-neighbours) (first (rest node-neighbours)) () after-removal-dep-teams)																							
				(let [dep-team-of-node-neighbour (dependent-team-of-node node-neighbour dependent-teams)
					  dep-team-of-node-has-other-dependencies? (dependent-team-has-other-dependencies? (dependent-team-of-node node-neighbour dependent-teams) new-team in-map)
					  node-of-dependent-team-has-other-dependencies? (node-has-other-dependencies? node-neighbour new-team in-map)] ;this can be a node of a dependent team, or a node that is a dependent team by itself														  
					(if (not (empty? recruited)) ;if a recruit was found return what you found
						[recruited after-removal-dep-teams] ;if no recruit found nothing is removed	from after-removal-dep-teams														  
						(if (not dep-team-of-node-has-other-dependencies?)
							(recur nil nil dep-team-of-node-neighbour (remove-dep-team-from-dep-teams dep-team-of-node-neighbour dependent-teams)) ;nils to show that we dont care about that as the loops will stop
							(if (not node-of-dependent-team-has-other-dependencies?)
								(recur nil nil (list node-neighbour) (remove-from-previous-dep-team node-neighbour dependent-teams)) ;wrap it because its easier if it always handles a list
								(recur (rest node-neighbours) (first (rest node-neighbours)) () after-removal-dep-teams))))))))) ;will be empty if not found, if found won't reach here

								
;returns a vector where the first element is the expanded indie teams and the second the updated dependent teams
;in-map - map	eg. {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}
;out-map - map	eg. {:x #{:y :x}, :y #{:y :x}, :a #{:b :a}, :b #{:b :a}, :k #{:m :k}, :m #{:m :k}, :z #{:o :b}, :w #{:m :z}, :o #{:y :w}}
;independent-teams - list of lists eg ((:k :m) (:a :b) (:x :y))
;dependent-teams - list of lists eg ((:k :m) (:a :b) (:x :y))
(defn expand-indie-teams [independent-teams dependent-teams in-map out-map]
	(loop [indie-teams independent-teams dep-teams dependent-teams new-indies ()] ;for every indie team
		(if (empty? indie-teams)
			[new-indies dep-teams]
			(let [[team new-dep-teams] (loop [indie-team-nodes (first indie-teams) d-teams dep-teams new-team (first indie-teams) node (first indie-team-nodes)] ;nodes is a list of keywords, (first indies-teams) is one team
										(if (empty? indie-team-nodes)																							 ;new-team will hold the indie nodes with the new recruits as the process goes
											[new-team d-teams]																									 ;that's because we need something to hold all the nodes of the team and we can't rely on indie-team-nodes because we rest it
											(let [[recruited after-removal-dep-teams] (recruit-node-or-dep-team node in-map out-map d-teams new-team)]
												(if (empty? recruited)
													(recur (rest indie-team-nodes) d-teams new-team (first (rest indie-team-nodes)))
													(recur (concat new-team recruited) after-removal-dep-teams (concat new-team recruited) (first (concat new-team recruited)))))))]
				(recur (rest indie-teams) new-dep-teams (conj new-indies team))))))
				
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;					

;returns the system-map given in input but all of the function paramterer vector won't include dependencies from variables included in csv files
;system-map - map
;fileValues - map
(defn remove-file-dependencies [system-map fileValues]
	(zipmap (keys system-map) (map #(update-in % [:params] (fn [params] (vec (apply disj (set params) (keys fileValues))))) (vals system-map))))

;returns a map of the form {:independent ((...)(...)(...)) :dependent ((...)(...)(...))} where sublists represent teams
;system-map - map		
(defn create-team-map [system-map fileValues]
	(let [system-map (remove-file-dependencies system-map fileValues)
		  in-map (ingoing system-map)
		  out-map  (outgoing system-map)
		  reachabilities (create-reachability-map (keys system-map) out-map) ;reachability-map in the form ({:x #{:c :v}}...{}{}{})
		  teams (find-teams reachabilities)
		  independent-teams (find-independent-teams teams in-map)
		  dependent-teams (find-dependent-teams independent-teams teams)
		  [independent-teams dependent-teams] (expand-indie-teams (map #(drop 1 %) independent-teams) (map #(drop 1 %) dependent-teams) in-map out-map)]
		  (zipmap [:independent :dependent] [independent-teams dependent-teams]))) 

;part is (:a :b :c) and shows a team
;output {:a {:params [...] :func ...} :b {:params [...] :func ...}}
(defn create-subsystem [team system-map]
	(zipmap team (map #(system-map %) team)))		  	

;distributes work depending on available cores
;tasks - list, vector
;cores - number 	
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

;returns a map of in the form {:independent ((...)(...)(...)) :dependent ((...)(...)(...))} where sublists represent subsystem meaning that they can be combined teams if there aren't enough threads to run them seperately
;teams - map (the result of 'create-team-map')
;system-map - map							   
(defn create-subsystem-map [teams system-map]
	(let [cores (.availableProcessors (Runtime/getRuntime))
	      cores-for-independent cores
		  partitioned-independent-teams (work-sharing (:independent teams) cores-for-independent)
		  independent-subsystems (for [subsystem partitioned-independent-teams]
						(create-subsystem (flatten subsystem) system-map))
		  dependent-subsystem (create-subsystem (flatten (:dependent teams)) system-map)] ;all the dependent teams are going into the same subsystem, so 'flatten' groups them into the same list
	(zipmap [:independent :dependent] [independent-subsystems dependent-subsystem])))

		  
		  
		  