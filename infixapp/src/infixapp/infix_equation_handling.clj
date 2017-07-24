(in-ns 'infixapp.core)			  

(defn is-operator? [s]
	(if (empty? (filter #(= s %) ["=" "+" "-" "*" "/" "abs" "signum" "**" "exp" "log" "e" "ð" "sqrt" "fact" "sin" "cos" "tan" "asin" "acos" "atan" "sinh" "cosh" "tanh" "min" "max" "ln" "floor"])) 
		false true))

(defn is-number? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))	

(defn remove-quote [s]
	(apply str (butlast s)))	  

(defn find-equation-init-val [s]
	(if (str/includes? s "#")
		(let [v (str/split s #"#")
			  equation (first v)
			  init-val (Float/parseFloat (last v))]
			  [equation init-val])
		[s :no-val]))		

(defn find-name-params [s]
	(let [v (str/split s #"[\s\(\)\,\=\+\-\*\/\%]") ;even though we have removed the whitespaces, some are inserted again due to care-of-division and so we have to break there to		
		  names (filter #(not (empty? %)) ;without it, it keeps the "" occuring
							(filter #(not (or (is-number? %) (is-operator? %))) v))
		  without-duplicates (set (rest names));if it depends from itself it will also be here
		  f-name (if (str/ends-with? (first names) "'")
						(remove-quote (first names))
						(first names))] 
		  [(keyword f-name) (map keyword without-duplicates)]))	;the first element will be the name of the function	  
		  
;returns [function-name parameters] where function-name is string and the contents of 'parameters' are strings strings
;input string function MUST have quote on function name
(defn find-function-elements [s]
	(let [[equation init-val] (find-equation-init-val s)
		  [func-name params] (find-name-params equation)]
		  [func-name params init-val equation])) 																		

(defn create-function [expr]
  (if-let [f (parse-all expression expr)]
	(let [extended-env (merge base-env {:min (fn [x y] (if (< x y) x y)) :max (fn [x y] (if (> x y) x y)) :ln (fn [x] (Math/log x)) :floor (fn [x] (long (Math/floor x)))})]
		(fn [params]
		  (f (merge extended-env params))))))										 
	  	  
;returns map with properties of function
;input is a full equation string. Example "hypot = sqrt( x ** 2 + y ** 2)"	  
(defn create-function-map [s]
	(let [[func-name params init-val equation] (find-function-elements s)
		  expr (last (str/split equation #"="))]
		  {func-name {:init-val init-val :params (vec params) :func (create-function expr) :differential (not (keyword? init-val))}}))  ;here func-name is a keyword

(defn is-constant? [s]
	(is-number? 
		(second (str/split s #"="))))

(defn format-if-neg [constant]
	(let [val-str (second (str/split constant #"="))]
		(if (str/includes? val-str "-")
			(apply str "(" val-str ")")
			val-str)))	

(defn remove-spaces [strings]
	(for [s strings]
		(let [s (str/replace s #"\s" "")]
			s)))	 						
			
(defn substitute [eqs names constants-map]
	(let [infix-equations ["abs" "signum" "exp" "log" "sqrt" "fact" "sin" "cos" "tan" "asin" "acos" "atan" "sinh" "cosh" "tanh" "min" "max" "ln" "floor"]
		  names-no-quotes (map #(if (str/ends-with? % "'") (remove-quote %) %) names)
		  terms (apply conj infix-equations names-no-quotes)
		  repeats (take (count terms) (iterate inc 1))
		  substitutes (map #(apply str (repeat % "@")) repeats)
		  substitute-map (zipmap substitutes terms)
		  substituted-with-ats (for [string eqs] ;substituted-with-ats is strings not map
								(loop [s string substs substitutes]
									(if (not (empty? substs))
										(recur (str/replace s (re-pattern (str "\\b" (substitute-map (first substs)) "\\b")) (first substs)) (rest substs))
										s)))
		  inverted-substitute-map (clojure.set/map-invert substitute-map) 
		  keys-whose-value-must-change (map #(inverted-substitute-map %) (keys constants-map)) ;the result is a list of @@@@  @@@@@@@@@@@ @@   
		  altered-substitute-map  (loop [k keys-whose-value-must-change updated-map substitute-map]
									(if (empty? k)
										updated-map
										(do
											(recur (rest k) (assoc updated-map (first k) (constants-map (substitute-map (first k))))))))
		 
		  substitution-complete (for [string substituted-with-ats]
								(loop [s string eq (reverse substitutes)] ;we reverse because we want to start matching from the many, else we would match smaller series of "@" inside bigger ones
									(if (not (empty? eq))
										(recur (str/replace s (first eq) (altered-substitute-map (first eq))) (rest eq))
										s)))]
		  substitution-complete))				  
		  
(defn replace-constants [strings]
	(let [group-result (group-by is-constant? strings)
		  eqs (group-result false)
		  constants (group-result true)
		  names (map #(first (str/split % #"=")) strings)
		  constant-values-strs (for [c constants] (format-if-neg c))
		  constant-names (map #(first (str/split % #"=")) constants)
		  m (zipmap constant-names constant-values-strs)
		  substituted (substitute eqs names m)]
		 substituted))

(defn care-of-neg-signs [strings]
	(for [s strings]
		(let [internals-replaced (str/replace s #"\(\-" "(0-")
			  internals-replaced (str/replace internals-replaced #",-" ",(0-1)*")]
			(str/replace internals-replaced #"\=\-" "=(0-1)*"))))					
			
(defn calc-init-vals [eqs-map-no-init-values diff-eqs-map fileValues]
	(loop [ks (keys eqs-map-no-init-values) m (merge eqs-map-no-init-values diff-eqs-map)] 
		(if ks 
			(let [params (:params ((first ks) m))
				  f (:func ((first ks) m))
				  values (map #(if (fileValues %) 
								((fileValues %) 0) 
								(:init-val (% m)))
							  params)]
				  (if (contains? (set values) :no-val)
					(recur (conj (vec (next ks)) (first ks)) m) ;we make it vec, because below we need to conj elements at the back to process them later
					(recur (next ks) (assoc-in m [(first ks) :init-val] (f (zipmap params values))))))
			(apply dissoc m (keys diff-eqs-map)))))
					
(defn remove-empty-strings [strings]
	(filter #(not (empty? %)) strings))

(defn care-of-division [strings]
	(for [s strings]
		(str/replace s #"\/0\." "/ 0.")))	

(defn strings-to-function-map-parallel [equations]
	(let [cores (.availableProcessors (Runtime/getRuntime))
		  groups (if (>= (quot (count equations) cores) 1)
									(partition-all cores equations)
									(partition-all (count equations) equations))
		  gos	(doall 
				  (map #(async/go (apply merge (map create-function-map %))) groups))
		  diff-eqs-map	(apply merge (map #(async/<!! %) gos))]
		  diff-eqs-map))		
		
;input is a vector or sequence		  
(defn create-system-map [strings fileValues]
	(let [removed-empty-strings (remove-empty-strings strings)
		  no-spaces (remove-spaces removed-empty-strings)
		  constants-replaced (replace-constants no-spaces) ;does not include constants
		  care-negs-replaced (care-of-neg-signs constants-replaced)
		  care-division (care-of-division care-negs-replaced)
		  group-result (group-by #(str/includes? % "#") care-division)
		  diff-eqs (group-result true)
		  eqs (group-result false)
		  diff-eqs-map (strings-to-function-map-parallel diff-eqs)
		  eqs-map-no-init-values (strings-to-function-map-parallel eqs)
		  eqs-map (calc-init-vals eqs-map-no-init-values diff-eqs-map fileValues)]
		(merge diff-eqs-map eqs-map)))		
