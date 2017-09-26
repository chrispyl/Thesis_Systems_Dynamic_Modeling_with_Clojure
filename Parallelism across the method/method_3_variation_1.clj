; 'coll' returned collection type
; 'n' how many times to repeat
; 'f' the function to be repeated
; Like `repeatedly` but faster and returns given collection type
(defn repeatedly*
  [coll n f]
  (if-not (instance? clojure.lang.IEditableCollection coll)
    (loop [v coll idx 0]
      (if (>= idx n)
        v
        (recur (conj v (f)) (inc idx))))
    (loop [v (transient coll) idx 0]
      (if (>= idx n)
        (persistent! v)
        (recur (conj! v (f)) (inc idx))))))

; 'funcs' the input functions in the form [initial-value args function]
; Partitions the functions to chunks evenly sized chunks, the number of the chunks depends on the available logical cores
(defn work-sharing [funcs]
  (let [core-num (.availableProcessors (Runtime/getRuntime)) ;ideal without +2
        funcs-num (count funcs)]
    (cond 
      (<= funcs-num core-num) (partition 1 funcs)
      (> funcs-num core-num) (if (> (mod funcs-num core-num) 0)
                               (let [initial-division (partition-all (quot funcs-num core-num) funcs)]
                                 (concat
                                   (drop (mod funcs-num core-num) (take core-num initial-division))
                                   (map conj initial-division (take-last (mod funcs-num core-num) funcs))))
                               (partition-all (quot funcs-num core-num) funcs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 'func' a function in the form [initial-value args function]
; 'iteration' the nuber of the previous iteration, used to let func know from where to find its parameters
; 'storage-func' a vector showing where the parameter values of 'func' can be found 
; Calculates the new value of function 'func'
(defn calc-new-value [func iteration storage-func]
  (apply (peek func) (map #(deref ((deref %) iteration)) (butlast storage-func))))

; 'iteration' the nuber of the previous iteration, used to take the previous value produced by the Euler method
; 'step' the step of the integration
; 'func' a function in the form [initial-value args function]
; 'storage-func' a vector showing where the parameter values of 'func' can be found
; Calculates the new value of a function based on the Euler method
(defn euler-method [iteration step func storage-func]
  (+ @(@(peek storage-func) iteration) (* step (calc-new-value func iteration storage-func))))

; 'iteration' the nuber of the previous iteration
; 'step' the step of the integration
; 'func' a function in the form [initial-value args function]
; 'storage-func' a vector showing where the parameter values of 'func' can be found, its last element shows to which atom the produced values for this function are saved
; Assocs the new value produced by the Euler method to the atom pointed by (peek storage-func)
(defn update-atom [iteration step func storage-func]  
  (swap! (peek storage-func) assoc (+ iteration 1) (deliver (@(peek storage-func) (+ iteration 1)) (euler-method iteration step func storage-func))))

; 'iterations' the total number of iterations
; 'step' the step of the integration
; 'chunk' a list of functions, whose form is [initial-value args function]
; 'storage-chunk' a list containing all the 'storage-func's for this chunk  
; Calculates all the values of all the functions in one chunk
(defn calc-funcs-in-chunk [iterations step chunk storage-chunk]                              
  (loop [iteration 0]
    (when-not (= iteration iterations)
      (loop [c chunk s-c storage-chunk]
        (when (seq c)
          (update-atom iteration step (first c) (first s-c))
          (recur (rest c) (rest s-c))))
      (recur (inc iteration)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (([_f _h _f] [_e _g] [_w _h]) ([_r] [_e] [_w]))

; 'chunks' the chunks produced by work-sharing
; 'fn-atom-map' a map with the input functions as keys and the corresponding atoms as values
; A list of lists. Each sublist consists of vectors. Each vector holds the atoms where a function 
; will save its values. Each sublist corresponds to a chunk and is in the same order as the chunks;
; Outputs something like (([atom_f atom_h atom_f] [atom_e atom_g] [atom_w atom_h]) ([atom_r] [atom_e] [atom_w]))
(defn where-to-save [chunks fn-atom-map]
  (for [chunk chunks]
    (for [function chunk]
      (doall 
        (mapv #(fn-atom-map %) (rest function))))))

; 'func' an input funciton in the form [initial-value args function]
; 'iterations' the total number of iterations
; Creates a vector of size (+ iterations 1) and fills it with promises, the size is (+ iterations 1) 
; because we take into account the initial value
(defn create-value-vec [func iterations]
  (let [v (repeatedly* [] (+ iterations 1) promise)]
    (deliver (v 0) (first func))
    v))

; 'funcs' the input functions in the form [initial-value args function]
; 'iterations' the total number of iterations
; Creates as many atoms as the input functions which will hold the produced values of the Euler method for each function
(defn create-atoms [funcs iterations]
  (mapv #(atom (create-value-vec % iterations)) funcs))

; 'funcs' the input functions in the form [initial-value args function]
; 'atoms' the atoms produced in create-atoms
; Creates a map with the inputs functions as keys and the corresponding atoms as values
(defn create-fns-atoms-map [funcs atoms]
  (zipmap (map peek funcs) atoms))

; 'start' where the integration will start
; 'end' where the integration will end
; 'step' the step of the integration
; 'funcs' the input functions in the form [initial-value args function]
; Distributes the functions to threads 
(defn synchronous-shared [start end step & funcs]
  (let [iterations (long (quot (- end start) step))
        chunks (work-sharing funcs)
        atoms (create-atoms funcs iterations)      
        fns-atoms-map (create-fns-atoms-map funcs atoms)
        storage (where-to-save chunks fns-atoms-map)]
    (doall 
      (map #(future (calc-funcs-in-chunk iterations step % %2)) chunks storage))
    (doall 
      (map #(deref (peek (deref %))) atoms))))