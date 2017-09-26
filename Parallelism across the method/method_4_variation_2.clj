(defn repeatedly*
  "Like `repeatedly` but faster and returns given collection type."
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

(defn work-sharing [funcs]
  (let [core-num (.availableProcessors (Runtime/getRuntime)) ;idaniko einai xwris to +2
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn calc-new-value [func iteration storage-func]
  (apply (peek func) (map #(deref ((deref %) iteration)) (butlast storage-func))))

(defn euler-method [iteration step func storage-func]
  (+ @(@(peek storage-func) iteration) (* step (calc-new-value func iteration storage-func))))

(defn update-atom [iteration step func storage-func]  
  (swap! (peek storage-func) assoc (+ iteration 1) (deliver (@(peek storage-func) (+ iteration 1)) (euler-method iteration step func storage-func))))

(defn calc-funcs-in-chunk [iterations step chunk storage-chunk]                              
  (loop [iteration 0]
    (when-not (= iteration iterations)
      (loop [c chunk s-c storage-chunk]
        (when (seq c)
          (update-atom iteration step (first c) (first s-c))
          (recur (rest c) (rest s-c))))
      (recur (inc iteration)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (where-to-save [[[0 'f 'h 'f][0 'e 'g][0 'w 'h]], [[0 'r][0 'e][0 'w]]])
; (([_f _h _f] [_e _g] [_w _h]) ([_r] [_e] [_w]))

(defn where-to-save [partitions fn-atom-map]
  (for [partition partitions]
    (for [function partition]
      (doall 
        (mapv #(fn-atom-map %) (rest function))))))

(defn create-value-vec [func iterations]
  (let [v (repeatedly* [] (+ iterations 1) promise)]
    (deliver (v 0) (first func))
    v))

(defn create-atoms [funcs iterations]
  (mapv #(atom (create-value-vec % iterations)) funcs))

(defn create-fns-atoms-map [funcs atoms]
  (zipmap (map peek funcs) atoms))

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

  