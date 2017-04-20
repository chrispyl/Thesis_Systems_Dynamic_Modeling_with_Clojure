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
  (let [core-num (+ (.availableProcessors (Runtime/getRuntime)) 2)
        funcs-num (count funcs)]
    (cond 
      (<= funcs-num core-num) (partition 1 funcs)
      (> funcs-num core-num) (if (> (mod funcs-num core-num) 0)
                               (let [initial-division (partition-all (quot funcs-num core-num) funcs)]
                                 (concat
                                   (drop (mod funcs-num core-num) (take core-num initial-division))
                                   (map conj initial-division (take-last (mod funcs-num core-num) funcs))))
                               (partition-all (quot funcs-num core-num) funcs)))))

(defn create-val-map [funcs iterations]
  (zipmap (map peek funcs) (map (fn [v f]
                                  (deliver (nth v 0) (first f))
                                  v) 
                                (repeatedly* [] 
                                             (count funcs) 
                                             (fn [] (repeatedly* [] (+ iterations 1) promise))) 
                                funcs)))

(defn calc-new-value [function val-map iteration]
  (apply (peek function) 
         (doall 
           (map #(deref ((@val-map %) iteration)) (rest (butlast function))))))

(defn euler [function val-map iteration step]
  (+ @((@val-map (peek function)) iteration) (* step (calc-new-value function val-map iteration))))

(defn update-map [function val-map new-val iteration]
  (swap! val-map assoc-in [(peek function) (+ iteration 1)] (deliver ((@val-map (peek function)) (+ iteration 1)) new-val)))

(defn calc-funcs [val-map iterations step chunk]
  (loop [i 0]
    (if (= i iterations)
      :end
      (do
        (doall 
          (map #(update-map % val-map (euler % val-map i step) i) chunk))
        (recur (inc i))))))

(defn synchronous-shared [start end step & funcs]
  (let [iterations (long (quot (- end start) step))
        val-map (atom (create-val-map funcs iterations))
        chunks (work-sharing funcs)]
    (doall 
      (map #(future (calc-funcs val-map iterations step %)) chunks))
    (doall 
      (map #(deref (last %)) (vals @val-map)))))

  