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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;block thread until value of element in position "iteration" is not nil
(defn block-thread [iteration var-f]
  (while (nil? (@@var-f iteration))))

(defn request-value [var-f iteration]
  (let [val (@@var-f iteration)]
    (if (nil? val)
      (do
        (block-thread iteration var-f)
        (@@var-f iteration))
      val)))

(defn calc-new-value [func iteration storage-func]
  (apply (peek func) (map #(request-value % iteration) (butlast storage-func))))

(defn euler-method [iteration step func storage-func]
  (+ (@@(peek storage-func) iteration) (* step (calc-new-value func iteration storage-func))))

(defn update-atom [iteration step func storage-func] 
  ;(println "first" (class (peek storage-func)))
  ;(println "second" (class @(peek storage-func)))
  ;(println "third" (class @@(peek storage-func)))
  ;(println "peek storage-func is " (peek storage-func) "end")
  ;(println "deliver to " (@@(peek storage-func) (+ iteration 1)))
  ;(println "class is " (class @(peek storage-func)))
  ;(println "delivering this " (deliver (@@(peek storage-func) (+ iteration 1)) (euler-method iteration step func storage-func)))
  (swap! @(peek storage-func) assoc (+ iteration 1) (euler-method iteration step func storage-func)))

(defn calc-funcs-in-chunk [iterations step chunk storage-chunk]
  (loop [iteration 0]
    (if (= iteration iterations)
      :end
      (do
        (doall
          (map #(update-atom iteration step % %2) chunk storage-chunk))
        (recur (inc iteration))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (where-to-save [[[0 'f 'h 'f][0 'e 'g][0 'w 'h]], [[0 'r][0 'e][0 'w]]])
; (([_f _h _f] [_e _g] [_w _h]) ([_r] [_e] [_w]))

(defn where-to-save [partitions fn-var-map]
  (for [partition partitions]
    (for [function partition]
      (doall 
        (mapv #(fn-var-map %) (rest function))))))

(defn create-value-vec [func iterations]
  (let [v (repeatedly* [] (+ iterations 1) (fn [] nil))]
    (assoc v 0 (first func))))

(defn dash-func-names [funcs]
  (doall (map #(str "_" (peek %)) funcs)))

(defn define-vars [names funcs iterations]
    (doall (map #(intern *ns* (symbol %) (atom (create-value-vec %2 iterations))) names funcs)))

(defn create-fns-vars-map [funcs vars]
  (zipmap (map peek funcs) vars))

(defn synchronous-shared [start end step & funcs]
  (let [iterations (long (quot (- end start) step))
        chunks (work-sharing funcs)
        dashed-names (dash-func-names funcs)
        vars (define-vars dashed-names funcs iterations)
        fns-vars-map (create-fns-vars-map funcs vars)
        storage (where-to-save chunks fns-vars-map)]
    (doall 
      (map #(future (calc-funcs-in-chunk iterations step % %2)) chunks storage))
    (doall 
      (map #(request-value % iterations) vars)))) ;take last values, we do it this way because values already exist

  