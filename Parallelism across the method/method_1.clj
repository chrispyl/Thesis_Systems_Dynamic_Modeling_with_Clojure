;; 'pair' a vector formatted as [fn value]
;; 'fn-vals' the atom which holds the map that associates functions to values 
;; Updates the map the atom holds by putting the value from pair to the end of the vector of fn
(defn update-fn-vals [pair fn-vals]
  (swap! fn-vals assoc-in [(first pair) (count (@fn-vals (first pair)))] (peek pair)))

;; 'function' a function formatted as [initial-value fn1 fn2 ... fn]
;; 'fn-vals-map' the map which associates functions to values 
;; Calculates the given function
(defn calc-fn [function fn-vals-map] 
  (apply (peek function) (doall (map #(peek (fn-vals-map %)) (butlast (next function)))))) ;(butlast (next function)) is used to take out the intial-value and function name from the [initial-value fn1 fn2 ... fn]. After that we can search fn-vals with fn1 fn2 ... as keys

;; 'prev-val' a vector of with the values previously produced for the corresponding function
;; 'step' the step of the integration 
;; 'function' a function formatted as [initial-value fn1 fn2 ... fn]
;; 'fn-vals-map' the map which associates functions to values 
;; Applies the Euler method for the function given
(defn euler [prev-vals step function fn-vals-map]
  (+ (peek prev-vals) (* step (calc-fn function fn-vals-map)))) ;peek is used to get the most recent (from previous iteration) value

;; 'fn-chunk' a pack of functions each one formatted as [initial-value fn1 fn2 ... fn]
;; 'step' the step of the integration
;; 'fn-vals-map' the map which associates functions to values 
;; For all the functions included in the chunk, calculates the new values and creates pairs
;; which are vectors. Each pair's first element is a function to which we applied the Euler 
;; method to get a new value. The second element is the new value.
(defn process-chunk [fn-chunk step fn-vals-map]
  (doall (map #(into [] [(peek %) (euler (fn-vals-map (peek %)) step % fn-vals-map)]) fn-chunk))) ; map returns ([f 5] [h 3] [g 6] ...)
  
;; Returns the number of available logical processors plus 2. We must know this number 
;; because we want to assign each chunk to a future and we want to use all the available
;; threads in the future threadpool. The default size of the threadpool is this number.
(defn avail-proc []
  (+ (.availableProcessors (Runtime/getRuntime)) 2))

;; 'vals' the values that need to be chunked
;; Partitions the values provided, depending on the number of cpu threads
;; This is done by checking the number of the values provided. If they are less 
;; than the available threads there will be one value in each chunk. Else
;; the division total_vals/total_threads is made to determine the chunk size. 
;; If the division is perfect the chunk size is the quotent of it, else it is the
;; quotent+1. This is done to avoid circumstances such as the following example
;;
;; total_threads=4
;; total_vals=6
;; total_vals/total_threads=1
;; 
;; So there would be 6 chunks with one value each. This is undesired because each
;; afterward each chunk will be assigned to a thread and we won't have enough 
;; threads. Also, it leaves threads with not much work to do. By increasing the
;; quotent by one there are no excess chunks and more work is assigned to each threads.
(defn make-chunks [vals]
  (if (<= (count vals) (avail-proc))
    (partition 1 vals)
    (partition-all (if (> (mod (count vals) (avail-proc)) 0)
                     (inc (quot (count vals) (avail-proc)))
                     (quot (count vals) (avail-proc))) 
                   vals)))

;; 'functions' the input functions formatted as [initial-value fn1 fn2 ... fn]
;; Creates an ArrayMap which associates functions to the corresponding initial values
;; fns are the keys and initial-values the values.
(defn create-vals-map [functions]
  (zipmap (doall (map #(peek %) functions)) (doall (map #(vector (first %)) functions)))) ;The first map looks at the back of each function to get all the function names. The second gets all the initial values. Zipmap makes the association

;; 'start' where the Euler integration starts
;; 'end' where the Euler integration ends
;; 'step' the step of the integration 
;; 'funcs' input functions in the format [initial-value fn1 fn2 ... fn] where fn1, fn2 
;; are the functions that produce the values fn accepts in the order fn accepts them.
;; The logic is to pack input functions into chunks and process each chunk in a seperate
;; thread with future. Then, wait for all futures to be completed and start the 
;; next iteration.
(defn parallel-euler-non-linear [start end step & funcs]
  (let [fn-vals (atom (create-vals-map funcs)) ;fn-vals is an atom that holds an ArrayMap. This ArrayMap associates the given functions with all the values each function producesz
        fn-chunks (make-chunks funcs)] ;fn-chunks is List that contains all the functions partitioned into chunks, each one in a sublist
    (loop [i start] ;loop from start to end, this loop will happen (end-start)*step times
      (if (>= i end) 
        (doall (map (fn [k] [k (peek (@fn-vals k))]) (keys @fn-vals))) ;before exiting the loop return the values located at end of the vectors from fn-vals which are the final values of the Euler method
        (do         
          (let [pairs (doall (map #(future (process-chunk % step @fn-vals)) fn-chunks))] ;each function chunk is processed in another thread, the result is a list of sublists, with each sublist containing pairs of type [fn new-value]      
            (while (some false? (doall (map #(future-done? %) pairs)))) ;wait for the futures to complete
            (doall (map #(update-fn-vals % fn-vals) (apply concat (doall (map deref pairs)))))) ;concantenate the sublists of pairs, so we now have a list of pairs without sublists. Then update fn-vals with the new values
          (recur (+ i step)))))))
  