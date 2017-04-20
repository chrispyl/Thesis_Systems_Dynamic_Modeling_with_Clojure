;; 'func' a function formatted as [initial-value param0 para1 .... function-name]
;; 'function-number' the position of the function in the initial input
;; 'param-positions' a vector of vectors that helps us know from where the 'func's' parameter values will be taken
;; 'iteration' the current iteration of the Euler integration
;; 'nv' a vector of vectors containong all the produced values in each step. The original was named 'v' i the code and because we copy it, we name the copy 'nv' from 'new v'
;; Calculates the value of function 'func'.
(defn calc-function [func function-number param-positions iteration nv]
  (apply (peek func) (doall (map #((nv %) iteration) (param-positions function-number))))) ;(nv %) we take a subvector from this, and the element in position 'iteration' of the subvector from this (((nv %) iteration))
                                                                                           ;(param-positions function-number) is a vector containing the positions of the subvectors in 'nv' where the values we need are

;; 'nv' a vector of vectors containong all the produced values in each step. The original was named 'v' i the code and because we copy it, we name the copy 'nv' from 'new v'
;; 'function-number' the position of the function in the initial input
;; 'param-positions' a vector of vectors that helps us know from where the 'func's' parameter values will be taken
;; 'sv' a sub-vector of nv which has the produced values of the function we are applying the Euler method at
;; 'iteration' the current iteration of the Euler integration
;; 'func' a function formatted as [initial-value param0 para1 .... function-name]
;; 'step' the step of the integration 
;; Calculates the new value of a function with the Euler method x(t+1)=x(t)+h*F(x(t),t)
(defn euler [nv function-number param-positions sv iteration func step]
  (+ (sv iteration) (* step (calc-function func function-number param-positions iteration nv))))

;; 'funcs' the input functions, each one formatted as [initial-value param0 para1 .... function-name]
;; 'f-positions' a map with functions as keys and the numbers of the functions in the order they were given as values
(defn param-pos [funcs f-positions]
  (doall (mapv (fn [f] (doall (mapv #(f-positions %) (rest (butlast f))))) funcs)))

;; 'funcs' the input functions, each one formatted as [initial-value param0 para1 .... function-name]
;; Creates a vector of subvectors and in each subvector puts the initial value of a function
(defn init-vecs [funcs]
  (mapv (fn [f] [(first f)]) funcs))

;; 'funcs' the input functions, each one formatted as [initial-value param0 para1 .... function-name]
;; creates a map with functions as keys and the numbers of the functions in the order they were given as values
(defn f-pos [funcs]
  (zipmap (doall (map peek funcs)) (range)))

;; 'start' where the Euler integration starts
;; 'end' where the Euler integration ends
;; 'step' the step of the integration 
;; 'funcs' input functions in the format [initial-value fn1 fn2 ... fn] where fn1, fn2 
;; are the functions that produce the values fn accepts in the order fn accepts them.
;; Applies the euler method to a system of equations and returns the resuls. It works
;; in a single thread, so it process each function sequentially. The data structure
;; holding the values is a vector of vectors. To find the 
(defn one-thread-non-linear [start end step & funcs]
  (let [funcs (into [] funcs) ;needed in order to use the vectors' associtative property later (funcs j), when we want to take a function by its' index 
        f-positions (f-pos funcs)
        param-positions (param-pos funcs f-positions)
        iterations (long (quot (- end start) step))] ;(quot (- end start) step) creates a Double, but Doubles can't equal Longs later in (= i iterations) so we convert it
    (loop [i 0 v (init-vecs funcs)]
      (if (= i iterations)
        (doall (map #(peek %) v)) ;return the last value of each function
        (recur (inc i) (loop [j 0 nv v] ;using another loop-recur because we want to make assocs in different places
                         (if (= j (count param-positions))
                           nv
                           (recur (inc j) (assoc nv j (conj (nv j) (euler nv j param-positions (nv j) i (funcs j) step)))))))))))

