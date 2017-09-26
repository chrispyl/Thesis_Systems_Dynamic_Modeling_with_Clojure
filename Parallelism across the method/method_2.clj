;; 'coll' the returned collection type
;; 'n' how many times to execute the given function
;; 'f' a function of no args
;; Same as repeatedly but faster and returns given collection type.
;; Taken from https://github.com/ptaoussanis/nippy
;; It was preferred from 'repeatedly' because we use it to create a lot of
;; vectors each one having thousands of elements, so we need something fast 
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

;; 'start' where the Euler integration starts
;; 'end' where the Euler integration ends
;; 'step' the step of the integration 
;; Calculates how many iterations will be done for the Euler method
(defn calc-iterations [start end step]
  (quot (- end start) step))

;; 'functions' the given functions that we must integrate with the Euler method
;; each one formatted as [initial-value param0 para1 .... function-name]
;; 'iterations' how many iterations will be done for the Euler method
;; For every given function we create a vector with size equal to the amount of 
;; iterations+1 and filled by promises. The number (iterations+1) arises from the
;; initial-value+iterations. Then, we deliver the initial value of each function
;; to the first element of the corresponding vector. Next, we assign these vectors
;; to agents. Finally, a lazyseq consisting of the above agents is returned.
(defn create-agents [functions iterations] 
    (mapv agent ;assign the vectors to agents. We use mapv because later we will need to search for specific agents based on their number in the vector so we need fast access time
       (map (fn [v f]
              (deliver (nth v 0) (first f)) ;deliver to first element of vector 'v', the initial value of function f
              v) 
            (repeatedly* [] 
                         (count functions) 
                         (fn [] (repeatedly* [] (+ iterations 1) promise))) ;create a vector with size (iterations + 1), filled with promises
            functions)))

;; 'functions' the given functions that we must integrate with the Euler method
;; each one formatted as [initial-value param0 para1 .... function-name]
;; Returns a vector of subvectors. Each subvector corresponds to a function and its'
;; elements are numbers. These numbers show from wich functions, in 'functions' vector,
;; a function will take its parameters. For example, if 'functions' is [[1 h g f][0 g g][2 h f h]]
;; the first subvector correspoding to functio f will be [2 1]. The whole vector
;; returned will be [[2 1][1][2 0]]. We use vectors because of their constant access
;; time to the elements.
(defn create-positions-vector [functions]
  (let [func-params (map #(butlast (next %)) functions) ;a lazyseq with each element the names of the functions whose values another function takes as parameters (h g, g, h f...)
        funcs (map peek functions) ;a lazyseq with each element being the name of a function (f g h...)
        func-map (zipmap funcs (iterate inc 0))] ;an map that associates function names to the order the appear in 'functions' vector. (f 0, g 1, h 2 .....)
    (mapv  ;mapv is used to create a vector and not a lazyseq because we need to access elements fast later
      (fn [params] (mapv #(func-map %) params)) ;this function gives the subvectors [1 2 0] 
      func-params)))

;; 'function' a function formatted as [initial-value param0 para1 .... function-name]
;; 'iteration' the number of the iteration starting from 1 that are currently in
;; 'parameter-subvector' a subvector from the vector created by the function 'create-positions-vector'
;; 'agents' all the agents created by the function 'create-agents'
;; Calculates the value of a function for a given iteration.
(defn calc-function [function iteration parameter-subvector agents]
  (apply (peek function)  ;apply is used because we want to take the parameters seperated, otherwise they will be though as one parameter 
         (map (fn [param] @(@(nth agents param) iteration)) parameter-subvector))) ;tries to take the value of a function pointed by an element of 'parameter-subvector', in position 'iteration'. If it doesn't succeed the thread calculating this will block until the value is ready

;; 'prev-val' the last calculated value of a function
;; 'step' the step of the integration 
;; 'function' a function formatted as [initial-value param0 para1 .... function-name]
;; 'iteration' the number of the iteration starting from 1 that are currently in
;; 'parameter-subvector' a subvector from the vector created by the function 'create-positions-vector'
;; 'agents' all the agents created by the function 'create-agents'
;; Calculates the new value of a function with the Euler method x(t+1)=x(t)+h*F(x(t),t)
(defn euler [prev-val step function iteration parameter-subvector agents]
  (+ prev-val (* step (calc-function function iteration parameter-subvector agents))))

;; 'start' where the Euler integration starts
;; 'end' where the Euler integration ends
;; 'step' the step of the integration 
;; 'funcs' input functions in the format [initial-value fn1 fn2 ... fn] where fn1, fn2 
;; are the functions that produce the values fn accepts in the order fn accepts them.
;; Applies the euler method to a system of equations and returns the resuls. This is 
;; done in an asynchronous manner because we don't wait to send new actions to the agents
;; and as a result they can keep producing values as long the they have the previous ones
;; they require. 'Send-off' is used in stead of 'send' because we assign each function to 
;; an agent. So, if all the threads in the threadpool are blocked waiting for values to be 
;; produced a new one will be created and we won't have to wait forever.
;; **send uses a threadpool of size (logic-cores +2)**
;; **send-off creates a new thread if the others are busy, this thread is destroyed
;; after 1 minute of inactivity**
(defn asynchronous-non-linear [start end step & funcs]
  (let [agents (create-agents funcs (calc-iterations start end step)) ;create the agents
        parameter-vector (create-positions-vector funcs) ;create the parameter vector
        iterations-plus-one (long (+ (calc-iterations start end step) 1))] ;plus one since we start from 1, because in zero are the initial values. Also, in order to compare next with 'i' we have to cast to long, otherwise the equality will never be true
    (loop [i 1] ;start counting from 1. In position 0 the initial values exist
      (if (= i iterations-plus-one)     
        (doall (map #(deref (peek (deref %))) agents)) ;show only the last values, with 'deref' this expression will wait until all agents have calculated their values
        (do
          (dorun ;to actually send the functions to the agents because map is lazy
            (map #(send-off % (fn [agent_v]
                                (deliver (nth agent_v i) ;agent_v is a vector, (nth agent_v i) is the promise in position i of agent_v we want to deliver to
                                         (euler ;deliver the new value produced by the Euler method
                                           @(nth agent_v (- i 1)) step %2 (- i 1) %3 agents)) ;we dereference (nth agent_v (- i 1)) in order to take it's value because it is a promise
                                agent_v))
                 agents funcs parameter-vector))
          (recur (inc i)))))))
