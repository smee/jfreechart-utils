(ns binning)

(defn bin-fn 
  "Create a function that returns the interval that a value x falls in.
Example:
    ((bin-fn 0 10 2) 5.5) 
    => [4 6]"
  [min max step] 
  (fn [v] (let [bin-start (int (/ (- v min) step))] 
            [(+ min (* bin-start step)) (+ min (* (inc bin-start) step))])))

(defn bounded-bin-idx-fn 
  "Create a function that returns the index of a bin that a value x falls in. If the value is out of bounds,
returns the nearest valid idx.
Example:  
    ((bounded-bin-idx-fn 0 10 2) 5.5) 
    => 2
    ((bounded-bin-idx-fn 0 10 2) -5) 
    => 0
    ((bounded-bin-idx-fn 0 10 2) 100) 
    => 4
"
  [minimum maximum step] 
  {:pre [(every? number? [minimum maximum step]) 
         (< minimum maximum)
         (<= step (- maximum minimum))]} 
  (let [supremum-bin (dec (int (/ (- maximum minimum) step)))] 
       (fn [^double v]
         (let [bin-start (int (/ (- v minimum) step))]
           (-> bin-start (max 0) (min supremum-bin))))))

(defn bounded-bin-fn 
  "Create a function that returns the interval that a value x falls in. If the value is out of bounds,
returns the nearest valid bin.
Example:  
    ((bounded-bin-fn 0 10 2) 5.5) 
    => [4 6]
    ((bounded-bin-fn 0 10 2) -5) 
    => [0 2]
    ((bounded-bin-fn 0 10 2) 100) 
    => [8 10
"
  [minimum maximum step] 
  {:pre [(every? number? [minimum maximum step]) 
         (< minimum maximum)
         (<= step (- maximum minimum))]} 
  (let [supremum-bin (dec (int (/ (- maximum minimum) step)))
        idx-fn (bounded-bin-idx-fn minimum maximum step)] 
       (fn [^double v]
         (let [bin-start (idx-fn v) ] 
              [(+ minimum (* bin-start step)) (+ minimum (* (inc bin-start) step))]))))
