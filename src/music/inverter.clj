(ns music.inverter)

(defn step
    ([coll]
    (conj (vec (rest coll)) (first coll)))
    ([coll steps]
        (loop [current-coll coll times steps]
            (if (zero? times)
                current-coll
                (recur (step current-coll) (dec times))))))

(defn invert
"return a scale inverted by a degree number.
could be refactored to return a lazy sequence?
also degree could be an element in the collection.
"
    [scale degree]
        (loop [s scale count degree]
        (if (zero? count)
            s
            ; note that we have to convert (rest s) back to a vec,
            ; conj will not do what we want with a seq
            (recur (step s) (dec count))
)))

(defn rootify
    [scale note]
    (loop [new-scale scale]
    (if (= (first new-scale) note)
        new-scale 
        (recur (step new-scale)))
        ))

(defn scale-stepper
    ([scale recipe]
    (lazy-seq 
        (cons 
            (first scale) 
            (scale-stepper (take 
                (count scale) 
                (cycle (invert scale (first recipe)))) (invert recipe 1))
    )))
    ([scale chord recipe]
    (lazy-seq
        (cons
            (first scale) 
            (scale-stepper (take 
                (count scale) 
                (cycle (invert scale (first recipe)))) (invert recipe 1))
    ))))

(defn make-scale
    [recipe voicing scale degree]
    (take (count scale) 
    (scale-stepper (invert scale (nth voicing degree)) (step recipe degree))))

(defn inverter
    [recipe spelling scale]
    (let
    [voicing (map dec spelling)
    counts (range (count voicing))]
    (apply map vector
        (map (partial make-scale recipe voicing scale) counts))))

