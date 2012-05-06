(ns music.inverter)

(defn step
    ([coll]
    (conj (vec (rest coll)) (first coll)))
    ([coll steps]
        (loop [current-coll coll times steps]
            (if (zero? times)
                current-coll
                (recur (step current-coll) (dec times))))))

(defmulti transposer (fn [_ thing] (class thing)))

(defmethod transposer Long 
    [scale degree]
    (loop [new-scale scale count degree]
    (if (zero? count)
        new-scale 
        (recur (step new-scale) (dec count)))))

(defmethod transposer clojure.lang.Keyword 
    [scale note]
    (loop [new-scale scale]
    (if (= (first new-scale) note)
        new-scale 
        (recur (step new-scale)))))

(defn scale-stepper
    ([scale recipe]
    (lazy-seq 
        (cons 
            (first scale) 
            (scale-stepper (take 
                (count scale) 
                (cycle (transposer scale (first recipe)))) (transposer recipe 1))
    )))
    ([scale chord recipe]
    (lazy-seq
        (cons
            (first scale) 
            (scale-stepper (take 
                (count scale) 
                (cycle (transposer scale (first recipe)))) (transposer recipe 1))
    ))))

(defn make-scale
    [recipe voicing scale degree]
    (take (count scale) 
    (scale-stepper (transposer scale (nth voicing degree)) (step recipe degree))))

(defn inverter
    [recipe spelling scale]
    (let
    [voicing (map dec spelling)
    counts (range (count voicing))]
    (apply map vector
        (map (partial make-scale recipe voicing scale) counts))))

