(ns music.inverter)

(defn step
    "a stepper, which pops off the first item and adds it to
    the end. there's probably something in clojure.core that
    does this, should investigate more."
    ([coll]
    (conj (vec (rest coll)) (first coll)))
    ([coll steps]
        (loop [current-coll coll times steps]
            (if (zero? times)
                current-coll
                (recur (step current-coll) (dec times))))))

(defmulti transposer (fn [_ thing] (class thing)))

(defmethod transposer
    Long [scale degree]
    (loop [new-scale scale count degree]
    (if (zero? count)
        new-scale 
        (recur (step new-scale) (dec count)))))

(defmethod transposer 
    clojure.lang.Keyword [scale note]
    (loop [new-scale scale]
    (if (= (first new-scale) note)
        new-scale 
        (recur (step new-scale)))))

(defn scale-stepper
    "step through a scale given an intervallic recipe.
    the recipe should be an intervallic sequence:
    common tone, second, second would be expressed as [0 1 1].
    "
    ([scale recipe]
    (lazy-seq 
        (cons 
            (first scale) 
            (scale-stepper (take 
                (count scale) 
                (cycle (transposer scale (first recipe)))) (transposer recipe 1))
    ))))

(defn make-scale
    "make a scale using a voicing, scale and degree.
    this is really an implementation detail and probably
    not directly useful."
    [recipe voicing scale degree]
    (take (count scale) 
    (scale-stepper (transposer scale (nth voicing degree)) (step recipe degree))))

(defn inverter
    "create a chord scale given an intervallic recipe, spelling of chord,
    and the scale to use"
    [recipe spelling scale]
    (let
    [voicing (map dec spelling)
    counts (range (count voicing))]
    (apply map vector
        (map (partial make-scale recipe voicing scale) counts))))

