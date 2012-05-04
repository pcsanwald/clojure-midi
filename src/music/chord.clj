(ns music.chord
(:require [clojure.math.combinatorics :as comb])
)
(def C [:C :D :E :F :G :A :B])

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
            (recur (conj (vec (rest s)) (first s)) (dec count))
)))

(defn rootify
    [scale note]
    (loop [new-scale scale]
    (if (= (first new-scale) note)
        new-scale 
        (recur (conj (vec (rest new-scale)) (first new-scale)))
        )))

(defn chord 
    ([num_voices scale degree] 
        (let [degree (if (keyword? degree) (.indexOf scale degree) degree)] 
            (take num_voices (take-nth 2 (cycle (invert scale degree))))
        )
    ) 
    ([scale degree] 
        ; default to 3 voiced chords if no number of voices is specified
        (chord 3 scale degree))

    ([scale] 
        (chord scale (first scale)))
    )

; use partial function application to define a triad
(def triad (partial chord 3))
(def seventh_chord (partial chord 4))

(defn intervals
    ([scale interval] 
    (if (<= interval 0)
        nil
        (let [interval_to_invert (dec interval)] 
            (lazy-seq (cons (first scale) (intervals (invert scale interval_to_invert) interval)
    ))))))

(defn chord_scale [voices interval scale]
    (map 
        #(chord voices scale %) 
        (take (count scale) (intervals scale interval))))

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


(comment
;  
(map vector
    (take 7 (scale-stepper C [1 1 2]))
    (take 7 (scale-stepper (rootify C :E) [1 2 1]))
    (take 7 (scale-stepper (rootify C :G) [2 1 1]))
)
)
