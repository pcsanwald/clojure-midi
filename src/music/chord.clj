(ns music.chord)
(def C [:C :D :E :F :G :A :B])

(def invert
    (fn [scale degree]
        (loop [s scale count degree]
        (if (zero? count)
            s
            ; note that we have to convert (rest s) back to a vec,
            ; conj will not do what we want with a seq
            (recur (conj (vec (rest s)) (first s)) (dec count))
))))

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

(defn intervals
    ([scale interval] 
    (let [interval_to_invert (dec interval)] 
    (lazy-seq (cons (first scale) (intervals (invert scale interval_to_invert) interval)
    )))))

(comment
; some examples of usage of the above
(map #(c/triad c/C %)(take 7 (c/intervals c/C 3)))
)