(ns music.main
(:use music.midi music.chord))

(defn -main []
    (let [progression (map #(music.chord/triad music.chord/C %)(take 7 (music.chord/intervals music.chord/C 3)))]
    (doseq [toplay (map #(make-chord %) progression)] 
    (perform toplay)
    )))
