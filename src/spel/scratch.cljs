(ns spel.scratch)

(comment
  (defn walk-step [sprite delta speed]
    (let [{:keys [destination position]} sprite]
      (when destination
        (when (< 5 (m/distance position destination))
          (let [diff (m/v- destination position)
                dist (m/vdiv diff (m/length diff))
                step (m/v* dist (* delta speed))]
            (m/!v+ position step)))))))
