(ns spel.core
  (:require ["@pixi/filter-pixelate" :as pixelate]
            ["@pixi/filter-crt" :as crt]
            [applied-science.js-interop :as j]
            [kitchen-async.promise :as promise]
            [lambdaisland.glogi :as log]
            [spel.thicc :as thicc]
            [lambdaisland.glogi.console :as glogi-console]
            [spel.engine :as engine]
            [spel.maniac-mansion]
            [spel.main-menu]
            [spel.space-invaders]
            [spel.invasie]
            [spel.ink]))

spel.space-invaders/no-clean-ns
spel.maniac-mansion/no-clean-ns
spel.invasie/no-clean-ns
spel.main-menu/no-clean-ns

(glogi-console/install!)
(log/set-levels {:glogi/root :all})

(j/assoc! engine/stage :filters
          #js [#_(doto (pixi/filters.ColorMatrixFilter.) (.polaroid))
               #_(pixelate/PixelateFilter. 10)
               #_(crt/CRTFilter. #js {:lineWidth 0.2
                                      :vignetting 0})])

#_
(j/assoc! engine/bg-layer :filters
          [(pixelate/PixelateFilter. 10)])

(defn search-params []
  (js/URLSearchParams. js/window.location.search))

(defn query-param [k] (.get ^js (search-params) k))

(defn init! [scene]
  (promise/do
    (engine/init!)
    (engine/load-scene scene)
    (engine/goto-scene scene)
    (engine/mount-canvas!)
    (engine/add-dom-handlers!)))

(defonce init-once (init! (keyword (or (query-param "scene") "main-menu"))))

(defn on-hot-reload []
  (log/info :hot-reload! {})
  (run! #(.remove %) (thicc/query-all "canvas"))
  (init! (:scene (engine/scene-state)))
  #_(engine/stop-scene (engine/scene-state))
  #_(engine/start-scene (engine/scene-state)))

(comment
  (engine/goto-scene :maniac-mansion))
