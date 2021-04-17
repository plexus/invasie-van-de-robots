(ns spel.core
  (:require ["@pixi/filter-pixelate" :as pixelate]
            ["@pixi/filter-crt" :as crt]
            [applied-science.js-interop :as j]
            [kitchen-async.promise :as promise]
            [lambdaisland.glogi :as log]
            [lambdaisland.glogi.console :as glogi-console]
            [spel.engine :as engine]
            [spel.maniac-mansion]
            [spel.main-menu]
            [spel.space-invaders]
            [spel.invasie]))

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

(defn search-params []
  (js/URLSearchParams. js/window.location.search))

(defn query-param [k] (.get ^js (search-params) k))

(defonce init-once
  (let [scene (keyword (or (query-param "scene") "main-menu"))]
    (promise/do
      (engine/init!)
      (engine/load-scene scene)
      (engine/goto-scene scene)
      (engine/mount-canvas!)
      (engine/add-dom-handlers!))))

(defn on-hot-reload []
  (log/info :hot-reload! {})
  (engine/stop-scene (engine/scene-state))
  (engine/start-scene (engine/scene-state)))

(comment
  (engine/goto-scene :maniac-mansion))
