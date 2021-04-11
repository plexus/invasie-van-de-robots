(ns spel.engine
  (:require ["hammerjs" :as Hammer]
            ["pixi.js" :as pixi]
            ["@pixi/filter-pixelate" :as pixelate]
            ["@pixi/filter-crt" :as crt]
            ["pixi-plugin-bump/src/Bump" :as Bump]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [lambdaisland.puck :as p]
            [lambdaisland.puck.collisions :as collisions]
            [kitchen-async.promise :as promise]
            [lambdaisland.puck.math :as m]
            [lambdaisland.puck.daedalus :as puck-daedalus]
            [lambdaisland.daedalus :as daedalus]
            [lambdaisland.glogi.console :as glogi-console]
            [lambdaisland.glogi :as log]
            [spel.svg :as svg]))

(defonce state (atom {:time 0
                      :scenes {}
                      :sprites {}}))

(def world-height 1000)
(def achtergrond-kleur 0xFFFFFF)

(defonce app (p/full-screen-app {:background-color "white"}))
(defonce renderer (:renderer app))

;; stage
;; ├── fill-layer
;; │   └── bg-graphics
;; └── world
;;    └── viewport
;;      ├── bg-layer
;;      └── sprites
(defonce stage (:stage app))

(defonce bg-graphics (p/graphics))
(defonce fill-layer (p/container {} bg-graphics))

(defonce bg-layer (p/container {}))
(defonce sprite-layer (p/container {}))
(defonce viewport (p/container {} bg-layer sprite-layer))
(defonce world (p/container {} viewport))

(defonce add-layers-once (conj! stage fill-layer world))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Life cycle

(defmulti load-scene :scene)
(defmulti start-scene :scene)
(defmulti tick-scene :scene)
(defmulti stop-scene :scene)
(defmulti handle-event (fn [s e] (:scene s)))

(defmethod load-scene :default [_])
(defmethod start-scene :default [_])
(defmethod tick-scene :default [_])
(defmethod stop-scene :default [_]
  (p/remove-children bg-layer)
  (p/remove-children sprite-layer))

(defmethod handle-event :default [_ _])

(defn scene-state
  ([]
   (scene-state (:scene @state)))
  ([scene]
   (scene-state @state scene))
  ([state scene]
   (assoc (get-in state [:scenes scene]) :scene scene)))

(defn scene-swap! [f & args]
  (swap! state
         (fn [s]
           (apply update-in s [:scenes (:scene s)] f args))))

(add-watch state ::switch-scene
           (fn [_ _ old new]
             (when (not= (:scene old) (:scene new))
               (let [scene-path [:scenes (:scene new)]
                     new-scene (scene-state new (:scene new))]
                 (log/info :switching-scene {:old (:scene old) :new (:scene new)} )
                 (promise/do
                   (stop-scene old)
                   (if-not (:loaded? new)
                     (do
                       (log/debug :loading-scene new)
                       (promise/let [new-state (load-scene new-scene)]
                         (swap! state update-in scene-path
                                (fn [scene]
                                  (cond-> scene
                                    (map? new-state)
                                    (merge new-state)
                                    :->
                                    (assoc :scene (:scene new)
                                           :loaded? true))))
                         (start-scene (get-in @state scene-path))))
                     (start-scene new-scene)))))))

(defn goto-scene [name]
  (swap! state assoc :scene name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn screen-size []
  (get-in app [:renderer :screen]))

(defn screen-to-world-ratio []
  (let [{:keys [width height]} (screen-size)]
    (/ height world-height)))

(defn visible-world-width []
  (/ (:width (screen-size)) (screen-to-world-ratio)))

(defn resize-pixi []
  (let [{:keys [width height]} (screen-size)]
    (let [ratio (screen-to-world-ratio)]
      (p/assign! world {:x 0
                        :scale {:x ratio
                                :y ratio}}))
    (.beginFill bg-graphics achtergrond-kleur)
    (.drawRect bg-graphics 0 0 width height)
    (.endFill bg-graphics)))

(defn viewport->world [point]
  (.applyInverse (get-in viewport [:transform :worldTransform])
                 point))

(defn pan-viewport [x]
  (j/assoc! viewport :x (- x)))

(defn center-viewport []
  (pan-viewport (- (/ (visible-world-width) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn make-sprite!
  ([name texture]
   (log/trace :making-sprite {:name name :texture texture})
   (let [sprite (p/sprite texture)]
     (swap! state assoc-in [:sprites name] sprite)
     sprite)))

(defn sprite [name]
  (get-in @state [:sprites name]))

(defn init! []
  (resize-pixi)
  (p/listen! app :resize resize-pixi)
  (p/pixelate!)

  (p/listen! (:ticker app)
             :tick
             (fn [delta]
               (try
                 (swap! state update :time + delta)
                 (let [{:keys [loaded?] :as state} (scene-state)]
                   (when loaded? (tick-scene (assoc state :delta delta))))
                 (catch :default e
                   (prn "game-loop error" e)))))

  (doseq [t [:click :touchstart]]
    (p/listen! (first (js/document.getElementsByTagName "canvas"))
               (name t) t
               (fn [e] (handle-event (scene-state) [t e])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn draw-background [sprite]
  (p/remove-children bg-layer)
  (let [{:keys [width height]} (screen-size)
        texture-height (:height (:texture sprite))]
    (p/assign! sprite {:anchor {:x 0 :y 0}
                       :scale {:x (/ world-height texture-height)
                               :y (/ world-height texture-height)}})
    (conj! bg-layer sprite)))

(defn pad-hit-area! [target pad-x pad-y]
  (let [{:keys [x y width height]} (p/local-bounds target)]
    (j/assoc! target :hitArea (p/rectangle (- x (/ pad-x 2))
                                           (- y (/ pad-y 2))
                                           (+ width pad-x)
                                           (+ height pad-y)))))

(defn key-event->keyword [e]
  (let [k (.-key e)]
    (case k
      " " :Space
      (keyword k))))

(defn move-sprite [sprite delta]
  (j/update! sprite :x + (* (j/get sprite :vx 0) delta))
  (j/update! sprite :y + (* (j/get sprite :vy 0) delta)))

(defn clamp [minv v maxv]
  (min (max minv v) maxv))

(def coll-res (collisions/Result.))
(defn filter-collisions [coll-obj system]
  (collisions/update! system)
  (seq (filter #(collisions/collides? coll-obj % coll-res)
               (collisions/potentials coll-obj))))
