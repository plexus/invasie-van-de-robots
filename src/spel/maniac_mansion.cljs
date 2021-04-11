(ns spel.maniac-mansion
  (:require [applied-science.js-interop :as j]
            [kitchen-async.promise :as promise]
            [lambdaisland.daedalus :as daedalus]
            [lambdaisland.puck :as p]
            [lambdaisland.puck.collisions :as collisions]
            [lambdaisland.puck.daedalus :as puck-daedalus]
            [lambdaisland.puck.math :as m]
            [spel.engine :refer [app bg-layer draw-background
                                 handle-event load-scene scene-state
                                 scene-swap! sprite-layer start-scene
                                 stop-scene tick-scene viewport
                                 viewport->world visible-world-width
                                 world-height clamp]]
            [spel.svg :as svg]))

(def mm-svg-url "images/maniac-mansion-achtegronden.svg")
(def images {:player            "images/jacko.png"
             :cliffs            "images/magic-cliffs-preview-detail.png"
             :fabriek-binnen    "images/fabriek_binnen.jpg"
             :fabriek-buiten    "images/fabriek_buiten.jpg"
             :hut               "images/hut.jpg"
             :oude-fabriek      "images/oude_fabriek.jpg"
             :supermarkt        "images/supermarkt.jpg"
             :werkplaats        "images/werkplaats.jpg"
             :bulkhead-back     "images/bulkhead-walls-back.png"
             :bulkhead-pipes    "images/bulkhead-walls-pipes.png"
             :bulkhead-platform "images/bulkhead-walls-platform.png"})

(defn build-rooms [base-texture elements]
  (let [{rooms      :room
         obstacles  :obstacle
         collisions :collision
         :as        groups}
        (group-by :type (vals elements))]
    (into {}
          (map
           (juxt :id
                 (fn [{:keys [id rect]}]
                   (let [obstacles       (->> obstacles
                                              (filter (comp #{id} :for))
                                              (map :path))
                         ratio           (/ world-height (:height rect))
                         width           (* (:width rect) ratio)
                         player-coll-obj (collisions/Circle. 0 0 20)
                         collission-sys  (conj! (collisions/system) player-coll-obj)]
                     {:id              id
                      :sprite          (p/sprite (p/texture base-texture rect))
                      :ratio           ratio
                      :width           width
                      :player-coll-obj player-coll-obj
                      :collision-sys   (reduce (fn [sys {:keys [rect action]}]
                                                 (conj! sys
                                                        (doto (collisions/rectangle
                                                               (* (:x rect) ratio) (* (:y rect) ratio)
                                                               (* (:width rect) ratio) (* (:height rect) ratio))
                                                          (j/assoc! :action action))))
                                               collission-sys
                                               (filter (comp #{id} :origin) collisions))
                      :mesh            (reduce
                                        (fn [mesh path]
                                          (run! #(m/!v* % ratio) path)
                                          (conj! mesh (daedalus/polygon (map (juxt :x :y) path)))
                                          mesh)
                                        (daedalus/build-rect-mesh width world-height)
                                        obstacles)}))))
          rooms)))

(defmethod load-scene :maniac-mansion [scene]
  (promise/let [{:keys [player]} (p/load-resources! app {:player (:player images)})
                svg (svg/fetch-svg mm-svg-url)]
    (let [player (doto (p/sprite player)
                   (puck-daedalus/with-radius 20)
                   (p/assign! {:position {:x 2000 :y 950}
                               :scale {:x 0.2 :y 0.2}
                               :anchor {:x 0.5 :y 1}}))

          elements (svg/elements svg)
          bg           (:background elements)
          base         (svg/base-texture (:html-image bg)
                                         mm-svg-url
                                         "maniac-mansion")
          rooms        (build-rooms base elements)
          initial-room :hallway

          debug-graphics (p/graphics)
          path-handler   (daedalus/path-handler
                          {:entity player
                           :mesh   (:mesh (get rooms initial-room))
                           :view   (puck-daedalus/simple-view debug-graphics)
                           :sampling-distance 20})]

      (assoc scene
             :room initial-room
             :elements elements
             :player player
             :path-handler path-handler
             :rooms rooms
             :debug-graphics debug-graphics
             :debug? false))))

(defmethod start-scene :maniac-mansion [{:keys [rooms room player debug-graphics debug? path-handler]}]
  (draw-background (-> rooms room :sprite))
  (conj! sprite-layer player)
  (when debug?
    (conj! sprite-layer debug-graphics)
    (daedalus/debug-draw path-handler)))

(let [debug-count (atom 0)]
  (defn debug-draw [{:keys [debug-graphics path-handler debug?]}
                    {:keys [collision-sys player-coll-obj]}]
    (when (and debug? (= 0 (mod (swap! debug-count inc) 10)))
      (daedalus/debug-draw path-handler)
      (.draw collision-sys debug-graphics))))

(def coll-res (collisions/Result.))

(defn player-collisions [{:keys [x y]}
                         {:keys [collision-sys player-coll-obj]}]
  (j/assoc! player-coll-obj :x x :y y)
  (collisions/update! collision-sys)
  (seq (filter #(collisions/collides? player-coll-obj % coll-res)
               (collisions/potentials player-coll-obj))))

(defmethod tick-scene :maniac-mansion [{:keys [delta player elements
                                               path-handler
                                               debug-graphics
                                               rooms room
                                               debug?
                                               pause-collisions?]
                                        :as state}]
  (let [room (get rooms room)]
    (daedalus/next! path-handler)
    (debug-draw state room)

    ;; Keep the viewport centered on Player, clamping on the sides. Note that
    ;; our "viewport" works by shifting a base layer in the opposite
    ;; direction, so movements are negative
    (let [bg-width (:width (p/local-bounds bg-layer))
          viewport-max (visible-world-width)]
      (j/assoc! viewport :x (- (clamp 0
                                      (- (:x player) (/ viewport-max 2))
                                      (- bg-width viewport-max)))))

    (if-let [[obj] (player-collisions player room)]
      (when (not pause-collisions?)
        (let [[_action room target] (.-action obj)
              {:keys [ratio width mesh]} (get rooms room)
              rect (get-in elements [target :rect])
              player-x (* (+ (:x rect) (/ (:width rect) 2)) ratio)
              player-y (* (+ (:y rect) (:height rect)) ratio)]
          (stop-scene state)
          (daedalus/set-mesh path-handler mesh)
          (daedalus/set-location path-handler player-x player-y)
          (scene-swap! assoc
                       :room room
                       :pause-collisions? true)
          (start-scene (scene-state))))
      (when pause-collisions?
        (scene-swap! assoc :pause-collisions? false)))))


(defmethod handle-event :maniac-mansion [{:keys [player path-handler rooms room]} [t e]]
  (let [coords (if (:clientX e) e (first (:touches e)))
        destination (viewport->world (p/point (:clientX coords)
                                              (:clientY coords)))]
    (daedalus/set-destination path-handler (:x destination) (:y destination))))

(def no-clean-ns nil)
