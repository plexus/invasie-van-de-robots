(ns spel.invasie
  (:require ["pixi.js" :as pixi]
            [applied-science.js-interop :as j]
            [kitchen-async.promise :as promise]
            [lambdaisland.daedalus :as daedalus]
            [lambdaisland.glogi :as log]
            [lambdaisland.puck :as p]
            [lambdaisland.puck.collisions :as collisions]
            [lambdaisland.puck.daedalus :as puck-daedalus]
            [lambdaisland.puck.math :as m]
            [spel.thicc :as thicc]
            [spel.svg :as svg]
            [spel.engine :refer [app bg-layer draw-background
                                 handle-event load-scene scene-state
                                 scene-swap! sprite-layer ui-layer
                                 start-scene stop-scene tick-scene
                                 viewport pan-viewport viewport->world
                                 visible-world-width
                                 goto-scene
                                 world-height clamp
                                 filter-collisions]]))

(def svg-url "images/invasie_van_de_robots.svg")

(def debug? true)

(defn show-text [text]
  (let [box (thicc/el-by-id "text-box")]
    (j/assoc! box :innerText text)
    (j/assoc-in! box [:style :display] "block")))

(defn hide-text []
  (let [box (thicc/el-by-id "text-box")]
    (j/assoc-in! box [:style :display] "none")))

(defn flash-text [text]
  (show-text text)
  (js/setTimeout hide-text 2000))



(defn center-around-player [player]
  ;; Keep the viewport centered on Player, clamping on the sides. Note that
  ;; our "viewport" works by shifting a base layer in the opposite
  ;; direction, so movements are negative
  (let [bg-width (:width (p/local-bounds bg-layer))
        viewport-max (visible-world-width)]
    (if (< bg-width viewport-max)
      (pan-viewport (/ (- bg-width viewport-max) 2))
      (pan-viewport (clamp 0
                           (- (:x player) (/ viewport-max 2))
                           (- bg-width viewport-max))))))


(defn enter-room [{:keys [sprites room rooms inventory]}]
  (draw-background (get sprites room))
  (let [player (:player sprites)
        {:keys [player-size items]} (get rooms room)
        player-scale (/ player-size (:height (:texture player)))]
    (center-around-player player)
    (p/assign! player {:scale {:x player-scale :y player-scale}})

    (doseq [sprite sprite-layer
            :when (not= sprite player)]
      (disj! sprite-layer sprite))

    (doseq [{:keys [item rect] :as item-data} items
            :when (not (some #{item} inventory))
            :let [{:keys [texture] :as sprite} (get sprites item)
                  {:keys [width height]} texture
                  scale (min (/ (:heigt rect) height)
                             (/ (:width rect) width))]]
      (conj! sprite-layer sprite)
      (p/assign! sprite {:x (:x rect)
                         :y (:y rect)
                         :scale {:x scale :y scale}
                         :item item-data}))))

(defn build-collision-system [player-collision-object collisions]
  (let [sys (collisions/system)]
    (conj! sys player-collision-object)
    (reduce (fn [sys {:keys [rect path action] :as coll}]
              (conj! sys
                     (doto (cond
                             rect
                             (collisions/rectangle rect)
                             path
                             (let [x (apply min (map :x path))
                                   y (apply min (map :y path))]
                               (collisions/polygon x y (cond-> (map #(-> %
                                                                         (update :x - x)
                                                                         (update :y - y))
                                                                    path)
                                                         (not (m/clockwise? path))
                                                         reverse))))
                       (j/assoc! :action action))))
            sys
            collisions)))

(defn build-mesh [width obstacles]
  (reduce (fn [mesh path]
            (conj! mesh (daedalus/polygon (map (juxt :x :y) path)))
            mesh)
          (daedalus/build-rect-mesh width world-height)
          obstacles))

(defn scale-to-room [ratio obj]
  (cond
    (instance? pixi/Rectangle obj)
    (p/rectangle (* (:x obj) ratio)
                 (* (:y obj) ratio)
                 (* (:width obj) ratio)
                 (* (:height obj) ratio))
    (instance? pixi/Point obj)
    (p/point (* (:x obj) ratio)
             (* (:y obj) ratio))
    (sequential? obj)
    (map (partial scale-to-room ratio) obj)))

(defn build-rooms [elements sprites]
  (let [rooms (->> elements vals (filter (comp #{:room} :type)))]
    (doall
     (for [{:keys [id rect origin]} rooms]
       (let [ratio            (/ world-height (:height rect))
             width            (* (:width rect) ratio)
             elements         (->> elements
                                   vals
                                   (filter (comp #{id} :origin)))
             obstacles        (->> elements
                                   (filter (comp #{:obstacle} :type))
                                   (map :path)
                                   (scale-to-room ratio))
             collision-objs   (->> elements
                                   (filter (comp #{:collision} :type))
                                   (map (fn [obj]
                                          (cond-> obj
                                            (:path obj)
                                            (update :path #(scale-to-room ratio %))
                                            (:rect obj)
                                            (update :rect #(scale-to-room ratio %))))))
             player-size      (some #(when (= :player-size (:type %))
                                       (:rect %))
                                    elements)
             player-collision (collisions/Circle. 0 0 20)
             items            (->> elements
                                   (filter (comp #{:room-item} :type))
                                   (map (fn [i]
                                          (update i :rect #(scale-to-room ratio %)))))]
         {:id               id
          :sprite           (get sprites id)
          :ratio            ratio
          :width            width
          :player-collision player-collision
          :player-size      (when player-size
                              (* (:height player-size) ratio))
          :collision-sys    (build-collision-system player-collision collision-objs)
          :obstacles        obstacles
          :mesh             (build-mesh width obstacles)
          :items            items})))))

(defmethod load-scene :invasie [scene]
  (promise/let [svg (svg/fetch-svg svg-url)
                elements (svg/elements svg)]
    (let [start      (:start elements)
          sprites    (->> elements
                          vals
                          (filter :texture)
                          (map (fn [{:keys [id texture]}]
                                 [id (j/assoc! (p/sprite texture) :id id)]))
                          (into {}))
          start-room (:origin start)
          rooms      (into {}
                           (map (juxt :id identity))
                           (build-rooms elements sprites))
          player     (doto (puck-daedalus/with-radius (:player sprites) 20)
                       (p/assign!
                        {:anchor   {:x 0.5 :y 1}
                         :position (doto (m/v* (doto (:point start) prn)
                                               (get-in rooms [start-room :ratio]))
                                     prn)}))

          debug-graphics (p/graphics)
          path-handler   (daedalus/path-handler
                          {:entity            player
                           :mesh              (get-in rooms [start-room :mesh])
                           :view              (puck-daedalus/simple-view debug-graphics)
                           :sampling-distance 20})
          ui-graphics (p/graphics)]

      (assoc scene
             :player player
             :room start-room
             :rooms rooms
             :sprites sprites
             :elements elements
             :path-handler path-handler
             :debug-graphics debug-graphics
             :ui-graphics ui-graphics
             :ui-size 0.1
             :inventory [:hand]))))

(defn draw-inventory [{:keys [ui-graphics inventory sprites]}]
  (p/with-fill [ui-graphics {:color 0xf5c842}]
    (p/rect ui-graphics 0 0 10000 100))

  (dotimes [i 21]
    (p/with-fill [ui-graphics {:color 0}]
      (p/rect ui-graphics (+ 5 (* 95 i)) 5 90 90)))

  (doseq [[item idx] (map vector inventory (range))]
    (let [{:keys [texture] :as sprite} (get sprites item)
          {:keys [width height]} texture
          size (if (< width height) height width)
          scale (/ 70 size)]
      (conj! ui-layer sprite)
      (p/assign! sprite {:x (+ 15 (* 95 idx))
                         :y 15
                         :scale {:x scale :y scale}
                         :interactive true
                         :buttonMode true})
      (p/listen!
       sprite
       [:mousedown :touchstart]
       (fn [evt]
         (p/assign! sprite {:data (j/get evt :data)
                            :alpha 0.5
                            :dragging true})))

      (p/listen!
       sprite
       [:mousemove :touchmove]
       (fn [evt]
         (when (j/get sprite :dragging)
           (j/assoc! sprite :position (p/local-position (j/get sprite :data) (:parent sprite))))))

      (p/listen!
       sprite
       [:mouseup :mouseupoutside :touchend :touchendoutside]
       (fn [evt]
         (p/assign! sprite {:alpha 1
                            :dragging false
                            :data nil})
         (doseq [item-sprite sprite-layer
                 :when (and (:item item-sprite)
                            (p/rect-overlap? sprite item-sprite))]
           (let [{:keys [x y width height]} (p/bounds item-sprite)
                 center {:x (+ x (/ width 2))
                         :y (+ y (/ height 2))}]
             (if (< 400 (m/distance (:player sprites) center))
               (flash-text "Je bent te ver")
               (scene-swap! update :inventory conj (:id item-sprite)))))
         (draw-inventory (scene-state)))))))

(defmethod start-scene :invasie [{:keys [player debug-graphics ui-graphics]
                                  :as scene}]
  (conj! sprite-layer player debug-graphics)
  (conj! ui-layer ui-graphics)
  (enter-room scene)
  (draw-inventory scene))

(let [debug-count (atom 0)]
  (defn debug-draw [{:keys [debug-graphics path-handler]}
                    {:keys [collision-sys]}]
    (when (and debug? (= 0 (mod (swap! debug-count inc) 10)))
      (daedalus/debug-draw path-handler)
      (.draw collision-sys debug-graphics))))

(defmethod tick-scene :invasie [{:keys [delta player elements
                                        path-handler
                                        #_debug-graphics
                                        rooms room
                                        debug?
                                        pause-collisions?]
                                 :as state}]
  (let [{:keys [player-collision collision-sys]
         :as room} (get rooms room)]
    (daedalus/next! path-handler)
    (j/assoc! player-collision
              :x (:x player)
              :y (:y player))
    (debug-draw state room)
    (center-around-player player)

    ;; set col-obj x y
    (if-let [[obj :as collisions] (doall (filter-collisions player-collision collision-sys))]
      (do
        (prn collisions)
        (when (not pause-collisions?)
          (let [[_action room target] (.-action obj)
                {:keys [ratio width mesh]} (get rooms room)
                rect (get-in elements [target :rect])
                player-x (* (+ (:x rect) (/ (:width rect) 2)) ratio)
                player-y (* (+ (:y rect) (:height rect)) ratio)]
            (daedalus/set-mesh path-handler mesh)
            (daedalus/set-location path-handler player-x player-y)
            (scene-swap! assoc
                         :room room
                         :pause-collisions? true)
            (enter-room (scene-state)))))
      (when pause-collisions?
        (scene-swap! assoc :pause-collisions? false)))))

(defmethod handle-event :invasie [{:keys [player path-handler rooms room]} [t e]]
  (let [coords (if (:clientX e) e (first (:touches e)))
        destination (viewport->world (p/point (:clientX coords)
                                              (:clientY coords)))]
    (daedalus/set-destination path-handler (:x destination) (:y destination))))

(def no-clean-ns nil)

(comment
  (goto-scene :invasie)
  (draw-inventory (scene-state))
  (:inventory (scene-state))
  (:width (:texture (:hand (:sprites (scene-state)))))
  (scene-swap! update :inventory conj :postnl)

  (:player (:sprites (scene-state)))
  (:slaapkamerdeur2 (:elements (scene-state)))
  )
