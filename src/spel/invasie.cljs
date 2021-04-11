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
            [spel.svg :as svg]
            [spel.engine :refer [app bg-layer draw-background
                                 handle-event load-scene scene-state
                                 scene-swap! sprite-layer start-scene
                                 stop-scene tick-scene
                                 viewport pan-viewport viewport->world
                                 visible-world-width
                                 goto-scene
                                 world-height clamp
                                 filter-collisions]]))

(def svg-url "images/invasie_van_de_robots.svg")

(def debug? false)

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


(defn enter-room [{:keys [sprites room rooms]}]
  (draw-background (get sprites room))
  (let [player (:player sprites)
        player-size (get-in rooms [room :player-size])
        player-scale (/ player-size (:height (:texture player)))]
    (center-around-player player)
    (p/assign! player {:scale {:x player-scale :y player-scale}})))

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
             player-collision (collisions/Circle. 0 0 20)]
         {:id               id
          :sprite           (get sprites id)
          :ratio            ratio
          :width            width
          :player-collision player-collision
          :player-size      (when player-size
                              (* (:height player-size) ratio))
          :collision-sys    (build-collision-system player-collision collision-objs)
          :obstacles        obstacles
          :mesh             (build-mesh width obstacles)})))))

(defn wait-for-textures [textures]
  (log/debug :wait-for-textures textures)
  (promise/all
   (for [txt textures]
     (do
       (log/debug :waiting-for txt)

       (promise/promise [resolve]
         (.. txt -baseTexture (once "loaded" (fn [txt]
                                               (log/debug :texture-ready txt)
                                               (resolve txt))))
         (when (.. txt -baseTexture -_textureID)
           (log/debug :texture-already-loaded (.. txt -_textureID))
           (resolve)
           ))))))

(defmethod load-scene :invasie [scene]
  (promise/let [svg (svg/fetch-svg svg-url)
                _ (log/debug :svg-fetched :ok)
                elements (svg/elements svg)
                _ (log/debug :elements elements)
                ;;       _ (wait-for-textures (->> elements vals (keep :texture)))
                _ (log/debug :got-textures :ok)]
    (log/info :invasie/loaded-svg {})
    (let [start      (:start elements)
          sprites    (->> elements
                          vals
                          (filter :texture)
                          (map (fn [{:keys [id texture]}]
                                 [id (p/sprite texture)]))
                          (into {}))
          start-room (:origin start)
          rooms      (into {}
                           (map (juxt :id identity))
                           (build-rooms elements sprites))
          player     (doto (puck-daedalus/with-radius (:player sprites) 20)
                       (p/assign!
                        {:anchor   {:x 0.5 :y 1}
                         :position (m/v* (:point start)
                                         (get-in rooms [start-room :ratio]))}))

          debug-graphics (p/graphics)
          path-handler   (daedalus/path-handler
                          {:entity            player
                           :mesh              (get-in rooms [start-room :mesh])
                           :view              (puck-daedalus/simple-view debug-graphics)
                           :sampling-distance 20})]

      (assoc scene
             :player player
             :room start-room
             :rooms rooms
             :sprites sprites
             :elements elements
             :path-handler path-handler
             :debug-graphics debug-graphics))))

(defmethod start-scene :invasie [{:keys [player debug-graphics] :as scene}]
  (conj! sprite-layer player debug-graphics)
  (enter-room scene))

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
          (enter-room (scene-state))))
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
  (:elements (scene-state)))
