(ns spel.invasie
  (:require ["pixi.js" :as pixi]
            [applied-science.js-interop :as j]
            [clojure.string :as str]
            [kitchen-async.promise :as promise]
            [lambdaisland.daedalus :as daedalus]
            [lambdaisland.glogi :as log]
            [lambdaisland.puck :as p]
            [lambdaisland.puck.collisions :as collisions]
            [lambdaisland.puck.daedalus :as puck-daedalus]
            [lambdaisland.puck.math :as m]
            [spel.engine
             :as engine
             :refer [app bg-layer draw-background
                     handle-event load-scene scene-state
                     scene-swap! sprite-layer sprite-layer-fg ui-layer
                     start-scene stop-scene tick-scene
                     viewport pan-viewport viewport->world
                     world
                     visible-world-width
                     goto-scene
                     world-height clamp
                     filter-collisions]]
            [spel.ink :as ink]
            [spel.svg :as svg]
            [spel.thicc :as thicc])
  (:require-macros [spel.ink :refer [inline-file]]))

(def svg-url "images/invasie_van_de_robots.svg")

(def images {:ventje1 "images/ventje_frame1.png"
             :ventje2 "images/ventje_frame2.png"
             :ventje3 "images/ventje_frame3.png"
             :ventje4 "images/ventje_frame4.png"
             :ventje-stil "images/ventje_stilstand.png"
             :wheelbot "images/wheelbot9000.png"})

(def story (ink/story (subs (inline-file "resources/public/scenario.json") 1)))

(declare run-components! story-next on-cue place-room-sprite!)

(defonce box-on-click
  (p/listen! (thicc/el-by-id "text-box")
             [:click :touchstart]
             (fn [e]
               (.preventDefault e)
               (.stopPropagation e)
               (run-components! :text-box-click (scene-state)))))

(defn show-text [text]
  (let [box (thicc/el-by-id "text-box")]
    (j/assoc! box :innerText text)
    (j/assoc-in! box [:style :display] "block")))

(defn show-html [html]
  (let [box (thicc/el-by-id "text-box")]
    (thicc/clear! box)
    (conj! box (thicc/dom html))
    (j/assoc-in! box [:style :display] "block")))

(defn hide-text []
  (let [box (thicc/el-by-id "text-box")]
    (j/assoc-in! box [:style :display] "none")))

(defn flash-text [text]
  (show-text text)
  (js/setTimeout hide-text 2000))

(defn text-box-visible? []
  (let [box (thicc/el-by-id "text-box")]
    (= "block" (j/get-in box [:style :display]))))

(defn room-state
  ([]
   (room-state (scene-state)))
  ([scene]
   (get (:rooms scene) (:room scene))))

(defn story-next []
  (prn "story-next")
  (cond
    (ink/can-continue? story)
    (let [text (ink/continue! story)]
      (if (str/starts-with? text ">>>")
        (do
          (hide-text)
          (on-cue (str/trim (str/replace text #">" ""))))
        (show-html [:p text])))

    (seq (ink/choices story))
    (show-html [:div
                [:p "KEUZES"]
                (for [{:keys [text index] :as choice} (ink/choices story)]
                  [:p [:a {:on-click (fn [e]
                                       (.preventDefault e)
                                       (.stopPropagation e)
                                       (ink/make-choice! story index)
                                       (story-next))} "- " text]])])
    :else
    (hide-text)))

(defn story-goto! [path]
  (ink/goto-path! story path)
  (story-next))

(defmulti handle-room :room)
(defmethod handle-room :default [scene])

(def +background
  {:tick (fn [{:keys [sprites room path-handler player] :as scene}]
           (let [bg                 (get sprites room)
                 overlays           (:bg-overlays (room-state))
                 should-be-visible? (into #{bg} overlays)
                 make-clickable (fn [sprite]
                                  (p/assign! sprite {:interactive true})
                                  (p/listen!
                                   sprite
                                   [:click :touchstart]
                                   ::bg-click
                                   (fn [e]
                                     (run-components! :bg-click
                                                      (scene-state)
                                                      (p/local-position (j/get e :data) bg-layer)))))]
             (doseq [sprite bg-layer
                     :when (not (should-be-visible? sprite))]
               (disj! bg-layer sprite))
             (when-not (some #{bg} bg-layer)
               (draw-background bg)
               (make-clickable bg))
             (doseq [[k sprite] overlays
                     :when (not (some #{sprite} bg-layer))]
               (place-room-sprite! sprite)
               (conj! bg-layer sprite)
               (make-clickable sprite))))})

(def +player
  {:enter (fn [{:keys [sprites room player]}]
            (conj! sprite-layer-fg player))
   :leave (fn [{:keys [player]}]
            (disj! sprite-layer-fg player))
   :bg-click (fn [{:keys [path-handler]} {:keys [x y]}]
               (daedalus/set-destination path-handler x y))
   :tick (fn [{:keys [player path-handler] :as scene} delta]
           (let [{:keys [player-collision collision-sys]
                  :as room} (room-state scene)
                 x-before (:x player)]
             (if (daedalus/next? path-handler)
               (do
                 (daedalus/next! path-handler)
                 (j/assoc! player-collision
                           :x (:x player)
                           :y (:y player))
                 (when-not (:playing player)
                   (p/play! player))
                 (when (not= (< 0 (- (:x player) x-before))
                             (< 0 (:x (:scale player))))
                   (p/assign! player {:scale {:x (- (:x (:scale player)))}})))

               (p/stop! player))))})

(def +debug
  {:enter (fn [{:keys [debug-graphics]}]
            (conj! sprite-layer debug-graphics))
   :leave (fn [{:keys [debug-graphics]}]
            (disj! sprite-layer debug-graphics))
   :tick (let [debug-count (atom 0)]
           (fn [scene delta]
             (let [{:keys [debug-graphics path-handler]} scene
                   {:keys [collision-sys]} (room-state scene)]
               (when (= 0 (mod (swap! debug-count inc) 10))
                 (daedalus/debug-draw path-handler)
                 (.draw collision-sys debug-graphics)))))})

(defmulti collission-action (fn [[action] scene] action))

(defmethod collission-action :goto-room [[_ target-room target] scene]
  (let [{:keys [player pause-collisions? rooms elements path-handler]} scene
        ;; (when (= :goto-room action))
        {:keys [ratio width mesh]} (get rooms target-room)
        rect (get-in elements [target :rect])
        player-x (* (+ (:x rect) (/ (:width rect) 2)) ratio)
        player-y (* (+ (:y rect) (:height rect)) ratio)]
    (daedalus/set-mesh path-handler mesh)
    (daedalus/set-location path-handler player-x player-y)
    (run-components! :leave (scene-state))
    (scene-swap! assoc
                 :room target-room
                 :pause-collisions? true)
    (run-components! :enter (scene-state))))

(defmethod collission-action :start-scene [[_ target-scene] scene]
  (goto-scene target-scene))

(def +collisions
  {:tick (fn [scene delta]
           (let [{:keys [player pause-collisions? rooms elements path-handler]} scene
                 {:keys [player-collision collision-sys]} (room-state scene)
                 x-before (:x player)]

             ;; set col-obj x y
             (if-let [[obj :as collisions] (doall (filter-collisions player-collision collision-sys))]
               (when (and (not pause-collisions?) (j/get obj :action))
                 (collission-action (j/get obj :action) scene))
               (when pause-collisions?
                 (scene-swap! assoc :pause-collisions? false)))))})

(def +npcs
  {:enter (fn [scene]
            (let [{:keys [sprites path-handler elements]} scene
                  {:keys [npcs]} (room-state scene)]
              (doseq [{:keys [id rect dialogue] :as npc} npcs
                      :let [{:keys [texture] :as sprite} (get sprites id)
                            {:keys [width height]} texture
                            scale (/ (:height rect) (:height texture))]]
                (conj! sprite-layer sprite)
                (p/assign! sprite {:x (:x rect)
                                   :y (:y rect)
                                   :scale {:x scale :y scale}
                                   :data npc
                                   :npc? true
                                   :interactive true})
                (when dialogue
                  (p/listen!
                   sprite
                   [:click :touchstart]
                   (fn [_]
                     (when-let [{{:keys [x y]} :point} (get-in (room-state) [:elements (keyword (str (name id) "-dialogue-spot"))])]
                       (daedalus/set-destination path-handler x y))
                     (story-goto! dialogue)))))))
   :tick (fn [scene delta]
           (doseq [sprite sprite-layer
                   :when (j/get sprite :npc?)
                   :let [path-handler (j/get sprite :path-handler)]
                   :when path-handler]
             (if (daedalus/next? path-handler)
               (daedalus/next! path-handler)
               (when (j/get sprite :on-arrival)
                 ((j/get sprite :on-arrival) sprite)))))
   :leave (fn [scene]
            (let [{:keys [sprites]} scene
                  {:keys [npcs]} (room-state scene)]
              (doseq [npc npcs]
                (disj! sprite-layer (get sprites (:id npc))))))})

(def +items
  {:load (fn [{:keys [elements sprites]}]
           (doseq [item-data (->> elements vals (filter (comp #{:item} :type)))]
             (p/assign! (get sprites (:id item-data)) {:item item-data})))
   :enter (fn [scene]
            (let [{:keys [sprites path-handler elements inventory]} scene
                  {:keys [items]} (room-state scene)]
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
   :leave (fn [scene]
            (let [{:keys [sprites]} scene
                  {:keys [items]} (room-state scene)]
              (doseq [item items]
                (disj! sprite-layer (get sprites (:item item))))))})

(def +player-scale
  {:enter
   (fn [scene]
     (let [{:keys [room player]} scene
           {:keys [player-size]} (room-state scene)
           _ (when-not player-size
               (log/error :msg (str "Geen player-size gedefinieerd voor kamer " room)))
           player-scale (/ player-size (:height (:texture player)))]
       (p/assign! player {:scale {:x player-scale :y player-scale}})))})

(def +camera
  (let [center-around-player
        (fn [{:keys [player]}]
          ;; Keep the viewport centered on Player, clamping on the sides. Note that
          ;; our "viewport" works by shifting a base layer in the opposite
          ;; direction, so movements are negative
          (let [bg-width (:width (p/local-bounds bg-layer))
                viewport-max (visible-world-width)]
            (if (< bg-width viewport-max)
              (pan-viewport (/ (- bg-width viewport-max) 2))
              (pan-viewport (clamp 0
                                   (- (:x player) (/ viewport-max 2))
                                   (- bg-width viewport-max))))))]
    {:enter center-around-player
     :tick center-around-player}))

(def +ink
  {:bg-click (fn [scene _]
               (when (text-box-visible?)
                 (reduced (story-next))))
   :text-box-click (fn [scene]
                     (story-next))})

(def +intro
  {:enter (fn [{:keys [images story]}]
            (let [robot1 (-> (get images :wheelbot)
                             :texture
                             p/sprite
                             (puck-daedalus/with-radius 20))
                  robot2 (-> (get images :wheelbot)
                             :texture
                             p/sprite
                             (puck-daedalus/with-radius 20))
                  {:keys [mesh]} (room-state)
                  scale 0.3
                  h1 (daedalus/path-handler {:entity            robot1
                                             :mesh              mesh
                                             :sampling-distance 8})
                  h2 (daedalus/path-handler {:entity            robot2
                                             :mesh              mesh
                                             :sampling-distance 8})]
              (p/assign! robot1 {:id :wheelbot1
                                 :anchor {:x 0.5 :y 1}
                                 :scale {:x (- scale) :y scale}
                                 :npc? true
                                 :path-handler h1})
              (p/assign! robot2 {:id :wheelbot2
                                 :anchor {:x 0.5 :y 1}
                                 :scale {:x scale :y scale}
                                 :npc? true
                                 :path-handler h2})
              (daedalus/set-location h1 500 900)
              (daedalus/set-location h2 1200 850)
              (conj! sprite-layer robot1 robot2)
              (story-goto! "intro")))})

(def standard-components
  [+background
   +ink
   +player-scale
   +player
   +collisions
   +camera
   +npcs
   +items
   #_+debug])

(def intro-components
  [+background
   +ink
   +camera
   +intro
   +npcs
   +items])

(defn run-components! [key scene & args]
  (let [{:keys [components]} scene]
    ;; return reduced to break the chain
    (reduce (fn [_ f] (apply f scene args))
            nil
            (keep key components))))

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

(defn place-room-sprite! [sprite]
  (let [rect (scale-to-room (:ratio (room-state))
                            (j/get sprite :rect))
        texture (j/get sprite :texture)
        scale  (/ (:height rect) (:height texture))]
    (p/assign! sprite {:x (:x rect)
                       :y (:y rect)
                       :scale {:x scale :y scale}})))

(defn build-rooms [elements sprites]
  (let [rooms (->> elements vals (filter (comp #{:room} :type)))]
    (doall
     (for [{:keys [id rect origin]} rooms]
       (let [ratio            (/ world-height (:height rect))
             width            (* (:width rect) ratio)
             elements         (->> elements
                                   vals
                                   (filter (comp #{id} :origin))
                                   (map (fn [v]
                                          (if (:point v)
                                            (update v :point #(scale-to-room ratio %))
                                            v))))
             obstacles        (->> elements
                                   (filter (comp #{:obstacle} :type))
                                   (map :path)
                                   (scale-to-room ratio))
             npcs             (->> elements
                                   (filter (comp #{:npc} :type))
                                   (map (fn [npc]
                                          (update npc :rect #(scale-to-room ratio %)))))
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
          :elements         (into {} (map (juxt :id identity)) elements)
          :ratio            ratio
          :width            width
          :player-collision player-collision
          :player-size      (when player-size
                              (* (:height player-size) ratio))
          :collision-sys    (build-collision-system player-collision collision-objs)
          :obstacles        obstacles
          :npcs             npcs
          :mesh             (build-mesh width obstacles)
          :items            items})))))

(defmethod load-scene :invasie [scene]
  (p/let [svg ^await (svg/fetch-svg svg-url)
          elements ^await (svg/elements svg)
          images ^await (p/load-resources! app images)]
    (let [start      (:start elements)
          sprites    (->> elements
                          vals
                          (filter :texture)
                          (map (fn [{:keys [id texture rect] :as element}]
                                 [id (j/assoc! (p/sprite texture)
                                               :id id
                                               :rect rect
                                               :element element)]))
                          (into {}))
          start-room (keyword (or (engine/query-param "room")
                                  (name (:origin start))))
          rooms      (into {}
                           (map (juxt :id identity))
                           (build-rooms elements sprites))
          ;; ventje-stil     (p/sprite (get-in images [:ventje-stil :texture]))
          ;; ventje          (p/container ventje-stil)

          ventje (p/animated-sprite (map (comp :texture images) [:ventje1 :ventje2 :ventje3 :ventje4]))
          player (doto (puck-daedalus/with-radius ventje 20)
                   (p/assign!
                    {:id :player
                     :anchor         {:x 0.5 :y 1}
                     :player?        true
                     :animationSpeed 0.2
                     :position       (m/v* (:point start)
                                           (get-in rooms [start-room :ratio]))}))

          debug-graphics (p/graphics)
          path-handler   (daedalus/path-handler
                          {:entity            player
                           :mesh              (get-in rooms [start-room :mesh])
                           :view              (puck-daedalus/simple-view debug-graphics)
                           :sampling-distance 22})
          ui-graphics    (p/graphics)
          scene (assoc scene
                       :player player
                       :room start-room
                       :rooms rooms
                       :sprites sprites
                       :elements elements
                       :path-handler path-handler
                       :debug-graphics debug-graphics
                       :ui-graphics ui-graphics
                       :ui-size 0.1
                       :inventory [:hand]
                       :images images
                       :intro-done? false
                       :story story
                       :components standard-components #_intro-components)]
      (run-components! :load scene)
      scene)))

(defmulti combine-items (fn [this that scene] (:id this)))

(defmethod combine-items :default [this that _]
  (log/warn :combine-items/unknown {:this (:id this) :that (:id that)})
  (story-goto! "combinatie_ken_ik_niet"))

(defmethod combine-items :spuitbus [this that _]
  (case (:id that)
    :postnl
    (scene-swap! update :inventory #(conj (vec (remove #{:postnl} %)) :postnl-zilver))
    (do
      (log/warn :combine-items/unknown {:this (:id this) :that (:id that)})
      (story-goto! "combinatie_ken_ik_niet"))))

(defmethod combine-items :hand [sprite item-sprite {:keys [player player-size inventory]}]
  (log/info :combine-items/hand {:this (:id sprite) :that (:id item-sprite)})
  (if (= :player (:id item-sprite))
    (show-text "Hihihi dat kittelt")
    (when-not (some #{(:id item-sprite)} inventory)
      (let [{:keys [player-size]} (room-state)
            {:keys [x y width height]} item-sprite
            item-center {:x (+ x (/ width 2))
                         :y (+ y (/ height 2))}
            player-center {:x (:x player)
                           :y (- (:y player) player-size)}]
        (if (< 350 (m/distance player-center item-center))
          (story-goto! "item_is_te_ver")
          (scene-swap! update :inventory conj (:id item-sprite)))))))

(defn draw-inventory [{:keys [ui-graphics inventory sprites player path-handler] :as scene}]
  (p/with-fill [ui-graphics {:color 0xf5c842}]
    (p/draw-rect ui-graphics 0 0 10000 100))

  (dotimes [i 21]
    (p/with-fill [ui-graphics {:color 0xffffffff}]
      (p/draw-rect ui-graphics (+ 5 (* 95 i)) 5 90 90)))

  (doseq [sprite ui-layer
          :when (and (:item sprite)
                     (not (some #{(:id sprite)} inventory)))]
    (disj! ui-layer sprite))

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
         (doseq [item-sprite (concat sprite-layer ui-layer [player])
                 :when (and (or (:item item-sprite)
                                (= :player (:id item-sprite)))
                            (not= sprite item-sprite)
                            (p/rect-overlap? sprite item-sprite))]
           (combine-items sprite item-sprite scene))
         (draw-inventory (scene-state)))))))

(defmethod start-scene :invasie [{:keys [player debug-graphics ui-graphics]
                                  :as scene}]

  (conj! ui-layer ui-graphics)
  (run-components! :enter scene)
  (draw-inventory scene))

(defmethod tick-scene :invasie [{:keys [delta] :as scene}]
  (run-components! :tick scene delta))

(defmethod handle-event :invasie [{:keys [player path-handler rooms room]} [t e]]
  )

(defmulti on-cue identity)

(defmethod on-cue :default [cue]
  (flash-text cue))

(defmethod on-cue "robots verlaten kamer" [cue]
  (prn cue)
  (let [{:keys [mesh ratio]} (room-state)
        {:keys [elements]}   (scene-state)
        {:keys [x y]}        (scale-to-room ratio (get-in elements [:slaapkamer-ingang :rect]))

        [bot1] (filter (comp #{:wheelbot1} #(j/get % :id)) sprite-layer)
        [bot2] (filter (comp #{:wheelbot2} #(j/get % :id)) sprite-layer)]
    (p/assign! bot2 {:scale {:x (- (j/get-in bot2 [:scale :x]))}})
    (js/setTimeout (fn []
                     (daedalus/set-destination (j/get bot1 :path-handler) x y)
                     (daedalus/set-destination (j/get bot2 :path-handler) x y)
                     (j/assoc! bot1 :on-arrival #(do (disj! sprite-layer %)
                                                     (js/setTimeout story-next 2000)))
                     (j/assoc! bot2 :on-arrival #(disj! sprite-layer %)))
                   1000)))

(defn open-kast! [state]
  (let [sprite (:open-kast (:sprites state))]
    (scene-swap! assoc-in [:rooms :slaapkamer :bg-overlays :kast] sprite)))

(defmethod on-cue "kast gaat open" [cue]
  (prn cue)
  (open-kast! (scene-state))
  (js/setTimeout (fn []
                   (scene-swap! (fn [scene]
                                  (assoc scene :components standard-components)))
                   (run-components! :enter (scene-state))
                   (js/setTimeout story-next 500))
                 500))

(def no-clean-ns nil)


;; (:slaapkamer (:rooms (scene-state)))

;; (-> (scene-state) :elements :keuken-ingang)
;; (-> (scene-state) :elements :keuken-uitgang)
;; (keys (-> (scene-state) :rooms :keuken))

;; (some #{(:player (scene-state))}sprite-layer)

;; (:position (:player (scene-state)))

;; (:debug-graphics (scene-state))

(comment
  (def g (p/graphics))
  (conj! viewport g)
  (p/with-fill [g {:color 0xf5c842}]

    (p/draw-rect g 867 906 20 20))

  (p/clear! g))
