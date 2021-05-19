(ns spel.space-invaders
  (:require [applied-science.js-interop :as j]
            [kitchen-async.promise :as promise]
            [lambdaisland.puck :as p]
            [spel.invasie :as invasie]
            [spel.engine
             :as engine
             :refer [app key-event->keyword load-scene
                     make-sprite! move-sprite pad-hit-area!
                     scene-swap! sprite sprite-layer
                     start-scene tick-scene
                     center-viewport]]))

(defmethod load-scene :space-invaders [state]
  (promise/let [{:keys [minispel stars]}
                (p/load-resources! app
                                   {:stars "images/stars.jpg"
                                    :minispel "images/minispel/minispel.json"})]
    (make-sprite! :stars stars)
    (doseq [k [:ruimteschip :loding :batterij :kogel
               :pijl-links :pijl-rechts :pijl-schiet]]
      (doto (make-sprite! k (j/get (:textures minispel) (str (name k) ".png")))
        (p/assign! {:anchor {:x 0.5 :y 0.5}})))
    (p/assign! (sprite :ruimteschip) {:scale {:x 0.5 :y 0.5}})
    (doseq [x (range 8)
            y (range 3)]
      (let [sprite (make-sprite! [:virus x y] (j/get (:textures minispel) "virus.png"))
            scale (+ 0.7 (* 0.2 (rand)))]
        (p/assign! sprite {:anchor {:x 0.5 :y 0.5} :scale {:x scale :y scale}})))
    (assoc state :virus-vx 1)))

(defn schiet-kogel! []
  (let [schip (sprite :ruimteschip)
        kogel (sprite :kogel)]
    (when (or (not (contains? sprite-layer kogel)) (<= (:y kogel) 0))
      (p/assign! kogel {:x (:x schip)
                        :y (- (:y schip) 110)
                        :vy -10
                        :visible true})
      (conj! sprite-layer kogel))))

(defmethod start-scene :space-invaders [_]
  (let [schip (sprite :ruimteschip)
        links (sprite :pijl-links)
        rechts (sprite :pijl-rechts)
        schiet (sprite :pijl-schiet)
        kogel (sprite :kogel)
        virussen (p/container {})
        ga-links! #(j/assoc! schip :vx -10)
        ga-rechts! #(j/assoc! schip :vx 10)]

    (p/assign! (sprite :stars) {:anchor {:x 0.5 :y 0}})
    (engine/draw-background (sprite :stars))

    (doseq [sprite [links rechts schiet]]
      (j/assoc! sprite :interactive true)
      (pad-hit-area! sprite 50 50))

    (doseq [e [:touchstart :mousedown]]
      (p/listen! links e ga-links!)
      (p/listen! rechts e ga-rechts!)
      (p/listen! schiet e schiet-kogel!))

    (p/listen! js/window :keydown #(scene-swap! update :keys (fnil conj #{}) (key-event->keyword %)))
    (p/listen! js/window :keyup #(scene-swap! update :keys disj (keyword (key-event->keyword %))))
    (p/listen! js/window :blur #(scene-swap! update :keys empty))

    (doseq [e [:touchend :mouseup]]
      (p/listen! links e #(j/assoc! schip :vx 0))
      (p/listen! rechts e #(j/assoc! schip :vx 0)))

    (p/assign! virussen {:x 0 :vx 2})

    (scene-swap! assoc :virussen virussen)

    (doseq [x (range 1 #_8)
            y (range 1 #_3)
            :let [sprite (sprite [:virus x y])]]
      (p/assign! sprite {:x (+ -400 (* 100 x))
                         :y (+ 100 (* 100 y))})
      (conj! virussen sprite))

    (conj! sprite-layer virussen schip links rechts schiet)

    (p/assign! schip {:x 0 :y 850})

    (p/assign! links {:x -650 :y 930 :interactive true})
    (p/assign! rechts {:x -500 :y 930 :interactive true})
    (p/assign! schiet {:x 550 :y 900 :interactive true})
    nil))

(defmethod tick-scene :space-invaders [{:keys [delta virussen keys]}]
  (center-viewport)
  (move-sprite virussen delta)

  (when (:ArrowLeft keys)
    (j/update! (sprite :ruimteschip) :x - 10))
  (when (:ArrowRight keys)
    (j/update! (sprite :ruimteschip) :x + 10))
  (when (:Space keys)
    (schiet-kogel!))

  (when (or (< (:x virussen) -400)
            (< 400 (:x virussen)))
    (j/update! virussen :vx * -1)
    (j/update! virussen :x #(engine/clamp -399 % 399))
    (j/update! virussen :y + 20))

  (doseq [n [:ruimteschip :kogel]]
    (move-sprite (sprite n) delta))

  (let [kogel (sprite :kogel)]
    (doseq [x (range 8)
            y (range 3)
            :let [virus (sprite [:virus x y])]]
      (when (and (:visible kogel) (p/rect-overlap? kogel virus))
        (p/assign! kogel {:visible false})
        (disj! sprite-layer kogel)
        (disj! virussen virus))
      (when (empty? virussen)
        (engine/goto-scene :invasie)
        (invasie/enter-room! (engine/scene-state) :kelder :kelder-ingang)))))

(def no-clean-ns nil)
