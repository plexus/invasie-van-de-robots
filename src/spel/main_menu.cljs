(ns spel.main-menu
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
                                 world-height]]
            [spel.engine :as engine]
            [spel.thicc :as thicc]))

(defmethod load-scene :main-menu [scene] scene)
(defmethod start-scene :main-menu [scene]
  (conj! (js/document.getElementById "center")
         (thicc/h [:nav#menu
                   [:h1 "INVASIE VAN DE ROBOTS"]
                   [:button {:on-click (fn []
                                         (engine/goto-scene :invasie))}
                    "START"]
                   [:section.credits
                    [:p "Idee en uitwerking: Cyriel Cumps"]
                    [:p "Tekeningen: Cyriel & Vic Cumps"]
                    [:p "Programmatie: Arne Brasseur"]]]))
  scene)

(defmethod stop-scene :main-menu [scene]
  (.remove (js/document.getElementById "menu"))
  scene)

(defmethod tick-scene :main-menu [scene] scene)
(defmethod handle-event :main-menu [scene event])

(def no-clean-ns nil)
