(ns spel.template
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
            [spel.svg :as svg]))

(defmethod load-scene :template [scene] scene)
(defmethod start-scene :template [scene] scene)
(defmethod stop-scene :template [scene] scene)
(defmethod tick-scene :template [scene] scene)
(defmethod handle-event :template [scene event])

(def no-clean-ns nil)
