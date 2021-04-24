(ns cljs.user
  (:require [portal.web]))

(defn portal []
  (portal.web/open)
  (add-tap #'portal.web/submit))

(comment
  (portal)

  )
