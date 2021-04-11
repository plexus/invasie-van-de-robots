(ns portal-session
  (:require [portal.web :as p]))


(p/open) ; Open a new inspector

(add-tap #'p/submit) ; Add portal as a tap> target

;; (tap> :hello) ; Start tapping out values

;; (p/clear) ; Clear all values

;; (tap> :world) ; Tap out more values

;; (remove-tap #'p/submit) ; Remove portal from tap> targetset

;; (p/close) ; Close the inspector when done
