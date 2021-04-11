(ns spel.thicc
  "Hiccup implementation which converts ClojureScript to JavaScript DOM elements
  directly. Largely works as any other hiccup implementation, but uses
  namespaced tag and attribute names to deal with DOM interfaces that are
  XML-namespaced, e.g. [:svg/circle {:cx 10 :cy 20 :r 5}]"
  (:require [goog.dom :as gdom]
            [clojure.string :as str]
            [camel-snake-kebab.core :as csk]))

(def namespaces
  {"svg" "http://www.w3.org/2000/svg"
   "xhtml" "http://www.w3.org/1999/xhtml"
   "xlink" "http://www.w3.org/1999/xlink"
   "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
   "cc" "http://creativecommons.org/ns#"
   "dc" "http://purl.org/dc/elements/1.1/"})

(defn split-tag [tag]
  (let [tag-str (name tag)
        tag-name (re-find #"[^#\.]+" tag-str)
        id (re-find #"[#][^#\.]+" tag-str)
        kls (re-seq #"[\.][^#\.]+" tag-str)]
    [(namespace tag)
     tag-name
     (when id (subs id 1))
     (doto (map #(subs % 1) kls) prn)]))

(defn split-el [[tag & tail]]
  (let [[tag-ns tag id kls] (split-tag tag)]
    [tag-ns
     tag
     (cond-> (if (map? (first tail))
               (first tail)
               {})
       id
       (assoc :id id)
       (seq kls)
       (update :class str (str/join " " kls)))
     (if (map? (first tail))
       (next tail)
       tail)]))

(declare h)

(defn h* [hiccup]
  (let [els (h hiccup)]
    (if (seq? els)
      els
      (list els))))

(defn create-el [tag-ns tag]
  (if tag-ns
    (js/document.createElementNS (get namespaces tag-ns) tag)
    (js/document.createElement tag)))

(defn set-attr [el k v]
  (let [n (csk/->camelCase (name k))]
    (if (qualified-keyword? k)
      (.setAttributeNS el (get namespaces (namespace k)) n v)
      (if (= "on-" (subs (name k) 0 3))
        ;; Set event handlers directly, rather than through setAttribute
        (unchecked-set el (str/lower-case n) v)
        (.setAttribute el n v)))))

(defn set-attrs [el attrs]
  (doseq [[k v] attrs]
    (set-attr el k v)))

(defn h [hiccup]
  (cond
    (string? hiccup)
    (gdom/createTextNode hiccup)

    (vector? hiccup)
    (let [[tag-ns tag attrs children] (split-el hiccup)
          el (create-el tag-ns tag)]
      (set-attrs el attrs)
      (apply gdom/append el (mapcat h* children))
      el)

    (seq? hiccup)
    (mapcat h* hiccup)

    :else
    (h (str hiccup))))
