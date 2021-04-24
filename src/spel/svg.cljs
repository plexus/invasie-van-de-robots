(ns spel.svg
  (:require ["pixi.js" :as pixi]
            [applied-science.js-interop :as j]
            [cljs.reader :as edn]
            [clojure.string :as str]
            [kitchen-async.promise :as promise]
            [lambdaisland.glogi :as log]
            [lambdaisland.puck :as p]
            [lambdaisland.puck.math :as math]))

(defprotocol Coercions
  (>str [_])
  (>num [_]))

(extend-protocol Coercions
  js/SVGAnimatedLength
  (>num [this] (>num (.-baseVal this)))
  js/SVGLength
  (>num [this] (.-value this))
  js/SVGAnimatedString
  (>str [this] (.-baseVal this))
  js/CharacterData
  (>str [this] (.-data this)))

(defn query [^js dom selector]
  (.querySelector dom selector))

(defn query-all [^js dom selector]
  (.querySelectorAll dom selector))

(defn fetch-svg
  "Given a URL to an SVG file, return a Promise of the root of the SVG DOM tree."
  [url]
  (promise/let [res (js/fetch url)
                txt (.text res)
                div (js/document.createElement "div")]
    (set! (.-innerHTML div) txt)
    (doto (first (.-children div))
      (j/assoc! :source-url url))))

(defn resolve-url
  ([url]
   (resolve-url url js/document.baseURI))
  ([url base]
   (js/URL. url base)))

(defn svg-image->html-img
  "Given an svg <image> element (SVGImageElement), return a equivalent
  HTMLImageElement."
  [image base-url]
  (log/debug :loading-image image)
  (promise/promise [resolve]
    (let [html-image (js/Image.) #_(js/document.createElement "img")]
      (when (= (.-id image) "pleintje")
        (def pleintje html-image))
      (j/assoc! html-image
                :onload (fn []
                          (log/debug :loaded-image {:id (.-id image)
                                                    :img html-image})
                          (resolve html-image))
                :onerror (fn [err]
                           (log/error :html-image-error err)
                           (resolve html-image)))
      (when-let [width (j/get image :width)]
        (j/assoc! html-image :width (Math/ceil (>num width))))
      (when-let [height (j/get image :height)]
        (j/assoc! html-image :height (Math/ceil (>num height))))
      (j/assoc! html-image :src (resolve-url (>str (j/get image :href)) base-url))
      (when (j/get html-image :complete)
        (log/info :already-complete html-image)
        (resolve html-image)))))

(defn base-texture
  "Create a pixi BaseTexture that can be used to create multiple textures with the
  same underlying image data."
  [html-image url name]
  (j/get (pixi/Texture.fromLoader html-image url name) :baseTexture))

(defn attr [el a]
  (.getAttribute el a))

(defn svg-rect->pixi
  "Get a pixi Rectangle for a given SVGRectElement (`<rect>`)"
  [rect]
  (pixi/Rectangle. (attr rect "x")
                   (attr rect "y")
                   (attr rect "width")
                   (attr rect "height")))

#_(defn load-svg
    "Load an SVG and return Pixi stuff. Assumes the SVG contains a single `<image>`
  tag with a `data:` url, and a number a `<rect>` elements definining the
  bounding boxes of sub-textures."
    [url name]
    (promise/let [svg (fetch-svg url)]
      (let [svg-img (query svg "image")
            html-img (svg-image->html-img svg-img)
            base (base-texture html-img url name)]
        {:base-texture base
         :textures (into {}
                         (for [rect (query-all svg "rect")]
                           #_(query (.-parentElement rect) "desc")
                           [(keyword (attr rect "id")) (pixi/Texture. base (svg-rect->pixi rect))]))})))

(defn path->cmds
  "Low level parsing of an SVG \"path-data\" string.
  Take an SVG `<path>`'s `d` attribute, and parse it to a sequence of commands,
  which are pairs consisting of a single letter string, and a sequence of
  numbers."
  [d]
  (for [[_ cmd coords] (re-seq #"([mlhzcqavMLHZCQAHV])\s*([^mlhzcqavMLHZCQAHV]*)\s*" d)]
    (cond-> [cmd]
      (not (str/blank? coords))
      (conj
       (map #(js/parseFloat % 10) (str/split coords #"[,\s]+"))))))

(defn path->coords
  "Convert a SVG path-data string to a sequence of vertices.
  This assumes that your path is a simple closed polygon. Curve and arc commands
  are flattened to straight lines from start to end point, and the
  `closepath` (`\"z\"`) command is ignored.

  This is meant for allowing you to edit polygons using an SVG editor, which you
  can then use in your game, e.g. for collision outlines or level data."
  [path-data]
  (loop [[[cmd coords] & cmds] (path->cmds path-data)
         res []
         pen (p/point 0 0)]
    (if cmd
      (let [relative? (= cmd (str/lower-case cmd))
            point (if relative?
                    (fn [pen [x y]]
                      (math/v+ (p/point x y) pen))
                    (fn [pen [x y]]
                      (p/point x y)))
            line-to (fn [coords]
                      (cons [(if relative? "l" "L") (flatten coords)]
                            cmds))]
        (case (str/upper-case cmd)
          ;; Move to
          "M"
          (let [coords (partition 2 coords)
                pen (point pen (first coords))]
            (if-let [coords (next coords)]
              (recur (line-to coords) (conj res pen) pen)
              (recur cmds (conj res pen) pen)))
          ;; Line
          "L"
          (let [coords (partition 2 coords)
                [pen points] (reduce (fn [[pen res] coord]
                                       (let [pen (point pen coord)]
                                         [pen (conj res pen)]))
                                     [pen res]
                                     coords)]
            (recur cmds points pen))
          ;; Horizontal line
          "H"
          (recur (line-to (for [x coords] [x (if relative? 0 (:y pen))])) res pen)
          ;; Vertical line
          "V"
          (recur (line-to (for [y coords] [(if relative? 0 (:x pen)) y])) res pen)
          ;; Quadratic Bezier / Cubic Bezier / Arc : just keep the start/end
          ;; vertices, ignore control points and parameters
          "Q"
          (recur (line-to (for [[_x1 _y1 x y] (partition 4 coords)] [x y])) res pen)
          "C"
          (recur (line-to (for [[_x1 _y1 _x2 _y2 x y] (partition 6 coords)] [x y])) res pen)
          "A"
          (recur (line-to (for [[_rx _ry
                                 _x-axis-rotation _large-arc-flag _sweep-flag
                                 x y] (partition 7 coords)]
                            [x y])) res pen)
          ;; Close the shape. Ignored.
          #_"Z"
          #_(recur cmds (conj res (first res)) (first res))
          (recur cmds res pen)))
      res)))

(defn desc-edn
  "Parse out EDN metadata from an element's description."
  [el]
  (try
    (when-let [desc (query el "desc")]
      (edn/read-string (.-innerHTML desc)))
    (catch :default _)))

(defn adjust-origin [{:keys [rect] :as origin} element]
  (let [{:keys [x y]} rect
        offset (p/point x y)]
    (cond
      (:path element)
      (update element :path (fn [path]
                              (map #(math/v- % offset) path)))
      (:rect element)
      (update element :rect (fn [r]
                              (-> r
                                  (update :x - x)
                                  (update :y - y))))

      (:point element)
      (update element :point math/v- offset)

      :else
      element)))

(defn adjust-origins
  "When an element has an `:origin` key in its EDN metadata pointing at the ID of
  another element, use that elements x/y position as the origin, adjusting any
  parsed out path/rect elements accordingly."
  [elements]
  (into {}
        (map (juxt key
                   (comp (fn [el]
                           (if-let [origin (:origin el)]
                             (adjust-origin (get elements origin) el)
                             el))
                         val)))
        elements))

(defn img-add-texture [data el url]
  (promise/let [html-img (svg-image->html-img el (resolve-url url))]
    (let [id (name (:id data))
          width (>num (j/get el :width))
          height (>num (j/get el :height))
          base-texture (pixi/BaseTexture. html-img
                                          (j/lit {:scaleMode pixi/settings.SCALE_MODE
                                                  :resolution 1
                                                  :width width
                                                  :height height}))]
      (j/assoc-in! base-texture [:resource :url] (str url "#" id))
      (assoc data
             :html-image html-img
             :texture (pixi/Texture. base-texture (pixi/Rectangle. 0 0 width height))
             :base-texture base-texture))))

#_(defn promise-map
    "Convert a Map<Key, Promise<Val>> into a Promise<Map<Key,Val>>"
    [m]
    (let [ks (keys m)]
      (promise/let [vs (promise/all (map #(get m %) ks))]
        (zipmap ks vs))))

(defn elements
  "Extract SVG elements that have EDN metadata.

  Takes an SVG DOM element and returns a sequence of maps, each corresponding
  with an SVG element. Only elements that have EDN metadata in a `<desc>`
  element are considered.

  To use this, in your SVG editor (Inkscape) use the \"description\" field under
  \"object properties\" to add metadata to elements, in the form of an EDN map.
  This function finds those elements, parses the EDN, and adds some extra keys.
  `:id` based on the SVG element `id` attribute, `:tag` the SVG element tag
  name (rect, path, image, etc.), `:element` the `SVGElement` instance. Path and
  rect elements are parsed to pixi/Point / pixi/Rectangle coordinates and added
  as `:path` / `:rect` respectively."
  [svg]
  (prn svg)
  (let [elements
        (->> (query-all svg "desc")
             (map #(.-parentNode %))
             (keep
              (fn [el]
                (when-let [data (desc-edn el)]
                  (let [data (assoc data
                                    :id (keyword (attr el "id"))
                                    :element el
                                    :tag (.-tagName el))]
                    (case (.-tagName el)
                      "path"
                      (assoc data :path (path->coords (attr el "d")))
                      "rect"
                      (assoc data :rect (svg-rect->pixi el))
                      "image"
                      (-> data
                          (assoc :rect (svg-rect->pixi el))
                          (img-add-texture el (j/get svg :source-url)))
                      "circle"
                      (assoc data
                             :point (p/point (attr el "cx") (attr el "cy"))
                             :radius (attr el "r"))
                      "ellipse"
                      (assoc data
                             :point (p/point (attr el "cx") (attr el "cy"))
                             :radius-x (attr el "rx")
                             :radius-y (attr el "ry")
                             :radius (/ (+ (attr el "rx") (attr el "ry")) 2))
                      #_else
                      data))))))]
    (promise/let [elements (promise/all elements)]
      (->> elements
           (into {} (map (juxt :id identity)))
           (adjust-origins)))))

(comment
  (promise/let [res (fetch-svg "images/maniac-mansion-achtegronden.svg")]
    (def ss res))

  (tap> (elements ss))

  (clojure.datafy/datafy (:rect (second (elements ss))))

  (promise/let [res (load-svg "images/maniac-mansion-achtegronden.svg" "maniac-mansion")]
    (def rr res))

  (def svg-img (query ss "image"))
  (def html-img (svg-image->html-img svg-img))

  (attr (query ss "path") "d")
  (def d "m 10.303405,268.05161 c 2.090292,-1.40839 22.032817,-12.55403 22.032817,-12.55403 l 355.173368,0.17187 41.94725,6.60637 14.36664,-0.1179 -44.53466,-90.4214 0.33798,-6.42356 -391.2863058,-0.29868 0.01038,105.2647 2.8618158,-2.2421 z")

  (path->coords d)
  ;; => [#pixi/Point {:x 10.303405, :y 268.05161} #pixi/Point {:x 32.336222, :y 255.49757999999997} #pixi/Point {:x 387.50959, :y 255.66944999999998} #pixi/Point {:x 429.45684, :y 262.27582} #pixi/Point {:x 443.82348, :y 262.15792} #pixi/Point {:x 399.28882, :y 171.73651999999998} #pixi/Point {:x 399.6268, :y 165.31295999999998} #pixi/Point {:x 8.340494200000023, :y 165.01427999999999} #pixi/Point {:x 8.350874200000023, :y 270.27898} #pixi/Point {:x 11.212690000000023, :y 268.03688}]


  (def base (base-texture html-img "images/maniac-mansion-achtegronden.svg" "maniac-mansion"))

  (desc-edn (query ss "path"))
  (map (juxt #(attr % "id") desc-edn identity) (map #(.-parentNode %) (query-all ss "desc")))

  (.getType (query ss "desc"))
  (elements ss)
  )
