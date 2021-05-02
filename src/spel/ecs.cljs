(ns spel.ecs)

(def world {:entities {}
            :systems #{}})

(defn add-entity [world entity components]
  (update world :entities
          assoc (:id entity)
          (assoc entity ::components components)))

(defn add-system [world system]
  (update world :systems conj system))

(defn- process [world entity key]
  (reduce (fn [e s]
            (let [f (get s key)]
              (if (and f (every? (::components e) (:requires s)))
                (f world e)
                e)))
          entity
          (:systems world)))

(defn tick [world]
  (update world :entities
          (fn [entities]
            (reduce
             (fn [es [id e]]
               (let [e (if (not (::loaded? e))
                         (assoc (process world e :load) ::loaded? true)
                         e)
                     e (process world e :tick)]
                 (if (:remove? e)
                   (do
                     (process world e :unload)
                     es)
                   (assoc es id e))))
             {}
             entities))))

(defn entity [world id]
  (get-in world [:entities id]))

(def physics
  {:id ::physics
   :requires #{:position :velocity}
   :load (fn [world entity]
           (merge {:x 0 :y 0 :vx 0 :vy 0} entity))
   :tick (fn [world entity]
           (-> entity
               (update :x + (:vx entity))
               (update :y + (:vy entity))))})


(def clip-bounds
  {:id ::clip-bounds
   :requires #{:position}
   :tick (fn [world entity]
           (if (< 100 (:x entity))
             (assoc entity :remove? true)
             entity))})

(-> world
    (add-entity {:id :foo
                 :vx 1}
                #{:position :velocity})
    (add-system physics)
    (add-system clip-bounds)
    (->> (iterate tick))
    (nth 101)

    )
