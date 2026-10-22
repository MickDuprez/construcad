(ns modeller.grid
  "Building grids and levels. A grid is a plan frame with two families of
  axis lines; a level is a named elevation. Every node position can be read
  back as grid intersection + offsets, which is what 2D detailing dimensions
  from.

  {:grid/id     :g1
   :grid/origin [0 0 0]          ; WCS
   :grid/angle  0                ; plan rotation, degrees about WCS Z
   :grid/x-style :numbers         ; how x-axes are labelled: :numbers or :letters
   :grid/y-style :letters
   :grid/x-axes [{:id :x1 :label \"1\" :offset 0} {:id :x2 :label \"2\" :offset 6000}] ; lines of constant local x
   :grid/y-axes [{:id :y1 :label \"A\" :offset 0} {:id :y2 :label \"B\" :offset 8000}]} ; lines of constant local y

  Node references name axes by :id, so switching label styles never breaks a
  reference. Labels are for people and drawings."
  (:require [clojure.string :as str]
            [modeller.frame :as frame]))

(defn grid-frame [{:grid/keys [origin angle]}]
  (frame/rotate-z (assoc frame/world :o (or origin [0 0 0])) (or angle 0)))

(def skipped-letters
  "Letters left out of lettered grids, as is usual on drawings."
  #{"I" "O"})

(defn- letter-label [n]
  ;; 0 -> A ... 23 -> Z, 24 -> AA (I and O skipped)
  (let [letters (remove skipped-letters (map (comp str char) (range 65 91)))
        base (count letters)]
    (loop [n n acc ""]
      (let [acc (str (nth letters (mod n base)) acc)]
        (if (< n base) acc (recur (dec (quot n base)) acc))))))

(defn labels
  "The first n axis labels in a style."
  [style n]
  (case style
    :letters (mapv letter-label (range n))
    (mapv #(str (inc %)) (range n))))

(defn- axes [prefix style spacings]
  (let [offsets (reductions + 0 spacings)]
    (mapv (fn [i offset label] {:id (keyword (str prefix (inc i))) :label label :offset offset})
          (range) offsets (labels style (count offsets)))))

(defn make-grid
  "A grid from bay spacings, e.g. {:x-spacings [6000 6000] :y-spacings [8000]}."
  [{:keys [id origin angle x-spacings y-spacings x-style y-style]
    :or {origin [0 0 0] angle 0 x-style :numbers y-style :letters}}]
  {:grid/id id :grid/origin origin :grid/angle angle
   :grid/x-style x-style :grid/y-style y-style
   :grid/x-axes (axes "x" x-style x-spacings)
   :grid/y-axes (axes "y" y-style y-spacings)})

(defn relabel
  "Switch label styles, keeping axis ids, so every node reference still holds."
  [grid {:keys [x-style y-style]}]
  (let [x-style (or x-style (:grid/x-style grid))
        y-style (or y-style (:grid/y-style grid))
        restyle (fn [axes style] (mapv #(assoc %1 :label %2) axes (labels style (count axes))))]
    (-> grid
        (assoc :grid/x-style x-style :grid/y-style y-style)
        (update :grid/x-axes restyle x-style)
        (update :grid/y-axes restyle y-style))))

(defn swap-styles
  "Numbers become letters and letters numbers."
  [grid]
  (relabel grid {:x-style (:grid/y-style grid) :y-style (:grid/x-style grid)}))

(defn- axis-offset
  "Offset of the axis named by id, or by label for hand-written references."
  [axes k]
  (or (some #(when (= k (:id %)) (:offset %)) axes)
      (some #(when (= k (:label %)) (:offset %)) axes)))

(defn intersection
  "Local [x y] of the crossing of two axes (ids or labels), in either order."
  [{:grid/keys [x-axes y-axes]} a b]
  (let [ax (axis-offset x-axes a) bx (axis-offset x-axes b)
        ay (axis-offset y-axes a) by (axis-offset y-axes b)]
    (cond
      (and ax by) [ax by]
      (and bx ay) [bx ay])))

(defn grid-point
  "WCS point at grid intersection [a b], at elevation z (WCS), plus an offset
  [dx dy dz] measured along the grid's own axes."
  [grid [a b] z offset]
  (when-let [[x y] (intersection grid a b)]
    (let [[dx dy dz] (or offset [0 0 0])]
      (frame/point->parent (grid-frame grid) [(+ x dx) (+ y dy) (+ z dz)]))))

(defn nearest-axes
  "Read a WCS point back against the grid: the closest axis of each family and
  the signed offset from it. This is the reference a detailer dimensions from."
  [{:grid/keys [x-axes y-axes] :as grid} p]
  (let [[x y z] (frame/point->local (grid-frame grid) p)
        nearest (fn [axes c]
                  (when (seq axes)
                    (let [a (apply min-key #(Math/abs (double (- c (:offset %)))) axes)]
                      {:id (:id a) :label (:label a) :offset (- c (:offset a))})))]
    {:x-axis (nearest x-axes x) :y-axis (nearest y-axes y) :z z}))

(defn level-elevation [levels level-name]
  (some #(when (= level-name (:level/name %)) (:level/elevation %)) levels))

(defn point-name
  "A grid reference as text, from a template where {x} {y} and {level} are
  replaced by the axis labels and level name, e.g. \"{y}{x}\" -> \"A1\",
  or \"{y}{level}\" -> \"A0\" for grid A on level 0."
  [template x-axis y-axis level]
  (-> template
      (str/replace "{x}" (str (:label x-axis)))
      (str/replace "{y}" (str (:label y-axis)))
      (str/replace "{level}" (str (:level/name level)))))

(defn nearest-level
  [levels z]
  (when (seq levels)
    (let [l (apply min-key #(Math/abs (double (- z (:level/elevation %)))) levels)]
      {:level (:level/name l) :offset (- z (:level/elevation l))})))

