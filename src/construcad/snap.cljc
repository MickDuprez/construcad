(ns construcad.snap
  "Snapping for the XLine tool and grips. The snap decides the node reference
  written, so a picked point is never just a number when it can be a
  reference:

    grid intersection  -> {:node/grid [y-axis x-axis] :node/level name}
    XLine end          -> {:node/of id :node/end :start|:end}
    along an XLine     -> {:node/on id :node/station d :node/from :start}
    nothing            -> {:node/at [x y z]}

  Screen projection comes in as a function, so this stays pure and the
  browser and the tests share it."
  (:require [clojure.string :as str]
            [modeller.frame :as frame]
            [modeller.grid :as grid]
            [modeller.model :as model]
            [modeller.node :as node]
            [modeller.vec :as v]))

(defn- round-mm [x] (Math/round (double x)))
(defn- round-point [p] (mapv round-mm p))

(defn grid-points
  "Every grid intersection at a level, as snap candidates."
  [g {level-name :level/name z :level/elevation}]
  (for [x (:grid/x-axes g) y (:grid/y-axes g)]
    {:kind :grid
     :label (grid/point-name (or (:grid/point-name g) "{y}{x}") x y nil)
     :point (grid/grid-point g [(:id y) (:id x)] z nil)
     :ref {:node/grid [(:id y) (:id x)] :node/level level-name}}))

(defn end-points
  "Both ends of every resolved XLine not in `exclude`."
  [resolved xlines exclude]
  (for [[id r] resolved :when (not (contains? exclude id))
        end [:start :end]]
    {:kind :end
     :label (str (get-in xlines [id :assembly/mark] "XLine") " " (name end))
     :point (get r end)
     :ref {:node/of id :node/end end}}))

(defn excluded
  "Ids a node of XLine `id` must not reference: itself and everything that
  depends on it, which would make a cycle."
  [xlines id]
  (if id (model/affected {:xlines xlines} [id]) #{}))

(defn- dist2 [[ax ay] [bx by]]
  (let [dx (- ax bx) dy (- ay by)] (+ (* dx dx) (* dy dy))))

(defn- along
  "Closest point to the cursor along one XLine, measured on screen."
  [project cursor id {:keys [start end length]} xlines]
  (let [[ax ay :as a] (project start) [bx by] (project end)
        dx (- bx ax) dy (- by ay) len2 (+ (* dx dx) (* dy dy))]
    (when (pos? len2)
      (let [[cx cy] cursor
            t (max 0.0 (min 1.0 (/ (+ (* (- cx ax) dx) (* (- cy ay) dy)) len2)))
            station (round-mm (* t length))
            t (/ station length)]
        {:kind :on
         :label (str (get-in xlines [id :assembly/mark] "XLine") " @" station)
         :point (v/lerp start end t)
         :screen [(+ ax (* t dx)) (+ ay (* t dy))]
         :ref {:node/on id :node/station station :node/from :start}}))))

(defn pick
  "The snap under the cursor within `radius` pixels, or nil. Points (XLine
  ends, then grid intersections) win over positions along an XLine.

  ctx: {:grid :level :resolved :xlines :exclude}, project: WCS -> [sx sy]."
  [{:keys [grid level resolved xlines exclude] :or {exclude #{}}} project cursor radius]
  (let [r2 (* radius radius)
        ;; Ends that coincide on screen (a column's base and top in plan, a
        ;; beam end sitting on a column top) go to the one nearest the
        ;; working level, then to an original end over one that only refers
        ;; to another end.
        z (:level/elevation level 0)
        tie (fn [{[_ _ pz] :point {of :node/of end :node/end} :ref}]
              [(if (and of (= :of (node/kind (get-in xlines [of (if (= :start end) :xline/start :xline/end)])))) 1 0)
               (Math/abs (double (- pz z)))])
        near (fn [cands]
               (->> cands
                    (keep #(let [d (dist2 (or (:screen %) (project (:point %))) cursor)]
                             (when (<= d r2) (assoc % :d2 d))))
                    (sort-by #(into [(Math/round (double (:d2 %)))] (tie %)))
                    first))]
    (or (near (end-points resolved xlines exclude))
        (when (and grid level) (near (grid-points grid level)))
        (near (keep (fn [[id r]] (when-not (contains? exclude id) (along project cursor id r xlines)))
                    resolved)))))

(defn free
  "No snap: an absolute node at the picked point, to the millimetre."
  [p]
  (let [p (round-point p)]
    {:kind :free :label (str/join "," p) :point p :ref {:node/at p}}))

;; --- typed coordinates --------------------------------------------------------

(defn parse-entry
  "x,y,z or @dx,dy,dz. {:relative? bool :values [x y z]} or {:error msg}."
  [text]
  (let [text (str/trim (or text ""))
        relative? (str/starts-with? text "@")
        body (if relative? (subs text 1) text)
        values (map #(parse-double (str/trim %)) (str/split body #","))]
    (if (and (= 3 (count values)) (every? some? values))
      {:relative? relative? :values (vec values)}
      {:error "Enter x,y,z or @dx,dy,dz"})))

(defn moved-ref
  "Node reference for an end moved by WCS `delta` from where it resolves
  (`point`). A grid node keeps its grid reference and takes the move as an
  offset along the grid's axes; anything else becomes absolute."
  [g ref point delta]
  (if (= :grid (node/kind ref))
    (let [d (frame/dir->local (grid/grid-frame g) delta)]
      (assoc ref :node/offset (round-point (v/v+ (or (:node/offset ref) [0 0 0]) d))))
    {:node/at (round-point (v/v+ point delta))}))
