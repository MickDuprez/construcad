(ns construcad.edits
  "Source edits: the only thing a browser may send. Each edit is validated
  against the model, applied to the in-memory model map, and described as the
  records it writes, so the store can save them in one transaction. Pure: no
  database here.

    {:edit/type :xline/set-node     :xline/id id :xline/end :start|:end :node ref}
    {:edit/type :xline/set-rotation :xline/id id :rotation deg}
    {:edit/type :xline/create       :xline/id id :start ref :end ref :role :beam}
    {:edit/type :grid/set-axis-offset :axis/id :x2 :offset 6500}
    {:edit/type :grid/relabel       :x-style :letters :y-style :numbers}
    {:edit/type :level/set-elevation :level/id id :elevation 4500}

  `plan` returns {:model m' :changed #{xline ids} :writes [record ...]} or
  {:error e}. A record is [:assembly id], [:assembly/create id], [:grid],
  [:level id] or [:connection/delete id]."
  (:require [modeller.core :as core]
            [modeller.grid :as grid]
            [modeller.model :as model]
            [modeller.node :as node]))

(defn- error [type & {:as data}] {:error (assoc data :error/type type)})

(defn- number3? [v] (and (sequential? v) (= 3 (count v)) (every? number? v)))

(defn node-error
  "Why a node reference can't be written into this model, or nil."
  [{:keys [grid levels xlines]} ref end]
  (let [kinds (filter #(contains? ref %) [:node/at :node/grid :node/of :node/on :node/rel])]
    (cond
      (not (map? ref)) {:reason :not-a-map}
      (not= 1 (count kinds)) {:reason :one-kind-required}
      (and (:node/offset ref) (not (number3? (:node/offset ref)))) {:reason :bad-offset}
      :else
      (case (node/kind ref)
        :at (when-not (number3? (:node/at ref)) {:reason :bad-point})
        :rel (cond (= :start end) {:reason :rel-on-start}
                   (not (number3? (:node/rel ref))) {:reason :bad-point})
        :grid (let [[a b] (:node/grid ref)]
                (cond (nil? (grid/intersection grid a b)) {:reason :unknown-grid-axis}
                      (and (:node/level ref) (nil? (grid/level-elevation levels (:node/level ref))))
                      {:reason :unknown-level}))
        :of (cond (not (contains? xlines (:node/of ref))) {:reason :unknown-xline}
                  (not (#{:start :end} (:node/end ref))) {:reason :bad-end})
        :on (cond (not (contains? xlines (:node/on ref))) {:reason :unknown-xline}
                  (not (number? (:node/station ref 0))) {:reason :bad-station}
                  (not (contains? #{nil :start :end} (:node/from ref))) {:reason :bad-from})))))

(defn- resolves?
  "nil when xline `id` resolves in model m, else the reason."
  [m id]
  (let [{:keys [resolved errors]} (model/resolve-xlines m)]
    (when-not (contains? resolved id)
      (or (some #(when (or (= id (:xline/id %)) (contains? (set (:xline/ids %)) id)) %) errors)
          (first errors)
          {:error/type :model/unresolved}))))

(defn- broken-connections
  "Connections owned by `id` whose owner node no longer sits on the XLine they
  reference. They are deleted with the edit, since a connection is only valid
  where it physically is."
  [m id]
  (for [c (vals (:connections m))
        :when (and (= id (:connection/owner c)) (model/connection-error m c))]
    (:connection/id c)))

(defn- drop-connections [m ids]
  {:model (update m :connections #(apply dissoc % ids))
   :writes (mapv #(vector :connection/delete %) ids)})

(defn- grid-dependents
  "XLines with a node on the grid (optionally only at `level-name`)."
  [{:keys [xlines]} level-name]
  (set (for [[id x] xlines
             ref [(:xline/start x) (:xline/end x)]
             :when (and (= :grid (node/kind ref))
                        (or (nil? level-name) (= level-name (:node/level ref))))]
         id)))

(def default-parts
  {:column [{:part/id :main :part/kind :extrusion :part/profile "200UC46.2"}]
   :beam [{:part/id :main :part/kind :extrusion :part/profile "310UB40.4"
           :part/insert :top-centre}]})

(defn- next-mark [xlines role]
  (let [prefix (if (= :column role) "C" "B")
        used (keep #(when (= role (:xline/role %)) (:assembly/mark %)) (vals xlines))]
    (str prefix (inc (count used)))))

(defn assembly-level
  "Level an assembly belongs to: the level its start node names, else the
  level nearest its resolved start."
  [{:keys [levels]} xline resolved]
  (let [name (or (:node/level (:xline/start xline))
                 (some->> (:start resolved) (#(nth % 2)) (grid/nearest-level levels) :level))]
    (some #(when (= name (:level/name %)) (:xt/id %)) levels)))

(defmulti plan* (fn [_m edit] (:edit/type edit)))

(defmethod plan* :default [_ edit] (error :edit/unknown-type :edit edit))

(defmethod plan* :xline/set-node [m {id :xline/id end :xline/end ref :node}]
  (let [k (case end :start :xline/start :end :xline/end nil)]
    (cond
      (not (contains? (:xlines m) id)) (error :edit/unknown-xline :xline/id id)
      (nil? k) (error :edit/bad-end :xline/end end)
      :else
      (if-let [e (node-error m ref end)]
        (error :edit/bad-node :node ref :reason (:reason e))
        (let [m' (assoc-in m [:xlines id k] ref)]
          (if (contains? (node/depends-on ref) id)
            (error :edit/self-reference :xline/id id)
            (if-let [e (resolves? m' id)]
              (error :edit/unresolved :xline/id id :cause e)
              (let [{m'' :model writes :writes} (drop-connections m' (broken-connections m' id))]
                {:model m'' :changed #{id} :writes (into [[:assembly id]] writes)}))))))))

(defmethod plan* :xline/set-rotation [m {id :xline/id rotation :rotation}]
  (cond
    (not (contains? (:xlines m) id)) (error :edit/unknown-xline :xline/id id)
    (not (number? rotation)) (error :edit/bad-rotation :rotation rotation)
    :else {:model (assoc-in m [:xlines id :xline/rotation] rotation)
           :changed #{id} :writes [[:assembly id]]}))

(defmethod plan* :xline/create [m {id :xline/id :keys [start end role]}]
  (let [role (or role :beam)]
    (cond
      (contains? (:xlines m) id) (error :edit/duplicate-id :xline/id id)
      (not (contains? default-parts role)) (error :edit/bad-role :role role)
      :else
      (if-let [[e which] (or (some-> (node-error m start :start) (vector :start))
                             (some-> (node-error m end :end) (vector :end)))]
        (error :edit/bad-node :xline/end which :reason (:reason e))
        (let [xline {:xline/id id :xline/role role :xline/start start :xline/end end
                     :xline/rotation 0 :assembly/mark (next-mark (:xlines m) role)
                     :assembly/parts (default-parts role)}
              m' (assoc-in m [:xlines id] xline)]
          (if-let [e (resolves? m' id)]
            (error :edit/unresolved :xline/id id :cause e)
            (let [r (get-in (model/resolve-xlines m') [:resolved id])]
              {:model (assoc-in m' [:xlines id :assembly/level] (assembly-level m' xline r))
               :changed #{id} :writes [[:assembly/create id]]})))))))

(defmethod plan* :grid/set-axis-offset [m {axis :axis/id offset :offset}]
  (let [family (some #(when (some (fn [a] (= axis (:id a))) (get-in m [:grid %])) %)
                     [:grid/x-axes :grid/y-axes])]
    (cond
      (nil? family) (error :edit/unknown-grid-axis :axis/id axis)
      (not (number? offset)) (error :edit/bad-offset :offset offset)
      :else
      (let [m' (update-in m [:grid family] (fn [axes] (mapv #(cond-> % (= axis (:id %)) (assoc :offset offset)) axes)))
            changed (grid-dependents m' nil)
            bad (some #(resolves? m' %) changed)]
        (if bad
          (error :edit/unresolved :cause bad)
          {:model m' :changed changed :writes [[:grid]]})))))

(defmethod plan* :grid/relabel [m {:keys [x-style y-style]}]
  (if-not (every? #(contains? #{nil :numbers :letters} %) [x-style y-style])
    (error :edit/bad-style :x-style x-style :y-style y-style)
    {:model (update m :grid grid/relabel {:x-style x-style :y-style y-style})
     :changed #{} :writes [[:grid]]}))

(defmethod plan* :level/set-elevation [m {id :level/id elevation :elevation}]
  (let [level (some #(when (= id (:xt/id %)) %) (:levels m))]
    (cond
      (nil? level) (error :edit/unknown-level :level/id id)
      (not (number? elevation)) (error :edit/bad-elevation :elevation elevation)
      :else
      (let [m' (update m :levels (fn [ls] (mapv #(cond-> % (= id (:xt/id %)) (assoc :level/elevation elevation)) ls)))
            changed (grid-dependents m' (:level/name level))
            bad (some #(resolves? m' %) changed)]
        (if bad
          (error :edit/unresolved :cause bad)
          {:model m' :changed changed :writes [[:level id]]})))))

(defn plan
  "Validate and apply one source edit. {:model :changed :writes} or {:error}."
  [m edit]
  (plan* m edit))

;; --- what readers receive ---------------------------------------------------

(defn- resolved-summary [resolved ids]
  (into {} (for [id ids :let [r (resolved id)] :when r]
             [id (select-keys r [:start :end :length])])))

(defn- xline-summary [xlines ids]
  (into {} (for [id ids :let [x (xlines id)] :when x]
             [id (dissoc x :assembly/parts)])))

(defn load-set
  "Assemblies on a level, plus everything they need to regenerate: the XLines
  their nodes hang off and the XLines their connections reference. All
  assemblies when level-id is nil."
  [{:keys [xlines connections]} level-id]
  (let [own (if level-id
              (set (keep (fn [[id x]] (when (= level-id (:assembly/level x)) id)) xlines))
              (set (keys xlines)))
        needs (fn [id] (into (model/xline-deps (xlines id))
                             (mapcat :connection/refs (filter #(= id (:connection/owner %)) (vals connections)))))]
    (loop [seen own queue (vec own)]
      (if-let [id (first queue)]
        (let [new (remove seen (filter xlines (needs id)))]
          (recur (into seen new) (into (subvec queue 1) new)))
        seen))))

(defn view
  "Everything a reader needs to show a set of assemblies."
  [m ids]
  (let [{:keys [resolved instructions errors]} (core/regenerate m ids)]
    {:grid (:grid m)
     :levels (:levels m)
     :instructions instructions
     :resolved (resolved-summary resolved ids)
     :xlines (xline-summary (:xlines m) ids)
     :errors errors}))

(defn patch
  "Upsert patch after an applied plan: instructions for the affected
  assemblies, plus the grid and levels when the edit wrote them."
  [m {m' :model :keys [changed writes]}]
  (let [{:keys [resolved instructions errors affected]} (core/edit m changed (constantly m'))
        kinds (set (map first writes))]
    (cond-> {:upsert instructions
             :resolved (resolved-summary resolved affected)
             :xlines (xline-summary (:xlines m') affected)
             :created (vec (keep #(when (= :assembly/create (first %)) (second %)) writes))
             :errors errors}
      (kinds :grid) (assoc :grid (:grid m'))
      (kinds :level) (assoc :levels (:levels m')))))
