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
    {:edit/type :assembly/apply-template :xline/ids [id ...] :template/id tid :size \"310UB40.4\"}
    {:edit/type :template/save      :template {...}}   ; connection or assembly template (library only)
    {:edit/type :assembly/update    :state {...}}      ; one assembly, from the assembly page
                                                       ; (shape: modeller.template/assembly-state)
    {:edit/type :settings/update    :settings {:gauge 100 :pitch nil}}
                                                       ; the project's detailing settings;
                                                       ; nil puts one back to its default
    {:edit/type :connection/update  :connection/id id :xline/id owner
     :connection {:connection/name .. :connection/parts [..] :connection/params {..}}}
                                                       ; one connection's own copy (a tree
                                                       ; of prims, modeller.prim; an old
                                                       ; :connection/actions list is read as one)

  `plan` returns {:model m' :changed #{xline ids} :writes [record ...]} or
  {:error e}. A record is [:assembly id], [:assembly/create id], [:grid],
  [:level id], [:connection id], [:connection/delete id], [:template id]
  or [:settings] (the building's project's own settings record).
  New ids (connections, templates) come from the edit's :new-id function,
  which the store supplies; a browser never chooses an id."
  (:require [modeller.assembly :as assembly]
            [modeller.connection :as connection]
            [modeller.core :as core]
            [modeller.grid :as grid]
            [modeller.model :as model]
            [modeller.node :as node]
            [modeller.prim :as prim]
            [modeller.settings :as settings]
            [modeller.template :as template]))

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

(def roles
  "Member roles, chosen when an assembly is created. Templates never change them."
  #{:column :beam :rafter :joist :purlin :girt :brace :panel})

(defn default-parts [role]
  (case role
    :column [{:part/id :main :part/kind :extrusion :part/profile "200UC46.2"}]
    :brace [{:part/id :main :part/kind :extrusion :part/profile "75x75x6EA"}]
    :panel [{:part/id :panel :part/kind :extrusion
             :part/profile {:section/builder :rect :b 150 :d 3000} :part/insert :bottom-centre}]
    [{:part/id :main :part/kind :extrusion :part/profile "310UB40.4" :part/insert :top-centre}]))

(def mark-prefix
  {:column "C" :beam "B" :rafter "R" :joist "J" :purlin "P" :girt "G" :brace "BR" :panel "TP"})

(defn- next-mark [xlines role]
  (let [prefix (mark-prefix role "B")
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
      (not (contains? roles role)) (error :edit/bad-role :role role)
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

;; --- templates ---------------------------------------------------------------

(defn- parts-error
  "Why a tree of prims can't be saved (modeller.prim/tree-errors against the
  prim types the core implements, modeller.connection/apply-prim), or nil."
  [parts]
  (first (prim/tree-errors parts (connection/known-types))))

(defn- template-result->plan
  "A template application's result as a plan: the assemblies it rewrote and
  the connections it upserted or removed."
  [{m' :model :keys [changed connections warnings]} extra-writes]
  {:model m'
   :changed changed
   :warnings warnings
   :writes (vec (concat extra-writes
                        (map #(vector :assembly %) (sort-by str changed))
                        (map #(vector :connection %) (:upsert connections))
                        (map #(vector :connection/delete %) (:remove connections))))})

(defmethod plan* :assembly/apply-template [m {ids :xline/ids tid :template/id size :size new-id :new-id}]
  (let [tpl (get-in m [:templates tid])
        new-id (or new-id random-uuid)]
    (cond
      (not= :assembly (:template/kind tpl)) (error :edit/unknown-template :template/id tid)
      (empty? ids) (error :edit/no-xlines)
      (not-every? (:xlines m) ids) (error :edit/unknown-xline :xline/ids ids)
      :else
      (let [r (reduce (fn [acc id]
                        (let [r (template/apply-template (:model acc) id tpl size new-id)]
                          (if (:error r)
                            (reduced r)
                            (-> acc
                                (assoc :model (:model r))
                                (update :changed into (:changed r))
                                (update-in [:connections :upsert] into (get-in r [:connections :upsert]))
                                (update-in [:connections :remove] into (get-in r [:connections :remove]))
                                (update :warnings into (:warnings r))))))
                      {:model m :changed #{} :connections {:upsert [] :remove []} :warnings []}
                      ids)]
        (if (:error r)
          {:error (:error r)}
          (template-result->plan r []))))))

(defn- copy-error
  "Why a connection's own copy can't be saved, or nil."
  [{:connection/keys [name params] :as copy}]
  (cond
    (not (and (string? name) (seq name))) {:reason :no-name}
    (not (or (nil? params) (map? params))) {:reason :bad-params}
    :else (parts-error (prim/connection-parts copy))))

(defn- condition-ok?
  "An end condition: none, a copy of known prims, or a library
  connection template to copy."
  [m c]
  (cond
    (nil? c) true
    (assembly/copy? c) (nil? (parts-error (prim/connection-parts c)))
    :else (= :connection (get-in m [:templates (:connection/template c) :template/kind]))))

(defmethod plan* :assembly/update [m {state :state new-id :new-id}]
  (let [id (:xline/id state)
        conn-ok? #(condition-ok? m %)]
    (cond
      (not (contains? (:xlines m) id)) (error :edit/unknown-xline :xline/id id)
      (and (:xline/role state) (not (roles (:xline/role state)))) (error :edit/bad-role :role (:xline/role state))
      (not (and (conn-ok? (:start state)) (conn-ok? (:end state)))) (error :edit/unknown-template)
      :else (let [r (template/update-assembly m state (or new-id random-uuid))]
              (if (:error r) {:error (:error r)} (template-result->plan r []))))))

(defn- template-error [m {:template/keys [kind] :as t}]
  (case kind
    :connection (parts-error (prim/template-parts t))
    :assembly (when-not (every? #(condition-ok? m (get t %)) [:template/start :template/end])
                {:reason :unknown-connection-template})
    {:reason :bad-kind}))

(def thumbnail-limit
  "Most characters a template's preview thumbnail (a PNG data URL) may take."
  200000)

(defn- thumbnail-error [{th :template/thumbnail}]
  (when (and (some? th)
             (not (and (string? th) (re-find #"^data:image/(png|jpeg|webp);base64," th)
                       (<= (count th) thumbnail-limit))))
    {:reason :bad-thumbnail}))

(defmethod plan* :template/save [m {t :template new-id :new-id}]
  (let [new-id (or new-id random-uuid)
        given (:template/id t)
        t (cond-> t
            (and (map? t) (nil? given)) (assoc :template/id (new-id))
            ;; an old action list is stored as its tree
            (and (map? t) (contains? t :template/actions))
            (-> (assoc :template/parts (prim/template-parts t)) (dissoc :template/actions)))
        id (:template/id t)]
    (if-let [e (or (when-not (map? t) {:reason :not-a-map})
                   ;; an id the browser sends must be one of this project's templates
                   (when (and given (not (contains? (:templates m) given))) {:reason :unknown-template})
                   (template-error m t)
                   (thumbnail-error t))]
      (error :edit/bad-template :reason (:reason e))
      ;; the library only: assemblies and connections hold their own copies,
      ;; so nothing in the model changes
      {:model (assoc-in m [:templates id] t)
       :changed #{}
       :writes [[:template id]]})))

(defmethod plan* :connection/update [m {cid :connection/id owner :xline/id copy :connection}]
  (let [c (get-in m [:connections cid])]
    (cond
      (nil? c) (error :edit/unknown-connection :connection/id cid)
      ;; the browser names the owner it is editing; it must be this one's
      (not= owner (:connection/owner c)) (error :edit/not-the-owner :connection/id cid :xline/id owner)
      (not (map? copy)) (error :edit/bad-connection :reason :not-a-map)
      :else
      (if-let [e (copy-error copy)]
        (error :edit/bad-connection :reason (:reason e))
        (let [{m' :model :keys [changed]} (template/update-connection m cid copy)]
          {:model m' :changed changed :writes [[:connection cid]]})))))

;; --- detailing settings -----------------------------------------------------------

(defn settings-readers
  "The XLines owning a connection that reads any of the settings `ks`
  (modeller.settings/uses): those regenerate when they change."
  [m ks]
  (set (for [c (vals (:connections m))
             :when (settings/reads-any? (settings/uses (assembly/own-recipe m c)) ks)]
         (:connection/owner c))))

(defmethod plan* :settings/update [m {changes :settings new-id :new-id}]
  (if-let [e (first (settings/errors changes))]
    (error :edit/bad-settings :setting (:setting e) :reason (:reason e))
    (let [stored (reduce-kv (fn [s k v] (if (nil? v) (dissoc s k) (assoc s k v)))
                            (or (:settings m) {}) changes)
          m' (cond-> (assoc m :settings stored)
               (nil? (:settings-id m)) (assoc :settings-id ((or new-id random-uuid))))]
      {:model m'
       :changed (settings-readers m' (settings/changed (:settings m) stored))
       :writes [[:settings]]})))

(defn settings-view
  "What the settings panel shows: the effective settings, what the project
  stored, and the fields with their defaults."
  [m]
  {:settings (settings/effective (:settings m))
   :stored (or (:settings m) {})
   :fields settings/fields})

(defn plan
  "Validate and apply one source edit. {:model :changed :writes} or {:error}."
  [m edit]
  (plan* m edit))

;; --- what readers receive ---------------------------------------------------

(defn- resolved-summary [resolved ids]
  (into {} (for [id ids :let [r (resolved id)] :when r]
             [id (select-keys r [:start :end :length])])))

(defn- xline-summary
  "The XLine records without their parts, plus the main part's profile
  (:assembly/profile), which the navigator and properties show as its size."
  [xlines ids]
  (into {} (for [id ids :let [x (xlines id)] :when x]
             [id (-> (dissoc x :assembly/parts)
                     (assoc :assembly/profile (:part/profile (first (:assembly/parts x)))))])))

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

(defn assembly-context
  "The records one assembly's regeneration needs: its XLine and every XLine
  its nodes depend on (transitively), the grid and levels they resolve
  against, the connections it owns, and the connections others own that
  bolt into it, with their owners (they cut its holes), and the project's
  detailing settings. Templates are left out; the page has them."
  [m id]
  (let [xlines (:xlines m)
        conns (into {} (filter #(or (= id (:connection/owner (val %)))
                                    (= id (first (:connection/refs (val %))))))
                    (:connections m))
        ids (loop [seen #{} queue (into [id] (keep :connection/owner) (vals conns))]
              (if-let [x (first queue)]
                (recur (conj seen x) (into (subvec queue 1)
                                           (remove seen (filter xlines (model/xline-deps (xlines x))))))
                seen))]
    {:grid (:grid m) :levels (:levels m) :settings (:settings m)
     :xlines (select-keys xlines ids)
     :connections conns}))

(defn assembly-page
  "What the assembly page needs to edit one assembly: its state
  (modeller.template/assembly-state), for each end that sits on another
  XLine that XLine's mark, role and size, so the page can resolve the
  connection's parameters against the real support, and the context its
  preview regenerates in. nil for an unknown id."
  [m id]
  (when-let [state (template/assembly-state m id)]
    {:state state
     :context (assembly-context m id)
     :supports (into {} (for [end [:start :end]
                              :let [sid (get-in state [:supports end])
                                    x (get-in m [:xlines sid])]
                              :when x]
                          [end {:xline/id sid :assembly/mark (:assembly/mark x) :xline/role (:xline/role x)
                                :size (let [p (:part/profile (first (:assembly/parts x)))]
                                        (when (string? p) p))}]))}))

(defn patch
  "Upsert patch after an applied plan: instructions for the affected
  assemblies, plus the grid and levels when the edit wrote them."
  [m {m' :model :keys [changed writes] :as planned}]
  (let [{:keys [resolved instructions errors affected]} (core/edit m changed (constantly m'))
        kinds (set (map first writes))]
    (cond-> {:upsert instructions
             :resolved (resolved-summary resolved affected)
             :xlines (xline-summary (:xlines m') affected)
             :created (vec (keep #(when (= :assembly/create (first %)) (second %)) writes))
             :warnings (vec (:warnings planned))
             :errors errors}
      (kinds :grid) (assoc :grid (:grid m'))
      (kinds :level) (assoc :levels (:levels m'))
      (kinds :template) (assoc :templates (:templates m'))
      (kinds :settings) (assoc :settings (settings-view m')))))
