(ns construcad.navigator
  "What the editor's side bars show, worked out from the view the browser
  holds (construcad.edits/view): the tree navigator (building > level >
  role > mark) and the properties of one assembly. Pure."
  (:require [clojure.string :as str]
            [modeller.grid :as grid]))

(def role-groups
  "Tree groups in the order they are listed: role, group label."
  [[:column "Columns"] [:beam "Beams"] [:rafter "Rafters"] [:joist "Joists"] [:purlin "Purlins"]
   [:girt "Girts"] [:brace "Braces"] [:panel "Panels"]])

(def role-label
  {:column "Column" :beam "Beam" :rafter "Rafter" :joist "Joist" :purlin "Purlin"
   :girt "Girt" :brace "Brace" :panel "Panel"})

(defn assembly-level
  "Id of the level an assembly is listed under: the level its start node is
  at or nearest below. nil when its start didn't resolve or there are no
  levels."
  [levels resolved]
  (when-let [[_ _ z] (:start resolved)]
    (:xt/id (grid/level-at-or-below levels z))))

(defn levels-of
  "Assembly id -> level id, for every assembly in the view."
  [{:keys [levels xlines resolved]}]
  (into {} (for [id (keys xlines)] [id (assembly-level levels (get resolved id))])))

(defn- natural-key
  "Sorts marks as people read them: C2 before C10."
  [s]
  (mapv #(if (re-matches #"\d+" %) [0 (parse-long %) ""] [1 0 %])
        (re-seq #"\d+|\D+" (str s))))

(defn- size-text [size] (when (string? size) size))

(defn- matches? [query {:keys [mark size]}]
  (or (str/blank? query)
      (let [q (str/lower-case (str/trim query))]
        (some #(str/includes? (str/lower-case (str %)) q) [mark size]))))

(defn tree
  "The navigator's tree for a view: levels (lowest first), each with role
  groups in role-groups order, each with its assemblies by mark.
  Assemblies that sit on no level (an unresolved start) come last, under a
  level with a nil id. `level-id` keeps one level; `query` keeps marks or
  sizes containing it. Empty levels and groups are left out.

    [{:level/id :level/name :groups [{:role :label :items [{:id :mark :size}]}]}]"
  [{:keys [levels xlines] :as view} & {:keys [level-id query]}]
  (let [level-of (levels-of view)
        items (->> xlines
                   (map (fn [[id x]] {:id id :mark (:assembly/mark x "XLine") :role (:xline/role x)
                                      :size (size-text (or (:assembly/size x) (:assembly/profile x))) :level (level-of id)}))
                   (filter #(or (nil? level-id) (= level-id (:level %))))
                   (filter #(matches? query %)))
        by-level (group-by :level items)
        known (set (map first role-groups))
        groups (fn [items]
                 (let [by-role (group-by #(if (known (:role %)) (:role %) :other) items)]
                   (vec (for [[role label] (conj role-groups [:other "Other"])
                              :let [xs (by-role role)]
                              :when (seq xs)]
                          {:role role :label label
                           :items (mapv #(select-keys % [:id :mark :size])
                                        (sort-by (juxt (comp natural-key :mark) (comp str :id)) xs))}))))]
    (vec (concat
          (for [l (sort-by :level/elevation levels)
                :let [xs (by-level (:xt/id l))]
                :when (seq xs)]
            {:level/id (:xt/id l) :level/name (:level/name l) :groups (groups xs)})
          (when-let [xs (seq (by-level nil))]
            [{:level/id nil :level/name "No level" :groups (groups xs)}])))))

(defn tree-path
  "Keys of the tree nodes that hold an assembly ([level-id] and [level-id role]),
  so selecting it in the viewport can open them."
  [view id]
  (let [level (assembly-level (:levels view) (get-in view [:resolved id]))
        role (get-in view [:xlines id :xline/role])]
    [[level] [level (if (some #(= role (first %)) role-groups) role :other)]]))

;; --- properties ----------------------------------------------------------------

(defn- fmt-mm [v]
  (let [r (/ (Math/round (* 10.0 v)) 10.0)]
    (if (== r (Math/floor r)) (str (long r)) (str r))))

(defn- top-parts
  "The part instructions of an assembly instruction: the children of its
  group that carry an :id, skipping the XLine itself."
  [instr]
  (let [group (loop [i instr]
                (case (:op i)
                  :transform (recur (:child i))
                  :group i
                  nil))]
    (filter #(and (keyword? (:id %)) (not= :xline (:op %))) (:children group))))

(defn- end-of-part
  "The end a connection's part belongs to: its id's namespace (:end/plate),
  or an older id's suffix (:plate-end)."
  [id]
  (or (#{:start :end} (some-> (namespace id) keyword))
      (cond (str/ends-with? (name id) "-start") :start
            (str/ends-with? (name id) "-end") :end)))

(defn parts
  "The parts list of one assembly from its instruction: the member's own
  parts (the first is the main member, at `size`), then the start's plate
  and bolts and the end's, with their sizes and counts.

    [{:id :kind (:main :part :plate :bolts) :end :size \"text\" :count n}]"
  [instr size]
  (let [ps (top-parts instr)]
    (->> (map-indexed
          (fn [i {:keys [id] kind :part/kind psize :part/size}]
            (case kind
              :plate {:id id :kind :plate :end (end-of-part id) :count 1
                      :size (str "PL " (fmt-mm (:t psize)) " × " (fmt-mm (:w psize)) " × " (fmt-mm (:h psize)))}
              :bolts {:id id :kind :bolts :end (end-of-part id) :count (:count psize)
                      :size (str "M" (fmt-mm (:dia psize)) " × " (fmt-mm (:length psize)))}
              {:id id :kind (if (zero? i) :main :part) :count 1
               :size (if (zero? i) (or (size-text size) "custom section") (name id))}))
          ps)
         (sort-by (juxt #(case (:end %) nil 0 :start 1 :end 2) #(case (:kind %) :plate 0 :bolts 1 0)))
         vec)))

(defn properties
  "The properties side bar for one assembly. `view` is what the browser
  holds; `page` the assembly's page data when it has arrived
  (construcad.edits/assembly-page), for its connections and what its ends
  sit on; `template-name` names a template id."
  [view id page template-name]
  (when-let [x (get-in view [:xlines id])]
    (let [{:keys [state supports]} page
          size (or (:assembly/size x) (:assembly/profile x) (:assembly/size state))
          ;; provenance is {:template/id :template/name}; older records hold a bare id
          from (fn [p] (cond
                         (map? p) {:id (:template/id p)
                                   :name (or (:template/name p) (template-name (:template/id p)) "(unknown)")}
                         (some? p) {:id p :name (or (template-name p) "(unknown)")}))]
      {:id id
       :mark (:assembly/mark x "XLine")
       :role (:xline/role x)
       :role-label (role-label (:xline/role x) "XLine")
       :size (or (size-text size) (when size "custom section"))
       :template (if-let [p (from (or (:assembly/from-template x) (:assembly/template x)))]
                   (assoc p :kind :from)
                   {:kind :none})
       :length (get-in view [:resolved id :length])
       :level (some #(when (= (assembly-level (:levels view) (get-in view [:resolved id])) (:xt/id %))
                       (:level/name %))
                    (:levels view))
       :ends (when page
               (into {} (for [end [:start :end]
                              :let [c (get state end)
                                    s (get supports end)]]
                          [end (cond-> {:support (when s (select-keys s [:assembly/mark :size :xline/role]))}
                                 c (assoc :connection-id (:connection/id c)
                                          :name (:connection/name c)
                                          :from (from (:connection/from-template c))))])))
       :parts (parts (get-in view [:instructions id]) size)})))
