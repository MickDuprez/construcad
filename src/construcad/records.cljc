(ns construcad.records
  "One XTDB document per record, as in the storage layout of the spec:

    :project    name, units, section library version
    :building   project id, name, grid id
    :grid       building id + the modeller.grid record
    :level      building id + name, elevation
    :assembly   building id, level id + the XLine record with its parts
    :connection building id + the connection record (owner = assembly id)
    :template   project id + the template record

  Records keep the core's own keys, so a document minus :db/doc-type is the
  record the core reads. This namespace only maps between documents and the
  model map; the store does the querying.")

(defn project-doc [{:keys [id name]}]
  {:db/doc-type :project :xt/id id :project/name name
   :project/units :mm :project/library "ASI 8th edition"})

(defn building-doc [{:keys [id project name grid-id]}]
  {:db/doc-type :building :xt/id id :building/project project
   :building/name name :building/grid grid-id})

(defn grid-doc [building grid]
  (assoc grid :db/doc-type :grid :xt/id (:grid/id grid) :grid/building building))

(defn level-doc [building level]
  (assoc level :db/doc-type :level :level/building building))

(defn assembly-doc [building xline]
  (assoc xline :db/doc-type :assembly :xt/id (:xline/id xline) :assembly/building building))

(defn connection-doc [building c]
  (assoc c :db/doc-type :connection :xt/id (:connection/id c) :connection/building building))

(defn template-doc [project t]
  (assoc t :db/doc-type :template :xt/id (:template/id t) :template/project project))

(defn model-docs
  "Every document for a building's model map."
  [project building m]
  (concat [(grid-doc building (:grid m))]
          (map #(level-doc building %) (:levels m))
          (map #(assembly-doc building %) (vals (:xlines m)))
          (map #(connection-doc building %) (vals (:connections m)))
          (map #(template-doc project %) (vals (:templates m)))))

(defn docs->model
  "The model map regenerate takes, from a building's documents and the
  section library."
  [{:keys [grid levels assemblies connections templates]} library]
  (let [by-id (fn [k docs] (into {} (map (juxt k identity)) docs))]
    {:grid grid
     :levels (vec (sort-by :level/elevation levels))
     :xlines (by-id :xline/id assemblies)
     :connections (by-id :connection/id connections)
     :templates (by-id :template/id templates)
     :builders (:builders library)
     :sections (:sections library)}))

(defn write-docs
  "Biff transaction documents for a plan's writes against the edited model.
  `project` is needed only when the plan writes a template."
  [building m' writes & {:keys [project]}]
  (for [[kind id] writes]
    (case kind
      :assembly (assoc (assembly-doc building (get-in m' [:xlines id])) :db/op :update)
      :assembly/create (assoc (assembly-doc building (get-in m' [:xlines id])) :db/op :create)
      :grid (assoc (grid-doc building (:grid m')) :db/op :update)
      :level (assoc (level-doc building (some #(when (= id (:xt/id %)) %) (:levels m'))) :db/op :update)
      ;; no :db/op: the whole document is replaced, so removed keys go too
      :connection (connection-doc building (get-in m' [:connections id]))
      :connection/delete {:db/op :delete :xt/id id}
      :template (template-doc project (get-in m' [:templates id])))))
