(ns construcad.store
  "XTDB reads and writes for buildings. Reading builds the model map the core
  regenerates; writing saves one plan's records in one transaction."
  (:require [clojure.tools.logging :as log]
            [com.biffweb :as biff :refer [q]]
            [construcad.edits :as edits]
            [construcad.records :as records]
            [construcad.seed :as seed]
            [modeller.library :as library]
            [modeller.sections-au :as au]
            [xtdb.api :as xt]))

(defn load-library []
  (let [lib (library/library {} au/rows)]
    (when (seq (:errors lib))
      (log/warn "Section library errors:" (:errors lib)))
    lib))

(defn- strip [doc] (dissoc doc :db/doc-type))

(defn- docs-where [db attr value]
  (map strip (q db {:find '(pull d [*]) :in '[v] :where [['d attr 'v]]} value)))

(defn buildings
  "Every building with its project name, for the building picker."
  [db]
  (sort-by :building/name
           (q db '{:find (pull b [* {:building/project [:xt/id :project/name]}])
                   :where [[b :building/project]]})))

(defn projects
  "Every project with its buildings, for the project and building pickers.
  There are no accounts yet, so every project is listed."
  [db]
  (let [buildings (group-by #(get-in % [:building/project :xt/id]) (buildings db))]
    (->> (q db '{:find (pull p [:xt/id :project/name]) :where [[p :project/name]]})
         (sort-by :project/name)
         (mapv (fn [p] (assoc p :buildings (mapv #(select-keys % [:xt/id :building/name])
                                                 (buildings (:xt/id p)))))))))

(defn first-building-id [db]
  (:xt/id (first (buildings db))))

(defn building-model
  "{:building doc :model m} for a building, or nil."
  [{:keys [biff/db construcad/library]} building-id]
  (when-let [building (some-> building-id (->> (xt/entity db)) (#(when (:building/project %) %)))]
    (let [project (:building/project building)]
      {:building building
       :model (records/docs->model
               {:grid (strip (xt/entity db (:building/grid building)))
                :levels (docs-where db :level/building building-id)
                :assemblies (docs-where db :assembly/building building-id)
                :connections (docs-where db :connection/building building-id)
                :templates (docs-where db :template/project project)
                ;; the settings of this building's own project only
                :settings (first (sort-by (comp str :xt/id) (docs-where db :settings/project project)))}
               library)})))

(defn level-view
  "What a reader needs to show one level (or every level when level-id is
  nil): the level's assemblies and everything they hang off."
  [ctx building-id level-id]
  (when-let [{:keys [building model]} (building-model ctx building-id)]
    (assoc (edits/view model (edits/load-set model level-id))
           :building (select-keys building [:xt/id :building/name])
           :level-id level-id)))

(defn templates
  "The project's template library, the section designations a template
  can be applied at (for size pickers) and the project's detailing
  settings (edits/settings-view), or nil for an unknown building."
  [{:keys [construcad/library] :as ctx} building-id]
  (when-let [{:keys [model]} (building-model ctx building-id)]
    {:templates (:templates model)
     :settings (edits/settings-view model)
     :sections (->> (vals (:sections library))
                    (remove :section/error)
                    (map #(-> (select-keys % [:section/designation :section/builder])
                              (assoc :mass (get-in % [:section/data :mass]))))
                    (sort-by (juxt :section/builder (comp - (fnil identity 0) :mass)))
                    vec)}))

(defn settings
  "A building's project's detailing settings (edits/settings-view), or nil
  for an unknown building."
  [ctx building-id]
  (when-let [{:keys [model]} (building-model ctx building-id)]
    (edits/settings-view model)))

(defn assembly-page
  "The assembly page's data for one assembly of a building (edits/assembly-page),
  the assembly named by its id as text; nil when either is unknown."
  [ctx building-id id-text]
  (when-let [{:keys [model]} (building-model ctx building-id)]
    (when-let [id (some #(when (= id-text (str %)) %) (keys (:xlines model)))]
      (edits/assembly-page model id))))

(defn apply-edit!
  "Validate an edit, save what it writes in one transaction, and return the
  upsert patch, or {:error e}. New XLines get their id here, never from the
  browser."
  [ctx building-id edit]
  (if-let [{m :model building :building} (building-model ctx building-id)]
    (let [edit (cond-> (assoc edit :new-id random-uuid)
                 (= :xline/create (:edit/type edit)) (assoc :xline/id (random-uuid)))
          planned (edits/plan m edit)]
      (if (:error planned)
        planned
        (do (biff/submit-tx ctx (vec (records/write-docs building-id (:model planned) (:writes planned)
                                                         :project (:building/project building))))
            (edits/patch m planned))))
    {:error {:error/type :edit/unknown-building :building/id building-id}}))

(defn seed-if-empty!
  "Seed the demo project the first time the app starts on an empty database."
  [ctx]
  (let [db (xt/db (:biff.xtdb/node ctx))]
    (when-not (first-building-id db)
      (let [{:keys [docs]} (seed/demo)]
        (log/info "Seeding the demo project")
        (biff/submit-tx (assoc ctx :biff/db db) (vec docs))))))
