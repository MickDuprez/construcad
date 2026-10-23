(ns construcad.template-edit
  "Pure helpers behind the template editors: drafts, parameter entry, and the
  preview models the browser regenerates with the real core (modeller.core),
  so a draft template previews without a round trip to the server.

  Preview bay: columns are vertical XLines of 3 m, rolled 90 degrees so a beam
  along +X meets a flange face; the beam hangs off the column tops (top
  centre on the node), as beams in the demo bay do."
  (:require [clojure.string :as str]
            [modeller.assembly :as assembly]
            [modeller.core :as core]
            [modeller.frame :as frame]
            [modeller.params :as params]
            [modeller.profile :as profile]
            [modeller.template :as template]))

;; --- drafts ------------------------------------------------------------------------

(def default-main
  {:part/insert :top-centre :part/offset [0 0] :part/rotation 0 :part/start-ext 0 :part/end-ext 0})

(defn blank [kind]
  (case kind
    :connection {:template/kind :connection :template/name "New connection"
                 :template/params (array-map :gap 10)
                 :template/actions [{:action/type :cut-to-face :action/face :auto}]}
    :assembly {:template/kind :assembly :template/name "New assembly" :template/role :beam
               :template/main default-main}))

(defn duplicate
  "A copy of a template that saves as a new one."
  [t]
  (-> t (dissoc :template/id) (update :template/name #(str % " (copy)"))))

(defn draft-errors
  "What stops a draft being saved, as messages; empty when it can be."
  [{:template/keys [kind name actions params]}]
  (concat (when (str/blank? name) ["Give the template a name."])
          (when (and (= :connection kind) (empty? actions)) ["A connection template needs at least one action."])
          (for [[k v] params :when (nil? v)] (str "Parameter " (clojure.core/name k) " has no value."))))

(defn move [v i delta]
  (let [j (+ i delta)]
    (if (and (< -1 i (count v)) (< -1 j (count v)))
      (assoc v i (v j) j (v i))
      v)))

(defn remove-at [v i] (into (subvec v 0 i) (subvec v (inc i))))

;; --- parameters --------------------------------------------------------------------

(defn parse-param
  "Typed text as a parameter value: a number, else a formula string; nil when blank."
  [text]
  (let [t (str/trim (str text))]
    (cond
      (str/blank? t) nil
      (re-matches #"-?\d+" t) (parse-long t)
      (re-matches #"-?(\d+\.\d*|\.\d+)" t) (parse-double t)
      :else t)))

(defn param-text [v] (if (nil? v) "" (str v)))

(defn- ordered [pairs] (apply array-map (mapcat identity pairs)))

(defn set-param
  "Params with `k` set to `v`, keeping the order rows were added in."
  [params k v]
  (ordered (if (contains? params k)
             (map (fn [[k' v']] [k' (if (= k k') v v')]) params)
             (concat params [[k v]]))))

(defn rename-param [params old new]
  (if (or (= old new) (contains? params new))
    params
    (ordered (map (fn [[k v]] [(if (= k old) new k) v]) params))))

(defn remove-param [params k] (ordered (remove #(= k (key %)) params)))

(defn param-key
  "A parameter name typed by the user as a keyword, or nil if it can't be one."
  [text]
  (let [t (str/trim (str text))]
    (when (re-matches #"[a-zA-Z][a-zA-Z0-9-]*" t) (keyword t))))

(defn section-data [lib size] (get-in lib [:sections size :section/data]))

(defn resolve-for
  "Parameters of connection template `t` (with `overrides`) resolved for an
  owner of section `size` on a support of `support-size`.
  {:params {k number} :errors [{:param k ...}]}"
  [lib t overrides size support-size]
  (params/resolve-params (merge (:template/params t) overrides)
                         (section-data lib size)
                         (section-data lib support-size)))

(defn end-summary
  "What an assembly template's end condition does at the preview sizes:
  the connection template, its resolved parameters (template order, overrides
  marked) and any formula errors. nil when the end has no connection."
  [lib templates condition size support-size]
  (when-let [t (get templates (:connection/template condition))]
    (let [overrides (:connection/params condition {})
          {:keys [params errors]} (resolve-for lib t overrides size support-size)]
      {:template t
       :rows (for [k (distinct (concat (keys (:template/params t)) (keys overrides)))]
               {:param k :value (get params k) :template-value (get-in t [:template/params k])
                :override (get overrides k) :overridden? (contains? overrides k)})
       :errors errors})))

;; --- end-on view of the main member -------------------------------------------------

(defn section-view
  "The main member seen end-on, in XLine XY (the XLine is the origin): the
  section loops and the nine insertion points where they now sit, for a
  draft's :template/main at section `size`. nil for an unknown size."
  [lib size main]
  (when-let [outline (get-in lib [:sections size :section/outline])]
    (let [f (assembly/part-frame (merge default-main main) outline)
          ->xy (fn [[x y]] (subvec (frame/point->parent f [x y 0]) 0 2))
          loops (map #(mapv ->xy (profile/tessellate-loop % 4)) (cons (:outer outline) (:holes outline)))]
      {:loops (vec loops)
       :points (into {} (for [[k [fx fy]] profile/insertion-points]
                          [k (->xy [(* fx (:b outline)) (* fy (:d outline))])]))
       :b (:b outline) :d (:d outline)})))

;; --- preview models ------------------------------------------------------------------

(def column-height 3000)

(defn- column [id x size]
  {:xline/id id :xline/role :column :assembly/mark (name id)
   :xline/start {:node/at [x 0 0]} :xline/end {:node/at [x 0 column-height]}
   :xline/rotation 90
   :assembly/parts [{:part/id :main :part/kind :extrusion :part/profile size :part/insert :centre}]})

(defn- beam [id start end size]
  {:xline/id id :xline/role :beam :assembly/mark (name id)
   :xline/start start :xline/end end :xline/rotation 0
   :assembly/parts [{:part/id :main :part/kind :extrusion :part/profile size :part/insert :top-centre}]})

(defn- base-model [lib templates xlines]
  {:grid nil :levels []
   :builders (:builders lib) :sections (:sections lib)
   :templates templates
   :xlines (into {} (map (juxt :xline/id identity)) xlines)
   :connections {}})

(defn- regenerated [m extra]
  (merge (select-keys (core/regenerate m) [:instructions :errors]) {:model m} extra))

(defn connection-preview
  "A beam of `size` running into a column of `support-size`, its end made by
  the draft connection template. {:instructions :errors :model}."
  [lib templates draft {:keys [size support-size]}]
  (let [id (or (:template/id draft) :preview/draft)
        m (-> (base-model lib (assoc templates id (assoc draft :template/id id))
                          [(column :column 0 support-size)
                           (beam :beam {:node/at [-1500 0 column-height]} {:node/of :column :node/end :end} size)])
              (assoc :connections {:joint {:connection/id :joint :connection/owner :beam :connection/end :end
                                           :connection/refs [:column] :connection/template id}}))]
    (regenerated m {})))

(defn assembly-preview
  "The draft assembly template applied at `size` in the preview bay: on a
  beam between two columns of `support-size`, or on a lone column for a
  column template. {:instructions :errors :warnings :model}."
  [lib templates draft {:keys [size support-size length] :or {length 4000}}]
  (let [id (or (:template/id draft) :preview/draft)
        draft (assoc draft :template/id id)
        column? (= :column (:template/role draft))
        m (base-model lib (assoc templates id draft)
                      (if column?
                        [(column :member 0 size)]
                        [(column :column-a 0 support-size)
                         (column :column-b length support-size)
                         (beam :member {:node/of :column-a :node/end :end} {:node/of :column-b :node/end :end} size)]))
        n (atom 0)
        r (template/apply-template m :member draft size #(keyword "preview" (str "conn-" (swap! n inc))))]
    (if (:error r)
      {:instructions {} :errors [(:error r)] :warnings [] :model m}
      (regenerated (:model r) {:warnings (:warnings r)}))))

(defn sizes-for-role
  "Designations to offer for a role: universal columns first for a column,
  everything else first for a beam."
  [sections role]
  (let [designation :section/designation
        column? #(re-find #"UC|SHS" (designation %))]
    (map designation (if (= :column role)
                       (concat (filter column? sections) (remove column? sections))
                       (concat (remove column? sections) (filter column? sections))))))
