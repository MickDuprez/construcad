(ns construcad.template-edit
  "Pure helpers behind the template editors: drafts, parameter entry, and the
  preview models the browser regenerates with the real core (modeller.core),
  so a draft template previews without a round trip to the server.

  The connection preview frames an owner of one size into a support of
  another, arranged as a vertical column (an XLine of 3 m, rolled 90 degrees
  so the owner meets a flange face) or as a horizontal beam the owner frames
  into at 90 degrees (a joist into a beam). Both hang off their XLines by the
  top centre, as beams in the demo bay do. An assembly template is shown on
  its member alone: a connection template knows nothing of member roles,
  only the two ECSs it joins."
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

(defn- uses-support? [v] (and (string? v) (boolean (re-find #"\bsupport\b" v))))

(defn end-summary
  "What an end condition does for an owner of `size`: the connection
  template, its resolved parameters (template order, overrides marked) and
  any formula errors. nil when the end has no connection. With no
  `support-size` (an assembly template can't know what its ends will sit on)
  a formula over `support` is marked :needs-support? instead of failing."
  [lib templates condition size support-size]
  (when-let [t (get templates (:connection/template condition))]
    (let [overrides (:connection/params condition {})
          {:keys [params errors]} (resolve-for lib t overrides size support-size)
          waits? (fn [k] (and (nil? support-size)
                              (uses-support? (get overrides k (get-in t [:template/params k])))))]
      {:template t
       :rows (for [k (distinct (concat (keys (:template/params t)) (keys overrides)))]
               {:param k :value (get params k) :template-value (get-in t [:template/params k])
                :override (get overrides k) :overridden? (contains? overrides k)
                :needs-support? (waits? k)})
       :errors (remove (comp waits? :param) errors)})))

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

(defn- round1 [x] (/ (Math/round (* 10.0 x)) 10.0))

(defn side-elevation
  "The main member seen from the side on an XLine of `length`, start on the
  left: distance along the XLine from the start node across, the XLine's
  local Y up. {:xline [0 length] :member [z0 z1] :y [y0 y1] :edges [y ...]},
  edges being the heights of the section's horizontal faces (flanges). nil
  for an unknown size."
  [lib size main length]
  (when-let [{:keys [loops]} (section-view lib size main)]
    (let [main (merge default-main main)
          ys (map second (apply concat loops))
          flat (fn [loop] (for [[[x0 y0] [x1 y1]] (map vector loop (concat (rest loop) [(first loop)]))
                                :when (and (< (abs (- y1 y0)) 1e-6) (> (abs (- x1 x0)) 1))]
                            (round1 y0)))]
      {:xline [0 length]
       :member [(- (:part/start-ext main)) (+ length (:part/end-ext main))]
       :y [(round1 (apply min ys)) (round1 (apply max ys))]
       :edges (vec (sort (distinct (mapcat flat loops))))})))

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

(def arrangements
  "How the connection preview arranges the support, for its picker."
  [[:column "Vertical column"] [:beam "Horizontal beam, framed into at 90°"]])

(defn connection-preview
  "An owner of `size` running into a support of `support-size`, its end made
  by the draft connection template. The support is a vertical column, or
  with `:arrangement :beam` a horizontal beam the owner meets at 90 degrees
  at mid-span, tops flush. {:instructions :errors :model}."
  [lib templates draft {:keys [size support-size arrangement]}]
  (let [id (or (:template/id draft) :preview/draft)
        z column-height
        [support owner-end] (if (= :beam arrangement)
                              [(beam :support {:node/at [0 -2000 z]} {:node/at [0 2000 z]} support-size)
                               {:node/on :support :node/station 2000}]
                              [(column :support 0 support-size) {:node/of :support :node/end :end}])
        m (-> (base-model lib (assoc templates id (assoc draft :template/id id))
                          [support (beam :owner {:node/at [-1500 0 z]} owner-end size)])
              (assoc :connections {:joint {:connection/id :joint :connection/owner :owner :connection/end :end
                                           :connection/refs [:support] :connection/template id}}))]
    (regenerated m {})))

(def preview-length 3000)

(defn assembly-preview
  "The draft assembly template applied at `size` to its member alone, on an
  XLine with nothing at either end, so no connection is made: what the
  template does to the member itself. {:instructions :errors :model}."
  [lib templates draft {:keys [size length] :or {length preview-length}}]
  (let [id (or (:template/id draft) :preview/draft)
        draft (assoc draft :template/id id)
        m (base-model lib (assoc templates id draft)
                      [(beam :member {:node/at [0 0 0]} {:node/at [length 0 0]} size)])
        r (template/apply-template m :member draft size (constantly :preview/conn))]
    (if (:error r)
      {:instructions {} :errors [(:error r)] :model m}
      (regenerated (:model r) {}))))

(defn save-edits
  "The edits that save the assembly page of one assembly: the XLine's
  rotation when it changed, then the assembly itself (:assembly/update)."
  [state {:keys [rotation rotation-was]}]
  (cond-> []
    (and (number? rotation) (not= rotation rotation-was))
    (conj {:edit/type :xline/set-rotation :xline/id (:xline/id state) :rotation rotation})
    true
    (conj {:edit/type :assembly/update :state (update state :main #(merge default-main %))})))

(def role-names
  "Member roles (construcad.edits/roles) in the order pickers offer them."
  [[:column "Column"] [:beam "Beam"] [:rafter "Rafter"] [:joist "Joist"] [:purlin "Purlin"]
   [:girt "Girt"] [:brace "Brace"] [:panel "Panel"]])

(def ^:private families
  (let [beam [#"UB\d" #"PFC" #"UC" #"RHS" #"SHS"]
        light [#"PFC" #"RHS" #"SHS" #"UB\d" #"EA"]]
    {:column [#"UC" #"UBP" #"SHS" #"RHS" #"UB"]
     :beam beam :rafter beam :joist beam
     :purlin light :girt light
     :brace [#"EA" #"UA" #"SHS" #"RHS" #"CHS"]}))

(defn sizes-for-role
  "Designations to offer for a role, the role's usual families first
  (universal beams for a beam, universal columns for a column), each in the
  order the server sent them."
  [sections role]
  (let [prefs (get families role (:beam families))
        rank (fn [d] (or (first (keep-indexed #(when (re-find %2 d) %1) prefs)) (count prefs)))]
    (sort-by rank (map :section/designation sections))))
