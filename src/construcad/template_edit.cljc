(ns construcad.template-edit
  "Pure helpers behind the template editors: drafts, parameter entry, and the
  preview models the browser regenerates with the real core (modeller.core),
  so a draft template previews without a round trip to the server.

  The connection preview frames an owner of one size into a support of
  another, arranged as a vertical column (an XLine of 3 m, rolled 90 degrees
  so the owner meets a flange face) or as a horizontal beam the owner frames
  into at 90 degrees (a joist into a beam). Both hang off their XLines by the
  top centre, as beams in the demo bay do. The assembly page shows its
  member alone, with whatever its connections did to it: a template's ends
  sit on sample columns, an assembly's on what they really sit on, but
  neither support is drawn. The page shows it in member-space, its XLine
  lying along X."
  (:require [clojure.string :as str]
            [modeller.assembly :as assembly]
            [modeller.core :as core]
            [modeller.frame :as frame]
            [modeller.instr :as instr]
            [modeller.params :as params]
            [modeller.prim :as prim]
            [modeller.profile :as profile]
            [modeller.settings :as settings]
            [modeller.template :as template]
            [modeller.vec :as v]))

;; --- drafts ------------------------------------------------------------------------

(def default-main
  {:part/insert :top-centre :part/offset [0 0] :part/rotation 0 :part/start-ext 0 :part/end-ext 0})

(defn blank [kind]
  (case kind
    :connection {:template/kind :connection :template/name "New connection"
                 :template/params (array-map)
                 :template/parts [{:prim/id :cut :prim/type :cut :prim/name "Cut to face"
                                   :prim/place {:at :support-face}}]}
    :assembly {:template/kind :assembly :template/name "New assembly" :template/role :beam
               :template/main default-main}))

(defn as-draft
  "A template as the editors hold it: a connection template's old action
  list read as its tree of parts."
  [t]
  (if (and (= :connection (:template/kind t)) (not (contains? t :template/parts)))
    (-> t (assoc :template/parts (prim/template-parts t)) (dissoc :template/actions))
    t))

(defn duplicate
  "A copy of a template that saves as a new one."
  [t]
  (-> (as-draft t) (dissoc :template/id :template/thumbnail) (update :template/name #(str % " (copy)"))))

(defn draft-errors
  "What stops a draft being saved, as messages; empty when it can be."
  [{:template/keys [kind name parts params]}]
  (concat (when (str/blank? name) ["Give the template a name."])
          (when (and (= :connection kind) (empty? parts)) ["A connection needs at least one part."])
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

;; --- the parts tree -------------------------------------------------------------------

(defn value-text
  "A prim value as the editor shows it: a parameter name without its colon."
  [v]
  (cond (nil? v) "" (keyword? v) (name v) :else (str v)))

(defn prim-defaults
  "A new prim of `type`: its name, placement and parameters. A child sits
  on its parent; a top-level prim on the support face. What the project's
  detailing settings give (thicknesses, gauge, pitch, bolt size, a cut's
  gap) is left blank, to inherit them; a snipe (a child cut) cuts flush."
  [type child?]
  (let [place (if child? {:at :parent} {:at :support-face})]
    (case type
      :cut {:prim/name "Cut" :prim/place (assoc place :offset [0 0 0])
            :prim/params (if child? {:target :parent :plane :xz :gap 0} {})}
      :plate {:prim/name "Plate" :prim/place place
              :prim/params {:shape {:rect ["bf" "d"]}}}
      :gusset {:prim/name "Gusset" :prim/place place :prim/params {:w 100 :h 100}}
      :hole-group {:prim/name "Holes" :prim/place place
                   :prim/params {:rows 2 :cols 2
                                 :through (if child? [:parent] [:support]) :bolts? true}}
      {:prim/name (prim/type-label type) :prim/place place :prim/params {}})))

(defn add-part
  "The draft with a new prim of `type` added under `parent-id` (nil: at
  the top level). {:draft d :id new-id}."
  [draft parent-id type]
  (let [parts (vec (:template/parts draft))
        id (prim/fresh-id parts type)
        p (merge {:prim/id id :prim/type type} (prim-defaults type (some? parent-id)))]
    {:draft (assoc draft :template/parts (prim/add-prim parts parent-id p)) :id id}))

(defn polygon-text [pts]
  (str/join "  " (map (fn [[x y]] (str (value-text x) "," (value-text y))) pts)))

(defn parse-polygon
  "Typed polygon points, \"0,0  100,0  0,50\" (pairs split by spaces or
  semicolons, a pair by a comma), each a number, parameter name or formula
  without spaces. nil unless there are at least three."
  [text]
  (let [pairs (remove str/blank? (str/split (str/trim (str text)) #"[;\s]+"))
        pts (for [p pairs :let [xy (str/split p #",")]]
              (when (= 2 (count xy)) [(parse-param (first xy)) (parse-param (second xy))]))]
    (when (and (>= (count pts) 3) (every? #(and % (every? some? %)) pts))
      (vec pts))))

(defn section-data [lib size] (get-in lib [:sections size :section/data]))

(defn resolve-for
  "Parameters of connection template `t` (with `overrides`) resolved for an
  owner of section `size` on a support of `support-size`, formulas seeing
  the project's `settings` (stored or effective, modeller.settings).
  {:params {k number} :errors [{:param k ...}]}"
  ([lib t overrides size support-size] (resolve-for lib t overrides size support-size nil))
  ([lib t overrides size support-size settings]
   (params/resolve-params (merge (:template/params t) overrides)
                          (section-data lib size)
                          (section-data lib support-size)
                          (settings/effective settings))))

(defn end-info
  "What an end's card shows: the connection's name, its prims' types in order,
  the library template it is (an assembly template's end) or came from (an
  assembly's end, a copy) and that template's thumbnail when the library
  still has it. nil when the end has no connection."
  [templates condition]
  (when condition
    (let [copy (template/connection-copy templates condition)
          from (if (assembly/copy? condition)
                 (:connection/from-template condition)
                 (template/provenance (get templates (:connection/template condition))))
          t (get templates (:template/id from))]
      {:name (or (:connection/name copy) (:template/name from) "Unnamed connection")
       :parts (prim/summary (:connection/parts copy))
       :from from
       :library? (not (assembly/copy? condition))
       :known? (some? copy)
       :thumbnail (:template/thumbnail t)})))

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

(defn- base-model [lib templates xlines settings]
  {:grid nil :levels [] :settings settings
   :builders (:builders lib) :sections (:sections lib)
   :templates templates
   :xlines (into {} (map (juxt :xline/id identity)) xlines)
   :connections {}})

(def arrangements
  "How the connection preview arranges the support, for its picker."
  [[:column "Vertical column"] [:beam "Horizontal beam, framed into at 90°"]])

(defn connection-preview
  "An owner of `size` running into a support of `support-size`, its end made
  by the draft connection template. The support is a vertical column, or
  with `:arrangement :beam` a horizontal beam the owner meets at 90 degrees
  at mid-span, tops flush. {:instructions :errors :model :detail}, :detail
  each prim's ECS and part (modeller.assembly/connection-detail). Blank
  fields take the project's `settings`."
  [lib templates draft {:keys [size support-size arrangement settings]}]
  (let [z column-height
        [support owner-end] (if (= :beam arrangement)
                              [(beam :support {:node/at [0 -2000 z]} {:node/at [0 2000 z]} support-size)
                               {:node/on :support :node/station 2000}]
                              [(column :support 0 support-size) {:node/of :support :node/end :end}])
        ;; long enough to measure a deep end detail; the editor shows a stub of it (stub-preview)
        m (-> (base-model lib templates [support (beam :owner {:node/at [-6000 0 z]} owner-end size)] settings)
              ;; the joint holds a copy of the draft, as a real connection does
              (assoc :connections {:joint (merge {:connection/id :joint :connection/owner :owner :connection/end :end
                                                  :connection/refs [:support]}
                                                 (template/template->copy draft nil))}))
        {:keys [resolved instructions errors]} (core/regenerate m)]
    {:instructions instructions :errors errors :model m
     :detail (assembly/connection-detail m resolved :joint)}))

(defn- member-only
  "The member's own regeneration, nothing around it: its instruction (main
  part, plates, bolts, holes, cuts), its XLine's two nodes for the preview
  to mark, and its errors. Whatever its ends sit on is in `m` only so the
  connections can be worked out; it is never drawn."
  [m id]
  (let [{:keys [resolved instructions errors]} (core/regenerate m [id])
        {:keys [start end]} (get resolved id)]
    {:instructions (select-keys instructions [id])
     :nodes (if start [start end] [])
     :errors errors
     :model m}))

(declare preview-length)

(defn assembly-preview
  "The draft assembly template applied at `size` to a member on an XLine of
  `length` (by default the template's preview-length). An end with a connection sits on a sample support (a vertical
  column of `support-size`, as the connection editor shows by default) so
  the connection is made, but only the member is returned: what the template
  does to the member itself. {:instructions {:member i} :nodes [start end]
  :errors :model}."
  [lib templates draft {:keys [size support-size length settings] :or {support-size "200UC46.2"}}]
  (let [length (or length (preview-length lib templates draft size support-size settings))
        id (or (:template/id draft) :preview/draft)
        draft (assoc draft :template/id id)
        z column-height
        node (fn [end x]
               (if (get draft (if (= :start end) :template/start :template/end))
                 {:node/of (keyword "support" (name end)) :node/end :end}
                 {:node/at [x 0 z]}))
        m (base-model lib (assoc templates id draft)
                      [(column :support/start 0 support-size) (column :support/end length support-size)
                       (beam :member (node :start 0) (node :end length) size)]
                      settings)
        r (template/apply-template m :member draft size (let [n (atom 0)] #(keyword "preview" (str "conn" (swap! n inc)))))]
    (if (:error r)
      {:instructions {} :nodes [] :errors [(:error r)] :model m}
      (member-only (:model r) :member))))

;; --- sample length -------------------------------------------------------------------

(defn- bbox
  "Axis-aligned bounds of an instruction in its parent space,
  {:min [x y z] :max [x y z]}, or nil when it has no solid. Cut and
  subtracted solids are bounded by their target."
  [instr]
  (let [merge-boxes (fn [boxes]
                      (when-let [bs (seq (remove nil? boxes))]
                        {:min (apply mapv min (map :min bs)) :max (apply mapv max (map :max bs))}))]
    (case (:op instr)
      :extrude (let [{[x0 y0] :min [x1 y1] :max} (profile/bounds (:profile instr))]
                 {:min [x0 y0 0] :max [x1 y1 (:length instr)]})
      :box (let [[sx sy sz] (:size instr)] {:min [(/ sx -2) (/ sy -2) 0] :max [(/ sx 2) (/ sy 2) sz]})
      :cylinder (let [r (:radius instr)] {:min [(- r) (- r) 0] :max [r r (:length instr)]})
      :transform (when-let [{[x0 y0 z0] :min [x1 y1 z1] :max} (bbox (:child instr))]
                   (let [ps (for [x [x0 x1] y [y0 y1] z [z0 z1]] (frame/point->parent (:frame instr) [x y z]))]
                     {:min (apply mapv min ps) :max (apply mapv max ps)}))
      (:slice :subtract) (bbox (:target instr))
      :group (merge-boxes (map bbox (:children instr)))
      (:union :intersect) (merge-boxes (map bbox (:solids instr)))
      nil)))

(defn- cut-spans
  "Z ranges ([z0 z1]) of the cuts wrapped round a part: each slice plane where
  it crosses the bounds of what it cuts, each subtracted tool within them."
  [instr]
  (case (:op instr)
    :slice (let [{[x0 y0 tz0] :min [x1 y1 tz1] :max :as b} (bbox (:target instr))
                 {[ox oy oz] :origin [nx ny nz] :normal} (:plane instr)]
             (cond-> (cut-spans (:target instr))
               (and b (> (abs nz) 1e-9))
               (conj (let [zs (for [x [x0 x1] y [y0 y1]]
                                (-> (- oz (/ (+ (* nx (- x ox)) (* ny (- y oy))) nz)) (max tz0) (min tz1)))]
                       [(apply min zs) (apply max zs)]))))
    :subtract (let [b (bbox (:target instr))]
                (into (cut-spans (:target instr))
                      (for [t (:tools instr) :let [{[_ _ z0] :min [_ _ z1] :max} (bbox t)] :when (and b z0)]
                        [(max z0 (get-in b [:min 2])) (min z1 (get-in b [:max 2]))])))
    []))

(def ^:private trial-length
  "The XLine a template is first built on to measure its ends: long enough
  that no end detail reaches the middle."
  20000)

(defn end-extents
  "How far each end's detail reaches in along the XLine from its node, for
  the member's instruction `instr` on an XLine of `length`, the main part
  being `main-id`: the connection parts (plates, bolts...) and the cuts and
  set-backs on the main part, each counted at the nearer end. Read from the
  bounds in the XLine's local Z. {:start mm :end mm}."
  [instr length main-id]
  (let [half (/ length 2.0)
        spans (for [p (get-in instr [:child :children])
                    :when (not= :xline (:op p))
                    span (if (= main-id (:id p))
                           (let [{[_ _ z0] :min [_ _ z1] :max} (bbox p)]
                             (cond-> (cut-spans p)
                               (and z0 (pos? z0)) (conj [z0 z0])
                               (and z1 (< z1 length)) (conj [z1 z1])))
                           (when-let [{[_ _ z0] :min [_ _ z1] :max} (bbox p)] [[z0 z1]]))]
                span)
        reach (fn [end] (reduce max 0 (for [[z0 z1] spans
                                             :let [start? (< (/ (+ z0 z1) 2.0) half)]
                                             :when (= start? (= :start end))]
                                         (if start? z1 (- length z0)))))]
    {:start (reach :start) :end (reach :end)}))

(defn preview-length
  "The sample XLine length for previewing assembly template `draft` at
  `size`: short, so the end details read, but long enough to keep both of
  them, built against `support-size` columns, apart. At least 1200 mm, else
  both ends' reach plus a clear gap of the section depth (300 at least),
  rounded up to the next 100 mm."
  ([lib templates draft size] (preview-length lib templates draft size "200UC46.2"))
  ([lib templates draft size support-size] (preview-length lib templates draft size support-size nil))
  ([lib templates draft size support-size settings]
   (let [{:keys [instructions]} (assembly-preview lib templates draft
                                                  {:size size :support-size support-size :length trial-length
                                                   :settings settings})
         {:keys [start end]} (if-let [i (:member instructions)]
                               (end-extents i trial-length :main)
                               {:start 0 :end 0})
         gap (max 300 (or (:d (section-data lib size)) 0))]
     (max 1200 (* 100 (long (Math/ceil (- (/ (+ start end gap) 100.0) 1e-9))))))))

(defn instance-preview
  "One assembly as the assembly page would save it: `state` applied to
  `context` (the records its regeneration needs, construcad.edits/assembly-
  context) and regenerated with the real core, the member alone.
  {:instructions {id i} :nodes [start end] :errors :model}."
  [lib templates context state]
  (let [id (:xline/id state)
        m (merge context {:builders (:builders lib) :sections (:sections lib)
                          :templates (merge templates (:templates context))})
        r (template/update-assembly m (update state :main #(merge default-main %))
                                    (let [n (atom 0)] #(keyword "preview" (str "conn" (swap! n inc)))))]
    (if (:error r)
      {:instructions {} :nodes [] :errors [(:error r)] :model m}
      (member-only (:model r) id))))

(defn connection-instance-preview
  "One end's connection of a real assembly, with `draft` (a connection
  template draft, construcad.template-edit's editor shape) in place of its
  own copy: the assembly `state` applied to `context` (as instance-preview)
  and regenerated with what that end sits on, both drawn where they are in
  the model. {:instructions {id i} :nodes [start end] :errors :model
  :detail}, :detail as connection-preview's."
  [lib templates context state end draft]
  (let [id (:xline/id state)
        support (get-in state [:supports end])
        copy (template/template->copy draft (get-in state [end :connection/from-template]))
        m (merge context {:builders (:builders lib) :sections (:sections lib)
                          :templates (merge templates (:templates context))})
        r (template/update-assembly m (-> state
                                          (update :main #(merge default-main %))
                                          (update end merge copy))
                                    (let [n (atom 0)] #(keyword "preview" (str "conn" (swap! n inc)))))]
    (if (:error r)
      {:instructions {} :nodes [] :errors [(:error r)] :model m}
      (let [{:keys [resolved instructions errors]} (core/regenerate (:model r) (remove nil? [id support]))
            conn (template/existing-connection (:model r) id end)
            {:keys [start] end-node :end} (get resolved id)]
        {:instructions instructions
         :nodes (if start [start end-node] [])
         :errors errors
         :model (:model r)
         :detail (when conn (assembly/connection-detail (:model r) resolved (:connection/id conn)))}))))

;; --- stubs: the connection editor's short members ---------------------------------------

(def stub-min "The shortest stub of a member the connection editor shows." 600)
(def stub-clear "Clear member kept past the connection's own extent." 300)

(defn stub-length
  "A stub long enough for a connection reaching `extent` along the member:
  that plus a clear length, at least stub-min, rounded up to 100 mm."
  [extent]
  (max stub-min (* 100 (long (Math/ceil (- (/ (+ extent stub-clear) 100.0) 1e-9))))))

(defn support-window
  "The stub of the support to show, [z0 z1] along its XLine, for a
  connection spanning `lo` to `hi` along it: stub-length of that span,
  centred on it."
  [lo hi]
  (let [l (stub-length (- hi lo))
        mid (/ (+ lo hi) 2.0)]
    [(- mid (/ l 2.0)) (+ mid (/ l 2.0))]))

(defn- children [instr] (get-in instr [:child :children]))

(defn- xline-length [instr] (some #(when (= :xline (:op %)) (:length %)) (children instr)))

(defn- corners [{[x0 y0 z0] :min [x1 y1 z1] :max}]
  (for [x [x0 x1] y [y0 y1] z [z0 z1]] [x y z]))

(defn support-span
  "How far the connection at `end` of the owner (its instruction
  `owner-instr`, main part `main-id`) reaches along the support's XLine
  (frame `support-frame`): the end's own parts (plates, gussets, bolts) and
  the owner's section at the node, projected on to it. [lo hi] in the
  support's local Z."
  [owner-instr end main-id support-frame]
  (let [length (xline-length owner-instr)
        node-z (if (= :start end) 0 length)
        f (:frame owner-instr)
        pts (for [p (children owner-instr)
                  :let [id (:id p) b (when (not= :xline (:op p)) (bbox p))]
                  :when b
                  c (cond (= main-id id) (corners (-> b (assoc-in [:min 2] node-z) (assoc-in [:max 2] node-z)))
                          (and (keyword? id) (= (name end) (namespace id))) (corners b))]
              (nth (frame/point->local support-frame (frame/point->parent f c)) 2))]
    (if (seq pts)
      [(reduce min pts) (reduce max pts)]
      (let [z (nth (frame/point->local support-frame (frame/point->parent f [0 0 node-z])) 2)] [z z]))))

(defn- quiet-cuts-outside
  "A part with the cuts wrapped round it that lie wholly outside [z0 z1]
  marked :slice/clip?, so their planes are not drawn where the part no
  longer is."
  [p z0 z1]
  (case (:op p)
    :slice (let [;; this plane's span, after those of the cuts inside it
                 span (first (drop (count (cut-spans (:target p))) (cut-spans p)))]
             (cond-> (update p :target quiet-cuts-outside z0 z1)
               (and span (or (<= (span 1) z0) (>= (span 0) z1))) (assoc :slice/clip? true)))
    :subtract (update p :target quiet-cuts-outside z0 z1)
    p))

(defn clip-member
  "A member's instruction cut down to [z0 z1] along its XLine: parts
  wholly outside go, parts across an end are sliced square there (the slice
  marked :slice/clip?, so no cut plane is drawn for it), cuts left outside
  are no longer drawn, and its XLine shortens to match."
  [instr [z0 z1]]
  (let [ends [[[0 0 z0] [0 0 1]] [[0 0 z1] [0 0 -1]]]
        clip (fn [p]
               (if (= :xline (:op p))
                 (let [a (max 0 z0) b (min (:length p) z1)]
                   (when (< a b)
                     (instr/transform {:o [0 0 a] :x [1 0 0] :y [0 1 0] :z [0 0 1]} (assoc p :length (- b a)))))
                 (if-let [{[_ _ pz0] :min [_ _ pz1] :max} (bbox p)]
                   (cond
                     (or (<= pz1 z0) (>= pz0 z1)) nil
                     (and (>= pz0 z0) (<= pz1 z1)) p
                     :else (merge (select-keys p [:id :part/kind :part/size])
                                  (reduce (fn [t [o n]] (assoc (instr/slice t {:origin o :normal n} :positive) :slice/clip? true))
                                          (quiet-cuts-outside (dissoc p :id :part/kind :part/size) z0 z1)
                                          (filter (fn [[[_ _ z] [_ _ nz]]] (if (pos? nz) (> z pz0) (< z pz1))) ends))))
                   p)))]
    (update-in instr [:child :children] #(vec (keep clip %)))))

(defn stub-preview
  "A connection preview (connection-preview's or connection-instance-
  preview's) with the owner and the support cut down to short stubs about
  the connection: the owner back from its node by stub-length of the end's
  reach (end-extents), the support by support-window of the connection's
  span along it. Both are the real result, sliced, so holes and the
  connection are as they would be. Adds :stubs {:owner mm :support mm}."
  [{:keys [instructions model] :as preview} owner-id end support-id]
  (let [owner (get instructions owner-id)
        support (get instructions support-id)
        length (some-> owner xline-length)]
    (if-not (and owner length)
      preview
      (let [main-id (:part/id (first (get-in model [:xlines owner-id :assembly/parts])) :main)
            reach (get (end-extents owner length main-id) end)
            l (min length (stub-length reach))
            [lo hi] (when support (support-span owner end main-id (:frame support)))
            window (when support (support-window lo hi))]
        (cond-> (assoc-in preview [:instructions owner-id]
                          (clip-member owner (if (= :start end) [0 l] [(- length l) length])))
          support (assoc-in [:instructions support-id] (clip-member support window))
          true (assoc :stubs {:owner l :support (when window (- (window 1) (window 0)))}))))))

(def member-space
  "Where the assembly page shows a member: its XLine along +X from the
  origin, the XLine's local Y up (+Z) and its local X towards +Y, so the
  side view looks at the web, start on the left, whichever way the member
  runs in the model."
  {:o [0 0 0] :x [0 1 0] :y [0 0 1] :z [1 0 0]})

(defn in-member-space
  "A member-only preview (instructions and its XLine's two nodes) moved
  into member-space."
  [{:keys [instructions nodes] :as preview}]
  (let [[start end] nodes
        length (if end (v/magnitude (v/v- end start)) 0)]
    (assoc preview
           :instructions (update-vals instructions #(assoc % :frame member-space))
           :nodes (if end [[0 0 0] [length 0 0]] []))))

(defn preview-views
  "Named view directions (eye side) for the member preview, square to its
  XLine from `nodes` [start end]: along it, beside it (start on the left),
  from above, and an iso between. A vertical or missing XLine is seen as if
  it ran along X."
  [[start end]]
  (let [d (when (and start end) (mapv - end start))
        [ax ay] (if d (let [l (Math/sqrt (+ (* (d 0) (d 0)) (* (d 1) (d 1))))]
                        (if (> l 1e-6) [(/ (d 0) l) (/ (d 1) l)] [1 0]))
                    [1 0])
        side [ay (- ax) 0]]
    [["Iso" [(+ ax (* 1.4 (side 0))) (+ ay (* 1.4 (side 1))) 0.9]]
     ["Plan" [0 0 1]]
     ["Side" side]
     ["End" [(- ax) (- ay) 0]]]))

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
