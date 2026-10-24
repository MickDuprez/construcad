(ns modeller.connection
  "Evaluating a connection's tree of primitives (modeller.prim) against the
  member that owns it (the owner) and the member its end sits on (the
  support), in the owner's construction space.

  Every prim starts from an anchor, an ECS:

    :node          the connection's node on the owner's XLine (its start or
                   end). Z runs along the XLine into the owner, Y is the
                   XLine's Y (up for a beam), X = Y x Z, so at the end node X
                   points the other way to the XLine's X: the two ends are a
                   half turn about Y apart, and a detail drawn for one end
                   comes out the same way up at the other.
    :support-face  where the owner's section centre line meets a face of the
                   support (:face, default :auto, the face the owner runs
                   into). Z is the face normal, out of the support towards
                   the owner; Y the XLine's Y made square to it; X = Y x Z.
    :owner-face    a face of the owner's own section (:face, default
                   :web-right, else :right), at the node: the origin where
                   the centre line, at the node, meets the face; Z the face's
                   outward normal; Y the XLine's Y made square to Z (along
                   the XLine, into the owner, for a top or bottom face);
                   X = Y x Z. For fin plates and cleats.
    :parent        the parent prim's ECS, the default for a child. A plate's
                   or gusset's is its profile's: XY in its plane, Z through
                   its thickness from the face it was drawn on.

  Faces are named in the section's own XY: :top :bottom :left :right (its
  bounds) and, for an I section, :web-left :web-right :flange-top-inner
  :flange-bottom-inner. Then the prim's :offset (in the anchor ECS), its
  :rotation (degrees about that Z) and :flip? (a half turn about X) apply,
  in that order.

  Prim types are a multimethod (apply-prim), like the actions were:

    :cut         slices a target (:target :owner-main, the default, :parent
                 or a part id) with a plane of its ECS (:plane :xy, the
                 default, :xz or :yz), pushed out by :gap along that plane's
                 normal (Z, Y or X), keeping the side the normal points to:
                 for :xy at the node and support-face anchors, the owner's
                 side. Flip or turn it to keep the other; :xz and :yz cut
                 square to a plate, for snipes.
    :plate       {:shape {:rect [w h]}} (centred, default the owner's bf x d)
                 or {:shape {:polygon [[x y] ...]}} extruded :t (10) along
                 +Z. An output part, :part/kind :plate.
    :gusset      a plate standing on its ECS's XY plane: the shape (default
                 the triangle [[0 0] [w 0] [0 h]], :w :h 100) drawn in the
                 XZ plane, legs along X and along Z, :t centred on it.
    :hole-group  :rows x :cols holes (:gauge across X, :pitch up Y, centred
                 on the ECS) of :dia (default :bolt-dia + 2), cut along Z
                 through each of :through (default [:parent]; also :support,
                 drilled in the support's own assembly, and :owner-main),
                 each cutter running :overrun (10) past both faces. With
                 :bolts? a bolt group (:bolt-dia 20, :bolt-length default
                 the thickness it passes through + 30) is an output part,
                 :part/kind :bolts.

  Output parts are named by the end and the prim's id: :end/plate,
  :start/bolts. Values are numbers, parameter names or formulas over the
  owner's section data, `support` and the resolved parameters."
  (:require [clojure.string :as str]
            [modeller.frame :as frame]
            [modeller.instr :as instr]
            [modeller.params :as params]
            [modeller.part :as part]
            [modeller.prim :as prim]
            [modeller.profile :as profile]
            [modeller.vec :as v]))

;; --- faces -------------------------------------------------------------------------

(defn section-faces
  "Named faces of a section in its own XY: {name {:origin :normal
  :thickness}}, normals outward, thickness the material behind the face."
  [{:keys [b d]} data builder]
  (let [hb (/ b 2.0) hd (/ d 2.0)
        {:keys [tf tw t]} data
        across (or tf t d)
        side (or t tw tf b)]
    (cond-> {:top    {:origin [0 hd 0] :normal [0 1 0] :thickness across}
             :bottom {:origin [0 (- hd) 0] :normal [0 -1 0] :thickness across}
             :right  {:origin [hb 0 0] :normal [1 0 0] :thickness side}
             :left   {:origin [(- hb) 0 0] :normal [-1 0 0] :thickness side}}
      (and (= :i-section builder) tw tf)
      (assoc :web-left {:origin [(- (/ tw 2.0)) 0 0] :normal [-1 0 0] :thickness tw}
             :web-right {:origin [(/ tw 2.0) 0 0] :normal [1 0 0] :thickness tw}
             :flange-top-inner {:origin [0 (- hd tf) 0] :normal [0 -1 0] :thickness tf}
             :flange-bottom-inner {:origin [0 (- tf hd) 0] :normal [0 1 0] :thickness tf}))))

(def face-names
  [:auto :top :bottom :left :right :web-left :web-right :flange-top-inner :flange-bottom-inner])

(defn- faces-in
  "Faces carried by frame f into its parent space."
  [f faces]
  (update-vals faces (fn [{:keys [origin normal] :as face}]
                       (assoc face :origin (frame/point->parent f origin) :normal (frame/dir->parent f normal)))))

;; --- context ------------------------------------------------------------------------

(defn context
  "What evaluating one connection needs: the owner (its main part, frame,
  length, section and faces), the support (its faces in the owner's space,
  its data), the end and the resolved parameters. `recipe` is the
  connection's own copy (modeller.assembly/own-recipe)."
  [model resolved owner-id owner conn recipe main-id]
  (let [ref-id (first (:connection/refs conn))
        ref-xline (get-in model [:xlines ref-id])
        ref-resolved (get resolved ref-id)
        owner-frame (get-in resolved [owner-id :frame])
        main (part/main-part owner)
        owner-outline (when main (:outline (part/part-outline model main)))
        owner-data (when main (part/section-data model main))
        support-main (part/main-part ref-xline)
        support-outline (when support-main (:outline (part/part-outline model support-main)))
        support-data (when support-main (part/section-data model support-main))
        main-frame (when owner-outline (part/part-frame main owner-outline))
        {:keys [params errors]} (params/resolve-params (:connection/params recipe) owner-data support-data)]
    {:model model
     :conn conn
     :end (:connection/end conn)
     :owner owner
     :owner-length (get-in resolved [owner-id :length])
     :owner-data owner-data
     :owner-outline owner-outline
     :support-data support-data
     :main-id main-id
     :main-frame main-frame
     :owner-faces (when main-frame
                    (faces-in main-frame (section-faces owner-outline owner-data (part/section-builder model main))))
     :support-faces (when (and support-outline owner-frame ref-resolved)
                      (faces-in (frame/relative owner-frame
                                                (frame/compose (:frame ref-resolved)
                                                               (part/part-frame support-main support-outline)))
                                (section-faces support-outline support-data (part/section-builder model support-main))))
     :params params
     :binds (params/bindings owner-data support-data (into {} (filter (comp number? val)) params))
     :param-errors errors}))

;; --- values -------------------------------------------------------------------------

(defn- error! [ctx e] (some-> (:errs ctx) (vswap! conj e)) nil)

(defn value
  "A prim's value as a number: a number, a parameter name (keyword or a
  string naming one) or a formula. nil gives `default`; an error is
  recorded and gives `default` (or 0)."
  [ctx v default]
  (let [fail (fn [e] (error! ctx e) (or default 0))]
    (cond
      (nil? v) default
      (number? v) v
      (keyword? v) (let [n (get (:params ctx) v)]
                     (if (number? n) n (fail {:error/type :prim/unknown-param :param v})))
      (string? v) (let [s (str/trim v)
                        n (when (re-matches #"[a-zA-Z][a-zA-Z0-9-]*" s) (get (:params ctx) (keyword s)))]
                    (if (number? n)
                      n
                      (let [{r :value e :error} (params/eval-formula s (:binds ctx))]
                        (if e (fail e) r))))
      :else (fail {:error/type :prim/bad-value :value v}))))

(defn- param [ctx k default] (value ctx (get-in ctx [:prim :prim/params k]) default))

;; --- anchors ------------------------------------------------------------------------

(defn- into-owner [{:keys [end]}] (if (= :start end) [0 0 1] [0 0 -1]))
(defn- node-z [{:keys [end owner-length]}] (if (= :start end) 0 owner-length))

(defn orient
  "A frame at `o` with Z along `z`: Y the first of `ups` not parallel to
  it, made square to it; X = Y x Z."
  [o z ups]
  (let [z (v/normalize z)
        up (some #(when-not (v/parallel? (v/normalize %) z) %) ups)
        y (v/normalize (v/v- up (v/scale z (v/dot up z))))]
    {:o o :x (v/cross y z) :y y :z z}))

(defn- centre-at-node [{:keys [main-frame] :as ctx}]
  (let [[cx cy] (:o main-frame [0 0 0])] [cx cy (node-z ctx)]))

(defn- onto-plane [p {:keys [origin normal]}]
  (v/v- p (v/scale normal (v/dot (v/v- p origin) normal))))

(defn pick-face
  "A named face of the support, or with :auto the bounding face the owner
  runs into."
  [{:keys [support-faces] :as ctx} face]
  (when support-faces
    (if (= :auto (or face :auto))
      (apply max-key #(v/dot (:normal %) (into-owner ctx))
             (vals (select-keys support-faces [:top :bottom :left :right])))
      (get support-faces face))))

(defmulti anchor-frame
  "The ECS a prim starts from, in the owner's construction space, or nil."
  (fn [_ctx place] (:at place)))

(defmethod anchor-frame :default [_ _] nil)

(defmethod anchor-frame :node [ctx _]
  (orient [0 0 (node-z ctx)] (into-owner ctx) [[0 1 0] [1 0 0]]))

(defmethod anchor-frame :support-face [{:keys [main-frame] :as ctx} {:keys [face]}]
  (when-let [{:keys [origin normal] :as plane} (pick-face ctx face)]
    (let [c (:o main-frame [0 0 0])
          nz (nth normal 2)
          ;; where the centre line (along Z) meets the face
          o (if (< (abs (double nz)) 1e-9)
              (onto-plane (centre-at-node ctx) plane)
              (v/v+ c [0 0 (/ (v/dot (v/v- origin c) normal) nz)]))]
      (orient o normal [[0 1 0] (into-owner ctx)]))))

(defmethod anchor-frame :owner-face [{:keys [owner-faces] :as ctx} {:keys [face]}]
  (when-let [{:keys [normal] :as plane} (get owner-faces (or face (if (:web-right owner-faces) :web-right :right)))]
    (orient (onto-plane (centre-at-node ctx) plane) normal [[0 1 0] (into-owner ctx)])))

(defmethod anchor-frame :parent [ctx _] (get-in ctx [:parent :frame]))

(defn place-frame
  "An anchor ECS moved by a prim's placement: offset, then rotation about
  Z, then flip (a half turn about X)."
  [ctx anchor {:keys [offset rotation flip?]}]
  (let [[dx dy dz] (or offset [0 0 0])
        f (frame/rotate-z (frame/translate anchor [(value ctx dx 0) (value ctx dy 0) (value ctx dz 0)])
                          (value ctx rotation 0))]
    (if flip? (assoc f :y (v/neg (:y f)) :z (v/neg (:z f))) f)))

;; --- prim types ---------------------------------------------------------------------

(defmulti apply-prim
  "Apply one prim. `ctx` carries its placed ECS (:frame), its :parent's
  output ({:frame :part :slab}) and the connection; `st` is {:parts {id
  instr} :added [ids] :outputs {prim-id output} :support-cuts [...]
  :errors [...]}. Returns st."
  (fn [_ctx prim _st] (:prim/type prim)))

(defmethod apply-prim :default [_ctx prim st]
  (update st :errors conj {:error/type :prim/unknown-type :prim/type (:prim/type prim)}))

(def types
  "The prim types an editor offers, in order: [type label help]."
  [[:cut "Cut" "Cuts the member (or its parent) with a plane of its ECS, keeping the side its normal points to."]
   [:plate "Plate" "A rectangle or polygon extruded t along +Z."]
   [:gusset "Gusset" "A triangle (or polygon) standing on its XY plane, legs along X and Z."]
   [:hole-group "Hole group" "Rows x cols holes along Z through its parent, the support or the member; bolts optional."]])

(defn known-types [] (set (keys (dissoc (methods apply-prim) :default))))

(defn- missing [st prim what]
  (update st :errors conj {:error/type :prim/missing-input :prim/type (:prim/type prim) :missing what}))

(defn- part-id [ctx prim] (keyword (name (:end ctx)) (name (:prim/id prim))))

(defn- target-id [ctx target]
  (case (or target :owner-main)
    :owner-main (:main-id ctx)
    :parent (get-in ctx [:parent :part])
    target))

(defn- rewrap
  "A part's instruction with `f` applied to its geometry, keeping its id,
  kind and size."
  [p f & args]
  (merge (select-keys p [:id :part/kind :part/size])
         (apply f (dissoc p :id :part/kind :part/size) args)))

(def cut-planes
  "A cut's :plane, and which of its ECS's axes is that plane's normal."
  {:xy :z :xz :y :yz :x})

(defmethod apply-prim :cut [ctx prim st]
  (let [id (target-id ctx (get-in prim [:prim/params :target]))
        f (:frame ctx)
        n (get f (get cut-planes (get-in prim [:prim/params :plane]) :z))
        plane {:origin (v/v+ (:o f) (v/scale n (param ctx :gap 0))) :normal n}]
    (if (get-in st [:parts id])
      (update-in st [:parts id] rewrap instr/slice plane :positive)
      (missing st prim :target))))

(defn- shape
  "A plate's outline in its XY, and its width and height: {:outline :w :h}."
  [ctx prim default-shape]
  (let [s (or (get-in prim [:prim/params :shape]) default-shape)]
    (if-let [pts (:polygon s)]
      (let [loop (mapv (fn [[x y]] [(value ctx x 0) (value ctx y 0)]) pts)
            {[x0 y0] :min [x1 y1] :max} (profile/bounds {:outer loop})]
        {:outline {:outer loop} :w (- x1 x0) :h (- y1 y0)})
      (let [[w h] (:rect s)
            data (:owner-data ctx)
            w (value ctx w (:bf data (:b data 100)))
            h (value ctx h (:d data 100))]
        {:outline (profile/centre {:outer (profile/rect-loop w h)}) :w w :h h}))))

(defn- add-plate
  "st with a plate part for `prim`: `outline` extruded `t` along +Z of
  profile frame `pf`, which its children see as their parent ECS."
  [ctx prim st pf {:keys [outline w h]} t]
  (let [id (part-id ctx prim)]
    (-> st
        (assoc-in [:parts id] (assoc (instr/transform pf (instr/extrude outline t))
                                     :id id :part/kind :plate :part/size {:t t :w w :h h}))
        (update :added conj id)
        (assoc-in [:outputs (:prim/id prim)] {:frame pf :part id :slab {:o (:o pf) :n (:z pf) :t t}}))))

(defmethod apply-prim :plate [ctx prim st]
  (add-plate ctx prim st (:frame ctx) (shape ctx prim nil) (param ctx :t 10)))

(defmethod apply-prim :gusset [ctx prim st]
  (let [{:keys [o x y z]} (:frame ctx)
        t (param ctx :t 10)
        w (param ctx :w 100) h (param ctx :h 100)
        ;; profile XY = the ECS's XZ, thickness along -Y from +t/2
        pf {:o (v/v+ o (v/scale y (/ t 2.0))) :x x :y z :z (v/neg y)}]
    (add-plate ctx prim st pf (shape ctx prim {:polygon [[0 0] [w 0] [0 h]]}) t)))

(defn- slab
  "The material a hole runs through for one :through target: {:o :n :t}
  (from :o, :t along :n), the part it cuts (or :support), or nil."
  [ctx st target axis]
  (case target
    :parent (when-let [{s :slab id :part} (:parent ctx)] (when (and s id) (assoc s :cut id)))
    :support (when-let [{:keys [origin normal thickness]} (pick-face ctx (:face ctx))]
               {:o (v/v- origin (v/scale normal thickness)) :n normal :t thickness :cut :support})
    :owner-main (let [{:keys [main-frame owner-outline owner-data main-id]} ctx]
                  (when main-frame
                    (let [[n t] (cond
                                  (v/parallel? axis (:x main-frame)) [(:x main-frame) (or (:tw owner-data) (:b owner-outline))]
                                  (v/parallel? axis (:y main-frame)) [(:y main-frame) (:d owner-outline)])]
                      (when n {:o (v/v- (:o main-frame) (v/scale n (/ t 2.0))) :n n :t t :cut main-id}))))
    nil))

(defmethod apply-prim :hole-group [ctx prim st]
  (let [f (:frame ctx)
        rows (long (param ctx :rows 2)) cols (long (param ctx :cols 2))
        gauge (param ctx :gauge 90) pitch (param ctx :pitch 70)
        bolt (param ctx :bolt-dia 20)
        dia (param ctx :dia (+ bolt 2))
        over (param ctx :overrun 10)
        through (get-in prim [:prim/params :through] [:parent])
        slabs (keep #(slab ctx st % (:z f)) through)
        centres (for [j (range rows) i (range cols)]
                  [(* gauge (- i (/ (dec cols) 2.0))) (* pitch (- j (/ (dec rows) 2.0)))])
        ;; where along each hole's axis a slab starts and ends
        span (fn [[x y] {:keys [o n t]}]
               (let [p (frame/point->parent f [x y 0])
                     dz (v/dot (:z f) n)
                     s0 (/ (v/dot (v/v- o p) n) dz)
                     s1 (+ s0 (/ t dz))]
                 [(min s0 s1) (max s0 s1)]))
        oblique (filter #(< (abs (double (v/dot (:z f) (:n %)))) 1e-6) slabs)]
    (cond
      (< (count slabs) (count through)) (missing st prim :through)
      (seq oblique) (update st :errors conj {:error/type :prim/hole-along-face :prim/type :hole-group})
      :else
      (let [cutters (fn [{:keys [cut] :as s}]
                      [cut (for [c centres :let [[s0 s1] (span c s)]]
                             (instr/transform (frame/translate f [(c 0) (c 1) (- s0 over)])
                                              (instr/cylinder (/ dia 2.0) (+ (- s1 s0) over over))))])
            st (reduce (fn [st [cut tools]]
                         (if (= :support cut)
                           (update st :support-cuts into tools)
                           (update-in st [:parts cut] rewrap instr/subtract tools)))
                       st
                       (map cutters slabs))]
        (if-not (get-in prim [:prim/params :bolts?])
          st
          (let [id (part-id ctx prim)
                stack (reduce + (map :t slabs))
                len (param ctx :bolt-length (+ stack 30))
                ;; each bolt starts at the far face of what it passes through and runs back along -Z
                bolts (for [c centres :let [top (reduce max 0 (map #(second (span c %)) slabs))]]
                        (instr/transform (-> (frame/translate f [(c 0) (c 1) top])
                                             (assoc :x (v/neg (:x f)) :z (v/neg (:z f))))
                                         (instr/cylinder (/ bolt 2.0) len)))]
            (-> st
                (assoc-in [:parts id] (assoc (instr/group bolts) :id id :part/kind :bolts
                                             :part/size {:dia bolt :length len :count (count centres)}))
                (update :added conj id))))))))

;; --- the tree -----------------------------------------------------------------------

(defn- eval-prim [ctx parent st p]
  (let [errs (volatile! [])
        place (merge {:at (if parent :parent :support-face)} (:prim/place p))
        ctx (assoc ctx :parent parent :prim p :errs errs
                   :face (if (= :support-face (:at place)) (:face place :auto) (:face ctx)))
        anchor (anchor-frame ctx place)
        id (:prim/id p)
        tag #(assoc % :prim/id id)]
    (if-not anchor
      (update st :errors conj (tag {:error/type :prim/no-anchor :at (:at place)}))
      (let [f (place-frame ctx anchor place)
            n (count (:errors st))
            st (apply-prim (assoc ctx :frame f) p (assoc-in st [:frames id] f))
            st (-> st
                   (update :errors #(into (into (subvec % 0 n) (map tag (subvec % n))) (map tag @errs))))
            me (get-in st [:outputs id] {:frame f})]
        (reduce #(eval-prim ctx me %1 %2) st (:prim/children p))))))

(defn apply-tree
  "Evaluate a connection's parts tree on `parts` (part id -> instruction,
  the owner's own). {:parts :added [output ids in order] :outputs {prim-id
  {:frame :part}} :frames {prim-id ECS} :support-cuts [...] :errors [...]},
  all in the owner's construction space."
  [ctx parts tree]
  (reduce #(eval-prim ctx nil %1 %2)
          {:parts parts :added [] :outputs {} :frames {} :support-cuts [] :errors []}
          tree))

(defn output-part
  "The part a prim makes or works on, to show it: a plate's or gusset's
  own, a hole group's bolts (else what it drills), a cut's target."
  [tree outputs end prim-id]
  (let [p (prim/find-prim tree prim-id)
        own (keyword (name end) (name prim-id))]
    (case (:prim/type p)
      (:plate :gusset) own
      :hole-group (if (get-in p [:prim/params :bolts?])
                    own
                    (when-let [parent (prim/parent-of tree prim-id)] (get-in outputs [parent :part])))
      :cut (let [t (get-in p [:prim/params :target] :owner-main)]
             (cond (= :parent t) (some->> (prim/parent-of tree prim-id) (get outputs) :part)
                   (= :owner-main t) nil
                   :else t))
      nil)))
