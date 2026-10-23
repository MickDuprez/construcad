(ns modeller.assembly
  "Turning an XLine and its parts into reader instructions, and applying the
  connection actions owned by that XLine. Everything is built in the XLine's
  construction space; the XLine frame is applied once, on the outside.

  A part:

    {:part/id        :main
     :part/kind      :extrusion
     :part/profile   \"310UB40.4\"    ; a library designation, or inline data such as
                                      ; {:section/builder :rect :b 200 :d 3000}
     :part/insert    :top-centre      ; which of the nine points sits on the XLine
     :part/offset    [0 0]            ; shift of that point off the XLine, local XY
     :part/rotation  0                ; roll of the section about that point, degrees
     :part/start-ext 0                ; length beyond the start node (negative = set back)
     :part/end-ext   0}

  A connection, owned by the XLine whose end it sits on, carries its own
  copy of the recipe: an ordered list of actions (each a map with
  :action/type) and the parameters they read. The template it was copied
  from is noted, nothing more; editing that template later changes nothing
  here.

    {:connection/id            :c-b1-end
     :connection/owner         :b1
     :connection/end           :end
     :connection/refs          [:c2]
     :connection/name          \"Square cut\"
     :connection/actions       [{:action/type :cut-to-face :action/face :auto}]
     :connection/params        {:gap 10}
     :connection/from-template {:template/id :square-cut :template/name \"Square cut\"}}

  A connection saved before connections held copies has
  :connection/template (a library id) and per-end overrides in
  :connection/params instead; it is read through that template (own-recipe)."
  (:require [modeller.frame :as frame]
            [modeller.instr :as instr]
            [modeller.model :as model]
            [modeller.params :as params]
            [modeller.profile :as profile]
            [modeller.profile-builder :as builder]
            [modeller.vec :as v]))

(defn part-frame
  "Frame of the part's profile origin in XLine construction space. The
  insertion point sits at the offset, and the section rolls about it."
  [part outline]
  (let [[ox oy] (or (:part/offset part) [0 0])
        rotated (frame/rotate-z (assoc frame/world :o [ox oy (- (:part/start-ext part 0))])
                                (:part/rotation part 0))
        [sx sy] (profile/insertion-shift outline (:part/insert part))]
    (frame/translate rotated [sx sy 0])))

(defn part-outline
  "Outline for a part's profile: the stored outline of a library section, or
  inline data built with the model's compiled builders. {:outline o} or {:error e}."
  [model {p :part/profile id :part/id}]
  (let [result (if (map? p)
                 (builder/build (get-in model [:builders (:section/builder p)])
                                (dissoc p :section/builder))
                 (if-let [o (get-in model [:sections p :section/outline])]
                   {:outline o}
                   {:error {:error/type :part/unknown-profile :profile p}}))]
    (cond-> result (:error result) (update :error assoc :part/id id))))

(defn part-instruction
  "Instruction for one extrusion part, or {:error ...}."
  [model part length]
  (let [{:keys [outline error]} (part-outline model part)]
    (if error
      {:error error}
      (let [l (+ length (:part/start-ext part 0) (:part/end-ext part 0))]
        (assoc (instr/transform (part-frame part outline) (instr/extrude outline l))
               :id (:part/id part))))))

(defn main-part [xline] (first (:assembly/parts xline)))

;; --- connection actions -----------------------------------------------------

(defmulti apply-action
  "Apply one template action. Receives the connection context and the map of
  part id -> instruction, returns the updated map (or adds :errors)."
  (fn [_ctx action _parts] (:action/type action)))

(defmethod apply-action :default [_ctx action parts]
  (update parts ::errors conj {:error/type :action/unknown :action action}))

(defn- support-faces-in-owner
  "Bounding faces of the supporting member's main part, in the owner's
  construction space."
  [{:keys [model owner-frame ref-xline ref-resolved]}]
  (let [part (main-part ref-xline)
        outline (when part (:outline (part-outline model part)))]
    (when outline
      (let [to-world (frame/compose (:frame ref-resolved) (part-frame part outline))]
        (update-vals (profile/face-planes outline)
                     #(frame/plane->local owner-frame (frame/plane->parent to-world %)))))))

(defn- approach-plane
  "The supporting face the owner approaches (or the named one), pushed out
  along its normal by `offset`, and the side of it the owner keeps."
  [{:keys [conn owner-length] :as ctx} face offset]
  (when-let [faces (support-faces-in-owner ctx)]
    (let [approach (if (= :start (:connection/end conn)) [0 0 -1] [0 0 1])
          {:keys [origin normal]} (if (= :auto (or face :auto))
                                    (apply min-key #(v/dot (:normal %) approach) (vals faces))
                                    (get faces face))
          plane {:origin (v/v+ origin (v/scale normal offset)) :normal normal}
          mid [0 0 (/ owner-length 2.0)]]
      {:plane plane
       :keep (if (pos? (v/dot (v/v- mid (:origin plane)) normal)) :positive :negative)})))

(defn- slice-part [parts part-id {:keys [plane keep]}]
  (update parts part-id #(merge (select-keys % [:part/kind :part/size])
                                (assoc (instr/slice (dissoc % :id :part/kind :part/size) plane keep)
                                       :id part-id))))

(defmethod apply-action :cut-to-face
  ;; Cut the owner's part square to the face of the supporting member that it
  ;; approaches, leaving a gap. {:action/type :cut-to-face :action/part :main
  ;;  :action/face :auto :action/gap 10}; the :gap parameter wins.
  [{:keys [params] :as ctx} action parts]
  (let [part-id (:action/part action (::main parts))
        gap (get params (:action/gap-param action :gap) (:action/gap action 0))
        cut (approach-plane ctx (:action/face action) gap)]
    (if-not (and cut (get parts part-id))
      (update parts ::errors conj {:error/type :action/missing-input :action action})
      (slice-part parts part-id cut))))

(defn- end-key [conn prefix]
  (keyword (str prefix "-" (name (:connection/end conn)))))

(defmethod apply-action :end-plate
  ;; A plate against the supporting face, the main part cut back behind it.
  ;; Parameters: :gap (face to plate, default 0), :plate-t, :plate-w, :plate-h
  ;; (defaults 10, the section's bf and d), :plate-dx :plate-dy (shift of the
  ;; plate centre from the section centre, in the plate's plane).
  [{:keys [params model owner owner-data conn] :as ctx} action parts]
  (let [part-id (:action/part action (::main parts))
        gap (get params :gap 0)
        t (get params :plate-t 10)
        w (get params :plate-w (:bf owner-data (:b owner-data 100)))
        h (get params :plate-h (:d owner-data 100))
        inner (approach-plane ctx (:action/face action) gap)
        main (some #(when (= part-id (:part/id %)) %) (:assembly/parts owner))
        outline (when main (:outline (part-outline model main)))]
    (if-not (and inner outline (get parts part-id))
      (update parts ::errors conj {:error/type :action/missing-input :action action})
      (let [{p :origin n :normal} (:plane inner)
            c (:o (part-frame main outline))
            ;; where the section's centre line meets the plate's inner face
            denom (nth n 2)
            z (if (< (Math/abs (double denom)) 1e-9) 0 (/ (v/dot (v/v- p c) n) denom))
            o (v/v+ c [0 0 z])
            x (v/normalize (v/v- [1 0 0] (v/scale n (v/dot [1 0 0] n))))
            f (frame/translate {:o o :x x :y (v/cross n x) :z n}
                               [(get params :plate-dx 0) (get params :plate-dy 0) 0])
            plate-id (end-key conn "plate")
            plate (assoc (instr/transform f (instr/extrude (profile/centre {:outer (profile/rect-loop w h)}) t))
                         :id plate-id :part/kind :plate :part/size {:t t :w w :h h})]
        (-> parts
            (slice-part part-id (approach-plane ctx (:action/face action) (+ gap t)))
            (assoc plate-id plate)
            (assoc-in [::plates (:connection/end conn)] {:id plate-id :frame f :t t :gap gap}))))))

(defmethod apply-action :bolt-group
  ;; Holes through the end plate made by this connection, and the bolts in
  ;; them. Parameters: :rows :cols (default 2 x 2), :gauge (across, default
  ;; 90), :pitch (up, default 70), :bolt-dia (20), :hole-dia (bolt + 2),
  ;; :bolt-length (default plate + support flange + 30), :hole-overrun (how
  ;; far the hole cutter runs past each plate face, default 10). The hole is
  ;; a cutting solid only; each bolt is its own solid at the real bolt length.
  ;; The matching holes in the supporting member go in ::support-cuts: they
  ;; run from inside the plate, across the gap and through the support's
  ;; flange (:tf), overrunning by the same amount; the support's own assembly
  ;; picks them up (see support-cuts).
  [{:keys [params conn support-data]} action parts]
  (let [{plate-id :id f :frame t :t gap :gap} (get-in parts [::plates (:connection/end conn)])]
    (if-not plate-id
      (update parts ::errors conj {:error/type :action/no-plate :action action})
      (let [rows (long (get params :rows 2)) cols (long (get params :cols 2))
            gauge (get params :gauge 90) pitch (get params :pitch 70)
            bolt (get params :bolt-dia 20) hole (get params :hole-dia (+ bolt 2))
            len (get params :bolt-length (+ t (:tf support-data 10) 30))
            centres (for [j (range rows) i (range cols)]
                      [(* gauge (- i (/ (dec cols) 2.0))) (* pitch (- j (/ (dec rows) 2.0)))])
            ;; holes run through the plate along its +Z; bolts start at the plate's
            ;; outer face and run back through it into the support
            ;; the hole cutter runs well past both plate faces, so the boolean never
            ;; meets a coplanar face (that is what leaves facets and flicker)
            over (get params :hole-overrun 10)
            hole-tools (for [[x y] centres]
                         (instr/transform (frame/translate f [x y (- over)])
                                          (instr/cylinder (/ hole 2.0) (+ t over over))))
            bolts (for [[x y] centres]
                    (instr/transform (-> (frame/translate f [x y t])
                                         (assoc :x (v/neg (:x f)) :z (v/neg (:z f))))
                                     (instr/cylinder (/ bolt 2.0) len)))
            flipped (fn [x y z] (-> (frame/translate f [x y z])
                                    (assoc :x (v/neg (:x f)) :z (v/neg (:z f)))))
            support-tools (for [[x y] centres]
                            (instr/transform (flipped x y over)
                                             (instr/cylinder (/ hole 2.0)
                                                             (+ over (or gap 0) (:tf support-data 10) over))))
            bolts-id (end-key conn "bolts")]
        (-> parts
            (update ::support-cuts (fnil into []) support-tools)
            (update plate-id #(assoc (instr/subtract (dissoc % :id :part/kind :part/size) hole-tools)
                                     :id plate-id :part/kind :plate :part/size (:part/size %)))
            (assoc bolts-id (assoc (instr/group bolts) :id bolts-id :part/kind :bolts
                                   :part/size {:dia bolt :length len :count (count centres)})))))))

(defn- section-data
  "Table data for a part's profile: the library row, or the inline map."
  [model part]
  (let [p (:part/profile part)]
    (if (map? p) p (get-in model [:sections p :section/data]))))

(def recipe-keys
  [:connection/name :connection/actions :connection/params :connection/from-template])

(defn own-recipe
  "A connection's own name, actions, parameters and provenance. One saved
  before connections held copies names a library template
  (:connection/template) and keeps only per-end overrides: it is read
  through that template, as it was made."
  [model conn]
  (if (contains? conn :connection/actions)
    (select-keys conn recipe-keys)
    (let [t (get-in model [:templates (:connection/template conn)])]
      (cond-> {:connection/name (:template/name t)
               :connection/actions (vec (:template/actions t))
               :connection/params (merge (:template/params t) (:connection/params conn))}
        t (assoc :connection/from-template {:template/id (:template/id t) :template/name (:template/name t)})))))

(defn- connection-context [model resolved owner-id owner conn]
  (let [ref-id (first (:connection/refs conn))
        ref-xline (get-in model [:xlines ref-id])
        owner-data (section-data model (main-part owner))
        support-data (some->> (main-part ref-xline) (section-data model))
        {:keys [params errors]} (params/resolve-params (:connection/params (own-recipe model conn))
                                                       owner-data support-data)]
    {:model model
     :conn conn
     :owner owner
     :owner-data owner-data
     :support-data support-data
     :owner-frame (get-in resolved [owner-id :frame])
     :owner-length (get-in resolved [owner-id :length])
     :ref-xline ref-xline
     :ref-resolved (get resolved ref-id)
     :params params
     :param-errors errors}))

(defn- own-parts
  "Part id -> instruction for an XLine's own parts, before any connection."
  [model xline length]
  (let [built (map #(part-instruction model % length) (:assembly/parts xline))]
    {:built built
     :parts (into {::main (:part/id (main-part xline)) ::errors []}
                  (keep #(when (:id %) [(:id %) %]) built))}))

(defn- apply-connection [model resolved id xline parts conn]
  (if-let [e (model/connection-error model conn)]
    (update parts ::errors conj e)
    (let [ctx (connection-context model resolved id xline conn)]
      (reduce #(apply-action ctx %2 %1)
              (update parts ::errors into (:param-errors ctx))
              (:connection/actions (own-recipe model conn))))))

(defn support-cuts
  "Cutting solids that connections owned by other XLines put into XLine
  `id` (bolt holes through a column flange), in `id`'s construction space.
  Each is worked out by building the owner's parts and running that one
  connection, then carried from the owner's space into this one."
  [model resolved id]
  (for [conn (vals (:connections model))
        :when (and (= id (first (:connection/refs conn)))
                   (not= id (:connection/owner conn))
                   (nil? (model/connection-error model conn)))
        :let [owner-id (:connection/owner conn)
              owner (get-in model [:xlines owner-id])
              {:keys [frame length]} (get resolved owner-id)]
        :when frame
        :let [{:keys [parts]} (own-parts model owner length)
              cuts (::support-cuts (apply-connection model resolved owner-id owner parts conn))
              to-here (frame/relative (get-in resolved [id :frame]) frame)]
        cut cuts]
    (if (= :transform (:op cut))
      (instr/transform (frame/compose to-here (:frame cut)) (:child cut))
      (instr/transform to-here cut))))

(defn assembly-instruction
  "Instructions for one XLine-rooted assembly. Returns {:instr i :errors [...]}."
  [model resolved id]
  (let [xline (get-in model [:xlines id])
        {:keys [frame length]} (get resolved id)
        {:keys [built parts]} (own-parts model xline length)
        owned (filter #(= id (:connection/owner %)) (vals (:connections model)))
        parts (reduce #(apply-connection model resolved id xline %1 %2) parts owned)
        main-id (::main parts)
        cuts (support-cuts model resolved id)
        parts (if (and (seq cuts) (get parts main-id))
                (update parts main-id #(merge (select-keys % [:part/kind :part/size])
                                              (assoc (instr/subtract (dissoc % :id :part/kind :part/size) cuts)
                                                     :id main-id)))
                parts)
        order (map :part/id (:assembly/parts xline))
        ;; parts the connections added (plates, bolts), after the XLine's own
        added (sort-by str (remove (set order) (filter #(and (keyword? %) (nil? (namespace %))) (keys parts))))]
    {:instr (assoc (instr/transform frame
                                    (instr/group (cons {:op :xline :id id :length length}
                                                       (keep parts (concat order added)))))
                   :id id)
     :errors (concat (keep :error built) (::errors parts))}))
