(ns modeller.assembly
  "Turning an XLine and its parts into reader instructions, and applying the
  connections owned by that XLine. Everything is built in the XLine's
  construction space; the XLine frame is applied once, on the outside.
  Parts are modeller.part's.

  A connection, owned by the XLine whose end it sits on, carries its own
  copy of the recipe: a tree of primitives (modeller.prim, evaluated by
  modeller.connection) and the parameters they read. The template it was
  copied from is noted, nothing more; editing that template later changes
  nothing here.

    {:connection/id            :c-b1-end
     :connection/owner         :b1
     :connection/end           :end
     :connection/refs          [:c2]
     :connection/name          \"Square cut\"
     :connection/parts         [{:prim/id :cut :prim/type :cut
                                 :prim/place {:at :support-face :offset [0 0 :gap]}}]
     :connection/params        {:gap 10}
     :connection/from-template {:template/id :square-cut :template/name \"Square cut\"}}

  A connection saved as a flat list of actions (:connection/actions) is
  read as the equivalent tree (modeller.prim/actions->parts). One saved
  before connections held copies has :connection/template (a library id)
  and per-end overrides in :connection/params instead; it is read through
  that template (own-recipe)."
  (:require [modeller.connection :as connection]
            [modeller.frame :as frame]
            [modeller.instr :as instr]
            [modeller.model :as model]
            [modeller.part :as part]
            [modeller.prim :as prim]))

(def part-frame part/part-frame)
(def part-outline part/part-outline)
(def part-instruction part/part-instruction)
(def main-part part/main-part)

(def recipe-keys
  [:connection/name :connection/parts :connection/params :connection/from-template])

(defn copy?
  "Does this map hold its own recipe (a tree, or an old action list)?"
  [c]
  (or (contains? c :connection/parts) (contains? c :connection/actions)))

(defn normalise-copy
  "A copy's own name, parts, parameters and provenance, an old action list
  read as its tree."
  [c]
  (-> (select-keys c recipe-keys)
      (assoc :connection/parts (prim/connection-parts c))))

(defn own-recipe
  "A connection's own name, parts, parameters and provenance. One saved
  before connections held copies names a library template
  (:connection/template) and keeps only per-end overrides: it is read
  through that template, as it was made."
  [model conn]
  (if (copy? conn)
    (normalise-copy conn)
    (let [t (get-in model [:templates (:connection/template conn)])
          params (merge (:template/params t) (:connection/params conn))]
      (cond-> {:connection/name (:template/name t)
               :connection/parts (prim/template-parts (assoc t :template/params params))
               :connection/params params}
        t (assoc :connection/from-template {:template/id (:template/id t) :template/name (:template/name t)})))))

(defn- own-parts
  "Part id -> instruction for an XLine's own parts, before any connection."
  [model xline length]
  (let [built (map #(part/part-instruction model % length) (:assembly/parts xline))]
    {:built built
     :parts (into {::main (:part/id (part/main-part xline)) ::errors []}
                  (keep #(when (:id %) [(:id %) %]) built))}))

(defn- run-connection
  "Evaluate one connection on the owner's `parts`: modeller.connection/apply-tree's result."
  [model resolved id xline parts conn]
  (let [recipe (own-recipe model conn)
        ctx (connection/context model resolved id xline conn recipe (::main parts))]
    (-> (connection/apply-tree ctx parts (:connection/parts recipe))
        (update :errors #(into (vec (:param-errors ctx)) %))
        (assoc :tree (:connection/parts recipe)))))

(defn- apply-connection [model resolved id xline parts conn]
  (if-let [e (model/connection-error model conn)]
    (update parts ::errors conj e)
    (let [{p :parts :keys [added errors support-cuts]} (run-connection model resolved id xline parts conn)]
      (-> p
          (update ::errors into errors)
          (update ::support-cuts (fnil into []) support-cuts)
          (update ::added (fnil into []) added)))))

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

(defn connection-detail
  "One connection worked out on its owner, for an editor to show: each
  prim's ECS and the part it makes or works on, the ECSs in world space.
  {:frames {prim-id frame} :outputs {prim-id part-id} :errors [...]}, or nil."
  [model resolved conn-id]
  (when-let [conn (get-in model [:connections conn-id])]
    (let [id (:connection/owner conn)
          xline (get-in model [:xlines id])
          {:keys [frame length]} (get resolved id)]
      (when (and frame (nil? (model/connection-error model conn)))
        (let [{:keys [frames outputs errors tree]} (run-connection model resolved id xline
                                                                   (:parts (own-parts model xline length)) conn)]
          {:frames (update-vals frames #(frame/compose frame %))
           :outputs (into {} (for [p (prim/walk tree)
                                   :let [o (connection/output-part tree outputs (:connection/end conn) (:prim/id p))]
                                   :when o]
                               [(:prim/id p) o]))
           :errors errors})))))

(defn assembly-instruction
  "Instructions for one XLine-rooted assembly. Returns {:instr i :errors [...]}."
  [model resolved id]
  (let [xline (get-in model [:xlines id])
        {:keys [frame length]} (get resolved id)
        {:keys [built parts]} (own-parts model xline length)
        owned (sort-by #(if (= :start (:connection/end %)) 0 1)
                       (filter #(= id (:connection/owner %)) (vals (:connections model))))
        parts (reduce #(apply-connection model resolved id xline %1 %2) parts owned)
        main-id (::main parts)
        cuts (support-cuts model resolved id)
        parts (if (and (seq cuts) (get parts main-id))
                (update parts main-id #(merge (select-keys % [:id :part/kind :part/size])
                                              (instr/subtract (dissoc % :id :part/kind :part/size) cuts)))
                parts)
        order (map :part/id (:assembly/parts xline))
        ;; parts the connections added (plates, bolts), after the XLine's own:
        ;; the start's first, each in the order its connection made them
        added (distinct (remove (set order) (::added parts)))]
    {:instr (assoc (instr/transform frame
                                    (instr/group (cons {:op :xline :id id :length length}
                                                       (keep parts (concat order added)))))
                   :id id)
     :errors (concat (keep :error built) (::errors parts))}))
